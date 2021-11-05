{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EC2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.EC2 where

import Amazonka.EC2
import qualified Data.Proxy as Proxy
import Test.Amazonka.EC2.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestModifyCapacityReservation $
--             newModifyCapacityReservation
--
--         , requestGetAssociatedIpv6PoolCidrs $
--             newGetAssociatedIpv6PoolCidrs
--
--         , requestImportInstance $
--             newImportInstance
--
--         , requestDescribeCapacityReservationFleets $
--             newDescribeCapacityReservationFleets
--
--         , requestModifySecurityGroupRules $
--             newModifySecurityGroupRules
--
--         , requestRevokeSecurityGroupEgress $
--             newRevokeSecurityGroupEgress
--
--         , requestCreateNetworkInterfacePermission $
--             newCreateNetworkInterfacePermission
--
--         , requestSendDiagnosticInterrupt $
--             newSendDiagnosticInterrupt
--
--         , requestDeleteLaunchTemplate $
--             newDeleteLaunchTemplate
--
--         , requestRejectVpcEndpointConnections $
--             newRejectVpcEndpointConnections
--
--         , requestCreateVpnGateway $
--             newCreateVpnGateway
--
--         , requestCreateNetworkAcl $
--             newCreateNetworkAcl
--
--         , requestDeleteKeyPair $
--             newDeleteKeyPair
--
--         , requestDescribeSecurityGroupReferences $
--             newDescribeSecurityGroupReferences
--
--         , requestDeleteFleets $
--             newDeleteFleets
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestCreateTransitGatewayRouteTable $
--             newCreateTransitGatewayRouteTable
--
--         , requestModifyInstanceMetadataOptions $
--             newModifyInstanceMetadataOptions
--
--         , requestUpdateSecurityGroupRuleDescriptionsIngress $
--             newUpdateSecurityGroupRuleDescriptionsIngress
--
--         , requestDisassociateSubnetCidrBlock $
--             newDisassociateSubnetCidrBlock
--
--         , requestDetachNetworkInterface $
--             newDetachNetworkInterface
--
--         , requestDetachInternetGateway $
--             newDetachInternetGateway
--
--         , requestDeleteVpcEndpoints $
--             newDeleteVpcEndpoints
--
--         , requestDescribeClientVpnEndpoints $
--             newDescribeClientVpnEndpoints
--
--         , requestDeleteFlowLogs $
--             newDeleteFlowLogs
--
--         , requestDescribeVpcClassicLink $
--             newDescribeVpcClassicLink
--
--         , requestGetAssociatedEnclaveCertificateIamRoles $
--             newGetAssociatedEnclaveCertificateIamRoles
--
--         , requestAssociateTransitGatewayMulticastDomain $
--             newAssociateTransitGatewayMulticastDomain
--
--         , requestModifySubnetAttribute $
--             newModifySubnetAttribute
--
--         , requestDetachVolume $
--             newDetachVolume
--
--         , requestDescribeInstanceCreditSpecifications $
--             newDescribeInstanceCreditSpecifications
--
--         , requestCancelBundleTask $
--             newCancelBundleTask
--
--         , requestDescribeByoipCidrs $
--             newDescribeByoipCidrs
--
--         , requestAcceptReservedInstancesExchangeQuote $
--             newAcceptReservedInstancesExchangeQuote
--
--         , requestReleaseAddress $
--             newReleaseAddress
--
--         , requestDescribeInstanceTypeOfferings $
--             newDescribeInstanceTypeOfferings
--
--         , requestCreateInternetGateway $
--             newCreateInternetGateway
--
--         , requestDeleteVpnConnection $
--             newDeleteVpnConnection
--
--         , requestDescribeBundleTasks $
--             newDescribeBundleTasks
--
--         , requestAuthorizeSecurityGroupEgress $
--             newAuthorizeSecurityGroupEgress
--
--         , requestEnableTransitGatewayRouteTablePropagation $
--             newEnableTransitGatewayRouteTablePropagation
--
--         , requestDeregisterImage $
--             newDeregisterImage
--
--         , requestDeleteVpcEndpointConnectionNotifications $
--             newDeleteVpcEndpointConnectionNotifications
--
--         , requestDescribeCoipPools $
--             newDescribeCoipPools
--
--         , requestResetAddressAttribute $
--             newResetAddressAttribute
--
--         , requestGetTransitGatewayMulticastDomainAssociations $
--             newGetTransitGatewayMulticastDomainAssociations
--
--         , requestDeleteLocalGatewayRouteTableVpcAssociation $
--             newDeleteLocalGatewayRouteTableVpcAssociation
--
--         , requestModifyNetworkInterfaceAttribute $
--             newModifyNetworkInterfaceAttribute
--
--         , requestModifyVpcTenancy $
--             newModifyVpcTenancy
--
--         , requestDescribeInstanceTypes $
--             newDescribeInstanceTypes
--
--         , requestCancelCapacityReservationFleets $
--             newCancelCapacityReservationFleets
--
--         , requestDescribeClientVpnAuthorizationRules $
--             newDescribeClientVpnAuthorizationRules
--
--         , requestDeleteTransitGatewayVpcAttachment $
--             newDeleteTransitGatewayVpcAttachment
--
--         , requestDeleteTransitGatewayMulticastDomain $
--             newDeleteTransitGatewayMulticastDomain
--
--         , requestCancelReservedInstancesListing $
--             newCancelReservedInstancesListing
--
--         , requestAttachClassicLinkVpc $
--             newAttachClassicLinkVpc
--
--         , requestDisableTransitGatewayRouteTablePropagation $
--             newDisableTransitGatewayRouteTablePropagation
--
--         , requestDescribeVpcClassicLinkDnsSupport $
--             newDescribeVpcClassicLinkDnsSupport
--
--         , requestAssociateSubnetCidrBlock $
--             newAssociateSubnetCidrBlock
--
--         , requestCreateNetworkInsightsPath $
--             newCreateNetworkInsightsPath
--
--         , requestRunScheduledInstances $
--             newRunScheduledInstances
--
--         , requestCreateTransitGatewayRoute $
--             newCreateTransitGatewayRoute
--
--         , requestCreateTransitGatewayPrefixListReference $
--             newCreateTransitGatewayPrefixListReference
--
--         , requestCancelSpotFleetRequests $
--             newCancelSpotFleetRequests
--
--         , requestModifyCapacityReservationFleet $
--             newModifyCapacityReservationFleet
--
--         , requestDescribeSpotPriceHistory $
--             newDescribeSpotPriceHistory
--
--         , requestDeleteTransitGatewayConnectPeer $
--             newDeleteTransitGatewayConnectPeer
--
--         , requestDescribeDhcpOptions $
--             newDescribeDhcpOptions
--
--         , requestImportImage $
--             newImportImage
--
--         , requestCreateLocalGatewayRouteTableVpcAssociation $
--             newCreateLocalGatewayRouteTableVpcAssociation
--
--         , requestCopyFpgaImage $
--             newCopyFpgaImage
--
--         , requestImportClientVpnClientCertificateRevocationList $
--             newImportClientVpnClientCertificateRevocationList
--
--         , requestStopInstances $
--             newStopInstances
--
--         , requestEnableEbsEncryptionByDefault $
--             newEnableEbsEncryptionByDefault
--
--         , requestModifyAddressAttribute $
--             newModifyAddressAttribute
--
--         , requestDeregisterTransitGatewayMulticastGroupSources $
--             newDeregisterTransitGatewayMulticastGroupSources
--
--         , requestModifyLaunchTemplate $
--             newModifyLaunchTemplate
--
--         , requestModifyVpcEndpointConnectionNotification $
--             newModifyVpcEndpointConnectionNotification
--
--         , requestDescribeInternetGateways $
--             newDescribeInternetGateways
--
--         , requestDisableVpcClassicLink $
--             newDisableVpcClassicLink
--
--         , requestGetGroupsForCapacityReservation $
--             newGetGroupsForCapacityReservation
--
--         , requestDeleteLaunchTemplateVersions $
--             newDeleteLaunchTemplateVersions
--
--         , requestBundleInstance $
--             newBundleInstance
--
--         , requestDescribeNetworkInterfaces $
--             newDescribeNetworkInterfaces
--
--         , requestReplaceNetworkAclAssociation $
--             newReplaceNetworkAclAssociation
--
--         , requestAssociateInstanceEventWindow $
--             newAssociateInstanceEventWindow
--
--         , requestDescribeNatGateways $
--             newDescribeNatGateways
--
--         , requestDescribeAddresses $
--             newDescribeAddresses
--
--         , requestRestoreManagedPrefixListVersion $
--             newRestoreManagedPrefixListVersion
--
--         , requestDescribeSnapshotAttribute $
--             newDescribeSnapshotAttribute
--
--         , requestDescribeIdentityIdFormat $
--             newDescribeIdentityIdFormat
--
--         , requestReplaceRoute $
--             newReplaceRoute
--
--         , requestDescribeVpcEndpointServices $
--             newDescribeVpcEndpointServices
--
--         , requestDeleteLocalGatewayRoute $
--             newDeleteLocalGatewayRoute
--
--         , requestAuthorizeSecurityGroupIngress $
--             newAuthorizeSecurityGroupIngress
--
--         , requestCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnection
--
--         , requestDescribeSubnets $
--             newDescribeSubnets
--
--         , requestGetTransitGatewayAttachmentPropagations $
--             newGetTransitGatewayAttachmentPropagations
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestPurchaseReservedInstancesOffering $
--             newPurchaseReservedInstancesOffering
--
--         , requestDeleteNetworkAclEntry $
--             newDeleteNetworkAclEntry
--
--         , requestResetSnapshotAttribute $
--             newResetSnapshotAttribute
--
--         , requestDescribeVpnConnections $
--             newDescribeVpnConnections
--
--         , requestModifyInstanceEventStartTime $
--             newModifyInstanceEventStartTime
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestReplaceNetworkAclEntry $
--             newReplaceNetworkAclEntry
--
--         , requestDeleteInstanceEventWindow $
--             newDeleteInstanceEventWindow
--
--         , requestDescribeVpcEndpoints $
--             newDescribeVpcEndpoints
--
--         , requestCreateTrafficMirrorFilter $
--             newCreateTrafficMirrorFilter
--
--         , requestResetInstanceAttribute $
--             newResetInstanceAttribute
--
--         , requestModifyIdentityIdFormat $
--             newModifyIdentityIdFormat
--
--         , requestAttachNetworkInterface $
--             newAttachNetworkInterface
--
--         , requestCreateCapacityReservation $
--             newCreateCapacityReservation
--
--         , requestDescribeInstanceStatus $
--             newDescribeInstanceStatus
--
--         , requestImportKeyPair $
--             newImportKeyPair
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestConfirmProductInstance $
--             newConfirmProductInstance
--
--         , requestDescribeInstanceAttribute $
--             newDescribeInstanceAttribute
--
--         , requestDescribeReservedInstancesOfferings $
--             newDescribeReservedInstancesOfferings
--
--         , requestCreateCustomerGateway $
--             newCreateCustomerGateway
--
--         , requestDescribeNetworkInsightsAnalyses $
--             newDescribeNetworkInsightsAnalyses
--
--         , requestDescribeFleets $
--             newDescribeFleets
--
--         , requestDeleteNetworkInsightsAnalysis $
--             newDeleteNetworkInsightsAnalysis
--
--         , requestCreateTransitGatewayPeeringAttachment $
--             newCreateTransitGatewayPeeringAttachment
--
--         , requestDeleteSecurityGroup $
--             newDeleteSecurityGroup
--
--         , requestDescribePublicIpv4Pools $
--             newDescribePublicIpv4Pools
--
--         , requestDescribeClientVpnTargetNetworks $
--             newDescribeClientVpnTargetNetworks
--
--         , requestDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnection
--
--         , requestAttachInternetGateway $
--             newAttachInternetGateway
--
--         , requestModifyInstancePlacement $
--             newModifyInstancePlacement
--
--         , requestDescribeFlowLogs $
--             newDescribeFlowLogs
--
--         , requestDescribeLocalGatewayVirtualInterfaceGroups $
--             newDescribeLocalGatewayVirtualInterfaceGroups
--
--         , requestDeleteTransitGatewayConnect $
--             newDeleteTransitGatewayConnect
--
--         , requestDescribeLocalGatewayRouteTableVpcAssociations $
--             newDescribeLocalGatewayRouteTableVpcAssociations
--
--         , requestDescribeVpcEndpointConnectionNotifications $
--             newDescribeVpcEndpointConnectionNotifications
--
--         , requestGetManagedPrefixListEntries $
--             newGetManagedPrefixListEntries
--
--         , requestDisassociateInstanceEventWindow $
--             newDisassociateInstanceEventWindow
--
--         , requestRunInstances $
--             newRunInstances
--
--         , requestCreateSnapshots $
--             newCreateSnapshots
--
--         , requestAssociateDhcpOptions $
--             newAssociateDhcpOptions
--
--         , requestDeleteTrafficMirrorFilterRule $
--             newDeleteTrafficMirrorFilterRule
--
--         , requestDescribeReservedInstances $
--             newDescribeReservedInstances
--
--         , requestDescribeIdFormat $
--             newDescribeIdFormat
--
--         , requestDescribeVpcs $
--             newDescribeVpcs
--
--         , requestDescribeConversionTasks $
--             newDescribeConversionTasks
--
--         , requestDisableImageDeprecation $
--             newDisableImageDeprecation
--
--         , requestCreateLaunchTemplateVersion $
--             newCreateLaunchTemplateVersion
--
--         , requestGetManagedPrefixListAssociations $
--             newGetManagedPrefixListAssociations
--
--         , requestDisableVpcClassicLinkDnsSupport $
--             newDisableVpcClassicLinkDnsSupport
--
--         , requestApplySecurityGroupsToClientVpnTargetNetwork $
--             newApplySecurityGroupsToClientVpnTargetNetwork
--
--         , requestDescribeTrafficMirrorTargets $
--             newDescribeTrafficMirrorTargets
--
--         , requestDescribeVolumesModifications $
--             newDescribeVolumesModifications
--
--         , requestExportImage $
--             newExportImage
--
--         , requestCreateFpgaImage $
--             newCreateFpgaImage
--
--         , requestAcceptVpcEndpointConnections $
--             newAcceptVpcEndpointConnections
--
--         , requestDeleteClientVpnEndpoint $
--             newDeleteClientVpnEndpoint
--
--         , requestSearchTransitGatewayRoutes $
--             newSearchTransitGatewayRoutes
--
--         , requestGetLaunchTemplateData $
--             newGetLaunchTemplateData
--
--         , requestAllocateAddress $
--             newAllocateAddress
--
--         , requestAcceptTransitGatewayVpcAttachment $
--             newAcceptTransitGatewayVpcAttachment
--
--         , requestCancelConversionTask $
--             newCancelConversionTask
--
--         , requestModifyImageAttribute $
--             newModifyImageAttribute
--
--         , requestCreateRouteTable $
--             newCreateRouteTable
--
--         , requestRejectTransitGatewayPeeringAttachment $
--             newRejectTransitGatewayPeeringAttachment
--
--         , requestReportInstanceStatus $
--             newReportInstanceStatus
--
--         , requestAttachVolume $
--             newAttachVolume
--
--         , requestRequestSpotInstances $
--             newRequestSpotInstances
--
--         , requestWithdrawByoipCidr $
--             newWithdrawByoipCidr
--
--         , requestDescribeHostReservationOfferings $
--             newDescribeHostReservationOfferings
--
--         , requestResetFpgaImageAttribute $
--             newResetFpgaImageAttribute
--
--         , requestModifyVpnConnection $
--             newModifyVpnConnection
--
--         , requestCreateTrafficMirrorFilterRule $
--             newCreateTrafficMirrorFilterRule
--
--         , requestDeleteTransitGateway $
--             newDeleteTransitGateway
--
--         , requestStartVpcEndpointServicePrivateDnsVerification $
--             newStartVpcEndpointServicePrivateDnsVerification
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestRejectVpcPeeringConnection $
--             newRejectVpcPeeringConnection
--
--         , requestDescribeClientVpnRoutes $
--             newDescribeClientVpnRoutes
--
--         , requestDeleteVpnConnectionRoute $
--             newDeleteVpnConnectionRoute
--
--         , requestAssociateEnclaveCertificateIamRole $
--             newAssociateEnclaveCertificateIamRole
--
--         , requestModifyVpcEndpoint $
--             newModifyVpcEndpoint
--
--         , requestDescribeFpgaImageAttribute $
--             newDescribeFpgaImageAttribute
--
--         , requestAllocateHosts $
--             newAllocateHosts
--
--         , requestCreateClientVpnEndpoint $
--             newCreateClientVpnEndpoint
--
--         , requestCreateTrafficMirrorSession $
--             newCreateTrafficMirrorSession
--
--         , requestRegisterImage $
--             newRegisterImage
--
--         , requestAdvertiseByoipCidr $
--             newAdvertiseByoipCidr
--
--         , requestModifyFleet $
--             newModifyFleet
--
--         , requestRevokeSecurityGroupIngress $
--             newRevokeSecurityGroupIngress
--
--         , requestGetEbsDefaultKmsKeyId $
--             newGetEbsDefaultKmsKeyId
--
--         , requestDescribeHostReservations $
--             newDescribeHostReservations
--
--         , requestUpdateSecurityGroupRuleDescriptionsEgress $
--             newUpdateSecurityGroupRuleDescriptionsEgress
--
--         , requestEnableVpcClassicLinkDnsSupport $
--             newEnableVpcClassicLinkDnsSupport
--
--         , requestDescribeVpcEndpointConnections $
--             newDescribeVpcEndpointConnections
--
--         , requestModifyReservedInstances $
--             newModifyReservedInstances
--
--         , requestDeleteFpgaImage $
--             newDeleteFpgaImage
--
--         , requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
--
--         , requestEnableImageDeprecation $
--             newEnableImageDeprecation
--
--         , requestDescribeScheduledInstances $
--             newDescribeScheduledInstances
--
--         , requestSearchTransitGatewayMulticastGroups $
--             newSearchTransitGatewayMulticastGroups
--
--         , requestCreateFlowLogs $
--             newCreateFlowLogs
--
--         , requestDescribeSpotFleetRequests $
--             newDescribeSpotFleetRequests
--
--         , requestMoveAddressToVpc $
--             newMoveAddressToVpc
--
--         , requestDescribeFleetInstances $
--             newDescribeFleetInstances
--
--         , requestDescribeLaunchTemplateVersions $
--             newDescribeLaunchTemplateVersions
--
--         , requestStartNetworkInsightsAnalysis $
--             newStartNetworkInsightsAnalysis
--
--         , requestModifyInstanceCreditSpecification $
--             newModifyInstanceCreditSpecification
--
--         , requestDescribePrincipalIdFormat $
--             newDescribePrincipalIdFormat
--
--         , requestDescribeTransitGateways $
--             newDescribeTransitGateways
--
--         , requestDeleteNetworkAcl $
--             newDeleteNetworkAcl
--
--         , requestDisassociateTransitGatewayMulticastDomain $
--             newDisassociateTransitGatewayMulticastDomain
--
--         , requestDeleteTransitGatewayRouteTable $
--             newDeleteTransitGatewayRouteTable
--
--         , requestDescribeSecurityGroupRules $
--             newDescribeSecurityGroupRules
--
--         , requestCreateLaunchTemplate $
--             newCreateLaunchTemplate
--
--         , requestCreateVpcEndpointConnectionNotification $
--             newCreateVpcEndpointConnectionNotification
--
--         , requestDeleteNetworkInterfacePermission $
--             newDeleteNetworkInterfacePermission
--
--         , requestDeleteVpnGateway $
--             newDeleteVpnGateway
--
--         , requestCreateStoreImageTask $
--             newCreateStoreImageTask
--
--         , requestCreateTrafficMirrorTarget $
--             newCreateTrafficMirrorTarget
--
--         , requestDescribeImportImageTasks $
--             newDescribeImportImageTasks
--
--         , requestDescribeVolumeAttribute $
--             newDescribeVolumeAttribute
--
--         , requestDescribeMovingAddresses $
--             newDescribeMovingAddresses
--
--         , requestExportTransitGatewayRoutes $
--             newExportTransitGatewayRoutes
--
--         , requestGetPasswordData $
--             newGetPasswordData
--
--         , requestCreateVpc $
--             newCreateVpc
--
--         , requestModifyVpcPeeringConnectionOptions $
--             newModifyVpcPeeringConnectionOptions
--
--         , requestDescribeFpgaImages $
--             newDescribeFpgaImages
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestAcceptTransitGatewayPeeringAttachment $
--             newAcceptTransitGatewayPeeringAttachment
--
--         , requestDisassociateAddress $
--             newDisassociateAddress
--
--         , requestModifyTrafficMirrorFilterNetworkServices $
--             newModifyTrafficMirrorFilterNetworkServices
--
--         , requestDescribeEgressOnlyInternetGateways $
--             newDescribeEgressOnlyInternetGateways
--
--         , requestDeleteVpc $
--             newDeleteVpc
--
--         , requestCreateInstanceExportTask $
--             newCreateInstanceExportTask
--
--         , requestRejectTransitGatewayVpcAttachment $
--             newRejectTransitGatewayVpcAttachment
--
--         , requestDescribeTrafficMirrorSessions $
--             newDescribeTrafficMirrorSessions
--
--         , requestGetTransitGatewayRouteTableAssociations $
--             newGetTransitGatewayRouteTableAssociations
--
--         , requestAssociateVpcCidrBlock $
--             newAssociateVpcCidrBlock
--
--         , requestDescribeVpcAttribute $
--             newDescribeVpcAttribute
--
--         , requestCreateVolume $
--             newCreateVolume
--
--         , requestCreateDefaultSubnet $
--             newCreateDefaultSubnet
--
--         , requestDescribeScheduledInstanceAvailability $
--             newDescribeScheduledInstanceAvailability
--
--         , requestDisassociateClientVpnTargetNetwork $
--             newDisassociateClientVpnTargetNetwork
--
--         , requestCreateClientVpnRoute $
--             newCreateClientVpnRoute
--
--         , requestModifyVolumeAttribute $
--             newModifyVolumeAttribute
--
--         , requestExportClientVpnClientConfiguration $
--             newExportClientVpnClientConfiguration
--
--         , requestDescribeTrunkInterfaceAssociations $
--             newDescribeTrunkInterfaceAssociations
--
--         , requestDeleteTrafficMirrorTarget $
--             newDeleteTrafficMirrorTarget
--
--         , requestDescribeSpotDatafeedSubscription $
--             newDescribeSpotDatafeedSubscription
--
--         , requestDescribeLocalGatewayRouteTables $
--             newDescribeLocalGatewayRouteTables
--
--         , requestDescribePrefixLists $
--             newDescribePrefixLists
--
--         , requestAssociateTransitGatewayRouteTable $
--             newAssociateTransitGatewayRouteTable
--
--         , requestDeletePlacementGroup $
--             newDeletePlacementGroup
--
--         , requestModifyTransitGateway $
--             newModifyTransitGateway
--
--         , requestDeleteTransitGatewayPrefixListReference $
--             newDeleteTransitGatewayPrefixListReference
--
--         , requestCreateTransitGatewayMulticastDomain $
--             newCreateTransitGatewayMulticastDomain
--
--         , requestDeregisterInstanceEventNotificationAttributes $
--             newDeregisterInstanceEventNotificationAttributes
--
--         , requestRequestSpotFleet $
--             newRequestSpotFleet
--
--         , requestDeleteNetworkInsightsPath $
--             newDeleteNetworkInsightsPath
--
--         , requestDescribeTransitGatewayConnects $
--             newDescribeTransitGatewayConnects
--
--         , requestDeleteTransitGatewayRoute $
--             newDeleteTransitGatewayRoute
--
--         , requestCreateTransitGatewayConnectPeer $
--             newCreateTransitGatewayConnectPeer
--
--         , requestDisableEbsEncryptionByDefault $
--             newDisableEbsEncryptionByDefault
--
--         , requestDeregisterTransitGatewayMulticastGroupMembers $
--             newDeregisterTransitGatewayMulticastGroupMembers
--
--         , requestAssociateTrunkInterface $
--             newAssociateTrunkInterface
--
--         , requestCreateSubnet $
--             newCreateSubnet
--
--         , requestCreateNetworkInterface $
--             newCreateNetworkInterface
--
--         , requestDescribeSecurityGroups $
--             newDescribeSecurityGroups
--
--         , requestGetCapacityReservationUsage $
--             newGetCapacityReservationUsage
--
--         , requestCreateTransitGatewayVpcAttachment $
--             newCreateTransitGatewayVpcAttachment
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestModifySpotFleetRequest $
--             newModifySpotFleetRequest
--
--         , requestDetachVpnGateway $
--             newDetachVpnGateway
--
--         , requestModifyManagedPrefixList $
--             newModifyManagedPrefixList
--
--         , requestGetHostReservationPurchasePreview $
--             newGetHostReservationPurchasePreview
--
--         , requestEnableVolumeIO $
--             newEnableVolumeIO
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDescribeInstanceEventWindows $
--             newDescribeInstanceEventWindows
--
--         , requestDisableSerialConsoleAccess $
--             newDisableSerialConsoleAccess
--
--         , requestCreateNatGateway $
--             newCreateNatGateway
--
--         , requestDescribeLocalGatewayVirtualInterfaces $
--             newDescribeLocalGatewayVirtualInterfaces
--
--         , requestDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnections
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestCreateVpcEndpointServiceConfiguration $
--             newCreateVpcEndpointServiceConfiguration
--
--         , requestCreateDefaultVpc $
--             newCreateDefaultVpc
--
--         , requestDisassociateVpcCidrBlock $
--             newDisassociateVpcCidrBlock
--
--         , requestDescribeTrafficMirrorFilters $
--             newDescribeTrafficMirrorFilters
--
--         , requestDescribeFastSnapshotRestores $
--             newDescribeFastSnapshotRestores
--
--         , requestCancelCapacityReservation $
--             newCancelCapacityReservation
--
--         , requestDeleteNetworkInterface $
--             newDeleteNetworkInterface
--
--         , requestDisassociateTransitGatewayRouteTable $
--             newDisassociateTransitGatewayRouteTable
--
--         , requestReplaceRouteTableAssociation $
--             newReplaceRouteTableAssociation
--
--         , requestStartInstances $
--             newStartInstances
--
--         , requestCreatePlacementGroup $
--             newCreatePlacementGroup
--
--         , requestDescribeInstanceEventNotificationAttributes $
--             newDescribeInstanceEventNotificationAttributes
--
--         , requestDescribeCapacityReservations $
--             newDescribeCapacityReservations
--
--         , requestModifyClientVpnEndpoint $
--             newModifyClientVpnEndpoint
--
--         , requestModifyInstanceCapacityReservationAttributes $
--             newModifyInstanceCapacityReservationAttributes
--
--         , requestDescribeAggregateIdFormat $
--             newDescribeAggregateIdFormat
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestGetSubnetCidrReservations $
--             newGetSubnetCidrReservations
--
--         , requestAssociateAddress $
--             newAssociateAddress
--
--         , requestModifyTrafficMirrorFilterRule $
--             newModifyTrafficMirrorFilterRule
--
--         , requestDescribeNetworkInterfaceAttribute $
--             newDescribeNetworkInterfaceAttribute
--
--         , requestReplaceIamInstanceProfileAssociation $
--             newReplaceIamInstanceProfileAssociation
--
--         , requestAssociateClientVpnTargetNetwork $
--             newAssociateClientVpnTargetNetwork
--
--         , requestReleaseHosts $
--             newReleaseHosts
--
--         , requestEnableSerialConsoleAccess $
--             newEnableSerialConsoleAccess
--
--         , requestResetNetworkInterfaceAttribute $
--             newResetNetworkInterfaceAttribute
--
--         , requestDeleteInternetGateway $
--             newDeleteInternetGateway
--
--         , requestDescribeReservedInstancesListings $
--             newDescribeReservedInstancesListings
--
--         , requestCreateVpnConnection $
--             newCreateVpnConnection
--
--         , requestReplaceTransitGatewayRoute $
--             newReplaceTransitGatewayRoute
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestDeleteNatGateway $
--             newDeleteNatGateway
--
--         , requestDescribeImportSnapshotTasks $
--             newDescribeImportSnapshotTasks
--
--         , requestGetCoipPoolUsage $
--             newGetCoipPoolUsage
--
--         , requestDescribeCustomerGateways $
--             newDescribeCustomerGateways
--
--         , requestDeleteSubnet $
--             newDeleteSubnet
--
--         , requestCopyImage $
--             newCopyImage
--
--         , requestCreateVpcEndpoint $
--             newCreateVpcEndpoint
--
--         , requestModifyTrafficMirrorSession $
--             newModifyTrafficMirrorSession
--
--         , requestDescribeCarrierGateways $
--             newDescribeCarrierGateways
--
--         , requestDescribeTransitGatewayPeeringAttachments $
--             newDescribeTransitGatewayPeeringAttachments
--
--         , requestDeleteQueuedReservedInstances $
--             newDeleteQueuedReservedInstances
--
--         , requestDescribeTransitGatewayMulticastDomains $
--             newDescribeTransitGatewayMulticastDomains
--
--         , requestGetDefaultCreditSpecification $
--             newGetDefaultCreditSpecification
--
--         , requestUnmonitorInstances $
--             newUnmonitorInstances
--
--         , requestDescribeTransitGatewayVpcAttachments $
--             newDescribeTransitGatewayVpcAttachments
--
--         , requestDescribeTransitGatewayConnectPeers $
--             newDescribeTransitGatewayConnectPeers
--
--         , requestCreateSecurityGroup $
--             newCreateSecurityGroup
--
--         , requestCreateInstanceEventWindow $
--             newCreateInstanceEventWindow
--
--         , requestGetEbsEncryptionByDefault $
--             newGetEbsEncryptionByDefault
--
--         , requestImportVolume $
--             newImportVolume
--
--         , requestDeleteCarrierGateway $
--             newDeleteCarrierGateway
--
--         , requestDisableVgwRoutePropagation $
--             newDisableVgwRoutePropagation
--
--         , requestDeleteTrafficMirrorFilter $
--             newDeleteTrafficMirrorFilter
--
--         , requestModifyVpnTunnelCertificate $
--             newModifyVpnTunnelCertificate
--
--         , requestCreateSpotDatafeedSubscription $
--             newCreateSpotDatafeedSubscription
--
--         , requestCancelSpotInstanceRequests $
--             newCancelSpotInstanceRequests
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestDescribeVpcEndpointServiceConfigurations $
--             newDescribeVpcEndpointServiceConfigurations
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestAssignPrivateIpAddresses $
--             newAssignPrivateIpAddresses
--
--         , requestAuthorizeClientVpnIngress $
--             newAuthorizeClientVpnIngress
--
--         , requestDeleteTransitGatewayPeeringAttachment $
--             newDeleteTransitGatewayPeeringAttachment
--
--         , requestModifyInstanceAttribute $
--             newModifyInstanceAttribute
--
--         , requestDeleteCustomerGateway $
--             newDeleteCustomerGateway
--
--         , requestDisassociateIamInstanceProfile $
--             newDisassociateIamInstanceProfile
--
--         , requestTerminateClientVpnConnections $
--             newTerminateClientVpnConnections
--
--         , requestCreateTransitGatewayConnect $
--             newCreateTransitGatewayConnect
--
--         , requestDisassociateRouteTable $
--             newDisassociateRouteTable
--
--         , requestGetConsoleScreenshot $
--             newGetConsoleScreenshot
--
--         , requestGetFlowLogsIntegrationTemplate $
--             newGetFlowLogsIntegrationTemplate
--
--         , requestResetEbsDefaultKmsKeyId $
--             newResetEbsDefaultKmsKeyId
--
--         , requestAssignIpv6Addresses $
--             newAssignIpv6Addresses
--
--         , requestModifyVpnTunnelOptions $
--             newModifyVpnTunnelOptions
--
--         , requestModifyEbsDefaultKmsKeyId $
--             newModifyEbsDefaultKmsKeyId
--
--         , requestDeleteSpotDatafeedSubscription $
--             newDeleteSpotDatafeedSubscription
--
--         , requestModifyVolume $
--             newModifyVolume
--
--         , requestEnableVpcClassicLink $
--             newEnableVpcClassicLink
--
--         , requestDescribePlacementGroups $
--             newDescribePlacementGroups
--
--         , requestProvisionByoipCidr $
--             newProvisionByoipCidr
--
--         , requestDisassociateEnclaveCertificateIamRole $
--             newDisassociateEnclaveCertificateIamRole
--
--         , requestModifyAvailabilityZoneGroup $
--             newModifyAvailabilityZoneGroup
--
--         , requestDescribeStaleSecurityGroups $
--             newDescribeStaleSecurityGroups
--
--         , requestCreateCarrierGateway $
--             newCreateCarrierGateway
--
--         , requestDescribeExportImageTasks $
--             newDescribeExportImageTasks
--
--         , requestPurchaseScheduledInstances $
--             newPurchaseScheduledInstances
--
--         , requestEnableVgwRoutePropagation $
--             newEnableVgwRoutePropagation
--
--         , requestDescribeSpotFleetRequestHistory $
--             newDescribeSpotFleetRequestHistory
--
--         , requestModifySnapshotAttribute $
--             newModifySnapshotAttribute
--
--         , requestDescribeIamInstanceProfileAssociations $
--             newDescribeIamInstanceProfileAssociations
--
--         , requestDescribeNetworkInsightsPaths $
--             newDescribeNetworkInsightsPaths
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateLocalGatewayRoute $
--             newCreateLocalGatewayRoute
--
--         , requestCreateNetworkAclEntry $
--             newCreateNetworkAclEntry
--
--         , requestDescribeTransitGatewayAttachments $
--             newDescribeTransitGatewayAttachments
--
--         , requestCreateReservedInstancesListing $
--             newCreateReservedInstancesListing
--
--         , requestDescribeIpv6Pools $
--             newDescribeIpv6Pools
--
--         , requestAttachVpnGateway $
--             newAttachVpnGateway
--
--         , requestDescribeLocalGateways $
--             newDescribeLocalGateways
--
--         , requestModifyVpcEndpointServicePermissions $
--             newModifyVpcEndpointServicePermissions
--
--         , requestExportClientVpnClientCertificateRevocationList $
--             newExportClientVpnClientCertificateRevocationList
--
--         , requestCreateDhcpOptions $
--             newCreateDhcpOptions
--
--         , requestRegisterTransitGatewayMulticastGroupSources $
--             newRegisterTransitGatewayMulticastGroupSources
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestGetTransitGatewayRouteTablePropagations $
--             newGetTransitGatewayRouteTablePropagations
--
--         , requestModifyFpgaImageAttribute $
--             newModifyFpgaImageAttribute
--
--         , requestModifyHosts $
--             newModifyHosts
--
--         , requestRebootInstances $
--             newRebootInstances
--
--         , requestModifyVpcEndpointServiceConfiguration $
--             newModifyVpcEndpointServiceConfiguration
--
--         , requestCreateTransitGateway $
--             newCreateTransitGateway
--
--         , requestUnassignIpv6Addresses $
--             newUnassignIpv6Addresses
--
--         , requestDeleteTrafficMirrorSession $
--             newDeleteTrafficMirrorSession
--
--         , requestCreateManagedPrefixList $
--             newCreateManagedPrefixList
--
--         , requestCreateReplaceRootVolumeTask $
--             newCreateReplaceRootVolumeTask
--
--         , requestAssociateIamInstanceProfile $
--             newAssociateIamInstanceProfile
--
--         , requestModifyDefaultCreditSpecification $
--             newModifyDefaultCreditSpecification
--
--         , requestDeleteEgressOnlyInternetGateway $
--             newDeleteEgressOnlyInternetGateway
--
--         , requestPurchaseHostReservation $
--             newPurchaseHostReservation
--
--         , requestModifyTransitGatewayVpcAttachment $
--             newModifyTransitGatewayVpcAttachment
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestDescribeClassicLinkInstances $
--             newDescribeClassicLinkInstances
--
--         , requestTerminateInstances $
--             newTerminateInstances
--
--         , requestDescribeStoreImageTasks $
--             newDescribeStoreImageTasks
--
--         , requestGetVpnConnectionDeviceTypes $
--             newGetVpnConnectionDeviceTypes
--
--         , requestGetTransitGatewayPrefixListReferences $
--             newGetTransitGatewayPrefixListReferences
--
--         , requestDescribeKeyPairs $
--             newDescribeKeyPairs
--
--         , requestDisableFastSnapshotRestores $
--             newDisableFastSnapshotRestores
--
--         , requestDescribeLaunchTemplates $
--             newDescribeLaunchTemplates
--
--         , requestCreateVpnConnectionRoute $
--             newCreateVpnConnectionRoute
--
--         , requestAssociateRouteTable $
--             newAssociateRouteTable
--
--         , requestCreateSubnetCidrReservation $
--             newCreateSubnetCidrReservation
--
--         , requestDescribeVpnGateways $
--             newDescribeVpnGateways
--
--         , requestModifyVpnConnectionOptions $
--             newModifyVpnConnectionOptions
--
--         , requestGetConsoleOutput $
--             newGetConsoleOutput
--
--         , requestDescribeHosts $
--             newDescribeHosts
--
--         , requestDescribeImageAttribute $
--             newDescribeImageAttribute
--
--         , requestModifyIdFormat $
--             newModifyIdFormat
--
--         , requestRegisterTransitGatewayMulticastGroupMembers $
--             newRegisterTransitGatewayMulticastGroupMembers
--
--         , requestDeleteManagedPrefixList $
--             newDeleteManagedPrefixList
--
--         , requestDeleteRouteTable $
--             newDeleteRouteTable
--
--         , requestResetImageAttribute $
--             newResetImageAttribute
--
--         , requestModifyTransitGatewayPrefixListReference $
--             newModifyTransitGatewayPrefixListReference
--
--         , requestDescribeTransitGatewayRouteTables $
--             newDescribeTransitGatewayRouteTables
--
--         , requestCreateEgressOnlyInternetGateway $
--             newCreateEgressOnlyInternetGateway
--
--         , requestDescribeReservedInstancesModifications $
--             newDescribeReservedInstancesModifications
--
--         , requestDescribeSpotInstanceRequests $
--             newDescribeSpotInstanceRequests
--
--         , requestRevokeClientVpnIngress $
--             newRevokeClientVpnIngress
--
--         , requestUnassignPrivateIpAddresses $
--             newUnassignPrivateIpAddresses
--
--         , requestDescribeNetworkInterfacePermissions $
--             newDescribeNetworkInterfacePermissions
--
--         , requestEnableFastSnapshotRestores $
--             newEnableFastSnapshotRestores
--
--         , requestDescribeVpcEndpointServicePermissions $
--             newDescribeVpcEndpointServicePermissions
--
--         , requestDeleteDhcpOptions $
--             newDeleteDhcpOptions
--
--         , requestCreateRestoreImageTask $
--             newCreateRestoreImageTask
--
--         , requestRegisterInstanceEventNotificationAttributes $
--             newRegisterInstanceEventNotificationAttributes
--
--         , requestGetVpnConnectionDeviceSampleConfiguration $
--             newGetVpnConnectionDeviceSampleConfiguration
--
--         , requestDeleteSubnetCidrReservation $
--             newDeleteSubnetCidrReservation
--
--         , requestDescribeNetworkAcls $
--             newDescribeNetworkAcls
--
--         , requestCancelImportTask $
--             newCancelImportTask
--
--         , requestDetachClassicLinkVpc $
--             newDetachClassicLinkVpc
--
--         , requestCreateCapacityReservationFleet $
--             newCreateCapacityReservationFleet
--
--         , requestDescribeRegions $
--             newDescribeRegions
--
--         , requestMonitorInstances $
--             newMonitorInstances
--
--         , requestRejectTransitGatewayMulticastDomainAssociations $
--             newRejectTransitGatewayMulticastDomainAssociations
--
--         , requestAcceptTransitGatewayMulticastDomainAssociations $
--             newAcceptTransitGatewayMulticastDomainAssociations
--
--         , requestSearchLocalGatewayRoutes $
--             newSearchLocalGatewayRoutes
--
--         , requestDeleteClientVpnRoute $
--             newDeleteClientVpnRoute
--
--         , requestAcceptVpcPeeringConnection $
--             newAcceptVpcPeeringConnection
--
--         , requestImportSnapshot $
--             newImportSnapshot
--
--         , requestDescribeAddressesAttribute $
--             newDescribeAddressesAttribute
--
--         , requestDescribeVolumeStatus $
--             newDescribeVolumeStatus
--
--         , requestDescribeReplaceRootVolumeTasks $
--             newDescribeReplaceRootVolumeTasks
--
--         , requestModifyInstanceEventWindow $
--             newModifyInstanceEventWindow
--
--         , requestDescribeRouteTables $
--             newDescribeRouteTables
--
--         , requestDescribeAvailabilityZones $
--             newDescribeAvailabilityZones
--
--         , requestModifyVpcAttribute $
--             newModifyVpcAttribute
--
--         , requestDescribeClientVpnConnections $
--             newDescribeClientVpnConnections
--
--         , requestDescribeFleetHistory $
--             newDescribeFleetHistory
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestDescribeElasticGpus $
--             newDescribeElasticGpus
--
--         , requestDisassociateTrunkInterface $
--             newDisassociateTrunkInterface
--
--         , requestRestoreAddressToClassic $
--             newRestoreAddressToClassic
--
--         , requestDescribeManagedPrefixLists $
--             newDescribeManagedPrefixLists
--
--         , requestCreateKeyPair $
--             newCreateKeyPair
--
--         , requestGetReservedInstancesExchangeQuote $
--             newGetReservedInstancesExchangeQuote
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--         , requestDeprovisionByoipCidr $
--             newDeprovisionByoipCidr
--
--         , requestGetSerialConsoleAccessStatus $
--             newGetSerialConsoleAccessStatus
--
--         , requestDeleteVpcEndpointServiceConfigurations $
--             newDeleteVpcEndpointServiceConfigurations
--
--         , requestDescribeSpotFleetInstances $
--             newDescribeSpotFleetInstances
--
--           ]

--     , testGroup "response"
--         [ responseModifyCapacityReservation $
--             newModifyCapacityReservationResponse
--
--         , responseGetAssociatedIpv6PoolCidrs $
--             newGetAssociatedIpv6PoolCidrsResponse
--
--         , responseImportInstance $
--             newImportInstanceResponse
--
--         , responseDescribeCapacityReservationFleets $
--             newDescribeCapacityReservationFleetsResponse
--
--         , responseModifySecurityGroupRules $
--             newModifySecurityGroupRulesResponse
--
--         , responseRevokeSecurityGroupEgress $
--             newRevokeSecurityGroupEgressResponse
--
--         , responseCreateNetworkInterfacePermission $
--             newCreateNetworkInterfacePermissionResponse
--
--         , responseSendDiagnosticInterrupt $
--             newSendDiagnosticInterruptResponse
--
--         , responseDeleteLaunchTemplate $
--             newDeleteLaunchTemplateResponse
--
--         , responseRejectVpcEndpointConnections $
--             newRejectVpcEndpointConnectionsResponse
--
--         , responseCreateVpnGateway $
--             newCreateVpnGatewayResponse
--
--         , responseCreateNetworkAcl $
--             newCreateNetworkAclResponse
--
--         , responseDeleteKeyPair $
--             newDeleteKeyPairResponse
--
--         , responseDescribeSecurityGroupReferences $
--             newDescribeSecurityGroupReferencesResponse
--
--         , responseDeleteFleets $
--             newDeleteFleetsResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseCreateTransitGatewayRouteTable $
--             newCreateTransitGatewayRouteTableResponse
--
--         , responseModifyInstanceMetadataOptions $
--             newModifyInstanceMetadataOptionsResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsIngress $
--             newUpdateSecurityGroupRuleDescriptionsIngressResponse
--
--         , responseDisassociateSubnetCidrBlock $
--             newDisassociateSubnetCidrBlockResponse
--
--         , responseDetachNetworkInterface $
--             newDetachNetworkInterfaceResponse
--
--         , responseDetachInternetGateway $
--             newDetachInternetGatewayResponse
--
--         , responseDeleteVpcEndpoints $
--             newDeleteVpcEndpointsResponse
--
--         , responseDescribeClientVpnEndpoints $
--             newDescribeClientVpnEndpointsResponse
--
--         , responseDeleteFlowLogs $
--             newDeleteFlowLogsResponse
--
--         , responseDescribeVpcClassicLink $
--             newDescribeVpcClassicLinkResponse
--
--         , responseGetAssociatedEnclaveCertificateIamRoles $
--             newGetAssociatedEnclaveCertificateIamRolesResponse
--
--         , responseAssociateTransitGatewayMulticastDomain $
--             newAssociateTransitGatewayMulticastDomainResponse
--
--         , responseModifySubnetAttribute $
--             newModifySubnetAttributeResponse
--
--         , responseDetachVolume $
--             newVolumeAttachment
--
--         , responseDescribeInstanceCreditSpecifications $
--             newDescribeInstanceCreditSpecificationsResponse
--
--         , responseCancelBundleTask $
--             newCancelBundleTaskResponse
--
--         , responseDescribeByoipCidrs $
--             newDescribeByoipCidrsResponse
--
--         , responseAcceptReservedInstancesExchangeQuote $
--             newAcceptReservedInstancesExchangeQuoteResponse
--
--         , responseReleaseAddress $
--             newReleaseAddressResponse
--
--         , responseDescribeInstanceTypeOfferings $
--             newDescribeInstanceTypeOfferingsResponse
--
--         , responseCreateInternetGateway $
--             newCreateInternetGatewayResponse
--
--         , responseDeleteVpnConnection $
--             newDeleteVpnConnectionResponse
--
--         , responseDescribeBundleTasks $
--             newDescribeBundleTasksResponse
--
--         , responseAuthorizeSecurityGroupEgress $
--             newAuthorizeSecurityGroupEgressResponse
--
--         , responseEnableTransitGatewayRouteTablePropagation $
--             newEnableTransitGatewayRouteTablePropagationResponse
--
--         , responseDeregisterImage $
--             newDeregisterImageResponse
--
--         , responseDeleteVpcEndpointConnectionNotifications $
--             newDeleteVpcEndpointConnectionNotificationsResponse
--
--         , responseDescribeCoipPools $
--             newDescribeCoipPoolsResponse
--
--         , responseResetAddressAttribute $
--             newResetAddressAttributeResponse
--
--         , responseGetTransitGatewayMulticastDomainAssociations $
--             newGetTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseDeleteLocalGatewayRouteTableVpcAssociation $
--             newDeleteLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseModifyNetworkInterfaceAttribute $
--             newModifyNetworkInterfaceAttributeResponse
--
--         , responseModifyVpcTenancy $
--             newModifyVpcTenancyResponse
--
--         , responseDescribeInstanceTypes $
--             newDescribeInstanceTypesResponse
--
--         , responseCancelCapacityReservationFleets $
--             newCancelCapacityReservationFleetsResponse
--
--         , responseDescribeClientVpnAuthorizationRules $
--             newDescribeClientVpnAuthorizationRulesResponse
--
--         , responseDeleteTransitGatewayVpcAttachment $
--             newDeleteTransitGatewayVpcAttachmentResponse
--
--         , responseDeleteTransitGatewayMulticastDomain $
--             newDeleteTransitGatewayMulticastDomainResponse
--
--         , responseCancelReservedInstancesListing $
--             newCancelReservedInstancesListingResponse
--
--         , responseAttachClassicLinkVpc $
--             newAttachClassicLinkVpcResponse
--
--         , responseDisableTransitGatewayRouteTablePropagation $
--             newDisableTransitGatewayRouteTablePropagationResponse
--
--         , responseDescribeVpcClassicLinkDnsSupport $
--             newDescribeVpcClassicLinkDnsSupportResponse
--
--         , responseAssociateSubnetCidrBlock $
--             newAssociateSubnetCidrBlockResponse
--
--         , responseCreateNetworkInsightsPath $
--             newCreateNetworkInsightsPathResponse
--
--         , responseRunScheduledInstances $
--             newRunScheduledInstancesResponse
--
--         , responseCreateTransitGatewayRoute $
--             newCreateTransitGatewayRouteResponse
--
--         , responseCreateTransitGatewayPrefixListReference $
--             newCreateTransitGatewayPrefixListReferenceResponse
--
--         , responseCancelSpotFleetRequests $
--             newCancelSpotFleetRequestsResponse
--
--         , responseModifyCapacityReservationFleet $
--             newModifyCapacityReservationFleetResponse
--
--         , responseDescribeSpotPriceHistory $
--             newDescribeSpotPriceHistoryResponse
--
--         , responseDeleteTransitGatewayConnectPeer $
--             newDeleteTransitGatewayConnectPeerResponse
--
--         , responseDescribeDhcpOptions $
--             newDescribeDhcpOptionsResponse
--
--         , responseImportImage $
--             newImportImageResponse
--
--         , responseCreateLocalGatewayRouteTableVpcAssociation $
--             newCreateLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseCopyFpgaImage $
--             newCopyFpgaImageResponse
--
--         , responseImportClientVpnClientCertificateRevocationList $
--             newImportClientVpnClientCertificateRevocationListResponse
--
--         , responseStopInstances $
--             newStopInstancesResponse
--
--         , responseEnableEbsEncryptionByDefault $
--             newEnableEbsEncryptionByDefaultResponse
--
--         , responseModifyAddressAttribute $
--             newModifyAddressAttributeResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupSources $
--             newDeregisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseModifyLaunchTemplate $
--             newModifyLaunchTemplateResponse
--
--         , responseModifyVpcEndpointConnectionNotification $
--             newModifyVpcEndpointConnectionNotificationResponse
--
--         , responseDescribeInternetGateways $
--             newDescribeInternetGatewaysResponse
--
--         , responseDisableVpcClassicLink $
--             newDisableVpcClassicLinkResponse
--
--         , responseGetGroupsForCapacityReservation $
--             newGetGroupsForCapacityReservationResponse
--
--         , responseDeleteLaunchTemplateVersions $
--             newDeleteLaunchTemplateVersionsResponse
--
--         , responseBundleInstance $
--             newBundleInstanceResponse
--
--         , responseDescribeNetworkInterfaces $
--             newDescribeNetworkInterfacesResponse
--
--         , responseReplaceNetworkAclAssociation $
--             newReplaceNetworkAclAssociationResponse
--
--         , responseAssociateInstanceEventWindow $
--             newAssociateInstanceEventWindowResponse
--
--         , responseDescribeNatGateways $
--             newDescribeNatGatewaysResponse
--
--         , responseDescribeAddresses $
--             newDescribeAddressesResponse
--
--         , responseRestoreManagedPrefixListVersion $
--             newRestoreManagedPrefixListVersionResponse
--
--         , responseDescribeSnapshotAttribute $
--             newDescribeSnapshotAttributeResponse
--
--         , responseDescribeIdentityIdFormat $
--             newDescribeIdentityIdFormatResponse
--
--         , responseReplaceRoute $
--             newReplaceRouteResponse
--
--         , responseDescribeVpcEndpointServices $
--             newDescribeVpcEndpointServicesResponse
--
--         , responseDeleteLocalGatewayRoute $
--             newDeleteLocalGatewayRouteResponse
--
--         , responseAuthorizeSecurityGroupIngress $
--             newAuthorizeSecurityGroupIngressResponse
--
--         , responseCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnectionResponse
--
--         , responseDescribeSubnets $
--             newDescribeSubnetsResponse
--
--         , responseGetTransitGatewayAttachmentPropagations $
--             newGetTransitGatewayAttachmentPropagationsResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responsePurchaseReservedInstancesOffering $
--             newPurchaseReservedInstancesOfferingResponse
--
--         , responseDeleteNetworkAclEntry $
--             newDeleteNetworkAclEntryResponse
--
--         , responseResetSnapshotAttribute $
--             newResetSnapshotAttributeResponse
--
--         , responseDescribeVpnConnections $
--             newDescribeVpnConnectionsResponse
--
--         , responseModifyInstanceEventStartTime $
--             newModifyInstanceEventStartTimeResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse
--
--         , responseReplaceNetworkAclEntry $
--             newReplaceNetworkAclEntryResponse
--
--         , responseDeleteInstanceEventWindow $
--             newDeleteInstanceEventWindowResponse
--
--         , responseDescribeVpcEndpoints $
--             newDescribeVpcEndpointsResponse
--
--         , responseCreateTrafficMirrorFilter $
--             newCreateTrafficMirrorFilterResponse
--
--         , responseResetInstanceAttribute $
--             newResetInstanceAttributeResponse
--
--         , responseModifyIdentityIdFormat $
--             newModifyIdentityIdFormatResponse
--
--         , responseAttachNetworkInterface $
--             newAttachNetworkInterfaceResponse
--
--         , responseCreateCapacityReservation $
--             newCreateCapacityReservationResponse
--
--         , responseDescribeInstanceStatus $
--             newDescribeInstanceStatusResponse
--
--         , responseImportKeyPair $
--             newImportKeyPairResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseConfirmProductInstance $
--             newConfirmProductInstanceResponse
--
--         , responseDescribeInstanceAttribute $
--             newDescribeInstanceAttributeResponse
--
--         , responseDescribeReservedInstancesOfferings $
--             newDescribeReservedInstancesOfferingsResponse
--
--         , responseCreateCustomerGateway $
--             newCreateCustomerGatewayResponse
--
--         , responseDescribeNetworkInsightsAnalyses $
--             newDescribeNetworkInsightsAnalysesResponse
--
--         , responseDescribeFleets $
--             newDescribeFleetsResponse
--
--         , responseDeleteNetworkInsightsAnalysis $
--             newDeleteNetworkInsightsAnalysisResponse
--
--         , responseCreateTransitGatewayPeeringAttachment $
--             newCreateTransitGatewayPeeringAttachmentResponse
--
--         , responseDeleteSecurityGroup $
--             newDeleteSecurityGroupResponse
--
--         , responseDescribePublicIpv4Pools $
--             newDescribePublicIpv4PoolsResponse
--
--         , responseDescribeClientVpnTargetNetworks $
--             newDescribeClientVpnTargetNetworksResponse
--
--         , responseDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnectionResponse
--
--         , responseAttachInternetGateway $
--             newAttachInternetGatewayResponse
--
--         , responseModifyInstancePlacement $
--             newModifyInstancePlacementResponse
--
--         , responseDescribeFlowLogs $
--             newDescribeFlowLogsResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaceGroups $
--             newDescribeLocalGatewayVirtualInterfaceGroupsResponse
--
--         , responseDeleteTransitGatewayConnect $
--             newDeleteTransitGatewayConnectResponse
--
--         , responseDescribeLocalGatewayRouteTableVpcAssociations $
--             newDescribeLocalGatewayRouteTableVpcAssociationsResponse
--
--         , responseDescribeVpcEndpointConnectionNotifications $
--             newDescribeVpcEndpointConnectionNotificationsResponse
--
--         , responseGetManagedPrefixListEntries $
--             newGetManagedPrefixListEntriesResponse
--
--         , responseDisassociateInstanceEventWindow $
--             newDisassociateInstanceEventWindowResponse
--
--         , responseRunInstances $
--             newReservation
--
--         , responseCreateSnapshots $
--             newCreateSnapshotsResponse
--
--         , responseAssociateDhcpOptions $
--             newAssociateDhcpOptionsResponse
--
--         , responseDeleteTrafficMirrorFilterRule $
--             newDeleteTrafficMirrorFilterRuleResponse
--
--         , responseDescribeReservedInstances $
--             newDescribeReservedInstancesResponse
--
--         , responseDescribeIdFormat $
--             newDescribeIdFormatResponse
--
--         , responseDescribeVpcs $
--             newDescribeVpcsResponse
--
--         , responseDescribeConversionTasks $
--             newDescribeConversionTasksResponse
--
--         , responseDisableImageDeprecation $
--             newDisableImageDeprecationResponse
--
--         , responseCreateLaunchTemplateVersion $
--             newCreateLaunchTemplateVersionResponse
--
--         , responseGetManagedPrefixListAssociations $
--             newGetManagedPrefixListAssociationsResponse
--
--         , responseDisableVpcClassicLinkDnsSupport $
--             newDisableVpcClassicLinkDnsSupportResponse
--
--         , responseApplySecurityGroupsToClientVpnTargetNetwork $
--             newApplySecurityGroupsToClientVpnTargetNetworkResponse
--
--         , responseDescribeTrafficMirrorTargets $
--             newDescribeTrafficMirrorTargetsResponse
--
--         , responseDescribeVolumesModifications $
--             newDescribeVolumesModificationsResponse
--
--         , responseExportImage $
--             newExportImageResponse
--
--         , responseCreateFpgaImage $
--             newCreateFpgaImageResponse
--
--         , responseAcceptVpcEndpointConnections $
--             newAcceptVpcEndpointConnectionsResponse
--
--         , responseDeleteClientVpnEndpoint $
--             newDeleteClientVpnEndpointResponse
--
--         , responseSearchTransitGatewayRoutes $
--             newSearchTransitGatewayRoutesResponse
--
--         , responseGetLaunchTemplateData $
--             newGetLaunchTemplateDataResponse
--
--         , responseAllocateAddress $
--             newAllocateAddressResponse
--
--         , responseAcceptTransitGatewayVpcAttachment $
--             newAcceptTransitGatewayVpcAttachmentResponse
--
--         , responseCancelConversionTask $
--             newCancelConversionTaskResponse
--
--         , responseModifyImageAttribute $
--             newModifyImageAttributeResponse
--
--         , responseCreateRouteTable $
--             newCreateRouteTableResponse
--
--         , responseRejectTransitGatewayPeeringAttachment $
--             newRejectTransitGatewayPeeringAttachmentResponse
--
--         , responseReportInstanceStatus $
--             newReportInstanceStatusResponse
--
--         , responseAttachVolume $
--             newVolumeAttachment
--
--         , responseRequestSpotInstances $
--             newRequestSpotInstancesResponse
--
--         , responseWithdrawByoipCidr $
--             newWithdrawByoipCidrResponse
--
--         , responseDescribeHostReservationOfferings $
--             newDescribeHostReservationOfferingsResponse
--
--         , responseResetFpgaImageAttribute $
--             newResetFpgaImageAttributeResponse
--
--         , responseModifyVpnConnection $
--             newModifyVpnConnectionResponse
--
--         , responseCreateTrafficMirrorFilterRule $
--             newCreateTrafficMirrorFilterRuleResponse
--
--         , responseDeleteTransitGateway $
--             newDeleteTransitGatewayResponse
--
--         , responseStartVpcEndpointServicePrivateDnsVerification $
--             newStartVpcEndpointServicePrivateDnsVerificationResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseRejectVpcPeeringConnection $
--             newRejectVpcPeeringConnectionResponse
--
--         , responseDescribeClientVpnRoutes $
--             newDescribeClientVpnRoutesResponse
--
--         , responseDeleteVpnConnectionRoute $
--             newDeleteVpnConnectionRouteResponse
--
--         , responseAssociateEnclaveCertificateIamRole $
--             newAssociateEnclaveCertificateIamRoleResponse
--
--         , responseModifyVpcEndpoint $
--             newModifyVpcEndpointResponse
--
--         , responseDescribeFpgaImageAttribute $
--             newDescribeFpgaImageAttributeResponse
--
--         , responseAllocateHosts $
--             newAllocateHostsResponse
--
--         , responseCreateClientVpnEndpoint $
--             newCreateClientVpnEndpointResponse
--
--         , responseCreateTrafficMirrorSession $
--             newCreateTrafficMirrorSessionResponse
--
--         , responseRegisterImage $
--             newRegisterImageResponse
--
--         , responseAdvertiseByoipCidr $
--             newAdvertiseByoipCidrResponse
--
--         , responseModifyFleet $
--             newModifyFleetResponse
--
--         , responseRevokeSecurityGroupIngress $
--             newRevokeSecurityGroupIngressResponse
--
--         , responseGetEbsDefaultKmsKeyId $
--             newGetEbsDefaultKmsKeyIdResponse
--
--         , responseDescribeHostReservations $
--             newDescribeHostReservationsResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsEgress $
--             newUpdateSecurityGroupRuleDescriptionsEgressResponse
--
--         , responseEnableVpcClassicLinkDnsSupport $
--             newEnableVpcClassicLinkDnsSupportResponse
--
--         , responseDescribeVpcEndpointConnections $
--             newDescribeVpcEndpointConnectionsResponse
--
--         , responseModifyReservedInstances $
--             newModifyReservedInstancesResponse
--
--         , responseDeleteFpgaImage $
--             newDeleteFpgaImageResponse
--
--         , responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
--
--         , responseEnableImageDeprecation $
--             newEnableImageDeprecationResponse
--
--         , responseDescribeScheduledInstances $
--             newDescribeScheduledInstancesResponse
--
--         , responseSearchTransitGatewayMulticastGroups $
--             newSearchTransitGatewayMulticastGroupsResponse
--
--         , responseCreateFlowLogs $
--             newCreateFlowLogsResponse
--
--         , responseDescribeSpotFleetRequests $
--             newDescribeSpotFleetRequestsResponse
--
--         , responseMoveAddressToVpc $
--             newMoveAddressToVpcResponse
--
--         , responseDescribeFleetInstances $
--             newDescribeFleetInstancesResponse
--
--         , responseDescribeLaunchTemplateVersions $
--             newDescribeLaunchTemplateVersionsResponse
--
--         , responseStartNetworkInsightsAnalysis $
--             newStartNetworkInsightsAnalysisResponse
--
--         , responseModifyInstanceCreditSpecification $
--             newModifyInstanceCreditSpecificationResponse
--
--         , responseDescribePrincipalIdFormat $
--             newDescribePrincipalIdFormatResponse
--
--         , responseDescribeTransitGateways $
--             newDescribeTransitGatewaysResponse
--
--         , responseDeleteNetworkAcl $
--             newDeleteNetworkAclResponse
--
--         , responseDisassociateTransitGatewayMulticastDomain $
--             newDisassociateTransitGatewayMulticastDomainResponse
--
--         , responseDeleteTransitGatewayRouteTable $
--             newDeleteTransitGatewayRouteTableResponse
--
--         , responseDescribeSecurityGroupRules $
--             newDescribeSecurityGroupRulesResponse
--
--         , responseCreateLaunchTemplate $
--             newCreateLaunchTemplateResponse
--
--         , responseCreateVpcEndpointConnectionNotification $
--             newCreateVpcEndpointConnectionNotificationResponse
--
--         , responseDeleteNetworkInterfacePermission $
--             newDeleteNetworkInterfacePermissionResponse
--
--         , responseDeleteVpnGateway $
--             newDeleteVpnGatewayResponse
--
--         , responseCreateStoreImageTask $
--             newCreateStoreImageTaskResponse
--
--         , responseCreateTrafficMirrorTarget $
--             newCreateTrafficMirrorTargetResponse
--
--         , responseDescribeImportImageTasks $
--             newDescribeImportImageTasksResponse
--
--         , responseDescribeVolumeAttribute $
--             newDescribeVolumeAttributeResponse
--
--         , responseDescribeMovingAddresses $
--             newDescribeMovingAddressesResponse
--
--         , responseExportTransitGatewayRoutes $
--             newExportTransitGatewayRoutesResponse
--
--         , responseGetPasswordData $
--             newGetPasswordDataResponse
--
--         , responseCreateVpc $
--             newCreateVpcResponse
--
--         , responseModifyVpcPeeringConnectionOptions $
--             newModifyVpcPeeringConnectionOptionsResponse
--
--         , responseDescribeFpgaImages $
--             newDescribeFpgaImagesResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseAcceptTransitGatewayPeeringAttachment $
--             newAcceptTransitGatewayPeeringAttachmentResponse
--
--         , responseDisassociateAddress $
--             newDisassociateAddressResponse
--
--         , responseModifyTrafficMirrorFilterNetworkServices $
--             newModifyTrafficMirrorFilterNetworkServicesResponse
--
--         , responseDescribeEgressOnlyInternetGateways $
--             newDescribeEgressOnlyInternetGatewaysResponse
--
--         , responseDeleteVpc $
--             newDeleteVpcResponse
--
--         , responseCreateInstanceExportTask $
--             newCreateInstanceExportTaskResponse
--
--         , responseRejectTransitGatewayVpcAttachment $
--             newRejectTransitGatewayVpcAttachmentResponse
--
--         , responseDescribeTrafficMirrorSessions $
--             newDescribeTrafficMirrorSessionsResponse
--
--         , responseGetTransitGatewayRouteTableAssociations $
--             newGetTransitGatewayRouteTableAssociationsResponse
--
--         , responseAssociateVpcCidrBlock $
--             newAssociateVpcCidrBlockResponse
--
--         , responseDescribeVpcAttribute $
--             newDescribeVpcAttributeResponse
--
--         , responseCreateVolume $
--             newVolume
--
--         , responseCreateDefaultSubnet $
--             newCreateDefaultSubnetResponse
--
--         , responseDescribeScheduledInstanceAvailability $
--             newDescribeScheduledInstanceAvailabilityResponse
--
--         , responseDisassociateClientVpnTargetNetwork $
--             newDisassociateClientVpnTargetNetworkResponse
--
--         , responseCreateClientVpnRoute $
--             newCreateClientVpnRouteResponse
--
--         , responseModifyVolumeAttribute $
--             newModifyVolumeAttributeResponse
--
--         , responseExportClientVpnClientConfiguration $
--             newExportClientVpnClientConfigurationResponse
--
--         , responseDescribeTrunkInterfaceAssociations $
--             newDescribeTrunkInterfaceAssociationsResponse
--
--         , responseDeleteTrafficMirrorTarget $
--             newDeleteTrafficMirrorTargetResponse
--
--         , responseDescribeSpotDatafeedSubscription $
--             newDescribeSpotDatafeedSubscriptionResponse
--
--         , responseDescribeLocalGatewayRouteTables $
--             newDescribeLocalGatewayRouteTablesResponse
--
--         , responseDescribePrefixLists $
--             newDescribePrefixListsResponse
--
--         , responseAssociateTransitGatewayRouteTable $
--             newAssociateTransitGatewayRouteTableResponse
--
--         , responseDeletePlacementGroup $
--             newDeletePlacementGroupResponse
--
--         , responseModifyTransitGateway $
--             newModifyTransitGatewayResponse
--
--         , responseDeleteTransitGatewayPrefixListReference $
--             newDeleteTransitGatewayPrefixListReferenceResponse
--
--         , responseCreateTransitGatewayMulticastDomain $
--             newCreateTransitGatewayMulticastDomainResponse
--
--         , responseDeregisterInstanceEventNotificationAttributes $
--             newDeregisterInstanceEventNotificationAttributesResponse
--
--         , responseRequestSpotFleet $
--             newRequestSpotFleetResponse
--
--         , responseDeleteNetworkInsightsPath $
--             newDeleteNetworkInsightsPathResponse
--
--         , responseDescribeTransitGatewayConnects $
--             newDescribeTransitGatewayConnectsResponse
--
--         , responseDeleteTransitGatewayRoute $
--             newDeleteTransitGatewayRouteResponse
--
--         , responseCreateTransitGatewayConnectPeer $
--             newCreateTransitGatewayConnectPeerResponse
--
--         , responseDisableEbsEncryptionByDefault $
--             newDisableEbsEncryptionByDefaultResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupMembers $
--             newDeregisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseAssociateTrunkInterface $
--             newAssociateTrunkInterfaceResponse
--
--         , responseCreateSubnet $
--             newCreateSubnetResponse
--
--         , responseCreateNetworkInterface $
--             newCreateNetworkInterfaceResponse
--
--         , responseDescribeSecurityGroups $
--             newDescribeSecurityGroupsResponse
--
--         , responseGetCapacityReservationUsage $
--             newGetCapacityReservationUsageResponse
--
--         , responseCreateTransitGatewayVpcAttachment $
--             newCreateTransitGatewayVpcAttachmentResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseModifySpotFleetRequest $
--             newModifySpotFleetRequestResponse
--
--         , responseDetachVpnGateway $
--             newDetachVpnGatewayResponse
--
--         , responseModifyManagedPrefixList $
--             newModifyManagedPrefixListResponse
--
--         , responseGetHostReservationPurchasePreview $
--             newGetHostReservationPurchasePreviewResponse
--
--         , responseEnableVolumeIO $
--             newEnableVolumeIOResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDescribeInstanceEventWindows $
--             newDescribeInstanceEventWindowsResponse
--
--         , responseDisableSerialConsoleAccess $
--             newDisableSerialConsoleAccessResponse
--
--         , responseCreateNatGateway $
--             newCreateNatGatewayResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaces $
--             newDescribeLocalGatewayVirtualInterfacesResponse
--
--         , responseDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnectionsResponse
--
--         , responseCancelExportTask $
--             newCancelExportTaskResponse
--
--         , responseCreateVpcEndpointServiceConfiguration $
--             newCreateVpcEndpointServiceConfigurationResponse
--
--         , responseCreateDefaultVpc $
--             newCreateDefaultVpcResponse
--
--         , responseDisassociateVpcCidrBlock $
--             newDisassociateVpcCidrBlockResponse
--
--         , responseDescribeTrafficMirrorFilters $
--             newDescribeTrafficMirrorFiltersResponse
--
--         , responseDescribeFastSnapshotRestores $
--             newDescribeFastSnapshotRestoresResponse
--
--         , responseCancelCapacityReservation $
--             newCancelCapacityReservationResponse
--
--         , responseDeleteNetworkInterface $
--             newDeleteNetworkInterfaceResponse
--
--         , responseDisassociateTransitGatewayRouteTable $
--             newDisassociateTransitGatewayRouteTableResponse
--
--         , responseReplaceRouteTableAssociation $
--             newReplaceRouteTableAssociationResponse
--
--         , responseStartInstances $
--             newStartInstancesResponse
--
--         , responseCreatePlacementGroup $
--             newCreatePlacementGroupResponse
--
--         , responseDescribeInstanceEventNotificationAttributes $
--             newDescribeInstanceEventNotificationAttributesResponse
--
--         , responseDescribeCapacityReservations $
--             newDescribeCapacityReservationsResponse
--
--         , responseModifyClientVpnEndpoint $
--             newModifyClientVpnEndpointResponse
--
--         , responseModifyInstanceCapacityReservationAttributes $
--             newModifyInstanceCapacityReservationAttributesResponse
--
--         , responseDescribeAggregateIdFormat $
--             newDescribeAggregateIdFormatResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseGetSubnetCidrReservations $
--             newGetSubnetCidrReservationsResponse
--
--         , responseAssociateAddress $
--             newAssociateAddressResponse
--
--         , responseModifyTrafficMirrorFilterRule $
--             newModifyTrafficMirrorFilterRuleResponse
--
--         , responseDescribeNetworkInterfaceAttribute $
--             newDescribeNetworkInterfaceAttributeResponse
--
--         , responseReplaceIamInstanceProfileAssociation $
--             newReplaceIamInstanceProfileAssociationResponse
--
--         , responseAssociateClientVpnTargetNetwork $
--             newAssociateClientVpnTargetNetworkResponse
--
--         , responseReleaseHosts $
--             newReleaseHostsResponse
--
--         , responseEnableSerialConsoleAccess $
--             newEnableSerialConsoleAccessResponse
--
--         , responseResetNetworkInterfaceAttribute $
--             newResetNetworkInterfaceAttributeResponse
--
--         , responseDeleteInternetGateway $
--             newDeleteInternetGatewayResponse
--
--         , responseDescribeReservedInstancesListings $
--             newDescribeReservedInstancesListingsResponse
--
--         , responseCreateVpnConnection $
--             newCreateVpnConnectionResponse
--
--         , responseReplaceTransitGatewayRoute $
--             newReplaceTransitGatewayRouteResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseDeleteNatGateway $
--             newDeleteNatGatewayResponse
--
--         , responseDescribeImportSnapshotTasks $
--             newDescribeImportSnapshotTasksResponse
--
--         , responseGetCoipPoolUsage $
--             newGetCoipPoolUsageResponse
--
--         , responseDescribeCustomerGateways $
--             newDescribeCustomerGatewaysResponse
--
--         , responseDeleteSubnet $
--             newDeleteSubnetResponse
--
--         , responseCopyImage $
--             newCopyImageResponse
--
--         , responseCreateVpcEndpoint $
--             newCreateVpcEndpointResponse
--
--         , responseModifyTrafficMirrorSession $
--             newModifyTrafficMirrorSessionResponse
--
--         , responseDescribeCarrierGateways $
--             newDescribeCarrierGatewaysResponse
--
--         , responseDescribeTransitGatewayPeeringAttachments $
--             newDescribeTransitGatewayPeeringAttachmentsResponse
--
--         , responseDeleteQueuedReservedInstances $
--             newDeleteQueuedReservedInstancesResponse
--
--         , responseDescribeTransitGatewayMulticastDomains $
--             newDescribeTransitGatewayMulticastDomainsResponse
--
--         , responseGetDefaultCreditSpecification $
--             newGetDefaultCreditSpecificationResponse
--
--         , responseUnmonitorInstances $
--             newUnmonitorInstancesResponse
--
--         , responseDescribeTransitGatewayVpcAttachments $
--             newDescribeTransitGatewayVpcAttachmentsResponse
--
--         , responseDescribeTransitGatewayConnectPeers $
--             newDescribeTransitGatewayConnectPeersResponse
--
--         , responseCreateSecurityGroup $
--             newCreateSecurityGroupResponse
--
--         , responseCreateInstanceEventWindow $
--             newCreateInstanceEventWindowResponse
--
--         , responseGetEbsEncryptionByDefault $
--             newGetEbsEncryptionByDefaultResponse
--
--         , responseImportVolume $
--             newImportVolumeResponse
--
--         , responseDeleteCarrierGateway $
--             newDeleteCarrierGatewayResponse
--
--         , responseDisableVgwRoutePropagation $
--             newDisableVgwRoutePropagationResponse
--
--         , responseDeleteTrafficMirrorFilter $
--             newDeleteTrafficMirrorFilterResponse
--
--         , responseModifyVpnTunnelCertificate $
--             newModifyVpnTunnelCertificateResponse
--
--         , responseCreateSpotDatafeedSubscription $
--             newCreateSpotDatafeedSubscriptionResponse
--
--         , responseCancelSpotInstanceRequests $
--             newCancelSpotInstanceRequestsResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse
--
--         , responseDescribeVpcEndpointServiceConfigurations $
--             newDescribeVpcEndpointServiceConfigurationsResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseAssignPrivateIpAddresses $
--             newAssignPrivateIpAddressesResponse
--
--         , responseAuthorizeClientVpnIngress $
--             newAuthorizeClientVpnIngressResponse
--
--         , responseDeleteTransitGatewayPeeringAttachment $
--             newDeleteTransitGatewayPeeringAttachmentResponse
--
--         , responseModifyInstanceAttribute $
--             newModifyInstanceAttributeResponse
--
--         , responseDeleteCustomerGateway $
--             newDeleteCustomerGatewayResponse
--
--         , responseDisassociateIamInstanceProfile $
--             newDisassociateIamInstanceProfileResponse
--
--         , responseTerminateClientVpnConnections $
--             newTerminateClientVpnConnectionsResponse
--
--         , responseCreateTransitGatewayConnect $
--             newCreateTransitGatewayConnectResponse
--
--         , responseDisassociateRouteTable $
--             newDisassociateRouteTableResponse
--
--         , responseGetConsoleScreenshot $
--             newGetConsoleScreenshotResponse
--
--         , responseGetFlowLogsIntegrationTemplate $
--             newGetFlowLogsIntegrationTemplateResponse
--
--         , responseResetEbsDefaultKmsKeyId $
--             newResetEbsDefaultKmsKeyIdResponse
--
--         , responseAssignIpv6Addresses $
--             newAssignIpv6AddressesResponse
--
--         , responseModifyVpnTunnelOptions $
--             newModifyVpnTunnelOptionsResponse
--
--         , responseModifyEbsDefaultKmsKeyId $
--             newModifyEbsDefaultKmsKeyIdResponse
--
--         , responseDeleteSpotDatafeedSubscription $
--             newDeleteSpotDatafeedSubscriptionResponse
--
--         , responseModifyVolume $
--             newModifyVolumeResponse
--
--         , responseEnableVpcClassicLink $
--             newEnableVpcClassicLinkResponse
--
--         , responseDescribePlacementGroups $
--             newDescribePlacementGroupsResponse
--
--         , responseProvisionByoipCidr $
--             newProvisionByoipCidrResponse
--
--         , responseDisassociateEnclaveCertificateIamRole $
--             newDisassociateEnclaveCertificateIamRoleResponse
--
--         , responseModifyAvailabilityZoneGroup $
--             newModifyAvailabilityZoneGroupResponse
--
--         , responseDescribeStaleSecurityGroups $
--             newDescribeStaleSecurityGroupsResponse
--
--         , responseCreateCarrierGateway $
--             newCreateCarrierGatewayResponse
--
--         , responseDescribeExportImageTasks $
--             newDescribeExportImageTasksResponse
--
--         , responsePurchaseScheduledInstances $
--             newPurchaseScheduledInstancesResponse
--
--         , responseEnableVgwRoutePropagation $
--             newEnableVgwRoutePropagationResponse
--
--         , responseDescribeSpotFleetRequestHistory $
--             newDescribeSpotFleetRequestHistoryResponse
--
--         , responseModifySnapshotAttribute $
--             newModifySnapshotAttributeResponse
--
--         , responseDescribeIamInstanceProfileAssociations $
--             newDescribeIamInstanceProfileAssociationsResponse
--
--         , responseDescribeNetworkInsightsPaths $
--             newDescribeNetworkInsightsPathsResponse
--
--         , responseCreateSnapshot $
--             newSnapshot
--
--         , responseCreateLocalGatewayRoute $
--             newCreateLocalGatewayRouteResponse
--
--         , responseCreateNetworkAclEntry $
--             newCreateNetworkAclEntryResponse
--
--         , responseDescribeTransitGatewayAttachments $
--             newDescribeTransitGatewayAttachmentsResponse
--
--         , responseCreateReservedInstancesListing $
--             newCreateReservedInstancesListingResponse
--
--         , responseDescribeIpv6Pools $
--             newDescribeIpv6PoolsResponse
--
--         , responseAttachVpnGateway $
--             newAttachVpnGatewayResponse
--
--         , responseDescribeLocalGateways $
--             newDescribeLocalGatewaysResponse
--
--         , responseModifyVpcEndpointServicePermissions $
--             newModifyVpcEndpointServicePermissionsResponse
--
--         , responseExportClientVpnClientCertificateRevocationList $
--             newExportClientVpnClientCertificateRevocationListResponse
--
--         , responseCreateDhcpOptions $
--             newCreateDhcpOptionsResponse
--
--         , responseRegisterTransitGatewayMulticastGroupSources $
--             newRegisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseGetTransitGatewayRouteTablePropagations $
--             newGetTransitGatewayRouteTablePropagationsResponse
--
--         , responseModifyFpgaImageAttribute $
--             newModifyFpgaImageAttributeResponse
--
--         , responseModifyHosts $
--             newModifyHostsResponse
--
--         , responseRebootInstances $
--             newRebootInstancesResponse
--
--         , responseModifyVpcEndpointServiceConfiguration $
--             newModifyVpcEndpointServiceConfigurationResponse
--
--         , responseCreateTransitGateway $
--             newCreateTransitGatewayResponse
--
--         , responseUnassignIpv6Addresses $
--             newUnassignIpv6AddressesResponse
--
--         , responseDeleteTrafficMirrorSession $
--             newDeleteTrafficMirrorSessionResponse
--
--         , responseCreateManagedPrefixList $
--             newCreateManagedPrefixListResponse
--
--         , responseCreateReplaceRootVolumeTask $
--             newCreateReplaceRootVolumeTaskResponse
--
--         , responseAssociateIamInstanceProfile $
--             newAssociateIamInstanceProfileResponse
--
--         , responseModifyDefaultCreditSpecification $
--             newModifyDefaultCreditSpecificationResponse
--
--         , responseDeleteEgressOnlyInternetGateway $
--             newDeleteEgressOnlyInternetGatewayResponse
--
--         , responsePurchaseHostReservation $
--             newPurchaseHostReservationResponse
--
--         , responseModifyTransitGatewayVpcAttachment $
--             newModifyTransitGatewayVpcAttachmentResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseDescribeClassicLinkInstances $
--             newDescribeClassicLinkInstancesResponse
--
--         , responseTerminateInstances $
--             newTerminateInstancesResponse
--
--         , responseDescribeStoreImageTasks $
--             newDescribeStoreImageTasksResponse
--
--         , responseGetVpnConnectionDeviceTypes $
--             newGetVpnConnectionDeviceTypesResponse
--
--         , responseGetTransitGatewayPrefixListReferences $
--             newGetTransitGatewayPrefixListReferencesResponse
--
--         , responseDescribeKeyPairs $
--             newDescribeKeyPairsResponse
--
--         , responseDisableFastSnapshotRestores $
--             newDisableFastSnapshotRestoresResponse
--
--         , responseDescribeLaunchTemplates $
--             newDescribeLaunchTemplatesResponse
--
--         , responseCreateVpnConnectionRoute $
--             newCreateVpnConnectionRouteResponse
--
--         , responseAssociateRouteTable $
--             newAssociateRouteTableResponse
--
--         , responseCreateSubnetCidrReservation $
--             newCreateSubnetCidrReservationResponse
--
--         , responseDescribeVpnGateways $
--             newDescribeVpnGatewaysResponse
--
--         , responseModifyVpnConnectionOptions $
--             newModifyVpnConnectionOptionsResponse
--
--         , responseGetConsoleOutput $
--             newGetConsoleOutputResponse
--
--         , responseDescribeHosts $
--             newDescribeHostsResponse
--
--         , responseDescribeImageAttribute $
--             newDescribeImageAttributeResponse
--
--         , responseModifyIdFormat $
--             newModifyIdFormatResponse
--
--         , responseRegisterTransitGatewayMulticastGroupMembers $
--             newRegisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseDeleteManagedPrefixList $
--             newDeleteManagedPrefixListResponse
--
--         , responseDeleteRouteTable $
--             newDeleteRouteTableResponse
--
--         , responseResetImageAttribute $
--             newResetImageAttributeResponse
--
--         , responseModifyTransitGatewayPrefixListReference $
--             newModifyTransitGatewayPrefixListReferenceResponse
--
--         , responseDescribeTransitGatewayRouteTables $
--             newDescribeTransitGatewayRouteTablesResponse
--
--         , responseCreateEgressOnlyInternetGateway $
--             newCreateEgressOnlyInternetGatewayResponse
--
--         , responseDescribeReservedInstancesModifications $
--             newDescribeReservedInstancesModificationsResponse
--
--         , responseDescribeSpotInstanceRequests $
--             newDescribeSpotInstanceRequestsResponse
--
--         , responseRevokeClientVpnIngress $
--             newRevokeClientVpnIngressResponse
--
--         , responseUnassignPrivateIpAddresses $
--             newUnassignPrivateIpAddressesResponse
--
--         , responseDescribeNetworkInterfacePermissions $
--             newDescribeNetworkInterfacePermissionsResponse
--
--         , responseEnableFastSnapshotRestores $
--             newEnableFastSnapshotRestoresResponse
--
--         , responseDescribeVpcEndpointServicePermissions $
--             newDescribeVpcEndpointServicePermissionsResponse
--
--         , responseDeleteDhcpOptions $
--             newDeleteDhcpOptionsResponse
--
--         , responseCreateRestoreImageTask $
--             newCreateRestoreImageTaskResponse
--
--         , responseRegisterInstanceEventNotificationAttributes $
--             newRegisterInstanceEventNotificationAttributesResponse
--
--         , responseGetVpnConnectionDeviceSampleConfiguration $
--             newGetVpnConnectionDeviceSampleConfigurationResponse
--
--         , responseDeleteSubnetCidrReservation $
--             newDeleteSubnetCidrReservationResponse
--
--         , responseDescribeNetworkAcls $
--             newDescribeNetworkAclsResponse
--
--         , responseCancelImportTask $
--             newCancelImportTaskResponse
--
--         , responseDetachClassicLinkVpc $
--             newDetachClassicLinkVpcResponse
--
--         , responseCreateCapacityReservationFleet $
--             newCreateCapacityReservationFleetResponse
--
--         , responseDescribeRegions $
--             newDescribeRegionsResponse
--
--         , responseMonitorInstances $
--             newMonitorInstancesResponse
--
--         , responseRejectTransitGatewayMulticastDomainAssociations $
--             newRejectTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseAcceptTransitGatewayMulticastDomainAssociations $
--             newAcceptTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseSearchLocalGatewayRoutes $
--             newSearchLocalGatewayRoutesResponse
--
--         , responseDeleteClientVpnRoute $
--             newDeleteClientVpnRouteResponse
--
--         , responseAcceptVpcPeeringConnection $
--             newAcceptVpcPeeringConnectionResponse
--
--         , responseImportSnapshot $
--             newImportSnapshotResponse
--
--         , responseDescribeAddressesAttribute $
--             newDescribeAddressesAttributeResponse
--
--         , responseDescribeVolumeStatus $
--             newDescribeVolumeStatusResponse
--
--         , responseDescribeReplaceRootVolumeTasks $
--             newDescribeReplaceRootVolumeTasksResponse
--
--         , responseModifyInstanceEventWindow $
--             newModifyInstanceEventWindowResponse
--
--         , responseDescribeRouteTables $
--             newDescribeRouteTablesResponse
--
--         , responseDescribeAvailabilityZones $
--             newDescribeAvailabilityZonesResponse
--
--         , responseModifyVpcAttribute $
--             newModifyVpcAttributeResponse
--
--         , responseDescribeClientVpnConnections $
--             newDescribeClientVpnConnectionsResponse
--
--         , responseDescribeFleetHistory $
--             newDescribeFleetHistoryResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseDescribeElasticGpus $
--             newDescribeElasticGpusResponse
--
--         , responseDisassociateTrunkInterface $
--             newDisassociateTrunkInterfaceResponse
--
--         , responseRestoreAddressToClassic $
--             newRestoreAddressToClassicResponse
--
--         , responseDescribeManagedPrefixLists $
--             newDescribeManagedPrefixListsResponse
--
--         , responseCreateKeyPair $
--             newCreateKeyPairResponse
--
--         , responseGetReservedInstancesExchangeQuote $
--             newGetReservedInstancesExchangeQuoteResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--         , responseDeprovisionByoipCidr $
--             newDeprovisionByoipCidrResponse
--
--         , responseGetSerialConsoleAccessStatus $
--             newGetSerialConsoleAccessStatusResponse
--
--         , responseDeleteVpcEndpointServiceConfigurations $
--             newDeleteVpcEndpointServiceConfigurationsResponse
--
--         , responseDescribeSpotFleetInstances $
--             newDescribeSpotFleetInstancesResponse
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

requestDescribeCapacityReservationFleets :: DescribeCapacityReservationFleets -> TestTree
requestDescribeCapacityReservationFleets =
  req
    "DescribeCapacityReservationFleets"
    "fixture/DescribeCapacityReservationFleets.yaml"

requestModifySecurityGroupRules :: ModifySecurityGroupRules -> TestTree
requestModifySecurityGroupRules =
  req
    "ModifySecurityGroupRules"
    "fixture/ModifySecurityGroupRules.yaml"

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

requestResetAddressAttribute :: ResetAddressAttribute -> TestTree
requestResetAddressAttribute =
  req
    "ResetAddressAttribute"
    "fixture/ResetAddressAttribute.yaml"

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

requestCancelCapacityReservationFleets :: CancelCapacityReservationFleets -> TestTree
requestCancelCapacityReservationFleets =
  req
    "CancelCapacityReservationFleets"
    "fixture/CancelCapacityReservationFleets.yaml"

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

requestCreateNetworkInsightsPath :: CreateNetworkInsightsPath -> TestTree
requestCreateNetworkInsightsPath =
  req
    "CreateNetworkInsightsPath"
    "fixture/CreateNetworkInsightsPath.yaml"

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

requestModifyCapacityReservationFleet :: ModifyCapacityReservationFleet -> TestTree
requestModifyCapacityReservationFleet =
  req
    "ModifyCapacityReservationFleet"
    "fixture/ModifyCapacityReservationFleet.yaml"

requestDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
requestDescribeSpotPriceHistory =
  req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory.yaml"

requestDeleteTransitGatewayConnectPeer :: DeleteTransitGatewayConnectPeer -> TestTree
requestDeleteTransitGatewayConnectPeer =
  req
    "DeleteTransitGatewayConnectPeer"
    "fixture/DeleteTransitGatewayConnectPeer.yaml"

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

requestModifyAddressAttribute :: ModifyAddressAttribute -> TestTree
requestModifyAddressAttribute =
  req
    "ModifyAddressAttribute"
    "fixture/ModifyAddressAttribute.yaml"

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

requestAssociateInstanceEventWindow :: AssociateInstanceEventWindow -> TestTree
requestAssociateInstanceEventWindow =
  req
    "AssociateInstanceEventWindow"
    "fixture/AssociateInstanceEventWindow.yaml"

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

requestDeleteInstanceEventWindow :: DeleteInstanceEventWindow -> TestTree
requestDeleteInstanceEventWindow =
  req
    "DeleteInstanceEventWindow"
    "fixture/DeleteInstanceEventWindow.yaml"

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

requestDescribeNetworkInsightsAnalyses :: DescribeNetworkInsightsAnalyses -> TestTree
requestDescribeNetworkInsightsAnalyses =
  req
    "DescribeNetworkInsightsAnalyses"
    "fixture/DescribeNetworkInsightsAnalyses.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets =
  req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestDeleteNetworkInsightsAnalysis :: DeleteNetworkInsightsAnalysis -> TestTree
requestDeleteNetworkInsightsAnalysis =
  req
    "DeleteNetworkInsightsAnalysis"
    "fixture/DeleteNetworkInsightsAnalysis.yaml"

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

requestDeleteTransitGatewayConnect :: DeleteTransitGatewayConnect -> TestTree
requestDeleteTransitGatewayConnect =
  req
    "DeleteTransitGatewayConnect"
    "fixture/DeleteTransitGatewayConnect.yaml"

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

requestDisassociateInstanceEventWindow :: DisassociateInstanceEventWindow -> TestTree
requestDisassociateInstanceEventWindow =
  req
    "DisassociateInstanceEventWindow"
    "fixture/DisassociateInstanceEventWindow.yaml"

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

requestDisableImageDeprecation :: DisableImageDeprecation -> TestTree
requestDisableImageDeprecation =
  req
    "DisableImageDeprecation"
    "fixture/DisableImageDeprecation.yaml"

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

requestEnableImageDeprecation :: EnableImageDeprecation -> TestTree
requestEnableImageDeprecation =
  req
    "EnableImageDeprecation"
    "fixture/EnableImageDeprecation.yaml"

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

requestStartNetworkInsightsAnalysis :: StartNetworkInsightsAnalysis -> TestTree
requestStartNetworkInsightsAnalysis =
  req
    "StartNetworkInsightsAnalysis"
    "fixture/StartNetworkInsightsAnalysis.yaml"

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

requestDescribeSecurityGroupRules :: DescribeSecurityGroupRules -> TestTree
requestDescribeSecurityGroupRules =
  req
    "DescribeSecurityGroupRules"
    "fixture/DescribeSecurityGroupRules.yaml"

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

requestCreateStoreImageTask :: CreateStoreImageTask -> TestTree
requestCreateStoreImageTask =
  req
    "CreateStoreImageTask"
    "fixture/CreateStoreImageTask.yaml"

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

requestDescribeTrunkInterfaceAssociations :: DescribeTrunkInterfaceAssociations -> TestTree
requestDescribeTrunkInterfaceAssociations =
  req
    "DescribeTrunkInterfaceAssociations"
    "fixture/DescribeTrunkInterfaceAssociations.yaml"

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

requestDeleteNetworkInsightsPath :: DeleteNetworkInsightsPath -> TestTree
requestDeleteNetworkInsightsPath =
  req
    "DeleteNetworkInsightsPath"
    "fixture/DeleteNetworkInsightsPath.yaml"

requestDescribeTransitGatewayConnects :: DescribeTransitGatewayConnects -> TestTree
requestDescribeTransitGatewayConnects =
  req
    "DescribeTransitGatewayConnects"
    "fixture/DescribeTransitGatewayConnects.yaml"

requestDeleteTransitGatewayRoute :: DeleteTransitGatewayRoute -> TestTree
requestDeleteTransitGatewayRoute =
  req
    "DeleteTransitGatewayRoute"
    "fixture/DeleteTransitGatewayRoute.yaml"

requestCreateTransitGatewayConnectPeer :: CreateTransitGatewayConnectPeer -> TestTree
requestCreateTransitGatewayConnectPeer =
  req
    "CreateTransitGatewayConnectPeer"
    "fixture/CreateTransitGatewayConnectPeer.yaml"

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

requestAssociateTrunkInterface :: AssociateTrunkInterface -> TestTree
requestAssociateTrunkInterface =
  req
    "AssociateTrunkInterface"
    "fixture/AssociateTrunkInterface.yaml"

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

requestDescribeInstanceEventWindows :: DescribeInstanceEventWindows -> TestTree
requestDescribeInstanceEventWindows =
  req
    "DescribeInstanceEventWindows"
    "fixture/DescribeInstanceEventWindows.yaml"

requestDisableSerialConsoleAccess :: DisableSerialConsoleAccess -> TestTree
requestDisableSerialConsoleAccess =
  req
    "DisableSerialConsoleAccess"
    "fixture/DisableSerialConsoleAccess.yaml"

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

requestGetSubnetCidrReservations :: GetSubnetCidrReservations -> TestTree
requestGetSubnetCidrReservations =
  req
    "GetSubnetCidrReservations"
    "fixture/GetSubnetCidrReservations.yaml"

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

requestEnableSerialConsoleAccess :: EnableSerialConsoleAccess -> TestTree
requestEnableSerialConsoleAccess =
  req
    "EnableSerialConsoleAccess"
    "fixture/EnableSerialConsoleAccess.yaml"

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

requestDescribeTransitGatewayConnectPeers :: DescribeTransitGatewayConnectPeers -> TestTree
requestDescribeTransitGatewayConnectPeers =
  req
    "DescribeTransitGatewayConnectPeers"
    "fixture/DescribeTransitGatewayConnectPeers.yaml"

requestCreateSecurityGroup :: CreateSecurityGroup -> TestTree
requestCreateSecurityGroup =
  req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

requestCreateInstanceEventWindow :: CreateInstanceEventWindow -> TestTree
requestCreateInstanceEventWindow =
  req
    "CreateInstanceEventWindow"
    "fixture/CreateInstanceEventWindow.yaml"

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

requestCreateTransitGatewayConnect :: CreateTransitGatewayConnect -> TestTree
requestCreateTransitGatewayConnect =
  req
    "CreateTransitGatewayConnect"
    "fixture/CreateTransitGatewayConnect.yaml"

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

requestGetFlowLogsIntegrationTemplate :: GetFlowLogsIntegrationTemplate -> TestTree
requestGetFlowLogsIntegrationTemplate =
  req
    "GetFlowLogsIntegrationTemplate"
    "fixture/GetFlowLogsIntegrationTemplate.yaml"

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

requestDescribeNetworkInsightsPaths :: DescribeNetworkInsightsPaths -> TestTree
requestDescribeNetworkInsightsPaths =
  req
    "DescribeNetworkInsightsPaths"
    "fixture/DescribeNetworkInsightsPaths.yaml"

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

requestCreateReplaceRootVolumeTask :: CreateReplaceRootVolumeTask -> TestTree
requestCreateReplaceRootVolumeTask =
  req
    "CreateReplaceRootVolumeTask"
    "fixture/CreateReplaceRootVolumeTask.yaml"

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

requestDescribeStoreImageTasks :: DescribeStoreImageTasks -> TestTree
requestDescribeStoreImageTasks =
  req
    "DescribeStoreImageTasks"
    "fixture/DescribeStoreImageTasks.yaml"

requestGetVpnConnectionDeviceTypes :: GetVpnConnectionDeviceTypes -> TestTree
requestGetVpnConnectionDeviceTypes =
  req
    "GetVpnConnectionDeviceTypes"
    "fixture/GetVpnConnectionDeviceTypes.yaml"

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

requestCreateSubnetCidrReservation :: CreateSubnetCidrReservation -> TestTree
requestCreateSubnetCidrReservation =
  req
    "CreateSubnetCidrReservation"
    "fixture/CreateSubnetCidrReservation.yaml"

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

requestCreateRestoreImageTask :: CreateRestoreImageTask -> TestTree
requestCreateRestoreImageTask =
  req
    "CreateRestoreImageTask"
    "fixture/CreateRestoreImageTask.yaml"

requestRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributes -> TestTree
requestRegisterInstanceEventNotificationAttributes =
  req
    "RegisterInstanceEventNotificationAttributes"
    "fixture/RegisterInstanceEventNotificationAttributes.yaml"

requestGetVpnConnectionDeviceSampleConfiguration :: GetVpnConnectionDeviceSampleConfiguration -> TestTree
requestGetVpnConnectionDeviceSampleConfiguration =
  req
    "GetVpnConnectionDeviceSampleConfiguration"
    "fixture/GetVpnConnectionDeviceSampleConfiguration.yaml"

requestDeleteSubnetCidrReservation :: DeleteSubnetCidrReservation -> TestTree
requestDeleteSubnetCidrReservation =
  req
    "DeleteSubnetCidrReservation"
    "fixture/DeleteSubnetCidrReservation.yaml"

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

requestCreateCapacityReservationFleet :: CreateCapacityReservationFleet -> TestTree
requestCreateCapacityReservationFleet =
  req
    "CreateCapacityReservationFleet"
    "fixture/CreateCapacityReservationFleet.yaml"

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

requestRejectTransitGatewayMulticastDomainAssociations :: RejectTransitGatewayMulticastDomainAssociations -> TestTree
requestRejectTransitGatewayMulticastDomainAssociations =
  req
    "RejectTransitGatewayMulticastDomainAssociations"
    "fixture/RejectTransitGatewayMulticastDomainAssociations.yaml"

requestAcceptTransitGatewayMulticastDomainAssociations :: AcceptTransitGatewayMulticastDomainAssociations -> TestTree
requestAcceptTransitGatewayMulticastDomainAssociations =
  req
    "AcceptTransitGatewayMulticastDomainAssociations"
    "fixture/AcceptTransitGatewayMulticastDomainAssociations.yaml"

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

requestDescribeAddressesAttribute :: DescribeAddressesAttribute -> TestTree
requestDescribeAddressesAttribute =
  req
    "DescribeAddressesAttribute"
    "fixture/DescribeAddressesAttribute.yaml"

requestDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
requestDescribeVolumeStatus =
  req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus.yaml"

requestDescribeReplaceRootVolumeTasks :: DescribeReplaceRootVolumeTasks -> TestTree
requestDescribeReplaceRootVolumeTasks =
  req
    "DescribeReplaceRootVolumeTasks"
    "fixture/DescribeReplaceRootVolumeTasks.yaml"

requestModifyInstanceEventWindow :: ModifyInstanceEventWindow -> TestTree
requestModifyInstanceEventWindow =
  req
    "ModifyInstanceEventWindow"
    "fixture/ModifyInstanceEventWindow.yaml"

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

requestDisassociateTrunkInterface :: DisassociateTrunkInterface -> TestTree
requestDisassociateTrunkInterface =
  req
    "DisassociateTrunkInterface"
    "fixture/DisassociateTrunkInterface.yaml"

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

requestGetSerialConsoleAccessStatus :: GetSerialConsoleAccessStatus -> TestTree
requestGetSerialConsoleAccessStatus =
  req
    "GetSerialConsoleAccessStatus"
    "fixture/GetSerialConsoleAccessStatus.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCapacityReservation)

responseGetAssociatedIpv6PoolCidrs :: GetAssociatedIpv6PoolCidrsResponse -> TestTree
responseGetAssociatedIpv6PoolCidrs =
  res
    "GetAssociatedIpv6PoolCidrsResponse"
    "fixture/GetAssociatedIpv6PoolCidrsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssociatedIpv6PoolCidrs)

responseImportInstance :: ImportInstanceResponse -> TestTree
responseImportInstance =
  res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportInstance)

responseDescribeCapacityReservationFleets :: DescribeCapacityReservationFleetsResponse -> TestTree
responseDescribeCapacityReservationFleets =
  res
    "DescribeCapacityReservationFleetsResponse"
    "fixture/DescribeCapacityReservationFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCapacityReservationFleets)

responseModifySecurityGroupRules :: ModifySecurityGroupRulesResponse -> TestTree
responseModifySecurityGroupRules =
  res
    "ModifySecurityGroupRulesResponse"
    "fixture/ModifySecurityGroupRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySecurityGroupRules)

responseRevokeSecurityGroupEgress :: RevokeSecurityGroupEgressResponse -> TestTree
responseRevokeSecurityGroupEgress =
  res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSecurityGroupEgress)

responseCreateNetworkInterfacePermission :: CreateNetworkInterfacePermissionResponse -> TestTree
responseCreateNetworkInterfacePermission =
  res
    "CreateNetworkInterfacePermissionResponse"
    "fixture/CreateNetworkInterfacePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkInterfacePermission)

responseSendDiagnosticInterrupt :: SendDiagnosticInterruptResponse -> TestTree
responseSendDiagnosticInterrupt =
  res
    "SendDiagnosticInterruptResponse"
    "fixture/SendDiagnosticInterruptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendDiagnosticInterrupt)

responseDeleteLaunchTemplate :: DeleteLaunchTemplateResponse -> TestTree
responseDeleteLaunchTemplate =
  res
    "DeleteLaunchTemplateResponse"
    "fixture/DeleteLaunchTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchTemplate)

responseRejectVpcEndpointConnections :: RejectVpcEndpointConnectionsResponse -> TestTree
responseRejectVpcEndpointConnections =
  res
    "RejectVpcEndpointConnectionsResponse"
    "fixture/RejectVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectVpcEndpointConnections)

responseCreateVpnGateway :: CreateVpnGatewayResponse -> TestTree
responseCreateVpnGateway =
  res
    "CreateVpnGatewayResponse"
    "fixture/CreateVpnGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpnGateway)

responseCreateNetworkAcl :: CreateNetworkAclResponse -> TestTree
responseCreateNetworkAcl =
  res
    "CreateNetworkAclResponse"
    "fixture/CreateNetworkAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkAcl)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeyPair)

responseDescribeSecurityGroupReferences :: DescribeSecurityGroupReferencesResponse -> TestTree
responseDescribeSecurityGroupReferences =
  res
    "DescribeSecurityGroupReferencesResponse"
    "fixture/DescribeSecurityGroupReferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityGroupReferences)

responseDeleteFleets :: DeleteFleetsResponse -> TestTree
responseDeleteFleets =
  res
    "DeleteFleetsResponse"
    "fixture/DeleteFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleets)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTableResponse -> TestTree
responseCreateTransitGatewayRouteTable =
  res
    "CreateTransitGatewayRouteTableResponse"
    "fixture/CreateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayRouteTable)

responseModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptionsResponse -> TestTree
responseModifyInstanceMetadataOptions =
  res
    "ModifyInstanceMetadataOptionsResponse"
    "fixture/ModifyInstanceMetadataOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceMetadataOptions)

responseUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsIngress =
  res
    "UpdateSecurityGroupRuleDescriptionsIngressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityGroupRuleDescriptionsIngress)

responseDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlockResponse -> TestTree
responseDisassociateSubnetCidrBlock =
  res
    "DisassociateSubnetCidrBlockResponse"
    "fixture/DisassociateSubnetCidrBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSubnetCidrBlock)

responseDetachNetworkInterface :: DetachNetworkInterfaceResponse -> TestTree
responseDetachNetworkInterface =
  res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachNetworkInterface)

responseDetachInternetGateway :: DetachInternetGatewayResponse -> TestTree
responseDetachInternetGateway =
  res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachInternetGateway)

responseDeleteVpcEndpoints :: DeleteVpcEndpointsResponse -> TestTree
responseDeleteVpcEndpoints =
  res
    "DeleteVpcEndpointsResponse"
    "fixture/DeleteVpcEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcEndpoints)

responseDescribeClientVpnEndpoints :: DescribeClientVpnEndpointsResponse -> TestTree
responseDescribeClientVpnEndpoints =
  res
    "DescribeClientVpnEndpointsResponse"
    "fixture/DescribeClientVpnEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnEndpoints)

responseDeleteFlowLogs :: DeleteFlowLogsResponse -> TestTree
responseDeleteFlowLogs =
  res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlowLogs)

responseDescribeVpcClassicLink :: DescribeVpcClassicLinkResponse -> TestTree
responseDescribeVpcClassicLink =
  res
    "DescribeVpcClassicLinkResponse"
    "fixture/DescribeVpcClassicLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcClassicLink)

responseGetAssociatedEnclaveCertificateIamRoles :: GetAssociatedEnclaveCertificateIamRolesResponse -> TestTree
responseGetAssociatedEnclaveCertificateIamRoles =
  res
    "GetAssociatedEnclaveCertificateIamRolesResponse"
    "fixture/GetAssociatedEnclaveCertificateIamRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssociatedEnclaveCertificateIamRoles)

responseAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomainResponse -> TestTree
responseAssociateTransitGatewayMulticastDomain =
  res
    "AssociateTransitGatewayMulticastDomainResponse"
    "fixture/AssociateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTransitGatewayMulticastDomain)

responseModifySubnetAttribute :: ModifySubnetAttributeResponse -> TestTree
responseModifySubnetAttribute =
  res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySubnetAttribute)

responseDetachVolume :: VolumeAttachment -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachVolume)

responseDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecificationsResponse -> TestTree
responseDescribeInstanceCreditSpecifications =
  res
    "DescribeInstanceCreditSpecificationsResponse"
    "fixture/DescribeInstanceCreditSpecificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceCreditSpecifications)

responseCancelBundleTask :: CancelBundleTaskResponse -> TestTree
responseCancelBundleTask =
  res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelBundleTask)

responseDescribeByoipCidrs :: DescribeByoipCidrsResponse -> TestTree
responseDescribeByoipCidrs =
  res
    "DescribeByoipCidrsResponse"
    "fixture/DescribeByoipCidrsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeByoipCidrs)

responseAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuoteResponse -> TestTree
responseAcceptReservedInstancesExchangeQuote =
  res
    "AcceptReservedInstancesExchangeQuoteResponse"
    "fixture/AcceptReservedInstancesExchangeQuoteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptReservedInstancesExchangeQuote)

responseReleaseAddress :: ReleaseAddressResponse -> TestTree
responseReleaseAddress =
  res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleaseAddress)

responseDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferingsResponse -> TestTree
responseDescribeInstanceTypeOfferings =
  res
    "DescribeInstanceTypeOfferingsResponse"
    "fixture/DescribeInstanceTypeOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceTypeOfferings)

responseCreateInternetGateway :: CreateInternetGatewayResponse -> TestTree
responseCreateInternetGateway =
  res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInternetGateway)

responseDeleteVpnConnection :: DeleteVpnConnectionResponse -> TestTree
responseDeleteVpnConnection =
  res
    "DeleteVpnConnectionResponse"
    "fixture/DeleteVpnConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpnConnection)

responseDescribeBundleTasks :: DescribeBundleTasksResponse -> TestTree
responseDescribeBundleTasks =
  res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBundleTasks)

responseAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgressResponse -> TestTree
responseAuthorizeSecurityGroupEgress =
  res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeSecurityGroupEgress)

responseEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagationResponse -> TestTree
responseEnableTransitGatewayRouteTablePropagation =
  res
    "EnableTransitGatewayRouteTablePropagationResponse"
    "fixture/EnableTransitGatewayRouteTablePropagationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableTransitGatewayRouteTablePropagation)

responseDeregisterImage :: DeregisterImageResponse -> TestTree
responseDeregisterImage =
  res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterImage)

responseDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotificationsResponse -> TestTree
responseDeleteVpcEndpointConnectionNotifications =
  res
    "DeleteVpcEndpointConnectionNotificationsResponse"
    "fixture/DeleteVpcEndpointConnectionNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcEndpointConnectionNotifications)

responseDescribeCoipPools :: DescribeCoipPoolsResponse -> TestTree
responseDescribeCoipPools =
  res
    "DescribeCoipPoolsResponse"
    "fixture/DescribeCoipPoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCoipPools)

responseResetAddressAttribute :: ResetAddressAttributeResponse -> TestTree
responseResetAddressAttribute =
  res
    "ResetAddressAttributeResponse"
    "fixture/ResetAddressAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetAddressAttribute)

responseGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseGetTransitGatewayMulticastDomainAssociations =
  res
    "GetTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/GetTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayMulticastDomainAssociations)

responseDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseDeleteLocalGatewayRouteTableVpcAssociation =
  res
    "DeleteLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocalGatewayRouteTableVpcAssociation)

responseModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttributeResponse -> TestTree
responseModifyNetworkInterfaceAttribute =
  res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyNetworkInterfaceAttribute)

responseModifyVpcTenancy :: ModifyVpcTenancyResponse -> TestTree
responseModifyVpcTenancy =
  res
    "ModifyVpcTenancyResponse"
    "fixture/ModifyVpcTenancyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcTenancy)

responseDescribeInstanceTypes :: DescribeInstanceTypesResponse -> TestTree
responseDescribeInstanceTypes =
  res
    "DescribeInstanceTypesResponse"
    "fixture/DescribeInstanceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceTypes)

responseCancelCapacityReservationFleets :: CancelCapacityReservationFleetsResponse -> TestTree
responseCancelCapacityReservationFleets =
  res
    "CancelCapacityReservationFleetsResponse"
    "fixture/CancelCapacityReservationFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCapacityReservationFleets)

responseDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRulesResponse -> TestTree
responseDescribeClientVpnAuthorizationRules =
  res
    "DescribeClientVpnAuthorizationRulesResponse"
    "fixture/DescribeClientVpnAuthorizationRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnAuthorizationRules)

responseDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachmentResponse -> TestTree
responseDeleteTransitGatewayVpcAttachment =
  res
    "DeleteTransitGatewayVpcAttachmentResponse"
    "fixture/DeleteTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayVpcAttachment)

responseDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomainResponse -> TestTree
responseDeleteTransitGatewayMulticastDomain =
  res
    "DeleteTransitGatewayMulticastDomainResponse"
    "fixture/DeleteTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayMulticastDomain)

responseCancelReservedInstancesListing :: CancelReservedInstancesListingResponse -> TestTree
responseCancelReservedInstancesListing =
  res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelReservedInstancesListing)

responseAttachClassicLinkVpc :: AttachClassicLinkVpcResponse -> TestTree
responseAttachClassicLinkVpc =
  res
    "AttachClassicLinkVpcResponse"
    "fixture/AttachClassicLinkVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachClassicLinkVpc)

responseDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagationResponse -> TestTree
responseDisableTransitGatewayRouteTablePropagation =
  res
    "DisableTransitGatewayRouteTablePropagationResponse"
    "fixture/DisableTransitGatewayRouteTablePropagationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableTransitGatewayRouteTablePropagation)

responseDescribeVpcClassicLinkDnsSupport :: DescribeVpcClassicLinkDnsSupportResponse -> TestTree
responseDescribeVpcClassicLinkDnsSupport =
  res
    "DescribeVpcClassicLinkDnsSupportResponse"
    "fixture/DescribeVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcClassicLinkDnsSupport)

responseAssociateSubnetCidrBlock :: AssociateSubnetCidrBlockResponse -> TestTree
responseAssociateSubnetCidrBlock =
  res
    "AssociateSubnetCidrBlockResponse"
    "fixture/AssociateSubnetCidrBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSubnetCidrBlock)

responseCreateNetworkInsightsPath :: CreateNetworkInsightsPathResponse -> TestTree
responseCreateNetworkInsightsPath =
  res
    "CreateNetworkInsightsPathResponse"
    "fixture/CreateNetworkInsightsPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkInsightsPath)

responseRunScheduledInstances :: RunScheduledInstancesResponse -> TestTree
responseRunScheduledInstances =
  res
    "RunScheduledInstancesResponse"
    "fixture/RunScheduledInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunScheduledInstances)

responseCreateTransitGatewayRoute :: CreateTransitGatewayRouteResponse -> TestTree
responseCreateTransitGatewayRoute =
  res
    "CreateTransitGatewayRouteResponse"
    "fixture/CreateTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayRoute)

responseCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReferenceResponse -> TestTree
responseCreateTransitGatewayPrefixListReference =
  res
    "CreateTransitGatewayPrefixListReferenceResponse"
    "fixture/CreateTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayPrefixListReference)

responseCancelSpotFleetRequests :: CancelSpotFleetRequestsResponse -> TestTree
responseCancelSpotFleetRequests =
  res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSpotFleetRequests)

responseModifyCapacityReservationFleet :: ModifyCapacityReservationFleetResponse -> TestTree
responseModifyCapacityReservationFleet =
  res
    "ModifyCapacityReservationFleetResponse"
    "fixture/ModifyCapacityReservationFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCapacityReservationFleet)

responseDescribeSpotPriceHistory :: DescribeSpotPriceHistoryResponse -> TestTree
responseDescribeSpotPriceHistory =
  res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotPriceHistory)

responseDeleteTransitGatewayConnectPeer :: DeleteTransitGatewayConnectPeerResponse -> TestTree
responseDeleteTransitGatewayConnectPeer =
  res
    "DeleteTransitGatewayConnectPeerResponse"
    "fixture/DeleteTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayConnectPeer)

responseDescribeDhcpOptions :: DescribeDhcpOptionsResponse -> TestTree
responseDescribeDhcpOptions =
  res
    "DescribeDhcpOptionsResponse"
    "fixture/DescribeDhcpOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDhcpOptions)

responseImportImage :: ImportImageResponse -> TestTree
responseImportImage =
  res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportImage)

responseCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseCreateLocalGatewayRouteTableVpcAssociation =
  res
    "CreateLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/CreateLocalGatewayRouteTableVpcAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocalGatewayRouteTableVpcAssociation)

responseCopyFpgaImage :: CopyFpgaImageResponse -> TestTree
responseCopyFpgaImage =
  res
    "CopyFpgaImageResponse"
    "fixture/CopyFpgaImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyFpgaImage)

responseImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationListResponse -> TestTree
responseImportClientVpnClientCertificateRevocationList =
  res
    "ImportClientVpnClientCertificateRevocationListResponse"
    "fixture/ImportClientVpnClientCertificateRevocationListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportClientVpnClientCertificateRevocationList)

responseStopInstances :: StopInstancesResponse -> TestTree
responseStopInstances =
  res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInstances)

responseEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefaultResponse -> TestTree
responseEnableEbsEncryptionByDefault =
  res
    "EnableEbsEncryptionByDefaultResponse"
    "fixture/EnableEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableEbsEncryptionByDefault)

responseModifyAddressAttribute :: ModifyAddressAttributeResponse -> TestTree
responseModifyAddressAttribute =
  res
    "ModifyAddressAttributeResponse"
    "fixture/ModifyAddressAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAddressAttribute)

responseDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupSources =
  res
    "DeregisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTransitGatewayMulticastGroupSources)

responseModifyLaunchTemplate :: ModifyLaunchTemplateResponse -> TestTree
responseModifyLaunchTemplate =
  res
    "ModifyLaunchTemplateResponse"
    "fixture/ModifyLaunchTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyLaunchTemplate)

responseModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotificationResponse -> TestTree
responseModifyVpcEndpointConnectionNotification =
  res
    "ModifyVpcEndpointConnectionNotificationResponse"
    "fixture/ModifyVpcEndpointConnectionNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpointConnectionNotification)

responseDescribeInternetGateways :: DescribeInternetGatewaysResponse -> TestTree
responseDescribeInternetGateways =
  res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInternetGateways)

responseDisableVpcClassicLink :: DisableVpcClassicLinkResponse -> TestTree
responseDisableVpcClassicLink =
  res
    "DisableVpcClassicLinkResponse"
    "fixture/DisableVpcClassicLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableVpcClassicLink)

responseGetGroupsForCapacityReservation :: GetGroupsForCapacityReservationResponse -> TestTree
responseGetGroupsForCapacityReservation =
  res
    "GetGroupsForCapacityReservationResponse"
    "fixture/GetGroupsForCapacityReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupsForCapacityReservation)

responseDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> TestTree
responseDeleteLaunchTemplateVersions =
  res
    "DeleteLaunchTemplateVersionsResponse"
    "fixture/DeleteLaunchTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchTemplateVersions)

responseBundleInstance :: BundleInstanceResponse -> TestTree
responseBundleInstance =
  res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BundleInstance)

responseDescribeNetworkInterfaces :: DescribeNetworkInterfacesResponse -> TestTree
responseDescribeNetworkInterfaces =
  res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInterfaces)

responseReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociationResponse -> TestTree
responseReplaceNetworkAclAssociation =
  res
    "ReplaceNetworkAclAssociationResponse"
    "fixture/ReplaceNetworkAclAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceNetworkAclAssociation)

responseAssociateInstanceEventWindow :: AssociateInstanceEventWindowResponse -> TestTree
responseAssociateInstanceEventWindow =
  res
    "AssociateInstanceEventWindowResponse"
    "fixture/AssociateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateInstanceEventWindow)

responseDescribeNatGateways :: DescribeNatGatewaysResponse -> TestTree
responseDescribeNatGateways =
  res
    "DescribeNatGatewaysResponse"
    "fixture/DescribeNatGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNatGateways)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddresses)

responseRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersionResponse -> TestTree
responseRestoreManagedPrefixListVersion =
  res
    "RestoreManagedPrefixListVersionResponse"
    "fixture/RestoreManagedPrefixListVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreManagedPrefixListVersion)

responseDescribeSnapshotAttribute :: DescribeSnapshotAttributeResponse -> TestTree
responseDescribeSnapshotAttribute =
  res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshotAttribute)

responseDescribeIdentityIdFormat :: DescribeIdentityIdFormatResponse -> TestTree
responseDescribeIdentityIdFormat =
  res
    "DescribeIdentityIdFormatResponse"
    "fixture/DescribeIdentityIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityIdFormat)

responseReplaceRoute :: ReplaceRouteResponse -> TestTree
responseReplaceRoute =
  res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceRoute)

responseDescribeVpcEndpointServices :: DescribeVpcEndpointServicesResponse -> TestTree
responseDescribeVpcEndpointServices =
  res
    "DescribeVpcEndpointServicesResponse"
    "fixture/DescribeVpcEndpointServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointServices)

responseDeleteLocalGatewayRoute :: DeleteLocalGatewayRouteResponse -> TestTree
responseDeleteLocalGatewayRoute =
  res
    "DeleteLocalGatewayRouteResponse"
    "fixture/DeleteLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocalGatewayRoute)

responseAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngressResponse -> TestTree
responseAuthorizeSecurityGroupIngress =
  res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeSecurityGroupIngress)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcPeeringConnection)

responseDescribeSubnets :: DescribeSubnetsResponse -> TestTree
responseDescribeSubnets =
  res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubnets)

responseGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagationsResponse -> TestTree
responseGetTransitGatewayAttachmentPropagations =
  res
    "GetTransitGatewayAttachmentPropagationsResponse"
    "fixture/GetTransitGatewayAttachmentPropagationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayAttachmentPropagations)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responsePurchaseReservedInstancesOffering :: PurchaseReservedInstancesOfferingResponse -> TestTree
responsePurchaseReservedInstancesOffering =
  res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedInstancesOffering)

responseDeleteNetworkAclEntry :: DeleteNetworkAclEntryResponse -> TestTree
responseDeleteNetworkAclEntry =
  res
    "DeleteNetworkAclEntryResponse"
    "fixture/DeleteNetworkAclEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkAclEntry)

responseResetSnapshotAttribute :: ResetSnapshotAttributeResponse -> TestTree
responseResetSnapshotAttribute =
  res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetSnapshotAttribute)

responseDescribeVpnConnections :: DescribeVpnConnectionsResponse -> TestTree
responseDescribeVpnConnections =
  res
    "DescribeVpnConnectionsResponse"
    "fixture/DescribeVpnConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpnConnections)

responseModifyInstanceEventStartTime :: ModifyInstanceEventStartTimeResponse -> TestTree
responseModifyInstanceEventStartTime =
  res
    "ModifyInstanceEventStartTimeResponse"
    "fixture/ModifyInstanceEventStartTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceEventStartTime)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoute)

responseReplaceNetworkAclEntry :: ReplaceNetworkAclEntryResponse -> TestTree
responseReplaceNetworkAclEntry =
  res
    "ReplaceNetworkAclEntryResponse"
    "fixture/ReplaceNetworkAclEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceNetworkAclEntry)

responseDeleteInstanceEventWindow :: DeleteInstanceEventWindowResponse -> TestTree
responseDeleteInstanceEventWindow =
  res
    "DeleteInstanceEventWindowResponse"
    "fixture/DeleteInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceEventWindow)

responseDescribeVpcEndpoints :: DescribeVpcEndpointsResponse -> TestTree
responseDescribeVpcEndpoints =
  res
    "DescribeVpcEndpointsResponse"
    "fixture/DescribeVpcEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpoints)

responseCreateTrafficMirrorFilter :: CreateTrafficMirrorFilterResponse -> TestTree
responseCreateTrafficMirrorFilter =
  res
    "CreateTrafficMirrorFilterResponse"
    "fixture/CreateTrafficMirrorFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficMirrorFilter)

responseResetInstanceAttribute :: ResetInstanceAttributeResponse -> TestTree
responseResetInstanceAttribute =
  res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetInstanceAttribute)

responseModifyIdentityIdFormat :: ModifyIdentityIdFormatResponse -> TestTree
responseModifyIdentityIdFormat =
  res
    "ModifyIdentityIdFormatResponse"
    "fixture/ModifyIdentityIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyIdentityIdFormat)

responseAttachNetworkInterface :: AttachNetworkInterfaceResponse -> TestTree
responseAttachNetworkInterface =
  res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachNetworkInterface)

responseCreateCapacityReservation :: CreateCapacityReservationResponse -> TestTree
responseCreateCapacityReservation =
  res
    "CreateCapacityReservationResponse"
    "fixture/CreateCapacityReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCapacityReservation)

responseDescribeInstanceStatus :: DescribeInstanceStatusResponse -> TestTree
responseDescribeInstanceStatus =
  res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceStatus)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportKeyPair)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseConfirmProductInstance :: ConfirmProductInstanceResponse -> TestTree
responseConfirmProductInstance =
  res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmProductInstance)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceAttribute)

responseDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferingsResponse -> TestTree
responseDescribeReservedInstancesOfferings =
  res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstancesOfferings)

responseCreateCustomerGateway :: CreateCustomerGatewayResponse -> TestTree
responseCreateCustomerGateway =
  res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomerGateway)

responseDescribeNetworkInsightsAnalyses :: DescribeNetworkInsightsAnalysesResponse -> TestTree
responseDescribeNetworkInsightsAnalyses =
  res
    "DescribeNetworkInsightsAnalysesResponse"
    "fixture/DescribeNetworkInsightsAnalysesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInsightsAnalyses)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleets)

responseDeleteNetworkInsightsAnalysis :: DeleteNetworkInsightsAnalysisResponse -> TestTree
responseDeleteNetworkInsightsAnalysis =
  res
    "DeleteNetworkInsightsAnalysisResponse"
    "fixture/DeleteNetworkInsightsAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInsightsAnalysis)

responseCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachmentResponse -> TestTree
responseCreateTransitGatewayPeeringAttachment =
  res
    "CreateTransitGatewayPeeringAttachmentResponse"
    "fixture/CreateTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayPeeringAttachment)

responseDeleteSecurityGroup :: DeleteSecurityGroupResponse -> TestTree
responseDeleteSecurityGroup =
  res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityGroup)

responseDescribePublicIpv4Pools :: DescribePublicIpv4PoolsResponse -> TestTree
responseDescribePublicIpv4Pools =
  res
    "DescribePublicIpv4PoolsResponse"
    "fixture/DescribePublicIpv4PoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePublicIpv4Pools)

responseDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworksResponse -> TestTree
responseDescribeClientVpnTargetNetworks =
  res
    "DescribeClientVpnTargetNetworksResponse"
    "fixture/DescribeClientVpnTargetNetworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnTargetNetworks)

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection =
  res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcPeeringConnection)

responseAttachInternetGateway :: AttachInternetGatewayResponse -> TestTree
responseAttachInternetGateway =
  res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachInternetGateway)

responseModifyInstancePlacement :: ModifyInstancePlacementResponse -> TestTree
responseModifyInstancePlacement =
  res
    "ModifyInstancePlacementResponse"
    "fixture/ModifyInstancePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstancePlacement)

responseDescribeFlowLogs :: DescribeFlowLogsResponse -> TestTree
responseDescribeFlowLogs =
  res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlowLogs)

responseDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaceGroups =
  res
    "DescribeLocalGatewayVirtualInterfaceGroupsResponse"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayVirtualInterfaceGroups)

responseDeleteTransitGatewayConnect :: DeleteTransitGatewayConnectResponse -> TestTree
responseDeleteTransitGatewayConnect =
  res
    "DeleteTransitGatewayConnectResponse"
    "fixture/DeleteTransitGatewayConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayConnect)

responseDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVpcAssociations =
  res
    "DescribeLocalGatewayRouteTableVpcAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayRouteTableVpcAssociations)

responseDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotificationsResponse -> TestTree
responseDescribeVpcEndpointConnectionNotifications =
  res
    "DescribeVpcEndpointConnectionNotificationsResponse"
    "fixture/DescribeVpcEndpointConnectionNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointConnectionNotifications)

responseGetManagedPrefixListEntries :: GetManagedPrefixListEntriesResponse -> TestTree
responseGetManagedPrefixListEntries =
  res
    "GetManagedPrefixListEntriesResponse"
    "fixture/GetManagedPrefixListEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetManagedPrefixListEntries)

responseDisassociateInstanceEventWindow :: DisassociateInstanceEventWindowResponse -> TestTree
responseDisassociateInstanceEventWindow =
  res
    "DisassociateInstanceEventWindowResponse"
    "fixture/DisassociateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateInstanceEventWindow)

responseRunInstances :: Reservation -> TestTree
responseRunInstances =
  res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunInstances)

responseCreateSnapshots :: CreateSnapshotsResponse -> TestTree
responseCreateSnapshots =
  res
    "CreateSnapshotsResponse"
    "fixture/CreateSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshots)

responseAssociateDhcpOptions :: AssociateDhcpOptionsResponse -> TestTree
responseAssociateDhcpOptions =
  res
    "AssociateDhcpOptionsResponse"
    "fixture/AssociateDhcpOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDhcpOptions)

responseDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRuleResponse -> TestTree
responseDeleteTrafficMirrorFilterRule =
  res
    "DeleteTrafficMirrorFilterRuleResponse"
    "fixture/DeleteTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficMirrorFilterRule)

responseDescribeReservedInstances :: DescribeReservedInstancesResponse -> TestTree
responseDescribeReservedInstances =
  res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstances)

responseDescribeIdFormat :: DescribeIdFormatResponse -> TestTree
responseDescribeIdFormat =
  res
    "DescribeIdFormatResponse"
    "fixture/DescribeIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdFormat)

responseDescribeVpcs :: DescribeVpcsResponse -> TestTree
responseDescribeVpcs =
  res
    "DescribeVpcsResponse"
    "fixture/DescribeVpcsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcs)

responseDescribeConversionTasks :: DescribeConversionTasksResponse -> TestTree
responseDescribeConversionTasks =
  res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConversionTasks)

responseDisableImageDeprecation :: DisableImageDeprecationResponse -> TestTree
responseDisableImageDeprecation =
  res
    "DisableImageDeprecationResponse"
    "fixture/DisableImageDeprecationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableImageDeprecation)

responseCreateLaunchTemplateVersion :: CreateLaunchTemplateVersionResponse -> TestTree
responseCreateLaunchTemplateVersion =
  res
    "CreateLaunchTemplateVersionResponse"
    "fixture/CreateLaunchTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunchTemplateVersion)

responseGetManagedPrefixListAssociations :: GetManagedPrefixListAssociationsResponse -> TestTree
responseGetManagedPrefixListAssociations =
  res
    "GetManagedPrefixListAssociationsResponse"
    "fixture/GetManagedPrefixListAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetManagedPrefixListAssociations)

responseDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupportResponse -> TestTree
responseDisableVpcClassicLinkDnsSupport =
  res
    "DisableVpcClassicLinkDnsSupportResponse"
    "fixture/DisableVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableVpcClassicLinkDnsSupport)

responseApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetworkResponse -> TestTree
responseApplySecurityGroupsToClientVpnTargetNetwork =
  res
    "ApplySecurityGroupsToClientVpnTargetNetworkResponse"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplySecurityGroupsToClientVpnTargetNetwork)

responseDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargetsResponse -> TestTree
responseDescribeTrafficMirrorTargets =
  res
    "DescribeTrafficMirrorTargetsResponse"
    "fixture/DescribeTrafficMirrorTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrafficMirrorTargets)

responseDescribeVolumesModifications :: DescribeVolumesModificationsResponse -> TestTree
responseDescribeVolumesModifications =
  res
    "DescribeVolumesModificationsResponse"
    "fixture/DescribeVolumesModificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumesModifications)

responseExportImage :: ExportImageResponse -> TestTree
responseExportImage =
  res
    "ExportImageResponse"
    "fixture/ExportImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportImage)

responseCreateFpgaImage :: CreateFpgaImageResponse -> TestTree
responseCreateFpgaImage =
  res
    "CreateFpgaImageResponse"
    "fixture/CreateFpgaImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFpgaImage)

responseAcceptVpcEndpointConnections :: AcceptVpcEndpointConnectionsResponse -> TestTree
responseAcceptVpcEndpointConnections =
  res
    "AcceptVpcEndpointConnectionsResponse"
    "fixture/AcceptVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptVpcEndpointConnections)

responseDeleteClientVpnEndpoint :: DeleteClientVpnEndpointResponse -> TestTree
responseDeleteClientVpnEndpoint =
  res
    "DeleteClientVpnEndpointResponse"
    "fixture/DeleteClientVpnEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClientVpnEndpoint)

responseSearchTransitGatewayRoutes :: SearchTransitGatewayRoutesResponse -> TestTree
responseSearchTransitGatewayRoutes =
  res
    "SearchTransitGatewayRoutesResponse"
    "fixture/SearchTransitGatewayRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTransitGatewayRoutes)

responseGetLaunchTemplateData :: GetLaunchTemplateDataResponse -> TestTree
responseGetLaunchTemplateData =
  res
    "GetLaunchTemplateDataResponse"
    "fixture/GetLaunchTemplateDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchTemplateData)

responseAllocateAddress :: AllocateAddressResponse -> TestTree
responseAllocateAddress =
  res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateAddress)

responseAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachmentResponse -> TestTree
responseAcceptTransitGatewayVpcAttachment =
  res
    "AcceptTransitGatewayVpcAttachmentResponse"
    "fixture/AcceptTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptTransitGatewayVpcAttachment)

responseCancelConversionTask :: CancelConversionTaskResponse -> TestTree
responseCancelConversionTask =
  res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelConversionTask)

responseModifyImageAttribute :: ModifyImageAttributeResponse -> TestTree
responseModifyImageAttribute =
  res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyImageAttribute)

responseCreateRouteTable :: CreateRouteTableResponse -> TestTree
responseCreateRouteTable =
  res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRouteTable)

responseRejectTransitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachmentResponse -> TestTree
responseRejectTransitGatewayPeeringAttachment =
  res
    "RejectTransitGatewayPeeringAttachmentResponse"
    "fixture/RejectTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectTransitGatewayPeeringAttachment)

responseReportInstanceStatus :: ReportInstanceStatusResponse -> TestTree
responseReportInstanceStatus =
  res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReportInstanceStatus)

responseAttachVolume :: VolumeAttachment -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachVolume)

responseRequestSpotInstances :: RequestSpotInstancesResponse -> TestTree
responseRequestSpotInstances =
  res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestSpotInstances)

responseWithdrawByoipCidr :: WithdrawByoipCidrResponse -> TestTree
responseWithdrawByoipCidr =
  res
    "WithdrawByoipCidrResponse"
    "fixture/WithdrawByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy WithdrawByoipCidr)

responseDescribeHostReservationOfferings :: DescribeHostReservationOfferingsResponse -> TestTree
responseDescribeHostReservationOfferings =
  res
    "DescribeHostReservationOfferingsResponse"
    "fixture/DescribeHostReservationOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHostReservationOfferings)

responseResetFpgaImageAttribute :: ResetFpgaImageAttributeResponse -> TestTree
responseResetFpgaImageAttribute =
  res
    "ResetFpgaImageAttributeResponse"
    "fixture/ResetFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetFpgaImageAttribute)

responseModifyVpnConnection :: ModifyVpnConnectionResponse -> TestTree
responseModifyVpnConnection =
  res
    "ModifyVpnConnectionResponse"
    "fixture/ModifyVpnConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpnConnection)

responseCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRuleResponse -> TestTree
responseCreateTrafficMirrorFilterRule =
  res
    "CreateTrafficMirrorFilterRuleResponse"
    "fixture/CreateTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficMirrorFilterRule)

responseDeleteTransitGateway :: DeleteTransitGatewayResponse -> TestTree
responseDeleteTransitGateway =
  res
    "DeleteTransitGatewayResponse"
    "fixture/DeleteTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGateway)

responseStartVpcEndpointServicePrivateDnsVerification :: StartVpcEndpointServicePrivateDnsVerificationResponse -> TestTree
responseStartVpcEndpointServicePrivateDnsVerification =
  res
    "StartVpcEndpointServicePrivateDnsVerificationResponse"
    "fixture/StartVpcEndpointServicePrivateDnsVerificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartVpcEndpointServicePrivateDnsVerification)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumes)

responseRejectVpcPeeringConnection :: RejectVpcPeeringConnectionResponse -> TestTree
responseRejectVpcPeeringConnection =
  res
    "RejectVpcPeeringConnectionResponse"
    "fixture/RejectVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectVpcPeeringConnection)

responseDescribeClientVpnRoutes :: DescribeClientVpnRoutesResponse -> TestTree
responseDescribeClientVpnRoutes =
  res
    "DescribeClientVpnRoutesResponse"
    "fixture/DescribeClientVpnRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnRoutes)

responseDeleteVpnConnectionRoute :: DeleteVpnConnectionRouteResponse -> TestTree
responseDeleteVpnConnectionRoute =
  res
    "DeleteVpnConnectionRouteResponse"
    "fixture/DeleteVpnConnectionRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpnConnectionRoute)

responseAssociateEnclaveCertificateIamRole :: AssociateEnclaveCertificateIamRoleResponse -> TestTree
responseAssociateEnclaveCertificateIamRole =
  res
    "AssociateEnclaveCertificateIamRoleResponse"
    "fixture/AssociateEnclaveCertificateIamRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateEnclaveCertificateIamRole)

responseModifyVpcEndpoint :: ModifyVpcEndpointResponse -> TestTree
responseModifyVpcEndpoint =
  res
    "ModifyVpcEndpointResponse"
    "fixture/ModifyVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpoint)

responseDescribeFpgaImageAttribute :: DescribeFpgaImageAttributeResponse -> TestTree
responseDescribeFpgaImageAttribute =
  res
    "DescribeFpgaImageAttributeResponse"
    "fixture/DescribeFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFpgaImageAttribute)

responseAllocateHosts :: AllocateHostsResponse -> TestTree
responseAllocateHosts =
  res
    "AllocateHostsResponse"
    "fixture/AllocateHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateHosts)

responseCreateClientVpnEndpoint :: CreateClientVpnEndpointResponse -> TestTree
responseCreateClientVpnEndpoint =
  res
    "CreateClientVpnEndpointResponse"
    "fixture/CreateClientVpnEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClientVpnEndpoint)

responseCreateTrafficMirrorSession :: CreateTrafficMirrorSessionResponse -> TestTree
responseCreateTrafficMirrorSession =
  res
    "CreateTrafficMirrorSessionResponse"
    "fixture/CreateTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficMirrorSession)

responseRegisterImage :: RegisterImageResponse -> TestTree
responseRegisterImage =
  res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterImage)

responseAdvertiseByoipCidr :: AdvertiseByoipCidrResponse -> TestTree
responseAdvertiseByoipCidr =
  res
    "AdvertiseByoipCidrResponse"
    "fixture/AdvertiseByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdvertiseByoipCidr)

responseModifyFleet :: ModifyFleetResponse -> TestTree
responseModifyFleet =
  res
    "ModifyFleetResponse"
    "fixture/ModifyFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyFleet)

responseRevokeSecurityGroupIngress :: RevokeSecurityGroupIngressResponse -> TestTree
responseRevokeSecurityGroupIngress =
  res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSecurityGroupIngress)

responseGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyIdResponse -> TestTree
responseGetEbsDefaultKmsKeyId =
  res
    "GetEbsDefaultKmsKeyIdResponse"
    "fixture/GetEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEbsDefaultKmsKeyId)

responseDescribeHostReservations :: DescribeHostReservationsResponse -> TestTree
responseDescribeHostReservations =
  res
    "DescribeHostReservationsResponse"
    "fixture/DescribeHostReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHostReservations)

responseUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsEgress =
  res
    "UpdateSecurityGroupRuleDescriptionsEgressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityGroupRuleDescriptionsEgress)

responseEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupportResponse -> TestTree
responseEnableVpcClassicLinkDnsSupport =
  res
    "EnableVpcClassicLinkDnsSupportResponse"
    "fixture/EnableVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableVpcClassicLinkDnsSupport)

responseDescribeVpcEndpointConnections :: DescribeVpcEndpointConnectionsResponse -> TestTree
responseDescribeVpcEndpointConnections =
  res
    "DescribeVpcEndpointConnectionsResponse"
    "fixture/DescribeVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointConnections)

responseModifyReservedInstances :: ModifyReservedInstancesResponse -> TestTree
responseModifyReservedInstances =
  res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReservedInstances)

responseDeleteFpgaImage :: DeleteFpgaImageResponse -> TestTree
responseDeleteFpgaImage =
  res
    "DeleteFpgaImageResponse"
    "fixture/DeleteFpgaImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFpgaImage)

responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  res
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

responseEnableImageDeprecation :: EnableImageDeprecationResponse -> TestTree
responseEnableImageDeprecation =
  res
    "EnableImageDeprecationResponse"
    "fixture/EnableImageDeprecationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableImageDeprecation)

responseDescribeScheduledInstances :: DescribeScheduledInstancesResponse -> TestTree
responseDescribeScheduledInstances =
  res
    "DescribeScheduledInstancesResponse"
    "fixture/DescribeScheduledInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledInstances)

responseSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroupsResponse -> TestTree
responseSearchTransitGatewayMulticastGroups =
  res
    "SearchTransitGatewayMulticastGroupsResponse"
    "fixture/SearchTransitGatewayMulticastGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTransitGatewayMulticastGroups)

responseCreateFlowLogs :: CreateFlowLogsResponse -> TestTree
responseCreateFlowLogs =
  res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlowLogs)

responseDescribeSpotFleetRequests :: DescribeSpotFleetRequestsResponse -> TestTree
responseDescribeSpotFleetRequests =
  res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotFleetRequests)

responseMoveAddressToVpc :: MoveAddressToVpcResponse -> TestTree
responseMoveAddressToVpc =
  res
    "MoveAddressToVpcResponse"
    "fixture/MoveAddressToVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MoveAddressToVpc)

responseDescribeFleetInstances :: DescribeFleetInstancesResponse -> TestTree
responseDescribeFleetInstances =
  res
    "DescribeFleetInstancesResponse"
    "fixture/DescribeFleetInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetInstances)

responseDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersionsResponse -> TestTree
responseDescribeLaunchTemplateVersions =
  res
    "DescribeLaunchTemplateVersionsResponse"
    "fixture/DescribeLaunchTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLaunchTemplateVersions)

responseStartNetworkInsightsAnalysis :: StartNetworkInsightsAnalysisResponse -> TestTree
responseStartNetworkInsightsAnalysis =
  res
    "StartNetworkInsightsAnalysisResponse"
    "fixture/StartNetworkInsightsAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNetworkInsightsAnalysis)

responseModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecificationResponse -> TestTree
responseModifyInstanceCreditSpecification =
  res
    "ModifyInstanceCreditSpecificationResponse"
    "fixture/ModifyInstanceCreditSpecificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceCreditSpecification)

responseDescribePrincipalIdFormat :: DescribePrincipalIdFormatResponse -> TestTree
responseDescribePrincipalIdFormat =
  res
    "DescribePrincipalIdFormatResponse"
    "fixture/DescribePrincipalIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePrincipalIdFormat)

responseDescribeTransitGateways :: DescribeTransitGatewaysResponse -> TestTree
responseDescribeTransitGateways =
  res
    "DescribeTransitGatewaysResponse"
    "fixture/DescribeTransitGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGateways)

responseDeleteNetworkAcl :: DeleteNetworkAclResponse -> TestTree
responseDeleteNetworkAcl =
  res
    "DeleteNetworkAclResponse"
    "fixture/DeleteNetworkAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkAcl)

responseDisassociateTransitGatewayMulticastDomain :: DisassociateTransitGatewayMulticastDomainResponse -> TestTree
responseDisassociateTransitGatewayMulticastDomain =
  res
    "DisassociateTransitGatewayMulticastDomainResponse"
    "fixture/DisassociateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTransitGatewayMulticastDomain)

responseDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTableResponse -> TestTree
responseDeleteTransitGatewayRouteTable =
  res
    "DeleteTransitGatewayRouteTableResponse"
    "fixture/DeleteTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayRouteTable)

responseDescribeSecurityGroupRules :: DescribeSecurityGroupRulesResponse -> TestTree
responseDescribeSecurityGroupRules =
  res
    "DescribeSecurityGroupRulesResponse"
    "fixture/DescribeSecurityGroupRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityGroupRules)

responseCreateLaunchTemplate :: CreateLaunchTemplateResponse -> TestTree
responseCreateLaunchTemplate =
  res
    "CreateLaunchTemplateResponse"
    "fixture/CreateLaunchTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunchTemplate)

responseCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotificationResponse -> TestTree
responseCreateVpcEndpointConnectionNotification =
  res
    "CreateVpcEndpointConnectionNotificationResponse"
    "fixture/CreateVpcEndpointConnectionNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpointConnectionNotification)

responseDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermissionResponse -> TestTree
responseDeleteNetworkInterfacePermission =
  res
    "DeleteNetworkInterfacePermissionResponse"
    "fixture/DeleteNetworkInterfacePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInterfacePermission)

responseDeleteVpnGateway :: DeleteVpnGatewayResponse -> TestTree
responseDeleteVpnGateway =
  res
    "DeleteVpnGatewayResponse"
    "fixture/DeleteVpnGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpnGateway)

responseCreateStoreImageTask :: CreateStoreImageTaskResponse -> TestTree
responseCreateStoreImageTask =
  res
    "CreateStoreImageTaskResponse"
    "fixture/CreateStoreImageTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStoreImageTask)

responseCreateTrafficMirrorTarget :: CreateTrafficMirrorTargetResponse -> TestTree
responseCreateTrafficMirrorTarget =
  res
    "CreateTrafficMirrorTargetResponse"
    "fixture/CreateTrafficMirrorTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficMirrorTarget)

responseDescribeImportImageTasks :: DescribeImportImageTasksResponse -> TestTree
responseDescribeImportImageTasks =
  res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImportImageTasks)

responseDescribeVolumeAttribute :: DescribeVolumeAttributeResponse -> TestTree
responseDescribeVolumeAttribute =
  res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumeAttribute)

responseDescribeMovingAddresses :: DescribeMovingAddressesResponse -> TestTree
responseDescribeMovingAddresses =
  res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMovingAddresses)

responseExportTransitGatewayRoutes :: ExportTransitGatewayRoutesResponse -> TestTree
responseExportTransitGatewayRoutes =
  res
    "ExportTransitGatewayRoutesResponse"
    "fixture/ExportTransitGatewayRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportTransitGatewayRoutes)

responseGetPasswordData :: GetPasswordDataResponse -> TestTree
responseGetPasswordData =
  res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPasswordData)

responseCreateVpc :: CreateVpcResponse -> TestTree
responseCreateVpc =
  res
    "CreateVpcResponse"
    "fixture/CreateVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpc)

responseModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptionsResponse -> TestTree
responseModifyVpcPeeringConnectionOptions =
  res
    "ModifyVpcPeeringConnectionOptionsResponse"
    "fixture/ModifyVpcPeeringConnectionOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcPeeringConnectionOptions)

responseDescribeFpgaImages :: DescribeFpgaImagesResponse -> TestTree
responseDescribeFpgaImages =
  res
    "DescribeFpgaImagesResponse"
    "fixture/DescribeFpgaImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFpgaImages)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopySnapshot)

responseAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachmentResponse -> TestTree
responseAcceptTransitGatewayPeeringAttachment =
  res
    "AcceptTransitGatewayPeeringAttachmentResponse"
    "fixture/AcceptTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptTransitGatewayPeeringAttachment)

responseDisassociateAddress :: DisassociateAddressResponse -> TestTree
responseDisassociateAddress =
  res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAddress)

responseModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServicesResponse -> TestTree
responseModifyTrafficMirrorFilterNetworkServices =
  res
    "ModifyTrafficMirrorFilterNetworkServicesResponse"
    "fixture/ModifyTrafficMirrorFilterNetworkServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTrafficMirrorFilterNetworkServices)

responseDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGatewaysResponse -> TestTree
responseDescribeEgressOnlyInternetGateways =
  res
    "DescribeEgressOnlyInternetGatewaysResponse"
    "fixture/DescribeEgressOnlyInternetGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEgressOnlyInternetGateways)

responseDeleteVpc :: DeleteVpcResponse -> TestTree
responseDeleteVpc =
  res
    "DeleteVpcResponse"
    "fixture/DeleteVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpc)

responseCreateInstanceExportTask :: CreateInstanceExportTaskResponse -> TestTree
responseCreateInstanceExportTask =
  res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceExportTask)

responseRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachmentResponse -> TestTree
responseRejectTransitGatewayVpcAttachment =
  res
    "RejectTransitGatewayVpcAttachmentResponse"
    "fixture/RejectTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectTransitGatewayVpcAttachment)

responseDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessionsResponse -> TestTree
responseDescribeTrafficMirrorSessions =
  res
    "DescribeTrafficMirrorSessionsResponse"
    "fixture/DescribeTrafficMirrorSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrafficMirrorSessions)

responseGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociationsResponse -> TestTree
responseGetTransitGatewayRouteTableAssociations =
  res
    "GetTransitGatewayRouteTableAssociationsResponse"
    "fixture/GetTransitGatewayRouteTableAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayRouteTableAssociations)

responseAssociateVpcCidrBlock :: AssociateVpcCidrBlockResponse -> TestTree
responseAssociateVpcCidrBlock =
  res
    "AssociateVpcCidrBlockResponse"
    "fixture/AssociateVpcCidrBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateVpcCidrBlock)

responseDescribeVpcAttribute :: DescribeVpcAttributeResponse -> TestTree
responseDescribeVpcAttribute =
  res
    "DescribeVpcAttributeResponse"
    "fixture/DescribeVpcAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcAttribute)

responseCreateVolume :: Volume -> TestTree
responseCreateVolume =
  res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVolume)

responseCreateDefaultSubnet :: CreateDefaultSubnetResponse -> TestTree
responseCreateDefaultSubnet =
  res
    "CreateDefaultSubnetResponse"
    "fixture/CreateDefaultSubnetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDefaultSubnet)

responseDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailabilityResponse -> TestTree
responseDescribeScheduledInstanceAvailability =
  res
    "DescribeScheduledInstanceAvailabilityResponse"
    "fixture/DescribeScheduledInstanceAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledInstanceAvailability)

responseDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetworkResponse -> TestTree
responseDisassociateClientVpnTargetNetwork =
  res
    "DisassociateClientVpnTargetNetworkResponse"
    "fixture/DisassociateClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateClientVpnTargetNetwork)

responseCreateClientVpnRoute :: CreateClientVpnRouteResponse -> TestTree
responseCreateClientVpnRoute =
  res
    "CreateClientVpnRouteResponse"
    "fixture/CreateClientVpnRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClientVpnRoute)

responseModifyVolumeAttribute :: ModifyVolumeAttributeResponse -> TestTree
responseModifyVolumeAttribute =
  res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVolumeAttribute)

responseExportClientVpnClientConfiguration :: ExportClientVpnClientConfigurationResponse -> TestTree
responseExportClientVpnClientConfiguration =
  res
    "ExportClientVpnClientConfigurationResponse"
    "fixture/ExportClientVpnClientConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportClientVpnClientConfiguration)

responseDescribeTrunkInterfaceAssociations :: DescribeTrunkInterfaceAssociationsResponse -> TestTree
responseDescribeTrunkInterfaceAssociations =
  res
    "DescribeTrunkInterfaceAssociationsResponse"
    "fixture/DescribeTrunkInterfaceAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrunkInterfaceAssociations)

responseDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTargetResponse -> TestTree
responseDeleteTrafficMirrorTarget =
  res
    "DeleteTrafficMirrorTargetResponse"
    "fixture/DeleteTrafficMirrorTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficMirrorTarget)

responseDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
responseDescribeSpotDatafeedSubscription =
  res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotDatafeedSubscription)

responseDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTablesResponse -> TestTree
responseDescribeLocalGatewayRouteTables =
  res
    "DescribeLocalGatewayRouteTablesResponse"
    "fixture/DescribeLocalGatewayRouteTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayRouteTables)

responseDescribePrefixLists :: DescribePrefixListsResponse -> TestTree
responseDescribePrefixLists =
  res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePrefixLists)

responseAssociateTransitGatewayRouteTable :: AssociateTransitGatewayRouteTableResponse -> TestTree
responseAssociateTransitGatewayRouteTable =
  res
    "AssociateTransitGatewayRouteTableResponse"
    "fixture/AssociateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTransitGatewayRouteTable)

responseDeletePlacementGroup :: DeletePlacementGroupResponse -> TestTree
responseDeletePlacementGroup =
  res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlacementGroup)

responseModifyTransitGateway :: ModifyTransitGatewayResponse -> TestTree
responseModifyTransitGateway =
  res
    "ModifyTransitGatewayResponse"
    "fixture/ModifyTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTransitGateway)

responseDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReferenceResponse -> TestTree
responseDeleteTransitGatewayPrefixListReference =
  res
    "DeleteTransitGatewayPrefixListReferenceResponse"
    "fixture/DeleteTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayPrefixListReference)

responseCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomainResponse -> TestTree
responseCreateTransitGatewayMulticastDomain =
  res
    "CreateTransitGatewayMulticastDomainResponse"
    "fixture/CreateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayMulticastDomain)

responseDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributesResponse -> TestTree
responseDeregisterInstanceEventNotificationAttributes =
  res
    "DeregisterInstanceEventNotificationAttributesResponse"
    "fixture/DeregisterInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterInstanceEventNotificationAttributes)

responseRequestSpotFleet :: RequestSpotFleetResponse -> TestTree
responseRequestSpotFleet =
  res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestSpotFleet)

responseDeleteNetworkInsightsPath :: DeleteNetworkInsightsPathResponse -> TestTree
responseDeleteNetworkInsightsPath =
  res
    "DeleteNetworkInsightsPathResponse"
    "fixture/DeleteNetworkInsightsPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInsightsPath)

responseDescribeTransitGatewayConnects :: DescribeTransitGatewayConnectsResponse -> TestTree
responseDescribeTransitGatewayConnects =
  res
    "DescribeTransitGatewayConnectsResponse"
    "fixture/DescribeTransitGatewayConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayConnects)

responseDeleteTransitGatewayRoute :: DeleteTransitGatewayRouteResponse -> TestTree
responseDeleteTransitGatewayRoute =
  res
    "DeleteTransitGatewayRouteResponse"
    "fixture/DeleteTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayRoute)

responseCreateTransitGatewayConnectPeer :: CreateTransitGatewayConnectPeerResponse -> TestTree
responseCreateTransitGatewayConnectPeer =
  res
    "CreateTransitGatewayConnectPeerResponse"
    "fixture/CreateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayConnectPeer)

responseDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefaultResponse -> TestTree
responseDisableEbsEncryptionByDefault =
  res
    "DisableEbsEncryptionByDefaultResponse"
    "fixture/DisableEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableEbsEncryptionByDefault)

responseDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupMembers =
  res
    "DeregisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTransitGatewayMulticastGroupMembers)

responseAssociateTrunkInterface :: AssociateTrunkInterfaceResponse -> TestTree
responseAssociateTrunkInterface =
  res
    "AssociateTrunkInterfaceResponse"
    "fixture/AssociateTrunkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTrunkInterface)

responseCreateSubnet :: CreateSubnetResponse -> TestTree
responseCreateSubnet =
  res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubnet)

responseCreateNetworkInterface :: CreateNetworkInterfaceResponse -> TestTree
responseCreateNetworkInterface =
  res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkInterface)

responseDescribeSecurityGroups :: DescribeSecurityGroupsResponse -> TestTree
responseDescribeSecurityGroups =
  res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityGroups)

responseGetCapacityReservationUsage :: GetCapacityReservationUsageResponse -> TestTree
responseGetCapacityReservationUsage =
  res
    "GetCapacityReservationUsageResponse"
    "fixture/GetCapacityReservationUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCapacityReservationUsage)

responseCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachmentResponse -> TestTree
responseCreateTransitGatewayVpcAttachment =
  res
    "CreateTransitGatewayVpcAttachmentResponse"
    "fixture/CreateTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayVpcAttachment)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExportTasks)

responseModifySpotFleetRequest :: ModifySpotFleetRequestResponse -> TestTree
responseModifySpotFleetRequest =
  res
    "ModifySpotFleetRequestResponse"
    "fixture/ModifySpotFleetRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySpotFleetRequest)

responseDetachVpnGateway :: DetachVpnGatewayResponse -> TestTree
responseDetachVpnGateway =
  res
    "DetachVpnGatewayResponse"
    "fixture/DetachVpnGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachVpnGateway)

responseModifyManagedPrefixList :: ModifyManagedPrefixListResponse -> TestTree
responseModifyManagedPrefixList =
  res
    "ModifyManagedPrefixListResponse"
    "fixture/ModifyManagedPrefixListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyManagedPrefixList)

responseGetHostReservationPurchasePreview :: GetHostReservationPurchasePreviewResponse -> TestTree
responseGetHostReservationPurchasePreview =
  res
    "GetHostReservationPurchasePreviewResponse"
    "fixture/GetHostReservationPurchasePreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostReservationPurchasePreview)

responseEnableVolumeIO :: EnableVolumeIOResponse -> TestTree
responseEnableVolumeIO =
  res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableVolumeIO)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstances)

responseDescribeInstanceEventWindows :: DescribeInstanceEventWindowsResponse -> TestTree
responseDescribeInstanceEventWindows =
  res
    "DescribeInstanceEventWindowsResponse"
    "fixture/DescribeInstanceEventWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceEventWindows)

responseDisableSerialConsoleAccess :: DisableSerialConsoleAccessResponse -> TestTree
responseDisableSerialConsoleAccess =
  res
    "DisableSerialConsoleAccessResponse"
    "fixture/DisableSerialConsoleAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSerialConsoleAccess)

responseCreateNatGateway :: CreateNatGatewayResponse -> TestTree
responseCreateNatGateway =
  res
    "CreateNatGatewayResponse"
    "fixture/CreateNatGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNatGateway)

responseDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfacesResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaces =
  res
    "DescribeLocalGatewayVirtualInterfacesResponse"
    "fixture/DescribeLocalGatewayVirtualInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayVirtualInterfaces)

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcPeeringConnections)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelExportTask)

responseCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfigurationResponse -> TestTree
responseCreateVpcEndpointServiceConfiguration =
  res
    "CreateVpcEndpointServiceConfigurationResponse"
    "fixture/CreateVpcEndpointServiceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpointServiceConfiguration)

responseCreateDefaultVpc :: CreateDefaultVpcResponse -> TestTree
responseCreateDefaultVpc =
  res
    "CreateDefaultVpcResponse"
    "fixture/CreateDefaultVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDefaultVpc)

responseDisassociateVpcCidrBlock :: DisassociateVpcCidrBlockResponse -> TestTree
responseDisassociateVpcCidrBlock =
  res
    "DisassociateVpcCidrBlockResponse"
    "fixture/DisassociateVpcCidrBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateVpcCidrBlock)

responseDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFiltersResponse -> TestTree
responseDescribeTrafficMirrorFilters =
  res
    "DescribeTrafficMirrorFiltersResponse"
    "fixture/DescribeTrafficMirrorFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrafficMirrorFilters)

responseDescribeFastSnapshotRestores :: DescribeFastSnapshotRestoresResponse -> TestTree
responseDescribeFastSnapshotRestores =
  res
    "DescribeFastSnapshotRestoresResponse"
    "fixture/DescribeFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFastSnapshotRestores)

responseCancelCapacityReservation :: CancelCapacityReservationResponse -> TestTree
responseCancelCapacityReservation =
  res
    "CancelCapacityReservationResponse"
    "fixture/CancelCapacityReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCapacityReservation)

responseDeleteNetworkInterface :: DeleteNetworkInterfaceResponse -> TestTree
responseDeleteNetworkInterface =
  res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInterface)

responseDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTableResponse -> TestTree
responseDisassociateTransitGatewayRouteTable =
  res
    "DisassociateTransitGatewayRouteTableResponse"
    "fixture/DisassociateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTransitGatewayRouteTable)

responseReplaceRouteTableAssociation :: ReplaceRouteTableAssociationResponse -> TestTree
responseReplaceRouteTableAssociation =
  res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceRouteTableAssociation)

responseStartInstances :: StartInstancesResponse -> TestTree
responseStartInstances =
  res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstances)

responseCreatePlacementGroup :: CreatePlacementGroupResponse -> TestTree
responseCreatePlacementGroup =
  res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlacementGroup)

responseDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributesResponse -> TestTree
responseDescribeInstanceEventNotificationAttributes =
  res
    "DescribeInstanceEventNotificationAttributesResponse"
    "fixture/DescribeInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceEventNotificationAttributes)

responseDescribeCapacityReservations :: DescribeCapacityReservationsResponse -> TestTree
responseDescribeCapacityReservations =
  res
    "DescribeCapacityReservationsResponse"
    "fixture/DescribeCapacityReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCapacityReservations)

responseModifyClientVpnEndpoint :: ModifyClientVpnEndpointResponse -> TestTree
responseModifyClientVpnEndpoint =
  res
    "ModifyClientVpnEndpointResponse"
    "fixture/ModifyClientVpnEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClientVpnEndpoint)

responseModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributesResponse -> TestTree
responseModifyInstanceCapacityReservationAttributes =
  res
    "ModifyInstanceCapacityReservationAttributesResponse"
    "fixture/ModifyInstanceCapacityReservationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceCapacityReservationAttributes)

responseDescribeAggregateIdFormat :: DescribeAggregateIdFormatResponse -> TestTree
responseDescribeAggregateIdFormat =
  res
    "DescribeAggregateIdFormatResponse"
    "fixture/DescribeAggregateIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAggregateIdFormat)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshots)

responseGetSubnetCidrReservations :: GetSubnetCidrReservationsResponse -> TestTree
responseGetSubnetCidrReservations =
  res
    "GetSubnetCidrReservationsResponse"
    "fixture/GetSubnetCidrReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubnetCidrReservations)

responseAssociateAddress :: AssociateAddressResponse -> TestTree
responseAssociateAddress =
  res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAddress)

responseModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRuleResponse -> TestTree
responseModifyTrafficMirrorFilterRule =
  res
    "ModifyTrafficMirrorFilterRuleResponse"
    "fixture/ModifyTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTrafficMirrorFilterRule)

responseDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttributeResponse -> TestTree
responseDescribeNetworkInterfaceAttribute =
  res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInterfaceAttribute)

responseReplaceIamInstanceProfileAssociation :: ReplaceIamInstanceProfileAssociationResponse -> TestTree
responseReplaceIamInstanceProfileAssociation =
  res
    "ReplaceIamInstanceProfileAssociationResponse"
    "fixture/ReplaceIamInstanceProfileAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceIamInstanceProfileAssociation)

responseAssociateClientVpnTargetNetwork :: AssociateClientVpnTargetNetworkResponse -> TestTree
responseAssociateClientVpnTargetNetwork =
  res
    "AssociateClientVpnTargetNetworkResponse"
    "fixture/AssociateClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateClientVpnTargetNetwork)

responseReleaseHosts :: ReleaseHostsResponse -> TestTree
responseReleaseHosts =
  res
    "ReleaseHostsResponse"
    "fixture/ReleaseHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleaseHosts)

responseEnableSerialConsoleAccess :: EnableSerialConsoleAccessResponse -> TestTree
responseEnableSerialConsoleAccess =
  res
    "EnableSerialConsoleAccessResponse"
    "fixture/EnableSerialConsoleAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSerialConsoleAccess)

responseResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttributeResponse -> TestTree
responseResetNetworkInterfaceAttribute =
  res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetNetworkInterfaceAttribute)

responseDeleteInternetGateway :: DeleteInternetGatewayResponse -> TestTree
responseDeleteInternetGateway =
  res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInternetGateway)

responseDescribeReservedInstancesListings :: DescribeReservedInstancesListingsResponse -> TestTree
responseDescribeReservedInstancesListings =
  res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstancesListings)

responseCreateVpnConnection :: CreateVpnConnectionResponse -> TestTree
responseCreateVpnConnection =
  res
    "CreateVpnConnectionResponse"
    "fixture/CreateVpnConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpnConnection)

responseReplaceTransitGatewayRoute :: ReplaceTransitGatewayRouteResponse -> TestTree
responseReplaceTransitGatewayRoute =
  res
    "ReplaceTransitGatewayRouteResponse"
    "fixture/ReplaceTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceTransitGatewayRoute)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseDeleteNatGateway :: DeleteNatGatewayResponse -> TestTree
responseDeleteNatGateway =
  res
    "DeleteNatGatewayResponse"
    "fixture/DeleteNatGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNatGateway)

responseDescribeImportSnapshotTasks :: DescribeImportSnapshotTasksResponse -> TestTree
responseDescribeImportSnapshotTasks =
  res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImportSnapshotTasks)

responseGetCoipPoolUsage :: GetCoipPoolUsageResponse -> TestTree
responseGetCoipPoolUsage =
  res
    "GetCoipPoolUsageResponse"
    "fixture/GetCoipPoolUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoipPoolUsage)

responseDescribeCustomerGateways :: DescribeCustomerGatewaysResponse -> TestTree
responseDescribeCustomerGateways =
  res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomerGateways)

responseDeleteSubnet :: DeleteSubnetResponse -> TestTree
responseDeleteSubnet =
  res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubnet)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyImage)

responseCreateVpcEndpoint :: CreateVpcEndpointResponse -> TestTree
responseCreateVpcEndpoint =
  res
    "CreateVpcEndpointResponse"
    "fixture/CreateVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpoint)

responseModifyTrafficMirrorSession :: ModifyTrafficMirrorSessionResponse -> TestTree
responseModifyTrafficMirrorSession =
  res
    "ModifyTrafficMirrorSessionResponse"
    "fixture/ModifyTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTrafficMirrorSession)

responseDescribeCarrierGateways :: DescribeCarrierGatewaysResponse -> TestTree
responseDescribeCarrierGateways =
  res
    "DescribeCarrierGatewaysResponse"
    "fixture/DescribeCarrierGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCarrierGateways)

responseDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachmentsResponse -> TestTree
responseDescribeTransitGatewayPeeringAttachments =
  res
    "DescribeTransitGatewayPeeringAttachmentsResponse"
    "fixture/DescribeTransitGatewayPeeringAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayPeeringAttachments)

responseDeleteQueuedReservedInstances :: DeleteQueuedReservedInstancesResponse -> TestTree
responseDeleteQueuedReservedInstances =
  res
    "DeleteQueuedReservedInstancesResponse"
    "fixture/DeleteQueuedReservedInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueuedReservedInstances)

responseDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomainsResponse -> TestTree
responseDescribeTransitGatewayMulticastDomains =
  res
    "DescribeTransitGatewayMulticastDomainsResponse"
    "fixture/DescribeTransitGatewayMulticastDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayMulticastDomains)

responseGetDefaultCreditSpecification :: GetDefaultCreditSpecificationResponse -> TestTree
responseGetDefaultCreditSpecification =
  res
    "GetDefaultCreditSpecificationResponse"
    "fixture/GetDefaultCreditSpecificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDefaultCreditSpecification)

responseUnmonitorInstances :: UnmonitorInstancesResponse -> TestTree
responseUnmonitorInstances =
  res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnmonitorInstances)

responseDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachmentsResponse -> TestTree
responseDescribeTransitGatewayVpcAttachments =
  res
    "DescribeTransitGatewayVpcAttachmentsResponse"
    "fixture/DescribeTransitGatewayVpcAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayVpcAttachments)

responseDescribeTransitGatewayConnectPeers :: DescribeTransitGatewayConnectPeersResponse -> TestTree
responseDescribeTransitGatewayConnectPeers =
  res
    "DescribeTransitGatewayConnectPeersResponse"
    "fixture/DescribeTransitGatewayConnectPeersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayConnectPeers)

responseCreateSecurityGroup :: CreateSecurityGroupResponse -> TestTree
responseCreateSecurityGroup =
  res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityGroup)

responseCreateInstanceEventWindow :: CreateInstanceEventWindowResponse -> TestTree
responseCreateInstanceEventWindow =
  res
    "CreateInstanceEventWindowResponse"
    "fixture/CreateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceEventWindow)

responseGetEbsEncryptionByDefault :: GetEbsEncryptionByDefaultResponse -> TestTree
responseGetEbsEncryptionByDefault =
  res
    "GetEbsEncryptionByDefaultResponse"
    "fixture/GetEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEbsEncryptionByDefault)

responseImportVolume :: ImportVolumeResponse -> TestTree
responseImportVolume =
  res
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportVolume)

responseDeleteCarrierGateway :: DeleteCarrierGatewayResponse -> TestTree
responseDeleteCarrierGateway =
  res
    "DeleteCarrierGatewayResponse"
    "fixture/DeleteCarrierGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCarrierGateway)

responseDisableVgwRoutePropagation :: DisableVgwRoutePropagationResponse -> TestTree
responseDisableVgwRoutePropagation =
  res
    "DisableVgwRoutePropagationResponse"
    "fixture/DisableVgwRoutePropagationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableVgwRoutePropagation)

responseDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilterResponse -> TestTree
responseDeleteTrafficMirrorFilter =
  res
    "DeleteTrafficMirrorFilterResponse"
    "fixture/DeleteTrafficMirrorFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficMirrorFilter)

responseModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificateResponse -> TestTree
responseModifyVpnTunnelCertificate =
  res
    "ModifyVpnTunnelCertificateResponse"
    "fixture/ModifyVpnTunnelCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpnTunnelCertificate)

responseCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscriptionResponse -> TestTree
responseCreateSpotDatafeedSubscription =
  res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSpotDatafeedSubscription)

responseCancelSpotInstanceRequests :: CancelSpotInstanceRequestsResponse -> TestTree
responseCancelSpotInstanceRequests =
  res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSpotInstanceRequests)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoute)

responseDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurationsResponse -> TestTree
responseDescribeVpcEndpointServiceConfigurations =
  res
    "DescribeVpcEndpointServiceConfigurationsResponse"
    "fixture/DescribeVpcEndpointServiceConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointServiceConfigurations)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseAssignPrivateIpAddresses :: AssignPrivateIpAddressesResponse -> TestTree
responseAssignPrivateIpAddresses =
  res
    "AssignPrivateIpAddressesResponse"
    "fixture/AssignPrivateIpAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignPrivateIpAddresses)

responseAuthorizeClientVpnIngress :: AuthorizeClientVpnIngressResponse -> TestTree
responseAuthorizeClientVpnIngress =
  res
    "AuthorizeClientVpnIngressResponse"
    "fixture/AuthorizeClientVpnIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeClientVpnIngress)

responseDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachmentResponse -> TestTree
responseDeleteTransitGatewayPeeringAttachment =
  res
    "DeleteTransitGatewayPeeringAttachmentResponse"
    "fixture/DeleteTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayPeeringAttachment)

responseModifyInstanceAttribute :: ModifyInstanceAttributeResponse -> TestTree
responseModifyInstanceAttribute =
  res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceAttribute)

responseDeleteCustomerGateway :: DeleteCustomerGatewayResponse -> TestTree
responseDeleteCustomerGateway =
  res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomerGateway)

responseDisassociateIamInstanceProfile :: DisassociateIamInstanceProfileResponse -> TestTree
responseDisassociateIamInstanceProfile =
  res
    "DisassociateIamInstanceProfileResponse"
    "fixture/DisassociateIamInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateIamInstanceProfile)

responseTerminateClientVpnConnections :: TerminateClientVpnConnectionsResponse -> TestTree
responseTerminateClientVpnConnections =
  res
    "TerminateClientVpnConnectionsResponse"
    "fixture/TerminateClientVpnConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateClientVpnConnections)

responseCreateTransitGatewayConnect :: CreateTransitGatewayConnectResponse -> TestTree
responseCreateTransitGatewayConnect =
  res
    "CreateTransitGatewayConnectResponse"
    "fixture/CreateTransitGatewayConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayConnect)

responseDisassociateRouteTable :: DisassociateRouteTableResponse -> TestTree
responseDisassociateRouteTable =
  res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRouteTable)

responseGetConsoleScreenshot :: GetConsoleScreenshotResponse -> TestTree
responseGetConsoleScreenshot =
  res
    "GetConsoleScreenshotResponse"
    "fixture/GetConsoleScreenshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConsoleScreenshot)

responseGetFlowLogsIntegrationTemplate :: GetFlowLogsIntegrationTemplateResponse -> TestTree
responseGetFlowLogsIntegrationTemplate =
  res
    "GetFlowLogsIntegrationTemplateResponse"
    "fixture/GetFlowLogsIntegrationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFlowLogsIntegrationTemplate)

responseResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyIdResponse -> TestTree
responseResetEbsDefaultKmsKeyId =
  res
    "ResetEbsDefaultKmsKeyIdResponse"
    "fixture/ResetEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetEbsDefaultKmsKeyId)

responseAssignIpv6Addresses :: AssignIpv6AddressesResponse -> TestTree
responseAssignIpv6Addresses =
  res
    "AssignIpv6AddressesResponse"
    "fixture/AssignIpv6AddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignIpv6Addresses)

responseModifyVpnTunnelOptions :: ModifyVpnTunnelOptionsResponse -> TestTree
responseModifyVpnTunnelOptions =
  res
    "ModifyVpnTunnelOptionsResponse"
    "fixture/ModifyVpnTunnelOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpnTunnelOptions)

responseModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyIdResponse -> TestTree
responseModifyEbsDefaultKmsKeyId =
  res
    "ModifyEbsDefaultKmsKeyIdResponse"
    "fixture/ModifyEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEbsDefaultKmsKeyId)

responseDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
responseDeleteSpotDatafeedSubscription =
  res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSpotDatafeedSubscription)

responseModifyVolume :: ModifyVolumeResponse -> TestTree
responseModifyVolume =
  res
    "ModifyVolumeResponse"
    "fixture/ModifyVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVolume)

responseEnableVpcClassicLink :: EnableVpcClassicLinkResponse -> TestTree
responseEnableVpcClassicLink =
  res
    "EnableVpcClassicLinkResponse"
    "fixture/EnableVpcClassicLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableVpcClassicLink)

responseDescribePlacementGroups :: DescribePlacementGroupsResponse -> TestTree
responseDescribePlacementGroups =
  res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlacementGroups)

responseProvisionByoipCidr :: ProvisionByoipCidrResponse -> TestTree
responseProvisionByoipCidr =
  res
    "ProvisionByoipCidrResponse"
    "fixture/ProvisionByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionByoipCidr)

responseDisassociateEnclaveCertificateIamRole :: DisassociateEnclaveCertificateIamRoleResponse -> TestTree
responseDisassociateEnclaveCertificateIamRole =
  res
    "DisassociateEnclaveCertificateIamRoleResponse"
    "fixture/DisassociateEnclaveCertificateIamRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateEnclaveCertificateIamRole)

responseModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroupResponse -> TestTree
responseModifyAvailabilityZoneGroup =
  res
    "ModifyAvailabilityZoneGroupResponse"
    "fixture/ModifyAvailabilityZoneGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAvailabilityZoneGroup)

responseDescribeStaleSecurityGroups :: DescribeStaleSecurityGroupsResponse -> TestTree
responseDescribeStaleSecurityGroups =
  res
    "DescribeStaleSecurityGroupsResponse"
    "fixture/DescribeStaleSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStaleSecurityGroups)

responseCreateCarrierGateway :: CreateCarrierGatewayResponse -> TestTree
responseCreateCarrierGateway =
  res
    "CreateCarrierGatewayResponse"
    "fixture/CreateCarrierGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCarrierGateway)

responseDescribeExportImageTasks :: DescribeExportImageTasksResponse -> TestTree
responseDescribeExportImageTasks =
  res
    "DescribeExportImageTasksResponse"
    "fixture/DescribeExportImageTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExportImageTasks)

responsePurchaseScheduledInstances :: PurchaseScheduledInstancesResponse -> TestTree
responsePurchaseScheduledInstances =
  res
    "PurchaseScheduledInstancesResponse"
    "fixture/PurchaseScheduledInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseScheduledInstances)

responseEnableVgwRoutePropagation :: EnableVgwRoutePropagationResponse -> TestTree
responseEnableVgwRoutePropagation =
  res
    "EnableVgwRoutePropagationResponse"
    "fixture/EnableVgwRoutePropagationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableVgwRoutePropagation)

responseDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistoryResponse -> TestTree
responseDescribeSpotFleetRequestHistory =
  res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotFleetRequestHistory)

responseModifySnapshotAttribute :: ModifySnapshotAttributeResponse -> TestTree
responseModifySnapshotAttribute =
  res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySnapshotAttribute)

responseDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociationsResponse -> TestTree
responseDescribeIamInstanceProfileAssociations =
  res
    "DescribeIamInstanceProfileAssociationsResponse"
    "fixture/DescribeIamInstanceProfileAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIamInstanceProfileAssociations)

responseDescribeNetworkInsightsPaths :: DescribeNetworkInsightsPathsResponse -> TestTree
responseDescribeNetworkInsightsPaths =
  res
    "DescribeNetworkInsightsPathsResponse"
    "fixture/DescribeNetworkInsightsPathsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInsightsPaths)

responseCreateSnapshot :: Snapshot -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateLocalGatewayRoute :: CreateLocalGatewayRouteResponse -> TestTree
responseCreateLocalGatewayRoute =
  res
    "CreateLocalGatewayRouteResponse"
    "fixture/CreateLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocalGatewayRoute)

responseCreateNetworkAclEntry :: CreateNetworkAclEntryResponse -> TestTree
responseCreateNetworkAclEntry =
  res
    "CreateNetworkAclEntryResponse"
    "fixture/CreateNetworkAclEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkAclEntry)

responseDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachmentsResponse -> TestTree
responseDescribeTransitGatewayAttachments =
  res
    "DescribeTransitGatewayAttachmentsResponse"
    "fixture/DescribeTransitGatewayAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayAttachments)

responseCreateReservedInstancesListing :: CreateReservedInstancesListingResponse -> TestTree
responseCreateReservedInstancesListing =
  res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReservedInstancesListing)

responseDescribeIpv6Pools :: DescribeIpv6PoolsResponse -> TestTree
responseDescribeIpv6Pools =
  res
    "DescribeIpv6PoolsResponse"
    "fixture/DescribeIpv6PoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIpv6Pools)

responseAttachVpnGateway :: AttachVpnGatewayResponse -> TestTree
responseAttachVpnGateway =
  res
    "AttachVpnGatewayResponse"
    "fixture/AttachVpnGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachVpnGateway)

responseDescribeLocalGateways :: DescribeLocalGatewaysResponse -> TestTree
responseDescribeLocalGateways =
  res
    "DescribeLocalGatewaysResponse"
    "fixture/DescribeLocalGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGateways)

responseModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissionsResponse -> TestTree
responseModifyVpcEndpointServicePermissions =
  res
    "ModifyVpcEndpointServicePermissionsResponse"
    "fixture/ModifyVpcEndpointServicePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpointServicePermissions)

responseExportClientVpnClientCertificateRevocationList :: ExportClientVpnClientCertificateRevocationListResponse -> TestTree
responseExportClientVpnClientCertificateRevocationList =
  res
    "ExportClientVpnClientCertificateRevocationListResponse"
    "fixture/ExportClientVpnClientCertificateRevocationListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportClientVpnClientCertificateRevocationList)

responseCreateDhcpOptions :: CreateDhcpOptionsResponse -> TestTree
responseCreateDhcpOptions =
  res
    "CreateDhcpOptionsResponse"
    "fixture/CreateDhcpOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDhcpOptions)

responseRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupSources =
  res
    "RegisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/RegisterTransitGatewayMulticastGroupSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTransitGatewayMulticastGroupSources)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagationsResponse -> TestTree
responseGetTransitGatewayRouteTablePropagations =
  res
    "GetTransitGatewayRouteTablePropagationsResponse"
    "fixture/GetTransitGatewayRouteTablePropagationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayRouteTablePropagations)

responseModifyFpgaImageAttribute :: ModifyFpgaImageAttributeResponse -> TestTree
responseModifyFpgaImageAttribute =
  res
    "ModifyFpgaImageAttributeResponse"
    "fixture/ModifyFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyFpgaImageAttribute)

responseModifyHosts :: ModifyHostsResponse -> TestTree
responseModifyHosts =
  res
    "ModifyHostsResponse"
    "fixture/ModifyHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyHosts)

responseRebootInstances :: RebootInstancesResponse -> TestTree
responseRebootInstances =
  res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootInstances)

responseModifyVpcEndpointServiceConfiguration :: ModifyVpcEndpointServiceConfigurationResponse -> TestTree
responseModifyVpcEndpointServiceConfiguration =
  res
    "ModifyVpcEndpointServiceConfigurationResponse"
    "fixture/ModifyVpcEndpointServiceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpointServiceConfiguration)

responseCreateTransitGateway :: CreateTransitGatewayResponse -> TestTree
responseCreateTransitGateway =
  res
    "CreateTransitGatewayResponse"
    "fixture/CreateTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGateway)

responseUnassignIpv6Addresses :: UnassignIpv6AddressesResponse -> TestTree
responseUnassignIpv6Addresses =
  res
    "UnassignIpv6AddressesResponse"
    "fixture/UnassignIpv6AddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnassignIpv6Addresses)

responseDeleteTrafficMirrorSession :: DeleteTrafficMirrorSessionResponse -> TestTree
responseDeleteTrafficMirrorSession =
  res
    "DeleteTrafficMirrorSessionResponse"
    "fixture/DeleteTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficMirrorSession)

responseCreateManagedPrefixList :: CreateManagedPrefixListResponse -> TestTree
responseCreateManagedPrefixList =
  res
    "CreateManagedPrefixListResponse"
    "fixture/CreateManagedPrefixListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateManagedPrefixList)

responseCreateReplaceRootVolumeTask :: CreateReplaceRootVolumeTaskResponse -> TestTree
responseCreateReplaceRootVolumeTask =
  res
    "CreateReplaceRootVolumeTaskResponse"
    "fixture/CreateReplaceRootVolumeTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplaceRootVolumeTask)

responseAssociateIamInstanceProfile :: AssociateIamInstanceProfileResponse -> TestTree
responseAssociateIamInstanceProfile =
  res
    "AssociateIamInstanceProfileResponse"
    "fixture/AssociateIamInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateIamInstanceProfile)

responseModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecificationResponse -> TestTree
responseModifyDefaultCreditSpecification =
  res
    "ModifyDefaultCreditSpecificationResponse"
    "fixture/ModifyDefaultCreditSpecificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDefaultCreditSpecification)

responseDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGatewayResponse -> TestTree
responseDeleteEgressOnlyInternetGateway =
  res
    "DeleteEgressOnlyInternetGatewayResponse"
    "fixture/DeleteEgressOnlyInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEgressOnlyInternetGateway)

responsePurchaseHostReservation :: PurchaseHostReservationResponse -> TestTree
responsePurchaseHostReservation =
  res
    "PurchaseHostReservationResponse"
    "fixture/PurchaseHostReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseHostReservation)

responseModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachmentResponse -> TestTree
responseModifyTransitGatewayVpcAttachment =
  res
    "ModifyTransitGatewayVpcAttachmentResponse"
    "fixture/ModifyTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTransitGatewayVpcAttachment)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImage)

responseDescribeClassicLinkInstances :: DescribeClassicLinkInstancesResponse -> TestTree
responseDescribeClassicLinkInstances =
  res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClassicLinkInstances)

responseTerminateInstances :: TerminateInstancesResponse -> TestTree
responseTerminateInstances =
  res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateInstances)

responseDescribeStoreImageTasks :: DescribeStoreImageTasksResponse -> TestTree
responseDescribeStoreImageTasks =
  res
    "DescribeStoreImageTasksResponse"
    "fixture/DescribeStoreImageTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStoreImageTasks)

responseGetVpnConnectionDeviceTypes :: GetVpnConnectionDeviceTypesResponse -> TestTree
responseGetVpnConnectionDeviceTypes =
  res
    "GetVpnConnectionDeviceTypesResponse"
    "fixture/GetVpnConnectionDeviceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpnConnectionDeviceTypes)

responseGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferencesResponse -> TestTree
responseGetTransitGatewayPrefixListReferences =
  res
    "GetTransitGatewayPrefixListReferencesResponse"
    "fixture/GetTransitGatewayPrefixListReferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayPrefixListReferences)

responseDescribeKeyPairs :: DescribeKeyPairsResponse -> TestTree
responseDescribeKeyPairs =
  res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKeyPairs)

responseDisableFastSnapshotRestores :: DisableFastSnapshotRestoresResponse -> TestTree
responseDisableFastSnapshotRestores =
  res
    "DisableFastSnapshotRestoresResponse"
    "fixture/DisableFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableFastSnapshotRestores)

responseDescribeLaunchTemplates :: DescribeLaunchTemplatesResponse -> TestTree
responseDescribeLaunchTemplates =
  res
    "DescribeLaunchTemplatesResponse"
    "fixture/DescribeLaunchTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLaunchTemplates)

responseCreateVpnConnectionRoute :: CreateVpnConnectionRouteResponse -> TestTree
responseCreateVpnConnectionRoute =
  res
    "CreateVpnConnectionRouteResponse"
    "fixture/CreateVpnConnectionRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpnConnectionRoute)

responseAssociateRouteTable :: AssociateRouteTableResponse -> TestTree
responseAssociateRouteTable =
  res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateRouteTable)

responseCreateSubnetCidrReservation :: CreateSubnetCidrReservationResponse -> TestTree
responseCreateSubnetCidrReservation =
  res
    "CreateSubnetCidrReservationResponse"
    "fixture/CreateSubnetCidrReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubnetCidrReservation)

responseDescribeVpnGateways :: DescribeVpnGatewaysResponse -> TestTree
responseDescribeVpnGateways =
  res
    "DescribeVpnGatewaysResponse"
    "fixture/DescribeVpnGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpnGateways)

responseModifyVpnConnectionOptions :: ModifyVpnConnectionOptionsResponse -> TestTree
responseModifyVpnConnectionOptions =
  res
    "ModifyVpnConnectionOptionsResponse"
    "fixture/ModifyVpnConnectionOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpnConnectionOptions)

responseGetConsoleOutput :: GetConsoleOutputResponse -> TestTree
responseGetConsoleOutput =
  res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConsoleOutput)

responseDescribeHosts :: DescribeHostsResponse -> TestTree
responseDescribeHosts =
  res
    "DescribeHostsResponse"
    "fixture/DescribeHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHosts)

responseDescribeImageAttribute :: DescribeImageAttributeResponse -> TestTree
responseDescribeImageAttribute =
  res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageAttribute)

responseModifyIdFormat :: ModifyIdFormatResponse -> TestTree
responseModifyIdFormat =
  res
    "ModifyIdFormatResponse"
    "fixture/ModifyIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyIdFormat)

responseRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupMembers =
  res
    "RegisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/RegisterTransitGatewayMulticastGroupMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTransitGatewayMulticastGroupMembers)

responseDeleteManagedPrefixList :: DeleteManagedPrefixListResponse -> TestTree
responseDeleteManagedPrefixList =
  res
    "DeleteManagedPrefixListResponse"
    "fixture/DeleteManagedPrefixListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteManagedPrefixList)

responseDeleteRouteTable :: DeleteRouteTableResponse -> TestTree
responseDeleteRouteTable =
  res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteTable)

responseResetImageAttribute :: ResetImageAttributeResponse -> TestTree
responseResetImageAttribute =
  res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetImageAttribute)

responseModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReferenceResponse -> TestTree
responseModifyTransitGatewayPrefixListReference =
  res
    "ModifyTransitGatewayPrefixListReferenceResponse"
    "fixture/ModifyTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTransitGatewayPrefixListReference)

responseDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTablesResponse -> TestTree
responseDescribeTransitGatewayRouteTables =
  res
    "DescribeTransitGatewayRouteTablesResponse"
    "fixture/DescribeTransitGatewayRouteTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayRouteTables)

responseCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGatewayResponse -> TestTree
responseCreateEgressOnlyInternetGateway =
  res
    "CreateEgressOnlyInternetGatewayResponse"
    "fixture/CreateEgressOnlyInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEgressOnlyInternetGateway)

responseDescribeReservedInstancesModifications :: DescribeReservedInstancesModificationsResponse -> TestTree
responseDescribeReservedInstancesModifications =
  res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstancesModifications)

responseDescribeSpotInstanceRequests :: DescribeSpotInstanceRequestsResponse -> TestTree
responseDescribeSpotInstanceRequests =
  res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotInstanceRequests)

responseRevokeClientVpnIngress :: RevokeClientVpnIngressResponse -> TestTree
responseRevokeClientVpnIngress =
  res
    "RevokeClientVpnIngressResponse"
    "fixture/RevokeClientVpnIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeClientVpnIngress)

responseUnassignPrivateIpAddresses :: UnassignPrivateIpAddressesResponse -> TestTree
responseUnassignPrivateIpAddresses =
  res
    "UnassignPrivateIpAddressesResponse"
    "fixture/UnassignPrivateIpAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnassignPrivateIpAddresses)

responseDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissionsResponse -> TestTree
responseDescribeNetworkInterfacePermissions =
  res
    "DescribeNetworkInterfacePermissionsResponse"
    "fixture/DescribeNetworkInterfacePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInterfacePermissions)

responseEnableFastSnapshotRestores :: EnableFastSnapshotRestoresResponse -> TestTree
responseEnableFastSnapshotRestores =
  res
    "EnableFastSnapshotRestoresResponse"
    "fixture/EnableFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableFastSnapshotRestores)

responseDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissionsResponse -> TestTree
responseDescribeVpcEndpointServicePermissions =
  res
    "DescribeVpcEndpointServicePermissionsResponse"
    "fixture/DescribeVpcEndpointServicePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointServicePermissions)

responseDeleteDhcpOptions :: DeleteDhcpOptionsResponse -> TestTree
responseDeleteDhcpOptions =
  res
    "DeleteDhcpOptionsResponse"
    "fixture/DeleteDhcpOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDhcpOptions)

responseCreateRestoreImageTask :: CreateRestoreImageTaskResponse -> TestTree
responseCreateRestoreImageTask =
  res
    "CreateRestoreImageTaskResponse"
    "fixture/CreateRestoreImageTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRestoreImageTask)

responseRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributesResponse -> TestTree
responseRegisterInstanceEventNotificationAttributes =
  res
    "RegisterInstanceEventNotificationAttributesResponse"
    "fixture/RegisterInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterInstanceEventNotificationAttributes)

responseGetVpnConnectionDeviceSampleConfiguration :: GetVpnConnectionDeviceSampleConfigurationResponse -> TestTree
responseGetVpnConnectionDeviceSampleConfiguration =
  res
    "GetVpnConnectionDeviceSampleConfigurationResponse"
    "fixture/GetVpnConnectionDeviceSampleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpnConnectionDeviceSampleConfiguration)

responseDeleteSubnetCidrReservation :: DeleteSubnetCidrReservationResponse -> TestTree
responseDeleteSubnetCidrReservation =
  res
    "DeleteSubnetCidrReservationResponse"
    "fixture/DeleteSubnetCidrReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubnetCidrReservation)

responseDescribeNetworkAcls :: DescribeNetworkAclsResponse -> TestTree
responseDescribeNetworkAcls =
  res
    "DescribeNetworkAclsResponse"
    "fixture/DescribeNetworkAclsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkAcls)

responseCancelImportTask :: CancelImportTaskResponse -> TestTree
responseCancelImportTask =
  res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelImportTask)

responseDetachClassicLinkVpc :: DetachClassicLinkVpcResponse -> TestTree
responseDetachClassicLinkVpc =
  res
    "DetachClassicLinkVpcResponse"
    "fixture/DetachClassicLinkVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachClassicLinkVpc)

responseCreateCapacityReservationFleet :: CreateCapacityReservationFleetResponse -> TestTree
responseCreateCapacityReservationFleet =
  res
    "CreateCapacityReservationFleetResponse"
    "fixture/CreateCapacityReservationFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCapacityReservationFleet)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegions)

responseMonitorInstances :: MonitorInstancesResponse -> TestTree
responseMonitorInstances =
  res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MonitorInstances)

responseRejectTransitGatewayMulticastDomainAssociations :: RejectTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseRejectTransitGatewayMulticastDomainAssociations =
  res
    "RejectTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/RejectTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectTransitGatewayMulticastDomainAssociations)

responseAcceptTransitGatewayMulticastDomainAssociations :: AcceptTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseAcceptTransitGatewayMulticastDomainAssociations =
  res
    "AcceptTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/AcceptTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptTransitGatewayMulticastDomainAssociations)

responseSearchLocalGatewayRoutes :: SearchLocalGatewayRoutesResponse -> TestTree
responseSearchLocalGatewayRoutes =
  res
    "SearchLocalGatewayRoutesResponse"
    "fixture/SearchLocalGatewayRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchLocalGatewayRoutes)

responseDeleteClientVpnRoute :: DeleteClientVpnRouteResponse -> TestTree
responseDeleteClientVpnRoute =
  res
    "DeleteClientVpnRouteResponse"
    "fixture/DeleteClientVpnRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClientVpnRoute)

responseAcceptVpcPeeringConnection :: AcceptVpcPeeringConnectionResponse -> TestTree
responseAcceptVpcPeeringConnection =
  res
    "AcceptVpcPeeringConnectionResponse"
    "fixture/AcceptVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptVpcPeeringConnection)

responseImportSnapshot :: ImportSnapshotResponse -> TestTree
responseImportSnapshot =
  res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportSnapshot)

responseDescribeAddressesAttribute :: DescribeAddressesAttributeResponse -> TestTree
responseDescribeAddressesAttribute =
  res
    "DescribeAddressesAttributeResponse"
    "fixture/DescribeAddressesAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddressesAttribute)

responseDescribeVolumeStatus :: DescribeVolumeStatusResponse -> TestTree
responseDescribeVolumeStatus =
  res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumeStatus)

responseDescribeReplaceRootVolumeTasks :: DescribeReplaceRootVolumeTasksResponse -> TestTree
responseDescribeReplaceRootVolumeTasks =
  res
    "DescribeReplaceRootVolumeTasksResponse"
    "fixture/DescribeReplaceRootVolumeTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplaceRootVolumeTasks)

responseModifyInstanceEventWindow :: ModifyInstanceEventWindowResponse -> TestTree
responseModifyInstanceEventWindow =
  res
    "ModifyInstanceEventWindowResponse"
    "fixture/ModifyInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceEventWindow)

responseDescribeRouteTables :: DescribeRouteTablesResponse -> TestTree
responseDescribeRouteTables =
  res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRouteTables)

responseDescribeAvailabilityZones :: DescribeAvailabilityZonesResponse -> TestTree
responseDescribeAvailabilityZones =
  res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAvailabilityZones)

responseModifyVpcAttribute :: ModifyVpcAttributeResponse -> TestTree
responseModifyVpcAttribute =
  res
    "ModifyVpcAttributeResponse"
    "fixture/ModifyVpcAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcAttribute)

responseDescribeClientVpnConnections :: DescribeClientVpnConnectionsResponse -> TestTree
responseDescribeClientVpnConnections =
  res
    "DescribeClientVpnConnectionsResponse"
    "fixture/DescribeClientVpnConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnConnections)

responseDescribeFleetHistory :: DescribeFleetHistoryResponse -> TestTree
responseDescribeFleetHistory =
  res
    "DescribeFleetHistoryResponse"
    "fixture/DescribeFleetHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetHistory)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImages)

responseDescribeElasticGpus :: DescribeElasticGpusResponse -> TestTree
responseDescribeElasticGpus =
  res
    "DescribeElasticGpusResponse"
    "fixture/DescribeElasticGpusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticGpus)

responseDisassociateTrunkInterface :: DisassociateTrunkInterfaceResponse -> TestTree
responseDisassociateTrunkInterface =
  res
    "DisassociateTrunkInterfaceResponse"
    "fixture/DisassociateTrunkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTrunkInterface)

responseRestoreAddressToClassic :: RestoreAddressToClassicResponse -> TestTree
responseRestoreAddressToClassic =
  res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreAddressToClassic)

responseDescribeManagedPrefixLists :: DescribeManagedPrefixListsResponse -> TestTree
responseDescribeManagedPrefixLists =
  res
    "DescribeManagedPrefixListsResponse"
    "fixture/DescribeManagedPrefixListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeManagedPrefixLists)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeyPair)

responseGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuoteResponse -> TestTree
responseGetReservedInstancesExchangeQuote =
  res
    "GetReservedInstancesExchangeQuoteResponse"
    "fixture/GetReservedInstancesExchangeQuoteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservedInstancesExchangeQuote)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVolume)

responseDeprovisionByoipCidr :: DeprovisionByoipCidrResponse -> TestTree
responseDeprovisionByoipCidr =
  res
    "DeprovisionByoipCidrResponse"
    "fixture/DeprovisionByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprovisionByoipCidr)

responseGetSerialConsoleAccessStatus :: GetSerialConsoleAccessStatusResponse -> TestTree
responseGetSerialConsoleAccessStatus =
  res
    "GetSerialConsoleAccessStatusResponse"
    "fixture/GetSerialConsoleAccessStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSerialConsoleAccessStatus)

responseDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurationsResponse -> TestTree
responseDeleteVpcEndpointServiceConfigurations =
  res
    "DeleteVpcEndpointServiceConfigurationsResponse"
    "fixture/DeleteVpcEndpointServiceConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcEndpointServiceConfigurations)

responseDescribeSpotFleetInstances :: DescribeSpotFleetInstancesResponse -> TestTree
responseDescribeSpotFleetInstances =
  res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotFleetInstances)
