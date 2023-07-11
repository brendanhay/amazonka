{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EC2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestAcceptAddressTransfer $
--             newAcceptAddressTransfer
--
--         , requestAcceptReservedInstancesExchangeQuote $
--             newAcceptReservedInstancesExchangeQuote
--
--         , requestAcceptTransitGatewayMulticastDomainAssociations $
--             newAcceptTransitGatewayMulticastDomainAssociations
--
--         , requestAcceptTransitGatewayPeeringAttachment $
--             newAcceptTransitGatewayPeeringAttachment
--
--         , requestAcceptTransitGatewayVpcAttachment $
--             newAcceptTransitGatewayVpcAttachment
--
--         , requestAcceptVpcEndpointConnections $
--             newAcceptVpcEndpointConnections
--
--         , requestAcceptVpcPeeringConnection $
--             newAcceptVpcPeeringConnection
--
--         , requestAdvertiseByoipCidr $
--             newAdvertiseByoipCidr
--
--         , requestAllocateAddress $
--             newAllocateAddress
--
--         , requestAllocateHosts $
--             newAllocateHosts
--
--         , requestAllocateIpamPoolCidr $
--             newAllocateIpamPoolCidr
--
--         , requestApplySecurityGroupsToClientVpnTargetNetwork $
--             newApplySecurityGroupsToClientVpnTargetNetwork
--
--         , requestAssignIpv6Addresses $
--             newAssignIpv6Addresses
--
--         , requestAssignPrivateIpAddresses $
--             newAssignPrivateIpAddresses
--
--         , requestAssociateAddress $
--             newAssociateAddress
--
--         , requestAssociateClientVpnTargetNetwork $
--             newAssociateClientVpnTargetNetwork
--
--         , requestAssociateDhcpOptions $
--             newAssociateDhcpOptions
--
--         , requestAssociateEnclaveCertificateIamRole $
--             newAssociateEnclaveCertificateIamRole
--
--         , requestAssociateIamInstanceProfile $
--             newAssociateIamInstanceProfile
--
--         , requestAssociateInstanceEventWindow $
--             newAssociateInstanceEventWindow
--
--         , requestAssociateRouteTable $
--             newAssociateRouteTable
--
--         , requestAssociateSubnetCidrBlock $
--             newAssociateSubnetCidrBlock
--
--         , requestAssociateTransitGatewayMulticastDomain $
--             newAssociateTransitGatewayMulticastDomain
--
--         , requestAssociateTransitGatewayPolicyTable $
--             newAssociateTransitGatewayPolicyTable
--
--         , requestAssociateTransitGatewayRouteTable $
--             newAssociateTransitGatewayRouteTable
--
--         , requestAssociateTrunkInterface $
--             newAssociateTrunkInterface
--
--         , requestAssociateVpcCidrBlock $
--             newAssociateVpcCidrBlock
--
--         , requestAttachClassicLinkVpc $
--             newAttachClassicLinkVpc
--
--         , requestAttachInternetGateway $
--             newAttachInternetGateway
--
--         , requestAttachNetworkInterface $
--             newAttachNetworkInterface
--
--         , requestAttachVerifiedAccessTrustProvider $
--             newAttachVerifiedAccessTrustProvider
--
--         , requestAttachVolume $
--             newAttachVolume
--
--         , requestAttachVpnGateway $
--             newAttachVpnGateway
--
--         , requestAuthorizeClientVpnIngress $
--             newAuthorizeClientVpnIngress
--
--         , requestAuthorizeSecurityGroupEgress $
--             newAuthorizeSecurityGroupEgress
--
--         , requestAuthorizeSecurityGroupIngress $
--             newAuthorizeSecurityGroupIngress
--
--         , requestBundleInstance $
--             newBundleInstance
--
--         , requestCancelBundleTask $
--             newCancelBundleTask
--
--         , requestCancelCapacityReservation $
--             newCancelCapacityReservation
--
--         , requestCancelCapacityReservationFleets $
--             newCancelCapacityReservationFleets
--
--         , requestCancelConversionTask $
--             newCancelConversionTask
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestCancelImageLaunchPermission $
--             newCancelImageLaunchPermission
--
--         , requestCancelImportTask $
--             newCancelImportTask
--
--         , requestCancelReservedInstancesListing $
--             newCancelReservedInstancesListing
--
--         , requestCancelSpotFleetRequests $
--             newCancelSpotFleetRequests
--
--         , requestCancelSpotInstanceRequests $
--             newCancelSpotInstanceRequests
--
--         , requestConfirmProductInstance $
--             newConfirmProductInstance
--
--         , requestCopyFpgaImage $
--             newCopyFpgaImage
--
--         , requestCopyImage $
--             newCopyImage
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestCreateCapacityReservation $
--             newCreateCapacityReservation
--
--         , requestCreateCapacityReservationFleet $
--             newCreateCapacityReservationFleet
--
--         , requestCreateCarrierGateway $
--             newCreateCarrierGateway
--
--         , requestCreateClientVpnEndpoint $
--             newCreateClientVpnEndpoint
--
--         , requestCreateClientVpnRoute $
--             newCreateClientVpnRoute
--
--         , requestCreateCoipCidr $
--             newCreateCoipCidr
--
--         , requestCreateCoipPool $
--             newCreateCoipPool
--
--         , requestCreateCustomerGateway $
--             newCreateCustomerGateway
--
--         , requestCreateDefaultSubnet $
--             newCreateDefaultSubnet
--
--         , requestCreateDefaultVpc $
--             newCreateDefaultVpc
--
--         , requestCreateDhcpOptions $
--             newCreateDhcpOptions
--
--         , requestCreateEgressOnlyInternetGateway $
--             newCreateEgressOnlyInternetGateway
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestCreateFlowLogs $
--             newCreateFlowLogs
--
--         , requestCreateFpgaImage $
--             newCreateFpgaImage
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestCreateInstanceEventWindow $
--             newCreateInstanceEventWindow
--
--         , requestCreateInstanceExportTask $
--             newCreateInstanceExportTask
--
--         , requestCreateInternetGateway $
--             newCreateInternetGateway
--
--         , requestCreateIpam $
--             newCreateIpam
--
--         , requestCreateIpamPool $
--             newCreateIpamPool
--
--         , requestCreateIpamScope $
--             newCreateIpamScope
--
--         , requestCreateKeyPair $
--             newCreateKeyPair
--
--         , requestCreateLaunchTemplate $
--             newCreateLaunchTemplate
--
--         , requestCreateLaunchTemplateVersion $
--             newCreateLaunchTemplateVersion
--
--         , requestCreateLocalGatewayRoute $
--             newCreateLocalGatewayRoute
--
--         , requestCreateLocalGatewayRouteTable $
--             newCreateLocalGatewayRouteTable
--
--         , requestCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation $
--             newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
--
--         , requestCreateLocalGatewayRouteTableVpcAssociation $
--             newCreateLocalGatewayRouteTableVpcAssociation
--
--         , requestCreateManagedPrefixList $
--             newCreateManagedPrefixList
--
--         , requestCreateNatGateway $
--             newCreateNatGateway
--
--         , requestCreateNetworkAcl $
--             newCreateNetworkAcl
--
--         , requestCreateNetworkAclEntry $
--             newCreateNetworkAclEntry
--
--         , requestCreateNetworkInsightsAccessScope $
--             newCreateNetworkInsightsAccessScope
--
--         , requestCreateNetworkInsightsPath $
--             newCreateNetworkInsightsPath
--
--         , requestCreateNetworkInterface $
--             newCreateNetworkInterface
--
--         , requestCreateNetworkInterfacePermission $
--             newCreateNetworkInterfacePermission
--
--         , requestCreatePlacementGroup $
--             newCreatePlacementGroup
--
--         , requestCreatePublicIpv4Pool $
--             newCreatePublicIpv4Pool
--
--         , requestCreateReplaceRootVolumeTask $
--             newCreateReplaceRootVolumeTask
--
--         , requestCreateReservedInstancesListing $
--             newCreateReservedInstancesListing
--
--         , requestCreateRestoreImageTask $
--             newCreateRestoreImageTask
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestCreateRouteTable $
--             newCreateRouteTable
--
--         , requestCreateSecurityGroup $
--             newCreateSecurityGroup
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateSnapshots $
--             newCreateSnapshots
--
--         , requestCreateSpotDatafeedSubscription $
--             newCreateSpotDatafeedSubscription
--
--         , requestCreateStoreImageTask $
--             newCreateStoreImageTask
--
--         , requestCreateSubnet $
--             newCreateSubnet
--
--         , requestCreateSubnetCidrReservation $
--             newCreateSubnetCidrReservation
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestCreateTrafficMirrorFilter $
--             newCreateTrafficMirrorFilter
--
--         , requestCreateTrafficMirrorFilterRule $
--             newCreateTrafficMirrorFilterRule
--
--         , requestCreateTrafficMirrorSession $
--             newCreateTrafficMirrorSession
--
--         , requestCreateTrafficMirrorTarget $
--             newCreateTrafficMirrorTarget
--
--         , requestCreateTransitGateway $
--             newCreateTransitGateway
--
--         , requestCreateTransitGatewayConnect $
--             newCreateTransitGatewayConnect
--
--         , requestCreateTransitGatewayConnectPeer $
--             newCreateTransitGatewayConnectPeer
--
--         , requestCreateTransitGatewayMulticastDomain $
--             newCreateTransitGatewayMulticastDomain
--
--         , requestCreateTransitGatewayPeeringAttachment $
--             newCreateTransitGatewayPeeringAttachment
--
--         , requestCreateTransitGatewayPolicyTable $
--             newCreateTransitGatewayPolicyTable
--
--         , requestCreateTransitGatewayPrefixListReference $
--             newCreateTransitGatewayPrefixListReference
--
--         , requestCreateTransitGatewayRoute $
--             newCreateTransitGatewayRoute
--
--         , requestCreateTransitGatewayRouteTable $
--             newCreateTransitGatewayRouteTable
--
--         , requestCreateTransitGatewayRouteTableAnnouncement $
--             newCreateTransitGatewayRouteTableAnnouncement
--
--         , requestCreateTransitGatewayVpcAttachment $
--             newCreateTransitGatewayVpcAttachment
--
--         , requestCreateVerifiedAccessEndpoint $
--             newCreateVerifiedAccessEndpoint
--
--         , requestCreateVerifiedAccessGroup $
--             newCreateVerifiedAccessGroup
--
--         , requestCreateVerifiedAccessInstance $
--             newCreateVerifiedAccessInstance
--
--         , requestCreateVerifiedAccessTrustProvider $
--             newCreateVerifiedAccessTrustProvider
--
--         , requestCreateVolume $
--             newCreateVolume
--
--         , requestCreateVpc $
--             newCreateVpc
--
--         , requestCreateVpcEndpoint $
--             newCreateVpcEndpoint
--
--         , requestCreateVpcEndpointConnectionNotification $
--             newCreateVpcEndpointConnectionNotification
--
--         , requestCreateVpcEndpointServiceConfiguration $
--             newCreateVpcEndpointServiceConfiguration
--
--         , requestCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnection
--
--         , requestCreateVpnConnection $
--             newCreateVpnConnection
--
--         , requestCreateVpnConnectionRoute $
--             newCreateVpnConnectionRoute
--
--         , requestCreateVpnGateway $
--             newCreateVpnGateway
--
--         , requestDeleteCarrierGateway $
--             newDeleteCarrierGateway
--
--         , requestDeleteClientVpnEndpoint $
--             newDeleteClientVpnEndpoint
--
--         , requestDeleteClientVpnRoute $
--             newDeleteClientVpnRoute
--
--         , requestDeleteCoipCidr $
--             newDeleteCoipCidr
--
--         , requestDeleteCoipPool $
--             newDeleteCoipPool
--
--         , requestDeleteCustomerGateway $
--             newDeleteCustomerGateway
--
--         , requestDeleteDhcpOptions $
--             newDeleteDhcpOptions
--
--         , requestDeleteEgressOnlyInternetGateway $
--             newDeleteEgressOnlyInternetGateway
--
--         , requestDeleteFleets $
--             newDeleteFleets
--
--         , requestDeleteFlowLogs $
--             newDeleteFlowLogs
--
--         , requestDeleteFpgaImage $
--             newDeleteFpgaImage
--
--         , requestDeleteInstanceEventWindow $
--             newDeleteInstanceEventWindow
--
--         , requestDeleteInternetGateway $
--             newDeleteInternetGateway
--
--         , requestDeleteIpam $
--             newDeleteIpam
--
--         , requestDeleteIpamPool $
--             newDeleteIpamPool
--
--         , requestDeleteIpamScope $
--             newDeleteIpamScope
--
--         , requestDeleteKeyPair $
--             newDeleteKeyPair
--
--         , requestDeleteLaunchTemplate $
--             newDeleteLaunchTemplate
--
--         , requestDeleteLaunchTemplateVersions $
--             newDeleteLaunchTemplateVersions
--
--         , requestDeleteLocalGatewayRoute $
--             newDeleteLocalGatewayRoute
--
--         , requestDeleteLocalGatewayRouteTable $
--             newDeleteLocalGatewayRouteTable
--
--         , requestDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation $
--             newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
--
--         , requestDeleteLocalGatewayRouteTableVpcAssociation $
--             newDeleteLocalGatewayRouteTableVpcAssociation
--
--         , requestDeleteManagedPrefixList $
--             newDeleteManagedPrefixList
--
--         , requestDeleteNatGateway $
--             newDeleteNatGateway
--
--         , requestDeleteNetworkAcl $
--             newDeleteNetworkAcl
--
--         , requestDeleteNetworkAclEntry $
--             newDeleteNetworkAclEntry
--
--         , requestDeleteNetworkInsightsAccessScope $
--             newDeleteNetworkInsightsAccessScope
--
--         , requestDeleteNetworkInsightsAccessScopeAnalysis $
--             newDeleteNetworkInsightsAccessScopeAnalysis
--
--         , requestDeleteNetworkInsightsAnalysis $
--             newDeleteNetworkInsightsAnalysis
--
--         , requestDeleteNetworkInsightsPath $
--             newDeleteNetworkInsightsPath
--
--         , requestDeleteNetworkInterface $
--             newDeleteNetworkInterface
--
--         , requestDeleteNetworkInterfacePermission $
--             newDeleteNetworkInterfacePermission
--
--         , requestDeletePlacementGroup $
--             newDeletePlacementGroup
--
--         , requestDeletePublicIpv4Pool $
--             newDeletePublicIpv4Pool
--
--         , requestDeleteQueuedReservedInstances $
--             newDeleteQueuedReservedInstances
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestDeleteRouteTable $
--             newDeleteRouteTable
--
--         , requestDeleteSecurityGroup $
--             newDeleteSecurityGroup
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDeleteSpotDatafeedSubscription $
--             newDeleteSpotDatafeedSubscription
--
--         , requestDeleteSubnet $
--             newDeleteSubnet
--
--         , requestDeleteSubnetCidrReservation $
--             newDeleteSubnetCidrReservation
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDeleteTrafficMirrorFilter $
--             newDeleteTrafficMirrorFilter
--
--         , requestDeleteTrafficMirrorFilterRule $
--             newDeleteTrafficMirrorFilterRule
--
--         , requestDeleteTrafficMirrorSession $
--             newDeleteTrafficMirrorSession
--
--         , requestDeleteTrafficMirrorTarget $
--             newDeleteTrafficMirrorTarget
--
--         , requestDeleteTransitGateway $
--             newDeleteTransitGateway
--
--         , requestDeleteTransitGatewayConnect $
--             newDeleteTransitGatewayConnect
--
--         , requestDeleteTransitGatewayConnectPeer $
--             newDeleteTransitGatewayConnectPeer
--
--         , requestDeleteTransitGatewayMulticastDomain $
--             newDeleteTransitGatewayMulticastDomain
--
--         , requestDeleteTransitGatewayPeeringAttachment $
--             newDeleteTransitGatewayPeeringAttachment
--
--         , requestDeleteTransitGatewayPolicyTable $
--             newDeleteTransitGatewayPolicyTable
--
--         , requestDeleteTransitGatewayPrefixListReference $
--             newDeleteTransitGatewayPrefixListReference
--
--         , requestDeleteTransitGatewayRoute $
--             newDeleteTransitGatewayRoute
--
--         , requestDeleteTransitGatewayRouteTable $
--             newDeleteTransitGatewayRouteTable
--
--         , requestDeleteTransitGatewayRouteTableAnnouncement $
--             newDeleteTransitGatewayRouteTableAnnouncement
--
--         , requestDeleteTransitGatewayVpcAttachment $
--             newDeleteTransitGatewayVpcAttachment
--
--         , requestDeleteVerifiedAccessEndpoint $
--             newDeleteVerifiedAccessEndpoint
--
--         , requestDeleteVerifiedAccessGroup $
--             newDeleteVerifiedAccessGroup
--
--         , requestDeleteVerifiedAccessInstance $
--             newDeleteVerifiedAccessInstance
--
--         , requestDeleteVerifiedAccessTrustProvider $
--             newDeleteVerifiedAccessTrustProvider
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--         , requestDeleteVpc $
--             newDeleteVpc
--
--         , requestDeleteVpcEndpointConnectionNotifications $
--             newDeleteVpcEndpointConnectionNotifications
--
--         , requestDeleteVpcEndpointServiceConfigurations $
--             newDeleteVpcEndpointServiceConfigurations
--
--         , requestDeleteVpcEndpoints $
--             newDeleteVpcEndpoints
--
--         , requestDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnection
--
--         , requestDeleteVpnConnection $
--             newDeleteVpnConnection
--
--         , requestDeleteVpnConnectionRoute $
--             newDeleteVpnConnectionRoute
--
--         , requestDeleteVpnGateway $
--             newDeleteVpnGateway
--
--         , requestDeprovisionByoipCidr $
--             newDeprovisionByoipCidr
--
--         , requestDeprovisionIpamPoolCidr $
--             newDeprovisionIpamPoolCidr
--
--         , requestDeprovisionPublicIpv4PoolCidr $
--             newDeprovisionPublicIpv4PoolCidr
--
--         , requestDeregisterImage $
--             newDeregisterImage
--
--         , requestDeregisterInstanceEventNotificationAttributes $
--             newDeregisterInstanceEventNotificationAttributes
--
--         , requestDeregisterTransitGatewayMulticastGroupMembers $
--             newDeregisterTransitGatewayMulticastGroupMembers
--
--         , requestDeregisterTransitGatewayMulticastGroupSources $
--             newDeregisterTransitGatewayMulticastGroupSources
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeAddressTransfers $
--             newDescribeAddressTransfers
--
--         , requestDescribeAddresses $
--             newDescribeAddresses
--
--         , requestDescribeAddressesAttribute $
--             newDescribeAddressesAttribute
--
--         , requestDescribeAggregateIdFormat $
--             newDescribeAggregateIdFormat
--
--         , requestDescribeAvailabilityZones $
--             newDescribeAvailabilityZones
--
--         , requestDescribeAwsNetworkPerformanceMetricSubscriptions $
--             newDescribeAwsNetworkPerformanceMetricSubscriptions
--
--         , requestDescribeBundleTasks $
--             newDescribeBundleTasks
--
--         , requestDescribeByoipCidrs $
--             newDescribeByoipCidrs
--
--         , requestDescribeCapacityReservationFleets $
--             newDescribeCapacityReservationFleets
--
--         , requestDescribeCapacityReservations $
--             newDescribeCapacityReservations
--
--         , requestDescribeCarrierGateways $
--             newDescribeCarrierGateways
--
--         , requestDescribeClassicLinkInstances $
--             newDescribeClassicLinkInstances
--
--         , requestDescribeClientVpnAuthorizationRules $
--             newDescribeClientVpnAuthorizationRules
--
--         , requestDescribeClientVpnConnections $
--             newDescribeClientVpnConnections
--
--         , requestDescribeClientVpnEndpoints $
--             newDescribeClientVpnEndpoints
--
--         , requestDescribeClientVpnRoutes $
--             newDescribeClientVpnRoutes
--
--         , requestDescribeClientVpnTargetNetworks $
--             newDescribeClientVpnTargetNetworks
--
--         , requestDescribeCoipPools $
--             newDescribeCoipPools
--
--         , requestDescribeConversionTasks $
--             newDescribeConversionTasks
--
--         , requestDescribeCustomerGateways $
--             newDescribeCustomerGateways
--
--         , requestDescribeDhcpOptions $
--             newDescribeDhcpOptions
--
--         , requestDescribeEgressOnlyInternetGateways $
--             newDescribeEgressOnlyInternetGateways
--
--         , requestDescribeElasticGpus $
--             newDescribeElasticGpus
--
--         , requestDescribeExportImageTasks $
--             newDescribeExportImageTasks
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestDescribeFastLaunchImages $
--             newDescribeFastLaunchImages
--
--         , requestDescribeFastSnapshotRestores $
--             newDescribeFastSnapshotRestores
--
--         , requestDescribeFleetHistory $
--             newDescribeFleetHistory
--
--         , requestDescribeFleetInstances $
--             newDescribeFleetInstances
--
--         , requestDescribeFleets $
--             newDescribeFleets
--
--         , requestDescribeFlowLogs $
--             newDescribeFlowLogs
--
--         , requestDescribeFpgaImageAttribute $
--             newDescribeFpgaImageAttribute
--
--         , requestDescribeFpgaImages $
--             newDescribeFpgaImages
--
--         , requestDescribeHostReservationOfferings $
--             newDescribeHostReservationOfferings
--
--         , requestDescribeHostReservations $
--             newDescribeHostReservations
--
--         , requestDescribeHosts $
--             newDescribeHosts
--
--         , requestDescribeIamInstanceProfileAssociations $
--             newDescribeIamInstanceProfileAssociations
--
--         , requestDescribeIdFormat $
--             newDescribeIdFormat
--
--         , requestDescribeIdentityIdFormat $
--             newDescribeIdentityIdFormat
--
--         , requestDescribeImageAttribute $
--             newDescribeImageAttribute
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestDescribeImportImageTasks $
--             newDescribeImportImageTasks
--
--         , requestDescribeImportSnapshotTasks $
--             newDescribeImportSnapshotTasks
--
--         , requestDescribeInstanceAttribute $
--             newDescribeInstanceAttribute
--
--         , requestDescribeInstanceCreditSpecifications $
--             newDescribeInstanceCreditSpecifications
--
--         , requestDescribeInstanceEventNotificationAttributes $
--             newDescribeInstanceEventNotificationAttributes
--
--         , requestDescribeInstanceEventWindows $
--             newDescribeInstanceEventWindows
--
--         , requestDescribeInstanceStatus $
--             newDescribeInstanceStatus
--
--         , requestDescribeInstanceTypeOfferings $
--             newDescribeInstanceTypeOfferings
--
--         , requestDescribeInstanceTypes $
--             newDescribeInstanceTypes
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDescribeInternetGateways $
--             newDescribeInternetGateways
--
--         , requestDescribeIpamPools $
--             newDescribeIpamPools
--
--         , requestDescribeIpamScopes $
--             newDescribeIpamScopes
--
--         , requestDescribeIpams $
--             newDescribeIpams
--
--         , requestDescribeIpv6Pools $
--             newDescribeIpv6Pools
--
--         , requestDescribeKeyPairs $
--             newDescribeKeyPairs
--
--         , requestDescribeLaunchTemplateVersions $
--             newDescribeLaunchTemplateVersions
--
--         , requestDescribeLaunchTemplates $
--             newDescribeLaunchTemplates
--
--         , requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
--
--         , requestDescribeLocalGatewayRouteTableVpcAssociations $
--             newDescribeLocalGatewayRouteTableVpcAssociations
--
--         , requestDescribeLocalGatewayRouteTables $
--             newDescribeLocalGatewayRouteTables
--
--         , requestDescribeLocalGatewayVirtualInterfaceGroups $
--             newDescribeLocalGatewayVirtualInterfaceGroups
--
--         , requestDescribeLocalGatewayVirtualInterfaces $
--             newDescribeLocalGatewayVirtualInterfaces
--
--         , requestDescribeLocalGateways $
--             newDescribeLocalGateways
--
--         , requestDescribeManagedPrefixLists $
--             newDescribeManagedPrefixLists
--
--         , requestDescribeMovingAddresses $
--             newDescribeMovingAddresses
--
--         , requestDescribeNatGateways $
--             newDescribeNatGateways
--
--         , requestDescribeNetworkAcls $
--             newDescribeNetworkAcls
--
--         , requestDescribeNetworkInsightsAccessScopeAnalyses $
--             newDescribeNetworkInsightsAccessScopeAnalyses
--
--         , requestDescribeNetworkInsightsAccessScopes $
--             newDescribeNetworkInsightsAccessScopes
--
--         , requestDescribeNetworkInsightsAnalyses $
--             newDescribeNetworkInsightsAnalyses
--
--         , requestDescribeNetworkInsightsPaths $
--             newDescribeNetworkInsightsPaths
--
--         , requestDescribeNetworkInterfaceAttribute $
--             newDescribeNetworkInterfaceAttribute
--
--         , requestDescribeNetworkInterfacePermissions $
--             newDescribeNetworkInterfacePermissions
--
--         , requestDescribeNetworkInterfaces $
--             newDescribeNetworkInterfaces
--
--         , requestDescribePlacementGroups $
--             newDescribePlacementGroups
--
--         , requestDescribePrefixLists $
--             newDescribePrefixLists
--
--         , requestDescribePrincipalIdFormat $
--             newDescribePrincipalIdFormat
--
--         , requestDescribePublicIpv4Pools $
--             newDescribePublicIpv4Pools
--
--         , requestDescribeRegions $
--             newDescribeRegions
--
--         , requestDescribeReplaceRootVolumeTasks $
--             newDescribeReplaceRootVolumeTasks
--
--         , requestDescribeReservedInstances $
--             newDescribeReservedInstances
--
--         , requestDescribeReservedInstancesListings $
--             newDescribeReservedInstancesListings
--
--         , requestDescribeReservedInstancesModifications $
--             newDescribeReservedInstancesModifications
--
--         , requestDescribeReservedInstancesOfferings $
--             newDescribeReservedInstancesOfferings
--
--         , requestDescribeRouteTables $
--             newDescribeRouteTables
--
--         , requestDescribeScheduledInstanceAvailability $
--             newDescribeScheduledInstanceAvailability
--
--         , requestDescribeScheduledInstances $
--             newDescribeScheduledInstances
--
--         , requestDescribeSecurityGroupReferences $
--             newDescribeSecurityGroupReferences
--
--         , requestDescribeSecurityGroupRules $
--             newDescribeSecurityGroupRules
--
--         , requestDescribeSecurityGroups $
--             newDescribeSecurityGroups
--
--         , requestDescribeSnapshotAttribute $
--             newDescribeSnapshotAttribute
--
--         , requestDescribeSnapshotTierStatus $
--             newDescribeSnapshotTierStatus
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeSpotDatafeedSubscription $
--             newDescribeSpotDatafeedSubscription
--
--         , requestDescribeSpotFleetInstances $
--             newDescribeSpotFleetInstances
--
--         , requestDescribeSpotFleetRequestHistory $
--             newDescribeSpotFleetRequestHistory
--
--         , requestDescribeSpotFleetRequests $
--             newDescribeSpotFleetRequests
--
--         , requestDescribeSpotInstanceRequests $
--             newDescribeSpotInstanceRequests
--
--         , requestDescribeSpotPriceHistory $
--             newDescribeSpotPriceHistory
--
--         , requestDescribeStaleSecurityGroups $
--             newDescribeStaleSecurityGroups
--
--         , requestDescribeStoreImageTasks $
--             newDescribeStoreImageTasks
--
--         , requestDescribeSubnets $
--             newDescribeSubnets
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeTrafficMirrorFilters $
--             newDescribeTrafficMirrorFilters
--
--         , requestDescribeTrafficMirrorSessions $
--             newDescribeTrafficMirrorSessions
--
--         , requestDescribeTrafficMirrorTargets $
--             newDescribeTrafficMirrorTargets
--
--         , requestDescribeTransitGatewayAttachments $
--             newDescribeTransitGatewayAttachments
--
--         , requestDescribeTransitGatewayConnectPeers $
--             newDescribeTransitGatewayConnectPeers
--
--         , requestDescribeTransitGatewayConnects $
--             newDescribeTransitGatewayConnects
--
--         , requestDescribeTransitGatewayMulticastDomains $
--             newDescribeTransitGatewayMulticastDomains
--
--         , requestDescribeTransitGatewayPeeringAttachments $
--             newDescribeTransitGatewayPeeringAttachments
--
--         , requestDescribeTransitGatewayPolicyTables $
--             newDescribeTransitGatewayPolicyTables
--
--         , requestDescribeTransitGatewayRouteTableAnnouncements $
--             newDescribeTransitGatewayRouteTableAnnouncements
--
--         , requestDescribeTransitGatewayRouteTables $
--             newDescribeTransitGatewayRouteTables
--
--         , requestDescribeTransitGatewayVpcAttachments $
--             newDescribeTransitGatewayVpcAttachments
--
--         , requestDescribeTransitGateways $
--             newDescribeTransitGateways
--
--         , requestDescribeTrunkInterfaceAssociations $
--             newDescribeTrunkInterfaceAssociations
--
--         , requestDescribeVerifiedAccessEndpoints $
--             newDescribeVerifiedAccessEndpoints
--
--         , requestDescribeVerifiedAccessGroups $
--             newDescribeVerifiedAccessGroups
--
--         , requestDescribeVerifiedAccessInstanceLoggingConfigurations $
--             newDescribeVerifiedAccessInstanceLoggingConfigurations
--
--         , requestDescribeVerifiedAccessInstances $
--             newDescribeVerifiedAccessInstances
--
--         , requestDescribeVerifiedAccessTrustProviders $
--             newDescribeVerifiedAccessTrustProviders
--
--         , requestDescribeVolumeAttribute $
--             newDescribeVolumeAttribute
--
--         , requestDescribeVolumeStatus $
--             newDescribeVolumeStatus
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestDescribeVolumesModifications $
--             newDescribeVolumesModifications
--
--         , requestDescribeVpcAttribute $
--             newDescribeVpcAttribute
--
--         , requestDescribeVpcClassicLink $
--             newDescribeVpcClassicLink
--
--         , requestDescribeVpcClassicLinkDnsSupport $
--             newDescribeVpcClassicLinkDnsSupport
--
--         , requestDescribeVpcEndpointConnectionNotifications $
--             newDescribeVpcEndpointConnectionNotifications
--
--         , requestDescribeVpcEndpointConnections $
--             newDescribeVpcEndpointConnections
--
--         , requestDescribeVpcEndpointServiceConfigurations $
--             newDescribeVpcEndpointServiceConfigurations
--
--         , requestDescribeVpcEndpointServicePermissions $
--             newDescribeVpcEndpointServicePermissions
--
--         , requestDescribeVpcEndpointServices $
--             newDescribeVpcEndpointServices
--
--         , requestDescribeVpcEndpoints $
--             newDescribeVpcEndpoints
--
--         , requestDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnections
--
--         , requestDescribeVpcs $
--             newDescribeVpcs
--
--         , requestDescribeVpnConnections $
--             newDescribeVpnConnections
--
--         , requestDescribeVpnGateways $
--             newDescribeVpnGateways
--
--         , requestDetachClassicLinkVpc $
--             newDetachClassicLinkVpc
--
--         , requestDetachInternetGateway $
--             newDetachInternetGateway
--
--         , requestDetachNetworkInterface $
--             newDetachNetworkInterface
--
--         , requestDetachVerifiedAccessTrustProvider $
--             newDetachVerifiedAccessTrustProvider
--
--         , requestDetachVolume $
--             newDetachVolume
--
--         , requestDetachVpnGateway $
--             newDetachVpnGateway
--
--         , requestDisableAddressTransfer $
--             newDisableAddressTransfer
--
--         , requestDisableAwsNetworkPerformanceMetricSubscription $
--             newDisableAwsNetworkPerformanceMetricSubscription
--
--         , requestDisableEbsEncryptionByDefault $
--             newDisableEbsEncryptionByDefault
--
--         , requestDisableFastLaunch $
--             newDisableFastLaunch
--
--         , requestDisableFastSnapshotRestores $
--             newDisableFastSnapshotRestores
--
--         , requestDisableImageDeprecation $
--             newDisableImageDeprecation
--
--         , requestDisableIpamOrganizationAdminAccount $
--             newDisableIpamOrganizationAdminAccount
--
--         , requestDisableSerialConsoleAccess $
--             newDisableSerialConsoleAccess
--
--         , requestDisableTransitGatewayRouteTablePropagation $
--             newDisableTransitGatewayRouteTablePropagation
--
--         , requestDisableVgwRoutePropagation $
--             newDisableVgwRoutePropagation
--
--         , requestDisableVpcClassicLink $
--             newDisableVpcClassicLink
--
--         , requestDisableVpcClassicLinkDnsSupport $
--             newDisableVpcClassicLinkDnsSupport
--
--         , requestDisassociateAddress $
--             newDisassociateAddress
--
--         , requestDisassociateClientVpnTargetNetwork $
--             newDisassociateClientVpnTargetNetwork
--
--         , requestDisassociateEnclaveCertificateIamRole $
--             newDisassociateEnclaveCertificateIamRole
--
--         , requestDisassociateIamInstanceProfile $
--             newDisassociateIamInstanceProfile
--
--         , requestDisassociateInstanceEventWindow $
--             newDisassociateInstanceEventWindow
--
--         , requestDisassociateRouteTable $
--             newDisassociateRouteTable
--
--         , requestDisassociateSubnetCidrBlock $
--             newDisassociateSubnetCidrBlock
--
--         , requestDisassociateTransitGatewayMulticastDomain $
--             newDisassociateTransitGatewayMulticastDomain
--
--         , requestDisassociateTransitGatewayPolicyTable $
--             newDisassociateTransitGatewayPolicyTable
--
--         , requestDisassociateTransitGatewayRouteTable $
--             newDisassociateTransitGatewayRouteTable
--
--         , requestDisassociateTrunkInterface $
--             newDisassociateTrunkInterface
--
--         , requestDisassociateVpcCidrBlock $
--             newDisassociateVpcCidrBlock
--
--         , requestEnableAddressTransfer $
--             newEnableAddressTransfer
--
--         , requestEnableAwsNetworkPerformanceMetricSubscription $
--             newEnableAwsNetworkPerformanceMetricSubscription
--
--         , requestEnableEbsEncryptionByDefault $
--             newEnableEbsEncryptionByDefault
--
--         , requestEnableFastLaunch $
--             newEnableFastLaunch
--
--         , requestEnableFastSnapshotRestores $
--             newEnableFastSnapshotRestores
--
--         , requestEnableImageDeprecation $
--             newEnableImageDeprecation
--
--         , requestEnableIpamOrganizationAdminAccount $
--             newEnableIpamOrganizationAdminAccount
--
--         , requestEnableReachabilityAnalyzerOrganizationSharing $
--             newEnableReachabilityAnalyzerOrganizationSharing
--
--         , requestEnableSerialConsoleAccess $
--             newEnableSerialConsoleAccess
--
--         , requestEnableTransitGatewayRouteTablePropagation $
--             newEnableTransitGatewayRouteTablePropagation
--
--         , requestEnableVgwRoutePropagation $
--             newEnableVgwRoutePropagation
--
--         , requestEnableVolumeIO $
--             newEnableVolumeIO
--
--         , requestEnableVpcClassicLink $
--             newEnableVpcClassicLink
--
--         , requestEnableVpcClassicLinkDnsSupport $
--             newEnableVpcClassicLinkDnsSupport
--
--         , requestExportClientVpnClientCertificateRevocationList $
--             newExportClientVpnClientCertificateRevocationList
--
--         , requestExportClientVpnClientConfiguration $
--             newExportClientVpnClientConfiguration
--
--         , requestExportImage $
--             newExportImage
--
--         , requestExportTransitGatewayRoutes $
--             newExportTransitGatewayRoutes
--
--         , requestGetAssociatedEnclaveCertificateIamRoles $
--             newGetAssociatedEnclaveCertificateIamRoles
--
--         , requestGetAssociatedIpv6PoolCidrs $
--             newGetAssociatedIpv6PoolCidrs
--
--         , requestGetAwsNetworkPerformanceData $
--             newGetAwsNetworkPerformanceData
--
--         , requestGetCapacityReservationUsage $
--             newGetCapacityReservationUsage
--
--         , requestGetCoipPoolUsage $
--             newGetCoipPoolUsage
--
--         , requestGetConsoleOutput $
--             newGetConsoleOutput
--
--         , requestGetConsoleScreenshot $
--             newGetConsoleScreenshot
--
--         , requestGetDefaultCreditSpecification $
--             newGetDefaultCreditSpecification
--
--         , requestGetEbsDefaultKmsKeyId $
--             newGetEbsDefaultKmsKeyId
--
--         , requestGetEbsEncryptionByDefault $
--             newGetEbsEncryptionByDefault
--
--         , requestGetFlowLogsIntegrationTemplate $
--             newGetFlowLogsIntegrationTemplate
--
--         , requestGetGroupsForCapacityReservation $
--             newGetGroupsForCapacityReservation
--
--         , requestGetHostReservationPurchasePreview $
--             newGetHostReservationPurchasePreview
--
--         , requestGetInstanceTypesFromInstanceRequirements $
--             newGetInstanceTypesFromInstanceRequirements
--
--         , requestGetInstanceUefiData $
--             newGetInstanceUefiData
--
--         , requestGetIpamAddressHistory $
--             newGetIpamAddressHistory
--
--         , requestGetIpamPoolAllocations $
--             newGetIpamPoolAllocations
--
--         , requestGetIpamPoolCidrs $
--             newGetIpamPoolCidrs
--
--         , requestGetIpamResourceCidrs $
--             newGetIpamResourceCidrs
--
--         , requestGetLaunchTemplateData $
--             newGetLaunchTemplateData
--
--         , requestGetManagedPrefixListAssociations $
--             newGetManagedPrefixListAssociations
--
--         , requestGetManagedPrefixListEntries $
--             newGetManagedPrefixListEntries
--
--         , requestGetNetworkInsightsAccessScopeAnalysisFindings $
--             newGetNetworkInsightsAccessScopeAnalysisFindings
--
--         , requestGetNetworkInsightsAccessScopeContent $
--             newGetNetworkInsightsAccessScopeContent
--
--         , requestGetPasswordData $
--             newGetPasswordData
--
--         , requestGetReservedInstancesExchangeQuote $
--             newGetReservedInstancesExchangeQuote
--
--         , requestGetSerialConsoleAccessStatus $
--             newGetSerialConsoleAccessStatus
--
--         , requestGetSpotPlacementScores $
--             newGetSpotPlacementScores
--
--         , requestGetSubnetCidrReservations $
--             newGetSubnetCidrReservations
--
--         , requestGetTransitGatewayAttachmentPropagations $
--             newGetTransitGatewayAttachmentPropagations
--
--         , requestGetTransitGatewayMulticastDomainAssociations $
--             newGetTransitGatewayMulticastDomainAssociations
--
--         , requestGetTransitGatewayPolicyTableAssociations $
--             newGetTransitGatewayPolicyTableAssociations
--
--         , requestGetTransitGatewayPolicyTableEntries $
--             newGetTransitGatewayPolicyTableEntries
--
--         , requestGetTransitGatewayPrefixListReferences $
--             newGetTransitGatewayPrefixListReferences
--
--         , requestGetTransitGatewayRouteTableAssociations $
--             newGetTransitGatewayRouteTableAssociations
--
--         , requestGetTransitGatewayRouteTablePropagations $
--             newGetTransitGatewayRouteTablePropagations
--
--         , requestGetVerifiedAccessEndpointPolicy $
--             newGetVerifiedAccessEndpointPolicy
--
--         , requestGetVerifiedAccessGroupPolicy $
--             newGetVerifiedAccessGroupPolicy
--
--         , requestGetVpnConnectionDeviceSampleConfiguration $
--             newGetVpnConnectionDeviceSampleConfiguration
--
--         , requestGetVpnConnectionDeviceTypes $
--             newGetVpnConnectionDeviceTypes
--
--         , requestImportClientVpnClientCertificateRevocationList $
--             newImportClientVpnClientCertificateRevocationList
--
--         , requestImportImage $
--             newImportImage
--
--         , requestImportInstance $
--             newImportInstance
--
--         , requestImportKeyPair $
--             newImportKeyPair
--
--         , requestImportSnapshot $
--             newImportSnapshot
--
--         , requestImportVolume $
--             newImportVolume
--
--         , requestListImagesInRecycleBin $
--             newListImagesInRecycleBin
--
--         , requestListSnapshotsInRecycleBin $
--             newListSnapshotsInRecycleBin
--
--         , requestModifyAddressAttribute $
--             newModifyAddressAttribute
--
--         , requestModifyAvailabilityZoneGroup $
--             newModifyAvailabilityZoneGroup
--
--         , requestModifyCapacityReservation $
--             newModifyCapacityReservation
--
--         , requestModifyCapacityReservationFleet $
--             newModifyCapacityReservationFleet
--
--         , requestModifyClientVpnEndpoint $
--             newModifyClientVpnEndpoint
--
--         , requestModifyDefaultCreditSpecification $
--             newModifyDefaultCreditSpecification
--
--         , requestModifyEbsDefaultKmsKeyId $
--             newModifyEbsDefaultKmsKeyId
--
--         , requestModifyFleet $
--             newModifyFleet
--
--         , requestModifyFpgaImageAttribute $
--             newModifyFpgaImageAttribute
--
--         , requestModifyHosts $
--             newModifyHosts
--
--         , requestModifyIdFormat $
--             newModifyIdFormat
--
--         , requestModifyIdentityIdFormat $
--             newModifyIdentityIdFormat
--
--         , requestModifyImageAttribute $
--             newModifyImageAttribute
--
--         , requestModifyInstanceAttribute $
--             newModifyInstanceAttribute
--
--         , requestModifyInstanceCapacityReservationAttributes $
--             newModifyInstanceCapacityReservationAttributes
--
--         , requestModifyInstanceCreditSpecification $
--             newModifyInstanceCreditSpecification
--
--         , requestModifyInstanceEventStartTime $
--             newModifyInstanceEventStartTime
--
--         , requestModifyInstanceEventWindow $
--             newModifyInstanceEventWindow
--
--         , requestModifyInstanceMaintenanceOptions $
--             newModifyInstanceMaintenanceOptions
--
--         , requestModifyInstanceMetadataOptions $
--             newModifyInstanceMetadataOptions
--
--         , requestModifyInstancePlacement $
--             newModifyInstancePlacement
--
--         , requestModifyIpam $
--             newModifyIpam
--
--         , requestModifyIpamPool $
--             newModifyIpamPool
--
--         , requestModifyIpamResourceCidr $
--             newModifyIpamResourceCidr
--
--         , requestModifyIpamScope $
--             newModifyIpamScope
--
--         , requestModifyLaunchTemplate $
--             newModifyLaunchTemplate
--
--         , requestModifyLocalGatewayRoute $
--             newModifyLocalGatewayRoute
--
--         , requestModifyManagedPrefixList $
--             newModifyManagedPrefixList
--
--         , requestModifyNetworkInterfaceAttribute $
--             newModifyNetworkInterfaceAttribute
--
--         , requestModifyPrivateDnsNameOptions $
--             newModifyPrivateDnsNameOptions
--
--         , requestModifyReservedInstances $
--             newModifyReservedInstances
--
--         , requestModifySecurityGroupRules $
--             newModifySecurityGroupRules
--
--         , requestModifySnapshotAttribute $
--             newModifySnapshotAttribute
--
--         , requestModifySnapshotTier $
--             newModifySnapshotTier
--
--         , requestModifySpotFleetRequest $
--             newModifySpotFleetRequest
--
--         , requestModifySubnetAttribute $
--             newModifySubnetAttribute
--
--         , requestModifyTrafficMirrorFilterNetworkServices $
--             newModifyTrafficMirrorFilterNetworkServices
--
--         , requestModifyTrafficMirrorFilterRule $
--             newModifyTrafficMirrorFilterRule
--
--         , requestModifyTrafficMirrorSession $
--             newModifyTrafficMirrorSession
--
--         , requestModifyTransitGateway $
--             newModifyTransitGateway
--
--         , requestModifyTransitGatewayPrefixListReference $
--             newModifyTransitGatewayPrefixListReference
--
--         , requestModifyTransitGatewayVpcAttachment $
--             newModifyTransitGatewayVpcAttachment
--
--         , requestModifyVerifiedAccessEndpoint $
--             newModifyVerifiedAccessEndpoint
--
--         , requestModifyVerifiedAccessEndpointPolicy $
--             newModifyVerifiedAccessEndpointPolicy
--
--         , requestModifyVerifiedAccessGroup $
--             newModifyVerifiedAccessGroup
--
--         , requestModifyVerifiedAccessGroupPolicy $
--             newModifyVerifiedAccessGroupPolicy
--
--         , requestModifyVerifiedAccessInstance $
--             newModifyVerifiedAccessInstance
--
--         , requestModifyVerifiedAccessInstanceLoggingConfiguration $
--             newModifyVerifiedAccessInstanceLoggingConfiguration
--
--         , requestModifyVerifiedAccessTrustProvider $
--             newModifyVerifiedAccessTrustProvider
--
--         , requestModifyVolume $
--             newModifyVolume
--
--         , requestModifyVolumeAttribute $
--             newModifyVolumeAttribute
--
--         , requestModifyVpcAttribute $
--             newModifyVpcAttribute
--
--         , requestModifyVpcEndpoint $
--             newModifyVpcEndpoint
--
--         , requestModifyVpcEndpointConnectionNotification $
--             newModifyVpcEndpointConnectionNotification
--
--         , requestModifyVpcEndpointServiceConfiguration $
--             newModifyVpcEndpointServiceConfiguration
--
--         , requestModifyVpcEndpointServicePayerResponsibility $
--             newModifyVpcEndpointServicePayerResponsibility
--
--         , requestModifyVpcEndpointServicePermissions $
--             newModifyVpcEndpointServicePermissions
--
--         , requestModifyVpcPeeringConnectionOptions $
--             newModifyVpcPeeringConnectionOptions
--
--         , requestModifyVpcTenancy $
--             newModifyVpcTenancy
--
--         , requestModifyVpnConnection $
--             newModifyVpnConnection
--
--         , requestModifyVpnConnectionOptions $
--             newModifyVpnConnectionOptions
--
--         , requestModifyVpnTunnelCertificate $
--             newModifyVpnTunnelCertificate
--
--         , requestModifyVpnTunnelOptions $
--             newModifyVpnTunnelOptions
--
--         , requestMonitorInstances $
--             newMonitorInstances
--
--         , requestMoveAddressToVpc $
--             newMoveAddressToVpc
--
--         , requestMoveByoipCidrToIpam $
--             newMoveByoipCidrToIpam
--
--         , requestProvisionByoipCidr $
--             newProvisionByoipCidr
--
--         , requestProvisionIpamPoolCidr $
--             newProvisionIpamPoolCidr
--
--         , requestProvisionPublicIpv4PoolCidr $
--             newProvisionPublicIpv4PoolCidr
--
--         , requestPurchaseHostReservation $
--             newPurchaseHostReservation
--
--         , requestPurchaseReservedInstancesOffering $
--             newPurchaseReservedInstancesOffering
--
--         , requestPurchaseScheduledInstances $
--             newPurchaseScheduledInstances
--
--         , requestRebootInstances $
--             newRebootInstances
--
--         , requestRegisterImage $
--             newRegisterImage
--
--         , requestRegisterInstanceEventNotificationAttributes $
--             newRegisterInstanceEventNotificationAttributes
--
--         , requestRegisterTransitGatewayMulticastGroupMembers $
--             newRegisterTransitGatewayMulticastGroupMembers
--
--         , requestRegisterTransitGatewayMulticastGroupSources $
--             newRegisterTransitGatewayMulticastGroupSources
--
--         , requestRejectTransitGatewayMulticastDomainAssociations $
--             newRejectTransitGatewayMulticastDomainAssociations
--
--         , requestRejectTransitGatewayPeeringAttachment $
--             newRejectTransitGatewayPeeringAttachment
--
--         , requestRejectTransitGatewayVpcAttachment $
--             newRejectTransitGatewayVpcAttachment
--
--         , requestRejectVpcEndpointConnections $
--             newRejectVpcEndpointConnections
--
--         , requestRejectVpcPeeringConnection $
--             newRejectVpcPeeringConnection
--
--         , requestReleaseAddress $
--             newReleaseAddress
--
--         , requestReleaseHosts $
--             newReleaseHosts
--
--         , requestReleaseIpamPoolAllocation $
--             newReleaseIpamPoolAllocation
--
--         , requestReplaceIamInstanceProfileAssociation $
--             newReplaceIamInstanceProfileAssociation
--
--         , requestReplaceNetworkAclAssociation $
--             newReplaceNetworkAclAssociation
--
--         , requestReplaceNetworkAclEntry $
--             newReplaceNetworkAclEntry
--
--         , requestReplaceRoute $
--             newReplaceRoute
--
--         , requestReplaceRouteTableAssociation $
--             newReplaceRouteTableAssociation
--
--         , requestReplaceTransitGatewayRoute $
--             newReplaceTransitGatewayRoute
--
--         , requestReportInstanceStatus $
--             newReportInstanceStatus
--
--         , requestRequestSpotFleet $
--             newRequestSpotFleet
--
--         , requestRequestSpotInstances $
--             newRequestSpotInstances
--
--         , requestResetAddressAttribute $
--             newResetAddressAttribute
--
--         , requestResetEbsDefaultKmsKeyId $
--             newResetEbsDefaultKmsKeyId
--
--         , requestResetFpgaImageAttribute $
--             newResetFpgaImageAttribute
--
--         , requestResetImageAttribute $
--             newResetImageAttribute
--
--         , requestResetInstanceAttribute $
--             newResetInstanceAttribute
--
--         , requestResetNetworkInterfaceAttribute $
--             newResetNetworkInterfaceAttribute
--
--         , requestResetSnapshotAttribute $
--             newResetSnapshotAttribute
--
--         , requestRestoreAddressToClassic $
--             newRestoreAddressToClassic
--
--         , requestRestoreImageFromRecycleBin $
--             newRestoreImageFromRecycleBin
--
--         , requestRestoreManagedPrefixListVersion $
--             newRestoreManagedPrefixListVersion
--
--         , requestRestoreSnapshotFromRecycleBin $
--             newRestoreSnapshotFromRecycleBin
--
--         , requestRestoreSnapshotTier $
--             newRestoreSnapshotTier
--
--         , requestRevokeClientVpnIngress $
--             newRevokeClientVpnIngress
--
--         , requestRevokeSecurityGroupEgress $
--             newRevokeSecurityGroupEgress
--
--         , requestRevokeSecurityGroupIngress $
--             newRevokeSecurityGroupIngress
--
--         , requestRunInstances $
--             newRunInstances
--
--         , requestRunScheduledInstances $
--             newRunScheduledInstances
--
--         , requestSearchLocalGatewayRoutes $
--             newSearchLocalGatewayRoutes
--
--         , requestSearchTransitGatewayMulticastGroups $
--             newSearchTransitGatewayMulticastGroups
--
--         , requestSearchTransitGatewayRoutes $
--             newSearchTransitGatewayRoutes
--
--         , requestSendDiagnosticInterrupt $
--             newSendDiagnosticInterrupt
--
--         , requestStartInstances $
--             newStartInstances
--
--         , requestStartNetworkInsightsAccessScopeAnalysis $
--             newStartNetworkInsightsAccessScopeAnalysis
--
--         , requestStartNetworkInsightsAnalysis $
--             newStartNetworkInsightsAnalysis
--
--         , requestStartVpcEndpointServicePrivateDnsVerification $
--             newStartVpcEndpointServicePrivateDnsVerification
--
--         , requestStopInstances $
--             newStopInstances
--
--         , requestTerminateClientVpnConnections $
--             newTerminateClientVpnConnections
--
--         , requestTerminateInstances $
--             newTerminateInstances
--
--         , requestUnassignIpv6Addresses $
--             newUnassignIpv6Addresses
--
--         , requestUnassignPrivateIpAddresses $
--             newUnassignPrivateIpAddresses
--
--         , requestUnmonitorInstances $
--             newUnmonitorInstances
--
--         , requestUpdateSecurityGroupRuleDescriptionsEgress $
--             newUpdateSecurityGroupRuleDescriptionsEgress
--
--         , requestUpdateSecurityGroupRuleDescriptionsIngress $
--             newUpdateSecurityGroupRuleDescriptionsIngress
--
--         , requestWithdrawByoipCidr $
--             newWithdrawByoipCidr
--
--           ]

--     , testGroup "response"
--         [ responseAcceptAddressTransfer $
--             newAcceptAddressTransferResponse
--
--         , responseAcceptReservedInstancesExchangeQuote $
--             newAcceptReservedInstancesExchangeQuoteResponse
--
--         , responseAcceptTransitGatewayMulticastDomainAssociations $
--             newAcceptTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseAcceptTransitGatewayPeeringAttachment $
--             newAcceptTransitGatewayPeeringAttachmentResponse
--
--         , responseAcceptTransitGatewayVpcAttachment $
--             newAcceptTransitGatewayVpcAttachmentResponse
--
--         , responseAcceptVpcEndpointConnections $
--             newAcceptVpcEndpointConnectionsResponse
--
--         , responseAcceptVpcPeeringConnection $
--             newAcceptVpcPeeringConnectionResponse
--
--         , responseAdvertiseByoipCidr $
--             newAdvertiseByoipCidrResponse
--
--         , responseAllocateAddress $
--             newAllocateAddressResponse
--
--         , responseAllocateHosts $
--             newAllocateHostsResponse
--
--         , responseAllocateIpamPoolCidr $
--             newAllocateIpamPoolCidrResponse
--
--         , responseApplySecurityGroupsToClientVpnTargetNetwork $
--             newApplySecurityGroupsToClientVpnTargetNetworkResponse
--
--         , responseAssignIpv6Addresses $
--             newAssignIpv6AddressesResponse
--
--         , responseAssignPrivateIpAddresses $
--             newAssignPrivateIpAddressesResponse
--
--         , responseAssociateAddress $
--             newAssociateAddressResponse
--
--         , responseAssociateClientVpnTargetNetwork $
--             newAssociateClientVpnTargetNetworkResponse
--
--         , responseAssociateDhcpOptions $
--             newAssociateDhcpOptionsResponse
--
--         , responseAssociateEnclaveCertificateIamRole $
--             newAssociateEnclaveCertificateIamRoleResponse
--
--         , responseAssociateIamInstanceProfile $
--             newAssociateIamInstanceProfileResponse
--
--         , responseAssociateInstanceEventWindow $
--             newAssociateInstanceEventWindowResponse
--
--         , responseAssociateRouteTable $
--             newAssociateRouteTableResponse
--
--         , responseAssociateSubnetCidrBlock $
--             newAssociateSubnetCidrBlockResponse
--
--         , responseAssociateTransitGatewayMulticastDomain $
--             newAssociateTransitGatewayMulticastDomainResponse
--
--         , responseAssociateTransitGatewayPolicyTable $
--             newAssociateTransitGatewayPolicyTableResponse
--
--         , responseAssociateTransitGatewayRouteTable $
--             newAssociateTransitGatewayRouteTableResponse
--
--         , responseAssociateTrunkInterface $
--             newAssociateTrunkInterfaceResponse
--
--         , responseAssociateVpcCidrBlock $
--             newAssociateVpcCidrBlockResponse
--
--         , responseAttachClassicLinkVpc $
--             newAttachClassicLinkVpcResponse
--
--         , responseAttachInternetGateway $
--             newAttachInternetGatewayResponse
--
--         , responseAttachNetworkInterface $
--             newAttachNetworkInterfaceResponse
--
--         , responseAttachVerifiedAccessTrustProvider $
--             newAttachVerifiedAccessTrustProviderResponse
--
--         , responseAttachVolume $
--             newVolumeAttachment
--
--         , responseAttachVpnGateway $
--             newAttachVpnGatewayResponse
--
--         , responseAuthorizeClientVpnIngress $
--             newAuthorizeClientVpnIngressResponse
--
--         , responseAuthorizeSecurityGroupEgress $
--             newAuthorizeSecurityGroupEgressResponse
--
--         , responseAuthorizeSecurityGroupIngress $
--             newAuthorizeSecurityGroupIngressResponse
--
--         , responseBundleInstance $
--             newBundleInstanceResponse
--
--         , responseCancelBundleTask $
--             newCancelBundleTaskResponse
--
--         , responseCancelCapacityReservation $
--             newCancelCapacityReservationResponse
--
--         , responseCancelCapacityReservationFleets $
--             newCancelCapacityReservationFleetsResponse
--
--         , responseCancelConversionTask $
--             newCancelConversionTaskResponse
--
--         , responseCancelExportTask $
--             newCancelExportTaskResponse
--
--         , responseCancelImageLaunchPermission $
--             newCancelImageLaunchPermissionResponse
--
--         , responseCancelImportTask $
--             newCancelImportTaskResponse
--
--         , responseCancelReservedInstancesListing $
--             newCancelReservedInstancesListingResponse
--
--         , responseCancelSpotFleetRequests $
--             newCancelSpotFleetRequestsResponse
--
--         , responseCancelSpotInstanceRequests $
--             newCancelSpotInstanceRequestsResponse
--
--         , responseConfirmProductInstance $
--             newConfirmProductInstanceResponse
--
--         , responseCopyFpgaImage $
--             newCopyFpgaImageResponse
--
--         , responseCopyImage $
--             newCopyImageResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseCreateCapacityReservation $
--             newCreateCapacityReservationResponse
--
--         , responseCreateCapacityReservationFleet $
--             newCreateCapacityReservationFleetResponse
--
--         , responseCreateCarrierGateway $
--             newCreateCarrierGatewayResponse
--
--         , responseCreateClientVpnEndpoint $
--             newCreateClientVpnEndpointResponse
--
--         , responseCreateClientVpnRoute $
--             newCreateClientVpnRouteResponse
--
--         , responseCreateCoipCidr $
--             newCreateCoipCidrResponse
--
--         , responseCreateCoipPool $
--             newCreateCoipPoolResponse
--
--         , responseCreateCustomerGateway $
--             newCreateCustomerGatewayResponse
--
--         , responseCreateDefaultSubnet $
--             newCreateDefaultSubnetResponse
--
--         , responseCreateDefaultVpc $
--             newCreateDefaultVpcResponse
--
--         , responseCreateDhcpOptions $
--             newCreateDhcpOptionsResponse
--
--         , responseCreateEgressOnlyInternetGateway $
--             newCreateEgressOnlyInternetGatewayResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseCreateFlowLogs $
--             newCreateFlowLogsResponse
--
--         , responseCreateFpgaImage $
--             newCreateFpgaImageResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseCreateInstanceEventWindow $
--             newCreateInstanceEventWindowResponse
--
--         , responseCreateInstanceExportTask $
--             newCreateInstanceExportTaskResponse
--
--         , responseCreateInternetGateway $
--             newCreateInternetGatewayResponse
--
--         , responseCreateIpam $
--             newCreateIpamResponse
--
--         , responseCreateIpamPool $
--             newCreateIpamPoolResponse
--
--         , responseCreateIpamScope $
--             newCreateIpamScopeResponse
--
--         , responseCreateKeyPair $
--             newCreateKeyPairResponse
--
--         , responseCreateLaunchTemplate $
--             newCreateLaunchTemplateResponse
--
--         , responseCreateLaunchTemplateVersion $
--             newCreateLaunchTemplateVersionResponse
--
--         , responseCreateLocalGatewayRoute $
--             newCreateLocalGatewayRouteResponse
--
--         , responseCreateLocalGatewayRouteTable $
--             newCreateLocalGatewayRouteTableResponse
--
--         , responseCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation $
--             newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
--
--         , responseCreateLocalGatewayRouteTableVpcAssociation $
--             newCreateLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseCreateManagedPrefixList $
--             newCreateManagedPrefixListResponse
--
--         , responseCreateNatGateway $
--             newCreateNatGatewayResponse
--
--         , responseCreateNetworkAcl $
--             newCreateNetworkAclResponse
--
--         , responseCreateNetworkAclEntry $
--             newCreateNetworkAclEntryResponse
--
--         , responseCreateNetworkInsightsAccessScope $
--             newCreateNetworkInsightsAccessScopeResponse
--
--         , responseCreateNetworkInsightsPath $
--             newCreateNetworkInsightsPathResponse
--
--         , responseCreateNetworkInterface $
--             newCreateNetworkInterfaceResponse
--
--         , responseCreateNetworkInterfacePermission $
--             newCreateNetworkInterfacePermissionResponse
--
--         , responseCreatePlacementGroup $
--             newCreatePlacementGroupResponse
--
--         , responseCreatePublicIpv4Pool $
--             newCreatePublicIpv4PoolResponse
--
--         , responseCreateReplaceRootVolumeTask $
--             newCreateReplaceRootVolumeTaskResponse
--
--         , responseCreateReservedInstancesListing $
--             newCreateReservedInstancesListingResponse
--
--         , responseCreateRestoreImageTask $
--             newCreateRestoreImageTaskResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse
--
--         , responseCreateRouteTable $
--             newCreateRouteTableResponse
--
--         , responseCreateSecurityGroup $
--             newCreateSecurityGroupResponse
--
--         , responseCreateSnapshot $
--             newSnapshot
--
--         , responseCreateSnapshots $
--             newCreateSnapshotsResponse
--
--         , responseCreateSpotDatafeedSubscription $
--             newCreateSpotDatafeedSubscriptionResponse
--
--         , responseCreateStoreImageTask $
--             newCreateStoreImageTaskResponse
--
--         , responseCreateSubnet $
--             newCreateSubnetResponse
--
--         , responseCreateSubnetCidrReservation $
--             newCreateSubnetCidrReservationResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseCreateTrafficMirrorFilter $
--             newCreateTrafficMirrorFilterResponse
--
--         , responseCreateTrafficMirrorFilterRule $
--             newCreateTrafficMirrorFilterRuleResponse
--
--         , responseCreateTrafficMirrorSession $
--             newCreateTrafficMirrorSessionResponse
--
--         , responseCreateTrafficMirrorTarget $
--             newCreateTrafficMirrorTargetResponse
--
--         , responseCreateTransitGateway $
--             newCreateTransitGatewayResponse
--
--         , responseCreateTransitGatewayConnect $
--             newCreateTransitGatewayConnectResponse
--
--         , responseCreateTransitGatewayConnectPeer $
--             newCreateTransitGatewayConnectPeerResponse
--
--         , responseCreateTransitGatewayMulticastDomain $
--             newCreateTransitGatewayMulticastDomainResponse
--
--         , responseCreateTransitGatewayPeeringAttachment $
--             newCreateTransitGatewayPeeringAttachmentResponse
--
--         , responseCreateTransitGatewayPolicyTable $
--             newCreateTransitGatewayPolicyTableResponse
--
--         , responseCreateTransitGatewayPrefixListReference $
--             newCreateTransitGatewayPrefixListReferenceResponse
--
--         , responseCreateTransitGatewayRoute $
--             newCreateTransitGatewayRouteResponse
--
--         , responseCreateTransitGatewayRouteTable $
--             newCreateTransitGatewayRouteTableResponse
--
--         , responseCreateTransitGatewayRouteTableAnnouncement $
--             newCreateTransitGatewayRouteTableAnnouncementResponse
--
--         , responseCreateTransitGatewayVpcAttachment $
--             newCreateTransitGatewayVpcAttachmentResponse
--
--         , responseCreateVerifiedAccessEndpoint $
--             newCreateVerifiedAccessEndpointResponse
--
--         , responseCreateVerifiedAccessGroup $
--             newCreateVerifiedAccessGroupResponse
--
--         , responseCreateVerifiedAccessInstance $
--             newCreateVerifiedAccessInstanceResponse
--
--         , responseCreateVerifiedAccessTrustProvider $
--             newCreateVerifiedAccessTrustProviderResponse
--
--         , responseCreateVolume $
--             newVolume
--
--         , responseCreateVpc $
--             newCreateVpcResponse
--
--         , responseCreateVpcEndpoint $
--             newCreateVpcEndpointResponse
--
--         , responseCreateVpcEndpointConnectionNotification $
--             newCreateVpcEndpointConnectionNotificationResponse
--
--         , responseCreateVpcEndpointServiceConfiguration $
--             newCreateVpcEndpointServiceConfigurationResponse
--
--         , responseCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnectionResponse
--
--         , responseCreateVpnConnection $
--             newCreateVpnConnectionResponse
--
--         , responseCreateVpnConnectionRoute $
--             newCreateVpnConnectionRouteResponse
--
--         , responseCreateVpnGateway $
--             newCreateVpnGatewayResponse
--
--         , responseDeleteCarrierGateway $
--             newDeleteCarrierGatewayResponse
--
--         , responseDeleteClientVpnEndpoint $
--             newDeleteClientVpnEndpointResponse
--
--         , responseDeleteClientVpnRoute $
--             newDeleteClientVpnRouteResponse
--
--         , responseDeleteCoipCidr $
--             newDeleteCoipCidrResponse
--
--         , responseDeleteCoipPool $
--             newDeleteCoipPoolResponse
--
--         , responseDeleteCustomerGateway $
--             newDeleteCustomerGatewayResponse
--
--         , responseDeleteDhcpOptions $
--             newDeleteDhcpOptionsResponse
--
--         , responseDeleteEgressOnlyInternetGateway $
--             newDeleteEgressOnlyInternetGatewayResponse
--
--         , responseDeleteFleets $
--             newDeleteFleetsResponse
--
--         , responseDeleteFlowLogs $
--             newDeleteFlowLogsResponse
--
--         , responseDeleteFpgaImage $
--             newDeleteFpgaImageResponse
--
--         , responseDeleteInstanceEventWindow $
--             newDeleteInstanceEventWindowResponse
--
--         , responseDeleteInternetGateway $
--             newDeleteInternetGatewayResponse
--
--         , responseDeleteIpam $
--             newDeleteIpamResponse
--
--         , responseDeleteIpamPool $
--             newDeleteIpamPoolResponse
--
--         , responseDeleteIpamScope $
--             newDeleteIpamScopeResponse
--
--         , responseDeleteKeyPair $
--             newDeleteKeyPairResponse
--
--         , responseDeleteLaunchTemplate $
--             newDeleteLaunchTemplateResponse
--
--         , responseDeleteLaunchTemplateVersions $
--             newDeleteLaunchTemplateVersionsResponse
--
--         , responseDeleteLocalGatewayRoute $
--             newDeleteLocalGatewayRouteResponse
--
--         , responseDeleteLocalGatewayRouteTable $
--             newDeleteLocalGatewayRouteTableResponse
--
--         , responseDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation $
--             newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
--
--         , responseDeleteLocalGatewayRouteTableVpcAssociation $
--             newDeleteLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseDeleteManagedPrefixList $
--             newDeleteManagedPrefixListResponse
--
--         , responseDeleteNatGateway $
--             newDeleteNatGatewayResponse
--
--         , responseDeleteNetworkAcl $
--             newDeleteNetworkAclResponse
--
--         , responseDeleteNetworkAclEntry $
--             newDeleteNetworkAclEntryResponse
--
--         , responseDeleteNetworkInsightsAccessScope $
--             newDeleteNetworkInsightsAccessScopeResponse
--
--         , responseDeleteNetworkInsightsAccessScopeAnalysis $
--             newDeleteNetworkInsightsAccessScopeAnalysisResponse
--
--         , responseDeleteNetworkInsightsAnalysis $
--             newDeleteNetworkInsightsAnalysisResponse
--
--         , responseDeleteNetworkInsightsPath $
--             newDeleteNetworkInsightsPathResponse
--
--         , responseDeleteNetworkInterface $
--             newDeleteNetworkInterfaceResponse
--
--         , responseDeleteNetworkInterfacePermission $
--             newDeleteNetworkInterfacePermissionResponse
--
--         , responseDeletePlacementGroup $
--             newDeletePlacementGroupResponse
--
--         , responseDeletePublicIpv4Pool $
--             newDeletePublicIpv4PoolResponse
--
--         , responseDeleteQueuedReservedInstances $
--             newDeleteQueuedReservedInstancesResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse
--
--         , responseDeleteRouteTable $
--             newDeleteRouteTableResponse
--
--         , responseDeleteSecurityGroup $
--             newDeleteSecurityGroupResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDeleteSpotDatafeedSubscription $
--             newDeleteSpotDatafeedSubscriptionResponse
--
--         , responseDeleteSubnet $
--             newDeleteSubnetResponse
--
--         , responseDeleteSubnetCidrReservation $
--             newDeleteSubnetCidrReservationResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDeleteTrafficMirrorFilter $
--             newDeleteTrafficMirrorFilterResponse
--
--         , responseDeleteTrafficMirrorFilterRule $
--             newDeleteTrafficMirrorFilterRuleResponse
--
--         , responseDeleteTrafficMirrorSession $
--             newDeleteTrafficMirrorSessionResponse
--
--         , responseDeleteTrafficMirrorTarget $
--             newDeleteTrafficMirrorTargetResponse
--
--         , responseDeleteTransitGateway $
--             newDeleteTransitGatewayResponse
--
--         , responseDeleteTransitGatewayConnect $
--             newDeleteTransitGatewayConnectResponse
--
--         , responseDeleteTransitGatewayConnectPeer $
--             newDeleteTransitGatewayConnectPeerResponse
--
--         , responseDeleteTransitGatewayMulticastDomain $
--             newDeleteTransitGatewayMulticastDomainResponse
--
--         , responseDeleteTransitGatewayPeeringAttachment $
--             newDeleteTransitGatewayPeeringAttachmentResponse
--
--         , responseDeleteTransitGatewayPolicyTable $
--             newDeleteTransitGatewayPolicyTableResponse
--
--         , responseDeleteTransitGatewayPrefixListReference $
--             newDeleteTransitGatewayPrefixListReferenceResponse
--
--         , responseDeleteTransitGatewayRoute $
--             newDeleteTransitGatewayRouteResponse
--
--         , responseDeleteTransitGatewayRouteTable $
--             newDeleteTransitGatewayRouteTableResponse
--
--         , responseDeleteTransitGatewayRouteTableAnnouncement $
--             newDeleteTransitGatewayRouteTableAnnouncementResponse
--
--         , responseDeleteTransitGatewayVpcAttachment $
--             newDeleteTransitGatewayVpcAttachmentResponse
--
--         , responseDeleteVerifiedAccessEndpoint $
--             newDeleteVerifiedAccessEndpointResponse
--
--         , responseDeleteVerifiedAccessGroup $
--             newDeleteVerifiedAccessGroupResponse
--
--         , responseDeleteVerifiedAccessInstance $
--             newDeleteVerifiedAccessInstanceResponse
--
--         , responseDeleteVerifiedAccessTrustProvider $
--             newDeleteVerifiedAccessTrustProviderResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--         , responseDeleteVpc $
--             newDeleteVpcResponse
--
--         , responseDeleteVpcEndpointConnectionNotifications $
--             newDeleteVpcEndpointConnectionNotificationsResponse
--
--         , responseDeleteVpcEndpointServiceConfigurations $
--             newDeleteVpcEndpointServiceConfigurationsResponse
--
--         , responseDeleteVpcEndpoints $
--             newDeleteVpcEndpointsResponse
--
--         , responseDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnectionResponse
--
--         , responseDeleteVpnConnection $
--             newDeleteVpnConnectionResponse
--
--         , responseDeleteVpnConnectionRoute $
--             newDeleteVpnConnectionRouteResponse
--
--         , responseDeleteVpnGateway $
--             newDeleteVpnGatewayResponse
--
--         , responseDeprovisionByoipCidr $
--             newDeprovisionByoipCidrResponse
--
--         , responseDeprovisionIpamPoolCidr $
--             newDeprovisionIpamPoolCidrResponse
--
--         , responseDeprovisionPublicIpv4PoolCidr $
--             newDeprovisionPublicIpv4PoolCidrResponse
--
--         , responseDeregisterImage $
--             newDeregisterImageResponse
--
--         , responseDeregisterInstanceEventNotificationAttributes $
--             newDeregisterInstanceEventNotificationAttributesResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupMembers $
--             newDeregisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupSources $
--             newDeregisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeAddressTransfers $
--             newDescribeAddressTransfersResponse
--
--         , responseDescribeAddresses $
--             newDescribeAddressesResponse
--
--         , responseDescribeAddressesAttribute $
--             newDescribeAddressesAttributeResponse
--
--         , responseDescribeAggregateIdFormat $
--             newDescribeAggregateIdFormatResponse
--
--         , responseDescribeAvailabilityZones $
--             newDescribeAvailabilityZonesResponse
--
--         , responseDescribeAwsNetworkPerformanceMetricSubscriptions $
--             newDescribeAwsNetworkPerformanceMetricSubscriptionsResponse
--
--         , responseDescribeBundleTasks $
--             newDescribeBundleTasksResponse
--
--         , responseDescribeByoipCidrs $
--             newDescribeByoipCidrsResponse
--
--         , responseDescribeCapacityReservationFleets $
--             newDescribeCapacityReservationFleetsResponse
--
--         , responseDescribeCapacityReservations $
--             newDescribeCapacityReservationsResponse
--
--         , responseDescribeCarrierGateways $
--             newDescribeCarrierGatewaysResponse
--
--         , responseDescribeClassicLinkInstances $
--             newDescribeClassicLinkInstancesResponse
--
--         , responseDescribeClientVpnAuthorizationRules $
--             newDescribeClientVpnAuthorizationRulesResponse
--
--         , responseDescribeClientVpnConnections $
--             newDescribeClientVpnConnectionsResponse
--
--         , responseDescribeClientVpnEndpoints $
--             newDescribeClientVpnEndpointsResponse
--
--         , responseDescribeClientVpnRoutes $
--             newDescribeClientVpnRoutesResponse
--
--         , responseDescribeClientVpnTargetNetworks $
--             newDescribeClientVpnTargetNetworksResponse
--
--         , responseDescribeCoipPools $
--             newDescribeCoipPoolsResponse
--
--         , responseDescribeConversionTasks $
--             newDescribeConversionTasksResponse
--
--         , responseDescribeCustomerGateways $
--             newDescribeCustomerGatewaysResponse
--
--         , responseDescribeDhcpOptions $
--             newDescribeDhcpOptionsResponse
--
--         , responseDescribeEgressOnlyInternetGateways $
--             newDescribeEgressOnlyInternetGatewaysResponse
--
--         , responseDescribeElasticGpus $
--             newDescribeElasticGpusResponse
--
--         , responseDescribeExportImageTasks $
--             newDescribeExportImageTasksResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseDescribeFastLaunchImages $
--             newDescribeFastLaunchImagesResponse
--
--         , responseDescribeFastSnapshotRestores $
--             newDescribeFastSnapshotRestoresResponse
--
--         , responseDescribeFleetHistory $
--             newDescribeFleetHistoryResponse
--
--         , responseDescribeFleetInstances $
--             newDescribeFleetInstancesResponse
--
--         , responseDescribeFleets $
--             newDescribeFleetsResponse
--
--         , responseDescribeFlowLogs $
--             newDescribeFlowLogsResponse
--
--         , responseDescribeFpgaImageAttribute $
--             newDescribeFpgaImageAttributeResponse
--
--         , responseDescribeFpgaImages $
--             newDescribeFpgaImagesResponse
--
--         , responseDescribeHostReservationOfferings $
--             newDescribeHostReservationOfferingsResponse
--
--         , responseDescribeHostReservations $
--             newDescribeHostReservationsResponse
--
--         , responseDescribeHosts $
--             newDescribeHostsResponse
--
--         , responseDescribeIamInstanceProfileAssociations $
--             newDescribeIamInstanceProfileAssociationsResponse
--
--         , responseDescribeIdFormat $
--             newDescribeIdFormatResponse
--
--         , responseDescribeIdentityIdFormat $
--             newDescribeIdentityIdFormatResponse
--
--         , responseDescribeImageAttribute $
--             newDescribeImageAttributeResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseDescribeImportImageTasks $
--             newDescribeImportImageTasksResponse
--
--         , responseDescribeImportSnapshotTasks $
--             newDescribeImportSnapshotTasksResponse
--
--         , responseDescribeInstanceAttribute $
--             newDescribeInstanceAttributeResponse
--
--         , responseDescribeInstanceCreditSpecifications $
--             newDescribeInstanceCreditSpecificationsResponse
--
--         , responseDescribeInstanceEventNotificationAttributes $
--             newDescribeInstanceEventNotificationAttributesResponse
--
--         , responseDescribeInstanceEventWindows $
--             newDescribeInstanceEventWindowsResponse
--
--         , responseDescribeInstanceStatus $
--             newDescribeInstanceStatusResponse
--
--         , responseDescribeInstanceTypeOfferings $
--             newDescribeInstanceTypeOfferingsResponse
--
--         , responseDescribeInstanceTypes $
--             newDescribeInstanceTypesResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDescribeInternetGateways $
--             newDescribeInternetGatewaysResponse
--
--         , responseDescribeIpamPools $
--             newDescribeIpamPoolsResponse
--
--         , responseDescribeIpamScopes $
--             newDescribeIpamScopesResponse
--
--         , responseDescribeIpams $
--             newDescribeIpamsResponse
--
--         , responseDescribeIpv6Pools $
--             newDescribeIpv6PoolsResponse
--
--         , responseDescribeKeyPairs $
--             newDescribeKeyPairsResponse
--
--         , responseDescribeLaunchTemplateVersions $
--             newDescribeLaunchTemplateVersionsResponse
--
--         , responseDescribeLaunchTemplates $
--             newDescribeLaunchTemplatesResponse
--
--         , responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
--
--         , responseDescribeLocalGatewayRouteTableVpcAssociations $
--             newDescribeLocalGatewayRouteTableVpcAssociationsResponse
--
--         , responseDescribeLocalGatewayRouteTables $
--             newDescribeLocalGatewayRouteTablesResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaceGroups $
--             newDescribeLocalGatewayVirtualInterfaceGroupsResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaces $
--             newDescribeLocalGatewayVirtualInterfacesResponse
--
--         , responseDescribeLocalGateways $
--             newDescribeLocalGatewaysResponse
--
--         , responseDescribeManagedPrefixLists $
--             newDescribeManagedPrefixListsResponse
--
--         , responseDescribeMovingAddresses $
--             newDescribeMovingAddressesResponse
--
--         , responseDescribeNatGateways $
--             newDescribeNatGatewaysResponse
--
--         , responseDescribeNetworkAcls $
--             newDescribeNetworkAclsResponse
--
--         , responseDescribeNetworkInsightsAccessScopeAnalyses $
--             newDescribeNetworkInsightsAccessScopeAnalysesResponse
--
--         , responseDescribeNetworkInsightsAccessScopes $
--             newDescribeNetworkInsightsAccessScopesResponse
--
--         , responseDescribeNetworkInsightsAnalyses $
--             newDescribeNetworkInsightsAnalysesResponse
--
--         , responseDescribeNetworkInsightsPaths $
--             newDescribeNetworkInsightsPathsResponse
--
--         , responseDescribeNetworkInterfaceAttribute $
--             newDescribeNetworkInterfaceAttributeResponse
--
--         , responseDescribeNetworkInterfacePermissions $
--             newDescribeNetworkInterfacePermissionsResponse
--
--         , responseDescribeNetworkInterfaces $
--             newDescribeNetworkInterfacesResponse
--
--         , responseDescribePlacementGroups $
--             newDescribePlacementGroupsResponse
--
--         , responseDescribePrefixLists $
--             newDescribePrefixListsResponse
--
--         , responseDescribePrincipalIdFormat $
--             newDescribePrincipalIdFormatResponse
--
--         , responseDescribePublicIpv4Pools $
--             newDescribePublicIpv4PoolsResponse
--
--         , responseDescribeRegions $
--             newDescribeRegionsResponse
--
--         , responseDescribeReplaceRootVolumeTasks $
--             newDescribeReplaceRootVolumeTasksResponse
--
--         , responseDescribeReservedInstances $
--             newDescribeReservedInstancesResponse
--
--         , responseDescribeReservedInstancesListings $
--             newDescribeReservedInstancesListingsResponse
--
--         , responseDescribeReservedInstancesModifications $
--             newDescribeReservedInstancesModificationsResponse
--
--         , responseDescribeReservedInstancesOfferings $
--             newDescribeReservedInstancesOfferingsResponse
--
--         , responseDescribeRouteTables $
--             newDescribeRouteTablesResponse
--
--         , responseDescribeScheduledInstanceAvailability $
--             newDescribeScheduledInstanceAvailabilityResponse
--
--         , responseDescribeScheduledInstances $
--             newDescribeScheduledInstancesResponse
--
--         , responseDescribeSecurityGroupReferences $
--             newDescribeSecurityGroupReferencesResponse
--
--         , responseDescribeSecurityGroupRules $
--             newDescribeSecurityGroupRulesResponse
--
--         , responseDescribeSecurityGroups $
--             newDescribeSecurityGroupsResponse
--
--         , responseDescribeSnapshotAttribute $
--             newDescribeSnapshotAttributeResponse
--
--         , responseDescribeSnapshotTierStatus $
--             newDescribeSnapshotTierStatusResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeSpotDatafeedSubscription $
--             newDescribeSpotDatafeedSubscriptionResponse
--
--         , responseDescribeSpotFleetInstances $
--             newDescribeSpotFleetInstancesResponse
--
--         , responseDescribeSpotFleetRequestHistory $
--             newDescribeSpotFleetRequestHistoryResponse
--
--         , responseDescribeSpotFleetRequests $
--             newDescribeSpotFleetRequestsResponse
--
--         , responseDescribeSpotInstanceRequests $
--             newDescribeSpotInstanceRequestsResponse
--
--         , responseDescribeSpotPriceHistory $
--             newDescribeSpotPriceHistoryResponse
--
--         , responseDescribeStaleSecurityGroups $
--             newDescribeStaleSecurityGroupsResponse
--
--         , responseDescribeStoreImageTasks $
--             newDescribeStoreImageTasksResponse
--
--         , responseDescribeSubnets $
--             newDescribeSubnetsResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeTrafficMirrorFilters $
--             newDescribeTrafficMirrorFiltersResponse
--
--         , responseDescribeTrafficMirrorSessions $
--             newDescribeTrafficMirrorSessionsResponse
--
--         , responseDescribeTrafficMirrorTargets $
--             newDescribeTrafficMirrorTargetsResponse
--
--         , responseDescribeTransitGatewayAttachments $
--             newDescribeTransitGatewayAttachmentsResponse
--
--         , responseDescribeTransitGatewayConnectPeers $
--             newDescribeTransitGatewayConnectPeersResponse
--
--         , responseDescribeTransitGatewayConnects $
--             newDescribeTransitGatewayConnectsResponse
--
--         , responseDescribeTransitGatewayMulticastDomains $
--             newDescribeTransitGatewayMulticastDomainsResponse
--
--         , responseDescribeTransitGatewayPeeringAttachments $
--             newDescribeTransitGatewayPeeringAttachmentsResponse
--
--         , responseDescribeTransitGatewayPolicyTables $
--             newDescribeTransitGatewayPolicyTablesResponse
--
--         , responseDescribeTransitGatewayRouteTableAnnouncements $
--             newDescribeTransitGatewayRouteTableAnnouncementsResponse
--
--         , responseDescribeTransitGatewayRouteTables $
--             newDescribeTransitGatewayRouteTablesResponse
--
--         , responseDescribeTransitGatewayVpcAttachments $
--             newDescribeTransitGatewayVpcAttachmentsResponse
--
--         , responseDescribeTransitGateways $
--             newDescribeTransitGatewaysResponse
--
--         , responseDescribeTrunkInterfaceAssociations $
--             newDescribeTrunkInterfaceAssociationsResponse
--
--         , responseDescribeVerifiedAccessEndpoints $
--             newDescribeVerifiedAccessEndpointsResponse
--
--         , responseDescribeVerifiedAccessGroups $
--             newDescribeVerifiedAccessGroupsResponse
--
--         , responseDescribeVerifiedAccessInstanceLoggingConfigurations $
--             newDescribeVerifiedAccessInstanceLoggingConfigurationsResponse
--
--         , responseDescribeVerifiedAccessInstances $
--             newDescribeVerifiedAccessInstancesResponse
--
--         , responseDescribeVerifiedAccessTrustProviders $
--             newDescribeVerifiedAccessTrustProvidersResponse
--
--         , responseDescribeVolumeAttribute $
--             newDescribeVolumeAttributeResponse
--
--         , responseDescribeVolumeStatus $
--             newDescribeVolumeStatusResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseDescribeVolumesModifications $
--             newDescribeVolumesModificationsResponse
--
--         , responseDescribeVpcAttribute $
--             newDescribeVpcAttributeResponse
--
--         , responseDescribeVpcClassicLink $
--             newDescribeVpcClassicLinkResponse
--
--         , responseDescribeVpcClassicLinkDnsSupport $
--             newDescribeVpcClassicLinkDnsSupportResponse
--
--         , responseDescribeVpcEndpointConnectionNotifications $
--             newDescribeVpcEndpointConnectionNotificationsResponse
--
--         , responseDescribeVpcEndpointConnections $
--             newDescribeVpcEndpointConnectionsResponse
--
--         , responseDescribeVpcEndpointServiceConfigurations $
--             newDescribeVpcEndpointServiceConfigurationsResponse
--
--         , responseDescribeVpcEndpointServicePermissions $
--             newDescribeVpcEndpointServicePermissionsResponse
--
--         , responseDescribeVpcEndpointServices $
--             newDescribeVpcEndpointServicesResponse
--
--         , responseDescribeVpcEndpoints $
--             newDescribeVpcEndpointsResponse
--
--         , responseDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnectionsResponse
--
--         , responseDescribeVpcs $
--             newDescribeVpcsResponse
--
--         , responseDescribeVpnConnections $
--             newDescribeVpnConnectionsResponse
--
--         , responseDescribeVpnGateways $
--             newDescribeVpnGatewaysResponse
--
--         , responseDetachClassicLinkVpc $
--             newDetachClassicLinkVpcResponse
--
--         , responseDetachInternetGateway $
--             newDetachInternetGatewayResponse
--
--         , responseDetachNetworkInterface $
--             newDetachNetworkInterfaceResponse
--
--         , responseDetachVerifiedAccessTrustProvider $
--             newDetachVerifiedAccessTrustProviderResponse
--
--         , responseDetachVolume $
--             newVolumeAttachment
--
--         , responseDetachVpnGateway $
--             newDetachVpnGatewayResponse
--
--         , responseDisableAddressTransfer $
--             newDisableAddressTransferResponse
--
--         , responseDisableAwsNetworkPerformanceMetricSubscription $
--             newDisableAwsNetworkPerformanceMetricSubscriptionResponse
--
--         , responseDisableEbsEncryptionByDefault $
--             newDisableEbsEncryptionByDefaultResponse
--
--         , responseDisableFastLaunch $
--             newDisableFastLaunchResponse
--
--         , responseDisableFastSnapshotRestores $
--             newDisableFastSnapshotRestoresResponse
--
--         , responseDisableImageDeprecation $
--             newDisableImageDeprecationResponse
--
--         , responseDisableIpamOrganizationAdminAccount $
--             newDisableIpamOrganizationAdminAccountResponse
--
--         , responseDisableSerialConsoleAccess $
--             newDisableSerialConsoleAccessResponse
--
--         , responseDisableTransitGatewayRouteTablePropagation $
--             newDisableTransitGatewayRouteTablePropagationResponse
--
--         , responseDisableVgwRoutePropagation $
--             newDisableVgwRoutePropagationResponse
--
--         , responseDisableVpcClassicLink $
--             newDisableVpcClassicLinkResponse
--
--         , responseDisableVpcClassicLinkDnsSupport $
--             newDisableVpcClassicLinkDnsSupportResponse
--
--         , responseDisassociateAddress $
--             newDisassociateAddressResponse
--
--         , responseDisassociateClientVpnTargetNetwork $
--             newDisassociateClientVpnTargetNetworkResponse
--
--         , responseDisassociateEnclaveCertificateIamRole $
--             newDisassociateEnclaveCertificateIamRoleResponse
--
--         , responseDisassociateIamInstanceProfile $
--             newDisassociateIamInstanceProfileResponse
--
--         , responseDisassociateInstanceEventWindow $
--             newDisassociateInstanceEventWindowResponse
--
--         , responseDisassociateRouteTable $
--             newDisassociateRouteTableResponse
--
--         , responseDisassociateSubnetCidrBlock $
--             newDisassociateSubnetCidrBlockResponse
--
--         , responseDisassociateTransitGatewayMulticastDomain $
--             newDisassociateTransitGatewayMulticastDomainResponse
--
--         , responseDisassociateTransitGatewayPolicyTable $
--             newDisassociateTransitGatewayPolicyTableResponse
--
--         , responseDisassociateTransitGatewayRouteTable $
--             newDisassociateTransitGatewayRouteTableResponse
--
--         , responseDisassociateTrunkInterface $
--             newDisassociateTrunkInterfaceResponse
--
--         , responseDisassociateVpcCidrBlock $
--             newDisassociateVpcCidrBlockResponse
--
--         , responseEnableAddressTransfer $
--             newEnableAddressTransferResponse
--
--         , responseEnableAwsNetworkPerformanceMetricSubscription $
--             newEnableAwsNetworkPerformanceMetricSubscriptionResponse
--
--         , responseEnableEbsEncryptionByDefault $
--             newEnableEbsEncryptionByDefaultResponse
--
--         , responseEnableFastLaunch $
--             newEnableFastLaunchResponse
--
--         , responseEnableFastSnapshotRestores $
--             newEnableFastSnapshotRestoresResponse
--
--         , responseEnableImageDeprecation $
--             newEnableImageDeprecationResponse
--
--         , responseEnableIpamOrganizationAdminAccount $
--             newEnableIpamOrganizationAdminAccountResponse
--
--         , responseEnableReachabilityAnalyzerOrganizationSharing $
--             newEnableReachabilityAnalyzerOrganizationSharingResponse
--
--         , responseEnableSerialConsoleAccess $
--             newEnableSerialConsoleAccessResponse
--
--         , responseEnableTransitGatewayRouteTablePropagation $
--             newEnableTransitGatewayRouteTablePropagationResponse
--
--         , responseEnableVgwRoutePropagation $
--             newEnableVgwRoutePropagationResponse
--
--         , responseEnableVolumeIO $
--             newEnableVolumeIOResponse
--
--         , responseEnableVpcClassicLink $
--             newEnableVpcClassicLinkResponse
--
--         , responseEnableVpcClassicLinkDnsSupport $
--             newEnableVpcClassicLinkDnsSupportResponse
--
--         , responseExportClientVpnClientCertificateRevocationList $
--             newExportClientVpnClientCertificateRevocationListResponse
--
--         , responseExportClientVpnClientConfiguration $
--             newExportClientVpnClientConfigurationResponse
--
--         , responseExportImage $
--             newExportImageResponse
--
--         , responseExportTransitGatewayRoutes $
--             newExportTransitGatewayRoutesResponse
--
--         , responseGetAssociatedEnclaveCertificateIamRoles $
--             newGetAssociatedEnclaveCertificateIamRolesResponse
--
--         , responseGetAssociatedIpv6PoolCidrs $
--             newGetAssociatedIpv6PoolCidrsResponse
--
--         , responseGetAwsNetworkPerformanceData $
--             newGetAwsNetworkPerformanceDataResponse
--
--         , responseGetCapacityReservationUsage $
--             newGetCapacityReservationUsageResponse
--
--         , responseGetCoipPoolUsage $
--             newGetCoipPoolUsageResponse
--
--         , responseGetConsoleOutput $
--             newGetConsoleOutputResponse
--
--         , responseGetConsoleScreenshot $
--             newGetConsoleScreenshotResponse
--
--         , responseGetDefaultCreditSpecification $
--             newGetDefaultCreditSpecificationResponse
--
--         , responseGetEbsDefaultKmsKeyId $
--             newGetEbsDefaultKmsKeyIdResponse
--
--         , responseGetEbsEncryptionByDefault $
--             newGetEbsEncryptionByDefaultResponse
--
--         , responseGetFlowLogsIntegrationTemplate $
--             newGetFlowLogsIntegrationTemplateResponse
--
--         , responseGetGroupsForCapacityReservation $
--             newGetGroupsForCapacityReservationResponse
--
--         , responseGetHostReservationPurchasePreview $
--             newGetHostReservationPurchasePreviewResponse
--
--         , responseGetInstanceTypesFromInstanceRequirements $
--             newGetInstanceTypesFromInstanceRequirementsResponse
--
--         , responseGetInstanceUefiData $
--             newGetInstanceUefiDataResponse
--
--         , responseGetIpamAddressHistory $
--             newGetIpamAddressHistoryResponse
--
--         , responseGetIpamPoolAllocations $
--             newGetIpamPoolAllocationsResponse
--
--         , responseGetIpamPoolCidrs $
--             newGetIpamPoolCidrsResponse
--
--         , responseGetIpamResourceCidrs $
--             newGetIpamResourceCidrsResponse
--
--         , responseGetLaunchTemplateData $
--             newGetLaunchTemplateDataResponse
--
--         , responseGetManagedPrefixListAssociations $
--             newGetManagedPrefixListAssociationsResponse
--
--         , responseGetManagedPrefixListEntries $
--             newGetManagedPrefixListEntriesResponse
--
--         , responseGetNetworkInsightsAccessScopeAnalysisFindings $
--             newGetNetworkInsightsAccessScopeAnalysisFindingsResponse
--
--         , responseGetNetworkInsightsAccessScopeContent $
--             newGetNetworkInsightsAccessScopeContentResponse
--
--         , responseGetPasswordData $
--             newGetPasswordDataResponse
--
--         , responseGetReservedInstancesExchangeQuote $
--             newGetReservedInstancesExchangeQuoteResponse
--
--         , responseGetSerialConsoleAccessStatus $
--             newGetSerialConsoleAccessStatusResponse
--
--         , responseGetSpotPlacementScores $
--             newGetSpotPlacementScoresResponse
--
--         , responseGetSubnetCidrReservations $
--             newGetSubnetCidrReservationsResponse
--
--         , responseGetTransitGatewayAttachmentPropagations $
--             newGetTransitGatewayAttachmentPropagationsResponse
--
--         , responseGetTransitGatewayMulticastDomainAssociations $
--             newGetTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseGetTransitGatewayPolicyTableAssociations $
--             newGetTransitGatewayPolicyTableAssociationsResponse
--
--         , responseGetTransitGatewayPolicyTableEntries $
--             newGetTransitGatewayPolicyTableEntriesResponse
--
--         , responseGetTransitGatewayPrefixListReferences $
--             newGetTransitGatewayPrefixListReferencesResponse
--
--         , responseGetTransitGatewayRouteTableAssociations $
--             newGetTransitGatewayRouteTableAssociationsResponse
--
--         , responseGetTransitGatewayRouteTablePropagations $
--             newGetTransitGatewayRouteTablePropagationsResponse
--
--         , responseGetVerifiedAccessEndpointPolicy $
--             newGetVerifiedAccessEndpointPolicyResponse
--
--         , responseGetVerifiedAccessGroupPolicy $
--             newGetVerifiedAccessGroupPolicyResponse
--
--         , responseGetVpnConnectionDeviceSampleConfiguration $
--             newGetVpnConnectionDeviceSampleConfigurationResponse
--
--         , responseGetVpnConnectionDeviceTypes $
--             newGetVpnConnectionDeviceTypesResponse
--
--         , responseImportClientVpnClientCertificateRevocationList $
--             newImportClientVpnClientCertificateRevocationListResponse
--
--         , responseImportImage $
--             newImportImageResponse
--
--         , responseImportInstance $
--             newImportInstanceResponse
--
--         , responseImportKeyPair $
--             newImportKeyPairResponse
--
--         , responseImportSnapshot $
--             newImportSnapshotResponse
--
--         , responseImportVolume $
--             newImportVolumeResponse
--
--         , responseListImagesInRecycleBin $
--             newListImagesInRecycleBinResponse
--
--         , responseListSnapshotsInRecycleBin $
--             newListSnapshotsInRecycleBinResponse
--
--         , responseModifyAddressAttribute $
--             newModifyAddressAttributeResponse
--
--         , responseModifyAvailabilityZoneGroup $
--             newModifyAvailabilityZoneGroupResponse
--
--         , responseModifyCapacityReservation $
--             newModifyCapacityReservationResponse
--
--         , responseModifyCapacityReservationFleet $
--             newModifyCapacityReservationFleetResponse
--
--         , responseModifyClientVpnEndpoint $
--             newModifyClientVpnEndpointResponse
--
--         , responseModifyDefaultCreditSpecification $
--             newModifyDefaultCreditSpecificationResponse
--
--         , responseModifyEbsDefaultKmsKeyId $
--             newModifyEbsDefaultKmsKeyIdResponse
--
--         , responseModifyFleet $
--             newModifyFleetResponse
--
--         , responseModifyFpgaImageAttribute $
--             newModifyFpgaImageAttributeResponse
--
--         , responseModifyHosts $
--             newModifyHostsResponse
--
--         , responseModifyIdFormat $
--             newModifyIdFormatResponse
--
--         , responseModifyIdentityIdFormat $
--             newModifyIdentityIdFormatResponse
--
--         , responseModifyImageAttribute $
--             newModifyImageAttributeResponse
--
--         , responseModifyInstanceAttribute $
--             newModifyInstanceAttributeResponse
--
--         , responseModifyInstanceCapacityReservationAttributes $
--             newModifyInstanceCapacityReservationAttributesResponse
--
--         , responseModifyInstanceCreditSpecification $
--             newModifyInstanceCreditSpecificationResponse
--
--         , responseModifyInstanceEventStartTime $
--             newModifyInstanceEventStartTimeResponse
--
--         , responseModifyInstanceEventWindow $
--             newModifyInstanceEventWindowResponse
--
--         , responseModifyInstanceMaintenanceOptions $
--             newModifyInstanceMaintenanceOptionsResponse
--
--         , responseModifyInstanceMetadataOptions $
--             newModifyInstanceMetadataOptionsResponse
--
--         , responseModifyInstancePlacement $
--             newModifyInstancePlacementResponse
--
--         , responseModifyIpam $
--             newModifyIpamResponse
--
--         , responseModifyIpamPool $
--             newModifyIpamPoolResponse
--
--         , responseModifyIpamResourceCidr $
--             newModifyIpamResourceCidrResponse
--
--         , responseModifyIpamScope $
--             newModifyIpamScopeResponse
--
--         , responseModifyLaunchTemplate $
--             newModifyLaunchTemplateResponse
--
--         , responseModifyLocalGatewayRoute $
--             newModifyLocalGatewayRouteResponse
--
--         , responseModifyManagedPrefixList $
--             newModifyManagedPrefixListResponse
--
--         , responseModifyNetworkInterfaceAttribute $
--             newModifyNetworkInterfaceAttributeResponse
--
--         , responseModifyPrivateDnsNameOptions $
--             newModifyPrivateDnsNameOptionsResponse
--
--         , responseModifyReservedInstances $
--             newModifyReservedInstancesResponse
--
--         , responseModifySecurityGroupRules $
--             newModifySecurityGroupRulesResponse
--
--         , responseModifySnapshotAttribute $
--             newModifySnapshotAttributeResponse
--
--         , responseModifySnapshotTier $
--             newModifySnapshotTierResponse
--
--         , responseModifySpotFleetRequest $
--             newModifySpotFleetRequestResponse
--
--         , responseModifySubnetAttribute $
--             newModifySubnetAttributeResponse
--
--         , responseModifyTrafficMirrorFilterNetworkServices $
--             newModifyTrafficMirrorFilterNetworkServicesResponse
--
--         , responseModifyTrafficMirrorFilterRule $
--             newModifyTrafficMirrorFilterRuleResponse
--
--         , responseModifyTrafficMirrorSession $
--             newModifyTrafficMirrorSessionResponse
--
--         , responseModifyTransitGateway $
--             newModifyTransitGatewayResponse
--
--         , responseModifyTransitGatewayPrefixListReference $
--             newModifyTransitGatewayPrefixListReferenceResponse
--
--         , responseModifyTransitGatewayVpcAttachment $
--             newModifyTransitGatewayVpcAttachmentResponse
--
--         , responseModifyVerifiedAccessEndpoint $
--             newModifyVerifiedAccessEndpointResponse
--
--         , responseModifyVerifiedAccessEndpointPolicy $
--             newModifyVerifiedAccessEndpointPolicyResponse
--
--         , responseModifyVerifiedAccessGroup $
--             newModifyVerifiedAccessGroupResponse
--
--         , responseModifyVerifiedAccessGroupPolicy $
--             newModifyVerifiedAccessGroupPolicyResponse
--
--         , responseModifyVerifiedAccessInstance $
--             newModifyVerifiedAccessInstanceResponse
--
--         , responseModifyVerifiedAccessInstanceLoggingConfiguration $
--             newModifyVerifiedAccessInstanceLoggingConfigurationResponse
--
--         , responseModifyVerifiedAccessTrustProvider $
--             newModifyVerifiedAccessTrustProviderResponse
--
--         , responseModifyVolume $
--             newModifyVolumeResponse
--
--         , responseModifyVolumeAttribute $
--             newModifyVolumeAttributeResponse
--
--         , responseModifyVpcAttribute $
--             newModifyVpcAttributeResponse
--
--         , responseModifyVpcEndpoint $
--             newModifyVpcEndpointResponse
--
--         , responseModifyVpcEndpointConnectionNotification $
--             newModifyVpcEndpointConnectionNotificationResponse
--
--         , responseModifyVpcEndpointServiceConfiguration $
--             newModifyVpcEndpointServiceConfigurationResponse
--
--         , responseModifyVpcEndpointServicePayerResponsibility $
--             newModifyVpcEndpointServicePayerResponsibilityResponse
--
--         , responseModifyVpcEndpointServicePermissions $
--             newModifyVpcEndpointServicePermissionsResponse
--
--         , responseModifyVpcPeeringConnectionOptions $
--             newModifyVpcPeeringConnectionOptionsResponse
--
--         , responseModifyVpcTenancy $
--             newModifyVpcTenancyResponse
--
--         , responseModifyVpnConnection $
--             newModifyVpnConnectionResponse
--
--         , responseModifyVpnConnectionOptions $
--             newModifyVpnConnectionOptionsResponse
--
--         , responseModifyVpnTunnelCertificate $
--             newModifyVpnTunnelCertificateResponse
--
--         , responseModifyVpnTunnelOptions $
--             newModifyVpnTunnelOptionsResponse
--
--         , responseMonitorInstances $
--             newMonitorInstancesResponse
--
--         , responseMoveAddressToVpc $
--             newMoveAddressToVpcResponse
--
--         , responseMoveByoipCidrToIpam $
--             newMoveByoipCidrToIpamResponse
--
--         , responseProvisionByoipCidr $
--             newProvisionByoipCidrResponse
--
--         , responseProvisionIpamPoolCidr $
--             newProvisionIpamPoolCidrResponse
--
--         , responseProvisionPublicIpv4PoolCidr $
--             newProvisionPublicIpv4PoolCidrResponse
--
--         , responsePurchaseHostReservation $
--             newPurchaseHostReservationResponse
--
--         , responsePurchaseReservedInstancesOffering $
--             newPurchaseReservedInstancesOfferingResponse
--
--         , responsePurchaseScheduledInstances $
--             newPurchaseScheduledInstancesResponse
--
--         , responseRebootInstances $
--             newRebootInstancesResponse
--
--         , responseRegisterImage $
--             newRegisterImageResponse
--
--         , responseRegisterInstanceEventNotificationAttributes $
--             newRegisterInstanceEventNotificationAttributesResponse
--
--         , responseRegisterTransitGatewayMulticastGroupMembers $
--             newRegisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseRegisterTransitGatewayMulticastGroupSources $
--             newRegisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseRejectTransitGatewayMulticastDomainAssociations $
--             newRejectTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseRejectTransitGatewayPeeringAttachment $
--             newRejectTransitGatewayPeeringAttachmentResponse
--
--         , responseRejectTransitGatewayVpcAttachment $
--             newRejectTransitGatewayVpcAttachmentResponse
--
--         , responseRejectVpcEndpointConnections $
--             newRejectVpcEndpointConnectionsResponse
--
--         , responseRejectVpcPeeringConnection $
--             newRejectVpcPeeringConnectionResponse
--
--         , responseReleaseAddress $
--             newReleaseAddressResponse
--
--         , responseReleaseHosts $
--             newReleaseHostsResponse
--
--         , responseReleaseIpamPoolAllocation $
--             newReleaseIpamPoolAllocationResponse
--
--         , responseReplaceIamInstanceProfileAssociation $
--             newReplaceIamInstanceProfileAssociationResponse
--
--         , responseReplaceNetworkAclAssociation $
--             newReplaceNetworkAclAssociationResponse
--
--         , responseReplaceNetworkAclEntry $
--             newReplaceNetworkAclEntryResponse
--
--         , responseReplaceRoute $
--             newReplaceRouteResponse
--
--         , responseReplaceRouteTableAssociation $
--             newReplaceRouteTableAssociationResponse
--
--         , responseReplaceTransitGatewayRoute $
--             newReplaceTransitGatewayRouteResponse
--
--         , responseReportInstanceStatus $
--             newReportInstanceStatusResponse
--
--         , responseRequestSpotFleet $
--             newRequestSpotFleetResponse
--
--         , responseRequestSpotInstances $
--             newRequestSpotInstancesResponse
--
--         , responseResetAddressAttribute $
--             newResetAddressAttributeResponse
--
--         , responseResetEbsDefaultKmsKeyId $
--             newResetEbsDefaultKmsKeyIdResponse
--
--         , responseResetFpgaImageAttribute $
--             newResetFpgaImageAttributeResponse
--
--         , responseResetImageAttribute $
--             newResetImageAttributeResponse
--
--         , responseResetInstanceAttribute $
--             newResetInstanceAttributeResponse
--
--         , responseResetNetworkInterfaceAttribute $
--             newResetNetworkInterfaceAttributeResponse
--
--         , responseResetSnapshotAttribute $
--             newResetSnapshotAttributeResponse
--
--         , responseRestoreAddressToClassic $
--             newRestoreAddressToClassicResponse
--
--         , responseRestoreImageFromRecycleBin $
--             newRestoreImageFromRecycleBinResponse
--
--         , responseRestoreManagedPrefixListVersion $
--             newRestoreManagedPrefixListVersionResponse
--
--         , responseRestoreSnapshotFromRecycleBin $
--             newRestoreSnapshotFromRecycleBinResponse
--
--         , responseRestoreSnapshotTier $
--             newRestoreSnapshotTierResponse
--
--         , responseRevokeClientVpnIngress $
--             newRevokeClientVpnIngressResponse
--
--         , responseRevokeSecurityGroupEgress $
--             newRevokeSecurityGroupEgressResponse
--
--         , responseRevokeSecurityGroupIngress $
--             newRevokeSecurityGroupIngressResponse
--
--         , responseRunInstances $
--             newReservation
--
--         , responseRunScheduledInstances $
--             newRunScheduledInstancesResponse
--
--         , responseSearchLocalGatewayRoutes $
--             newSearchLocalGatewayRoutesResponse
--
--         , responseSearchTransitGatewayMulticastGroups $
--             newSearchTransitGatewayMulticastGroupsResponse
--
--         , responseSearchTransitGatewayRoutes $
--             newSearchTransitGatewayRoutesResponse
--
--         , responseSendDiagnosticInterrupt $
--             newSendDiagnosticInterruptResponse
--
--         , responseStartInstances $
--             newStartInstancesResponse
--
--         , responseStartNetworkInsightsAccessScopeAnalysis $
--             newStartNetworkInsightsAccessScopeAnalysisResponse
--
--         , responseStartNetworkInsightsAnalysis $
--             newStartNetworkInsightsAnalysisResponse
--
--         , responseStartVpcEndpointServicePrivateDnsVerification $
--             newStartVpcEndpointServicePrivateDnsVerificationResponse
--
--         , responseStopInstances $
--             newStopInstancesResponse
--
--         , responseTerminateClientVpnConnections $
--             newTerminateClientVpnConnectionsResponse
--
--         , responseTerminateInstances $
--             newTerminateInstancesResponse
--
--         , responseUnassignIpv6Addresses $
--             newUnassignIpv6AddressesResponse
--
--         , responseUnassignPrivateIpAddresses $
--             newUnassignPrivateIpAddressesResponse
--
--         , responseUnmonitorInstances $
--             newUnmonitorInstancesResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsEgress $
--             newUpdateSecurityGroupRuleDescriptionsEgressResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsIngress $
--             newUpdateSecurityGroupRuleDescriptionsIngressResponse
--
--         , responseWithdrawByoipCidr $
--             newWithdrawByoipCidrResponse
--
--           ]
--     ]

-- Requests

requestAcceptAddressTransfer :: AcceptAddressTransfer -> TestTree
requestAcceptAddressTransfer =
  req
    "AcceptAddressTransfer"
    "fixture/AcceptAddressTransfer.yaml"

requestAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuote -> TestTree
requestAcceptReservedInstancesExchangeQuote =
  req
    "AcceptReservedInstancesExchangeQuote"
    "fixture/AcceptReservedInstancesExchangeQuote.yaml"

requestAcceptTransitGatewayMulticastDomainAssociations :: AcceptTransitGatewayMulticastDomainAssociations -> TestTree
requestAcceptTransitGatewayMulticastDomainAssociations =
  req
    "AcceptTransitGatewayMulticastDomainAssociations"
    "fixture/AcceptTransitGatewayMulticastDomainAssociations.yaml"

requestAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachment -> TestTree
requestAcceptTransitGatewayPeeringAttachment =
  req
    "AcceptTransitGatewayPeeringAttachment"
    "fixture/AcceptTransitGatewayPeeringAttachment.yaml"

requestAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachment -> TestTree
requestAcceptTransitGatewayVpcAttachment =
  req
    "AcceptTransitGatewayVpcAttachment"
    "fixture/AcceptTransitGatewayVpcAttachment.yaml"

requestAcceptVpcEndpointConnections :: AcceptVpcEndpointConnections -> TestTree
requestAcceptVpcEndpointConnections =
  req
    "AcceptVpcEndpointConnections"
    "fixture/AcceptVpcEndpointConnections.yaml"

requestAcceptVpcPeeringConnection :: AcceptVpcPeeringConnection -> TestTree
requestAcceptVpcPeeringConnection =
  req
    "AcceptVpcPeeringConnection"
    "fixture/AcceptVpcPeeringConnection.yaml"

requestAdvertiseByoipCidr :: AdvertiseByoipCidr -> TestTree
requestAdvertiseByoipCidr =
  req
    "AdvertiseByoipCidr"
    "fixture/AdvertiseByoipCidr.yaml"

requestAllocateAddress :: AllocateAddress -> TestTree
requestAllocateAddress =
  req
    "AllocateAddress"
    "fixture/AllocateAddress.yaml"

requestAllocateHosts :: AllocateHosts -> TestTree
requestAllocateHosts =
  req
    "AllocateHosts"
    "fixture/AllocateHosts.yaml"

requestAllocateIpamPoolCidr :: AllocateIpamPoolCidr -> TestTree
requestAllocateIpamPoolCidr =
  req
    "AllocateIpamPoolCidr"
    "fixture/AllocateIpamPoolCidr.yaml"

requestApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetwork -> TestTree
requestApplySecurityGroupsToClientVpnTargetNetwork =
  req
    "ApplySecurityGroupsToClientVpnTargetNetwork"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetwork.yaml"

requestAssignIpv6Addresses :: AssignIpv6Addresses -> TestTree
requestAssignIpv6Addresses =
  req
    "AssignIpv6Addresses"
    "fixture/AssignIpv6Addresses.yaml"

requestAssignPrivateIpAddresses :: AssignPrivateIpAddresses -> TestTree
requestAssignPrivateIpAddresses =
  req
    "AssignPrivateIpAddresses"
    "fixture/AssignPrivateIpAddresses.yaml"

requestAssociateAddress :: AssociateAddress -> TestTree
requestAssociateAddress =
  req
    "AssociateAddress"
    "fixture/AssociateAddress.yaml"

requestAssociateClientVpnTargetNetwork :: AssociateClientVpnTargetNetwork -> TestTree
requestAssociateClientVpnTargetNetwork =
  req
    "AssociateClientVpnTargetNetwork"
    "fixture/AssociateClientVpnTargetNetwork.yaml"

requestAssociateDhcpOptions :: AssociateDhcpOptions -> TestTree
requestAssociateDhcpOptions =
  req
    "AssociateDhcpOptions"
    "fixture/AssociateDhcpOptions.yaml"

requestAssociateEnclaveCertificateIamRole :: AssociateEnclaveCertificateIamRole -> TestTree
requestAssociateEnclaveCertificateIamRole =
  req
    "AssociateEnclaveCertificateIamRole"
    "fixture/AssociateEnclaveCertificateIamRole.yaml"

requestAssociateIamInstanceProfile :: AssociateIamInstanceProfile -> TestTree
requestAssociateIamInstanceProfile =
  req
    "AssociateIamInstanceProfile"
    "fixture/AssociateIamInstanceProfile.yaml"

requestAssociateInstanceEventWindow :: AssociateInstanceEventWindow -> TestTree
requestAssociateInstanceEventWindow =
  req
    "AssociateInstanceEventWindow"
    "fixture/AssociateInstanceEventWindow.yaml"

requestAssociateRouteTable :: AssociateRouteTable -> TestTree
requestAssociateRouteTable =
  req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable.yaml"

requestAssociateSubnetCidrBlock :: AssociateSubnetCidrBlock -> TestTree
requestAssociateSubnetCidrBlock =
  req
    "AssociateSubnetCidrBlock"
    "fixture/AssociateSubnetCidrBlock.yaml"

requestAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomain -> TestTree
requestAssociateTransitGatewayMulticastDomain =
  req
    "AssociateTransitGatewayMulticastDomain"
    "fixture/AssociateTransitGatewayMulticastDomain.yaml"

requestAssociateTransitGatewayPolicyTable :: AssociateTransitGatewayPolicyTable -> TestTree
requestAssociateTransitGatewayPolicyTable =
  req
    "AssociateTransitGatewayPolicyTable"
    "fixture/AssociateTransitGatewayPolicyTable.yaml"

requestAssociateTransitGatewayRouteTable :: AssociateTransitGatewayRouteTable -> TestTree
requestAssociateTransitGatewayRouteTable =
  req
    "AssociateTransitGatewayRouteTable"
    "fixture/AssociateTransitGatewayRouteTable.yaml"

requestAssociateTrunkInterface :: AssociateTrunkInterface -> TestTree
requestAssociateTrunkInterface =
  req
    "AssociateTrunkInterface"
    "fixture/AssociateTrunkInterface.yaml"

requestAssociateVpcCidrBlock :: AssociateVpcCidrBlock -> TestTree
requestAssociateVpcCidrBlock =
  req
    "AssociateVpcCidrBlock"
    "fixture/AssociateVpcCidrBlock.yaml"

requestAttachClassicLinkVpc :: AttachClassicLinkVpc -> TestTree
requestAttachClassicLinkVpc =
  req
    "AttachClassicLinkVpc"
    "fixture/AttachClassicLinkVpc.yaml"

requestAttachInternetGateway :: AttachInternetGateway -> TestTree
requestAttachInternetGateway =
  req
    "AttachInternetGateway"
    "fixture/AttachInternetGateway.yaml"

requestAttachNetworkInterface :: AttachNetworkInterface -> TestTree
requestAttachNetworkInterface =
  req
    "AttachNetworkInterface"
    "fixture/AttachNetworkInterface.yaml"

requestAttachVerifiedAccessTrustProvider :: AttachVerifiedAccessTrustProvider -> TestTree
requestAttachVerifiedAccessTrustProvider =
  req
    "AttachVerifiedAccessTrustProvider"
    "fixture/AttachVerifiedAccessTrustProvider.yaml"

requestAttachVolume :: AttachVolume -> TestTree
requestAttachVolume =
  req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

requestAttachVpnGateway :: AttachVpnGateway -> TestTree
requestAttachVpnGateway =
  req
    "AttachVpnGateway"
    "fixture/AttachVpnGateway.yaml"

requestAuthorizeClientVpnIngress :: AuthorizeClientVpnIngress -> TestTree
requestAuthorizeClientVpnIngress =
  req
    "AuthorizeClientVpnIngress"
    "fixture/AuthorizeClientVpnIngress.yaml"

requestAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
requestAuthorizeSecurityGroupEgress =
  req
    "AuthorizeSecurityGroupEgress"
    "fixture/AuthorizeSecurityGroupEgress.yaml"

requestAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
requestAuthorizeSecurityGroupIngress =
  req
    "AuthorizeSecurityGroupIngress"
    "fixture/AuthorizeSecurityGroupIngress.yaml"

requestBundleInstance :: BundleInstance -> TestTree
requestBundleInstance =
  req
    "BundleInstance"
    "fixture/BundleInstance.yaml"

requestCancelBundleTask :: CancelBundleTask -> TestTree
requestCancelBundleTask =
  req
    "CancelBundleTask"
    "fixture/CancelBundleTask.yaml"

requestCancelCapacityReservation :: CancelCapacityReservation -> TestTree
requestCancelCapacityReservation =
  req
    "CancelCapacityReservation"
    "fixture/CancelCapacityReservation.yaml"

requestCancelCapacityReservationFleets :: CancelCapacityReservationFleets -> TestTree
requestCancelCapacityReservationFleets =
  req
    "CancelCapacityReservationFleets"
    "fixture/CancelCapacityReservationFleets.yaml"

requestCancelConversionTask :: CancelConversionTask -> TestTree
requestCancelConversionTask =
  req
    "CancelConversionTask"
    "fixture/CancelConversionTask.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestCancelImageLaunchPermission :: CancelImageLaunchPermission -> TestTree
requestCancelImageLaunchPermission =
  req
    "CancelImageLaunchPermission"
    "fixture/CancelImageLaunchPermission.yaml"

requestCancelImportTask :: CancelImportTask -> TestTree
requestCancelImportTask =
  req
    "CancelImportTask"
    "fixture/CancelImportTask.yaml"

requestCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
requestCancelReservedInstancesListing =
  req
    "CancelReservedInstancesListing"
    "fixture/CancelReservedInstancesListing.yaml"

requestCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
requestCancelSpotFleetRequests =
  req
    "CancelSpotFleetRequests"
    "fixture/CancelSpotFleetRequests.yaml"

requestCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
requestCancelSpotInstanceRequests =
  req
    "CancelSpotInstanceRequests"
    "fixture/CancelSpotInstanceRequests.yaml"

requestConfirmProductInstance :: ConfirmProductInstance -> TestTree
requestConfirmProductInstance =
  req
    "ConfirmProductInstance"
    "fixture/ConfirmProductInstance.yaml"

requestCopyFpgaImage :: CopyFpgaImage -> TestTree
requestCopyFpgaImage =
  req
    "CopyFpgaImage"
    "fixture/CopyFpgaImage.yaml"

requestCopyImage :: CopyImage -> TestTree
requestCopyImage =
  req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestCreateCapacityReservation :: CreateCapacityReservation -> TestTree
requestCreateCapacityReservation =
  req
    "CreateCapacityReservation"
    "fixture/CreateCapacityReservation.yaml"

requestCreateCapacityReservationFleet :: CreateCapacityReservationFleet -> TestTree
requestCreateCapacityReservationFleet =
  req
    "CreateCapacityReservationFleet"
    "fixture/CreateCapacityReservationFleet.yaml"

requestCreateCarrierGateway :: CreateCarrierGateway -> TestTree
requestCreateCarrierGateway =
  req
    "CreateCarrierGateway"
    "fixture/CreateCarrierGateway.yaml"

requestCreateClientVpnEndpoint :: CreateClientVpnEndpoint -> TestTree
requestCreateClientVpnEndpoint =
  req
    "CreateClientVpnEndpoint"
    "fixture/CreateClientVpnEndpoint.yaml"

requestCreateClientVpnRoute :: CreateClientVpnRoute -> TestTree
requestCreateClientVpnRoute =
  req
    "CreateClientVpnRoute"
    "fixture/CreateClientVpnRoute.yaml"

requestCreateCoipCidr :: CreateCoipCidr -> TestTree
requestCreateCoipCidr =
  req
    "CreateCoipCidr"
    "fixture/CreateCoipCidr.yaml"

requestCreateCoipPool :: CreateCoipPool -> TestTree
requestCreateCoipPool =
  req
    "CreateCoipPool"
    "fixture/CreateCoipPool.yaml"

requestCreateCustomerGateway :: CreateCustomerGateway -> TestTree
requestCreateCustomerGateway =
  req
    "CreateCustomerGateway"
    "fixture/CreateCustomerGateway.yaml"

requestCreateDefaultSubnet :: CreateDefaultSubnet -> TestTree
requestCreateDefaultSubnet =
  req
    "CreateDefaultSubnet"
    "fixture/CreateDefaultSubnet.yaml"

requestCreateDefaultVpc :: CreateDefaultVpc -> TestTree
requestCreateDefaultVpc =
  req
    "CreateDefaultVpc"
    "fixture/CreateDefaultVpc.yaml"

requestCreateDhcpOptions :: CreateDhcpOptions -> TestTree
requestCreateDhcpOptions =
  req
    "CreateDhcpOptions"
    "fixture/CreateDhcpOptions.yaml"

requestCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGateway -> TestTree
requestCreateEgressOnlyInternetGateway =
  req
    "CreateEgressOnlyInternetGateway"
    "fixture/CreateEgressOnlyInternetGateway.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestCreateFlowLogs :: CreateFlowLogs -> TestTree
requestCreateFlowLogs =
  req
    "CreateFlowLogs"
    "fixture/CreateFlowLogs.yaml"

requestCreateFpgaImage :: CreateFpgaImage -> TestTree
requestCreateFpgaImage =
  req
    "CreateFpgaImage"
    "fixture/CreateFpgaImage.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestCreateInstanceEventWindow :: CreateInstanceEventWindow -> TestTree
requestCreateInstanceEventWindow =
  req
    "CreateInstanceEventWindow"
    "fixture/CreateInstanceEventWindow.yaml"

requestCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
requestCreateInstanceExportTask =
  req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask.yaml"

requestCreateInternetGateway :: CreateInternetGateway -> TestTree
requestCreateInternetGateway =
  req
    "CreateInternetGateway"
    "fixture/CreateInternetGateway.yaml"

requestCreateIpam :: CreateIpam -> TestTree
requestCreateIpam =
  req
    "CreateIpam"
    "fixture/CreateIpam.yaml"

requestCreateIpamPool :: CreateIpamPool -> TestTree
requestCreateIpamPool =
  req
    "CreateIpamPool"
    "fixture/CreateIpamPool.yaml"

requestCreateIpamScope :: CreateIpamScope -> TestTree
requestCreateIpamScope =
  req
    "CreateIpamScope"
    "fixture/CreateIpamScope.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair =
  req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestCreateLaunchTemplate :: CreateLaunchTemplate -> TestTree
requestCreateLaunchTemplate =
  req
    "CreateLaunchTemplate"
    "fixture/CreateLaunchTemplate.yaml"

requestCreateLaunchTemplateVersion :: CreateLaunchTemplateVersion -> TestTree
requestCreateLaunchTemplateVersion =
  req
    "CreateLaunchTemplateVersion"
    "fixture/CreateLaunchTemplateVersion.yaml"

requestCreateLocalGatewayRoute :: CreateLocalGatewayRoute -> TestTree
requestCreateLocalGatewayRoute =
  req
    "CreateLocalGatewayRoute"
    "fixture/CreateLocalGatewayRoute.yaml"

requestCreateLocalGatewayRouteTable :: CreateLocalGatewayRouteTable -> TestTree
requestCreateLocalGatewayRouteTable =
  req
    "CreateLocalGatewayRouteTable"
    "fixture/CreateLocalGatewayRouteTable.yaml"

requestCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation :: CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation -> TestTree
requestCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
  req
    "CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation"
    "fixture/CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation.yaml"

requestCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociation -> TestTree
requestCreateLocalGatewayRouteTableVpcAssociation =
  req
    "CreateLocalGatewayRouteTableVpcAssociation"
    "fixture/CreateLocalGatewayRouteTableVpcAssociation.yaml"

requestCreateManagedPrefixList :: CreateManagedPrefixList -> TestTree
requestCreateManagedPrefixList =
  req
    "CreateManagedPrefixList"
    "fixture/CreateManagedPrefixList.yaml"

requestCreateNatGateway :: CreateNatGateway -> TestTree
requestCreateNatGateway =
  req
    "CreateNatGateway"
    "fixture/CreateNatGateway.yaml"

requestCreateNetworkAcl :: CreateNetworkAcl -> TestTree
requestCreateNetworkAcl =
  req
    "CreateNetworkAcl"
    "fixture/CreateNetworkAcl.yaml"

requestCreateNetworkAclEntry :: CreateNetworkAclEntry -> TestTree
requestCreateNetworkAclEntry =
  req
    "CreateNetworkAclEntry"
    "fixture/CreateNetworkAclEntry.yaml"

requestCreateNetworkInsightsAccessScope :: CreateNetworkInsightsAccessScope -> TestTree
requestCreateNetworkInsightsAccessScope =
  req
    "CreateNetworkInsightsAccessScope"
    "fixture/CreateNetworkInsightsAccessScope.yaml"

requestCreateNetworkInsightsPath :: CreateNetworkInsightsPath -> TestTree
requestCreateNetworkInsightsPath =
  req
    "CreateNetworkInsightsPath"
    "fixture/CreateNetworkInsightsPath.yaml"

requestCreateNetworkInterface :: CreateNetworkInterface -> TestTree
requestCreateNetworkInterface =
  req
    "CreateNetworkInterface"
    "fixture/CreateNetworkInterface.yaml"

requestCreateNetworkInterfacePermission :: CreateNetworkInterfacePermission -> TestTree
requestCreateNetworkInterfacePermission =
  req
    "CreateNetworkInterfacePermission"
    "fixture/CreateNetworkInterfacePermission.yaml"

requestCreatePlacementGroup :: CreatePlacementGroup -> TestTree
requestCreatePlacementGroup =
  req
    "CreatePlacementGroup"
    "fixture/CreatePlacementGroup.yaml"

requestCreatePublicIpv4Pool :: CreatePublicIpv4Pool -> TestTree
requestCreatePublicIpv4Pool =
  req
    "CreatePublicIpv4Pool"
    "fixture/CreatePublicIpv4Pool.yaml"

requestCreateReplaceRootVolumeTask :: CreateReplaceRootVolumeTask -> TestTree
requestCreateReplaceRootVolumeTask =
  req
    "CreateReplaceRootVolumeTask"
    "fixture/CreateReplaceRootVolumeTask.yaml"

requestCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
requestCreateReservedInstancesListing =
  req
    "CreateReservedInstancesListing"
    "fixture/CreateReservedInstancesListing.yaml"

requestCreateRestoreImageTask :: CreateRestoreImageTask -> TestTree
requestCreateRestoreImageTask =
  req
    "CreateRestoreImageTask"
    "fixture/CreateRestoreImageTask.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestCreateRouteTable :: CreateRouteTable -> TestTree
requestCreateRouteTable =
  req
    "CreateRouteTable"
    "fixture/CreateRouteTable.yaml"

requestCreateSecurityGroup :: CreateSecurityGroup -> TestTree
requestCreateSecurityGroup =
  req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateSnapshots :: CreateSnapshots -> TestTree
requestCreateSnapshots =
  req
    "CreateSnapshots"
    "fixture/CreateSnapshots.yaml"

requestCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscription -> TestTree
requestCreateSpotDatafeedSubscription =
  req
    "CreateSpotDatafeedSubscription"
    "fixture/CreateSpotDatafeedSubscription.yaml"

requestCreateStoreImageTask :: CreateStoreImageTask -> TestTree
requestCreateStoreImageTask =
  req
    "CreateStoreImageTask"
    "fixture/CreateStoreImageTask.yaml"

requestCreateSubnet :: CreateSubnet -> TestTree
requestCreateSubnet =
  req
    "CreateSubnet"
    "fixture/CreateSubnet.yaml"

requestCreateSubnetCidrReservation :: CreateSubnetCidrReservation -> TestTree
requestCreateSubnetCidrReservation =
  req
    "CreateSubnetCidrReservation"
    "fixture/CreateSubnetCidrReservation.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestCreateTrafficMirrorFilter :: CreateTrafficMirrorFilter -> TestTree
requestCreateTrafficMirrorFilter =
  req
    "CreateTrafficMirrorFilter"
    "fixture/CreateTrafficMirrorFilter.yaml"

requestCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRule -> TestTree
requestCreateTrafficMirrorFilterRule =
  req
    "CreateTrafficMirrorFilterRule"
    "fixture/CreateTrafficMirrorFilterRule.yaml"

requestCreateTrafficMirrorSession :: CreateTrafficMirrorSession -> TestTree
requestCreateTrafficMirrorSession =
  req
    "CreateTrafficMirrorSession"
    "fixture/CreateTrafficMirrorSession.yaml"

requestCreateTrafficMirrorTarget :: CreateTrafficMirrorTarget -> TestTree
requestCreateTrafficMirrorTarget =
  req
    "CreateTrafficMirrorTarget"
    "fixture/CreateTrafficMirrorTarget.yaml"

requestCreateTransitGateway :: CreateTransitGateway -> TestTree
requestCreateTransitGateway =
  req
    "CreateTransitGateway"
    "fixture/CreateTransitGateway.yaml"

requestCreateTransitGatewayConnect :: CreateTransitGatewayConnect -> TestTree
requestCreateTransitGatewayConnect =
  req
    "CreateTransitGatewayConnect"
    "fixture/CreateTransitGatewayConnect.yaml"

requestCreateTransitGatewayConnectPeer :: CreateTransitGatewayConnectPeer -> TestTree
requestCreateTransitGatewayConnectPeer =
  req
    "CreateTransitGatewayConnectPeer"
    "fixture/CreateTransitGatewayConnectPeer.yaml"

requestCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomain -> TestTree
requestCreateTransitGatewayMulticastDomain =
  req
    "CreateTransitGatewayMulticastDomain"
    "fixture/CreateTransitGatewayMulticastDomain.yaml"

requestCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachment -> TestTree
requestCreateTransitGatewayPeeringAttachment =
  req
    "CreateTransitGatewayPeeringAttachment"
    "fixture/CreateTransitGatewayPeeringAttachment.yaml"

requestCreateTransitGatewayPolicyTable :: CreateTransitGatewayPolicyTable -> TestTree
requestCreateTransitGatewayPolicyTable =
  req
    "CreateTransitGatewayPolicyTable"
    "fixture/CreateTransitGatewayPolicyTable.yaml"

requestCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReference -> TestTree
requestCreateTransitGatewayPrefixListReference =
  req
    "CreateTransitGatewayPrefixListReference"
    "fixture/CreateTransitGatewayPrefixListReference.yaml"

requestCreateTransitGatewayRoute :: CreateTransitGatewayRoute -> TestTree
requestCreateTransitGatewayRoute =
  req
    "CreateTransitGatewayRoute"
    "fixture/CreateTransitGatewayRoute.yaml"

requestCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTable -> TestTree
requestCreateTransitGatewayRouteTable =
  req
    "CreateTransitGatewayRouteTable"
    "fixture/CreateTransitGatewayRouteTable.yaml"

requestCreateTransitGatewayRouteTableAnnouncement :: CreateTransitGatewayRouteTableAnnouncement -> TestTree
requestCreateTransitGatewayRouteTableAnnouncement =
  req
    "CreateTransitGatewayRouteTableAnnouncement"
    "fixture/CreateTransitGatewayRouteTableAnnouncement.yaml"

requestCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachment -> TestTree
requestCreateTransitGatewayVpcAttachment =
  req
    "CreateTransitGatewayVpcAttachment"
    "fixture/CreateTransitGatewayVpcAttachment.yaml"

requestCreateVerifiedAccessEndpoint :: CreateVerifiedAccessEndpoint -> TestTree
requestCreateVerifiedAccessEndpoint =
  req
    "CreateVerifiedAccessEndpoint"
    "fixture/CreateVerifiedAccessEndpoint.yaml"

requestCreateVerifiedAccessGroup :: CreateVerifiedAccessGroup -> TestTree
requestCreateVerifiedAccessGroup =
  req
    "CreateVerifiedAccessGroup"
    "fixture/CreateVerifiedAccessGroup.yaml"

requestCreateVerifiedAccessInstance :: CreateVerifiedAccessInstance -> TestTree
requestCreateVerifiedAccessInstance =
  req
    "CreateVerifiedAccessInstance"
    "fixture/CreateVerifiedAccessInstance.yaml"

requestCreateVerifiedAccessTrustProvider :: CreateVerifiedAccessTrustProvider -> TestTree
requestCreateVerifiedAccessTrustProvider =
  req
    "CreateVerifiedAccessTrustProvider"
    "fixture/CreateVerifiedAccessTrustProvider.yaml"

requestCreateVolume :: CreateVolume -> TestTree
requestCreateVolume =
  req
    "CreateVolume"
    "fixture/CreateVolume.yaml"

requestCreateVpc :: CreateVpc -> TestTree
requestCreateVpc =
  req
    "CreateVpc"
    "fixture/CreateVpc.yaml"

requestCreateVpcEndpoint :: CreateVpcEndpoint -> TestTree
requestCreateVpcEndpoint =
  req
    "CreateVpcEndpoint"
    "fixture/CreateVpcEndpoint.yaml"

requestCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotification -> TestTree
requestCreateVpcEndpointConnectionNotification =
  req
    "CreateVpcEndpointConnectionNotification"
    "fixture/CreateVpcEndpointConnectionNotification.yaml"

requestCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfiguration -> TestTree
requestCreateVpcEndpointServiceConfiguration =
  req
    "CreateVpcEndpointServiceConfiguration"
    "fixture/CreateVpcEndpointServiceConfiguration.yaml"

requestCreateVpcPeeringConnection :: CreateVpcPeeringConnection -> TestTree
requestCreateVpcPeeringConnection =
  req
    "CreateVpcPeeringConnection"
    "fixture/CreateVpcPeeringConnection.yaml"

requestCreateVpnConnection :: CreateVpnConnection -> TestTree
requestCreateVpnConnection =
  req
    "CreateVpnConnection"
    "fixture/CreateVpnConnection.yaml"

requestCreateVpnConnectionRoute :: CreateVpnConnectionRoute -> TestTree
requestCreateVpnConnectionRoute =
  req
    "CreateVpnConnectionRoute"
    "fixture/CreateVpnConnectionRoute.yaml"

requestCreateVpnGateway :: CreateVpnGateway -> TestTree
requestCreateVpnGateway =
  req
    "CreateVpnGateway"
    "fixture/CreateVpnGateway.yaml"

requestDeleteCarrierGateway :: DeleteCarrierGateway -> TestTree
requestDeleteCarrierGateway =
  req
    "DeleteCarrierGateway"
    "fixture/DeleteCarrierGateway.yaml"

requestDeleteClientVpnEndpoint :: DeleteClientVpnEndpoint -> TestTree
requestDeleteClientVpnEndpoint =
  req
    "DeleteClientVpnEndpoint"
    "fixture/DeleteClientVpnEndpoint.yaml"

requestDeleteClientVpnRoute :: DeleteClientVpnRoute -> TestTree
requestDeleteClientVpnRoute =
  req
    "DeleteClientVpnRoute"
    "fixture/DeleteClientVpnRoute.yaml"

requestDeleteCoipCidr :: DeleteCoipCidr -> TestTree
requestDeleteCoipCidr =
  req
    "DeleteCoipCidr"
    "fixture/DeleteCoipCidr.yaml"

requestDeleteCoipPool :: DeleteCoipPool -> TestTree
requestDeleteCoipPool =
  req
    "DeleteCoipPool"
    "fixture/DeleteCoipPool.yaml"

requestDeleteCustomerGateway :: DeleteCustomerGateway -> TestTree
requestDeleteCustomerGateway =
  req
    "DeleteCustomerGateway"
    "fixture/DeleteCustomerGateway.yaml"

requestDeleteDhcpOptions :: DeleteDhcpOptions -> TestTree
requestDeleteDhcpOptions =
  req
    "DeleteDhcpOptions"
    "fixture/DeleteDhcpOptions.yaml"

requestDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGateway -> TestTree
requestDeleteEgressOnlyInternetGateway =
  req
    "DeleteEgressOnlyInternetGateway"
    "fixture/DeleteEgressOnlyInternetGateway.yaml"

requestDeleteFleets :: DeleteFleets -> TestTree
requestDeleteFleets =
  req
    "DeleteFleets"
    "fixture/DeleteFleets.yaml"

requestDeleteFlowLogs :: DeleteFlowLogs -> TestTree
requestDeleteFlowLogs =
  req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs.yaml"

requestDeleteFpgaImage :: DeleteFpgaImage -> TestTree
requestDeleteFpgaImage =
  req
    "DeleteFpgaImage"
    "fixture/DeleteFpgaImage.yaml"

requestDeleteInstanceEventWindow :: DeleteInstanceEventWindow -> TestTree
requestDeleteInstanceEventWindow =
  req
    "DeleteInstanceEventWindow"
    "fixture/DeleteInstanceEventWindow.yaml"

requestDeleteInternetGateway :: DeleteInternetGateway -> TestTree
requestDeleteInternetGateway =
  req
    "DeleteInternetGateway"
    "fixture/DeleteInternetGateway.yaml"

requestDeleteIpam :: DeleteIpam -> TestTree
requestDeleteIpam =
  req
    "DeleteIpam"
    "fixture/DeleteIpam.yaml"

requestDeleteIpamPool :: DeleteIpamPool -> TestTree
requestDeleteIpamPool =
  req
    "DeleteIpamPool"
    "fixture/DeleteIpamPool.yaml"

requestDeleteIpamScope :: DeleteIpamScope -> TestTree
requestDeleteIpamScope =
  req
    "DeleteIpamScope"
    "fixture/DeleteIpamScope.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair =
  req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestDeleteLaunchTemplate :: DeleteLaunchTemplate -> TestTree
requestDeleteLaunchTemplate =
  req
    "DeleteLaunchTemplate"
    "fixture/DeleteLaunchTemplate.yaml"

requestDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersions -> TestTree
requestDeleteLaunchTemplateVersions =
  req
    "DeleteLaunchTemplateVersions"
    "fixture/DeleteLaunchTemplateVersions.yaml"

requestDeleteLocalGatewayRoute :: DeleteLocalGatewayRoute -> TestTree
requestDeleteLocalGatewayRoute =
  req
    "DeleteLocalGatewayRoute"
    "fixture/DeleteLocalGatewayRoute.yaml"

requestDeleteLocalGatewayRouteTable :: DeleteLocalGatewayRouteTable -> TestTree
requestDeleteLocalGatewayRouteTable =
  req
    "DeleteLocalGatewayRouteTable"
    "fixture/DeleteLocalGatewayRouteTable.yaml"

requestDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation :: DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation -> TestTree
requestDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
  req
    "DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation"
    "fixture/DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation.yaml"

requestDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociation -> TestTree
requestDeleteLocalGatewayRouteTableVpcAssociation =
  req
    "DeleteLocalGatewayRouteTableVpcAssociation"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociation.yaml"

requestDeleteManagedPrefixList :: DeleteManagedPrefixList -> TestTree
requestDeleteManagedPrefixList =
  req
    "DeleteManagedPrefixList"
    "fixture/DeleteManagedPrefixList.yaml"

requestDeleteNatGateway :: DeleteNatGateway -> TestTree
requestDeleteNatGateway =
  req
    "DeleteNatGateway"
    "fixture/DeleteNatGateway.yaml"

requestDeleteNetworkAcl :: DeleteNetworkAcl -> TestTree
requestDeleteNetworkAcl =
  req
    "DeleteNetworkAcl"
    "fixture/DeleteNetworkAcl.yaml"

requestDeleteNetworkAclEntry :: DeleteNetworkAclEntry -> TestTree
requestDeleteNetworkAclEntry =
  req
    "DeleteNetworkAclEntry"
    "fixture/DeleteNetworkAclEntry.yaml"

requestDeleteNetworkInsightsAccessScope :: DeleteNetworkInsightsAccessScope -> TestTree
requestDeleteNetworkInsightsAccessScope =
  req
    "DeleteNetworkInsightsAccessScope"
    "fixture/DeleteNetworkInsightsAccessScope.yaml"

requestDeleteNetworkInsightsAccessScopeAnalysis :: DeleteNetworkInsightsAccessScopeAnalysis -> TestTree
requestDeleteNetworkInsightsAccessScopeAnalysis =
  req
    "DeleteNetworkInsightsAccessScopeAnalysis"
    "fixture/DeleteNetworkInsightsAccessScopeAnalysis.yaml"

requestDeleteNetworkInsightsAnalysis :: DeleteNetworkInsightsAnalysis -> TestTree
requestDeleteNetworkInsightsAnalysis =
  req
    "DeleteNetworkInsightsAnalysis"
    "fixture/DeleteNetworkInsightsAnalysis.yaml"

requestDeleteNetworkInsightsPath :: DeleteNetworkInsightsPath -> TestTree
requestDeleteNetworkInsightsPath =
  req
    "DeleteNetworkInsightsPath"
    "fixture/DeleteNetworkInsightsPath.yaml"

requestDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
requestDeleteNetworkInterface =
  req
    "DeleteNetworkInterface"
    "fixture/DeleteNetworkInterface.yaml"

requestDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermission -> TestTree
requestDeleteNetworkInterfacePermission =
  req
    "DeleteNetworkInterfacePermission"
    "fixture/DeleteNetworkInterfacePermission.yaml"

requestDeletePlacementGroup :: DeletePlacementGroup -> TestTree
requestDeletePlacementGroup =
  req
    "DeletePlacementGroup"
    "fixture/DeletePlacementGroup.yaml"

requestDeletePublicIpv4Pool :: DeletePublicIpv4Pool -> TestTree
requestDeletePublicIpv4Pool =
  req
    "DeletePublicIpv4Pool"
    "fixture/DeletePublicIpv4Pool.yaml"

requestDeleteQueuedReservedInstances :: DeleteQueuedReservedInstances -> TestTree
requestDeleteQueuedReservedInstances =
  req
    "DeleteQueuedReservedInstances"
    "fixture/DeleteQueuedReservedInstances.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute =
  req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestDeleteRouteTable :: DeleteRouteTable -> TestTree
requestDeleteRouteTable =
  req
    "DeleteRouteTable"
    "fixture/DeleteRouteTable.yaml"

requestDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
requestDeleteSecurityGroup =
  req
    "DeleteSecurityGroup"
    "fixture/DeleteSecurityGroup.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
requestDeleteSpotDatafeedSubscription =
  req
    "DeleteSpotDatafeedSubscription"
    "fixture/DeleteSpotDatafeedSubscription.yaml"

requestDeleteSubnet :: DeleteSubnet -> TestTree
requestDeleteSubnet =
  req
    "DeleteSubnet"
    "fixture/DeleteSubnet.yaml"

requestDeleteSubnetCidrReservation :: DeleteSubnetCidrReservation -> TestTree
requestDeleteSubnetCidrReservation =
  req
    "DeleteSubnetCidrReservation"
    "fixture/DeleteSubnetCidrReservation.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilter -> TestTree
requestDeleteTrafficMirrorFilter =
  req
    "DeleteTrafficMirrorFilter"
    "fixture/DeleteTrafficMirrorFilter.yaml"

requestDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRule -> TestTree
requestDeleteTrafficMirrorFilterRule =
  req
    "DeleteTrafficMirrorFilterRule"
    "fixture/DeleteTrafficMirrorFilterRule.yaml"

requestDeleteTrafficMirrorSession :: DeleteTrafficMirrorSession -> TestTree
requestDeleteTrafficMirrorSession =
  req
    "DeleteTrafficMirrorSession"
    "fixture/DeleteTrafficMirrorSession.yaml"

requestDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTarget -> TestTree
requestDeleteTrafficMirrorTarget =
  req
    "DeleteTrafficMirrorTarget"
    "fixture/DeleteTrafficMirrorTarget.yaml"

requestDeleteTransitGateway :: DeleteTransitGateway -> TestTree
requestDeleteTransitGateway =
  req
    "DeleteTransitGateway"
    "fixture/DeleteTransitGateway.yaml"

requestDeleteTransitGatewayConnect :: DeleteTransitGatewayConnect -> TestTree
requestDeleteTransitGatewayConnect =
  req
    "DeleteTransitGatewayConnect"
    "fixture/DeleteTransitGatewayConnect.yaml"

requestDeleteTransitGatewayConnectPeer :: DeleteTransitGatewayConnectPeer -> TestTree
requestDeleteTransitGatewayConnectPeer =
  req
    "DeleteTransitGatewayConnectPeer"
    "fixture/DeleteTransitGatewayConnectPeer.yaml"

requestDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomain -> TestTree
requestDeleteTransitGatewayMulticastDomain =
  req
    "DeleteTransitGatewayMulticastDomain"
    "fixture/DeleteTransitGatewayMulticastDomain.yaml"

requestDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachment -> TestTree
requestDeleteTransitGatewayPeeringAttachment =
  req
    "DeleteTransitGatewayPeeringAttachment"
    "fixture/DeleteTransitGatewayPeeringAttachment.yaml"

requestDeleteTransitGatewayPolicyTable :: DeleteTransitGatewayPolicyTable -> TestTree
requestDeleteTransitGatewayPolicyTable =
  req
    "DeleteTransitGatewayPolicyTable"
    "fixture/DeleteTransitGatewayPolicyTable.yaml"

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

requestDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTable -> TestTree
requestDeleteTransitGatewayRouteTable =
  req
    "DeleteTransitGatewayRouteTable"
    "fixture/DeleteTransitGatewayRouteTable.yaml"

requestDeleteTransitGatewayRouteTableAnnouncement :: DeleteTransitGatewayRouteTableAnnouncement -> TestTree
requestDeleteTransitGatewayRouteTableAnnouncement =
  req
    "DeleteTransitGatewayRouteTableAnnouncement"
    "fixture/DeleteTransitGatewayRouteTableAnnouncement.yaml"

requestDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachment -> TestTree
requestDeleteTransitGatewayVpcAttachment =
  req
    "DeleteTransitGatewayVpcAttachment"
    "fixture/DeleteTransitGatewayVpcAttachment.yaml"

requestDeleteVerifiedAccessEndpoint :: DeleteVerifiedAccessEndpoint -> TestTree
requestDeleteVerifiedAccessEndpoint =
  req
    "DeleteVerifiedAccessEndpoint"
    "fixture/DeleteVerifiedAccessEndpoint.yaml"

requestDeleteVerifiedAccessGroup :: DeleteVerifiedAccessGroup -> TestTree
requestDeleteVerifiedAccessGroup =
  req
    "DeleteVerifiedAccessGroup"
    "fixture/DeleteVerifiedAccessGroup.yaml"

requestDeleteVerifiedAccessInstance :: DeleteVerifiedAccessInstance -> TestTree
requestDeleteVerifiedAccessInstance =
  req
    "DeleteVerifiedAccessInstance"
    "fixture/DeleteVerifiedAccessInstance.yaml"

requestDeleteVerifiedAccessTrustProvider :: DeleteVerifiedAccessTrustProvider -> TestTree
requestDeleteVerifiedAccessTrustProvider =
  req
    "DeleteVerifiedAccessTrustProvider"
    "fixture/DeleteVerifiedAccessTrustProvider.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestDeleteVpc :: DeleteVpc -> TestTree
requestDeleteVpc =
  req
    "DeleteVpc"
    "fixture/DeleteVpc.yaml"

requestDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotifications -> TestTree
requestDeleteVpcEndpointConnectionNotifications =
  req
    "DeleteVpcEndpointConnectionNotifications"
    "fixture/DeleteVpcEndpointConnectionNotifications.yaml"

requestDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurations -> TestTree
requestDeleteVpcEndpointServiceConfigurations =
  req
    "DeleteVpcEndpointServiceConfigurations"
    "fixture/DeleteVpcEndpointServiceConfigurations.yaml"

requestDeleteVpcEndpoints :: DeleteVpcEndpoints -> TestTree
requestDeleteVpcEndpoints =
  req
    "DeleteVpcEndpoints"
    "fixture/DeleteVpcEndpoints.yaml"

requestDeleteVpcPeeringConnection :: DeleteVpcPeeringConnection -> TestTree
requestDeleteVpcPeeringConnection =
  req
    "DeleteVpcPeeringConnection"
    "fixture/DeleteVpcPeeringConnection.yaml"

requestDeleteVpnConnection :: DeleteVpnConnection -> TestTree
requestDeleteVpnConnection =
  req
    "DeleteVpnConnection"
    "fixture/DeleteVpnConnection.yaml"

requestDeleteVpnConnectionRoute :: DeleteVpnConnectionRoute -> TestTree
requestDeleteVpnConnectionRoute =
  req
    "DeleteVpnConnectionRoute"
    "fixture/DeleteVpnConnectionRoute.yaml"

requestDeleteVpnGateway :: DeleteVpnGateway -> TestTree
requestDeleteVpnGateway =
  req
    "DeleteVpnGateway"
    "fixture/DeleteVpnGateway.yaml"

requestDeprovisionByoipCidr :: DeprovisionByoipCidr -> TestTree
requestDeprovisionByoipCidr =
  req
    "DeprovisionByoipCidr"
    "fixture/DeprovisionByoipCidr.yaml"

requestDeprovisionIpamPoolCidr :: DeprovisionIpamPoolCidr -> TestTree
requestDeprovisionIpamPoolCidr =
  req
    "DeprovisionIpamPoolCidr"
    "fixture/DeprovisionIpamPoolCidr.yaml"

requestDeprovisionPublicIpv4PoolCidr :: DeprovisionPublicIpv4PoolCidr -> TestTree
requestDeprovisionPublicIpv4PoolCidr =
  req
    "DeprovisionPublicIpv4PoolCidr"
    "fixture/DeprovisionPublicIpv4PoolCidr.yaml"

requestDeregisterImage :: DeregisterImage -> TestTree
requestDeregisterImage =
  req
    "DeregisterImage"
    "fixture/DeregisterImage.yaml"

requestDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributes -> TestTree
requestDeregisterInstanceEventNotificationAttributes =
  req
    "DeregisterInstanceEventNotificationAttributes"
    "fixture/DeregisterInstanceEventNotificationAttributes.yaml"

requestDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembers -> TestTree
requestDeregisterTransitGatewayMulticastGroupMembers =
  req
    "DeregisterTransitGatewayMulticastGroupMembers"
    "fixture/DeregisterTransitGatewayMulticastGroupMembers.yaml"

requestDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSources -> TestTree
requestDeregisterTransitGatewayMulticastGroupSources =
  req
    "DeregisterTransitGatewayMulticastGroupSources"
    "fixture/DeregisterTransitGatewayMulticastGroupSources.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeAddressTransfers :: DescribeAddressTransfers -> TestTree
requestDescribeAddressTransfers =
  req
    "DescribeAddressTransfers"
    "fixture/DescribeAddressTransfers.yaml"

requestDescribeAddresses :: DescribeAddresses -> TestTree
requestDescribeAddresses =
  req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

requestDescribeAddressesAttribute :: DescribeAddressesAttribute -> TestTree
requestDescribeAddressesAttribute =
  req
    "DescribeAddressesAttribute"
    "fixture/DescribeAddressesAttribute.yaml"

requestDescribeAggregateIdFormat :: DescribeAggregateIdFormat -> TestTree
requestDescribeAggregateIdFormat =
  req
    "DescribeAggregateIdFormat"
    "fixture/DescribeAggregateIdFormat.yaml"

requestDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
requestDescribeAvailabilityZones =
  req
    "DescribeAvailabilityZones"
    "fixture/DescribeAvailabilityZones.yaml"

requestDescribeAwsNetworkPerformanceMetricSubscriptions :: DescribeAwsNetworkPerformanceMetricSubscriptions -> TestTree
requestDescribeAwsNetworkPerformanceMetricSubscriptions =
  req
    "DescribeAwsNetworkPerformanceMetricSubscriptions"
    "fixture/DescribeAwsNetworkPerformanceMetricSubscriptions.yaml"

requestDescribeBundleTasks :: DescribeBundleTasks -> TestTree
requestDescribeBundleTasks =
  req
    "DescribeBundleTasks"
    "fixture/DescribeBundleTasks.yaml"

requestDescribeByoipCidrs :: DescribeByoipCidrs -> TestTree
requestDescribeByoipCidrs =
  req
    "DescribeByoipCidrs"
    "fixture/DescribeByoipCidrs.yaml"

requestDescribeCapacityReservationFleets :: DescribeCapacityReservationFleets -> TestTree
requestDescribeCapacityReservationFleets =
  req
    "DescribeCapacityReservationFleets"
    "fixture/DescribeCapacityReservationFleets.yaml"

requestDescribeCapacityReservations :: DescribeCapacityReservations -> TestTree
requestDescribeCapacityReservations =
  req
    "DescribeCapacityReservations"
    "fixture/DescribeCapacityReservations.yaml"

requestDescribeCarrierGateways :: DescribeCarrierGateways -> TestTree
requestDescribeCarrierGateways =
  req
    "DescribeCarrierGateways"
    "fixture/DescribeCarrierGateways.yaml"

requestDescribeClassicLinkInstances :: DescribeClassicLinkInstances -> TestTree
requestDescribeClassicLinkInstances =
  req
    "DescribeClassicLinkInstances"
    "fixture/DescribeClassicLinkInstances.yaml"

requestDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRules -> TestTree
requestDescribeClientVpnAuthorizationRules =
  req
    "DescribeClientVpnAuthorizationRules"
    "fixture/DescribeClientVpnAuthorizationRules.yaml"

requestDescribeClientVpnConnections :: DescribeClientVpnConnections -> TestTree
requestDescribeClientVpnConnections =
  req
    "DescribeClientVpnConnections"
    "fixture/DescribeClientVpnConnections.yaml"

requestDescribeClientVpnEndpoints :: DescribeClientVpnEndpoints -> TestTree
requestDescribeClientVpnEndpoints =
  req
    "DescribeClientVpnEndpoints"
    "fixture/DescribeClientVpnEndpoints.yaml"

requestDescribeClientVpnRoutes :: DescribeClientVpnRoutes -> TestTree
requestDescribeClientVpnRoutes =
  req
    "DescribeClientVpnRoutes"
    "fixture/DescribeClientVpnRoutes.yaml"

requestDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworks -> TestTree
requestDescribeClientVpnTargetNetworks =
  req
    "DescribeClientVpnTargetNetworks"
    "fixture/DescribeClientVpnTargetNetworks.yaml"

requestDescribeCoipPools :: DescribeCoipPools -> TestTree
requestDescribeCoipPools =
  req
    "DescribeCoipPools"
    "fixture/DescribeCoipPools.yaml"

requestDescribeConversionTasks :: DescribeConversionTasks -> TestTree
requestDescribeConversionTasks =
  req
    "DescribeConversionTasks"
    "fixture/DescribeConversionTasks.yaml"

requestDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
requestDescribeCustomerGateways =
  req
    "DescribeCustomerGateways"
    "fixture/DescribeCustomerGateways.yaml"

requestDescribeDhcpOptions :: DescribeDhcpOptions -> TestTree
requestDescribeDhcpOptions =
  req
    "DescribeDhcpOptions"
    "fixture/DescribeDhcpOptions.yaml"

requestDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGateways -> TestTree
requestDescribeEgressOnlyInternetGateways =
  req
    "DescribeEgressOnlyInternetGateways"
    "fixture/DescribeEgressOnlyInternetGateways.yaml"

requestDescribeElasticGpus :: DescribeElasticGpus -> TestTree
requestDescribeElasticGpus =
  req
    "DescribeElasticGpus"
    "fixture/DescribeElasticGpus.yaml"

requestDescribeExportImageTasks :: DescribeExportImageTasks -> TestTree
requestDescribeExportImageTasks =
  req
    "DescribeExportImageTasks"
    "fixture/DescribeExportImageTasks.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestDescribeFastLaunchImages :: DescribeFastLaunchImages -> TestTree
requestDescribeFastLaunchImages =
  req
    "DescribeFastLaunchImages"
    "fixture/DescribeFastLaunchImages.yaml"

requestDescribeFastSnapshotRestores :: DescribeFastSnapshotRestores -> TestTree
requestDescribeFastSnapshotRestores =
  req
    "DescribeFastSnapshotRestores"
    "fixture/DescribeFastSnapshotRestores.yaml"

requestDescribeFleetHistory :: DescribeFleetHistory -> TestTree
requestDescribeFleetHistory =
  req
    "DescribeFleetHistory"
    "fixture/DescribeFleetHistory.yaml"

requestDescribeFleetInstances :: DescribeFleetInstances -> TestTree
requestDescribeFleetInstances =
  req
    "DescribeFleetInstances"
    "fixture/DescribeFleetInstances.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets =
  req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestDescribeFlowLogs :: DescribeFlowLogs -> TestTree
requestDescribeFlowLogs =
  req
    "DescribeFlowLogs"
    "fixture/DescribeFlowLogs.yaml"

requestDescribeFpgaImageAttribute :: DescribeFpgaImageAttribute -> TestTree
requestDescribeFpgaImageAttribute =
  req
    "DescribeFpgaImageAttribute"
    "fixture/DescribeFpgaImageAttribute.yaml"

requestDescribeFpgaImages :: DescribeFpgaImages -> TestTree
requestDescribeFpgaImages =
  req
    "DescribeFpgaImages"
    "fixture/DescribeFpgaImages.yaml"

requestDescribeHostReservationOfferings :: DescribeHostReservationOfferings -> TestTree
requestDescribeHostReservationOfferings =
  req
    "DescribeHostReservationOfferings"
    "fixture/DescribeHostReservationOfferings.yaml"

requestDescribeHostReservations :: DescribeHostReservations -> TestTree
requestDescribeHostReservations =
  req
    "DescribeHostReservations"
    "fixture/DescribeHostReservations.yaml"

requestDescribeHosts :: DescribeHosts -> TestTree
requestDescribeHosts =
  req
    "DescribeHosts"
    "fixture/DescribeHosts.yaml"

requestDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociations -> TestTree
requestDescribeIamInstanceProfileAssociations =
  req
    "DescribeIamInstanceProfileAssociations"
    "fixture/DescribeIamInstanceProfileAssociations.yaml"

requestDescribeIdFormat :: DescribeIdFormat -> TestTree
requestDescribeIdFormat =
  req
    "DescribeIdFormat"
    "fixture/DescribeIdFormat.yaml"

requestDescribeIdentityIdFormat :: DescribeIdentityIdFormat -> TestTree
requestDescribeIdentityIdFormat =
  req
    "DescribeIdentityIdFormat"
    "fixture/DescribeIdentityIdFormat.yaml"

requestDescribeImageAttribute :: DescribeImageAttribute -> TestTree
requestDescribeImageAttribute =
  req
    "DescribeImageAttribute"
    "fixture/DescribeImageAttribute.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
requestDescribeImportImageTasks =
  req
    "DescribeImportImageTasks"
    "fixture/DescribeImportImageTasks.yaml"

requestDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
requestDescribeImportSnapshotTasks =
  req
    "DescribeImportSnapshotTasks"
    "fixture/DescribeImportSnapshotTasks.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute =
  req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecifications -> TestTree
requestDescribeInstanceCreditSpecifications =
  req
    "DescribeInstanceCreditSpecifications"
    "fixture/DescribeInstanceCreditSpecifications.yaml"

requestDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributes -> TestTree
requestDescribeInstanceEventNotificationAttributes =
  req
    "DescribeInstanceEventNotificationAttributes"
    "fixture/DescribeInstanceEventNotificationAttributes.yaml"

requestDescribeInstanceEventWindows :: DescribeInstanceEventWindows -> TestTree
requestDescribeInstanceEventWindows =
  req
    "DescribeInstanceEventWindows"
    "fixture/DescribeInstanceEventWindows.yaml"

requestDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
requestDescribeInstanceStatus =
  req
    "DescribeInstanceStatus"
    "fixture/DescribeInstanceStatus.yaml"

requestDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferings -> TestTree
requestDescribeInstanceTypeOfferings =
  req
    "DescribeInstanceTypeOfferings"
    "fixture/DescribeInstanceTypeOfferings.yaml"

requestDescribeInstanceTypes :: DescribeInstanceTypes -> TestTree
requestDescribeInstanceTypes =
  req
    "DescribeInstanceTypes"
    "fixture/DescribeInstanceTypes.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDescribeInternetGateways :: DescribeInternetGateways -> TestTree
requestDescribeInternetGateways =
  req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways.yaml"

requestDescribeIpamPools :: DescribeIpamPools -> TestTree
requestDescribeIpamPools =
  req
    "DescribeIpamPools"
    "fixture/DescribeIpamPools.yaml"

requestDescribeIpamScopes :: DescribeIpamScopes -> TestTree
requestDescribeIpamScopes =
  req
    "DescribeIpamScopes"
    "fixture/DescribeIpamScopes.yaml"

requestDescribeIpams :: DescribeIpams -> TestTree
requestDescribeIpams =
  req
    "DescribeIpams"
    "fixture/DescribeIpams.yaml"

requestDescribeIpv6Pools :: DescribeIpv6Pools -> TestTree
requestDescribeIpv6Pools =
  req
    "DescribeIpv6Pools"
    "fixture/DescribeIpv6Pools.yaml"

requestDescribeKeyPairs :: DescribeKeyPairs -> TestTree
requestDescribeKeyPairs =
  req
    "DescribeKeyPairs"
    "fixture/DescribeKeyPairs.yaml"

requestDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersions -> TestTree
requestDescribeLaunchTemplateVersions =
  req
    "DescribeLaunchTemplateVersions"
    "fixture/DescribeLaunchTemplateVersions.yaml"

requestDescribeLaunchTemplates :: DescribeLaunchTemplates -> TestTree
requestDescribeLaunchTemplates =
  req
    "DescribeLaunchTemplates"
    "fixture/DescribeLaunchTemplates.yaml"

requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  req
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations.yaml"

requestDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVpcAssociations =
  req
    "DescribeLocalGatewayRouteTableVpcAssociations"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociations.yaml"

requestDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTables -> TestTree
requestDescribeLocalGatewayRouteTables =
  req
    "DescribeLocalGatewayRouteTables"
    "fixture/DescribeLocalGatewayRouteTables.yaml"

requestDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroups -> TestTree
requestDescribeLocalGatewayVirtualInterfaceGroups =
  req
    "DescribeLocalGatewayVirtualInterfaceGroups"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroups.yaml"

requestDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfaces -> TestTree
requestDescribeLocalGatewayVirtualInterfaces =
  req
    "DescribeLocalGatewayVirtualInterfaces"
    "fixture/DescribeLocalGatewayVirtualInterfaces.yaml"

requestDescribeLocalGateways :: DescribeLocalGateways -> TestTree
requestDescribeLocalGateways =
  req
    "DescribeLocalGateways"
    "fixture/DescribeLocalGateways.yaml"

requestDescribeManagedPrefixLists :: DescribeManagedPrefixLists -> TestTree
requestDescribeManagedPrefixLists =
  req
    "DescribeManagedPrefixLists"
    "fixture/DescribeManagedPrefixLists.yaml"

requestDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
requestDescribeMovingAddresses =
  req
    "DescribeMovingAddresses"
    "fixture/DescribeMovingAddresses.yaml"

requestDescribeNatGateways :: DescribeNatGateways -> TestTree
requestDescribeNatGateways =
  req
    "DescribeNatGateways"
    "fixture/DescribeNatGateways.yaml"

requestDescribeNetworkAcls :: DescribeNetworkAcls -> TestTree
requestDescribeNetworkAcls =
  req
    "DescribeNetworkAcls"
    "fixture/DescribeNetworkAcls.yaml"

requestDescribeNetworkInsightsAccessScopeAnalyses :: DescribeNetworkInsightsAccessScopeAnalyses -> TestTree
requestDescribeNetworkInsightsAccessScopeAnalyses =
  req
    "DescribeNetworkInsightsAccessScopeAnalyses"
    "fixture/DescribeNetworkInsightsAccessScopeAnalyses.yaml"

requestDescribeNetworkInsightsAccessScopes :: DescribeNetworkInsightsAccessScopes -> TestTree
requestDescribeNetworkInsightsAccessScopes =
  req
    "DescribeNetworkInsightsAccessScopes"
    "fixture/DescribeNetworkInsightsAccessScopes.yaml"

requestDescribeNetworkInsightsAnalyses :: DescribeNetworkInsightsAnalyses -> TestTree
requestDescribeNetworkInsightsAnalyses =
  req
    "DescribeNetworkInsightsAnalyses"
    "fixture/DescribeNetworkInsightsAnalyses.yaml"

requestDescribeNetworkInsightsPaths :: DescribeNetworkInsightsPaths -> TestTree
requestDescribeNetworkInsightsPaths =
  req
    "DescribeNetworkInsightsPaths"
    "fixture/DescribeNetworkInsightsPaths.yaml"

requestDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
requestDescribeNetworkInterfaceAttribute =
  req
    "DescribeNetworkInterfaceAttribute"
    "fixture/DescribeNetworkInterfaceAttribute.yaml"

requestDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissions -> TestTree
requestDescribeNetworkInterfacePermissions =
  req
    "DescribeNetworkInterfacePermissions"
    "fixture/DescribeNetworkInterfacePermissions.yaml"

requestDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
requestDescribeNetworkInterfaces =
  req
    "DescribeNetworkInterfaces"
    "fixture/DescribeNetworkInterfaces.yaml"

requestDescribePlacementGroups :: DescribePlacementGroups -> TestTree
requestDescribePlacementGroups =
  req
    "DescribePlacementGroups"
    "fixture/DescribePlacementGroups.yaml"

requestDescribePrefixLists :: DescribePrefixLists -> TestTree
requestDescribePrefixLists =
  req
    "DescribePrefixLists"
    "fixture/DescribePrefixLists.yaml"

requestDescribePrincipalIdFormat :: DescribePrincipalIdFormat -> TestTree
requestDescribePrincipalIdFormat =
  req
    "DescribePrincipalIdFormat"
    "fixture/DescribePrincipalIdFormat.yaml"

requestDescribePublicIpv4Pools :: DescribePublicIpv4Pools -> TestTree
requestDescribePublicIpv4Pools =
  req
    "DescribePublicIpv4Pools"
    "fixture/DescribePublicIpv4Pools.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions =
  req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestDescribeReplaceRootVolumeTasks :: DescribeReplaceRootVolumeTasks -> TestTree
requestDescribeReplaceRootVolumeTasks =
  req
    "DescribeReplaceRootVolumeTasks"
    "fixture/DescribeReplaceRootVolumeTasks.yaml"

requestDescribeReservedInstances :: DescribeReservedInstances -> TestTree
requestDescribeReservedInstances =
  req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances.yaml"

requestDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
requestDescribeReservedInstancesListings =
  req
    "DescribeReservedInstancesListings"
    "fixture/DescribeReservedInstancesListings.yaml"

requestDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
requestDescribeReservedInstancesModifications =
  req
    "DescribeReservedInstancesModifications"
    "fixture/DescribeReservedInstancesModifications.yaml"

requestDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
requestDescribeReservedInstancesOfferings =
  req
    "DescribeReservedInstancesOfferings"
    "fixture/DescribeReservedInstancesOfferings.yaml"

requestDescribeRouteTables :: DescribeRouteTables -> TestTree
requestDescribeRouteTables =
  req
    "DescribeRouteTables"
    "fixture/DescribeRouteTables.yaml"

requestDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailability -> TestTree
requestDescribeScheduledInstanceAvailability =
  req
    "DescribeScheduledInstanceAvailability"
    "fixture/DescribeScheduledInstanceAvailability.yaml"

requestDescribeScheduledInstances :: DescribeScheduledInstances -> TestTree
requestDescribeScheduledInstances =
  req
    "DescribeScheduledInstances"
    "fixture/DescribeScheduledInstances.yaml"

requestDescribeSecurityGroupReferences :: DescribeSecurityGroupReferences -> TestTree
requestDescribeSecurityGroupReferences =
  req
    "DescribeSecurityGroupReferences"
    "fixture/DescribeSecurityGroupReferences.yaml"

requestDescribeSecurityGroupRules :: DescribeSecurityGroupRules -> TestTree
requestDescribeSecurityGroupRules =
  req
    "DescribeSecurityGroupRules"
    "fixture/DescribeSecurityGroupRules.yaml"

requestDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
requestDescribeSecurityGroups =
  req
    "DescribeSecurityGroups"
    "fixture/DescribeSecurityGroups.yaml"

requestDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
requestDescribeSnapshotAttribute =
  req
    "DescribeSnapshotAttribute"
    "fixture/DescribeSnapshotAttribute.yaml"

requestDescribeSnapshotTierStatus :: DescribeSnapshotTierStatus -> TestTree
requestDescribeSnapshotTierStatus =
  req
    "DescribeSnapshotTierStatus"
    "fixture/DescribeSnapshotTierStatus.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
requestDescribeSpotDatafeedSubscription =
  req
    "DescribeSpotDatafeedSubscription"
    "fixture/DescribeSpotDatafeedSubscription.yaml"

requestDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
requestDescribeSpotFleetInstances =
  req
    "DescribeSpotFleetInstances"
    "fixture/DescribeSpotFleetInstances.yaml"

requestDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
requestDescribeSpotFleetRequestHistory =
  req
    "DescribeSpotFleetRequestHistory"
    "fixture/DescribeSpotFleetRequestHistory.yaml"

requestDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
requestDescribeSpotFleetRequests =
  req
    "DescribeSpotFleetRequests"
    "fixture/DescribeSpotFleetRequests.yaml"

requestDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
requestDescribeSpotInstanceRequests =
  req
    "DescribeSpotInstanceRequests"
    "fixture/DescribeSpotInstanceRequests.yaml"

requestDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
requestDescribeSpotPriceHistory =
  req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory.yaml"

requestDescribeStaleSecurityGroups :: DescribeStaleSecurityGroups -> TestTree
requestDescribeStaleSecurityGroups =
  req
    "DescribeStaleSecurityGroups"
    "fixture/DescribeStaleSecurityGroups.yaml"

requestDescribeStoreImageTasks :: DescribeStoreImageTasks -> TestTree
requestDescribeStoreImageTasks =
  req
    "DescribeStoreImageTasks"
    "fixture/DescribeStoreImageTasks.yaml"

requestDescribeSubnets :: DescribeSubnets -> TestTree
requestDescribeSubnets =
  req
    "DescribeSubnets"
    "fixture/DescribeSubnets.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFilters -> TestTree
requestDescribeTrafficMirrorFilters =
  req
    "DescribeTrafficMirrorFilters"
    "fixture/DescribeTrafficMirrorFilters.yaml"

requestDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessions -> TestTree
requestDescribeTrafficMirrorSessions =
  req
    "DescribeTrafficMirrorSessions"
    "fixture/DescribeTrafficMirrorSessions.yaml"

requestDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargets -> TestTree
requestDescribeTrafficMirrorTargets =
  req
    "DescribeTrafficMirrorTargets"
    "fixture/DescribeTrafficMirrorTargets.yaml"

requestDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachments -> TestTree
requestDescribeTransitGatewayAttachments =
  req
    "DescribeTransitGatewayAttachments"
    "fixture/DescribeTransitGatewayAttachments.yaml"

requestDescribeTransitGatewayConnectPeers :: DescribeTransitGatewayConnectPeers -> TestTree
requestDescribeTransitGatewayConnectPeers =
  req
    "DescribeTransitGatewayConnectPeers"
    "fixture/DescribeTransitGatewayConnectPeers.yaml"

requestDescribeTransitGatewayConnects :: DescribeTransitGatewayConnects -> TestTree
requestDescribeTransitGatewayConnects =
  req
    "DescribeTransitGatewayConnects"
    "fixture/DescribeTransitGatewayConnects.yaml"

requestDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomains -> TestTree
requestDescribeTransitGatewayMulticastDomains =
  req
    "DescribeTransitGatewayMulticastDomains"
    "fixture/DescribeTransitGatewayMulticastDomains.yaml"

requestDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachments -> TestTree
requestDescribeTransitGatewayPeeringAttachments =
  req
    "DescribeTransitGatewayPeeringAttachments"
    "fixture/DescribeTransitGatewayPeeringAttachments.yaml"

requestDescribeTransitGatewayPolicyTables :: DescribeTransitGatewayPolicyTables -> TestTree
requestDescribeTransitGatewayPolicyTables =
  req
    "DescribeTransitGatewayPolicyTables"
    "fixture/DescribeTransitGatewayPolicyTables.yaml"

requestDescribeTransitGatewayRouteTableAnnouncements :: DescribeTransitGatewayRouteTableAnnouncements -> TestTree
requestDescribeTransitGatewayRouteTableAnnouncements =
  req
    "DescribeTransitGatewayRouteTableAnnouncements"
    "fixture/DescribeTransitGatewayRouteTableAnnouncements.yaml"

requestDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTables -> TestTree
requestDescribeTransitGatewayRouteTables =
  req
    "DescribeTransitGatewayRouteTables"
    "fixture/DescribeTransitGatewayRouteTables.yaml"

requestDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachments -> TestTree
requestDescribeTransitGatewayVpcAttachments =
  req
    "DescribeTransitGatewayVpcAttachments"
    "fixture/DescribeTransitGatewayVpcAttachments.yaml"

requestDescribeTransitGateways :: DescribeTransitGateways -> TestTree
requestDescribeTransitGateways =
  req
    "DescribeTransitGateways"
    "fixture/DescribeTransitGateways.yaml"

requestDescribeTrunkInterfaceAssociations :: DescribeTrunkInterfaceAssociations -> TestTree
requestDescribeTrunkInterfaceAssociations =
  req
    "DescribeTrunkInterfaceAssociations"
    "fixture/DescribeTrunkInterfaceAssociations.yaml"

requestDescribeVerifiedAccessEndpoints :: DescribeVerifiedAccessEndpoints -> TestTree
requestDescribeVerifiedAccessEndpoints =
  req
    "DescribeVerifiedAccessEndpoints"
    "fixture/DescribeVerifiedAccessEndpoints.yaml"

requestDescribeVerifiedAccessGroups :: DescribeVerifiedAccessGroups -> TestTree
requestDescribeVerifiedAccessGroups =
  req
    "DescribeVerifiedAccessGroups"
    "fixture/DescribeVerifiedAccessGroups.yaml"

requestDescribeVerifiedAccessInstanceLoggingConfigurations :: DescribeVerifiedAccessInstanceLoggingConfigurations -> TestTree
requestDescribeVerifiedAccessInstanceLoggingConfigurations =
  req
    "DescribeVerifiedAccessInstanceLoggingConfigurations"
    "fixture/DescribeVerifiedAccessInstanceLoggingConfigurations.yaml"

requestDescribeVerifiedAccessInstances :: DescribeVerifiedAccessInstances -> TestTree
requestDescribeVerifiedAccessInstances =
  req
    "DescribeVerifiedAccessInstances"
    "fixture/DescribeVerifiedAccessInstances.yaml"

requestDescribeVerifiedAccessTrustProviders :: DescribeVerifiedAccessTrustProviders -> TestTree
requestDescribeVerifiedAccessTrustProviders =
  req
    "DescribeVerifiedAccessTrustProviders"
    "fixture/DescribeVerifiedAccessTrustProviders.yaml"

requestDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
requestDescribeVolumeAttribute =
  req
    "DescribeVolumeAttribute"
    "fixture/DescribeVolumeAttribute.yaml"

requestDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
requestDescribeVolumeStatus =
  req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes =
  req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestDescribeVolumesModifications :: DescribeVolumesModifications -> TestTree
requestDescribeVolumesModifications =
  req
    "DescribeVolumesModifications"
    "fixture/DescribeVolumesModifications.yaml"

requestDescribeVpcAttribute :: DescribeVpcAttribute -> TestTree
requestDescribeVpcAttribute =
  req
    "DescribeVpcAttribute"
    "fixture/DescribeVpcAttribute.yaml"

requestDescribeVpcClassicLink :: DescribeVpcClassicLink -> TestTree
requestDescribeVpcClassicLink =
  req
    "DescribeVpcClassicLink"
    "fixture/DescribeVpcClassicLink.yaml"

requestDescribeVpcClassicLinkDnsSupport :: DescribeVpcClassicLinkDnsSupport -> TestTree
requestDescribeVpcClassicLinkDnsSupport =
  req
    "DescribeVpcClassicLinkDnsSupport"
    "fixture/DescribeVpcClassicLinkDnsSupport.yaml"

requestDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotifications -> TestTree
requestDescribeVpcEndpointConnectionNotifications =
  req
    "DescribeVpcEndpointConnectionNotifications"
    "fixture/DescribeVpcEndpointConnectionNotifications.yaml"

requestDescribeVpcEndpointConnections :: DescribeVpcEndpointConnections -> TestTree
requestDescribeVpcEndpointConnections =
  req
    "DescribeVpcEndpointConnections"
    "fixture/DescribeVpcEndpointConnections.yaml"

requestDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurations -> TestTree
requestDescribeVpcEndpointServiceConfigurations =
  req
    "DescribeVpcEndpointServiceConfigurations"
    "fixture/DescribeVpcEndpointServiceConfigurations.yaml"

requestDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissions -> TestTree
requestDescribeVpcEndpointServicePermissions =
  req
    "DescribeVpcEndpointServicePermissions"
    "fixture/DescribeVpcEndpointServicePermissions.yaml"

requestDescribeVpcEndpointServices :: DescribeVpcEndpointServices -> TestTree
requestDescribeVpcEndpointServices =
  req
    "DescribeVpcEndpointServices"
    "fixture/DescribeVpcEndpointServices.yaml"

requestDescribeVpcEndpoints :: DescribeVpcEndpoints -> TestTree
requestDescribeVpcEndpoints =
  req
    "DescribeVpcEndpoints"
    "fixture/DescribeVpcEndpoints.yaml"

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections =
  req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestDescribeVpcs :: DescribeVpcs -> TestTree
requestDescribeVpcs =
  req
    "DescribeVpcs"
    "fixture/DescribeVpcs.yaml"

requestDescribeVpnConnections :: DescribeVpnConnections -> TestTree
requestDescribeVpnConnections =
  req
    "DescribeVpnConnections"
    "fixture/DescribeVpnConnections.yaml"

requestDescribeVpnGateways :: DescribeVpnGateways -> TestTree
requestDescribeVpnGateways =
  req
    "DescribeVpnGateways"
    "fixture/DescribeVpnGateways.yaml"

requestDetachClassicLinkVpc :: DetachClassicLinkVpc -> TestTree
requestDetachClassicLinkVpc =
  req
    "DetachClassicLinkVpc"
    "fixture/DetachClassicLinkVpc.yaml"

requestDetachInternetGateway :: DetachInternetGateway -> TestTree
requestDetachInternetGateway =
  req
    "DetachInternetGateway"
    "fixture/DetachInternetGateway.yaml"

requestDetachNetworkInterface :: DetachNetworkInterface -> TestTree
requestDetachNetworkInterface =
  req
    "DetachNetworkInterface"
    "fixture/DetachNetworkInterface.yaml"

requestDetachVerifiedAccessTrustProvider :: DetachVerifiedAccessTrustProvider -> TestTree
requestDetachVerifiedAccessTrustProvider =
  req
    "DetachVerifiedAccessTrustProvider"
    "fixture/DetachVerifiedAccessTrustProvider.yaml"

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume =
  req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestDetachVpnGateway :: DetachVpnGateway -> TestTree
requestDetachVpnGateway =
  req
    "DetachVpnGateway"
    "fixture/DetachVpnGateway.yaml"

requestDisableAddressTransfer :: DisableAddressTransfer -> TestTree
requestDisableAddressTransfer =
  req
    "DisableAddressTransfer"
    "fixture/DisableAddressTransfer.yaml"

requestDisableAwsNetworkPerformanceMetricSubscription :: DisableAwsNetworkPerformanceMetricSubscription -> TestTree
requestDisableAwsNetworkPerformanceMetricSubscription =
  req
    "DisableAwsNetworkPerformanceMetricSubscription"
    "fixture/DisableAwsNetworkPerformanceMetricSubscription.yaml"

requestDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefault -> TestTree
requestDisableEbsEncryptionByDefault =
  req
    "DisableEbsEncryptionByDefault"
    "fixture/DisableEbsEncryptionByDefault.yaml"

requestDisableFastLaunch :: DisableFastLaunch -> TestTree
requestDisableFastLaunch =
  req
    "DisableFastLaunch"
    "fixture/DisableFastLaunch.yaml"

requestDisableFastSnapshotRestores :: DisableFastSnapshotRestores -> TestTree
requestDisableFastSnapshotRestores =
  req
    "DisableFastSnapshotRestores"
    "fixture/DisableFastSnapshotRestores.yaml"

requestDisableImageDeprecation :: DisableImageDeprecation -> TestTree
requestDisableImageDeprecation =
  req
    "DisableImageDeprecation"
    "fixture/DisableImageDeprecation.yaml"

requestDisableIpamOrganizationAdminAccount :: DisableIpamOrganizationAdminAccount -> TestTree
requestDisableIpamOrganizationAdminAccount =
  req
    "DisableIpamOrganizationAdminAccount"
    "fixture/DisableIpamOrganizationAdminAccount.yaml"

requestDisableSerialConsoleAccess :: DisableSerialConsoleAccess -> TestTree
requestDisableSerialConsoleAccess =
  req
    "DisableSerialConsoleAccess"
    "fixture/DisableSerialConsoleAccess.yaml"

requestDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagation -> TestTree
requestDisableTransitGatewayRouteTablePropagation =
  req
    "DisableTransitGatewayRouteTablePropagation"
    "fixture/DisableTransitGatewayRouteTablePropagation.yaml"

requestDisableVgwRoutePropagation :: DisableVgwRoutePropagation -> TestTree
requestDisableVgwRoutePropagation =
  req
    "DisableVgwRoutePropagation"
    "fixture/DisableVgwRoutePropagation.yaml"

requestDisableVpcClassicLink :: DisableVpcClassicLink -> TestTree
requestDisableVpcClassicLink =
  req
    "DisableVpcClassicLink"
    "fixture/DisableVpcClassicLink.yaml"

requestDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupport -> TestTree
requestDisableVpcClassicLinkDnsSupport =
  req
    "DisableVpcClassicLinkDnsSupport"
    "fixture/DisableVpcClassicLinkDnsSupport.yaml"

requestDisassociateAddress :: DisassociateAddress -> TestTree
requestDisassociateAddress =
  req
    "DisassociateAddress"
    "fixture/DisassociateAddress.yaml"

requestDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetwork -> TestTree
requestDisassociateClientVpnTargetNetwork =
  req
    "DisassociateClientVpnTargetNetwork"
    "fixture/DisassociateClientVpnTargetNetwork.yaml"

requestDisassociateEnclaveCertificateIamRole :: DisassociateEnclaveCertificateIamRole -> TestTree
requestDisassociateEnclaveCertificateIamRole =
  req
    "DisassociateEnclaveCertificateIamRole"
    "fixture/DisassociateEnclaveCertificateIamRole.yaml"

requestDisassociateIamInstanceProfile :: DisassociateIamInstanceProfile -> TestTree
requestDisassociateIamInstanceProfile =
  req
    "DisassociateIamInstanceProfile"
    "fixture/DisassociateIamInstanceProfile.yaml"

requestDisassociateInstanceEventWindow :: DisassociateInstanceEventWindow -> TestTree
requestDisassociateInstanceEventWindow =
  req
    "DisassociateInstanceEventWindow"
    "fixture/DisassociateInstanceEventWindow.yaml"

requestDisassociateRouteTable :: DisassociateRouteTable -> TestTree
requestDisassociateRouteTable =
  req
    "DisassociateRouteTable"
    "fixture/DisassociateRouteTable.yaml"

requestDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlock -> TestTree
requestDisassociateSubnetCidrBlock =
  req
    "DisassociateSubnetCidrBlock"
    "fixture/DisassociateSubnetCidrBlock.yaml"

requestDisassociateTransitGatewayMulticastDomain :: DisassociateTransitGatewayMulticastDomain -> TestTree
requestDisassociateTransitGatewayMulticastDomain =
  req
    "DisassociateTransitGatewayMulticastDomain"
    "fixture/DisassociateTransitGatewayMulticastDomain.yaml"

requestDisassociateTransitGatewayPolicyTable :: DisassociateTransitGatewayPolicyTable -> TestTree
requestDisassociateTransitGatewayPolicyTable =
  req
    "DisassociateTransitGatewayPolicyTable"
    "fixture/DisassociateTransitGatewayPolicyTable.yaml"

requestDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTable -> TestTree
requestDisassociateTransitGatewayRouteTable =
  req
    "DisassociateTransitGatewayRouteTable"
    "fixture/DisassociateTransitGatewayRouteTable.yaml"

requestDisassociateTrunkInterface :: DisassociateTrunkInterface -> TestTree
requestDisassociateTrunkInterface =
  req
    "DisassociateTrunkInterface"
    "fixture/DisassociateTrunkInterface.yaml"

requestDisassociateVpcCidrBlock :: DisassociateVpcCidrBlock -> TestTree
requestDisassociateVpcCidrBlock =
  req
    "DisassociateVpcCidrBlock"
    "fixture/DisassociateVpcCidrBlock.yaml"

requestEnableAddressTransfer :: EnableAddressTransfer -> TestTree
requestEnableAddressTransfer =
  req
    "EnableAddressTransfer"
    "fixture/EnableAddressTransfer.yaml"

requestEnableAwsNetworkPerformanceMetricSubscription :: EnableAwsNetworkPerformanceMetricSubscription -> TestTree
requestEnableAwsNetworkPerformanceMetricSubscription =
  req
    "EnableAwsNetworkPerformanceMetricSubscription"
    "fixture/EnableAwsNetworkPerformanceMetricSubscription.yaml"

requestEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefault -> TestTree
requestEnableEbsEncryptionByDefault =
  req
    "EnableEbsEncryptionByDefault"
    "fixture/EnableEbsEncryptionByDefault.yaml"

requestEnableFastLaunch :: EnableFastLaunch -> TestTree
requestEnableFastLaunch =
  req
    "EnableFastLaunch"
    "fixture/EnableFastLaunch.yaml"

requestEnableFastSnapshotRestores :: EnableFastSnapshotRestores -> TestTree
requestEnableFastSnapshotRestores =
  req
    "EnableFastSnapshotRestores"
    "fixture/EnableFastSnapshotRestores.yaml"

requestEnableImageDeprecation :: EnableImageDeprecation -> TestTree
requestEnableImageDeprecation =
  req
    "EnableImageDeprecation"
    "fixture/EnableImageDeprecation.yaml"

requestEnableIpamOrganizationAdminAccount :: EnableIpamOrganizationAdminAccount -> TestTree
requestEnableIpamOrganizationAdminAccount =
  req
    "EnableIpamOrganizationAdminAccount"
    "fixture/EnableIpamOrganizationAdminAccount.yaml"

requestEnableReachabilityAnalyzerOrganizationSharing :: EnableReachabilityAnalyzerOrganizationSharing -> TestTree
requestEnableReachabilityAnalyzerOrganizationSharing =
  req
    "EnableReachabilityAnalyzerOrganizationSharing"
    "fixture/EnableReachabilityAnalyzerOrganizationSharing.yaml"

requestEnableSerialConsoleAccess :: EnableSerialConsoleAccess -> TestTree
requestEnableSerialConsoleAccess =
  req
    "EnableSerialConsoleAccess"
    "fixture/EnableSerialConsoleAccess.yaml"

requestEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagation -> TestTree
requestEnableTransitGatewayRouteTablePropagation =
  req
    "EnableTransitGatewayRouteTablePropagation"
    "fixture/EnableTransitGatewayRouteTablePropagation.yaml"

requestEnableVgwRoutePropagation :: EnableVgwRoutePropagation -> TestTree
requestEnableVgwRoutePropagation =
  req
    "EnableVgwRoutePropagation"
    "fixture/EnableVgwRoutePropagation.yaml"

requestEnableVolumeIO :: EnableVolumeIO -> TestTree
requestEnableVolumeIO =
  req
    "EnableVolumeIO"
    "fixture/EnableVolumeIO.yaml"

requestEnableVpcClassicLink :: EnableVpcClassicLink -> TestTree
requestEnableVpcClassicLink =
  req
    "EnableVpcClassicLink"
    "fixture/EnableVpcClassicLink.yaml"

requestEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupport -> TestTree
requestEnableVpcClassicLinkDnsSupport =
  req
    "EnableVpcClassicLinkDnsSupport"
    "fixture/EnableVpcClassicLinkDnsSupport.yaml"

requestExportClientVpnClientCertificateRevocationList :: ExportClientVpnClientCertificateRevocationList -> TestTree
requestExportClientVpnClientCertificateRevocationList =
  req
    "ExportClientVpnClientCertificateRevocationList"
    "fixture/ExportClientVpnClientCertificateRevocationList.yaml"

requestExportClientVpnClientConfiguration :: ExportClientVpnClientConfiguration -> TestTree
requestExportClientVpnClientConfiguration =
  req
    "ExportClientVpnClientConfiguration"
    "fixture/ExportClientVpnClientConfiguration.yaml"

requestExportImage :: ExportImage -> TestTree
requestExportImage =
  req
    "ExportImage"
    "fixture/ExportImage.yaml"

requestExportTransitGatewayRoutes :: ExportTransitGatewayRoutes -> TestTree
requestExportTransitGatewayRoutes =
  req
    "ExportTransitGatewayRoutes"
    "fixture/ExportTransitGatewayRoutes.yaml"

requestGetAssociatedEnclaveCertificateIamRoles :: GetAssociatedEnclaveCertificateIamRoles -> TestTree
requestGetAssociatedEnclaveCertificateIamRoles =
  req
    "GetAssociatedEnclaveCertificateIamRoles"
    "fixture/GetAssociatedEnclaveCertificateIamRoles.yaml"

requestGetAssociatedIpv6PoolCidrs :: GetAssociatedIpv6PoolCidrs -> TestTree
requestGetAssociatedIpv6PoolCidrs =
  req
    "GetAssociatedIpv6PoolCidrs"
    "fixture/GetAssociatedIpv6PoolCidrs.yaml"

requestGetAwsNetworkPerformanceData :: GetAwsNetworkPerformanceData -> TestTree
requestGetAwsNetworkPerformanceData =
  req
    "GetAwsNetworkPerformanceData"
    "fixture/GetAwsNetworkPerformanceData.yaml"

requestGetCapacityReservationUsage :: GetCapacityReservationUsage -> TestTree
requestGetCapacityReservationUsage =
  req
    "GetCapacityReservationUsage"
    "fixture/GetCapacityReservationUsage.yaml"

requestGetCoipPoolUsage :: GetCoipPoolUsage -> TestTree
requestGetCoipPoolUsage =
  req
    "GetCoipPoolUsage"
    "fixture/GetCoipPoolUsage.yaml"

requestGetConsoleOutput :: GetConsoleOutput -> TestTree
requestGetConsoleOutput =
  req
    "GetConsoleOutput"
    "fixture/GetConsoleOutput.yaml"

requestGetConsoleScreenshot :: GetConsoleScreenshot -> TestTree
requestGetConsoleScreenshot =
  req
    "GetConsoleScreenshot"
    "fixture/GetConsoleScreenshot.yaml"

requestGetDefaultCreditSpecification :: GetDefaultCreditSpecification -> TestTree
requestGetDefaultCreditSpecification =
  req
    "GetDefaultCreditSpecification"
    "fixture/GetDefaultCreditSpecification.yaml"

requestGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyId -> TestTree
requestGetEbsDefaultKmsKeyId =
  req
    "GetEbsDefaultKmsKeyId"
    "fixture/GetEbsDefaultKmsKeyId.yaml"

requestGetEbsEncryptionByDefault :: GetEbsEncryptionByDefault -> TestTree
requestGetEbsEncryptionByDefault =
  req
    "GetEbsEncryptionByDefault"
    "fixture/GetEbsEncryptionByDefault.yaml"

requestGetFlowLogsIntegrationTemplate :: GetFlowLogsIntegrationTemplate -> TestTree
requestGetFlowLogsIntegrationTemplate =
  req
    "GetFlowLogsIntegrationTemplate"
    "fixture/GetFlowLogsIntegrationTemplate.yaml"

requestGetGroupsForCapacityReservation :: GetGroupsForCapacityReservation -> TestTree
requestGetGroupsForCapacityReservation =
  req
    "GetGroupsForCapacityReservation"
    "fixture/GetGroupsForCapacityReservation.yaml"

requestGetHostReservationPurchasePreview :: GetHostReservationPurchasePreview -> TestTree
requestGetHostReservationPurchasePreview =
  req
    "GetHostReservationPurchasePreview"
    "fixture/GetHostReservationPurchasePreview.yaml"

requestGetInstanceTypesFromInstanceRequirements :: GetInstanceTypesFromInstanceRequirements -> TestTree
requestGetInstanceTypesFromInstanceRequirements =
  req
    "GetInstanceTypesFromInstanceRequirements"
    "fixture/GetInstanceTypesFromInstanceRequirements.yaml"

requestGetInstanceUefiData :: GetInstanceUefiData -> TestTree
requestGetInstanceUefiData =
  req
    "GetInstanceUefiData"
    "fixture/GetInstanceUefiData.yaml"

requestGetIpamAddressHistory :: GetIpamAddressHistory -> TestTree
requestGetIpamAddressHistory =
  req
    "GetIpamAddressHistory"
    "fixture/GetIpamAddressHistory.yaml"

requestGetIpamPoolAllocations :: GetIpamPoolAllocations -> TestTree
requestGetIpamPoolAllocations =
  req
    "GetIpamPoolAllocations"
    "fixture/GetIpamPoolAllocations.yaml"

requestGetIpamPoolCidrs :: GetIpamPoolCidrs -> TestTree
requestGetIpamPoolCidrs =
  req
    "GetIpamPoolCidrs"
    "fixture/GetIpamPoolCidrs.yaml"

requestGetIpamResourceCidrs :: GetIpamResourceCidrs -> TestTree
requestGetIpamResourceCidrs =
  req
    "GetIpamResourceCidrs"
    "fixture/GetIpamResourceCidrs.yaml"

requestGetLaunchTemplateData :: GetLaunchTemplateData -> TestTree
requestGetLaunchTemplateData =
  req
    "GetLaunchTemplateData"
    "fixture/GetLaunchTemplateData.yaml"

requestGetManagedPrefixListAssociations :: GetManagedPrefixListAssociations -> TestTree
requestGetManagedPrefixListAssociations =
  req
    "GetManagedPrefixListAssociations"
    "fixture/GetManagedPrefixListAssociations.yaml"

requestGetManagedPrefixListEntries :: GetManagedPrefixListEntries -> TestTree
requestGetManagedPrefixListEntries =
  req
    "GetManagedPrefixListEntries"
    "fixture/GetManagedPrefixListEntries.yaml"

requestGetNetworkInsightsAccessScopeAnalysisFindings :: GetNetworkInsightsAccessScopeAnalysisFindings -> TestTree
requestGetNetworkInsightsAccessScopeAnalysisFindings =
  req
    "GetNetworkInsightsAccessScopeAnalysisFindings"
    "fixture/GetNetworkInsightsAccessScopeAnalysisFindings.yaml"

requestGetNetworkInsightsAccessScopeContent :: GetNetworkInsightsAccessScopeContent -> TestTree
requestGetNetworkInsightsAccessScopeContent =
  req
    "GetNetworkInsightsAccessScopeContent"
    "fixture/GetNetworkInsightsAccessScopeContent.yaml"

requestGetPasswordData :: GetPasswordData -> TestTree
requestGetPasswordData =
  req
    "GetPasswordData"
    "fixture/GetPasswordData.yaml"

requestGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuote -> TestTree
requestGetReservedInstancesExchangeQuote =
  req
    "GetReservedInstancesExchangeQuote"
    "fixture/GetReservedInstancesExchangeQuote.yaml"

requestGetSerialConsoleAccessStatus :: GetSerialConsoleAccessStatus -> TestTree
requestGetSerialConsoleAccessStatus =
  req
    "GetSerialConsoleAccessStatus"
    "fixture/GetSerialConsoleAccessStatus.yaml"

requestGetSpotPlacementScores :: GetSpotPlacementScores -> TestTree
requestGetSpotPlacementScores =
  req
    "GetSpotPlacementScores"
    "fixture/GetSpotPlacementScores.yaml"

requestGetSubnetCidrReservations :: GetSubnetCidrReservations -> TestTree
requestGetSubnetCidrReservations =
  req
    "GetSubnetCidrReservations"
    "fixture/GetSubnetCidrReservations.yaml"

requestGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagations -> TestTree
requestGetTransitGatewayAttachmentPropagations =
  req
    "GetTransitGatewayAttachmentPropagations"
    "fixture/GetTransitGatewayAttachmentPropagations.yaml"

requestGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociations -> TestTree
requestGetTransitGatewayMulticastDomainAssociations =
  req
    "GetTransitGatewayMulticastDomainAssociations"
    "fixture/GetTransitGatewayMulticastDomainAssociations.yaml"

requestGetTransitGatewayPolicyTableAssociations :: GetTransitGatewayPolicyTableAssociations -> TestTree
requestGetTransitGatewayPolicyTableAssociations =
  req
    "GetTransitGatewayPolicyTableAssociations"
    "fixture/GetTransitGatewayPolicyTableAssociations.yaml"

requestGetTransitGatewayPolicyTableEntries :: GetTransitGatewayPolicyTableEntries -> TestTree
requestGetTransitGatewayPolicyTableEntries =
  req
    "GetTransitGatewayPolicyTableEntries"
    "fixture/GetTransitGatewayPolicyTableEntries.yaml"

requestGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferences -> TestTree
requestGetTransitGatewayPrefixListReferences =
  req
    "GetTransitGatewayPrefixListReferences"
    "fixture/GetTransitGatewayPrefixListReferences.yaml"

requestGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociations -> TestTree
requestGetTransitGatewayRouteTableAssociations =
  req
    "GetTransitGatewayRouteTableAssociations"
    "fixture/GetTransitGatewayRouteTableAssociations.yaml"

requestGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagations -> TestTree
requestGetTransitGatewayRouteTablePropagations =
  req
    "GetTransitGatewayRouteTablePropagations"
    "fixture/GetTransitGatewayRouteTablePropagations.yaml"

requestGetVerifiedAccessEndpointPolicy :: GetVerifiedAccessEndpointPolicy -> TestTree
requestGetVerifiedAccessEndpointPolicy =
  req
    "GetVerifiedAccessEndpointPolicy"
    "fixture/GetVerifiedAccessEndpointPolicy.yaml"

requestGetVerifiedAccessGroupPolicy :: GetVerifiedAccessGroupPolicy -> TestTree
requestGetVerifiedAccessGroupPolicy =
  req
    "GetVerifiedAccessGroupPolicy"
    "fixture/GetVerifiedAccessGroupPolicy.yaml"

requestGetVpnConnectionDeviceSampleConfiguration :: GetVpnConnectionDeviceSampleConfiguration -> TestTree
requestGetVpnConnectionDeviceSampleConfiguration =
  req
    "GetVpnConnectionDeviceSampleConfiguration"
    "fixture/GetVpnConnectionDeviceSampleConfiguration.yaml"

requestGetVpnConnectionDeviceTypes :: GetVpnConnectionDeviceTypes -> TestTree
requestGetVpnConnectionDeviceTypes =
  req
    "GetVpnConnectionDeviceTypes"
    "fixture/GetVpnConnectionDeviceTypes.yaml"

requestImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationList -> TestTree
requestImportClientVpnClientCertificateRevocationList =
  req
    "ImportClientVpnClientCertificateRevocationList"
    "fixture/ImportClientVpnClientCertificateRevocationList.yaml"

requestImportImage :: ImportImage -> TestTree
requestImportImage =
  req
    "ImportImage"
    "fixture/ImportImage.yaml"

requestImportInstance :: ImportInstance -> TestTree
requestImportInstance =
  req
    "ImportInstance"
    "fixture/ImportInstance.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair =
  req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestImportSnapshot :: ImportSnapshot -> TestTree
requestImportSnapshot =
  req
    "ImportSnapshot"
    "fixture/ImportSnapshot.yaml"

requestImportVolume :: ImportVolume -> TestTree
requestImportVolume =
  req
    "ImportVolume"
    "fixture/ImportVolume.yaml"

requestListImagesInRecycleBin :: ListImagesInRecycleBin -> TestTree
requestListImagesInRecycleBin =
  req
    "ListImagesInRecycleBin"
    "fixture/ListImagesInRecycleBin.yaml"

requestListSnapshotsInRecycleBin :: ListSnapshotsInRecycleBin -> TestTree
requestListSnapshotsInRecycleBin =
  req
    "ListSnapshotsInRecycleBin"
    "fixture/ListSnapshotsInRecycleBin.yaml"

requestModifyAddressAttribute :: ModifyAddressAttribute -> TestTree
requestModifyAddressAttribute =
  req
    "ModifyAddressAttribute"
    "fixture/ModifyAddressAttribute.yaml"

requestModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroup -> TestTree
requestModifyAvailabilityZoneGroup =
  req
    "ModifyAvailabilityZoneGroup"
    "fixture/ModifyAvailabilityZoneGroup.yaml"

requestModifyCapacityReservation :: ModifyCapacityReservation -> TestTree
requestModifyCapacityReservation =
  req
    "ModifyCapacityReservation"
    "fixture/ModifyCapacityReservation.yaml"

requestModifyCapacityReservationFleet :: ModifyCapacityReservationFleet -> TestTree
requestModifyCapacityReservationFleet =
  req
    "ModifyCapacityReservationFleet"
    "fixture/ModifyCapacityReservationFleet.yaml"

requestModifyClientVpnEndpoint :: ModifyClientVpnEndpoint -> TestTree
requestModifyClientVpnEndpoint =
  req
    "ModifyClientVpnEndpoint"
    "fixture/ModifyClientVpnEndpoint.yaml"

requestModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecification -> TestTree
requestModifyDefaultCreditSpecification =
  req
    "ModifyDefaultCreditSpecification"
    "fixture/ModifyDefaultCreditSpecification.yaml"

requestModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyId -> TestTree
requestModifyEbsDefaultKmsKeyId =
  req
    "ModifyEbsDefaultKmsKeyId"
    "fixture/ModifyEbsDefaultKmsKeyId.yaml"

requestModifyFleet :: ModifyFleet -> TestTree
requestModifyFleet =
  req
    "ModifyFleet"
    "fixture/ModifyFleet.yaml"

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

requestModifyIdFormat :: ModifyIdFormat -> TestTree
requestModifyIdFormat =
  req
    "ModifyIdFormat"
    "fixture/ModifyIdFormat.yaml"

requestModifyIdentityIdFormat :: ModifyIdentityIdFormat -> TestTree
requestModifyIdentityIdFormat =
  req
    "ModifyIdentityIdFormat"
    "fixture/ModifyIdentityIdFormat.yaml"

requestModifyImageAttribute :: ModifyImageAttribute -> TestTree
requestModifyImageAttribute =
  req
    "ModifyImageAttribute"
    "fixture/ModifyImageAttribute.yaml"

requestModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
requestModifyInstanceAttribute =
  req
    "ModifyInstanceAttribute"
    "fixture/ModifyInstanceAttribute.yaml"

requestModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributes -> TestTree
requestModifyInstanceCapacityReservationAttributes =
  req
    "ModifyInstanceCapacityReservationAttributes"
    "fixture/ModifyInstanceCapacityReservationAttributes.yaml"

requestModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecification -> TestTree
requestModifyInstanceCreditSpecification =
  req
    "ModifyInstanceCreditSpecification"
    "fixture/ModifyInstanceCreditSpecification.yaml"

requestModifyInstanceEventStartTime :: ModifyInstanceEventStartTime -> TestTree
requestModifyInstanceEventStartTime =
  req
    "ModifyInstanceEventStartTime"
    "fixture/ModifyInstanceEventStartTime.yaml"

requestModifyInstanceEventWindow :: ModifyInstanceEventWindow -> TestTree
requestModifyInstanceEventWindow =
  req
    "ModifyInstanceEventWindow"
    "fixture/ModifyInstanceEventWindow.yaml"

requestModifyInstanceMaintenanceOptions :: ModifyInstanceMaintenanceOptions -> TestTree
requestModifyInstanceMaintenanceOptions =
  req
    "ModifyInstanceMaintenanceOptions"
    "fixture/ModifyInstanceMaintenanceOptions.yaml"

requestModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptions -> TestTree
requestModifyInstanceMetadataOptions =
  req
    "ModifyInstanceMetadataOptions"
    "fixture/ModifyInstanceMetadataOptions.yaml"

requestModifyInstancePlacement :: ModifyInstancePlacement -> TestTree
requestModifyInstancePlacement =
  req
    "ModifyInstancePlacement"
    "fixture/ModifyInstancePlacement.yaml"

requestModifyIpam :: ModifyIpam -> TestTree
requestModifyIpam =
  req
    "ModifyIpam"
    "fixture/ModifyIpam.yaml"

requestModifyIpamPool :: ModifyIpamPool -> TestTree
requestModifyIpamPool =
  req
    "ModifyIpamPool"
    "fixture/ModifyIpamPool.yaml"

requestModifyIpamResourceCidr :: ModifyIpamResourceCidr -> TestTree
requestModifyIpamResourceCidr =
  req
    "ModifyIpamResourceCidr"
    "fixture/ModifyIpamResourceCidr.yaml"

requestModifyIpamScope :: ModifyIpamScope -> TestTree
requestModifyIpamScope =
  req
    "ModifyIpamScope"
    "fixture/ModifyIpamScope.yaml"

requestModifyLaunchTemplate :: ModifyLaunchTemplate -> TestTree
requestModifyLaunchTemplate =
  req
    "ModifyLaunchTemplate"
    "fixture/ModifyLaunchTemplate.yaml"

requestModifyLocalGatewayRoute :: ModifyLocalGatewayRoute -> TestTree
requestModifyLocalGatewayRoute =
  req
    "ModifyLocalGatewayRoute"
    "fixture/ModifyLocalGatewayRoute.yaml"

requestModifyManagedPrefixList :: ModifyManagedPrefixList -> TestTree
requestModifyManagedPrefixList =
  req
    "ModifyManagedPrefixList"
    "fixture/ModifyManagedPrefixList.yaml"

requestModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
requestModifyNetworkInterfaceAttribute =
  req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute.yaml"

requestModifyPrivateDnsNameOptions :: ModifyPrivateDnsNameOptions -> TestTree
requestModifyPrivateDnsNameOptions =
  req
    "ModifyPrivateDnsNameOptions"
    "fixture/ModifyPrivateDnsNameOptions.yaml"

requestModifyReservedInstances :: ModifyReservedInstances -> TestTree
requestModifyReservedInstances =
  req
    "ModifyReservedInstances"
    "fixture/ModifyReservedInstances.yaml"

requestModifySecurityGroupRules :: ModifySecurityGroupRules -> TestTree
requestModifySecurityGroupRules =
  req
    "ModifySecurityGroupRules"
    "fixture/ModifySecurityGroupRules.yaml"

requestModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
requestModifySnapshotAttribute =
  req
    "ModifySnapshotAttribute"
    "fixture/ModifySnapshotAttribute.yaml"

requestModifySnapshotTier :: ModifySnapshotTier -> TestTree
requestModifySnapshotTier =
  req
    "ModifySnapshotTier"
    "fixture/ModifySnapshotTier.yaml"

requestModifySpotFleetRequest :: ModifySpotFleetRequest -> TestTree
requestModifySpotFleetRequest =
  req
    "ModifySpotFleetRequest"
    "fixture/ModifySpotFleetRequest.yaml"

requestModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
requestModifySubnetAttribute =
  req
    "ModifySubnetAttribute"
    "fixture/ModifySubnetAttribute.yaml"

requestModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServices -> TestTree
requestModifyTrafficMirrorFilterNetworkServices =
  req
    "ModifyTrafficMirrorFilterNetworkServices"
    "fixture/ModifyTrafficMirrorFilterNetworkServices.yaml"

requestModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRule -> TestTree
requestModifyTrafficMirrorFilterRule =
  req
    "ModifyTrafficMirrorFilterRule"
    "fixture/ModifyTrafficMirrorFilterRule.yaml"

requestModifyTrafficMirrorSession :: ModifyTrafficMirrorSession -> TestTree
requestModifyTrafficMirrorSession =
  req
    "ModifyTrafficMirrorSession"
    "fixture/ModifyTrafficMirrorSession.yaml"

requestModifyTransitGateway :: ModifyTransitGateway -> TestTree
requestModifyTransitGateway =
  req
    "ModifyTransitGateway"
    "fixture/ModifyTransitGateway.yaml"

requestModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReference -> TestTree
requestModifyTransitGatewayPrefixListReference =
  req
    "ModifyTransitGatewayPrefixListReference"
    "fixture/ModifyTransitGatewayPrefixListReference.yaml"

requestModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachment -> TestTree
requestModifyTransitGatewayVpcAttachment =
  req
    "ModifyTransitGatewayVpcAttachment"
    "fixture/ModifyTransitGatewayVpcAttachment.yaml"

requestModifyVerifiedAccessEndpoint :: ModifyVerifiedAccessEndpoint -> TestTree
requestModifyVerifiedAccessEndpoint =
  req
    "ModifyVerifiedAccessEndpoint"
    "fixture/ModifyVerifiedAccessEndpoint.yaml"

requestModifyVerifiedAccessEndpointPolicy :: ModifyVerifiedAccessEndpointPolicy -> TestTree
requestModifyVerifiedAccessEndpointPolicy =
  req
    "ModifyVerifiedAccessEndpointPolicy"
    "fixture/ModifyVerifiedAccessEndpointPolicy.yaml"

requestModifyVerifiedAccessGroup :: ModifyVerifiedAccessGroup -> TestTree
requestModifyVerifiedAccessGroup =
  req
    "ModifyVerifiedAccessGroup"
    "fixture/ModifyVerifiedAccessGroup.yaml"

requestModifyVerifiedAccessGroupPolicy :: ModifyVerifiedAccessGroupPolicy -> TestTree
requestModifyVerifiedAccessGroupPolicy =
  req
    "ModifyVerifiedAccessGroupPolicy"
    "fixture/ModifyVerifiedAccessGroupPolicy.yaml"

requestModifyVerifiedAccessInstance :: ModifyVerifiedAccessInstance -> TestTree
requestModifyVerifiedAccessInstance =
  req
    "ModifyVerifiedAccessInstance"
    "fixture/ModifyVerifiedAccessInstance.yaml"

requestModifyVerifiedAccessInstanceLoggingConfiguration :: ModifyVerifiedAccessInstanceLoggingConfiguration -> TestTree
requestModifyVerifiedAccessInstanceLoggingConfiguration =
  req
    "ModifyVerifiedAccessInstanceLoggingConfiguration"
    "fixture/ModifyVerifiedAccessInstanceLoggingConfiguration.yaml"

requestModifyVerifiedAccessTrustProvider :: ModifyVerifiedAccessTrustProvider -> TestTree
requestModifyVerifiedAccessTrustProvider =
  req
    "ModifyVerifiedAccessTrustProvider"
    "fixture/ModifyVerifiedAccessTrustProvider.yaml"

requestModifyVolume :: ModifyVolume -> TestTree
requestModifyVolume =
  req
    "ModifyVolume"
    "fixture/ModifyVolume.yaml"

requestModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
requestModifyVolumeAttribute =
  req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute.yaml"

requestModifyVpcAttribute :: ModifyVpcAttribute -> TestTree
requestModifyVpcAttribute =
  req
    "ModifyVpcAttribute"
    "fixture/ModifyVpcAttribute.yaml"

requestModifyVpcEndpoint :: ModifyVpcEndpoint -> TestTree
requestModifyVpcEndpoint =
  req
    "ModifyVpcEndpoint"
    "fixture/ModifyVpcEndpoint.yaml"

requestModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotification -> TestTree
requestModifyVpcEndpointConnectionNotification =
  req
    "ModifyVpcEndpointConnectionNotification"
    "fixture/ModifyVpcEndpointConnectionNotification.yaml"

requestModifyVpcEndpointServiceConfiguration :: ModifyVpcEndpointServiceConfiguration -> TestTree
requestModifyVpcEndpointServiceConfiguration =
  req
    "ModifyVpcEndpointServiceConfiguration"
    "fixture/ModifyVpcEndpointServiceConfiguration.yaml"

requestModifyVpcEndpointServicePayerResponsibility :: ModifyVpcEndpointServicePayerResponsibility -> TestTree
requestModifyVpcEndpointServicePayerResponsibility =
  req
    "ModifyVpcEndpointServicePayerResponsibility"
    "fixture/ModifyVpcEndpointServicePayerResponsibility.yaml"

requestModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissions -> TestTree
requestModifyVpcEndpointServicePermissions =
  req
    "ModifyVpcEndpointServicePermissions"
    "fixture/ModifyVpcEndpointServicePermissions.yaml"

requestModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptions -> TestTree
requestModifyVpcPeeringConnectionOptions =
  req
    "ModifyVpcPeeringConnectionOptions"
    "fixture/ModifyVpcPeeringConnectionOptions.yaml"

requestModifyVpcTenancy :: ModifyVpcTenancy -> TestTree
requestModifyVpcTenancy =
  req
    "ModifyVpcTenancy"
    "fixture/ModifyVpcTenancy.yaml"

requestModifyVpnConnection :: ModifyVpnConnection -> TestTree
requestModifyVpnConnection =
  req
    "ModifyVpnConnection"
    "fixture/ModifyVpnConnection.yaml"

requestModifyVpnConnectionOptions :: ModifyVpnConnectionOptions -> TestTree
requestModifyVpnConnectionOptions =
  req
    "ModifyVpnConnectionOptions"
    "fixture/ModifyVpnConnectionOptions.yaml"

requestModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificate -> TestTree
requestModifyVpnTunnelCertificate =
  req
    "ModifyVpnTunnelCertificate"
    "fixture/ModifyVpnTunnelCertificate.yaml"

requestModifyVpnTunnelOptions :: ModifyVpnTunnelOptions -> TestTree
requestModifyVpnTunnelOptions =
  req
    "ModifyVpnTunnelOptions"
    "fixture/ModifyVpnTunnelOptions.yaml"

requestMonitorInstances :: MonitorInstances -> TestTree
requestMonitorInstances =
  req
    "MonitorInstances"
    "fixture/MonitorInstances.yaml"

requestMoveAddressToVpc :: MoveAddressToVpc -> TestTree
requestMoveAddressToVpc =
  req
    "MoveAddressToVpc"
    "fixture/MoveAddressToVpc.yaml"

requestMoveByoipCidrToIpam :: MoveByoipCidrToIpam -> TestTree
requestMoveByoipCidrToIpam =
  req
    "MoveByoipCidrToIpam"
    "fixture/MoveByoipCidrToIpam.yaml"

requestProvisionByoipCidr :: ProvisionByoipCidr -> TestTree
requestProvisionByoipCidr =
  req
    "ProvisionByoipCidr"
    "fixture/ProvisionByoipCidr.yaml"

requestProvisionIpamPoolCidr :: ProvisionIpamPoolCidr -> TestTree
requestProvisionIpamPoolCidr =
  req
    "ProvisionIpamPoolCidr"
    "fixture/ProvisionIpamPoolCidr.yaml"

requestProvisionPublicIpv4PoolCidr :: ProvisionPublicIpv4PoolCidr -> TestTree
requestProvisionPublicIpv4PoolCidr =
  req
    "ProvisionPublicIpv4PoolCidr"
    "fixture/ProvisionPublicIpv4PoolCidr.yaml"

requestPurchaseHostReservation :: PurchaseHostReservation -> TestTree
requestPurchaseHostReservation =
  req
    "PurchaseHostReservation"
    "fixture/PurchaseHostReservation.yaml"

requestPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
requestPurchaseReservedInstancesOffering =
  req
    "PurchaseReservedInstancesOffering"
    "fixture/PurchaseReservedInstancesOffering.yaml"

requestPurchaseScheduledInstances :: PurchaseScheduledInstances -> TestTree
requestPurchaseScheduledInstances =
  req
    "PurchaseScheduledInstances"
    "fixture/PurchaseScheduledInstances.yaml"

requestRebootInstances :: RebootInstances -> TestTree
requestRebootInstances =
  req
    "RebootInstances"
    "fixture/RebootInstances.yaml"

requestRegisterImage :: RegisterImage -> TestTree
requestRegisterImage =
  req
    "RegisterImage"
    "fixture/RegisterImage.yaml"

requestRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributes -> TestTree
requestRegisterInstanceEventNotificationAttributes =
  req
    "RegisterInstanceEventNotificationAttributes"
    "fixture/RegisterInstanceEventNotificationAttributes.yaml"

requestRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembers -> TestTree
requestRegisterTransitGatewayMulticastGroupMembers =
  req
    "RegisterTransitGatewayMulticastGroupMembers"
    "fixture/RegisterTransitGatewayMulticastGroupMembers.yaml"

requestRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSources -> TestTree
requestRegisterTransitGatewayMulticastGroupSources =
  req
    "RegisterTransitGatewayMulticastGroupSources"
    "fixture/RegisterTransitGatewayMulticastGroupSources.yaml"

requestRejectTransitGatewayMulticastDomainAssociations :: RejectTransitGatewayMulticastDomainAssociations -> TestTree
requestRejectTransitGatewayMulticastDomainAssociations =
  req
    "RejectTransitGatewayMulticastDomainAssociations"
    "fixture/RejectTransitGatewayMulticastDomainAssociations.yaml"

requestRejectTransitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachment -> TestTree
requestRejectTransitGatewayPeeringAttachment =
  req
    "RejectTransitGatewayPeeringAttachment"
    "fixture/RejectTransitGatewayPeeringAttachment.yaml"

requestRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachment -> TestTree
requestRejectTransitGatewayVpcAttachment =
  req
    "RejectTransitGatewayVpcAttachment"
    "fixture/RejectTransitGatewayVpcAttachment.yaml"

requestRejectVpcEndpointConnections :: RejectVpcEndpointConnections -> TestTree
requestRejectVpcEndpointConnections =
  req
    "RejectVpcEndpointConnections"
    "fixture/RejectVpcEndpointConnections.yaml"

requestRejectVpcPeeringConnection :: RejectVpcPeeringConnection -> TestTree
requestRejectVpcPeeringConnection =
  req
    "RejectVpcPeeringConnection"
    "fixture/RejectVpcPeeringConnection.yaml"

requestReleaseAddress :: ReleaseAddress -> TestTree
requestReleaseAddress =
  req
    "ReleaseAddress"
    "fixture/ReleaseAddress.yaml"

requestReleaseHosts :: ReleaseHosts -> TestTree
requestReleaseHosts =
  req
    "ReleaseHosts"
    "fixture/ReleaseHosts.yaml"

requestReleaseIpamPoolAllocation :: ReleaseIpamPoolAllocation -> TestTree
requestReleaseIpamPoolAllocation =
  req
    "ReleaseIpamPoolAllocation"
    "fixture/ReleaseIpamPoolAllocation.yaml"

requestReplaceIamInstanceProfileAssociation :: ReplaceIamInstanceProfileAssociation -> TestTree
requestReplaceIamInstanceProfileAssociation =
  req
    "ReplaceIamInstanceProfileAssociation"
    "fixture/ReplaceIamInstanceProfileAssociation.yaml"

requestReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociation -> TestTree
requestReplaceNetworkAclAssociation =
  req
    "ReplaceNetworkAclAssociation"
    "fixture/ReplaceNetworkAclAssociation.yaml"

requestReplaceNetworkAclEntry :: ReplaceNetworkAclEntry -> TestTree
requestReplaceNetworkAclEntry =
  req
    "ReplaceNetworkAclEntry"
    "fixture/ReplaceNetworkAclEntry.yaml"

requestReplaceRoute :: ReplaceRoute -> TestTree
requestReplaceRoute =
  req
    "ReplaceRoute"
    "fixture/ReplaceRoute.yaml"

requestReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
requestReplaceRouteTableAssociation =
  req
    "ReplaceRouteTableAssociation"
    "fixture/ReplaceRouteTableAssociation.yaml"

requestReplaceTransitGatewayRoute :: ReplaceTransitGatewayRoute -> TestTree
requestReplaceTransitGatewayRoute =
  req
    "ReplaceTransitGatewayRoute"
    "fixture/ReplaceTransitGatewayRoute.yaml"

requestReportInstanceStatus :: ReportInstanceStatus -> TestTree
requestReportInstanceStatus =
  req
    "ReportInstanceStatus"
    "fixture/ReportInstanceStatus.yaml"

requestRequestSpotFleet :: RequestSpotFleet -> TestTree
requestRequestSpotFleet =
  req
    "RequestSpotFleet"
    "fixture/RequestSpotFleet.yaml"

requestRequestSpotInstances :: RequestSpotInstances -> TestTree
requestRequestSpotInstances =
  req
    "RequestSpotInstances"
    "fixture/RequestSpotInstances.yaml"

requestResetAddressAttribute :: ResetAddressAttribute -> TestTree
requestResetAddressAttribute =
  req
    "ResetAddressAttribute"
    "fixture/ResetAddressAttribute.yaml"

requestResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyId -> TestTree
requestResetEbsDefaultKmsKeyId =
  req
    "ResetEbsDefaultKmsKeyId"
    "fixture/ResetEbsDefaultKmsKeyId.yaml"

requestResetFpgaImageAttribute :: ResetFpgaImageAttribute -> TestTree
requestResetFpgaImageAttribute =
  req
    "ResetFpgaImageAttribute"
    "fixture/ResetFpgaImageAttribute.yaml"

requestResetImageAttribute :: ResetImageAttribute -> TestTree
requestResetImageAttribute =
  req
    "ResetImageAttribute"
    "fixture/ResetImageAttribute.yaml"

requestResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
requestResetInstanceAttribute =
  req
    "ResetInstanceAttribute"
    "fixture/ResetInstanceAttribute.yaml"

requestResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
requestResetNetworkInterfaceAttribute =
  req
    "ResetNetworkInterfaceAttribute"
    "fixture/ResetNetworkInterfaceAttribute.yaml"

requestResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
requestResetSnapshotAttribute =
  req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute.yaml"

requestRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
requestRestoreAddressToClassic =
  req
    "RestoreAddressToClassic"
    "fixture/RestoreAddressToClassic.yaml"

requestRestoreImageFromRecycleBin :: RestoreImageFromRecycleBin -> TestTree
requestRestoreImageFromRecycleBin =
  req
    "RestoreImageFromRecycleBin"
    "fixture/RestoreImageFromRecycleBin.yaml"

requestRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersion -> TestTree
requestRestoreManagedPrefixListVersion =
  req
    "RestoreManagedPrefixListVersion"
    "fixture/RestoreManagedPrefixListVersion.yaml"

requestRestoreSnapshotFromRecycleBin :: RestoreSnapshotFromRecycleBin -> TestTree
requestRestoreSnapshotFromRecycleBin =
  req
    "RestoreSnapshotFromRecycleBin"
    "fixture/RestoreSnapshotFromRecycleBin.yaml"

requestRestoreSnapshotTier :: RestoreSnapshotTier -> TestTree
requestRestoreSnapshotTier =
  req
    "RestoreSnapshotTier"
    "fixture/RestoreSnapshotTier.yaml"

requestRevokeClientVpnIngress :: RevokeClientVpnIngress -> TestTree
requestRevokeClientVpnIngress =
  req
    "RevokeClientVpnIngress"
    "fixture/RevokeClientVpnIngress.yaml"

requestRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
requestRevokeSecurityGroupEgress =
  req
    "RevokeSecurityGroupEgress"
    "fixture/RevokeSecurityGroupEgress.yaml"

requestRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
requestRevokeSecurityGroupIngress =
  req
    "RevokeSecurityGroupIngress"
    "fixture/RevokeSecurityGroupIngress.yaml"

requestRunInstances :: RunInstances -> TestTree
requestRunInstances =
  req
    "RunInstances"
    "fixture/RunInstances.yaml"

requestRunScheduledInstances :: RunScheduledInstances -> TestTree
requestRunScheduledInstances =
  req
    "RunScheduledInstances"
    "fixture/RunScheduledInstances.yaml"

requestSearchLocalGatewayRoutes :: SearchLocalGatewayRoutes -> TestTree
requestSearchLocalGatewayRoutes =
  req
    "SearchLocalGatewayRoutes"
    "fixture/SearchLocalGatewayRoutes.yaml"

requestSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroups -> TestTree
requestSearchTransitGatewayMulticastGroups =
  req
    "SearchTransitGatewayMulticastGroups"
    "fixture/SearchTransitGatewayMulticastGroups.yaml"

requestSearchTransitGatewayRoutes :: SearchTransitGatewayRoutes -> TestTree
requestSearchTransitGatewayRoutes =
  req
    "SearchTransitGatewayRoutes"
    "fixture/SearchTransitGatewayRoutes.yaml"

requestSendDiagnosticInterrupt :: SendDiagnosticInterrupt -> TestTree
requestSendDiagnosticInterrupt =
  req
    "SendDiagnosticInterrupt"
    "fixture/SendDiagnosticInterrupt.yaml"

requestStartInstances :: StartInstances -> TestTree
requestStartInstances =
  req
    "StartInstances"
    "fixture/StartInstances.yaml"

requestStartNetworkInsightsAccessScopeAnalysis :: StartNetworkInsightsAccessScopeAnalysis -> TestTree
requestStartNetworkInsightsAccessScopeAnalysis =
  req
    "StartNetworkInsightsAccessScopeAnalysis"
    "fixture/StartNetworkInsightsAccessScopeAnalysis.yaml"

requestStartNetworkInsightsAnalysis :: StartNetworkInsightsAnalysis -> TestTree
requestStartNetworkInsightsAnalysis =
  req
    "StartNetworkInsightsAnalysis"
    "fixture/StartNetworkInsightsAnalysis.yaml"

requestStartVpcEndpointServicePrivateDnsVerification :: StartVpcEndpointServicePrivateDnsVerification -> TestTree
requestStartVpcEndpointServicePrivateDnsVerification =
  req
    "StartVpcEndpointServicePrivateDnsVerification"
    "fixture/StartVpcEndpointServicePrivateDnsVerification.yaml"

requestStopInstances :: StopInstances -> TestTree
requestStopInstances =
  req
    "StopInstances"
    "fixture/StopInstances.yaml"

requestTerminateClientVpnConnections :: TerminateClientVpnConnections -> TestTree
requestTerminateClientVpnConnections =
  req
    "TerminateClientVpnConnections"
    "fixture/TerminateClientVpnConnections.yaml"

requestTerminateInstances :: TerminateInstances -> TestTree
requestTerminateInstances =
  req
    "TerminateInstances"
    "fixture/TerminateInstances.yaml"

requestUnassignIpv6Addresses :: UnassignIpv6Addresses -> TestTree
requestUnassignIpv6Addresses =
  req
    "UnassignIpv6Addresses"
    "fixture/UnassignIpv6Addresses.yaml"

requestUnassignPrivateIpAddresses :: UnassignPrivateIpAddresses -> TestTree
requestUnassignPrivateIpAddresses =
  req
    "UnassignPrivateIpAddresses"
    "fixture/UnassignPrivateIpAddresses.yaml"

requestUnmonitorInstances :: UnmonitorInstances -> TestTree
requestUnmonitorInstances =
  req
    "UnmonitorInstances"
    "fixture/UnmonitorInstances.yaml"

requestUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsEgress =
  req
    "UpdateSecurityGroupRuleDescriptionsEgress"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgress.yaml"

requestUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsIngress =
  req
    "UpdateSecurityGroupRuleDescriptionsIngress"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngress.yaml"

requestWithdrawByoipCidr :: WithdrawByoipCidr -> TestTree
requestWithdrawByoipCidr =
  req
    "WithdrawByoipCidr"
    "fixture/WithdrawByoipCidr.yaml"

-- Responses

responseAcceptAddressTransfer :: AcceptAddressTransferResponse -> TestTree
responseAcceptAddressTransfer =
  res
    "AcceptAddressTransferResponse"
    "fixture/AcceptAddressTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptAddressTransfer)

responseAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuoteResponse -> TestTree
responseAcceptReservedInstancesExchangeQuote =
  res
    "AcceptReservedInstancesExchangeQuoteResponse"
    "fixture/AcceptReservedInstancesExchangeQuoteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptReservedInstancesExchangeQuote)

responseAcceptTransitGatewayMulticastDomainAssociations :: AcceptTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseAcceptTransitGatewayMulticastDomainAssociations =
  res
    "AcceptTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/AcceptTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptTransitGatewayMulticastDomainAssociations)

responseAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachmentResponse -> TestTree
responseAcceptTransitGatewayPeeringAttachment =
  res
    "AcceptTransitGatewayPeeringAttachmentResponse"
    "fixture/AcceptTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptTransitGatewayPeeringAttachment)

responseAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachmentResponse -> TestTree
responseAcceptTransitGatewayVpcAttachment =
  res
    "AcceptTransitGatewayVpcAttachmentResponse"
    "fixture/AcceptTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptTransitGatewayVpcAttachment)

responseAcceptVpcEndpointConnections :: AcceptVpcEndpointConnectionsResponse -> TestTree
responseAcceptVpcEndpointConnections =
  res
    "AcceptVpcEndpointConnectionsResponse"
    "fixture/AcceptVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptVpcEndpointConnections)

responseAcceptVpcPeeringConnection :: AcceptVpcPeeringConnectionResponse -> TestTree
responseAcceptVpcPeeringConnection =
  res
    "AcceptVpcPeeringConnectionResponse"
    "fixture/AcceptVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptVpcPeeringConnection)

responseAdvertiseByoipCidr :: AdvertiseByoipCidrResponse -> TestTree
responseAdvertiseByoipCidr =
  res
    "AdvertiseByoipCidrResponse"
    "fixture/AdvertiseByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdvertiseByoipCidr)

responseAllocateAddress :: AllocateAddressResponse -> TestTree
responseAllocateAddress =
  res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateAddress)

responseAllocateHosts :: AllocateHostsResponse -> TestTree
responseAllocateHosts =
  res
    "AllocateHostsResponse"
    "fixture/AllocateHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateHosts)

responseAllocateIpamPoolCidr :: AllocateIpamPoolCidrResponse -> TestTree
responseAllocateIpamPoolCidr =
  res
    "AllocateIpamPoolCidrResponse"
    "fixture/AllocateIpamPoolCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateIpamPoolCidr)

responseApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetworkResponse -> TestTree
responseApplySecurityGroupsToClientVpnTargetNetwork =
  res
    "ApplySecurityGroupsToClientVpnTargetNetworkResponse"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplySecurityGroupsToClientVpnTargetNetwork)

responseAssignIpv6Addresses :: AssignIpv6AddressesResponse -> TestTree
responseAssignIpv6Addresses =
  res
    "AssignIpv6AddressesResponse"
    "fixture/AssignIpv6AddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignIpv6Addresses)

responseAssignPrivateIpAddresses :: AssignPrivateIpAddressesResponse -> TestTree
responseAssignPrivateIpAddresses =
  res
    "AssignPrivateIpAddressesResponse"
    "fixture/AssignPrivateIpAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignPrivateIpAddresses)

responseAssociateAddress :: AssociateAddressResponse -> TestTree
responseAssociateAddress =
  res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAddress)

responseAssociateClientVpnTargetNetwork :: AssociateClientVpnTargetNetworkResponse -> TestTree
responseAssociateClientVpnTargetNetwork =
  res
    "AssociateClientVpnTargetNetworkResponse"
    "fixture/AssociateClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateClientVpnTargetNetwork)

responseAssociateDhcpOptions :: AssociateDhcpOptionsResponse -> TestTree
responseAssociateDhcpOptions =
  res
    "AssociateDhcpOptionsResponse"
    "fixture/AssociateDhcpOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDhcpOptions)

responseAssociateEnclaveCertificateIamRole :: AssociateEnclaveCertificateIamRoleResponse -> TestTree
responseAssociateEnclaveCertificateIamRole =
  res
    "AssociateEnclaveCertificateIamRoleResponse"
    "fixture/AssociateEnclaveCertificateIamRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateEnclaveCertificateIamRole)

responseAssociateIamInstanceProfile :: AssociateIamInstanceProfileResponse -> TestTree
responseAssociateIamInstanceProfile =
  res
    "AssociateIamInstanceProfileResponse"
    "fixture/AssociateIamInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateIamInstanceProfile)

responseAssociateInstanceEventWindow :: AssociateInstanceEventWindowResponse -> TestTree
responseAssociateInstanceEventWindow =
  res
    "AssociateInstanceEventWindowResponse"
    "fixture/AssociateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateInstanceEventWindow)

responseAssociateRouteTable :: AssociateRouteTableResponse -> TestTree
responseAssociateRouteTable =
  res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateRouteTable)

responseAssociateSubnetCidrBlock :: AssociateSubnetCidrBlockResponse -> TestTree
responseAssociateSubnetCidrBlock =
  res
    "AssociateSubnetCidrBlockResponse"
    "fixture/AssociateSubnetCidrBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSubnetCidrBlock)

responseAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomainResponse -> TestTree
responseAssociateTransitGatewayMulticastDomain =
  res
    "AssociateTransitGatewayMulticastDomainResponse"
    "fixture/AssociateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTransitGatewayMulticastDomain)

responseAssociateTransitGatewayPolicyTable :: AssociateTransitGatewayPolicyTableResponse -> TestTree
responseAssociateTransitGatewayPolicyTable =
  res
    "AssociateTransitGatewayPolicyTableResponse"
    "fixture/AssociateTransitGatewayPolicyTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTransitGatewayPolicyTable)

responseAssociateTransitGatewayRouteTable :: AssociateTransitGatewayRouteTableResponse -> TestTree
responseAssociateTransitGatewayRouteTable =
  res
    "AssociateTransitGatewayRouteTableResponse"
    "fixture/AssociateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTransitGatewayRouteTable)

responseAssociateTrunkInterface :: AssociateTrunkInterfaceResponse -> TestTree
responseAssociateTrunkInterface =
  res
    "AssociateTrunkInterfaceResponse"
    "fixture/AssociateTrunkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTrunkInterface)

responseAssociateVpcCidrBlock :: AssociateVpcCidrBlockResponse -> TestTree
responseAssociateVpcCidrBlock =
  res
    "AssociateVpcCidrBlockResponse"
    "fixture/AssociateVpcCidrBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateVpcCidrBlock)

responseAttachClassicLinkVpc :: AttachClassicLinkVpcResponse -> TestTree
responseAttachClassicLinkVpc =
  res
    "AttachClassicLinkVpcResponse"
    "fixture/AttachClassicLinkVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachClassicLinkVpc)

responseAttachInternetGateway :: AttachInternetGatewayResponse -> TestTree
responseAttachInternetGateway =
  res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachInternetGateway)

responseAttachNetworkInterface :: AttachNetworkInterfaceResponse -> TestTree
responseAttachNetworkInterface =
  res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachNetworkInterface)

responseAttachVerifiedAccessTrustProvider :: AttachVerifiedAccessTrustProviderResponse -> TestTree
responseAttachVerifiedAccessTrustProvider =
  res
    "AttachVerifiedAccessTrustProviderResponse"
    "fixture/AttachVerifiedAccessTrustProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachVerifiedAccessTrustProvider)

responseAttachVolume :: VolumeAttachment -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachVolume)

responseAttachVpnGateway :: AttachVpnGatewayResponse -> TestTree
responseAttachVpnGateway =
  res
    "AttachVpnGatewayResponse"
    "fixture/AttachVpnGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachVpnGateway)

responseAuthorizeClientVpnIngress :: AuthorizeClientVpnIngressResponse -> TestTree
responseAuthorizeClientVpnIngress =
  res
    "AuthorizeClientVpnIngressResponse"
    "fixture/AuthorizeClientVpnIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeClientVpnIngress)

responseAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgressResponse -> TestTree
responseAuthorizeSecurityGroupEgress =
  res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeSecurityGroupEgress)

responseAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngressResponse -> TestTree
responseAuthorizeSecurityGroupIngress =
  res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeSecurityGroupIngress)

responseBundleInstance :: BundleInstanceResponse -> TestTree
responseBundleInstance =
  res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BundleInstance)

responseCancelBundleTask :: CancelBundleTaskResponse -> TestTree
responseCancelBundleTask =
  res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelBundleTask)

responseCancelCapacityReservation :: CancelCapacityReservationResponse -> TestTree
responseCancelCapacityReservation =
  res
    "CancelCapacityReservationResponse"
    "fixture/CancelCapacityReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCapacityReservation)

responseCancelCapacityReservationFleets :: CancelCapacityReservationFleetsResponse -> TestTree
responseCancelCapacityReservationFleets =
  res
    "CancelCapacityReservationFleetsResponse"
    "fixture/CancelCapacityReservationFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCapacityReservationFleets)

responseCancelConversionTask :: CancelConversionTaskResponse -> TestTree
responseCancelConversionTask =
  res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelConversionTask)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelExportTask)

responseCancelImageLaunchPermission :: CancelImageLaunchPermissionResponse -> TestTree
responseCancelImageLaunchPermission =
  res
    "CancelImageLaunchPermissionResponse"
    "fixture/CancelImageLaunchPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelImageLaunchPermission)

responseCancelImportTask :: CancelImportTaskResponse -> TestTree
responseCancelImportTask =
  res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelImportTask)

responseCancelReservedInstancesListing :: CancelReservedInstancesListingResponse -> TestTree
responseCancelReservedInstancesListing =
  res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelReservedInstancesListing)

responseCancelSpotFleetRequests :: CancelSpotFleetRequestsResponse -> TestTree
responseCancelSpotFleetRequests =
  res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSpotFleetRequests)

responseCancelSpotInstanceRequests :: CancelSpotInstanceRequestsResponse -> TestTree
responseCancelSpotInstanceRequests =
  res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSpotInstanceRequests)

responseConfirmProductInstance :: ConfirmProductInstanceResponse -> TestTree
responseConfirmProductInstance =
  res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmProductInstance)

responseCopyFpgaImage :: CopyFpgaImageResponse -> TestTree
responseCopyFpgaImage =
  res
    "CopyFpgaImageResponse"
    "fixture/CopyFpgaImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyFpgaImage)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyImage)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopySnapshot)

responseCreateCapacityReservation :: CreateCapacityReservationResponse -> TestTree
responseCreateCapacityReservation =
  res
    "CreateCapacityReservationResponse"
    "fixture/CreateCapacityReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCapacityReservation)

responseCreateCapacityReservationFleet :: CreateCapacityReservationFleetResponse -> TestTree
responseCreateCapacityReservationFleet =
  res
    "CreateCapacityReservationFleetResponse"
    "fixture/CreateCapacityReservationFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCapacityReservationFleet)

responseCreateCarrierGateway :: CreateCarrierGatewayResponse -> TestTree
responseCreateCarrierGateway =
  res
    "CreateCarrierGatewayResponse"
    "fixture/CreateCarrierGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCarrierGateway)

responseCreateClientVpnEndpoint :: CreateClientVpnEndpointResponse -> TestTree
responseCreateClientVpnEndpoint =
  res
    "CreateClientVpnEndpointResponse"
    "fixture/CreateClientVpnEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClientVpnEndpoint)

responseCreateClientVpnRoute :: CreateClientVpnRouteResponse -> TestTree
responseCreateClientVpnRoute =
  res
    "CreateClientVpnRouteResponse"
    "fixture/CreateClientVpnRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClientVpnRoute)

responseCreateCoipCidr :: CreateCoipCidrResponse -> TestTree
responseCreateCoipCidr =
  res
    "CreateCoipCidrResponse"
    "fixture/CreateCoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCoipCidr)

responseCreateCoipPool :: CreateCoipPoolResponse -> TestTree
responseCreateCoipPool =
  res
    "CreateCoipPoolResponse"
    "fixture/CreateCoipPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCoipPool)

responseCreateCustomerGateway :: CreateCustomerGatewayResponse -> TestTree
responseCreateCustomerGateway =
  res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomerGateway)

responseCreateDefaultSubnet :: CreateDefaultSubnetResponse -> TestTree
responseCreateDefaultSubnet =
  res
    "CreateDefaultSubnetResponse"
    "fixture/CreateDefaultSubnetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDefaultSubnet)

responseCreateDefaultVpc :: CreateDefaultVpcResponse -> TestTree
responseCreateDefaultVpc =
  res
    "CreateDefaultVpcResponse"
    "fixture/CreateDefaultVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDefaultVpc)

responseCreateDhcpOptions :: CreateDhcpOptionsResponse -> TestTree
responseCreateDhcpOptions =
  res
    "CreateDhcpOptionsResponse"
    "fixture/CreateDhcpOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDhcpOptions)

responseCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGatewayResponse -> TestTree
responseCreateEgressOnlyInternetGateway =
  res
    "CreateEgressOnlyInternetGatewayResponse"
    "fixture/CreateEgressOnlyInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEgressOnlyInternetGateway)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseCreateFlowLogs :: CreateFlowLogsResponse -> TestTree
responseCreateFlowLogs =
  res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlowLogs)

responseCreateFpgaImage :: CreateFpgaImageResponse -> TestTree
responseCreateFpgaImage =
  res
    "CreateFpgaImageResponse"
    "fixture/CreateFpgaImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFpgaImage)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImage)

responseCreateInstanceEventWindow :: CreateInstanceEventWindowResponse -> TestTree
responseCreateInstanceEventWindow =
  res
    "CreateInstanceEventWindowResponse"
    "fixture/CreateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceEventWindow)

responseCreateInstanceExportTask :: CreateInstanceExportTaskResponse -> TestTree
responseCreateInstanceExportTask =
  res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceExportTask)

responseCreateInternetGateway :: CreateInternetGatewayResponse -> TestTree
responseCreateInternetGateway =
  res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInternetGateway)

responseCreateIpam :: CreateIpamResponse -> TestTree
responseCreateIpam =
  res
    "CreateIpamResponse"
    "fixture/CreateIpamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIpam)

responseCreateIpamPool :: CreateIpamPoolResponse -> TestTree
responseCreateIpamPool =
  res
    "CreateIpamPoolResponse"
    "fixture/CreateIpamPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIpamPool)

responseCreateIpamScope :: CreateIpamScopeResponse -> TestTree
responseCreateIpamScope =
  res
    "CreateIpamScopeResponse"
    "fixture/CreateIpamScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIpamScope)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeyPair)

responseCreateLaunchTemplate :: CreateLaunchTemplateResponse -> TestTree
responseCreateLaunchTemplate =
  res
    "CreateLaunchTemplateResponse"
    "fixture/CreateLaunchTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunchTemplate)

responseCreateLaunchTemplateVersion :: CreateLaunchTemplateVersionResponse -> TestTree
responseCreateLaunchTemplateVersion =
  res
    "CreateLaunchTemplateVersionResponse"
    "fixture/CreateLaunchTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunchTemplateVersion)

responseCreateLocalGatewayRoute :: CreateLocalGatewayRouteResponse -> TestTree
responseCreateLocalGatewayRoute =
  res
    "CreateLocalGatewayRouteResponse"
    "fixture/CreateLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocalGatewayRoute)

responseCreateLocalGatewayRouteTable :: CreateLocalGatewayRouteTableResponse -> TestTree
responseCreateLocalGatewayRouteTable =
  res
    "CreateLocalGatewayRouteTableResponse"
    "fixture/CreateLocalGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocalGatewayRouteTable)

responseCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation :: CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse -> TestTree
responseCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
  res
    "CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse"
    "fixture/CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation)

responseCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseCreateLocalGatewayRouteTableVpcAssociation =
  res
    "CreateLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/CreateLocalGatewayRouteTableVpcAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocalGatewayRouteTableVpcAssociation)

responseCreateManagedPrefixList :: CreateManagedPrefixListResponse -> TestTree
responseCreateManagedPrefixList =
  res
    "CreateManagedPrefixListResponse"
    "fixture/CreateManagedPrefixListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateManagedPrefixList)

responseCreateNatGateway :: CreateNatGatewayResponse -> TestTree
responseCreateNatGateway =
  res
    "CreateNatGatewayResponse"
    "fixture/CreateNatGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNatGateway)

responseCreateNetworkAcl :: CreateNetworkAclResponse -> TestTree
responseCreateNetworkAcl =
  res
    "CreateNetworkAclResponse"
    "fixture/CreateNetworkAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkAcl)

responseCreateNetworkAclEntry :: CreateNetworkAclEntryResponse -> TestTree
responseCreateNetworkAclEntry =
  res
    "CreateNetworkAclEntryResponse"
    "fixture/CreateNetworkAclEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkAclEntry)

responseCreateNetworkInsightsAccessScope :: CreateNetworkInsightsAccessScopeResponse -> TestTree
responseCreateNetworkInsightsAccessScope =
  res
    "CreateNetworkInsightsAccessScopeResponse"
    "fixture/CreateNetworkInsightsAccessScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkInsightsAccessScope)

responseCreateNetworkInsightsPath :: CreateNetworkInsightsPathResponse -> TestTree
responseCreateNetworkInsightsPath =
  res
    "CreateNetworkInsightsPathResponse"
    "fixture/CreateNetworkInsightsPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkInsightsPath)

responseCreateNetworkInterface :: CreateNetworkInterfaceResponse -> TestTree
responseCreateNetworkInterface =
  res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkInterface)

responseCreateNetworkInterfacePermission :: CreateNetworkInterfacePermissionResponse -> TestTree
responseCreateNetworkInterfacePermission =
  res
    "CreateNetworkInterfacePermissionResponse"
    "fixture/CreateNetworkInterfacePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkInterfacePermission)

responseCreatePlacementGroup :: CreatePlacementGroupResponse -> TestTree
responseCreatePlacementGroup =
  res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlacementGroup)

responseCreatePublicIpv4Pool :: CreatePublicIpv4PoolResponse -> TestTree
responseCreatePublicIpv4Pool =
  res
    "CreatePublicIpv4PoolResponse"
    "fixture/CreatePublicIpv4PoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePublicIpv4Pool)

responseCreateReplaceRootVolumeTask :: CreateReplaceRootVolumeTaskResponse -> TestTree
responseCreateReplaceRootVolumeTask =
  res
    "CreateReplaceRootVolumeTaskResponse"
    "fixture/CreateReplaceRootVolumeTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplaceRootVolumeTask)

responseCreateReservedInstancesListing :: CreateReservedInstancesListingResponse -> TestTree
responseCreateReservedInstancesListing =
  res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReservedInstancesListing)

responseCreateRestoreImageTask :: CreateRestoreImageTaskResponse -> TestTree
responseCreateRestoreImageTask =
  res
    "CreateRestoreImageTaskResponse"
    "fixture/CreateRestoreImageTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRestoreImageTask)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoute)

responseCreateRouteTable :: CreateRouteTableResponse -> TestTree
responseCreateRouteTable =
  res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRouteTable)

responseCreateSecurityGroup :: CreateSecurityGroupResponse -> TestTree
responseCreateSecurityGroup =
  res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityGroup)

responseCreateSnapshot :: Snapshot -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateSnapshots :: CreateSnapshotsResponse -> TestTree
responseCreateSnapshots =
  res
    "CreateSnapshotsResponse"
    "fixture/CreateSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshots)

responseCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscriptionResponse -> TestTree
responseCreateSpotDatafeedSubscription =
  res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSpotDatafeedSubscription)

responseCreateStoreImageTask :: CreateStoreImageTaskResponse -> TestTree
responseCreateStoreImageTask =
  res
    "CreateStoreImageTaskResponse"
    "fixture/CreateStoreImageTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStoreImageTask)

responseCreateSubnet :: CreateSubnetResponse -> TestTree
responseCreateSubnet =
  res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubnet)

responseCreateSubnetCidrReservation :: CreateSubnetCidrReservationResponse -> TestTree
responseCreateSubnetCidrReservation =
  res
    "CreateSubnetCidrReservationResponse"
    "fixture/CreateSubnetCidrReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubnetCidrReservation)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responseCreateTrafficMirrorFilter :: CreateTrafficMirrorFilterResponse -> TestTree
responseCreateTrafficMirrorFilter =
  res
    "CreateTrafficMirrorFilterResponse"
    "fixture/CreateTrafficMirrorFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficMirrorFilter)

responseCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRuleResponse -> TestTree
responseCreateTrafficMirrorFilterRule =
  res
    "CreateTrafficMirrorFilterRuleResponse"
    "fixture/CreateTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficMirrorFilterRule)

responseCreateTrafficMirrorSession :: CreateTrafficMirrorSessionResponse -> TestTree
responseCreateTrafficMirrorSession =
  res
    "CreateTrafficMirrorSessionResponse"
    "fixture/CreateTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficMirrorSession)

responseCreateTrafficMirrorTarget :: CreateTrafficMirrorTargetResponse -> TestTree
responseCreateTrafficMirrorTarget =
  res
    "CreateTrafficMirrorTargetResponse"
    "fixture/CreateTrafficMirrorTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficMirrorTarget)

responseCreateTransitGateway :: CreateTransitGatewayResponse -> TestTree
responseCreateTransitGateway =
  res
    "CreateTransitGatewayResponse"
    "fixture/CreateTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGateway)

responseCreateTransitGatewayConnect :: CreateTransitGatewayConnectResponse -> TestTree
responseCreateTransitGatewayConnect =
  res
    "CreateTransitGatewayConnectResponse"
    "fixture/CreateTransitGatewayConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayConnect)

responseCreateTransitGatewayConnectPeer :: CreateTransitGatewayConnectPeerResponse -> TestTree
responseCreateTransitGatewayConnectPeer =
  res
    "CreateTransitGatewayConnectPeerResponse"
    "fixture/CreateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayConnectPeer)

responseCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomainResponse -> TestTree
responseCreateTransitGatewayMulticastDomain =
  res
    "CreateTransitGatewayMulticastDomainResponse"
    "fixture/CreateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayMulticastDomain)

responseCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachmentResponse -> TestTree
responseCreateTransitGatewayPeeringAttachment =
  res
    "CreateTransitGatewayPeeringAttachmentResponse"
    "fixture/CreateTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayPeeringAttachment)

responseCreateTransitGatewayPolicyTable :: CreateTransitGatewayPolicyTableResponse -> TestTree
responseCreateTransitGatewayPolicyTable =
  res
    "CreateTransitGatewayPolicyTableResponse"
    "fixture/CreateTransitGatewayPolicyTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayPolicyTable)

responseCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReferenceResponse -> TestTree
responseCreateTransitGatewayPrefixListReference =
  res
    "CreateTransitGatewayPrefixListReferenceResponse"
    "fixture/CreateTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayPrefixListReference)

responseCreateTransitGatewayRoute :: CreateTransitGatewayRouteResponse -> TestTree
responseCreateTransitGatewayRoute =
  res
    "CreateTransitGatewayRouteResponse"
    "fixture/CreateTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayRoute)

responseCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTableResponse -> TestTree
responseCreateTransitGatewayRouteTable =
  res
    "CreateTransitGatewayRouteTableResponse"
    "fixture/CreateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayRouteTable)

responseCreateTransitGatewayRouteTableAnnouncement :: CreateTransitGatewayRouteTableAnnouncementResponse -> TestTree
responseCreateTransitGatewayRouteTableAnnouncement =
  res
    "CreateTransitGatewayRouteTableAnnouncementResponse"
    "fixture/CreateTransitGatewayRouteTableAnnouncementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayRouteTableAnnouncement)

responseCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachmentResponse -> TestTree
responseCreateTransitGatewayVpcAttachment =
  res
    "CreateTransitGatewayVpcAttachmentResponse"
    "fixture/CreateTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayVpcAttachment)

responseCreateVerifiedAccessEndpoint :: CreateVerifiedAccessEndpointResponse -> TestTree
responseCreateVerifiedAccessEndpoint =
  res
    "CreateVerifiedAccessEndpointResponse"
    "fixture/CreateVerifiedAccessEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVerifiedAccessEndpoint)

responseCreateVerifiedAccessGroup :: CreateVerifiedAccessGroupResponse -> TestTree
responseCreateVerifiedAccessGroup =
  res
    "CreateVerifiedAccessGroupResponse"
    "fixture/CreateVerifiedAccessGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVerifiedAccessGroup)

responseCreateVerifiedAccessInstance :: CreateVerifiedAccessInstanceResponse -> TestTree
responseCreateVerifiedAccessInstance =
  res
    "CreateVerifiedAccessInstanceResponse"
    "fixture/CreateVerifiedAccessInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVerifiedAccessInstance)

responseCreateVerifiedAccessTrustProvider :: CreateVerifiedAccessTrustProviderResponse -> TestTree
responseCreateVerifiedAccessTrustProvider =
  res
    "CreateVerifiedAccessTrustProviderResponse"
    "fixture/CreateVerifiedAccessTrustProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVerifiedAccessTrustProvider)

responseCreateVolume :: Volume -> TestTree
responseCreateVolume =
  res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVolume)

responseCreateVpc :: CreateVpcResponse -> TestTree
responseCreateVpc =
  res
    "CreateVpcResponse"
    "fixture/CreateVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpc)

responseCreateVpcEndpoint :: CreateVpcEndpointResponse -> TestTree
responseCreateVpcEndpoint =
  res
    "CreateVpcEndpointResponse"
    "fixture/CreateVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpoint)

responseCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotificationResponse -> TestTree
responseCreateVpcEndpointConnectionNotification =
  res
    "CreateVpcEndpointConnectionNotificationResponse"
    "fixture/CreateVpcEndpointConnectionNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpointConnectionNotification)

responseCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfigurationResponse -> TestTree
responseCreateVpcEndpointServiceConfiguration =
  res
    "CreateVpcEndpointServiceConfigurationResponse"
    "fixture/CreateVpcEndpointServiceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpointServiceConfiguration)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcPeeringConnection)

responseCreateVpnConnection :: CreateVpnConnectionResponse -> TestTree
responseCreateVpnConnection =
  res
    "CreateVpnConnectionResponse"
    "fixture/CreateVpnConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpnConnection)

responseCreateVpnConnectionRoute :: CreateVpnConnectionRouteResponse -> TestTree
responseCreateVpnConnectionRoute =
  res
    "CreateVpnConnectionRouteResponse"
    "fixture/CreateVpnConnectionRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpnConnectionRoute)

responseCreateVpnGateway :: CreateVpnGatewayResponse -> TestTree
responseCreateVpnGateway =
  res
    "CreateVpnGatewayResponse"
    "fixture/CreateVpnGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpnGateway)

responseDeleteCarrierGateway :: DeleteCarrierGatewayResponse -> TestTree
responseDeleteCarrierGateway =
  res
    "DeleteCarrierGatewayResponse"
    "fixture/DeleteCarrierGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCarrierGateway)

responseDeleteClientVpnEndpoint :: DeleteClientVpnEndpointResponse -> TestTree
responseDeleteClientVpnEndpoint =
  res
    "DeleteClientVpnEndpointResponse"
    "fixture/DeleteClientVpnEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClientVpnEndpoint)

responseDeleteClientVpnRoute :: DeleteClientVpnRouteResponse -> TestTree
responseDeleteClientVpnRoute =
  res
    "DeleteClientVpnRouteResponse"
    "fixture/DeleteClientVpnRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClientVpnRoute)

responseDeleteCoipCidr :: DeleteCoipCidrResponse -> TestTree
responseDeleteCoipCidr =
  res
    "DeleteCoipCidrResponse"
    "fixture/DeleteCoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCoipCidr)

responseDeleteCoipPool :: DeleteCoipPoolResponse -> TestTree
responseDeleteCoipPool =
  res
    "DeleteCoipPoolResponse"
    "fixture/DeleteCoipPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCoipPool)

responseDeleteCustomerGateway :: DeleteCustomerGatewayResponse -> TestTree
responseDeleteCustomerGateway =
  res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomerGateway)

responseDeleteDhcpOptions :: DeleteDhcpOptionsResponse -> TestTree
responseDeleteDhcpOptions =
  res
    "DeleteDhcpOptionsResponse"
    "fixture/DeleteDhcpOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDhcpOptions)

responseDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGatewayResponse -> TestTree
responseDeleteEgressOnlyInternetGateway =
  res
    "DeleteEgressOnlyInternetGatewayResponse"
    "fixture/DeleteEgressOnlyInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEgressOnlyInternetGateway)

responseDeleteFleets :: DeleteFleetsResponse -> TestTree
responseDeleteFleets =
  res
    "DeleteFleetsResponse"
    "fixture/DeleteFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleets)

responseDeleteFlowLogs :: DeleteFlowLogsResponse -> TestTree
responseDeleteFlowLogs =
  res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlowLogs)

responseDeleteFpgaImage :: DeleteFpgaImageResponse -> TestTree
responseDeleteFpgaImage =
  res
    "DeleteFpgaImageResponse"
    "fixture/DeleteFpgaImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFpgaImage)

responseDeleteInstanceEventWindow :: DeleteInstanceEventWindowResponse -> TestTree
responseDeleteInstanceEventWindow =
  res
    "DeleteInstanceEventWindowResponse"
    "fixture/DeleteInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceEventWindow)

responseDeleteInternetGateway :: DeleteInternetGatewayResponse -> TestTree
responseDeleteInternetGateway =
  res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInternetGateway)

responseDeleteIpam :: DeleteIpamResponse -> TestTree
responseDeleteIpam =
  res
    "DeleteIpamResponse"
    "fixture/DeleteIpamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIpam)

responseDeleteIpamPool :: DeleteIpamPoolResponse -> TestTree
responseDeleteIpamPool =
  res
    "DeleteIpamPoolResponse"
    "fixture/DeleteIpamPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIpamPool)

responseDeleteIpamScope :: DeleteIpamScopeResponse -> TestTree
responseDeleteIpamScope =
  res
    "DeleteIpamScopeResponse"
    "fixture/DeleteIpamScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIpamScope)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeyPair)

responseDeleteLaunchTemplate :: DeleteLaunchTemplateResponse -> TestTree
responseDeleteLaunchTemplate =
  res
    "DeleteLaunchTemplateResponse"
    "fixture/DeleteLaunchTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchTemplate)

responseDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> TestTree
responseDeleteLaunchTemplateVersions =
  res
    "DeleteLaunchTemplateVersionsResponse"
    "fixture/DeleteLaunchTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchTemplateVersions)

responseDeleteLocalGatewayRoute :: DeleteLocalGatewayRouteResponse -> TestTree
responseDeleteLocalGatewayRoute =
  res
    "DeleteLocalGatewayRouteResponse"
    "fixture/DeleteLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocalGatewayRoute)

responseDeleteLocalGatewayRouteTable :: DeleteLocalGatewayRouteTableResponse -> TestTree
responseDeleteLocalGatewayRouteTable =
  res
    "DeleteLocalGatewayRouteTableResponse"
    "fixture/DeleteLocalGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocalGatewayRouteTable)

responseDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation :: DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse -> TestTree
responseDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
  res
    "DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse"
    "fixture/DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation)

responseDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseDeleteLocalGatewayRouteTableVpcAssociation =
  res
    "DeleteLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocalGatewayRouteTableVpcAssociation)

responseDeleteManagedPrefixList :: DeleteManagedPrefixListResponse -> TestTree
responseDeleteManagedPrefixList =
  res
    "DeleteManagedPrefixListResponse"
    "fixture/DeleteManagedPrefixListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteManagedPrefixList)

responseDeleteNatGateway :: DeleteNatGatewayResponse -> TestTree
responseDeleteNatGateway =
  res
    "DeleteNatGatewayResponse"
    "fixture/DeleteNatGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNatGateway)

responseDeleteNetworkAcl :: DeleteNetworkAclResponse -> TestTree
responseDeleteNetworkAcl =
  res
    "DeleteNetworkAclResponse"
    "fixture/DeleteNetworkAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkAcl)

responseDeleteNetworkAclEntry :: DeleteNetworkAclEntryResponse -> TestTree
responseDeleteNetworkAclEntry =
  res
    "DeleteNetworkAclEntryResponse"
    "fixture/DeleteNetworkAclEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkAclEntry)

responseDeleteNetworkInsightsAccessScope :: DeleteNetworkInsightsAccessScopeResponse -> TestTree
responseDeleteNetworkInsightsAccessScope =
  res
    "DeleteNetworkInsightsAccessScopeResponse"
    "fixture/DeleteNetworkInsightsAccessScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInsightsAccessScope)

responseDeleteNetworkInsightsAccessScopeAnalysis :: DeleteNetworkInsightsAccessScopeAnalysisResponse -> TestTree
responseDeleteNetworkInsightsAccessScopeAnalysis =
  res
    "DeleteNetworkInsightsAccessScopeAnalysisResponse"
    "fixture/DeleteNetworkInsightsAccessScopeAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInsightsAccessScopeAnalysis)

responseDeleteNetworkInsightsAnalysis :: DeleteNetworkInsightsAnalysisResponse -> TestTree
responseDeleteNetworkInsightsAnalysis =
  res
    "DeleteNetworkInsightsAnalysisResponse"
    "fixture/DeleteNetworkInsightsAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInsightsAnalysis)

responseDeleteNetworkInsightsPath :: DeleteNetworkInsightsPathResponse -> TestTree
responseDeleteNetworkInsightsPath =
  res
    "DeleteNetworkInsightsPathResponse"
    "fixture/DeleteNetworkInsightsPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInsightsPath)

responseDeleteNetworkInterface :: DeleteNetworkInterfaceResponse -> TestTree
responseDeleteNetworkInterface =
  res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInterface)

responseDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermissionResponse -> TestTree
responseDeleteNetworkInterfacePermission =
  res
    "DeleteNetworkInterfacePermissionResponse"
    "fixture/DeleteNetworkInterfacePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkInterfacePermission)

responseDeletePlacementGroup :: DeletePlacementGroupResponse -> TestTree
responseDeletePlacementGroup =
  res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlacementGroup)

responseDeletePublicIpv4Pool :: DeletePublicIpv4PoolResponse -> TestTree
responseDeletePublicIpv4Pool =
  res
    "DeletePublicIpv4PoolResponse"
    "fixture/DeletePublicIpv4PoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePublicIpv4Pool)

responseDeleteQueuedReservedInstances :: DeleteQueuedReservedInstancesResponse -> TestTree
responseDeleteQueuedReservedInstances =
  res
    "DeleteQueuedReservedInstancesResponse"
    "fixture/DeleteQueuedReservedInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueuedReservedInstances)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoute)

responseDeleteRouteTable :: DeleteRouteTableResponse -> TestTree
responseDeleteRouteTable =
  res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteTable)

responseDeleteSecurityGroup :: DeleteSecurityGroupResponse -> TestTree
responseDeleteSecurityGroup =
  res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityGroup)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
responseDeleteSpotDatafeedSubscription =
  res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSpotDatafeedSubscription)

responseDeleteSubnet :: DeleteSubnetResponse -> TestTree
responseDeleteSubnet =
  res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubnet)

responseDeleteSubnetCidrReservation :: DeleteSubnetCidrReservationResponse -> TestTree
responseDeleteSubnetCidrReservation =
  res
    "DeleteSubnetCidrReservationResponse"
    "fixture/DeleteSubnetCidrReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubnetCidrReservation)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilterResponse -> TestTree
responseDeleteTrafficMirrorFilter =
  res
    "DeleteTrafficMirrorFilterResponse"
    "fixture/DeleteTrafficMirrorFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficMirrorFilter)

responseDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRuleResponse -> TestTree
responseDeleteTrafficMirrorFilterRule =
  res
    "DeleteTrafficMirrorFilterRuleResponse"
    "fixture/DeleteTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficMirrorFilterRule)

responseDeleteTrafficMirrorSession :: DeleteTrafficMirrorSessionResponse -> TestTree
responseDeleteTrafficMirrorSession =
  res
    "DeleteTrafficMirrorSessionResponse"
    "fixture/DeleteTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficMirrorSession)

responseDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTargetResponse -> TestTree
responseDeleteTrafficMirrorTarget =
  res
    "DeleteTrafficMirrorTargetResponse"
    "fixture/DeleteTrafficMirrorTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficMirrorTarget)

responseDeleteTransitGateway :: DeleteTransitGatewayResponse -> TestTree
responseDeleteTransitGateway =
  res
    "DeleteTransitGatewayResponse"
    "fixture/DeleteTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGateway)

responseDeleteTransitGatewayConnect :: DeleteTransitGatewayConnectResponse -> TestTree
responseDeleteTransitGatewayConnect =
  res
    "DeleteTransitGatewayConnectResponse"
    "fixture/DeleteTransitGatewayConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayConnect)

responseDeleteTransitGatewayConnectPeer :: DeleteTransitGatewayConnectPeerResponse -> TestTree
responseDeleteTransitGatewayConnectPeer =
  res
    "DeleteTransitGatewayConnectPeerResponse"
    "fixture/DeleteTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayConnectPeer)

responseDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomainResponse -> TestTree
responseDeleteTransitGatewayMulticastDomain =
  res
    "DeleteTransitGatewayMulticastDomainResponse"
    "fixture/DeleteTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayMulticastDomain)

responseDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachmentResponse -> TestTree
responseDeleteTransitGatewayPeeringAttachment =
  res
    "DeleteTransitGatewayPeeringAttachmentResponse"
    "fixture/DeleteTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayPeeringAttachment)

responseDeleteTransitGatewayPolicyTable :: DeleteTransitGatewayPolicyTableResponse -> TestTree
responseDeleteTransitGatewayPolicyTable =
  res
    "DeleteTransitGatewayPolicyTableResponse"
    "fixture/DeleteTransitGatewayPolicyTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayPolicyTable)

responseDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReferenceResponse -> TestTree
responseDeleteTransitGatewayPrefixListReference =
  res
    "DeleteTransitGatewayPrefixListReferenceResponse"
    "fixture/DeleteTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayPrefixListReference)

responseDeleteTransitGatewayRoute :: DeleteTransitGatewayRouteResponse -> TestTree
responseDeleteTransitGatewayRoute =
  res
    "DeleteTransitGatewayRouteResponse"
    "fixture/DeleteTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayRoute)

responseDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTableResponse -> TestTree
responseDeleteTransitGatewayRouteTable =
  res
    "DeleteTransitGatewayRouteTableResponse"
    "fixture/DeleteTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayRouteTable)

responseDeleteTransitGatewayRouteTableAnnouncement :: DeleteTransitGatewayRouteTableAnnouncementResponse -> TestTree
responseDeleteTransitGatewayRouteTableAnnouncement =
  res
    "DeleteTransitGatewayRouteTableAnnouncementResponse"
    "fixture/DeleteTransitGatewayRouteTableAnnouncementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayRouteTableAnnouncement)

responseDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachmentResponse -> TestTree
responseDeleteTransitGatewayVpcAttachment =
  res
    "DeleteTransitGatewayVpcAttachmentResponse"
    "fixture/DeleteTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTransitGatewayVpcAttachment)

responseDeleteVerifiedAccessEndpoint :: DeleteVerifiedAccessEndpointResponse -> TestTree
responseDeleteVerifiedAccessEndpoint =
  res
    "DeleteVerifiedAccessEndpointResponse"
    "fixture/DeleteVerifiedAccessEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVerifiedAccessEndpoint)

responseDeleteVerifiedAccessGroup :: DeleteVerifiedAccessGroupResponse -> TestTree
responseDeleteVerifiedAccessGroup =
  res
    "DeleteVerifiedAccessGroupResponse"
    "fixture/DeleteVerifiedAccessGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVerifiedAccessGroup)

responseDeleteVerifiedAccessInstance :: DeleteVerifiedAccessInstanceResponse -> TestTree
responseDeleteVerifiedAccessInstance =
  res
    "DeleteVerifiedAccessInstanceResponse"
    "fixture/DeleteVerifiedAccessInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVerifiedAccessInstance)

responseDeleteVerifiedAccessTrustProvider :: DeleteVerifiedAccessTrustProviderResponse -> TestTree
responseDeleteVerifiedAccessTrustProvider =
  res
    "DeleteVerifiedAccessTrustProviderResponse"
    "fixture/DeleteVerifiedAccessTrustProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVerifiedAccessTrustProvider)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVolume)

responseDeleteVpc :: DeleteVpcResponse -> TestTree
responseDeleteVpc =
  res
    "DeleteVpcResponse"
    "fixture/DeleteVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpc)

responseDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotificationsResponse -> TestTree
responseDeleteVpcEndpointConnectionNotifications =
  res
    "DeleteVpcEndpointConnectionNotificationsResponse"
    "fixture/DeleteVpcEndpointConnectionNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcEndpointConnectionNotifications)

responseDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurationsResponse -> TestTree
responseDeleteVpcEndpointServiceConfigurations =
  res
    "DeleteVpcEndpointServiceConfigurationsResponse"
    "fixture/DeleteVpcEndpointServiceConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcEndpointServiceConfigurations)

responseDeleteVpcEndpoints :: DeleteVpcEndpointsResponse -> TestTree
responseDeleteVpcEndpoints =
  res
    "DeleteVpcEndpointsResponse"
    "fixture/DeleteVpcEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcEndpoints)

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection =
  res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcPeeringConnection)

responseDeleteVpnConnection :: DeleteVpnConnectionResponse -> TestTree
responseDeleteVpnConnection =
  res
    "DeleteVpnConnectionResponse"
    "fixture/DeleteVpnConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpnConnection)

responseDeleteVpnConnectionRoute :: DeleteVpnConnectionRouteResponse -> TestTree
responseDeleteVpnConnectionRoute =
  res
    "DeleteVpnConnectionRouteResponse"
    "fixture/DeleteVpnConnectionRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpnConnectionRoute)

responseDeleteVpnGateway :: DeleteVpnGatewayResponse -> TestTree
responseDeleteVpnGateway =
  res
    "DeleteVpnGatewayResponse"
    "fixture/DeleteVpnGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpnGateway)

responseDeprovisionByoipCidr :: DeprovisionByoipCidrResponse -> TestTree
responseDeprovisionByoipCidr =
  res
    "DeprovisionByoipCidrResponse"
    "fixture/DeprovisionByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprovisionByoipCidr)

responseDeprovisionIpamPoolCidr :: DeprovisionIpamPoolCidrResponse -> TestTree
responseDeprovisionIpamPoolCidr =
  res
    "DeprovisionIpamPoolCidrResponse"
    "fixture/DeprovisionIpamPoolCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprovisionIpamPoolCidr)

responseDeprovisionPublicIpv4PoolCidr :: DeprovisionPublicIpv4PoolCidrResponse -> TestTree
responseDeprovisionPublicIpv4PoolCidr =
  res
    "DeprovisionPublicIpv4PoolCidrResponse"
    "fixture/DeprovisionPublicIpv4PoolCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprovisionPublicIpv4PoolCidr)

responseDeregisterImage :: DeregisterImageResponse -> TestTree
responseDeregisterImage =
  res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterImage)

responseDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributesResponse -> TestTree
responseDeregisterInstanceEventNotificationAttributes =
  res
    "DeregisterInstanceEventNotificationAttributesResponse"
    "fixture/DeregisterInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterInstanceEventNotificationAttributes)

responseDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupMembers =
  res
    "DeregisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTransitGatewayMulticastGroupMembers)

responseDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupSources =
  res
    "DeregisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTransitGatewayMulticastGroupSources)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseDescribeAddressTransfers :: DescribeAddressTransfersResponse -> TestTree
responseDescribeAddressTransfers =
  res
    "DescribeAddressTransfersResponse"
    "fixture/DescribeAddressTransfersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddressTransfers)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddresses)

responseDescribeAddressesAttribute :: DescribeAddressesAttributeResponse -> TestTree
responseDescribeAddressesAttribute =
  res
    "DescribeAddressesAttributeResponse"
    "fixture/DescribeAddressesAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddressesAttribute)

responseDescribeAggregateIdFormat :: DescribeAggregateIdFormatResponse -> TestTree
responseDescribeAggregateIdFormat =
  res
    "DescribeAggregateIdFormatResponse"
    "fixture/DescribeAggregateIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAggregateIdFormat)

responseDescribeAvailabilityZones :: DescribeAvailabilityZonesResponse -> TestTree
responseDescribeAvailabilityZones =
  res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAvailabilityZones)

responseDescribeAwsNetworkPerformanceMetricSubscriptions :: DescribeAwsNetworkPerformanceMetricSubscriptionsResponse -> TestTree
responseDescribeAwsNetworkPerformanceMetricSubscriptions =
  res
    "DescribeAwsNetworkPerformanceMetricSubscriptionsResponse"
    "fixture/DescribeAwsNetworkPerformanceMetricSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAwsNetworkPerformanceMetricSubscriptions)

responseDescribeBundleTasks :: DescribeBundleTasksResponse -> TestTree
responseDescribeBundleTasks =
  res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBundleTasks)

responseDescribeByoipCidrs :: DescribeByoipCidrsResponse -> TestTree
responseDescribeByoipCidrs =
  res
    "DescribeByoipCidrsResponse"
    "fixture/DescribeByoipCidrsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeByoipCidrs)

responseDescribeCapacityReservationFleets :: DescribeCapacityReservationFleetsResponse -> TestTree
responseDescribeCapacityReservationFleets =
  res
    "DescribeCapacityReservationFleetsResponse"
    "fixture/DescribeCapacityReservationFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCapacityReservationFleets)

responseDescribeCapacityReservations :: DescribeCapacityReservationsResponse -> TestTree
responseDescribeCapacityReservations =
  res
    "DescribeCapacityReservationsResponse"
    "fixture/DescribeCapacityReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCapacityReservations)

responseDescribeCarrierGateways :: DescribeCarrierGatewaysResponse -> TestTree
responseDescribeCarrierGateways =
  res
    "DescribeCarrierGatewaysResponse"
    "fixture/DescribeCarrierGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCarrierGateways)

responseDescribeClassicLinkInstances :: DescribeClassicLinkInstancesResponse -> TestTree
responseDescribeClassicLinkInstances =
  res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClassicLinkInstances)

responseDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRulesResponse -> TestTree
responseDescribeClientVpnAuthorizationRules =
  res
    "DescribeClientVpnAuthorizationRulesResponse"
    "fixture/DescribeClientVpnAuthorizationRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnAuthorizationRules)

responseDescribeClientVpnConnections :: DescribeClientVpnConnectionsResponse -> TestTree
responseDescribeClientVpnConnections =
  res
    "DescribeClientVpnConnectionsResponse"
    "fixture/DescribeClientVpnConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnConnections)

responseDescribeClientVpnEndpoints :: DescribeClientVpnEndpointsResponse -> TestTree
responseDescribeClientVpnEndpoints =
  res
    "DescribeClientVpnEndpointsResponse"
    "fixture/DescribeClientVpnEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnEndpoints)

responseDescribeClientVpnRoutes :: DescribeClientVpnRoutesResponse -> TestTree
responseDescribeClientVpnRoutes =
  res
    "DescribeClientVpnRoutesResponse"
    "fixture/DescribeClientVpnRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnRoutes)

responseDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworksResponse -> TestTree
responseDescribeClientVpnTargetNetworks =
  res
    "DescribeClientVpnTargetNetworksResponse"
    "fixture/DescribeClientVpnTargetNetworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientVpnTargetNetworks)

responseDescribeCoipPools :: DescribeCoipPoolsResponse -> TestTree
responseDescribeCoipPools =
  res
    "DescribeCoipPoolsResponse"
    "fixture/DescribeCoipPoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCoipPools)

responseDescribeConversionTasks :: DescribeConversionTasksResponse -> TestTree
responseDescribeConversionTasks =
  res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConversionTasks)

responseDescribeCustomerGateways :: DescribeCustomerGatewaysResponse -> TestTree
responseDescribeCustomerGateways =
  res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomerGateways)

responseDescribeDhcpOptions :: DescribeDhcpOptionsResponse -> TestTree
responseDescribeDhcpOptions =
  res
    "DescribeDhcpOptionsResponse"
    "fixture/DescribeDhcpOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDhcpOptions)

responseDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGatewaysResponse -> TestTree
responseDescribeEgressOnlyInternetGateways =
  res
    "DescribeEgressOnlyInternetGatewaysResponse"
    "fixture/DescribeEgressOnlyInternetGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEgressOnlyInternetGateways)

responseDescribeElasticGpus :: DescribeElasticGpusResponse -> TestTree
responseDescribeElasticGpus =
  res
    "DescribeElasticGpusResponse"
    "fixture/DescribeElasticGpusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticGpus)

responseDescribeExportImageTasks :: DescribeExportImageTasksResponse -> TestTree
responseDescribeExportImageTasks =
  res
    "DescribeExportImageTasksResponse"
    "fixture/DescribeExportImageTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExportImageTasks)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExportTasks)

responseDescribeFastLaunchImages :: DescribeFastLaunchImagesResponse -> TestTree
responseDescribeFastLaunchImages =
  res
    "DescribeFastLaunchImagesResponse"
    "fixture/DescribeFastLaunchImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFastLaunchImages)

responseDescribeFastSnapshotRestores :: DescribeFastSnapshotRestoresResponse -> TestTree
responseDescribeFastSnapshotRestores =
  res
    "DescribeFastSnapshotRestoresResponse"
    "fixture/DescribeFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFastSnapshotRestores)

responseDescribeFleetHistory :: DescribeFleetHistoryResponse -> TestTree
responseDescribeFleetHistory =
  res
    "DescribeFleetHistoryResponse"
    "fixture/DescribeFleetHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetHistory)

responseDescribeFleetInstances :: DescribeFleetInstancesResponse -> TestTree
responseDescribeFleetInstances =
  res
    "DescribeFleetInstancesResponse"
    "fixture/DescribeFleetInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetInstances)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleets)

responseDescribeFlowLogs :: DescribeFlowLogsResponse -> TestTree
responseDescribeFlowLogs =
  res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlowLogs)

responseDescribeFpgaImageAttribute :: DescribeFpgaImageAttributeResponse -> TestTree
responseDescribeFpgaImageAttribute =
  res
    "DescribeFpgaImageAttributeResponse"
    "fixture/DescribeFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFpgaImageAttribute)

responseDescribeFpgaImages :: DescribeFpgaImagesResponse -> TestTree
responseDescribeFpgaImages =
  res
    "DescribeFpgaImagesResponse"
    "fixture/DescribeFpgaImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFpgaImages)

responseDescribeHostReservationOfferings :: DescribeHostReservationOfferingsResponse -> TestTree
responseDescribeHostReservationOfferings =
  res
    "DescribeHostReservationOfferingsResponse"
    "fixture/DescribeHostReservationOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHostReservationOfferings)

responseDescribeHostReservations :: DescribeHostReservationsResponse -> TestTree
responseDescribeHostReservations =
  res
    "DescribeHostReservationsResponse"
    "fixture/DescribeHostReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHostReservations)

responseDescribeHosts :: DescribeHostsResponse -> TestTree
responseDescribeHosts =
  res
    "DescribeHostsResponse"
    "fixture/DescribeHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHosts)

responseDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociationsResponse -> TestTree
responseDescribeIamInstanceProfileAssociations =
  res
    "DescribeIamInstanceProfileAssociationsResponse"
    "fixture/DescribeIamInstanceProfileAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIamInstanceProfileAssociations)

responseDescribeIdFormat :: DescribeIdFormatResponse -> TestTree
responseDescribeIdFormat =
  res
    "DescribeIdFormatResponse"
    "fixture/DescribeIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdFormat)

responseDescribeIdentityIdFormat :: DescribeIdentityIdFormatResponse -> TestTree
responseDescribeIdentityIdFormat =
  res
    "DescribeIdentityIdFormatResponse"
    "fixture/DescribeIdentityIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityIdFormat)

responseDescribeImageAttribute :: DescribeImageAttributeResponse -> TestTree
responseDescribeImageAttribute =
  res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageAttribute)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImages)

responseDescribeImportImageTasks :: DescribeImportImageTasksResponse -> TestTree
responseDescribeImportImageTasks =
  res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImportImageTasks)

responseDescribeImportSnapshotTasks :: DescribeImportSnapshotTasksResponse -> TestTree
responseDescribeImportSnapshotTasks =
  res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImportSnapshotTasks)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceAttribute)

responseDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecificationsResponse -> TestTree
responseDescribeInstanceCreditSpecifications =
  res
    "DescribeInstanceCreditSpecificationsResponse"
    "fixture/DescribeInstanceCreditSpecificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceCreditSpecifications)

responseDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributesResponse -> TestTree
responseDescribeInstanceEventNotificationAttributes =
  res
    "DescribeInstanceEventNotificationAttributesResponse"
    "fixture/DescribeInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceEventNotificationAttributes)

responseDescribeInstanceEventWindows :: DescribeInstanceEventWindowsResponse -> TestTree
responseDescribeInstanceEventWindows =
  res
    "DescribeInstanceEventWindowsResponse"
    "fixture/DescribeInstanceEventWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceEventWindows)

responseDescribeInstanceStatus :: DescribeInstanceStatusResponse -> TestTree
responseDescribeInstanceStatus =
  res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceStatus)

responseDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferingsResponse -> TestTree
responseDescribeInstanceTypeOfferings =
  res
    "DescribeInstanceTypeOfferingsResponse"
    "fixture/DescribeInstanceTypeOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceTypeOfferings)

responseDescribeInstanceTypes :: DescribeInstanceTypesResponse -> TestTree
responseDescribeInstanceTypes =
  res
    "DescribeInstanceTypesResponse"
    "fixture/DescribeInstanceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceTypes)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstances)

responseDescribeInternetGateways :: DescribeInternetGatewaysResponse -> TestTree
responseDescribeInternetGateways =
  res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInternetGateways)

responseDescribeIpamPools :: DescribeIpamPoolsResponse -> TestTree
responseDescribeIpamPools =
  res
    "DescribeIpamPoolsResponse"
    "fixture/DescribeIpamPoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIpamPools)

responseDescribeIpamScopes :: DescribeIpamScopesResponse -> TestTree
responseDescribeIpamScopes =
  res
    "DescribeIpamScopesResponse"
    "fixture/DescribeIpamScopesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIpamScopes)

responseDescribeIpams :: DescribeIpamsResponse -> TestTree
responseDescribeIpams =
  res
    "DescribeIpamsResponse"
    "fixture/DescribeIpamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIpams)

responseDescribeIpv6Pools :: DescribeIpv6PoolsResponse -> TestTree
responseDescribeIpv6Pools =
  res
    "DescribeIpv6PoolsResponse"
    "fixture/DescribeIpv6PoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIpv6Pools)

responseDescribeKeyPairs :: DescribeKeyPairsResponse -> TestTree
responseDescribeKeyPairs =
  res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKeyPairs)

responseDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersionsResponse -> TestTree
responseDescribeLaunchTemplateVersions =
  res
    "DescribeLaunchTemplateVersionsResponse"
    "fixture/DescribeLaunchTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLaunchTemplateVersions)

responseDescribeLaunchTemplates :: DescribeLaunchTemplatesResponse -> TestTree
responseDescribeLaunchTemplates =
  res
    "DescribeLaunchTemplatesResponse"
    "fixture/DescribeLaunchTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLaunchTemplates)

responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  res
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

responseDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVpcAssociations =
  res
    "DescribeLocalGatewayRouteTableVpcAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayRouteTableVpcAssociations)

responseDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTablesResponse -> TestTree
responseDescribeLocalGatewayRouteTables =
  res
    "DescribeLocalGatewayRouteTablesResponse"
    "fixture/DescribeLocalGatewayRouteTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayRouteTables)

responseDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaceGroups =
  res
    "DescribeLocalGatewayVirtualInterfaceGroupsResponse"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayVirtualInterfaceGroups)

responseDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfacesResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaces =
  res
    "DescribeLocalGatewayVirtualInterfacesResponse"
    "fixture/DescribeLocalGatewayVirtualInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGatewayVirtualInterfaces)

responseDescribeLocalGateways :: DescribeLocalGatewaysResponse -> TestTree
responseDescribeLocalGateways =
  res
    "DescribeLocalGatewaysResponse"
    "fixture/DescribeLocalGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocalGateways)

responseDescribeManagedPrefixLists :: DescribeManagedPrefixListsResponse -> TestTree
responseDescribeManagedPrefixLists =
  res
    "DescribeManagedPrefixListsResponse"
    "fixture/DescribeManagedPrefixListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeManagedPrefixLists)

responseDescribeMovingAddresses :: DescribeMovingAddressesResponse -> TestTree
responseDescribeMovingAddresses =
  res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMovingAddresses)

responseDescribeNatGateways :: DescribeNatGatewaysResponse -> TestTree
responseDescribeNatGateways =
  res
    "DescribeNatGatewaysResponse"
    "fixture/DescribeNatGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNatGateways)

responseDescribeNetworkAcls :: DescribeNetworkAclsResponse -> TestTree
responseDescribeNetworkAcls =
  res
    "DescribeNetworkAclsResponse"
    "fixture/DescribeNetworkAclsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkAcls)

responseDescribeNetworkInsightsAccessScopeAnalyses :: DescribeNetworkInsightsAccessScopeAnalysesResponse -> TestTree
responseDescribeNetworkInsightsAccessScopeAnalyses =
  res
    "DescribeNetworkInsightsAccessScopeAnalysesResponse"
    "fixture/DescribeNetworkInsightsAccessScopeAnalysesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInsightsAccessScopeAnalyses)

responseDescribeNetworkInsightsAccessScopes :: DescribeNetworkInsightsAccessScopesResponse -> TestTree
responseDescribeNetworkInsightsAccessScopes =
  res
    "DescribeNetworkInsightsAccessScopesResponse"
    "fixture/DescribeNetworkInsightsAccessScopesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInsightsAccessScopes)

responseDescribeNetworkInsightsAnalyses :: DescribeNetworkInsightsAnalysesResponse -> TestTree
responseDescribeNetworkInsightsAnalyses =
  res
    "DescribeNetworkInsightsAnalysesResponse"
    "fixture/DescribeNetworkInsightsAnalysesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInsightsAnalyses)

responseDescribeNetworkInsightsPaths :: DescribeNetworkInsightsPathsResponse -> TestTree
responseDescribeNetworkInsightsPaths =
  res
    "DescribeNetworkInsightsPathsResponse"
    "fixture/DescribeNetworkInsightsPathsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInsightsPaths)

responseDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttributeResponse -> TestTree
responseDescribeNetworkInterfaceAttribute =
  res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInterfaceAttribute)

responseDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissionsResponse -> TestTree
responseDescribeNetworkInterfacePermissions =
  res
    "DescribeNetworkInterfacePermissionsResponse"
    "fixture/DescribeNetworkInterfacePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInterfacePermissions)

responseDescribeNetworkInterfaces :: DescribeNetworkInterfacesResponse -> TestTree
responseDescribeNetworkInterfaces =
  res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNetworkInterfaces)

responseDescribePlacementGroups :: DescribePlacementGroupsResponse -> TestTree
responseDescribePlacementGroups =
  res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlacementGroups)

responseDescribePrefixLists :: DescribePrefixListsResponse -> TestTree
responseDescribePrefixLists =
  res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePrefixLists)

responseDescribePrincipalIdFormat :: DescribePrincipalIdFormatResponse -> TestTree
responseDescribePrincipalIdFormat =
  res
    "DescribePrincipalIdFormatResponse"
    "fixture/DescribePrincipalIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePrincipalIdFormat)

responseDescribePublicIpv4Pools :: DescribePublicIpv4PoolsResponse -> TestTree
responseDescribePublicIpv4Pools =
  res
    "DescribePublicIpv4PoolsResponse"
    "fixture/DescribePublicIpv4PoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePublicIpv4Pools)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegions)

responseDescribeReplaceRootVolumeTasks :: DescribeReplaceRootVolumeTasksResponse -> TestTree
responseDescribeReplaceRootVolumeTasks =
  res
    "DescribeReplaceRootVolumeTasksResponse"
    "fixture/DescribeReplaceRootVolumeTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplaceRootVolumeTasks)

responseDescribeReservedInstances :: DescribeReservedInstancesResponse -> TestTree
responseDescribeReservedInstances =
  res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstances)

responseDescribeReservedInstancesListings :: DescribeReservedInstancesListingsResponse -> TestTree
responseDescribeReservedInstancesListings =
  res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstancesListings)

responseDescribeReservedInstancesModifications :: DescribeReservedInstancesModificationsResponse -> TestTree
responseDescribeReservedInstancesModifications =
  res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstancesModifications)

responseDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferingsResponse -> TestTree
responseDescribeReservedInstancesOfferings =
  res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstancesOfferings)

responseDescribeRouteTables :: DescribeRouteTablesResponse -> TestTree
responseDescribeRouteTables =
  res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRouteTables)

responseDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailabilityResponse -> TestTree
responseDescribeScheduledInstanceAvailability =
  res
    "DescribeScheduledInstanceAvailabilityResponse"
    "fixture/DescribeScheduledInstanceAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledInstanceAvailability)

responseDescribeScheduledInstances :: DescribeScheduledInstancesResponse -> TestTree
responseDescribeScheduledInstances =
  res
    "DescribeScheduledInstancesResponse"
    "fixture/DescribeScheduledInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledInstances)

responseDescribeSecurityGroupReferences :: DescribeSecurityGroupReferencesResponse -> TestTree
responseDescribeSecurityGroupReferences =
  res
    "DescribeSecurityGroupReferencesResponse"
    "fixture/DescribeSecurityGroupReferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityGroupReferences)

responseDescribeSecurityGroupRules :: DescribeSecurityGroupRulesResponse -> TestTree
responseDescribeSecurityGroupRules =
  res
    "DescribeSecurityGroupRulesResponse"
    "fixture/DescribeSecurityGroupRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityGroupRules)

responseDescribeSecurityGroups :: DescribeSecurityGroupsResponse -> TestTree
responseDescribeSecurityGroups =
  res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityGroups)

responseDescribeSnapshotAttribute :: DescribeSnapshotAttributeResponse -> TestTree
responseDescribeSnapshotAttribute =
  res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshotAttribute)

responseDescribeSnapshotTierStatus :: DescribeSnapshotTierStatusResponse -> TestTree
responseDescribeSnapshotTierStatus =
  res
    "DescribeSnapshotTierStatusResponse"
    "fixture/DescribeSnapshotTierStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshotTierStatus)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshots)

responseDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
responseDescribeSpotDatafeedSubscription =
  res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotDatafeedSubscription)

responseDescribeSpotFleetInstances :: DescribeSpotFleetInstancesResponse -> TestTree
responseDescribeSpotFleetInstances =
  res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotFleetInstances)

responseDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistoryResponse -> TestTree
responseDescribeSpotFleetRequestHistory =
  res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotFleetRequestHistory)

responseDescribeSpotFleetRequests :: DescribeSpotFleetRequestsResponse -> TestTree
responseDescribeSpotFleetRequests =
  res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotFleetRequests)

responseDescribeSpotInstanceRequests :: DescribeSpotInstanceRequestsResponse -> TestTree
responseDescribeSpotInstanceRequests =
  res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotInstanceRequests)

responseDescribeSpotPriceHistory :: DescribeSpotPriceHistoryResponse -> TestTree
responseDescribeSpotPriceHistory =
  res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpotPriceHistory)

responseDescribeStaleSecurityGroups :: DescribeStaleSecurityGroupsResponse -> TestTree
responseDescribeStaleSecurityGroups =
  res
    "DescribeStaleSecurityGroupsResponse"
    "fixture/DescribeStaleSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStaleSecurityGroups)

responseDescribeStoreImageTasks :: DescribeStoreImageTasksResponse -> TestTree
responseDescribeStoreImageTasks =
  res
    "DescribeStoreImageTasksResponse"
    "fixture/DescribeStoreImageTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStoreImageTasks)

responseDescribeSubnets :: DescribeSubnetsResponse -> TestTree
responseDescribeSubnets =
  res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubnets)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFiltersResponse -> TestTree
responseDescribeTrafficMirrorFilters =
  res
    "DescribeTrafficMirrorFiltersResponse"
    "fixture/DescribeTrafficMirrorFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrafficMirrorFilters)

responseDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessionsResponse -> TestTree
responseDescribeTrafficMirrorSessions =
  res
    "DescribeTrafficMirrorSessionsResponse"
    "fixture/DescribeTrafficMirrorSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrafficMirrorSessions)

responseDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargetsResponse -> TestTree
responseDescribeTrafficMirrorTargets =
  res
    "DescribeTrafficMirrorTargetsResponse"
    "fixture/DescribeTrafficMirrorTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrafficMirrorTargets)

responseDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachmentsResponse -> TestTree
responseDescribeTransitGatewayAttachments =
  res
    "DescribeTransitGatewayAttachmentsResponse"
    "fixture/DescribeTransitGatewayAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayAttachments)

responseDescribeTransitGatewayConnectPeers :: DescribeTransitGatewayConnectPeersResponse -> TestTree
responseDescribeTransitGatewayConnectPeers =
  res
    "DescribeTransitGatewayConnectPeersResponse"
    "fixture/DescribeTransitGatewayConnectPeersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayConnectPeers)

responseDescribeTransitGatewayConnects :: DescribeTransitGatewayConnectsResponse -> TestTree
responseDescribeTransitGatewayConnects =
  res
    "DescribeTransitGatewayConnectsResponse"
    "fixture/DescribeTransitGatewayConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayConnects)

responseDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomainsResponse -> TestTree
responseDescribeTransitGatewayMulticastDomains =
  res
    "DescribeTransitGatewayMulticastDomainsResponse"
    "fixture/DescribeTransitGatewayMulticastDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayMulticastDomains)

responseDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachmentsResponse -> TestTree
responseDescribeTransitGatewayPeeringAttachments =
  res
    "DescribeTransitGatewayPeeringAttachmentsResponse"
    "fixture/DescribeTransitGatewayPeeringAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayPeeringAttachments)

responseDescribeTransitGatewayPolicyTables :: DescribeTransitGatewayPolicyTablesResponse -> TestTree
responseDescribeTransitGatewayPolicyTables =
  res
    "DescribeTransitGatewayPolicyTablesResponse"
    "fixture/DescribeTransitGatewayPolicyTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayPolicyTables)

responseDescribeTransitGatewayRouteTableAnnouncements :: DescribeTransitGatewayRouteTableAnnouncementsResponse -> TestTree
responseDescribeTransitGatewayRouteTableAnnouncements =
  res
    "DescribeTransitGatewayRouteTableAnnouncementsResponse"
    "fixture/DescribeTransitGatewayRouteTableAnnouncementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayRouteTableAnnouncements)

responseDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTablesResponse -> TestTree
responseDescribeTransitGatewayRouteTables =
  res
    "DescribeTransitGatewayRouteTablesResponse"
    "fixture/DescribeTransitGatewayRouteTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayRouteTables)

responseDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachmentsResponse -> TestTree
responseDescribeTransitGatewayVpcAttachments =
  res
    "DescribeTransitGatewayVpcAttachmentsResponse"
    "fixture/DescribeTransitGatewayVpcAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGatewayVpcAttachments)

responseDescribeTransitGateways :: DescribeTransitGatewaysResponse -> TestTree
responseDescribeTransitGateways =
  res
    "DescribeTransitGatewaysResponse"
    "fixture/DescribeTransitGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransitGateways)

responseDescribeTrunkInterfaceAssociations :: DescribeTrunkInterfaceAssociationsResponse -> TestTree
responseDescribeTrunkInterfaceAssociations =
  res
    "DescribeTrunkInterfaceAssociationsResponse"
    "fixture/DescribeTrunkInterfaceAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrunkInterfaceAssociations)

responseDescribeVerifiedAccessEndpoints :: DescribeVerifiedAccessEndpointsResponse -> TestTree
responseDescribeVerifiedAccessEndpoints =
  res
    "DescribeVerifiedAccessEndpointsResponse"
    "fixture/DescribeVerifiedAccessEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVerifiedAccessEndpoints)

responseDescribeVerifiedAccessGroups :: DescribeVerifiedAccessGroupsResponse -> TestTree
responseDescribeVerifiedAccessGroups =
  res
    "DescribeVerifiedAccessGroupsResponse"
    "fixture/DescribeVerifiedAccessGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVerifiedAccessGroups)

responseDescribeVerifiedAccessInstanceLoggingConfigurations :: DescribeVerifiedAccessInstanceLoggingConfigurationsResponse -> TestTree
responseDescribeVerifiedAccessInstanceLoggingConfigurations =
  res
    "DescribeVerifiedAccessInstanceLoggingConfigurationsResponse"
    "fixture/DescribeVerifiedAccessInstanceLoggingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVerifiedAccessInstanceLoggingConfigurations)

responseDescribeVerifiedAccessInstances :: DescribeVerifiedAccessInstancesResponse -> TestTree
responseDescribeVerifiedAccessInstances =
  res
    "DescribeVerifiedAccessInstancesResponse"
    "fixture/DescribeVerifiedAccessInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVerifiedAccessInstances)

responseDescribeVerifiedAccessTrustProviders :: DescribeVerifiedAccessTrustProvidersResponse -> TestTree
responseDescribeVerifiedAccessTrustProviders =
  res
    "DescribeVerifiedAccessTrustProvidersResponse"
    "fixture/DescribeVerifiedAccessTrustProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVerifiedAccessTrustProviders)

responseDescribeVolumeAttribute :: DescribeVolumeAttributeResponse -> TestTree
responseDescribeVolumeAttribute =
  res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumeAttribute)

responseDescribeVolumeStatus :: DescribeVolumeStatusResponse -> TestTree
responseDescribeVolumeStatus =
  res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumeStatus)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumes)

responseDescribeVolumesModifications :: DescribeVolumesModificationsResponse -> TestTree
responseDescribeVolumesModifications =
  res
    "DescribeVolumesModificationsResponse"
    "fixture/DescribeVolumesModificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumesModifications)

responseDescribeVpcAttribute :: DescribeVpcAttributeResponse -> TestTree
responseDescribeVpcAttribute =
  res
    "DescribeVpcAttributeResponse"
    "fixture/DescribeVpcAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcAttribute)

responseDescribeVpcClassicLink :: DescribeVpcClassicLinkResponse -> TestTree
responseDescribeVpcClassicLink =
  res
    "DescribeVpcClassicLinkResponse"
    "fixture/DescribeVpcClassicLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcClassicLink)

responseDescribeVpcClassicLinkDnsSupport :: DescribeVpcClassicLinkDnsSupportResponse -> TestTree
responseDescribeVpcClassicLinkDnsSupport =
  res
    "DescribeVpcClassicLinkDnsSupportResponse"
    "fixture/DescribeVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcClassicLinkDnsSupport)

responseDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotificationsResponse -> TestTree
responseDescribeVpcEndpointConnectionNotifications =
  res
    "DescribeVpcEndpointConnectionNotificationsResponse"
    "fixture/DescribeVpcEndpointConnectionNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointConnectionNotifications)

responseDescribeVpcEndpointConnections :: DescribeVpcEndpointConnectionsResponse -> TestTree
responseDescribeVpcEndpointConnections =
  res
    "DescribeVpcEndpointConnectionsResponse"
    "fixture/DescribeVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointConnections)

responseDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurationsResponse -> TestTree
responseDescribeVpcEndpointServiceConfigurations =
  res
    "DescribeVpcEndpointServiceConfigurationsResponse"
    "fixture/DescribeVpcEndpointServiceConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointServiceConfigurations)

responseDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissionsResponse -> TestTree
responseDescribeVpcEndpointServicePermissions =
  res
    "DescribeVpcEndpointServicePermissionsResponse"
    "fixture/DescribeVpcEndpointServicePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointServicePermissions)

responseDescribeVpcEndpointServices :: DescribeVpcEndpointServicesResponse -> TestTree
responseDescribeVpcEndpointServices =
  res
    "DescribeVpcEndpointServicesResponse"
    "fixture/DescribeVpcEndpointServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpointServices)

responseDescribeVpcEndpoints :: DescribeVpcEndpointsResponse -> TestTree
responseDescribeVpcEndpoints =
  res
    "DescribeVpcEndpointsResponse"
    "fixture/DescribeVpcEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpoints)

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcPeeringConnections)

responseDescribeVpcs :: DescribeVpcsResponse -> TestTree
responseDescribeVpcs =
  res
    "DescribeVpcsResponse"
    "fixture/DescribeVpcsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcs)

responseDescribeVpnConnections :: DescribeVpnConnectionsResponse -> TestTree
responseDescribeVpnConnections =
  res
    "DescribeVpnConnectionsResponse"
    "fixture/DescribeVpnConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpnConnections)

responseDescribeVpnGateways :: DescribeVpnGatewaysResponse -> TestTree
responseDescribeVpnGateways =
  res
    "DescribeVpnGatewaysResponse"
    "fixture/DescribeVpnGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpnGateways)

responseDetachClassicLinkVpc :: DetachClassicLinkVpcResponse -> TestTree
responseDetachClassicLinkVpc =
  res
    "DetachClassicLinkVpcResponse"
    "fixture/DetachClassicLinkVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachClassicLinkVpc)

responseDetachInternetGateway :: DetachInternetGatewayResponse -> TestTree
responseDetachInternetGateway =
  res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachInternetGateway)

responseDetachNetworkInterface :: DetachNetworkInterfaceResponse -> TestTree
responseDetachNetworkInterface =
  res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachNetworkInterface)

responseDetachVerifiedAccessTrustProvider :: DetachVerifiedAccessTrustProviderResponse -> TestTree
responseDetachVerifiedAccessTrustProvider =
  res
    "DetachVerifiedAccessTrustProviderResponse"
    "fixture/DetachVerifiedAccessTrustProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachVerifiedAccessTrustProvider)

responseDetachVolume :: VolumeAttachment -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachVolume)

responseDetachVpnGateway :: DetachVpnGatewayResponse -> TestTree
responseDetachVpnGateway =
  res
    "DetachVpnGatewayResponse"
    "fixture/DetachVpnGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachVpnGateway)

responseDisableAddressTransfer :: DisableAddressTransferResponse -> TestTree
responseDisableAddressTransfer =
  res
    "DisableAddressTransferResponse"
    "fixture/DisableAddressTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAddressTransfer)

responseDisableAwsNetworkPerformanceMetricSubscription :: DisableAwsNetworkPerformanceMetricSubscriptionResponse -> TestTree
responseDisableAwsNetworkPerformanceMetricSubscription =
  res
    "DisableAwsNetworkPerformanceMetricSubscriptionResponse"
    "fixture/DisableAwsNetworkPerformanceMetricSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAwsNetworkPerformanceMetricSubscription)

responseDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefaultResponse -> TestTree
responseDisableEbsEncryptionByDefault =
  res
    "DisableEbsEncryptionByDefaultResponse"
    "fixture/DisableEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableEbsEncryptionByDefault)

responseDisableFastLaunch :: DisableFastLaunchResponse -> TestTree
responseDisableFastLaunch =
  res
    "DisableFastLaunchResponse"
    "fixture/DisableFastLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableFastLaunch)

responseDisableFastSnapshotRestores :: DisableFastSnapshotRestoresResponse -> TestTree
responseDisableFastSnapshotRestores =
  res
    "DisableFastSnapshotRestoresResponse"
    "fixture/DisableFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableFastSnapshotRestores)

responseDisableImageDeprecation :: DisableImageDeprecationResponse -> TestTree
responseDisableImageDeprecation =
  res
    "DisableImageDeprecationResponse"
    "fixture/DisableImageDeprecationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableImageDeprecation)

responseDisableIpamOrganizationAdminAccount :: DisableIpamOrganizationAdminAccountResponse -> TestTree
responseDisableIpamOrganizationAdminAccount =
  res
    "DisableIpamOrganizationAdminAccountResponse"
    "fixture/DisableIpamOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableIpamOrganizationAdminAccount)

responseDisableSerialConsoleAccess :: DisableSerialConsoleAccessResponse -> TestTree
responseDisableSerialConsoleAccess =
  res
    "DisableSerialConsoleAccessResponse"
    "fixture/DisableSerialConsoleAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSerialConsoleAccess)

responseDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagationResponse -> TestTree
responseDisableTransitGatewayRouteTablePropagation =
  res
    "DisableTransitGatewayRouteTablePropagationResponse"
    "fixture/DisableTransitGatewayRouteTablePropagationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableTransitGatewayRouteTablePropagation)

responseDisableVgwRoutePropagation :: DisableVgwRoutePropagationResponse -> TestTree
responseDisableVgwRoutePropagation =
  res
    "DisableVgwRoutePropagationResponse"
    "fixture/DisableVgwRoutePropagationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableVgwRoutePropagation)

responseDisableVpcClassicLink :: DisableVpcClassicLinkResponse -> TestTree
responseDisableVpcClassicLink =
  res
    "DisableVpcClassicLinkResponse"
    "fixture/DisableVpcClassicLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableVpcClassicLink)

responseDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupportResponse -> TestTree
responseDisableVpcClassicLinkDnsSupport =
  res
    "DisableVpcClassicLinkDnsSupportResponse"
    "fixture/DisableVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableVpcClassicLinkDnsSupport)

responseDisassociateAddress :: DisassociateAddressResponse -> TestTree
responseDisassociateAddress =
  res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAddress)

responseDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetworkResponse -> TestTree
responseDisassociateClientVpnTargetNetwork =
  res
    "DisassociateClientVpnTargetNetworkResponse"
    "fixture/DisassociateClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateClientVpnTargetNetwork)

responseDisassociateEnclaveCertificateIamRole :: DisassociateEnclaveCertificateIamRoleResponse -> TestTree
responseDisassociateEnclaveCertificateIamRole =
  res
    "DisassociateEnclaveCertificateIamRoleResponse"
    "fixture/DisassociateEnclaveCertificateIamRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateEnclaveCertificateIamRole)

responseDisassociateIamInstanceProfile :: DisassociateIamInstanceProfileResponse -> TestTree
responseDisassociateIamInstanceProfile =
  res
    "DisassociateIamInstanceProfileResponse"
    "fixture/DisassociateIamInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateIamInstanceProfile)

responseDisassociateInstanceEventWindow :: DisassociateInstanceEventWindowResponse -> TestTree
responseDisassociateInstanceEventWindow =
  res
    "DisassociateInstanceEventWindowResponse"
    "fixture/DisassociateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateInstanceEventWindow)

responseDisassociateRouteTable :: DisassociateRouteTableResponse -> TestTree
responseDisassociateRouteTable =
  res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRouteTable)

responseDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlockResponse -> TestTree
responseDisassociateSubnetCidrBlock =
  res
    "DisassociateSubnetCidrBlockResponse"
    "fixture/DisassociateSubnetCidrBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSubnetCidrBlock)

responseDisassociateTransitGatewayMulticastDomain :: DisassociateTransitGatewayMulticastDomainResponse -> TestTree
responseDisassociateTransitGatewayMulticastDomain =
  res
    "DisassociateTransitGatewayMulticastDomainResponse"
    "fixture/DisassociateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTransitGatewayMulticastDomain)

responseDisassociateTransitGatewayPolicyTable :: DisassociateTransitGatewayPolicyTableResponse -> TestTree
responseDisassociateTransitGatewayPolicyTable =
  res
    "DisassociateTransitGatewayPolicyTableResponse"
    "fixture/DisassociateTransitGatewayPolicyTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTransitGatewayPolicyTable)

responseDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTableResponse -> TestTree
responseDisassociateTransitGatewayRouteTable =
  res
    "DisassociateTransitGatewayRouteTableResponse"
    "fixture/DisassociateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTransitGatewayRouteTable)

responseDisassociateTrunkInterface :: DisassociateTrunkInterfaceResponse -> TestTree
responseDisassociateTrunkInterface =
  res
    "DisassociateTrunkInterfaceResponse"
    "fixture/DisassociateTrunkInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTrunkInterface)

responseDisassociateVpcCidrBlock :: DisassociateVpcCidrBlockResponse -> TestTree
responseDisassociateVpcCidrBlock =
  res
    "DisassociateVpcCidrBlockResponse"
    "fixture/DisassociateVpcCidrBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateVpcCidrBlock)

responseEnableAddressTransfer :: EnableAddressTransferResponse -> TestTree
responseEnableAddressTransfer =
  res
    "EnableAddressTransferResponse"
    "fixture/EnableAddressTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAddressTransfer)

responseEnableAwsNetworkPerformanceMetricSubscription :: EnableAwsNetworkPerformanceMetricSubscriptionResponse -> TestTree
responseEnableAwsNetworkPerformanceMetricSubscription =
  res
    "EnableAwsNetworkPerformanceMetricSubscriptionResponse"
    "fixture/EnableAwsNetworkPerformanceMetricSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAwsNetworkPerformanceMetricSubscription)

responseEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefaultResponse -> TestTree
responseEnableEbsEncryptionByDefault =
  res
    "EnableEbsEncryptionByDefaultResponse"
    "fixture/EnableEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableEbsEncryptionByDefault)

responseEnableFastLaunch :: EnableFastLaunchResponse -> TestTree
responseEnableFastLaunch =
  res
    "EnableFastLaunchResponse"
    "fixture/EnableFastLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableFastLaunch)

responseEnableFastSnapshotRestores :: EnableFastSnapshotRestoresResponse -> TestTree
responseEnableFastSnapshotRestores =
  res
    "EnableFastSnapshotRestoresResponse"
    "fixture/EnableFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableFastSnapshotRestores)

responseEnableImageDeprecation :: EnableImageDeprecationResponse -> TestTree
responseEnableImageDeprecation =
  res
    "EnableImageDeprecationResponse"
    "fixture/EnableImageDeprecationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableImageDeprecation)

responseEnableIpamOrganizationAdminAccount :: EnableIpamOrganizationAdminAccountResponse -> TestTree
responseEnableIpamOrganizationAdminAccount =
  res
    "EnableIpamOrganizationAdminAccountResponse"
    "fixture/EnableIpamOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableIpamOrganizationAdminAccount)

responseEnableReachabilityAnalyzerOrganizationSharing :: EnableReachabilityAnalyzerOrganizationSharingResponse -> TestTree
responseEnableReachabilityAnalyzerOrganizationSharing =
  res
    "EnableReachabilityAnalyzerOrganizationSharingResponse"
    "fixture/EnableReachabilityAnalyzerOrganizationSharingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableReachabilityAnalyzerOrganizationSharing)

responseEnableSerialConsoleAccess :: EnableSerialConsoleAccessResponse -> TestTree
responseEnableSerialConsoleAccess =
  res
    "EnableSerialConsoleAccessResponse"
    "fixture/EnableSerialConsoleAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSerialConsoleAccess)

responseEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagationResponse -> TestTree
responseEnableTransitGatewayRouteTablePropagation =
  res
    "EnableTransitGatewayRouteTablePropagationResponse"
    "fixture/EnableTransitGatewayRouteTablePropagationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableTransitGatewayRouteTablePropagation)

responseEnableVgwRoutePropagation :: EnableVgwRoutePropagationResponse -> TestTree
responseEnableVgwRoutePropagation =
  res
    "EnableVgwRoutePropagationResponse"
    "fixture/EnableVgwRoutePropagationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableVgwRoutePropagation)

responseEnableVolumeIO :: EnableVolumeIOResponse -> TestTree
responseEnableVolumeIO =
  res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableVolumeIO)

responseEnableVpcClassicLink :: EnableVpcClassicLinkResponse -> TestTree
responseEnableVpcClassicLink =
  res
    "EnableVpcClassicLinkResponse"
    "fixture/EnableVpcClassicLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableVpcClassicLink)

responseEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupportResponse -> TestTree
responseEnableVpcClassicLinkDnsSupport =
  res
    "EnableVpcClassicLinkDnsSupportResponse"
    "fixture/EnableVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableVpcClassicLinkDnsSupport)

responseExportClientVpnClientCertificateRevocationList :: ExportClientVpnClientCertificateRevocationListResponse -> TestTree
responseExportClientVpnClientCertificateRevocationList =
  res
    "ExportClientVpnClientCertificateRevocationListResponse"
    "fixture/ExportClientVpnClientCertificateRevocationListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportClientVpnClientCertificateRevocationList)

responseExportClientVpnClientConfiguration :: ExportClientVpnClientConfigurationResponse -> TestTree
responseExportClientVpnClientConfiguration =
  res
    "ExportClientVpnClientConfigurationResponse"
    "fixture/ExportClientVpnClientConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportClientVpnClientConfiguration)

responseExportImage :: ExportImageResponse -> TestTree
responseExportImage =
  res
    "ExportImageResponse"
    "fixture/ExportImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportImage)

responseExportTransitGatewayRoutes :: ExportTransitGatewayRoutesResponse -> TestTree
responseExportTransitGatewayRoutes =
  res
    "ExportTransitGatewayRoutesResponse"
    "fixture/ExportTransitGatewayRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportTransitGatewayRoutes)

responseGetAssociatedEnclaveCertificateIamRoles :: GetAssociatedEnclaveCertificateIamRolesResponse -> TestTree
responseGetAssociatedEnclaveCertificateIamRoles =
  res
    "GetAssociatedEnclaveCertificateIamRolesResponse"
    "fixture/GetAssociatedEnclaveCertificateIamRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssociatedEnclaveCertificateIamRoles)

responseGetAssociatedIpv6PoolCidrs :: GetAssociatedIpv6PoolCidrsResponse -> TestTree
responseGetAssociatedIpv6PoolCidrs =
  res
    "GetAssociatedIpv6PoolCidrsResponse"
    "fixture/GetAssociatedIpv6PoolCidrsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssociatedIpv6PoolCidrs)

responseGetAwsNetworkPerformanceData :: GetAwsNetworkPerformanceDataResponse -> TestTree
responseGetAwsNetworkPerformanceData =
  res
    "GetAwsNetworkPerformanceDataResponse"
    "fixture/GetAwsNetworkPerformanceDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAwsNetworkPerformanceData)

responseGetCapacityReservationUsage :: GetCapacityReservationUsageResponse -> TestTree
responseGetCapacityReservationUsage =
  res
    "GetCapacityReservationUsageResponse"
    "fixture/GetCapacityReservationUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCapacityReservationUsage)

responseGetCoipPoolUsage :: GetCoipPoolUsageResponse -> TestTree
responseGetCoipPoolUsage =
  res
    "GetCoipPoolUsageResponse"
    "fixture/GetCoipPoolUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoipPoolUsage)

responseGetConsoleOutput :: GetConsoleOutputResponse -> TestTree
responseGetConsoleOutput =
  res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConsoleOutput)

responseGetConsoleScreenshot :: GetConsoleScreenshotResponse -> TestTree
responseGetConsoleScreenshot =
  res
    "GetConsoleScreenshotResponse"
    "fixture/GetConsoleScreenshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConsoleScreenshot)

responseGetDefaultCreditSpecification :: GetDefaultCreditSpecificationResponse -> TestTree
responseGetDefaultCreditSpecification =
  res
    "GetDefaultCreditSpecificationResponse"
    "fixture/GetDefaultCreditSpecificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDefaultCreditSpecification)

responseGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyIdResponse -> TestTree
responseGetEbsDefaultKmsKeyId =
  res
    "GetEbsDefaultKmsKeyIdResponse"
    "fixture/GetEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEbsDefaultKmsKeyId)

responseGetEbsEncryptionByDefault :: GetEbsEncryptionByDefaultResponse -> TestTree
responseGetEbsEncryptionByDefault =
  res
    "GetEbsEncryptionByDefaultResponse"
    "fixture/GetEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEbsEncryptionByDefault)

responseGetFlowLogsIntegrationTemplate :: GetFlowLogsIntegrationTemplateResponse -> TestTree
responseGetFlowLogsIntegrationTemplate =
  res
    "GetFlowLogsIntegrationTemplateResponse"
    "fixture/GetFlowLogsIntegrationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFlowLogsIntegrationTemplate)

responseGetGroupsForCapacityReservation :: GetGroupsForCapacityReservationResponse -> TestTree
responseGetGroupsForCapacityReservation =
  res
    "GetGroupsForCapacityReservationResponse"
    "fixture/GetGroupsForCapacityReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupsForCapacityReservation)

responseGetHostReservationPurchasePreview :: GetHostReservationPurchasePreviewResponse -> TestTree
responseGetHostReservationPurchasePreview =
  res
    "GetHostReservationPurchasePreviewResponse"
    "fixture/GetHostReservationPurchasePreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostReservationPurchasePreview)

responseGetInstanceTypesFromInstanceRequirements :: GetInstanceTypesFromInstanceRequirementsResponse -> TestTree
responseGetInstanceTypesFromInstanceRequirements =
  res
    "GetInstanceTypesFromInstanceRequirementsResponse"
    "fixture/GetInstanceTypesFromInstanceRequirementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceTypesFromInstanceRequirements)

responseGetInstanceUefiData :: GetInstanceUefiDataResponse -> TestTree
responseGetInstanceUefiData =
  res
    "GetInstanceUefiDataResponse"
    "fixture/GetInstanceUefiDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceUefiData)

responseGetIpamAddressHistory :: GetIpamAddressHistoryResponse -> TestTree
responseGetIpamAddressHistory =
  res
    "GetIpamAddressHistoryResponse"
    "fixture/GetIpamAddressHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIpamAddressHistory)

responseGetIpamPoolAllocations :: GetIpamPoolAllocationsResponse -> TestTree
responseGetIpamPoolAllocations =
  res
    "GetIpamPoolAllocationsResponse"
    "fixture/GetIpamPoolAllocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIpamPoolAllocations)

responseGetIpamPoolCidrs :: GetIpamPoolCidrsResponse -> TestTree
responseGetIpamPoolCidrs =
  res
    "GetIpamPoolCidrsResponse"
    "fixture/GetIpamPoolCidrsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIpamPoolCidrs)

responseGetIpamResourceCidrs :: GetIpamResourceCidrsResponse -> TestTree
responseGetIpamResourceCidrs =
  res
    "GetIpamResourceCidrsResponse"
    "fixture/GetIpamResourceCidrsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIpamResourceCidrs)

responseGetLaunchTemplateData :: GetLaunchTemplateDataResponse -> TestTree
responseGetLaunchTemplateData =
  res
    "GetLaunchTemplateDataResponse"
    "fixture/GetLaunchTemplateDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchTemplateData)

responseGetManagedPrefixListAssociations :: GetManagedPrefixListAssociationsResponse -> TestTree
responseGetManagedPrefixListAssociations =
  res
    "GetManagedPrefixListAssociationsResponse"
    "fixture/GetManagedPrefixListAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetManagedPrefixListAssociations)

responseGetManagedPrefixListEntries :: GetManagedPrefixListEntriesResponse -> TestTree
responseGetManagedPrefixListEntries =
  res
    "GetManagedPrefixListEntriesResponse"
    "fixture/GetManagedPrefixListEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetManagedPrefixListEntries)

responseGetNetworkInsightsAccessScopeAnalysisFindings :: GetNetworkInsightsAccessScopeAnalysisFindingsResponse -> TestTree
responseGetNetworkInsightsAccessScopeAnalysisFindings =
  res
    "GetNetworkInsightsAccessScopeAnalysisFindingsResponse"
    "fixture/GetNetworkInsightsAccessScopeAnalysisFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkInsightsAccessScopeAnalysisFindings)

responseGetNetworkInsightsAccessScopeContent :: GetNetworkInsightsAccessScopeContentResponse -> TestTree
responseGetNetworkInsightsAccessScopeContent =
  res
    "GetNetworkInsightsAccessScopeContentResponse"
    "fixture/GetNetworkInsightsAccessScopeContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkInsightsAccessScopeContent)

responseGetPasswordData :: GetPasswordDataResponse -> TestTree
responseGetPasswordData =
  res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPasswordData)

responseGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuoteResponse -> TestTree
responseGetReservedInstancesExchangeQuote =
  res
    "GetReservedInstancesExchangeQuoteResponse"
    "fixture/GetReservedInstancesExchangeQuoteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservedInstancesExchangeQuote)

responseGetSerialConsoleAccessStatus :: GetSerialConsoleAccessStatusResponse -> TestTree
responseGetSerialConsoleAccessStatus =
  res
    "GetSerialConsoleAccessStatusResponse"
    "fixture/GetSerialConsoleAccessStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSerialConsoleAccessStatus)

responseGetSpotPlacementScores :: GetSpotPlacementScoresResponse -> TestTree
responseGetSpotPlacementScores =
  res
    "GetSpotPlacementScoresResponse"
    "fixture/GetSpotPlacementScoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSpotPlacementScores)

responseGetSubnetCidrReservations :: GetSubnetCidrReservationsResponse -> TestTree
responseGetSubnetCidrReservations =
  res
    "GetSubnetCidrReservationsResponse"
    "fixture/GetSubnetCidrReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubnetCidrReservations)

responseGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagationsResponse -> TestTree
responseGetTransitGatewayAttachmentPropagations =
  res
    "GetTransitGatewayAttachmentPropagationsResponse"
    "fixture/GetTransitGatewayAttachmentPropagationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayAttachmentPropagations)

responseGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseGetTransitGatewayMulticastDomainAssociations =
  res
    "GetTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/GetTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayMulticastDomainAssociations)

responseGetTransitGatewayPolicyTableAssociations :: GetTransitGatewayPolicyTableAssociationsResponse -> TestTree
responseGetTransitGatewayPolicyTableAssociations =
  res
    "GetTransitGatewayPolicyTableAssociationsResponse"
    "fixture/GetTransitGatewayPolicyTableAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayPolicyTableAssociations)

responseGetTransitGatewayPolicyTableEntries :: GetTransitGatewayPolicyTableEntriesResponse -> TestTree
responseGetTransitGatewayPolicyTableEntries =
  res
    "GetTransitGatewayPolicyTableEntriesResponse"
    "fixture/GetTransitGatewayPolicyTableEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayPolicyTableEntries)

responseGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferencesResponse -> TestTree
responseGetTransitGatewayPrefixListReferences =
  res
    "GetTransitGatewayPrefixListReferencesResponse"
    "fixture/GetTransitGatewayPrefixListReferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayPrefixListReferences)

responseGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociationsResponse -> TestTree
responseGetTransitGatewayRouteTableAssociations =
  res
    "GetTransitGatewayRouteTableAssociationsResponse"
    "fixture/GetTransitGatewayRouteTableAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayRouteTableAssociations)

responseGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagationsResponse -> TestTree
responseGetTransitGatewayRouteTablePropagations =
  res
    "GetTransitGatewayRouteTablePropagationsResponse"
    "fixture/GetTransitGatewayRouteTablePropagationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayRouteTablePropagations)

responseGetVerifiedAccessEndpointPolicy :: GetVerifiedAccessEndpointPolicyResponse -> TestTree
responseGetVerifiedAccessEndpointPolicy =
  res
    "GetVerifiedAccessEndpointPolicyResponse"
    "fixture/GetVerifiedAccessEndpointPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVerifiedAccessEndpointPolicy)

responseGetVerifiedAccessGroupPolicy :: GetVerifiedAccessGroupPolicyResponse -> TestTree
responseGetVerifiedAccessGroupPolicy =
  res
    "GetVerifiedAccessGroupPolicyResponse"
    "fixture/GetVerifiedAccessGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVerifiedAccessGroupPolicy)

responseGetVpnConnectionDeviceSampleConfiguration :: GetVpnConnectionDeviceSampleConfigurationResponse -> TestTree
responseGetVpnConnectionDeviceSampleConfiguration =
  res
    "GetVpnConnectionDeviceSampleConfigurationResponse"
    "fixture/GetVpnConnectionDeviceSampleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpnConnectionDeviceSampleConfiguration)

responseGetVpnConnectionDeviceTypes :: GetVpnConnectionDeviceTypesResponse -> TestTree
responseGetVpnConnectionDeviceTypes =
  res
    "GetVpnConnectionDeviceTypesResponse"
    "fixture/GetVpnConnectionDeviceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpnConnectionDeviceTypes)

responseImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationListResponse -> TestTree
responseImportClientVpnClientCertificateRevocationList =
  res
    "ImportClientVpnClientCertificateRevocationListResponse"
    "fixture/ImportClientVpnClientCertificateRevocationListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportClientVpnClientCertificateRevocationList)

responseImportImage :: ImportImageResponse -> TestTree
responseImportImage =
  res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportImage)

responseImportInstance :: ImportInstanceResponse -> TestTree
responseImportInstance =
  res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportInstance)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportKeyPair)

responseImportSnapshot :: ImportSnapshotResponse -> TestTree
responseImportSnapshot =
  res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportSnapshot)

responseImportVolume :: ImportVolumeResponse -> TestTree
responseImportVolume =
  res
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportVolume)

responseListImagesInRecycleBin :: ListImagesInRecycleBinResponse -> TestTree
responseListImagesInRecycleBin =
  res
    "ListImagesInRecycleBinResponse"
    "fixture/ListImagesInRecycleBinResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImagesInRecycleBin)

responseListSnapshotsInRecycleBin :: ListSnapshotsInRecycleBinResponse -> TestTree
responseListSnapshotsInRecycleBin =
  res
    "ListSnapshotsInRecycleBinResponse"
    "fixture/ListSnapshotsInRecycleBinResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSnapshotsInRecycleBin)

responseModifyAddressAttribute :: ModifyAddressAttributeResponse -> TestTree
responseModifyAddressAttribute =
  res
    "ModifyAddressAttributeResponse"
    "fixture/ModifyAddressAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAddressAttribute)

responseModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroupResponse -> TestTree
responseModifyAvailabilityZoneGroup =
  res
    "ModifyAvailabilityZoneGroupResponse"
    "fixture/ModifyAvailabilityZoneGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAvailabilityZoneGroup)

responseModifyCapacityReservation :: ModifyCapacityReservationResponse -> TestTree
responseModifyCapacityReservation =
  res
    "ModifyCapacityReservationResponse"
    "fixture/ModifyCapacityReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCapacityReservation)

responseModifyCapacityReservationFleet :: ModifyCapacityReservationFleetResponse -> TestTree
responseModifyCapacityReservationFleet =
  res
    "ModifyCapacityReservationFleetResponse"
    "fixture/ModifyCapacityReservationFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCapacityReservationFleet)

responseModifyClientVpnEndpoint :: ModifyClientVpnEndpointResponse -> TestTree
responseModifyClientVpnEndpoint =
  res
    "ModifyClientVpnEndpointResponse"
    "fixture/ModifyClientVpnEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClientVpnEndpoint)

responseModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecificationResponse -> TestTree
responseModifyDefaultCreditSpecification =
  res
    "ModifyDefaultCreditSpecificationResponse"
    "fixture/ModifyDefaultCreditSpecificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDefaultCreditSpecification)

responseModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyIdResponse -> TestTree
responseModifyEbsDefaultKmsKeyId =
  res
    "ModifyEbsDefaultKmsKeyIdResponse"
    "fixture/ModifyEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEbsDefaultKmsKeyId)

responseModifyFleet :: ModifyFleetResponse -> TestTree
responseModifyFleet =
  res
    "ModifyFleetResponse"
    "fixture/ModifyFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyFleet)

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

responseModifyIdFormat :: ModifyIdFormatResponse -> TestTree
responseModifyIdFormat =
  res
    "ModifyIdFormatResponse"
    "fixture/ModifyIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyIdFormat)

responseModifyIdentityIdFormat :: ModifyIdentityIdFormatResponse -> TestTree
responseModifyIdentityIdFormat =
  res
    "ModifyIdentityIdFormatResponse"
    "fixture/ModifyIdentityIdFormatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyIdentityIdFormat)

responseModifyImageAttribute :: ModifyImageAttributeResponse -> TestTree
responseModifyImageAttribute =
  res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyImageAttribute)

responseModifyInstanceAttribute :: ModifyInstanceAttributeResponse -> TestTree
responseModifyInstanceAttribute =
  res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceAttribute)

responseModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributesResponse -> TestTree
responseModifyInstanceCapacityReservationAttributes =
  res
    "ModifyInstanceCapacityReservationAttributesResponse"
    "fixture/ModifyInstanceCapacityReservationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceCapacityReservationAttributes)

responseModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecificationResponse -> TestTree
responseModifyInstanceCreditSpecification =
  res
    "ModifyInstanceCreditSpecificationResponse"
    "fixture/ModifyInstanceCreditSpecificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceCreditSpecification)

responseModifyInstanceEventStartTime :: ModifyInstanceEventStartTimeResponse -> TestTree
responseModifyInstanceEventStartTime =
  res
    "ModifyInstanceEventStartTimeResponse"
    "fixture/ModifyInstanceEventStartTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceEventStartTime)

responseModifyInstanceEventWindow :: ModifyInstanceEventWindowResponse -> TestTree
responseModifyInstanceEventWindow =
  res
    "ModifyInstanceEventWindowResponse"
    "fixture/ModifyInstanceEventWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceEventWindow)

responseModifyInstanceMaintenanceOptions :: ModifyInstanceMaintenanceOptionsResponse -> TestTree
responseModifyInstanceMaintenanceOptions =
  res
    "ModifyInstanceMaintenanceOptionsResponse"
    "fixture/ModifyInstanceMaintenanceOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceMaintenanceOptions)

responseModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptionsResponse -> TestTree
responseModifyInstanceMetadataOptions =
  res
    "ModifyInstanceMetadataOptionsResponse"
    "fixture/ModifyInstanceMetadataOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceMetadataOptions)

responseModifyInstancePlacement :: ModifyInstancePlacementResponse -> TestTree
responseModifyInstancePlacement =
  res
    "ModifyInstancePlacementResponse"
    "fixture/ModifyInstancePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstancePlacement)

responseModifyIpam :: ModifyIpamResponse -> TestTree
responseModifyIpam =
  res
    "ModifyIpamResponse"
    "fixture/ModifyIpamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyIpam)

responseModifyIpamPool :: ModifyIpamPoolResponse -> TestTree
responseModifyIpamPool =
  res
    "ModifyIpamPoolResponse"
    "fixture/ModifyIpamPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyIpamPool)

responseModifyIpamResourceCidr :: ModifyIpamResourceCidrResponse -> TestTree
responseModifyIpamResourceCidr =
  res
    "ModifyIpamResourceCidrResponse"
    "fixture/ModifyIpamResourceCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyIpamResourceCidr)

responseModifyIpamScope :: ModifyIpamScopeResponse -> TestTree
responseModifyIpamScope =
  res
    "ModifyIpamScopeResponse"
    "fixture/ModifyIpamScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyIpamScope)

responseModifyLaunchTemplate :: ModifyLaunchTemplateResponse -> TestTree
responseModifyLaunchTemplate =
  res
    "ModifyLaunchTemplateResponse"
    "fixture/ModifyLaunchTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyLaunchTemplate)

responseModifyLocalGatewayRoute :: ModifyLocalGatewayRouteResponse -> TestTree
responseModifyLocalGatewayRoute =
  res
    "ModifyLocalGatewayRouteResponse"
    "fixture/ModifyLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyLocalGatewayRoute)

responseModifyManagedPrefixList :: ModifyManagedPrefixListResponse -> TestTree
responseModifyManagedPrefixList =
  res
    "ModifyManagedPrefixListResponse"
    "fixture/ModifyManagedPrefixListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyManagedPrefixList)

responseModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttributeResponse -> TestTree
responseModifyNetworkInterfaceAttribute =
  res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyNetworkInterfaceAttribute)

responseModifyPrivateDnsNameOptions :: ModifyPrivateDnsNameOptionsResponse -> TestTree
responseModifyPrivateDnsNameOptions =
  res
    "ModifyPrivateDnsNameOptionsResponse"
    "fixture/ModifyPrivateDnsNameOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyPrivateDnsNameOptions)

responseModifyReservedInstances :: ModifyReservedInstancesResponse -> TestTree
responseModifyReservedInstances =
  res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReservedInstances)

responseModifySecurityGroupRules :: ModifySecurityGroupRulesResponse -> TestTree
responseModifySecurityGroupRules =
  res
    "ModifySecurityGroupRulesResponse"
    "fixture/ModifySecurityGroupRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySecurityGroupRules)

responseModifySnapshotAttribute :: ModifySnapshotAttributeResponse -> TestTree
responseModifySnapshotAttribute =
  res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySnapshotAttribute)

responseModifySnapshotTier :: ModifySnapshotTierResponse -> TestTree
responseModifySnapshotTier =
  res
    "ModifySnapshotTierResponse"
    "fixture/ModifySnapshotTierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySnapshotTier)

responseModifySpotFleetRequest :: ModifySpotFleetRequestResponse -> TestTree
responseModifySpotFleetRequest =
  res
    "ModifySpotFleetRequestResponse"
    "fixture/ModifySpotFleetRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySpotFleetRequest)

responseModifySubnetAttribute :: ModifySubnetAttributeResponse -> TestTree
responseModifySubnetAttribute =
  res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySubnetAttribute)

responseModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServicesResponse -> TestTree
responseModifyTrafficMirrorFilterNetworkServices =
  res
    "ModifyTrafficMirrorFilterNetworkServicesResponse"
    "fixture/ModifyTrafficMirrorFilterNetworkServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTrafficMirrorFilterNetworkServices)

responseModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRuleResponse -> TestTree
responseModifyTrafficMirrorFilterRule =
  res
    "ModifyTrafficMirrorFilterRuleResponse"
    "fixture/ModifyTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTrafficMirrorFilterRule)

responseModifyTrafficMirrorSession :: ModifyTrafficMirrorSessionResponse -> TestTree
responseModifyTrafficMirrorSession =
  res
    "ModifyTrafficMirrorSessionResponse"
    "fixture/ModifyTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTrafficMirrorSession)

responseModifyTransitGateway :: ModifyTransitGatewayResponse -> TestTree
responseModifyTransitGateway =
  res
    "ModifyTransitGatewayResponse"
    "fixture/ModifyTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTransitGateway)

responseModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReferenceResponse -> TestTree
responseModifyTransitGatewayPrefixListReference =
  res
    "ModifyTransitGatewayPrefixListReferenceResponse"
    "fixture/ModifyTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTransitGatewayPrefixListReference)

responseModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachmentResponse -> TestTree
responseModifyTransitGatewayVpcAttachment =
  res
    "ModifyTransitGatewayVpcAttachmentResponse"
    "fixture/ModifyTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTransitGatewayVpcAttachment)

responseModifyVerifiedAccessEndpoint :: ModifyVerifiedAccessEndpointResponse -> TestTree
responseModifyVerifiedAccessEndpoint =
  res
    "ModifyVerifiedAccessEndpointResponse"
    "fixture/ModifyVerifiedAccessEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVerifiedAccessEndpoint)

responseModifyVerifiedAccessEndpointPolicy :: ModifyVerifiedAccessEndpointPolicyResponse -> TestTree
responseModifyVerifiedAccessEndpointPolicy =
  res
    "ModifyVerifiedAccessEndpointPolicyResponse"
    "fixture/ModifyVerifiedAccessEndpointPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVerifiedAccessEndpointPolicy)

responseModifyVerifiedAccessGroup :: ModifyVerifiedAccessGroupResponse -> TestTree
responseModifyVerifiedAccessGroup =
  res
    "ModifyVerifiedAccessGroupResponse"
    "fixture/ModifyVerifiedAccessGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVerifiedAccessGroup)

responseModifyVerifiedAccessGroupPolicy :: ModifyVerifiedAccessGroupPolicyResponse -> TestTree
responseModifyVerifiedAccessGroupPolicy =
  res
    "ModifyVerifiedAccessGroupPolicyResponse"
    "fixture/ModifyVerifiedAccessGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVerifiedAccessGroupPolicy)

responseModifyVerifiedAccessInstance :: ModifyVerifiedAccessInstanceResponse -> TestTree
responseModifyVerifiedAccessInstance =
  res
    "ModifyVerifiedAccessInstanceResponse"
    "fixture/ModifyVerifiedAccessInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVerifiedAccessInstance)

responseModifyVerifiedAccessInstanceLoggingConfiguration :: ModifyVerifiedAccessInstanceLoggingConfigurationResponse -> TestTree
responseModifyVerifiedAccessInstanceLoggingConfiguration =
  res
    "ModifyVerifiedAccessInstanceLoggingConfigurationResponse"
    "fixture/ModifyVerifiedAccessInstanceLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVerifiedAccessInstanceLoggingConfiguration)

responseModifyVerifiedAccessTrustProvider :: ModifyVerifiedAccessTrustProviderResponse -> TestTree
responseModifyVerifiedAccessTrustProvider =
  res
    "ModifyVerifiedAccessTrustProviderResponse"
    "fixture/ModifyVerifiedAccessTrustProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVerifiedAccessTrustProvider)

responseModifyVolume :: ModifyVolumeResponse -> TestTree
responseModifyVolume =
  res
    "ModifyVolumeResponse"
    "fixture/ModifyVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVolume)

responseModifyVolumeAttribute :: ModifyVolumeAttributeResponse -> TestTree
responseModifyVolumeAttribute =
  res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVolumeAttribute)

responseModifyVpcAttribute :: ModifyVpcAttributeResponse -> TestTree
responseModifyVpcAttribute =
  res
    "ModifyVpcAttributeResponse"
    "fixture/ModifyVpcAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcAttribute)

responseModifyVpcEndpoint :: ModifyVpcEndpointResponse -> TestTree
responseModifyVpcEndpoint =
  res
    "ModifyVpcEndpointResponse"
    "fixture/ModifyVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpoint)

responseModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotificationResponse -> TestTree
responseModifyVpcEndpointConnectionNotification =
  res
    "ModifyVpcEndpointConnectionNotificationResponse"
    "fixture/ModifyVpcEndpointConnectionNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpointConnectionNotification)

responseModifyVpcEndpointServiceConfiguration :: ModifyVpcEndpointServiceConfigurationResponse -> TestTree
responseModifyVpcEndpointServiceConfiguration =
  res
    "ModifyVpcEndpointServiceConfigurationResponse"
    "fixture/ModifyVpcEndpointServiceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpointServiceConfiguration)

responseModifyVpcEndpointServicePayerResponsibility :: ModifyVpcEndpointServicePayerResponsibilityResponse -> TestTree
responseModifyVpcEndpointServicePayerResponsibility =
  res
    "ModifyVpcEndpointServicePayerResponsibilityResponse"
    "fixture/ModifyVpcEndpointServicePayerResponsibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpointServicePayerResponsibility)

responseModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissionsResponse -> TestTree
responseModifyVpcEndpointServicePermissions =
  res
    "ModifyVpcEndpointServicePermissionsResponse"
    "fixture/ModifyVpcEndpointServicePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcEndpointServicePermissions)

responseModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptionsResponse -> TestTree
responseModifyVpcPeeringConnectionOptions =
  res
    "ModifyVpcPeeringConnectionOptionsResponse"
    "fixture/ModifyVpcPeeringConnectionOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcPeeringConnectionOptions)

responseModifyVpcTenancy :: ModifyVpcTenancyResponse -> TestTree
responseModifyVpcTenancy =
  res
    "ModifyVpcTenancyResponse"
    "fixture/ModifyVpcTenancyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpcTenancy)

responseModifyVpnConnection :: ModifyVpnConnectionResponse -> TestTree
responseModifyVpnConnection =
  res
    "ModifyVpnConnectionResponse"
    "fixture/ModifyVpnConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpnConnection)

responseModifyVpnConnectionOptions :: ModifyVpnConnectionOptionsResponse -> TestTree
responseModifyVpnConnectionOptions =
  res
    "ModifyVpnConnectionOptionsResponse"
    "fixture/ModifyVpnConnectionOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpnConnectionOptions)

responseModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificateResponse -> TestTree
responseModifyVpnTunnelCertificate =
  res
    "ModifyVpnTunnelCertificateResponse"
    "fixture/ModifyVpnTunnelCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpnTunnelCertificate)

responseModifyVpnTunnelOptions :: ModifyVpnTunnelOptionsResponse -> TestTree
responseModifyVpnTunnelOptions =
  res
    "ModifyVpnTunnelOptionsResponse"
    "fixture/ModifyVpnTunnelOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyVpnTunnelOptions)

responseMonitorInstances :: MonitorInstancesResponse -> TestTree
responseMonitorInstances =
  res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MonitorInstances)

responseMoveAddressToVpc :: MoveAddressToVpcResponse -> TestTree
responseMoveAddressToVpc =
  res
    "MoveAddressToVpcResponse"
    "fixture/MoveAddressToVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MoveAddressToVpc)

responseMoveByoipCidrToIpam :: MoveByoipCidrToIpamResponse -> TestTree
responseMoveByoipCidrToIpam =
  res
    "MoveByoipCidrToIpamResponse"
    "fixture/MoveByoipCidrToIpamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MoveByoipCidrToIpam)

responseProvisionByoipCidr :: ProvisionByoipCidrResponse -> TestTree
responseProvisionByoipCidr =
  res
    "ProvisionByoipCidrResponse"
    "fixture/ProvisionByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionByoipCidr)

responseProvisionIpamPoolCidr :: ProvisionIpamPoolCidrResponse -> TestTree
responseProvisionIpamPoolCidr =
  res
    "ProvisionIpamPoolCidrResponse"
    "fixture/ProvisionIpamPoolCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionIpamPoolCidr)

responseProvisionPublicIpv4PoolCidr :: ProvisionPublicIpv4PoolCidrResponse -> TestTree
responseProvisionPublicIpv4PoolCidr =
  res
    "ProvisionPublicIpv4PoolCidrResponse"
    "fixture/ProvisionPublicIpv4PoolCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionPublicIpv4PoolCidr)

responsePurchaseHostReservation :: PurchaseHostReservationResponse -> TestTree
responsePurchaseHostReservation =
  res
    "PurchaseHostReservationResponse"
    "fixture/PurchaseHostReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseHostReservation)

responsePurchaseReservedInstancesOffering :: PurchaseReservedInstancesOfferingResponse -> TestTree
responsePurchaseReservedInstancesOffering =
  res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedInstancesOffering)

responsePurchaseScheduledInstances :: PurchaseScheduledInstancesResponse -> TestTree
responsePurchaseScheduledInstances =
  res
    "PurchaseScheduledInstancesResponse"
    "fixture/PurchaseScheduledInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseScheduledInstances)

responseRebootInstances :: RebootInstancesResponse -> TestTree
responseRebootInstances =
  res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootInstances)

responseRegisterImage :: RegisterImageResponse -> TestTree
responseRegisterImage =
  res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterImage)

responseRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributesResponse -> TestTree
responseRegisterInstanceEventNotificationAttributes =
  res
    "RegisterInstanceEventNotificationAttributesResponse"
    "fixture/RegisterInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterInstanceEventNotificationAttributes)

responseRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupMembers =
  res
    "RegisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/RegisterTransitGatewayMulticastGroupMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTransitGatewayMulticastGroupMembers)

responseRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupSources =
  res
    "RegisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/RegisterTransitGatewayMulticastGroupSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTransitGatewayMulticastGroupSources)

responseRejectTransitGatewayMulticastDomainAssociations :: RejectTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseRejectTransitGatewayMulticastDomainAssociations =
  res
    "RejectTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/RejectTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectTransitGatewayMulticastDomainAssociations)

responseRejectTransitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachmentResponse -> TestTree
responseRejectTransitGatewayPeeringAttachment =
  res
    "RejectTransitGatewayPeeringAttachmentResponse"
    "fixture/RejectTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectTransitGatewayPeeringAttachment)

responseRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachmentResponse -> TestTree
responseRejectTransitGatewayVpcAttachment =
  res
    "RejectTransitGatewayVpcAttachmentResponse"
    "fixture/RejectTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectTransitGatewayVpcAttachment)

responseRejectVpcEndpointConnections :: RejectVpcEndpointConnectionsResponse -> TestTree
responseRejectVpcEndpointConnections =
  res
    "RejectVpcEndpointConnectionsResponse"
    "fixture/RejectVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectVpcEndpointConnections)

responseRejectVpcPeeringConnection :: RejectVpcPeeringConnectionResponse -> TestTree
responseRejectVpcPeeringConnection =
  res
    "RejectVpcPeeringConnectionResponse"
    "fixture/RejectVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectVpcPeeringConnection)

responseReleaseAddress :: ReleaseAddressResponse -> TestTree
responseReleaseAddress =
  res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleaseAddress)

responseReleaseHosts :: ReleaseHostsResponse -> TestTree
responseReleaseHosts =
  res
    "ReleaseHostsResponse"
    "fixture/ReleaseHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleaseHosts)

responseReleaseIpamPoolAllocation :: ReleaseIpamPoolAllocationResponse -> TestTree
responseReleaseIpamPoolAllocation =
  res
    "ReleaseIpamPoolAllocationResponse"
    "fixture/ReleaseIpamPoolAllocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleaseIpamPoolAllocation)

responseReplaceIamInstanceProfileAssociation :: ReplaceIamInstanceProfileAssociationResponse -> TestTree
responseReplaceIamInstanceProfileAssociation =
  res
    "ReplaceIamInstanceProfileAssociationResponse"
    "fixture/ReplaceIamInstanceProfileAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceIamInstanceProfileAssociation)

responseReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociationResponse -> TestTree
responseReplaceNetworkAclAssociation =
  res
    "ReplaceNetworkAclAssociationResponse"
    "fixture/ReplaceNetworkAclAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceNetworkAclAssociation)

responseReplaceNetworkAclEntry :: ReplaceNetworkAclEntryResponse -> TestTree
responseReplaceNetworkAclEntry =
  res
    "ReplaceNetworkAclEntryResponse"
    "fixture/ReplaceNetworkAclEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceNetworkAclEntry)

responseReplaceRoute :: ReplaceRouteResponse -> TestTree
responseReplaceRoute =
  res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceRoute)

responseReplaceRouteTableAssociation :: ReplaceRouteTableAssociationResponse -> TestTree
responseReplaceRouteTableAssociation =
  res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceRouteTableAssociation)

responseReplaceTransitGatewayRoute :: ReplaceTransitGatewayRouteResponse -> TestTree
responseReplaceTransitGatewayRoute =
  res
    "ReplaceTransitGatewayRouteResponse"
    "fixture/ReplaceTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceTransitGatewayRoute)

responseReportInstanceStatus :: ReportInstanceStatusResponse -> TestTree
responseReportInstanceStatus =
  res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReportInstanceStatus)

responseRequestSpotFleet :: RequestSpotFleetResponse -> TestTree
responseRequestSpotFleet =
  res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestSpotFleet)

responseRequestSpotInstances :: RequestSpotInstancesResponse -> TestTree
responseRequestSpotInstances =
  res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestSpotInstances)

responseResetAddressAttribute :: ResetAddressAttributeResponse -> TestTree
responseResetAddressAttribute =
  res
    "ResetAddressAttributeResponse"
    "fixture/ResetAddressAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetAddressAttribute)

responseResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyIdResponse -> TestTree
responseResetEbsDefaultKmsKeyId =
  res
    "ResetEbsDefaultKmsKeyIdResponse"
    "fixture/ResetEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetEbsDefaultKmsKeyId)

responseResetFpgaImageAttribute :: ResetFpgaImageAttributeResponse -> TestTree
responseResetFpgaImageAttribute =
  res
    "ResetFpgaImageAttributeResponse"
    "fixture/ResetFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetFpgaImageAttribute)

responseResetImageAttribute :: ResetImageAttributeResponse -> TestTree
responseResetImageAttribute =
  res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetImageAttribute)

responseResetInstanceAttribute :: ResetInstanceAttributeResponse -> TestTree
responseResetInstanceAttribute =
  res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetInstanceAttribute)

responseResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttributeResponse -> TestTree
responseResetNetworkInterfaceAttribute =
  res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetNetworkInterfaceAttribute)

responseResetSnapshotAttribute :: ResetSnapshotAttributeResponse -> TestTree
responseResetSnapshotAttribute =
  res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetSnapshotAttribute)

responseRestoreAddressToClassic :: RestoreAddressToClassicResponse -> TestTree
responseRestoreAddressToClassic =
  res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreAddressToClassic)

responseRestoreImageFromRecycleBin :: RestoreImageFromRecycleBinResponse -> TestTree
responseRestoreImageFromRecycleBin =
  res
    "RestoreImageFromRecycleBinResponse"
    "fixture/RestoreImageFromRecycleBinResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreImageFromRecycleBin)

responseRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersionResponse -> TestTree
responseRestoreManagedPrefixListVersion =
  res
    "RestoreManagedPrefixListVersionResponse"
    "fixture/RestoreManagedPrefixListVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreManagedPrefixListVersion)

responseRestoreSnapshotFromRecycleBin :: RestoreSnapshotFromRecycleBinResponse -> TestTree
responseRestoreSnapshotFromRecycleBin =
  res
    "RestoreSnapshotFromRecycleBinResponse"
    "fixture/RestoreSnapshotFromRecycleBinResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreSnapshotFromRecycleBin)

responseRestoreSnapshotTier :: RestoreSnapshotTierResponse -> TestTree
responseRestoreSnapshotTier =
  res
    "RestoreSnapshotTierResponse"
    "fixture/RestoreSnapshotTierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreSnapshotTier)

responseRevokeClientVpnIngress :: RevokeClientVpnIngressResponse -> TestTree
responseRevokeClientVpnIngress =
  res
    "RevokeClientVpnIngressResponse"
    "fixture/RevokeClientVpnIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeClientVpnIngress)

responseRevokeSecurityGroupEgress :: RevokeSecurityGroupEgressResponse -> TestTree
responseRevokeSecurityGroupEgress =
  res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSecurityGroupEgress)

responseRevokeSecurityGroupIngress :: RevokeSecurityGroupIngressResponse -> TestTree
responseRevokeSecurityGroupIngress =
  res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSecurityGroupIngress)

responseRunInstances :: Reservation -> TestTree
responseRunInstances =
  res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunInstances)

responseRunScheduledInstances :: RunScheduledInstancesResponse -> TestTree
responseRunScheduledInstances =
  res
    "RunScheduledInstancesResponse"
    "fixture/RunScheduledInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunScheduledInstances)

responseSearchLocalGatewayRoutes :: SearchLocalGatewayRoutesResponse -> TestTree
responseSearchLocalGatewayRoutes =
  res
    "SearchLocalGatewayRoutesResponse"
    "fixture/SearchLocalGatewayRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchLocalGatewayRoutes)

responseSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroupsResponse -> TestTree
responseSearchTransitGatewayMulticastGroups =
  res
    "SearchTransitGatewayMulticastGroupsResponse"
    "fixture/SearchTransitGatewayMulticastGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTransitGatewayMulticastGroups)

responseSearchTransitGatewayRoutes :: SearchTransitGatewayRoutesResponse -> TestTree
responseSearchTransitGatewayRoutes =
  res
    "SearchTransitGatewayRoutesResponse"
    "fixture/SearchTransitGatewayRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTransitGatewayRoutes)

responseSendDiagnosticInterrupt :: SendDiagnosticInterruptResponse -> TestTree
responseSendDiagnosticInterrupt =
  res
    "SendDiagnosticInterruptResponse"
    "fixture/SendDiagnosticInterruptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendDiagnosticInterrupt)

responseStartInstances :: StartInstancesResponse -> TestTree
responseStartInstances =
  res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstances)

responseStartNetworkInsightsAccessScopeAnalysis :: StartNetworkInsightsAccessScopeAnalysisResponse -> TestTree
responseStartNetworkInsightsAccessScopeAnalysis =
  res
    "StartNetworkInsightsAccessScopeAnalysisResponse"
    "fixture/StartNetworkInsightsAccessScopeAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNetworkInsightsAccessScopeAnalysis)

responseStartNetworkInsightsAnalysis :: StartNetworkInsightsAnalysisResponse -> TestTree
responseStartNetworkInsightsAnalysis =
  res
    "StartNetworkInsightsAnalysisResponse"
    "fixture/StartNetworkInsightsAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNetworkInsightsAnalysis)

responseStartVpcEndpointServicePrivateDnsVerification :: StartVpcEndpointServicePrivateDnsVerificationResponse -> TestTree
responseStartVpcEndpointServicePrivateDnsVerification =
  res
    "StartVpcEndpointServicePrivateDnsVerificationResponse"
    "fixture/StartVpcEndpointServicePrivateDnsVerificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartVpcEndpointServicePrivateDnsVerification)

responseStopInstances :: StopInstancesResponse -> TestTree
responseStopInstances =
  res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInstances)

responseTerminateClientVpnConnections :: TerminateClientVpnConnectionsResponse -> TestTree
responseTerminateClientVpnConnections =
  res
    "TerminateClientVpnConnectionsResponse"
    "fixture/TerminateClientVpnConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateClientVpnConnections)

responseTerminateInstances :: TerminateInstancesResponse -> TestTree
responseTerminateInstances =
  res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateInstances)

responseUnassignIpv6Addresses :: UnassignIpv6AddressesResponse -> TestTree
responseUnassignIpv6Addresses =
  res
    "UnassignIpv6AddressesResponse"
    "fixture/UnassignIpv6AddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnassignIpv6Addresses)

responseUnassignPrivateIpAddresses :: UnassignPrivateIpAddressesResponse -> TestTree
responseUnassignPrivateIpAddresses =
  res
    "UnassignPrivateIpAddressesResponse"
    "fixture/UnassignPrivateIpAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnassignPrivateIpAddresses)

responseUnmonitorInstances :: UnmonitorInstancesResponse -> TestTree
responseUnmonitorInstances =
  res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnmonitorInstances)

responseUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsEgress =
  res
    "UpdateSecurityGroupRuleDescriptionsEgressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityGroupRuleDescriptionsEgress)

responseUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsIngress =
  res
    "UpdateSecurityGroupRuleDescriptionsIngressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityGroupRuleDescriptionsIngress)

responseWithdrawByoipCidr :: WithdrawByoipCidrResponse -> TestTree
responseWithdrawByoipCidr =
  res
    "WithdrawByoipCidrResponse"
    "fixture/WithdrawByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy WithdrawByoipCidr)
