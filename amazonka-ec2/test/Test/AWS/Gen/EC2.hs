{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EC2
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.EC2 where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.EC2
import Test.AWS.EC2.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testImportInstance $
--             importInstance
--
--         , testRevokeSecurityGroupEgress $
--             revokeSecurityGroupEgress
--
--         , testCreateVPNGateway $
--             createVPNGateway
--
--         , testCreateNetworkACL $
--             createNetworkACL
--
--         , testDeleteKeyPair $
--             deleteKeyPair
--
--         , testDescribeTags $
--             describeTags
--
--         , testDetachNetworkInterface $
--             detachNetworkInterface
--
--         , testDetachInternetGateway $
--             detachInternetGateway
--
--         , testDeleteVPCEndpoints $
--             deleteVPCEndpoints
--
--         , testDeleteFlowLogs $
--             deleteFlowLogs
--
--         , testDescribeVPCClassicLink $
--             describeVPCClassicLink
--
--         , testModifySubnetAttribute $
--             modifySubnetAttribute
--
--         , testDetachVolume $
--             detachVolume
--
--         , testCancelBundleTask $
--             cancelBundleTask
--
--         , testReleaseAddress $
--             releaseAddress
--
--         , testCreateInternetGateway $
--             createInternetGateway
--
--         , testDeleteVPNConnection $
--             deleteVPNConnection
--
--         , testDescribeBundleTasks $
--             describeBundleTasks
--
--         , testAuthorizeSecurityGroupEgress $
--             authorizeSecurityGroupEgress
--
--         , testDeregisterImage $
--             deregisterImage
--
--         , testModifyNetworkInterfaceAttribute $
--             modifyNetworkInterfaceAttribute
--
--         , testCancelReservedInstancesListing $
--             cancelReservedInstancesListing
--
--         , testAttachClassicLinkVPC $
--             attachClassicLinkVPC
--
--         , testDescribeVPCClassicLinkDNSSupport $
--             describeVPCClassicLinkDNSSupport
--
--         , testRunScheduledInstances $
--             runScheduledInstances
--
--         , testCancelSpotFleetRequests $
--             cancelSpotFleetRequests
--
--         , testDescribeSpotPriceHistory $
--             describeSpotPriceHistory
--
--         , testDescribeDHCPOptions $
--             describeDHCPOptions
--
--         , testImportImage $
--             importImage
--
--         , testStopInstances $
--             stopInstances
--
--         , testDescribeInternetGateways $
--             describeInternetGateways
--
--         , testDisableVPCClassicLink $
--             disableVPCClassicLink
--
--         , testBundleInstance $
--             bundleInstance
--
--         , testDescribeNetworkInterfaces $
--             describeNetworkInterfaces
--
--         , testReplaceNetworkACLAssociation $
--             replaceNetworkACLAssociation
--
--         , testDescribeNatGateways $
--             describeNatGateways
--
--         , testDescribeAddresses $
--             describeAddresses
--
--         , testDescribeSnapshotAttribute $
--             describeSnapshotAttribute
--
--         , testReplaceRoute $
--             replaceRoute
--
--         , testDescribeVPCEndpointServices $
--             describeVPCEndpointServices
--
--         , testAuthorizeSecurityGroupIngress $
--             authorizeSecurityGroupIngress
--
--         , testCreateVPCPeeringConnection $
--             createVPCPeeringConnection
--
--         , testDescribeSubnets $
--             describeSubnets
--
--         , testCreateTags $
--             createTags
--
--         , testPurchaseReservedInstancesOffering $
--             purchaseReservedInstancesOffering
--
--         , testDeleteNetworkACLEntry $
--             deleteNetworkACLEntry
--
--         , testResetSnapshotAttribute $
--             resetSnapshotAttribute
--
--         , testDescribeVPNConnections $
--             describeVPNConnections
--
--         , testDeleteRoute $
--             deleteRoute
--
--         , testReplaceNetworkACLEntry $
--             replaceNetworkACLEntry
--
--         , testDescribeVPCEndpoints $
--             describeVPCEndpoints
--
--         , testResetInstanceAttribute $
--             resetInstanceAttribute
--
--         , testAttachNetworkInterface $
--             attachNetworkInterface
--
--         , testDescribeInstanceStatus $
--             describeInstanceStatus
--
--         , testImportKeyPair $
--             importKeyPair
--
--         , testDeleteTags $
--             deleteTags
--
--         , testConfirmProductInstance $
--             confirmProductInstance
--
--         , testDescribeInstanceAttribute $
--             describeInstanceAttribute
--
--         , testDescribeReservedInstancesOfferings $
--             describeReservedInstancesOfferings
--
--         , testCreateCustomerGateway $
--             createCustomerGateway
--
--         , testDeleteSecurityGroup $
--             deleteSecurityGroup
--
--         , testDeleteVPCPeeringConnection $
--             deleteVPCPeeringConnection
--
--         , testAttachInternetGateway $
--             attachInternetGateway
--
--         , testModifyInstancePlacement $
--             modifyInstancePlacement
--
--         , testDescribeFlowLogs $
--             describeFlowLogs
--
--         , testRunInstances $
--             runInstances
--
--         , testAssociateDHCPOptions $
--             associateDHCPOptions
--
--         , testDescribeReservedInstances $
--             describeReservedInstances
--
--         , testDescribeIdFormat $
--             describeIdFormat
--
--         , testDescribeVPCs $
--             describeVPCs
--
--         , testDescribeConversionTasks $
--             describeConversionTasks
--
--         , testDisableVPCClassicLinkDNSSupport $
--             disableVPCClassicLinkDNSSupport
--
--         , testAllocateAddress $
--             allocateAddress
--
--         , testCancelConversionTask $
--             cancelConversionTask
--
--         , testModifyImageAttribute $
--             modifyImageAttribute
--
--         , testCreateRouteTable $
--             createRouteTable
--
--         , testReportInstanceStatus $
--             reportInstanceStatus
--
--         , testAttachVolume $
--             attachVolume
--
--         , testRequestSpotInstances $
--             requestSpotInstances
--
--         , testDescribeVolumes $
--             describeVolumes
--
--         , testRejectVPCPeeringConnection $
--             rejectVPCPeeringConnection
--
--         , testDeleteVPNConnectionRoute $
--             deleteVPNConnectionRoute
--
--         , testModifyVPCEndpoint $
--             modifyVPCEndpoint
--
--         , testAllocateHosts $
--             allocateHosts
--
--         , testRegisterImage $
--             registerImage
--
--         , testRevokeSecurityGroupIngress $
--             revokeSecurityGroupIngress
--
--         , testEnableVPCClassicLinkDNSSupport $
--             enableVPCClassicLinkDNSSupport
--
--         , testModifyReservedInstances $
--             modifyReservedInstances
--
--         , testDescribeScheduledInstances $
--             describeScheduledInstances
--
--         , testCreateFlowLogs $
--             createFlowLogs
--
--         , testDescribeSpotFleetRequests $
--             describeSpotFleetRequests
--
--         , testMoveAddressToVPC $
--             moveAddressToVPC
--
--         , testDeleteNetworkACL $
--             deleteNetworkACL
--
--         , testDeleteVPNGateway $
--             deleteVPNGateway
--
--         , testDescribeImportImageTasks $
--             describeImportImageTasks
--
--         , testDescribeVolumeAttribute $
--             describeVolumeAttribute
--
--         , testDescribeMovingAddresses $
--             describeMovingAddresses
--
--         , testGetPasswordData $
--             getPasswordData
--
--         , testCreateVPC $
--             createVPC
--
--         , testCopySnapshot $
--             copySnapshot
--
--         , testDisassociateAddress $
--             disassociateAddress
--
--         , testDeleteVPC $
--             deleteVPC
--
--         , testCreateInstanceExportTask $
--             createInstanceExportTask
--
--         , testDescribeVPCAttribute $
--             describeVPCAttribute
--
--         , testCreateVolume $
--             createVolume
--
--         , testDescribeScheduledInstanceAvailability $
--             describeScheduledInstanceAvailability
--
--         , testModifyVolumeAttribute $
--             modifyVolumeAttribute
--
--         , testDescribeSpotDatafeedSubscription $
--             describeSpotDatafeedSubscription
--
--         , testDescribePrefixLists $
--             describePrefixLists
--
--         , testDeletePlacementGroup $
--             deletePlacementGroup
--
--         , testRequestSpotFleet $
--             requestSpotFleet
--
--         , testCreateSubnet $
--             createSubnet
--
--         , testCreateNetworkInterface $
--             createNetworkInterface
--
--         , testDescribeSecurityGroups $
--             describeSecurityGroups
--
--         , testDescribeExportTasks $
--             describeExportTasks
--
--         , testModifySpotFleetRequest $
--             modifySpotFleetRequest
--
--         , testDetachVPNGateway $
--             detachVPNGateway
--
--         , testEnableVolumeIO $
--             enableVolumeIO
--
--         , testDescribeInstances $
--             describeInstances
--
--         , testCreateNatGateway $
--             createNatGateway
--
--         , testDescribeVPCPeeringConnections $
--             describeVPCPeeringConnections
--
--         , testCancelExportTask $
--             cancelExportTask
--
--         , testDeleteNetworkInterface $
--             deleteNetworkInterface
--
--         , testReplaceRouteTableAssociation $
--             replaceRouteTableAssociation
--
--         , testStartInstances $
--             startInstances
--
--         , testCreatePlacementGroup $
--             createPlacementGroup
--
--         , testDescribeSnapshots $
--             describeSnapshots
--
--         , testAssociateAddress $
--             associateAddress
--
--         , testDescribeNetworkInterfaceAttribute $
--             describeNetworkInterfaceAttribute
--
--         , testReleaseHosts $
--             releaseHosts
--
--         , testResetNetworkInterfaceAttribute $
--             resetNetworkInterfaceAttribute
--
--         , testDeleteInternetGateway $
--             deleteInternetGateway
--
--         , testDescribeReservedInstancesListings $
--             describeReservedInstancesListings
--
--         , testCreateVPNConnection $
--             createVPNConnection
--
--         , testDeleteNatGateway $
--             deleteNatGateway
--
--         , testDescribeImportSnapshotTasks $
--             describeImportSnapshotTasks
--
--         , testDescribeCustomerGateways $
--             describeCustomerGateways
--
--         , testDeleteSubnet $
--             deleteSubnet
--
--         , testCopyImage $
--             copyImage
--
--         , testCreateVPCEndpoint $
--             createVPCEndpoint
--
--         , testUnmonitorInstances $
--             unmonitorInstances
--
--         , testCreateSecurityGroup $
--             createSecurityGroup
--
--         , testImportVolume $
--             importVolume
--
--         , testDisableVGWRoutePropagation $
--             disableVGWRoutePropagation
--
--         , testCreateSpotDatafeedSubscription $
--             createSpotDatafeedSubscription
--
--         , testCancelSpotInstanceRequests $
--             cancelSpotInstanceRequests
--
--         , testCreateRoute $
--             createRoute
--
--         , testDeleteSnapshot $
--             deleteSnapshot
--
--         , testAssignPrivateIPAddresses $
--             assignPrivateIPAddresses
--
--         , testModifyInstanceAttribute $
--             modifyInstanceAttribute
--
--         , testDeleteCustomerGateway $
--             deleteCustomerGateway
--
--         , testDisassociateRouteTable $
--             disassociateRouteTable
--
--         , testDeleteSpotDatafeedSubscription $
--             deleteSpotDatafeedSubscription
--
--         , testEnableVPCClassicLink $
--             enableVPCClassicLink
--
--         , testDescribePlacementGroups $
--             describePlacementGroups
--
--         , testPurchaseScheduledInstances $
--             purchaseScheduledInstances
--
--         , testEnableVGWRoutePropagation $
--             enableVGWRoutePropagation
--
--         , testDescribeSpotFleetRequestHistory $
--             describeSpotFleetRequestHistory
--
--         , testModifySnapshotAttribute $
--             modifySnapshotAttribute
--
--         , testCreateSnapshot $
--             createSnapshot
--
--         , testCreateNetworkACLEntry $
--             createNetworkACLEntry
--
--         , testCreateReservedInstancesListing $
--             createReservedInstancesListing
--
--         , testAttachVPNGateway $
--             attachVPNGateway
--
--         , testCreateDHCPOptions $
--             createDHCPOptions
--
--         , testDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , testModifyHosts $
--             modifyHosts
--
--         , testRebootInstances $
--             rebootInstances
--
--         , testCreateImage $
--             createImage
--
--         , testDescribeClassicLinkInstances $
--             describeClassicLinkInstances
--
--         , testTerminateInstances $
--             terminateInstances
--
--         , testDescribeKeyPairs $
--             describeKeyPairs
--
--         , testCreateVPNConnectionRoute $
--             createVPNConnectionRoute
--
--         , testAssociateRouteTable $
--             associateRouteTable
--
--         , testDescribeVPNGateways $
--             describeVPNGateways
--
--         , testGetConsoleOutput $
--             getConsoleOutput
--
--         , testDescribeHosts $
--             describeHosts
--
--         , testDescribeImageAttribute $
--             describeImageAttribute
--
--         , testModifyIdFormat $
--             modifyIdFormat
--
--         , testDeleteRouteTable $
--             deleteRouteTable
--
--         , testResetImageAttribute $
--             resetImageAttribute
--
--         , testDescribeReservedInstancesModifications $
--             describeReservedInstancesModifications
--
--         , testDescribeSpotInstanceRequests $
--             describeSpotInstanceRequests
--
--         , testUnassignPrivateIPAddresses $
--             unassignPrivateIPAddresses
--
--         , testDeleteDHCPOptions $
--             deleteDHCPOptions
--
--         , testDescribeNetworkACLs $
--             describeNetworkACLs
--
--         , testCancelImportTask $
--             cancelImportTask
--
--         , testDetachClassicLinkVPC $
--             detachClassicLinkVPC
--
--         , testDescribeRegions $
--             describeRegions
--
--         , testMonitorInstances $
--             monitorInstances
--
--         , testAcceptVPCPeeringConnection $
--             acceptVPCPeeringConnection
--
--         , testImportSnapshot $
--             importSnapshot
--
--         , testDescribeVolumeStatus $
--             describeVolumeStatus
--
--         , testDescribeRouteTables $
--             describeRouteTables
--
--         , testDescribeAvailabilityZones $
--             describeAvailabilityZones
--
--         , testModifyVPCAttribute $
--             modifyVPCAttribute
--
--         , testDescribeImages $
--             describeImages
--
--         , testRestoreAddressToClassic $
--             restoreAddressToClassic
--
--         , testCreateKeyPair $
--             createKeyPair
--
--         , testDeleteVolume $
--             deleteVolume
--
--         , testDescribeSpotFleetInstances $
--             describeSpotFleetInstances
--
--           ]

--     , testGroup "response"
--         [ testImportInstanceResponse $
--             importInstanceResponse
--
--         , testRevokeSecurityGroupEgressResponse $
--             revokeSecurityGroupEgressResponse
--
--         , testCreateVPNGatewayResponse $
--             createVPNGatewayResponse
--
--         , testCreateNetworkACLResponse $
--             createNetworkACLResponse
--
--         , testDeleteKeyPairResponse $
--             deleteKeyPairResponse
--
--         , testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testDetachNetworkInterfaceResponse $
--             detachNetworkInterfaceResponse
--
--         , testDetachInternetGatewayResponse $
--             detachInternetGatewayResponse
--
--         , testDeleteVPCEndpointsResponse $
--             deleteVPCEndpointsResponse
--
--         , testDeleteFlowLogsResponse $
--             deleteFlowLogsResponse
--
--         , testDescribeVPCClassicLinkResponse $
--             describeVPCClassicLinkResponse
--
--         , testModifySubnetAttributeResponse $
--             modifySubnetAttributeResponse
--
--         , testDetachVolumeResponse $
--             volumeAttachment
--
--         , testCancelBundleTaskResponse $
--             cancelBundleTaskResponse
--
--         , testReleaseAddressResponse $
--             releaseAddressResponse
--
--         , testCreateInternetGatewayResponse $
--             createInternetGatewayResponse
--
--         , testDeleteVPNConnectionResponse $
--             deleteVPNConnectionResponse
--
--         , testDescribeBundleTasksResponse $
--             describeBundleTasksResponse
--
--         , testAuthorizeSecurityGroupEgressResponse $
--             authorizeSecurityGroupEgressResponse
--
--         , testDeregisterImageResponse $
--             deregisterImageResponse
--
--         , testModifyNetworkInterfaceAttributeResponse $
--             modifyNetworkInterfaceAttributeResponse
--
--         , testCancelReservedInstancesListingResponse $
--             cancelReservedInstancesListingResponse
--
--         , testAttachClassicLinkVPCResponse $
--             attachClassicLinkVPCResponse
--
--         , testDescribeVPCClassicLinkDNSSupportResponse $
--             describeVPCClassicLinkDNSSupportResponse
--
--         , testRunScheduledInstancesResponse $
--             runScheduledInstancesResponse
--
--         , testCancelSpotFleetRequestsResponse $
--             cancelSpotFleetRequestsResponse
--
--         , testDescribeSpotPriceHistoryResponse $
--             describeSpotPriceHistoryResponse
--
--         , testDescribeDHCPOptionsResponse $
--             describeDHCPOptionsResponse
--
--         , testImportImageResponse $
--             importImageResponse
--
--         , testStopInstancesResponse $
--             stopInstancesResponse
--
--         , testDescribeInternetGatewaysResponse $
--             describeInternetGatewaysResponse
--
--         , testDisableVPCClassicLinkResponse $
--             disableVPCClassicLinkResponse
--
--         , testBundleInstanceResponse $
--             bundleInstanceResponse
--
--         , testDescribeNetworkInterfacesResponse $
--             describeNetworkInterfacesResponse
--
--         , testReplaceNetworkACLAssociationResponse $
--             replaceNetworkACLAssociationResponse
--
--         , testDescribeNatGatewaysResponse $
--             describeNatGatewaysResponse
--
--         , testDescribeAddressesResponse $
--             describeAddressesResponse
--
--         , testDescribeSnapshotAttributeResponse $
--             describeSnapshotAttributeResponse
--
--         , testReplaceRouteResponse $
--             replaceRouteResponse
--
--         , testDescribeVPCEndpointServicesResponse $
--             describeVPCEndpointServicesResponse
--
--         , testAuthorizeSecurityGroupIngressResponse $
--             authorizeSecurityGroupIngressResponse
--
--         , testCreateVPCPeeringConnectionResponse $
--             createVPCPeeringConnectionResponse
--
--         , testDescribeSubnetsResponse $
--             describeSubnetsResponse
--
--         , testCreateTagsResponse $
--             createTagsResponse
--
--         , testPurchaseReservedInstancesOfferingResponse $
--             purchaseReservedInstancesOfferingResponse
--
--         , testDeleteNetworkACLEntryResponse $
--             deleteNetworkACLEntryResponse
--
--         , testResetSnapshotAttributeResponse $
--             resetSnapshotAttributeResponse
--
--         , testDescribeVPNConnectionsResponse $
--             describeVPNConnectionsResponse
--
--         , testDeleteRouteResponse $
--             deleteRouteResponse
--
--         , testReplaceNetworkACLEntryResponse $
--             replaceNetworkACLEntryResponse
--
--         , testDescribeVPCEndpointsResponse $
--             describeVPCEndpointsResponse
--
--         , testResetInstanceAttributeResponse $
--             resetInstanceAttributeResponse
--
--         , testAttachNetworkInterfaceResponse $
--             attachNetworkInterfaceResponse
--
--         , testDescribeInstanceStatusResponse $
--             describeInstanceStatusResponse
--
--         , testImportKeyPairResponse $
--             importKeyPairResponse
--
--         , testDeleteTagsResponse $
--             deleteTagsResponse
--
--         , testConfirmProductInstanceResponse $
--             confirmProductInstanceResponse
--
--         , testDescribeInstanceAttributeResponse $
--             describeInstanceAttributeResponse
--
--         , testDescribeReservedInstancesOfferingsResponse $
--             describeReservedInstancesOfferingsResponse
--
--         , testCreateCustomerGatewayResponse $
--             createCustomerGatewayResponse
--
--         , testDeleteSecurityGroupResponse $
--             deleteSecurityGroupResponse
--
--         , testDeleteVPCPeeringConnectionResponse $
--             deleteVPCPeeringConnectionResponse
--
--         , testAttachInternetGatewayResponse $
--             attachInternetGatewayResponse
--
--         , testModifyInstancePlacementResponse $
--             modifyInstancePlacementResponse
--
--         , testDescribeFlowLogsResponse $
--             describeFlowLogsResponse
--
--         , testRunInstancesResponse $
--             reservation
--
--         , testAssociateDHCPOptionsResponse $
--             associateDHCPOptionsResponse
--
--         , testDescribeReservedInstancesResponse $
--             describeReservedInstancesResponse
--
--         , testDescribeIdFormatResponse $
--             describeIdFormatResponse
--
--         , testDescribeVPCsResponse $
--             describeVPCsResponse
--
--         , testDescribeConversionTasksResponse $
--             describeConversionTasksResponse
--
--         , testDisableVPCClassicLinkDNSSupportResponse $
--             disableVPCClassicLinkDNSSupportResponse
--
--         , testAllocateAddressResponse $
--             allocateAddressResponse
--
--         , testCancelConversionTaskResponse $
--             cancelConversionTaskResponse
--
--         , testModifyImageAttributeResponse $
--             modifyImageAttributeResponse
--
--         , testCreateRouteTableResponse $
--             createRouteTableResponse
--
--         , testReportInstanceStatusResponse $
--             reportInstanceStatusResponse
--
--         , testAttachVolumeResponse $
--             volumeAttachment
--
--         , testRequestSpotInstancesResponse $
--             requestSpotInstancesResponse
--
--         , testDescribeVolumesResponse $
--             describeVolumesResponse
--
--         , testRejectVPCPeeringConnectionResponse $
--             rejectVPCPeeringConnectionResponse
--
--         , testDeleteVPNConnectionRouteResponse $
--             deleteVPNConnectionRouteResponse
--
--         , testModifyVPCEndpointResponse $
--             modifyVPCEndpointResponse
--
--         , testAllocateHostsResponse $
--             allocateHostsResponse
--
--         , testRegisterImageResponse $
--             registerImageResponse
--
--         , testRevokeSecurityGroupIngressResponse $
--             revokeSecurityGroupIngressResponse
--
--         , testEnableVPCClassicLinkDNSSupportResponse $
--             enableVPCClassicLinkDNSSupportResponse
--
--         , testModifyReservedInstancesResponse $
--             modifyReservedInstancesResponse
--
--         , testDescribeScheduledInstancesResponse $
--             describeScheduledInstancesResponse
--
--         , testCreateFlowLogsResponse $
--             createFlowLogsResponse
--
--         , testDescribeSpotFleetRequestsResponse $
--             describeSpotFleetRequestsResponse
--
--         , testMoveAddressToVPCResponse $
--             moveAddressToVPCResponse
--
--         , testDeleteNetworkACLResponse $
--             deleteNetworkACLResponse
--
--         , testDeleteVPNGatewayResponse $
--             deleteVPNGatewayResponse
--
--         , testDescribeImportImageTasksResponse $
--             describeImportImageTasksResponse
--
--         , testDescribeVolumeAttributeResponse $
--             describeVolumeAttributeResponse
--
--         , testDescribeMovingAddressesResponse $
--             describeMovingAddressesResponse
--
--         , testGetPasswordDataResponse $
--             getPasswordDataResponse
--
--         , testCreateVPCResponse $
--             createVPCResponse
--
--         , testCopySnapshotResponse $
--             copySnapshotResponse
--
--         , testDisassociateAddressResponse $
--             disassociateAddressResponse
--
--         , testDeleteVPCResponse $
--             deleteVPCResponse
--
--         , testCreateInstanceExportTaskResponse $
--             createInstanceExportTaskResponse
--
--         , testDescribeVPCAttributeResponse $
--             describeVPCAttributeResponse
--
--         , testCreateVolumeResponse $
--             volume
--
--         , testDescribeScheduledInstanceAvailabilityResponse $
--             describeScheduledInstanceAvailabilityResponse
--
--         , testModifyVolumeAttributeResponse $
--             modifyVolumeAttributeResponse
--
--         , testDescribeSpotDatafeedSubscriptionResponse $
--             describeSpotDatafeedSubscriptionResponse
--
--         , testDescribePrefixListsResponse $
--             describePrefixListsResponse
--
--         , testDeletePlacementGroupResponse $
--             deletePlacementGroupResponse
--
--         , testRequestSpotFleetResponse $
--             requestSpotFleetResponse
--
--         , testCreateSubnetResponse $
--             createSubnetResponse
--
--         , testCreateNetworkInterfaceResponse $
--             createNetworkInterfaceResponse
--
--         , testDescribeSecurityGroupsResponse $
--             describeSecurityGroupsResponse
--
--         , testDescribeExportTasksResponse $
--             describeExportTasksResponse
--
--         , testModifySpotFleetRequestResponse $
--             modifySpotFleetRequestResponse
--
--         , testDetachVPNGatewayResponse $
--             detachVPNGatewayResponse
--
--         , testEnableVolumeIOResponse $
--             enableVolumeIOResponse
--
--         , testDescribeInstancesResponse $
--             describeInstancesResponse
--
--         , testCreateNatGatewayResponse $
--             createNatGatewayResponse
--
--         , testDescribeVPCPeeringConnectionsResponse $
--             describeVPCPeeringConnectionsResponse
--
--         , testCancelExportTaskResponse $
--             cancelExportTaskResponse
--
--         , testDeleteNetworkInterfaceResponse $
--             deleteNetworkInterfaceResponse
--
--         , testReplaceRouteTableAssociationResponse $
--             replaceRouteTableAssociationResponse
--
--         , testStartInstancesResponse $
--             startInstancesResponse
--
--         , testCreatePlacementGroupResponse $
--             createPlacementGroupResponse
--
--         , testDescribeSnapshotsResponse $
--             describeSnapshotsResponse
--
--         , testAssociateAddressResponse $
--             associateAddressResponse
--
--         , testDescribeNetworkInterfaceAttributeResponse $
--             describeNetworkInterfaceAttributeResponse
--
--         , testReleaseHostsResponse $
--             releaseHostsResponse
--
--         , testResetNetworkInterfaceAttributeResponse $
--             resetNetworkInterfaceAttributeResponse
--
--         , testDeleteInternetGatewayResponse $
--             deleteInternetGatewayResponse
--
--         , testDescribeReservedInstancesListingsResponse $
--             describeReservedInstancesListingsResponse
--
--         , testCreateVPNConnectionResponse $
--             createVPNConnectionResponse
--
--         , testDeleteNatGatewayResponse $
--             deleteNatGatewayResponse
--
--         , testDescribeImportSnapshotTasksResponse $
--             describeImportSnapshotTasksResponse
--
--         , testDescribeCustomerGatewaysResponse $
--             describeCustomerGatewaysResponse
--
--         , testDeleteSubnetResponse $
--             deleteSubnetResponse
--
--         , testCopyImageResponse $
--             copyImageResponse
--
--         , testCreateVPCEndpointResponse $
--             createVPCEndpointResponse
--
--         , testUnmonitorInstancesResponse $
--             unmonitorInstancesResponse
--
--         , testCreateSecurityGroupResponse $
--             createSecurityGroupResponse
--
--         , testImportVolumeResponse $
--             importVolumeResponse
--
--         , testDisableVGWRoutePropagationResponse $
--             disableVGWRoutePropagationResponse
--
--         , testCreateSpotDatafeedSubscriptionResponse $
--             createSpotDatafeedSubscriptionResponse
--
--         , testCancelSpotInstanceRequestsResponse $
--             cancelSpotInstanceRequestsResponse
--
--         , testCreateRouteResponse $
--             createRouteResponse
--
--         , testDeleteSnapshotResponse $
--             deleteSnapshotResponse
--
--         , testAssignPrivateIPAddressesResponse $
--             assignPrivateIPAddressesResponse
--
--         , testModifyInstanceAttributeResponse $
--             modifyInstanceAttributeResponse
--
--         , testDeleteCustomerGatewayResponse $
--             deleteCustomerGatewayResponse
--
--         , testDisassociateRouteTableResponse $
--             disassociateRouteTableResponse
--
--         , testDeleteSpotDatafeedSubscriptionResponse $
--             deleteSpotDatafeedSubscriptionResponse
--
--         , testEnableVPCClassicLinkResponse $
--             enableVPCClassicLinkResponse
--
--         , testDescribePlacementGroupsResponse $
--             describePlacementGroupsResponse
--
--         , testPurchaseScheduledInstancesResponse $
--             purchaseScheduledInstancesResponse
--
--         , testEnableVGWRoutePropagationResponse $
--             enableVGWRoutePropagationResponse
--
--         , testDescribeSpotFleetRequestHistoryResponse $
--             describeSpotFleetRequestHistoryResponse
--
--         , testModifySnapshotAttributeResponse $
--             modifySnapshotAttributeResponse
--
--         , testCreateSnapshotResponse $
--             snapshot
--
--         , testCreateNetworkACLEntryResponse $
--             createNetworkACLEntryResponse
--
--         , testCreateReservedInstancesListingResponse $
--             createReservedInstancesListingResponse
--
--         , testAttachVPNGatewayResponse $
--             attachVPNGatewayResponse
--
--         , testCreateDHCPOptionsResponse $
--             createDHCPOptionsResponse
--
--         , testDescribeAccountAttributesResponse $
--             describeAccountAttributesResponse
--
--         , testModifyHostsResponse $
--             modifyHostsResponse
--
--         , testRebootInstancesResponse $
--             rebootInstancesResponse
--
--         , testCreateImageResponse $
--             createImageResponse
--
--         , testDescribeClassicLinkInstancesResponse $
--             describeClassicLinkInstancesResponse
--
--         , testTerminateInstancesResponse $
--             terminateInstancesResponse
--
--         , testDescribeKeyPairsResponse $
--             describeKeyPairsResponse
--
--         , testCreateVPNConnectionRouteResponse $
--             createVPNConnectionRouteResponse
--
--         , testAssociateRouteTableResponse $
--             associateRouteTableResponse
--
--         , testDescribeVPNGatewaysResponse $
--             describeVPNGatewaysResponse
--
--         , testGetConsoleOutputResponse $
--             getConsoleOutputResponse
--
--         , testDescribeHostsResponse $
--             describeHostsResponse
--
--         , testDescribeImageAttributeResponse $
--             describeImageAttributeResponse
--
--         , testModifyIdFormatResponse $
--             modifyIdFormatResponse
--
--         , testDeleteRouteTableResponse $
--             deleteRouteTableResponse
--
--         , testResetImageAttributeResponse $
--             resetImageAttributeResponse
--
--         , testDescribeReservedInstancesModificationsResponse $
--             describeReservedInstancesModificationsResponse
--
--         , testDescribeSpotInstanceRequestsResponse $
--             describeSpotInstanceRequestsResponse
--
--         , testUnassignPrivateIPAddressesResponse $
--             unassignPrivateIPAddressesResponse
--
--         , testDeleteDHCPOptionsResponse $
--             deleteDHCPOptionsResponse
--
--         , testDescribeNetworkACLsResponse $
--             describeNetworkACLsResponse
--
--         , testCancelImportTaskResponse $
--             cancelImportTaskResponse
--
--         , testDetachClassicLinkVPCResponse $
--             detachClassicLinkVPCResponse
--
--         , testDescribeRegionsResponse $
--             describeRegionsResponse
--
--         , testMonitorInstancesResponse $
--             monitorInstancesResponse
--
--         , testAcceptVPCPeeringConnectionResponse $
--             acceptVPCPeeringConnectionResponse
--
--         , testImportSnapshotResponse $
--             importSnapshotResponse
--
--         , testDescribeVolumeStatusResponse $
--             describeVolumeStatusResponse
--
--         , testDescribeRouteTablesResponse $
--             describeRouteTablesResponse
--
--         , testDescribeAvailabilityZonesResponse $
--             describeAvailabilityZonesResponse
--
--         , testModifyVPCAttributeResponse $
--             modifyVPCAttributeResponse
--
--         , testDescribeImagesResponse $
--             describeImagesResponse
--
--         , testRestoreAddressToClassicResponse $
--             restoreAddressToClassicResponse
--
--         , testCreateKeyPairResponse $
--             createKeyPairResponse
--
--         , testDeleteVolumeResponse $
--             deleteVolumeResponse
--
--         , testDescribeSpotFleetInstancesResponse $
--             describeSpotFleetInstancesResponse
--
--           ]
--     ]

-- Requests

testImportInstance :: ImportInstance -> TestTree
testImportInstance = req
    "ImportInstance"
    "fixture/ImportInstance.yaml"

testRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
testRevokeSecurityGroupEgress = req
    "RevokeSecurityGroupEgress"
    "fixture/RevokeSecurityGroupEgress.yaml"

testCreateVPNGateway :: CreateVPNGateway -> TestTree
testCreateVPNGateway = req
    "CreateVPNGateway"
    "fixture/CreateVPNGateway.yaml"

testCreateNetworkACL :: CreateNetworkACL -> TestTree
testCreateNetworkACL = req
    "CreateNetworkACL"
    "fixture/CreateNetworkACL.yaml"

testDeleteKeyPair :: DeleteKeyPair -> TestTree
testDeleteKeyPair = req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

testDetachNetworkInterface :: DetachNetworkInterface -> TestTree
testDetachNetworkInterface = req
    "DetachNetworkInterface"
    "fixture/DetachNetworkInterface.yaml"

testDetachInternetGateway :: DetachInternetGateway -> TestTree
testDetachInternetGateway = req
    "DetachInternetGateway"
    "fixture/DetachInternetGateway.yaml"

testDeleteVPCEndpoints :: DeleteVPCEndpoints -> TestTree
testDeleteVPCEndpoints = req
    "DeleteVPCEndpoints"
    "fixture/DeleteVPCEndpoints.yaml"

testDeleteFlowLogs :: DeleteFlowLogs -> TestTree
testDeleteFlowLogs = req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs.yaml"

testDescribeVPCClassicLink :: DescribeVPCClassicLink -> TestTree
testDescribeVPCClassicLink = req
    "DescribeVPCClassicLink"
    "fixture/DescribeVPCClassicLink.yaml"

testModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
testModifySubnetAttribute = req
    "ModifySubnetAttribute"
    "fixture/ModifySubnetAttribute.yaml"

testDetachVolume :: DetachVolume -> TestTree
testDetachVolume = req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

testCancelBundleTask :: CancelBundleTask -> TestTree
testCancelBundleTask = req
    "CancelBundleTask"
    "fixture/CancelBundleTask.yaml"

testReleaseAddress :: ReleaseAddress -> TestTree
testReleaseAddress = req
    "ReleaseAddress"
    "fixture/ReleaseAddress.yaml"

testCreateInternetGateway :: CreateInternetGateway -> TestTree
testCreateInternetGateway = req
    "CreateInternetGateway"
    "fixture/CreateInternetGateway.yaml"

testDeleteVPNConnection :: DeleteVPNConnection -> TestTree
testDeleteVPNConnection = req
    "DeleteVPNConnection"
    "fixture/DeleteVPNConnection.yaml"

testDescribeBundleTasks :: DescribeBundleTasks -> TestTree
testDescribeBundleTasks = req
    "DescribeBundleTasks"
    "fixture/DescribeBundleTasks.yaml"

testAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
testAuthorizeSecurityGroupEgress = req
    "AuthorizeSecurityGroupEgress"
    "fixture/AuthorizeSecurityGroupEgress.yaml"

testDeregisterImage :: DeregisterImage -> TestTree
testDeregisterImage = req
    "DeregisterImage"
    "fixture/DeregisterImage.yaml"

testModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
testModifyNetworkInterfaceAttribute = req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute.yaml"

testCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
testCancelReservedInstancesListing = req
    "CancelReservedInstancesListing"
    "fixture/CancelReservedInstancesListing.yaml"

testAttachClassicLinkVPC :: AttachClassicLinkVPC -> TestTree
testAttachClassicLinkVPC = req
    "AttachClassicLinkVPC"
    "fixture/AttachClassicLinkVPC.yaml"

testDescribeVPCClassicLinkDNSSupport :: DescribeVPCClassicLinkDNSSupport -> TestTree
testDescribeVPCClassicLinkDNSSupport = req
    "DescribeVPCClassicLinkDNSSupport"
    "fixture/DescribeVPCClassicLinkDNSSupport.yaml"

testRunScheduledInstances :: RunScheduledInstances -> TestTree
testRunScheduledInstances = req
    "RunScheduledInstances"
    "fixture/RunScheduledInstances.yaml"

testCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
testCancelSpotFleetRequests = req
    "CancelSpotFleetRequests"
    "fixture/CancelSpotFleetRequests.yaml"

testDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
testDescribeSpotPriceHistory = req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory.yaml"

testDescribeDHCPOptions :: DescribeDHCPOptions -> TestTree
testDescribeDHCPOptions = req
    "DescribeDHCPOptions"
    "fixture/DescribeDHCPOptions.yaml"

testImportImage :: ImportImage -> TestTree
testImportImage = req
    "ImportImage"
    "fixture/ImportImage.yaml"

testStopInstances :: StopInstances -> TestTree
testStopInstances = req
    "StopInstances"
    "fixture/StopInstances.yaml"

testDescribeInternetGateways :: DescribeInternetGateways -> TestTree
testDescribeInternetGateways = req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways.yaml"

testDisableVPCClassicLink :: DisableVPCClassicLink -> TestTree
testDisableVPCClassicLink = req
    "DisableVPCClassicLink"
    "fixture/DisableVPCClassicLink.yaml"

testBundleInstance :: BundleInstance -> TestTree
testBundleInstance = req
    "BundleInstance"
    "fixture/BundleInstance.yaml"

testDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
testDescribeNetworkInterfaces = req
    "DescribeNetworkInterfaces"
    "fixture/DescribeNetworkInterfaces.yaml"

testReplaceNetworkACLAssociation :: ReplaceNetworkACLAssociation -> TestTree
testReplaceNetworkACLAssociation = req
    "ReplaceNetworkACLAssociation"
    "fixture/ReplaceNetworkACLAssociation.yaml"

testDescribeNatGateways :: DescribeNatGateways -> TestTree
testDescribeNatGateways = req
    "DescribeNatGateways"
    "fixture/DescribeNatGateways.yaml"

testDescribeAddresses :: DescribeAddresses -> TestTree
testDescribeAddresses = req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

testDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
testDescribeSnapshotAttribute = req
    "DescribeSnapshotAttribute"
    "fixture/DescribeSnapshotAttribute.yaml"

testReplaceRoute :: ReplaceRoute -> TestTree
testReplaceRoute = req
    "ReplaceRoute"
    "fixture/ReplaceRoute.yaml"

testDescribeVPCEndpointServices :: DescribeVPCEndpointServices -> TestTree
testDescribeVPCEndpointServices = req
    "DescribeVPCEndpointServices"
    "fixture/DescribeVPCEndpointServices.yaml"

testAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
testAuthorizeSecurityGroupIngress = req
    "AuthorizeSecurityGroupIngress"
    "fixture/AuthorizeSecurityGroupIngress.yaml"

testCreateVPCPeeringConnection :: CreateVPCPeeringConnection -> TestTree
testCreateVPCPeeringConnection = req
    "CreateVPCPeeringConnection"
    "fixture/CreateVPCPeeringConnection.yaml"

testDescribeSubnets :: DescribeSubnets -> TestTree
testDescribeSubnets = req
    "DescribeSubnets"
    "fixture/DescribeSubnets.yaml"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

testPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
testPurchaseReservedInstancesOffering = req
    "PurchaseReservedInstancesOffering"
    "fixture/PurchaseReservedInstancesOffering.yaml"

testDeleteNetworkACLEntry :: DeleteNetworkACLEntry -> TestTree
testDeleteNetworkACLEntry = req
    "DeleteNetworkACLEntry"
    "fixture/DeleteNetworkACLEntry.yaml"

testResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
testResetSnapshotAttribute = req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute.yaml"

testDescribeVPNConnections :: DescribeVPNConnections -> TestTree
testDescribeVPNConnections = req
    "DescribeVPNConnections"
    "fixture/DescribeVPNConnections.yaml"

testDeleteRoute :: DeleteRoute -> TestTree
testDeleteRoute = req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

testReplaceNetworkACLEntry :: ReplaceNetworkACLEntry -> TestTree
testReplaceNetworkACLEntry = req
    "ReplaceNetworkACLEntry"
    "fixture/ReplaceNetworkACLEntry.yaml"

testDescribeVPCEndpoints :: DescribeVPCEndpoints -> TestTree
testDescribeVPCEndpoints = req
    "DescribeVPCEndpoints"
    "fixture/DescribeVPCEndpoints.yaml"

testResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
testResetInstanceAttribute = req
    "ResetInstanceAttribute"
    "fixture/ResetInstanceAttribute.yaml"

testAttachNetworkInterface :: AttachNetworkInterface -> TestTree
testAttachNetworkInterface = req
    "AttachNetworkInterface"
    "fixture/AttachNetworkInterface.yaml"

testDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
testDescribeInstanceStatus = req
    "DescribeInstanceStatus"
    "fixture/DescribeInstanceStatus.yaml"

testImportKeyPair :: ImportKeyPair -> TestTree
testImportKeyPair = req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

testConfirmProductInstance :: ConfirmProductInstance -> TestTree
testConfirmProductInstance = req
    "ConfirmProductInstance"
    "fixture/ConfirmProductInstance.yaml"

testDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
testDescribeInstanceAttribute = req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

testDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
testDescribeReservedInstancesOfferings = req
    "DescribeReservedInstancesOfferings"
    "fixture/DescribeReservedInstancesOfferings.yaml"

testCreateCustomerGateway :: CreateCustomerGateway -> TestTree
testCreateCustomerGateway = req
    "CreateCustomerGateway"
    "fixture/CreateCustomerGateway.yaml"

testDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
testDeleteSecurityGroup = req
    "DeleteSecurityGroup"
    "fixture/DeleteSecurityGroup.yaml"

testDeleteVPCPeeringConnection :: DeleteVPCPeeringConnection -> TestTree
testDeleteVPCPeeringConnection = req
    "DeleteVPCPeeringConnection"
    "fixture/DeleteVPCPeeringConnection.yaml"

testAttachInternetGateway :: AttachInternetGateway -> TestTree
testAttachInternetGateway = req
    "AttachInternetGateway"
    "fixture/AttachInternetGateway.yaml"

testModifyInstancePlacement :: ModifyInstancePlacement -> TestTree
testModifyInstancePlacement = req
    "ModifyInstancePlacement"
    "fixture/ModifyInstancePlacement.yaml"

testDescribeFlowLogs :: DescribeFlowLogs -> TestTree
testDescribeFlowLogs = req
    "DescribeFlowLogs"
    "fixture/DescribeFlowLogs.yaml"

testRunInstances :: RunInstances -> TestTree
testRunInstances = req
    "RunInstances"
    "fixture/RunInstances.yaml"

testAssociateDHCPOptions :: AssociateDHCPOptions -> TestTree
testAssociateDHCPOptions = req
    "AssociateDHCPOptions"
    "fixture/AssociateDHCPOptions.yaml"

testDescribeReservedInstances :: DescribeReservedInstances -> TestTree
testDescribeReservedInstances = req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances.yaml"

testDescribeIdFormat :: DescribeIdFormat -> TestTree
testDescribeIdFormat = req
    "DescribeIdFormat"
    "fixture/DescribeIdFormat.yaml"

testDescribeVPCs :: DescribeVPCs -> TestTree
testDescribeVPCs = req
    "DescribeVPCs"
    "fixture/DescribeVPCs.yaml"

testDescribeConversionTasks :: DescribeConversionTasks -> TestTree
testDescribeConversionTasks = req
    "DescribeConversionTasks"
    "fixture/DescribeConversionTasks.yaml"

testDisableVPCClassicLinkDNSSupport :: DisableVPCClassicLinkDNSSupport -> TestTree
testDisableVPCClassicLinkDNSSupport = req
    "DisableVPCClassicLinkDNSSupport"
    "fixture/DisableVPCClassicLinkDNSSupport.yaml"

testAllocateAddress :: AllocateAddress -> TestTree
testAllocateAddress = req
    "AllocateAddress"
    "fixture/AllocateAddress.yaml"

testCancelConversionTask :: CancelConversionTask -> TestTree
testCancelConversionTask = req
    "CancelConversionTask"
    "fixture/CancelConversionTask.yaml"

testModifyImageAttribute :: ModifyImageAttribute -> TestTree
testModifyImageAttribute = req
    "ModifyImageAttribute"
    "fixture/ModifyImageAttribute.yaml"

testCreateRouteTable :: CreateRouteTable -> TestTree
testCreateRouteTable = req
    "CreateRouteTable"
    "fixture/CreateRouteTable.yaml"

testReportInstanceStatus :: ReportInstanceStatus -> TestTree
testReportInstanceStatus = req
    "ReportInstanceStatus"
    "fixture/ReportInstanceStatus.yaml"

testAttachVolume :: AttachVolume -> TestTree
testAttachVolume = req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

testRequestSpotInstances :: RequestSpotInstances -> TestTree
testRequestSpotInstances = req
    "RequestSpotInstances"
    "fixture/RequestSpotInstances.yaml"

testDescribeVolumes :: DescribeVolumes -> TestTree
testDescribeVolumes = req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

testRejectVPCPeeringConnection :: RejectVPCPeeringConnection -> TestTree
testRejectVPCPeeringConnection = req
    "RejectVPCPeeringConnection"
    "fixture/RejectVPCPeeringConnection.yaml"

testDeleteVPNConnectionRoute :: DeleteVPNConnectionRoute -> TestTree
testDeleteVPNConnectionRoute = req
    "DeleteVPNConnectionRoute"
    "fixture/DeleteVPNConnectionRoute.yaml"

testModifyVPCEndpoint :: ModifyVPCEndpoint -> TestTree
testModifyVPCEndpoint = req
    "ModifyVPCEndpoint"
    "fixture/ModifyVPCEndpoint.yaml"

testAllocateHosts :: AllocateHosts -> TestTree
testAllocateHosts = req
    "AllocateHosts"
    "fixture/AllocateHosts.yaml"

testRegisterImage :: RegisterImage -> TestTree
testRegisterImage = req
    "RegisterImage"
    "fixture/RegisterImage.yaml"

testRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
testRevokeSecurityGroupIngress = req
    "RevokeSecurityGroupIngress"
    "fixture/RevokeSecurityGroupIngress.yaml"

testEnableVPCClassicLinkDNSSupport :: EnableVPCClassicLinkDNSSupport -> TestTree
testEnableVPCClassicLinkDNSSupport = req
    "EnableVPCClassicLinkDNSSupport"
    "fixture/EnableVPCClassicLinkDNSSupport.yaml"

testModifyReservedInstances :: ModifyReservedInstances -> TestTree
testModifyReservedInstances = req
    "ModifyReservedInstances"
    "fixture/ModifyReservedInstances.yaml"

testDescribeScheduledInstances :: DescribeScheduledInstances -> TestTree
testDescribeScheduledInstances = req
    "DescribeScheduledInstances"
    "fixture/DescribeScheduledInstances.yaml"

testCreateFlowLogs :: CreateFlowLogs -> TestTree
testCreateFlowLogs = req
    "CreateFlowLogs"
    "fixture/CreateFlowLogs.yaml"

testDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
testDescribeSpotFleetRequests = req
    "DescribeSpotFleetRequests"
    "fixture/DescribeSpotFleetRequests.yaml"

testMoveAddressToVPC :: MoveAddressToVPC -> TestTree
testMoveAddressToVPC = req
    "MoveAddressToVPC"
    "fixture/MoveAddressToVPC.yaml"

testDeleteNetworkACL :: DeleteNetworkACL -> TestTree
testDeleteNetworkACL = req
    "DeleteNetworkACL"
    "fixture/DeleteNetworkACL.yaml"

testDeleteVPNGateway :: DeleteVPNGateway -> TestTree
testDeleteVPNGateway = req
    "DeleteVPNGateway"
    "fixture/DeleteVPNGateway.yaml"

testDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
testDescribeImportImageTasks = req
    "DescribeImportImageTasks"
    "fixture/DescribeImportImageTasks.yaml"

testDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
testDescribeVolumeAttribute = req
    "DescribeVolumeAttribute"
    "fixture/DescribeVolumeAttribute.yaml"

testDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
testDescribeMovingAddresses = req
    "DescribeMovingAddresses"
    "fixture/DescribeMovingAddresses.yaml"

testGetPasswordData :: GetPasswordData -> TestTree
testGetPasswordData = req
    "GetPasswordData"
    "fixture/GetPasswordData.yaml"

testCreateVPC :: CreateVPC -> TestTree
testCreateVPC = req
    "CreateVPC"
    "fixture/CreateVPC.yaml"

testCopySnapshot :: CopySnapshot -> TestTree
testCopySnapshot = req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

testDisassociateAddress :: DisassociateAddress -> TestTree
testDisassociateAddress = req
    "DisassociateAddress"
    "fixture/DisassociateAddress.yaml"

testDeleteVPC :: DeleteVPC -> TestTree
testDeleteVPC = req
    "DeleteVPC"
    "fixture/DeleteVPC.yaml"

testCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
testCreateInstanceExportTask = req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask.yaml"

testDescribeVPCAttribute :: DescribeVPCAttribute -> TestTree
testDescribeVPCAttribute = req
    "DescribeVPCAttribute"
    "fixture/DescribeVPCAttribute.yaml"

testCreateVolume :: CreateVolume -> TestTree
testCreateVolume = req
    "CreateVolume"
    "fixture/CreateVolume.yaml"

testDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailability -> TestTree
testDescribeScheduledInstanceAvailability = req
    "DescribeScheduledInstanceAvailability"
    "fixture/DescribeScheduledInstanceAvailability.yaml"

testModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
testModifyVolumeAttribute = req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute.yaml"

testDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
testDescribeSpotDatafeedSubscription = req
    "DescribeSpotDatafeedSubscription"
    "fixture/DescribeSpotDatafeedSubscription.yaml"

testDescribePrefixLists :: DescribePrefixLists -> TestTree
testDescribePrefixLists = req
    "DescribePrefixLists"
    "fixture/DescribePrefixLists.yaml"

testDeletePlacementGroup :: DeletePlacementGroup -> TestTree
testDeletePlacementGroup = req
    "DeletePlacementGroup"
    "fixture/DeletePlacementGroup.yaml"

testRequestSpotFleet :: RequestSpotFleet -> TestTree
testRequestSpotFleet = req
    "RequestSpotFleet"
    "fixture/RequestSpotFleet.yaml"

testCreateSubnet :: CreateSubnet -> TestTree
testCreateSubnet = req
    "CreateSubnet"
    "fixture/CreateSubnet.yaml"

testCreateNetworkInterface :: CreateNetworkInterface -> TestTree
testCreateNetworkInterface = req
    "CreateNetworkInterface"
    "fixture/CreateNetworkInterface.yaml"

testDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
testDescribeSecurityGroups = req
    "DescribeSecurityGroups"
    "fixture/DescribeSecurityGroups.yaml"

testDescribeExportTasks :: DescribeExportTasks -> TestTree
testDescribeExportTasks = req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

testModifySpotFleetRequest :: ModifySpotFleetRequest -> TestTree
testModifySpotFleetRequest = req
    "ModifySpotFleetRequest"
    "fixture/ModifySpotFleetRequest.yaml"

testDetachVPNGateway :: DetachVPNGateway -> TestTree
testDetachVPNGateway = req
    "DetachVPNGateway"
    "fixture/DetachVPNGateway.yaml"

testEnableVolumeIO :: EnableVolumeIO -> TestTree
testEnableVolumeIO = req
    "EnableVolumeIO"
    "fixture/EnableVolumeIO.yaml"

testDescribeInstances :: DescribeInstances -> TestTree
testDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

testCreateNatGateway :: CreateNatGateway -> TestTree
testCreateNatGateway = req
    "CreateNatGateway"
    "fixture/CreateNatGateway.yaml"

testDescribeVPCPeeringConnections :: DescribeVPCPeeringConnections -> TestTree
testDescribeVPCPeeringConnections = req
    "DescribeVPCPeeringConnections"
    "fixture/DescribeVPCPeeringConnections.yaml"

testCancelExportTask :: CancelExportTask -> TestTree
testCancelExportTask = req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

testDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
testDeleteNetworkInterface = req
    "DeleteNetworkInterface"
    "fixture/DeleteNetworkInterface.yaml"

testReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
testReplaceRouteTableAssociation = req
    "ReplaceRouteTableAssociation"
    "fixture/ReplaceRouteTableAssociation.yaml"

testStartInstances :: StartInstances -> TestTree
testStartInstances = req
    "StartInstances"
    "fixture/StartInstances.yaml"

testCreatePlacementGroup :: CreatePlacementGroup -> TestTree
testCreatePlacementGroup = req
    "CreatePlacementGroup"
    "fixture/CreatePlacementGroup.yaml"

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

testAssociateAddress :: AssociateAddress -> TestTree
testAssociateAddress = req
    "AssociateAddress"
    "fixture/AssociateAddress.yaml"

testDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
testDescribeNetworkInterfaceAttribute = req
    "DescribeNetworkInterfaceAttribute"
    "fixture/DescribeNetworkInterfaceAttribute.yaml"

testReleaseHosts :: ReleaseHosts -> TestTree
testReleaseHosts = req
    "ReleaseHosts"
    "fixture/ReleaseHosts.yaml"

testResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
testResetNetworkInterfaceAttribute = req
    "ResetNetworkInterfaceAttribute"
    "fixture/ResetNetworkInterfaceAttribute.yaml"

testDeleteInternetGateway :: DeleteInternetGateway -> TestTree
testDeleteInternetGateway = req
    "DeleteInternetGateway"
    "fixture/DeleteInternetGateway.yaml"

testDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
testDescribeReservedInstancesListings = req
    "DescribeReservedInstancesListings"
    "fixture/DescribeReservedInstancesListings.yaml"

testCreateVPNConnection :: CreateVPNConnection -> TestTree
testCreateVPNConnection = req
    "CreateVPNConnection"
    "fixture/CreateVPNConnection.yaml"

testDeleteNatGateway :: DeleteNatGateway -> TestTree
testDeleteNatGateway = req
    "DeleteNatGateway"
    "fixture/DeleteNatGateway.yaml"

testDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
testDescribeImportSnapshotTasks = req
    "DescribeImportSnapshotTasks"
    "fixture/DescribeImportSnapshotTasks.yaml"

testDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
testDescribeCustomerGateways = req
    "DescribeCustomerGateways"
    "fixture/DescribeCustomerGateways.yaml"

testDeleteSubnet :: DeleteSubnet -> TestTree
testDeleteSubnet = req
    "DeleteSubnet"
    "fixture/DeleteSubnet.yaml"

testCopyImage :: CopyImage -> TestTree
testCopyImage = req
    "CopyImage"
    "fixture/CopyImage.yaml"

testCreateVPCEndpoint :: CreateVPCEndpoint -> TestTree
testCreateVPCEndpoint = req
    "CreateVPCEndpoint"
    "fixture/CreateVPCEndpoint.yaml"

testUnmonitorInstances :: UnmonitorInstances -> TestTree
testUnmonitorInstances = req
    "UnmonitorInstances"
    "fixture/UnmonitorInstances.yaml"

testCreateSecurityGroup :: CreateSecurityGroup -> TestTree
testCreateSecurityGroup = req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

testImportVolume :: ImportVolume -> TestTree
testImportVolume = req
    "ImportVolume"
    "fixture/ImportVolume.yaml"

testDisableVGWRoutePropagation :: DisableVGWRoutePropagation -> TestTree
testDisableVGWRoutePropagation = req
    "DisableVGWRoutePropagation"
    "fixture/DisableVGWRoutePropagation.yaml"

testCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscription -> TestTree
testCreateSpotDatafeedSubscription = req
    "CreateSpotDatafeedSubscription"
    "fixture/CreateSpotDatafeedSubscription.yaml"

testCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
testCancelSpotInstanceRequests = req
    "CancelSpotInstanceRequests"
    "fixture/CancelSpotInstanceRequests.yaml"

testCreateRoute :: CreateRoute -> TestTree
testCreateRoute = req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

testAssignPrivateIPAddresses :: AssignPrivateIPAddresses -> TestTree
testAssignPrivateIPAddresses = req
    "AssignPrivateIPAddresses"
    "fixture/AssignPrivateIPAddresses.yaml"

testModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
testModifyInstanceAttribute = req
    "ModifyInstanceAttribute"
    "fixture/ModifyInstanceAttribute.yaml"

testDeleteCustomerGateway :: DeleteCustomerGateway -> TestTree
testDeleteCustomerGateway = req
    "DeleteCustomerGateway"
    "fixture/DeleteCustomerGateway.yaml"

testDisassociateRouteTable :: DisassociateRouteTable -> TestTree
testDisassociateRouteTable = req
    "DisassociateRouteTable"
    "fixture/DisassociateRouteTable.yaml"

testDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
testDeleteSpotDatafeedSubscription = req
    "DeleteSpotDatafeedSubscription"
    "fixture/DeleteSpotDatafeedSubscription.yaml"

testEnableVPCClassicLink :: EnableVPCClassicLink -> TestTree
testEnableVPCClassicLink = req
    "EnableVPCClassicLink"
    "fixture/EnableVPCClassicLink.yaml"

testDescribePlacementGroups :: DescribePlacementGroups -> TestTree
testDescribePlacementGroups = req
    "DescribePlacementGroups"
    "fixture/DescribePlacementGroups.yaml"

testPurchaseScheduledInstances :: PurchaseScheduledInstances -> TestTree
testPurchaseScheduledInstances = req
    "PurchaseScheduledInstances"
    "fixture/PurchaseScheduledInstances.yaml"

testEnableVGWRoutePropagation :: EnableVGWRoutePropagation -> TestTree
testEnableVGWRoutePropagation = req
    "EnableVGWRoutePropagation"
    "fixture/EnableVGWRoutePropagation.yaml"

testDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
testDescribeSpotFleetRequestHistory = req
    "DescribeSpotFleetRequestHistory"
    "fixture/DescribeSpotFleetRequestHistory.yaml"

testModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
testModifySnapshotAttribute = req
    "ModifySnapshotAttribute"
    "fixture/ModifySnapshotAttribute.yaml"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

testCreateNetworkACLEntry :: CreateNetworkACLEntry -> TestTree
testCreateNetworkACLEntry = req
    "CreateNetworkACLEntry"
    "fixture/CreateNetworkACLEntry.yaml"

testCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
testCreateReservedInstancesListing = req
    "CreateReservedInstancesListing"
    "fixture/CreateReservedInstancesListing.yaml"

testAttachVPNGateway :: AttachVPNGateway -> TestTree
testAttachVPNGateway = req
    "AttachVPNGateway"
    "fixture/AttachVPNGateway.yaml"

testCreateDHCPOptions :: CreateDHCPOptions -> TestTree
testCreateDHCPOptions = req
    "CreateDHCPOptions"
    "fixture/CreateDHCPOptions.yaml"

testDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
testDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

testModifyHosts :: ModifyHosts -> TestTree
testModifyHosts = req
    "ModifyHosts"
    "fixture/ModifyHosts.yaml"

testRebootInstances :: RebootInstances -> TestTree
testRebootInstances = req
    "RebootInstances"
    "fixture/RebootInstances.yaml"

testCreateImage :: CreateImage -> TestTree
testCreateImage = req
    "CreateImage"
    "fixture/CreateImage.yaml"

testDescribeClassicLinkInstances :: DescribeClassicLinkInstances -> TestTree
testDescribeClassicLinkInstances = req
    "DescribeClassicLinkInstances"
    "fixture/DescribeClassicLinkInstances.yaml"

testTerminateInstances :: TerminateInstances -> TestTree
testTerminateInstances = req
    "TerminateInstances"
    "fixture/TerminateInstances.yaml"

testDescribeKeyPairs :: DescribeKeyPairs -> TestTree
testDescribeKeyPairs = req
    "DescribeKeyPairs"
    "fixture/DescribeKeyPairs.yaml"

testCreateVPNConnectionRoute :: CreateVPNConnectionRoute -> TestTree
testCreateVPNConnectionRoute = req
    "CreateVPNConnectionRoute"
    "fixture/CreateVPNConnectionRoute.yaml"

testAssociateRouteTable :: AssociateRouteTable -> TestTree
testAssociateRouteTable = req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable.yaml"

testDescribeVPNGateways :: DescribeVPNGateways -> TestTree
testDescribeVPNGateways = req
    "DescribeVPNGateways"
    "fixture/DescribeVPNGateways.yaml"

testGetConsoleOutput :: GetConsoleOutput -> TestTree
testGetConsoleOutput = req
    "GetConsoleOutput"
    "fixture/GetConsoleOutput.yaml"

testDescribeHosts :: DescribeHosts -> TestTree
testDescribeHosts = req
    "DescribeHosts"
    "fixture/DescribeHosts.yaml"

testDescribeImageAttribute :: DescribeImageAttribute -> TestTree
testDescribeImageAttribute = req
    "DescribeImageAttribute"
    "fixture/DescribeImageAttribute.yaml"

testModifyIdFormat :: ModifyIdFormat -> TestTree
testModifyIdFormat = req
    "ModifyIdFormat"
    "fixture/ModifyIdFormat.yaml"

testDeleteRouteTable :: DeleteRouteTable -> TestTree
testDeleteRouteTable = req
    "DeleteRouteTable"
    "fixture/DeleteRouteTable.yaml"

testResetImageAttribute :: ResetImageAttribute -> TestTree
testResetImageAttribute = req
    "ResetImageAttribute"
    "fixture/ResetImageAttribute.yaml"

testDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
testDescribeReservedInstancesModifications = req
    "DescribeReservedInstancesModifications"
    "fixture/DescribeReservedInstancesModifications.yaml"

testDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
testDescribeSpotInstanceRequests = req
    "DescribeSpotInstanceRequests"
    "fixture/DescribeSpotInstanceRequests.yaml"

testUnassignPrivateIPAddresses :: UnassignPrivateIPAddresses -> TestTree
testUnassignPrivateIPAddresses = req
    "UnassignPrivateIPAddresses"
    "fixture/UnassignPrivateIPAddresses.yaml"

testDeleteDHCPOptions :: DeleteDHCPOptions -> TestTree
testDeleteDHCPOptions = req
    "DeleteDHCPOptions"
    "fixture/DeleteDHCPOptions.yaml"

testDescribeNetworkACLs :: DescribeNetworkACLs -> TestTree
testDescribeNetworkACLs = req
    "DescribeNetworkACLs"
    "fixture/DescribeNetworkACLs.yaml"

testCancelImportTask :: CancelImportTask -> TestTree
testCancelImportTask = req
    "CancelImportTask"
    "fixture/CancelImportTask.yaml"

testDetachClassicLinkVPC :: DetachClassicLinkVPC -> TestTree
testDetachClassicLinkVPC = req
    "DetachClassicLinkVPC"
    "fixture/DetachClassicLinkVPC.yaml"

testDescribeRegions :: DescribeRegions -> TestTree
testDescribeRegions = req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

testMonitorInstances :: MonitorInstances -> TestTree
testMonitorInstances = req
    "MonitorInstances"
    "fixture/MonitorInstances.yaml"

testAcceptVPCPeeringConnection :: AcceptVPCPeeringConnection -> TestTree
testAcceptVPCPeeringConnection = req
    "AcceptVPCPeeringConnection"
    "fixture/AcceptVPCPeeringConnection.yaml"

testImportSnapshot :: ImportSnapshot -> TestTree
testImportSnapshot = req
    "ImportSnapshot"
    "fixture/ImportSnapshot.yaml"

testDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
testDescribeVolumeStatus = req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus.yaml"

testDescribeRouteTables :: DescribeRouteTables -> TestTree
testDescribeRouteTables = req
    "DescribeRouteTables"
    "fixture/DescribeRouteTables.yaml"

testDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
testDescribeAvailabilityZones = req
    "DescribeAvailabilityZones"
    "fixture/DescribeAvailabilityZones.yaml"

testModifyVPCAttribute :: ModifyVPCAttribute -> TestTree
testModifyVPCAttribute = req
    "ModifyVPCAttribute"
    "fixture/ModifyVPCAttribute.yaml"

testDescribeImages :: DescribeImages -> TestTree
testDescribeImages = req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

testRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
testRestoreAddressToClassic = req
    "RestoreAddressToClassic"
    "fixture/RestoreAddressToClassic.yaml"

testCreateKeyPair :: CreateKeyPair -> TestTree
testCreateKeyPair = req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

testDeleteVolume :: DeleteVolume -> TestTree
testDeleteVolume = req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

testDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
testDescribeSpotFleetInstances = req
    "DescribeSpotFleetInstances"
    "fixture/DescribeSpotFleetInstances.yaml"

-- Responses

testImportInstanceResponse :: ImportInstanceResponse -> TestTree
testImportInstanceResponse = res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    eC2
    (Proxy :: Proxy ImportInstance)

testRevokeSecurityGroupEgressResponse :: RevokeSecurityGroupEgressResponse -> TestTree
testRevokeSecurityGroupEgressResponse = res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse.proto"
    eC2
    (Proxy :: Proxy RevokeSecurityGroupEgress)

testCreateVPNGatewayResponse :: CreateVPNGatewayResponse -> TestTree
testCreateVPNGatewayResponse = res
    "CreateVPNGatewayResponse"
    "fixture/CreateVPNGatewayResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPNGateway)

testCreateNetworkACLResponse :: CreateNetworkACLResponse -> TestTree
testCreateNetworkACLResponse = res
    "CreateNetworkACLResponse"
    "fixture/CreateNetworkACLResponse.proto"
    eC2
    (Proxy :: Proxy CreateNetworkACL)

testDeleteKeyPairResponse :: DeleteKeyPairResponse -> TestTree
testDeleteKeyPairResponse = res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    eC2
    (Proxy :: Proxy DeleteKeyPair)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeTags)

testDetachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse -> TestTree
testDetachNetworkInterfaceResponse = res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    eC2
    (Proxy :: Proxy DetachNetworkInterface)

testDetachInternetGatewayResponse :: DetachInternetGatewayResponse -> TestTree
testDetachInternetGatewayResponse = res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DetachInternetGateway)

testDeleteVPCEndpointsResponse :: DeleteVPCEndpointsResponse -> TestTree
testDeleteVPCEndpointsResponse = res
    "DeleteVPCEndpointsResponse"
    "fixture/DeleteVPCEndpointsResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPCEndpoints)

testDeleteFlowLogsResponse :: DeleteFlowLogsResponse -> TestTree
testDeleteFlowLogsResponse = res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    eC2
    (Proxy :: Proxy DeleteFlowLogs)

testDescribeVPCClassicLinkResponse :: DescribeVPCClassicLinkResponse -> TestTree
testDescribeVPCClassicLinkResponse = res
    "DescribeVPCClassicLinkResponse"
    "fixture/DescribeVPCClassicLinkResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCClassicLink)

testModifySubnetAttributeResponse :: ModifySubnetAttributeResponse -> TestTree
testModifySubnetAttributeResponse = res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifySubnetAttribute)

testDetachVolumeResponse :: VolumeAttachment -> TestTree
testDetachVolumeResponse = res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    eC2
    (Proxy :: Proxy DetachVolume)

testCancelBundleTaskResponse :: CancelBundleTaskResponse -> TestTree
testCancelBundleTaskResponse = res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse.proto"
    eC2
    (Proxy :: Proxy CancelBundleTask)

testReleaseAddressResponse :: ReleaseAddressResponse -> TestTree
testReleaseAddressResponse = res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    eC2
    (Proxy :: Proxy ReleaseAddress)

testCreateInternetGatewayResponse :: CreateInternetGatewayResponse -> TestTree
testCreateInternetGatewayResponse = res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    eC2
    (Proxy :: Proxy CreateInternetGateway)

testDeleteVPNConnectionResponse :: DeleteVPNConnectionResponse -> TestTree
testDeleteVPNConnectionResponse = res
    "DeleteVPNConnectionResponse"
    "fixture/DeleteVPNConnectionResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPNConnection)

testDescribeBundleTasksResponse :: DescribeBundleTasksResponse -> TestTree
testDescribeBundleTasksResponse = res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeBundleTasks)

testAuthorizeSecurityGroupEgressResponse :: AuthorizeSecurityGroupEgressResponse -> TestTree
testAuthorizeSecurityGroupEgressResponse = res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    eC2
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

testDeregisterImageResponse :: DeregisterImageResponse -> TestTree
testDeregisterImageResponse = res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse.proto"
    eC2
    (Proxy :: Proxy DeregisterImage)

testModifyNetworkInterfaceAttributeResponse :: ModifyNetworkInterfaceAttributeResponse -> TestTree
testModifyNetworkInterfaceAttributeResponse = res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

testCancelReservedInstancesListingResponse :: CancelReservedInstancesListingResponse -> TestTree
testCancelReservedInstancesListingResponse = res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse.proto"
    eC2
    (Proxy :: Proxy CancelReservedInstancesListing)

testAttachClassicLinkVPCResponse :: AttachClassicLinkVPCResponse -> TestTree
testAttachClassicLinkVPCResponse = res
    "AttachClassicLinkVPCResponse"
    "fixture/AttachClassicLinkVPCResponse.proto"
    eC2
    (Proxy :: Proxy AttachClassicLinkVPC)

testDescribeVPCClassicLinkDNSSupportResponse :: DescribeVPCClassicLinkDNSSupportResponse -> TestTree
testDescribeVPCClassicLinkDNSSupportResponse = res
    "DescribeVPCClassicLinkDNSSupportResponse"
    "fixture/DescribeVPCClassicLinkDNSSupportResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCClassicLinkDNSSupport)

testRunScheduledInstancesResponse :: RunScheduledInstancesResponse -> TestTree
testRunScheduledInstancesResponse = res
    "RunScheduledInstancesResponse"
    "fixture/RunScheduledInstancesResponse.proto"
    eC2
    (Proxy :: Proxy RunScheduledInstances)

testCancelSpotFleetRequestsResponse :: CancelSpotFleetRequestsResponse -> TestTree
testCancelSpotFleetRequestsResponse = res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    eC2
    (Proxy :: Proxy CancelSpotFleetRequests)

testDescribeSpotPriceHistoryResponse :: DescribeSpotPriceHistoryResponse -> TestTree
testDescribeSpotPriceHistoryResponse = res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotPriceHistory)

testDescribeDHCPOptionsResponse :: DescribeDHCPOptionsResponse -> TestTree
testDescribeDHCPOptionsResponse = res
    "DescribeDHCPOptionsResponse"
    "fixture/DescribeDHCPOptionsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeDHCPOptions)

testImportImageResponse :: ImportImageResponse -> TestTree
testImportImageResponse = res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    eC2
    (Proxy :: Proxy ImportImage)

testStopInstancesResponse :: StopInstancesResponse -> TestTree
testStopInstancesResponse = res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    eC2
    (Proxy :: Proxy StopInstances)

testDescribeInternetGatewaysResponse :: DescribeInternetGatewaysResponse -> TestTree
testDescribeInternetGatewaysResponse = res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    eC2
    (Proxy :: Proxy DescribeInternetGateways)

testDisableVPCClassicLinkResponse :: DisableVPCClassicLinkResponse -> TestTree
testDisableVPCClassicLinkResponse = res
    "DisableVPCClassicLinkResponse"
    "fixture/DisableVPCClassicLinkResponse.proto"
    eC2
    (Proxy :: Proxy DisableVPCClassicLink)

testBundleInstanceResponse :: BundleInstanceResponse -> TestTree
testBundleInstanceResponse = res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    eC2
    (Proxy :: Proxy BundleInstance)

testDescribeNetworkInterfacesResponse :: DescribeNetworkInterfacesResponse -> TestTree
testDescribeNetworkInterfacesResponse = res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeNetworkInterfaces)

testReplaceNetworkACLAssociationResponse :: ReplaceNetworkACLAssociationResponse -> TestTree
testReplaceNetworkACLAssociationResponse = res
    "ReplaceNetworkACLAssociationResponse"
    "fixture/ReplaceNetworkACLAssociationResponse.proto"
    eC2
    (Proxy :: Proxy ReplaceNetworkACLAssociation)

testDescribeNatGatewaysResponse :: DescribeNatGatewaysResponse -> TestTree
testDescribeNatGatewaysResponse = res
    "DescribeNatGatewaysResponse"
    "fixture/DescribeNatGatewaysResponse.proto"
    eC2
    (Proxy :: Proxy DescribeNatGateways)

testDescribeAddressesResponse :: DescribeAddressesResponse -> TestTree
testDescribeAddressesResponse = res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeAddresses)

testDescribeSnapshotAttributeResponse :: DescribeSnapshotAttributeResponse -> TestTree
testDescribeSnapshotAttributeResponse = res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSnapshotAttribute)

testReplaceRouteResponse :: ReplaceRouteResponse -> TestTree
testReplaceRouteResponse = res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    eC2
    (Proxy :: Proxy ReplaceRoute)

testDescribeVPCEndpointServicesResponse :: DescribeVPCEndpointServicesResponse -> TestTree
testDescribeVPCEndpointServicesResponse = res
    "DescribeVPCEndpointServicesResponse"
    "fixture/DescribeVPCEndpointServicesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCEndpointServices)

testAuthorizeSecurityGroupIngressResponse :: AuthorizeSecurityGroupIngressResponse -> TestTree
testAuthorizeSecurityGroupIngressResponse = res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    eC2
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

testCreateVPCPeeringConnectionResponse :: CreateVPCPeeringConnectionResponse -> TestTree
testCreateVPCPeeringConnectionResponse = res
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPCPeeringConnection)

testDescribeSubnetsResponse :: DescribeSubnetsResponse -> TestTree
testDescribeSubnetsResponse = res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSubnets)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    eC2
    (Proxy :: Proxy CreateTags)

testPurchaseReservedInstancesOfferingResponse :: PurchaseReservedInstancesOfferingResponse -> TestTree
testPurchaseReservedInstancesOfferingResponse = res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    eC2
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

testDeleteNetworkACLEntryResponse :: DeleteNetworkACLEntryResponse -> TestTree
testDeleteNetworkACLEntryResponse = res
    "DeleteNetworkACLEntryResponse"
    "fixture/DeleteNetworkACLEntryResponse.proto"
    eC2
    (Proxy :: Proxy DeleteNetworkACLEntry)

testResetSnapshotAttributeResponse :: ResetSnapshotAttributeResponse -> TestTree
testResetSnapshotAttributeResponse = res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ResetSnapshotAttribute)

testDescribeVPNConnectionsResponse :: DescribeVPNConnectionsResponse -> TestTree
testDescribeVPNConnectionsResponse = res
    "DescribeVPNConnectionsResponse"
    "fixture/DescribeVPNConnectionsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPNConnections)

testDeleteRouteResponse :: DeleteRouteResponse -> TestTree
testDeleteRouteResponse = res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    eC2
    (Proxy :: Proxy DeleteRoute)

testReplaceNetworkACLEntryResponse :: ReplaceNetworkACLEntryResponse -> TestTree
testReplaceNetworkACLEntryResponse = res
    "ReplaceNetworkACLEntryResponse"
    "fixture/ReplaceNetworkACLEntryResponse.proto"
    eC2
    (Proxy :: Proxy ReplaceNetworkACLEntry)

testDescribeVPCEndpointsResponse :: DescribeVPCEndpointsResponse -> TestTree
testDescribeVPCEndpointsResponse = res
    "DescribeVPCEndpointsResponse"
    "fixture/DescribeVPCEndpointsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCEndpoints)

testResetInstanceAttributeResponse :: ResetInstanceAttributeResponse -> TestTree
testResetInstanceAttributeResponse = res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ResetInstanceAttribute)

testAttachNetworkInterfaceResponse :: AttachNetworkInterfaceResponse -> TestTree
testAttachNetworkInterfaceResponse = res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse.proto"
    eC2
    (Proxy :: Proxy AttachNetworkInterface)

testDescribeInstanceStatusResponse :: DescribeInstanceStatusResponse -> TestTree
testDescribeInstanceStatusResponse = res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse.proto"
    eC2
    (Proxy :: Proxy DescribeInstanceStatus)

testImportKeyPairResponse :: ImportKeyPairResponse -> TestTree
testImportKeyPairResponse = res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    eC2
    (Proxy :: Proxy ImportKeyPair)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    eC2
    (Proxy :: Proxy DeleteTags)

testConfirmProductInstanceResponse :: ConfirmProductInstanceResponse -> TestTree
testConfirmProductInstanceResponse = res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    eC2
    (Proxy :: Proxy ConfirmProductInstance)

testDescribeInstanceAttributeResponse :: DescribeInstanceAttributeResponse -> TestTree
testDescribeInstanceAttributeResponse = res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeInstanceAttribute)

testDescribeReservedInstancesOfferingsResponse :: DescribeReservedInstancesOfferingsResponse -> TestTree
testDescribeReservedInstancesOfferingsResponse = res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

testCreateCustomerGatewayResponse :: CreateCustomerGatewayResponse -> TestTree
testCreateCustomerGatewayResponse = res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    eC2
    (Proxy :: Proxy CreateCustomerGateway)

testDeleteSecurityGroupResponse :: DeleteSecurityGroupResponse -> TestTree
testDeleteSecurityGroupResponse = res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    eC2
    (Proxy :: Proxy DeleteSecurityGroup)

testDeleteVPCPeeringConnectionResponse :: DeleteVPCPeeringConnectionResponse -> TestTree
testDeleteVPCPeeringConnectionResponse = res
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPCPeeringConnection)

testAttachInternetGatewayResponse :: AttachInternetGatewayResponse -> TestTree
testAttachInternetGatewayResponse = res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    eC2
    (Proxy :: Proxy AttachInternetGateway)

testModifyInstancePlacementResponse :: ModifyInstancePlacementResponse -> TestTree
testModifyInstancePlacementResponse = res
    "ModifyInstancePlacementResponse"
    "fixture/ModifyInstancePlacementResponse.proto"
    eC2
    (Proxy :: Proxy ModifyInstancePlacement)

testDescribeFlowLogsResponse :: DescribeFlowLogsResponse -> TestTree
testDescribeFlowLogsResponse = res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeFlowLogs)

testRunInstancesResponse :: Reservation -> TestTree
testRunInstancesResponse = res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    eC2
    (Proxy :: Proxy RunInstances)

testAssociateDHCPOptionsResponse :: AssociateDHCPOptionsResponse -> TestTree
testAssociateDHCPOptionsResponse = res
    "AssociateDHCPOptionsResponse"
    "fixture/AssociateDHCPOptionsResponse.proto"
    eC2
    (Proxy :: Proxy AssociateDHCPOptions)

testDescribeReservedInstancesResponse :: DescribeReservedInstancesResponse -> TestTree
testDescribeReservedInstancesResponse = res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeReservedInstances)

testDescribeIdFormatResponse :: DescribeIdFormatResponse -> TestTree
testDescribeIdFormatResponse = res
    "DescribeIdFormatResponse"
    "fixture/DescribeIdFormatResponse.proto"
    eC2
    (Proxy :: Proxy DescribeIdFormat)

testDescribeVPCsResponse :: DescribeVPCsResponse -> TestTree
testDescribeVPCsResponse = res
    "DescribeVPCsResponse"
    "fixture/DescribeVPCsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCs)

testDescribeConversionTasksResponse :: DescribeConversionTasksResponse -> TestTree
testDescribeConversionTasksResponse = res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeConversionTasks)

testDisableVPCClassicLinkDNSSupportResponse :: DisableVPCClassicLinkDNSSupportResponse -> TestTree
testDisableVPCClassicLinkDNSSupportResponse = res
    "DisableVPCClassicLinkDNSSupportResponse"
    "fixture/DisableVPCClassicLinkDNSSupportResponse.proto"
    eC2
    (Proxy :: Proxy DisableVPCClassicLinkDNSSupport)

testAllocateAddressResponse :: AllocateAddressResponse -> TestTree
testAllocateAddressResponse = res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    eC2
    (Proxy :: Proxy AllocateAddress)

testCancelConversionTaskResponse :: CancelConversionTaskResponse -> TestTree
testCancelConversionTaskResponse = res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse.proto"
    eC2
    (Proxy :: Proxy CancelConversionTask)

testModifyImageAttributeResponse :: ModifyImageAttributeResponse -> TestTree
testModifyImageAttributeResponse = res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyImageAttribute)

testCreateRouteTableResponse :: CreateRouteTableResponse -> TestTree
testCreateRouteTableResponse = res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    eC2
    (Proxy :: Proxy CreateRouteTable)

testReportInstanceStatusResponse :: ReportInstanceStatusResponse -> TestTree
testReportInstanceStatusResponse = res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    eC2
    (Proxy :: Proxy ReportInstanceStatus)

testAttachVolumeResponse :: VolumeAttachment -> TestTree
testAttachVolumeResponse = res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    eC2
    (Proxy :: Proxy AttachVolume)

testRequestSpotInstancesResponse :: RequestSpotInstancesResponse -> TestTree
testRequestSpotInstancesResponse = res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    eC2
    (Proxy :: Proxy RequestSpotInstances)

testDescribeVolumesResponse :: DescribeVolumesResponse -> TestTree
testDescribeVolumesResponse = res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVolumes)

testRejectVPCPeeringConnectionResponse :: RejectVPCPeeringConnectionResponse -> TestTree
testRejectVPCPeeringConnectionResponse = res
    "RejectVPCPeeringConnectionResponse"
    "fixture/RejectVPCPeeringConnectionResponse.proto"
    eC2
    (Proxy :: Proxy RejectVPCPeeringConnection)

testDeleteVPNConnectionRouteResponse :: DeleteVPNConnectionRouteResponse -> TestTree
testDeleteVPNConnectionRouteResponse = res
    "DeleteVPNConnectionRouteResponse"
    "fixture/DeleteVPNConnectionRouteResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPNConnectionRoute)

testModifyVPCEndpointResponse :: ModifyVPCEndpointResponse -> TestTree
testModifyVPCEndpointResponse = res
    "ModifyVPCEndpointResponse"
    "fixture/ModifyVPCEndpointResponse.proto"
    eC2
    (Proxy :: Proxy ModifyVPCEndpoint)

testAllocateHostsResponse :: AllocateHostsResponse -> TestTree
testAllocateHostsResponse = res
    "AllocateHostsResponse"
    "fixture/AllocateHostsResponse.proto"
    eC2
    (Proxy :: Proxy AllocateHosts)

testRegisterImageResponse :: RegisterImageResponse -> TestTree
testRegisterImageResponse = res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    eC2
    (Proxy :: Proxy RegisterImage)

testRevokeSecurityGroupIngressResponse :: RevokeSecurityGroupIngressResponse -> TestTree
testRevokeSecurityGroupIngressResponse = res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    eC2
    (Proxy :: Proxy RevokeSecurityGroupIngress)

testEnableVPCClassicLinkDNSSupportResponse :: EnableVPCClassicLinkDNSSupportResponse -> TestTree
testEnableVPCClassicLinkDNSSupportResponse = res
    "EnableVPCClassicLinkDNSSupportResponse"
    "fixture/EnableVPCClassicLinkDNSSupportResponse.proto"
    eC2
    (Proxy :: Proxy EnableVPCClassicLinkDNSSupport)

testModifyReservedInstancesResponse :: ModifyReservedInstancesResponse -> TestTree
testModifyReservedInstancesResponse = res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    eC2
    (Proxy :: Proxy ModifyReservedInstances)

testDescribeScheduledInstancesResponse :: DescribeScheduledInstancesResponse -> TestTree
testDescribeScheduledInstancesResponse = res
    "DescribeScheduledInstancesResponse"
    "fixture/DescribeScheduledInstancesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeScheduledInstances)

testCreateFlowLogsResponse :: CreateFlowLogsResponse -> TestTree
testCreateFlowLogsResponse = res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    eC2
    (Proxy :: Proxy CreateFlowLogs)

testDescribeSpotFleetRequestsResponse :: DescribeSpotFleetRequestsResponse -> TestTree
testDescribeSpotFleetRequestsResponse = res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotFleetRequests)

testMoveAddressToVPCResponse :: MoveAddressToVPCResponse -> TestTree
testMoveAddressToVPCResponse = res
    "MoveAddressToVPCResponse"
    "fixture/MoveAddressToVPCResponse.proto"
    eC2
    (Proxy :: Proxy MoveAddressToVPC)

testDeleteNetworkACLResponse :: DeleteNetworkACLResponse -> TestTree
testDeleteNetworkACLResponse = res
    "DeleteNetworkACLResponse"
    "fixture/DeleteNetworkACLResponse.proto"
    eC2
    (Proxy :: Proxy DeleteNetworkACL)

testDeleteVPNGatewayResponse :: DeleteVPNGatewayResponse -> TestTree
testDeleteVPNGatewayResponse = res
    "DeleteVPNGatewayResponse"
    "fixture/DeleteVPNGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPNGateway)

testDescribeImportImageTasksResponse :: DescribeImportImageTasksResponse -> TestTree
testDescribeImportImageTasksResponse = res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeImportImageTasks)

testDescribeVolumeAttributeResponse :: DescribeVolumeAttributeResponse -> TestTree
testDescribeVolumeAttributeResponse = res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVolumeAttribute)

testDescribeMovingAddressesResponse :: DescribeMovingAddressesResponse -> TestTree
testDescribeMovingAddressesResponse = res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeMovingAddresses)

testGetPasswordDataResponse :: GetPasswordDataResponse -> TestTree
testGetPasswordDataResponse = res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse.proto"
    eC2
    (Proxy :: Proxy GetPasswordData)

testCreateVPCResponse :: CreateVPCResponse -> TestTree
testCreateVPCResponse = res
    "CreateVPCResponse"
    "fixture/CreateVPCResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPC)

testCopySnapshotResponse :: CopySnapshotResponse -> TestTree
testCopySnapshotResponse = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    eC2
    (Proxy :: Proxy CopySnapshot)

testDisassociateAddressResponse :: DisassociateAddressResponse -> TestTree
testDisassociateAddressResponse = res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse.proto"
    eC2
    (Proxy :: Proxy DisassociateAddress)

testDeleteVPCResponse :: DeleteVPCResponse -> TestTree
testDeleteVPCResponse = res
    "DeleteVPCResponse"
    "fixture/DeleteVPCResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPC)

testCreateInstanceExportTaskResponse :: CreateInstanceExportTaskResponse -> TestTree
testCreateInstanceExportTaskResponse = res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    eC2
    (Proxy :: Proxy CreateInstanceExportTask)

testDescribeVPCAttributeResponse :: DescribeVPCAttributeResponse -> TestTree
testDescribeVPCAttributeResponse = res
    "DescribeVPCAttributeResponse"
    "fixture/DescribeVPCAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCAttribute)

testCreateVolumeResponse :: Volume -> TestTree
testCreateVolumeResponse = res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    eC2
    (Proxy :: Proxy CreateVolume)

testDescribeScheduledInstanceAvailabilityResponse :: DescribeScheduledInstanceAvailabilityResponse -> TestTree
testDescribeScheduledInstanceAvailabilityResponse = res
    "DescribeScheduledInstanceAvailabilityResponse"
    "fixture/DescribeScheduledInstanceAvailabilityResponse.proto"
    eC2
    (Proxy :: Proxy DescribeScheduledInstanceAvailability)

testModifyVolumeAttributeResponse :: ModifyVolumeAttributeResponse -> TestTree
testModifyVolumeAttributeResponse = res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyVolumeAttribute)

testDescribeSpotDatafeedSubscriptionResponse :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
testDescribeSpotDatafeedSubscriptionResponse = res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

testDescribePrefixListsResponse :: DescribePrefixListsResponse -> TestTree
testDescribePrefixListsResponse = res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    eC2
    (Proxy :: Proxy DescribePrefixLists)

testDeletePlacementGroupResponse :: DeletePlacementGroupResponse -> TestTree
testDeletePlacementGroupResponse = res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    eC2
    (Proxy :: Proxy DeletePlacementGroup)

testRequestSpotFleetResponse :: RequestSpotFleetResponse -> TestTree
testRequestSpotFleetResponse = res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    eC2
    (Proxy :: Proxy RequestSpotFleet)

testCreateSubnetResponse :: CreateSubnetResponse -> TestTree
testCreateSubnetResponse = res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    eC2
    (Proxy :: Proxy CreateSubnet)

testCreateNetworkInterfaceResponse :: CreateNetworkInterfaceResponse -> TestTree
testCreateNetworkInterfaceResponse = res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse.proto"
    eC2
    (Proxy :: Proxy CreateNetworkInterface)

testDescribeSecurityGroupsResponse :: DescribeSecurityGroupsResponse -> TestTree
testDescribeSecurityGroupsResponse = res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSecurityGroups)

testDescribeExportTasksResponse :: DescribeExportTasksResponse -> TestTree
testDescribeExportTasksResponse = res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeExportTasks)

testModifySpotFleetRequestResponse :: ModifySpotFleetRequestResponse -> TestTree
testModifySpotFleetRequestResponse = res
    "ModifySpotFleetRequestResponse"
    "fixture/ModifySpotFleetRequestResponse.proto"
    eC2
    (Proxy :: Proxy ModifySpotFleetRequest)

testDetachVPNGatewayResponse :: DetachVPNGatewayResponse -> TestTree
testDetachVPNGatewayResponse = res
    "DetachVPNGatewayResponse"
    "fixture/DetachVPNGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DetachVPNGateway)

testEnableVolumeIOResponse :: EnableVolumeIOResponse -> TestTree
testEnableVolumeIOResponse = res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    eC2
    (Proxy :: Proxy EnableVolumeIO)

testDescribeInstancesResponse :: DescribeInstancesResponse -> TestTree
testDescribeInstancesResponse = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeInstances)

testCreateNatGatewayResponse :: CreateNatGatewayResponse -> TestTree
testCreateNatGatewayResponse = res
    "CreateNatGatewayResponse"
    "fixture/CreateNatGatewayResponse.proto"
    eC2
    (Proxy :: Proxy CreateNatGateway)

testDescribeVPCPeeringConnectionsResponse :: DescribeVPCPeeringConnectionsResponse -> TestTree
testDescribeVPCPeeringConnectionsResponse = res
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCPeeringConnections)

testCancelExportTaskResponse :: CancelExportTaskResponse -> TestTree
testCancelExportTaskResponse = res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    eC2
    (Proxy :: Proxy CancelExportTask)

testDeleteNetworkInterfaceResponse :: DeleteNetworkInterfaceResponse -> TestTree
testDeleteNetworkInterfaceResponse = res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    eC2
    (Proxy :: Proxy DeleteNetworkInterface)

testReplaceRouteTableAssociationResponse :: ReplaceRouteTableAssociationResponse -> TestTree
testReplaceRouteTableAssociationResponse = res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    eC2
    (Proxy :: Proxy ReplaceRouteTableAssociation)

testStartInstancesResponse :: StartInstancesResponse -> TestTree
testStartInstancesResponse = res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    eC2
    (Proxy :: Proxy StartInstances)

testCreatePlacementGroupResponse :: CreatePlacementGroupResponse -> TestTree
testCreatePlacementGroupResponse = res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    eC2
    (Proxy :: Proxy CreatePlacementGroup)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSnapshots)

testAssociateAddressResponse :: AssociateAddressResponse -> TestTree
testAssociateAddressResponse = res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    eC2
    (Proxy :: Proxy AssociateAddress)

testDescribeNetworkInterfaceAttributeResponse :: DescribeNetworkInterfaceAttributeResponse -> TestTree
testDescribeNetworkInterfaceAttributeResponse = res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

testReleaseHostsResponse :: ReleaseHostsResponse -> TestTree
testReleaseHostsResponse = res
    "ReleaseHostsResponse"
    "fixture/ReleaseHostsResponse.proto"
    eC2
    (Proxy :: Proxy ReleaseHosts)

testResetNetworkInterfaceAttributeResponse :: ResetNetworkInterfaceAttributeResponse -> TestTree
testResetNetworkInterfaceAttributeResponse = res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

testDeleteInternetGatewayResponse :: DeleteInternetGatewayResponse -> TestTree
testDeleteInternetGatewayResponse = res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DeleteInternetGateway)

testDescribeReservedInstancesListingsResponse :: DescribeReservedInstancesListingsResponse -> TestTree
testDescribeReservedInstancesListingsResponse = res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeReservedInstancesListings)

testCreateVPNConnectionResponse :: CreateVPNConnectionResponse -> TestTree
testCreateVPNConnectionResponse = res
    "CreateVPNConnectionResponse"
    "fixture/CreateVPNConnectionResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPNConnection)

testDeleteNatGatewayResponse :: DeleteNatGatewayResponse -> TestTree
testDeleteNatGatewayResponse = res
    "DeleteNatGatewayResponse"
    "fixture/DeleteNatGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DeleteNatGateway)

testDescribeImportSnapshotTasksResponse :: DescribeImportSnapshotTasksResponse -> TestTree
testDescribeImportSnapshotTasksResponse = res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeImportSnapshotTasks)

testDescribeCustomerGatewaysResponse :: DescribeCustomerGatewaysResponse -> TestTree
testDescribeCustomerGatewaysResponse = res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    eC2
    (Proxy :: Proxy DescribeCustomerGateways)

testDeleteSubnetResponse :: DeleteSubnetResponse -> TestTree
testDeleteSubnetResponse = res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    eC2
    (Proxy :: Proxy DeleteSubnet)

testCopyImageResponse :: CopyImageResponse -> TestTree
testCopyImageResponse = res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    eC2
    (Proxy :: Proxy CopyImage)

testCreateVPCEndpointResponse :: CreateVPCEndpointResponse -> TestTree
testCreateVPCEndpointResponse = res
    "CreateVPCEndpointResponse"
    "fixture/CreateVPCEndpointResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPCEndpoint)

testUnmonitorInstancesResponse :: UnmonitorInstancesResponse -> TestTree
testUnmonitorInstancesResponse = res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    eC2
    (Proxy :: Proxy UnmonitorInstances)

testCreateSecurityGroupResponse :: CreateSecurityGroupResponse -> TestTree
testCreateSecurityGroupResponse = res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    eC2
    (Proxy :: Proxy CreateSecurityGroup)

testImportVolumeResponse :: ImportVolumeResponse -> TestTree
testImportVolumeResponse = res
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse.proto"
    eC2
    (Proxy :: Proxy ImportVolume)

testDisableVGWRoutePropagationResponse :: DisableVGWRoutePropagationResponse -> TestTree
testDisableVGWRoutePropagationResponse = res
    "DisableVGWRoutePropagationResponse"
    "fixture/DisableVGWRoutePropagationResponse.proto"
    eC2
    (Proxy :: Proxy DisableVGWRoutePropagation)

testCreateSpotDatafeedSubscriptionResponse :: CreateSpotDatafeedSubscriptionResponse -> TestTree
testCreateSpotDatafeedSubscriptionResponse = res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse.proto"
    eC2
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

testCancelSpotInstanceRequestsResponse :: CancelSpotInstanceRequestsResponse -> TestTree
testCancelSpotInstanceRequestsResponse = res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse.proto"
    eC2
    (Proxy :: Proxy CancelSpotInstanceRequests)

testCreateRouteResponse :: CreateRouteResponse -> TestTree
testCreateRouteResponse = res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    eC2
    (Proxy :: Proxy CreateRoute)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    eC2
    (Proxy :: Proxy DeleteSnapshot)

testAssignPrivateIPAddressesResponse :: AssignPrivateIPAddressesResponse -> TestTree
testAssignPrivateIPAddressesResponse = res
    "AssignPrivateIPAddressesResponse"
    "fixture/AssignPrivateIPAddressesResponse.proto"
    eC2
    (Proxy :: Proxy AssignPrivateIPAddresses)

testModifyInstanceAttributeResponse :: ModifyInstanceAttributeResponse -> TestTree
testModifyInstanceAttributeResponse = res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyInstanceAttribute)

testDeleteCustomerGatewayResponse :: DeleteCustomerGatewayResponse -> TestTree
testDeleteCustomerGatewayResponse = res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DeleteCustomerGateway)

testDisassociateRouteTableResponse :: DisassociateRouteTableResponse -> TestTree
testDisassociateRouteTableResponse = res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    eC2
    (Proxy :: Proxy DisassociateRouteTable)

testDeleteSpotDatafeedSubscriptionResponse :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
testDeleteSpotDatafeedSubscriptionResponse = res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    eC2
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

testEnableVPCClassicLinkResponse :: EnableVPCClassicLinkResponse -> TestTree
testEnableVPCClassicLinkResponse = res
    "EnableVPCClassicLinkResponse"
    "fixture/EnableVPCClassicLinkResponse.proto"
    eC2
    (Proxy :: Proxy EnableVPCClassicLink)

testDescribePlacementGroupsResponse :: DescribePlacementGroupsResponse -> TestTree
testDescribePlacementGroupsResponse = res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    eC2
    (Proxy :: Proxy DescribePlacementGroups)

testPurchaseScheduledInstancesResponse :: PurchaseScheduledInstancesResponse -> TestTree
testPurchaseScheduledInstancesResponse = res
    "PurchaseScheduledInstancesResponse"
    "fixture/PurchaseScheduledInstancesResponse.proto"
    eC2
    (Proxy :: Proxy PurchaseScheduledInstances)

testEnableVGWRoutePropagationResponse :: EnableVGWRoutePropagationResponse -> TestTree
testEnableVGWRoutePropagationResponse = res
    "EnableVGWRoutePropagationResponse"
    "fixture/EnableVGWRoutePropagationResponse.proto"
    eC2
    (Proxy :: Proxy EnableVGWRoutePropagation)

testDescribeSpotFleetRequestHistoryResponse :: DescribeSpotFleetRequestHistoryResponse -> TestTree
testDescribeSpotFleetRequestHistoryResponse = res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

testModifySnapshotAttributeResponse :: ModifySnapshotAttributeResponse -> TestTree
testModifySnapshotAttributeResponse = res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifySnapshotAttribute)

testCreateSnapshotResponse :: Snapshot -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    eC2
    (Proxy :: Proxy CreateSnapshot)

testCreateNetworkACLEntryResponse :: CreateNetworkACLEntryResponse -> TestTree
testCreateNetworkACLEntryResponse = res
    "CreateNetworkACLEntryResponse"
    "fixture/CreateNetworkACLEntryResponse.proto"
    eC2
    (Proxy :: Proxy CreateNetworkACLEntry)

testCreateReservedInstancesListingResponse :: CreateReservedInstancesListingResponse -> TestTree
testCreateReservedInstancesListingResponse = res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    eC2
    (Proxy :: Proxy CreateReservedInstancesListing)

testAttachVPNGatewayResponse :: AttachVPNGatewayResponse -> TestTree
testAttachVPNGatewayResponse = res
    "AttachVPNGatewayResponse"
    "fixture/AttachVPNGatewayResponse.proto"
    eC2
    (Proxy :: Proxy AttachVPNGateway)

testCreateDHCPOptionsResponse :: CreateDHCPOptionsResponse -> TestTree
testCreateDHCPOptionsResponse = res
    "CreateDHCPOptionsResponse"
    "fixture/CreateDHCPOptionsResponse.proto"
    eC2
    (Proxy :: Proxy CreateDHCPOptions)

testDescribeAccountAttributesResponse :: DescribeAccountAttributesResponse -> TestTree
testDescribeAccountAttributesResponse = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeAccountAttributes)

testModifyHostsResponse :: ModifyHostsResponse -> TestTree
testModifyHostsResponse = res
    "ModifyHostsResponse"
    "fixture/ModifyHostsResponse.proto"
    eC2
    (Proxy :: Proxy ModifyHosts)

testRebootInstancesResponse :: RebootInstancesResponse -> TestTree
testRebootInstancesResponse = res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    eC2
    (Proxy :: Proxy RebootInstances)

testCreateImageResponse :: CreateImageResponse -> TestTree
testCreateImageResponse = res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    eC2
    (Proxy :: Proxy CreateImage)

testDescribeClassicLinkInstancesResponse :: DescribeClassicLinkInstancesResponse -> TestTree
testDescribeClassicLinkInstancesResponse = res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeClassicLinkInstances)

testTerminateInstancesResponse :: TerminateInstancesResponse -> TestTree
testTerminateInstancesResponse = res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse.proto"
    eC2
    (Proxy :: Proxy TerminateInstances)

testDescribeKeyPairsResponse :: DescribeKeyPairsResponse -> TestTree
testDescribeKeyPairsResponse = res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeKeyPairs)

testCreateVPNConnectionRouteResponse :: CreateVPNConnectionRouteResponse -> TestTree
testCreateVPNConnectionRouteResponse = res
    "CreateVPNConnectionRouteResponse"
    "fixture/CreateVPNConnectionRouteResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPNConnectionRoute)

testAssociateRouteTableResponse :: AssociateRouteTableResponse -> TestTree
testAssociateRouteTableResponse = res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    eC2
    (Proxy :: Proxy AssociateRouteTable)

testDescribeVPNGatewaysResponse :: DescribeVPNGatewaysResponse -> TestTree
testDescribeVPNGatewaysResponse = res
    "DescribeVPNGatewaysResponse"
    "fixture/DescribeVPNGatewaysResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPNGateways)

testGetConsoleOutputResponse :: GetConsoleOutputResponse -> TestTree
testGetConsoleOutputResponse = res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    eC2
    (Proxy :: Proxy GetConsoleOutput)

testDescribeHostsResponse :: DescribeHostsResponse -> TestTree
testDescribeHostsResponse = res
    "DescribeHostsResponse"
    "fixture/DescribeHostsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeHosts)

testDescribeImageAttributeResponse :: DescribeImageAttributeResponse -> TestTree
testDescribeImageAttributeResponse = res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeImageAttribute)

testModifyIdFormatResponse :: ModifyIdFormatResponse -> TestTree
testModifyIdFormatResponse = res
    "ModifyIdFormatResponse"
    "fixture/ModifyIdFormatResponse.proto"
    eC2
    (Proxy :: Proxy ModifyIdFormat)

testDeleteRouteTableResponse :: DeleteRouteTableResponse -> TestTree
testDeleteRouteTableResponse = res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    eC2
    (Proxy :: Proxy DeleteRouteTable)

testResetImageAttributeResponse :: ResetImageAttributeResponse -> TestTree
testResetImageAttributeResponse = res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ResetImageAttribute)

testDescribeReservedInstancesModificationsResponse :: DescribeReservedInstancesModificationsResponse -> TestTree
testDescribeReservedInstancesModificationsResponse = res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeReservedInstancesModifications)

testDescribeSpotInstanceRequestsResponse :: DescribeSpotInstanceRequestsResponse -> TestTree
testDescribeSpotInstanceRequestsResponse = res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotInstanceRequests)

testUnassignPrivateIPAddressesResponse :: UnassignPrivateIPAddressesResponse -> TestTree
testUnassignPrivateIPAddressesResponse = res
    "UnassignPrivateIPAddressesResponse"
    "fixture/UnassignPrivateIPAddressesResponse.proto"
    eC2
    (Proxy :: Proxy UnassignPrivateIPAddresses)

testDeleteDHCPOptionsResponse :: DeleteDHCPOptionsResponse -> TestTree
testDeleteDHCPOptionsResponse = res
    "DeleteDHCPOptionsResponse"
    "fixture/DeleteDHCPOptionsResponse.proto"
    eC2
    (Proxy :: Proxy DeleteDHCPOptions)

testDescribeNetworkACLsResponse :: DescribeNetworkACLsResponse -> TestTree
testDescribeNetworkACLsResponse = res
    "DescribeNetworkACLsResponse"
    "fixture/DescribeNetworkACLsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeNetworkACLs)

testCancelImportTaskResponse :: CancelImportTaskResponse -> TestTree
testCancelImportTaskResponse = res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse.proto"
    eC2
    (Proxy :: Proxy CancelImportTask)

testDetachClassicLinkVPCResponse :: DetachClassicLinkVPCResponse -> TestTree
testDetachClassicLinkVPCResponse = res
    "DetachClassicLinkVPCResponse"
    "fixture/DetachClassicLinkVPCResponse.proto"
    eC2
    (Proxy :: Proxy DetachClassicLinkVPC)

testDescribeRegionsResponse :: DescribeRegionsResponse -> TestTree
testDescribeRegionsResponse = res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeRegions)

testMonitorInstancesResponse :: MonitorInstancesResponse -> TestTree
testMonitorInstancesResponse = res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    eC2
    (Proxy :: Proxy MonitorInstances)

testAcceptVPCPeeringConnectionResponse :: AcceptVPCPeeringConnectionResponse -> TestTree
testAcceptVPCPeeringConnectionResponse = res
    "AcceptVPCPeeringConnectionResponse"
    "fixture/AcceptVPCPeeringConnectionResponse.proto"
    eC2
    (Proxy :: Proxy AcceptVPCPeeringConnection)

testImportSnapshotResponse :: ImportSnapshotResponse -> TestTree
testImportSnapshotResponse = res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    eC2
    (Proxy :: Proxy ImportSnapshot)

testDescribeVolumeStatusResponse :: DescribeVolumeStatusResponse -> TestTree
testDescribeVolumeStatusResponse = res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVolumeStatus)

testDescribeRouteTablesResponse :: DescribeRouteTablesResponse -> TestTree
testDescribeRouteTablesResponse = res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeRouteTables)

testDescribeAvailabilityZonesResponse :: DescribeAvailabilityZonesResponse -> TestTree
testDescribeAvailabilityZonesResponse = res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeAvailabilityZones)

testModifyVPCAttributeResponse :: ModifyVPCAttributeResponse -> TestTree
testModifyVPCAttributeResponse = res
    "ModifyVPCAttributeResponse"
    "fixture/ModifyVPCAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyVPCAttribute)

testDescribeImagesResponse :: DescribeImagesResponse -> TestTree
testDescribeImagesResponse = res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeImages)

testRestoreAddressToClassicResponse :: RestoreAddressToClassicResponse -> TestTree
testRestoreAddressToClassicResponse = res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    eC2
    (Proxy :: Proxy RestoreAddressToClassic)

testCreateKeyPairResponse :: CreateKeyPairResponse -> TestTree
testCreateKeyPairResponse = res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    eC2
    (Proxy :: Proxy CreateKeyPair)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVolume)

testDescribeSpotFleetInstancesResponse :: DescribeSpotFleetInstancesResponse -> TestTree
testDescribeSpotFleetInstancesResponse = res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotFleetInstances)
