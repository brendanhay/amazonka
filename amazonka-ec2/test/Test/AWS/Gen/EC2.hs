{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.EC2
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.EC2 where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.EC2

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDetachNetworkInterface $
--             detachNetworkInterface
--
--         , testDeleteVPCEndpoints $
--             deleteVPCEndpoints
--
--         , testDeleteKeyPair $
--             deleteKeyPair
--
--         , testDeleteFlowLogs $
--             deleteFlowLogs
--
--         , testDescribeTags $
--             describeTags
--
--         , testRevokeSecurityGroupEgress $
--             revokeSecurityGroupEgress
--
--         , testCreateVPNGateway $
--             createVPNGateway
--
--         , testDetachInternetGateway $
--             detachInternetGateway
--
--         , testCreateNetworkACL $
--             createNetworkACL
--
--         , testImportInstance $
--             importInstance
--
--         , testDescribeVPCClassicLink $
--             describeVPCClassicLink
--
--         , testDeleteVPNConnection $
--             deleteVPNConnection
--
--         , testAuthorizeSecurityGroupEgress $
--             authorizeSecurityGroupEgress
--
--         , testDescribeBundleTasks $
--             describeBundleTasks
--
--         , testCreateInternetGateway $
--             createInternetGateway
--
--         , testReleaseAddress $
--             releaseAddress
--
--         , testCancelBundleTask $
--             cancelBundleTask
--
--         , testModifyNetworkInterfaceAttribute $
--             modifyNetworkInterfaceAttribute
--
--         , testModifySubnetAttribute $
--             modifySubnetAttribute
--
--         , testDeregisterImage $
--             deregisterImage
--
--         , testDetachVolume $
--             detachVolume
--
--         , testCancelReservedInstancesListing $
--             cancelReservedInstancesListing
--
--         , testAttachClassicLinkVPC $
--             attachClassicLinkVPC
--
--         , testCancelSpotFleetRequests $
--             cancelSpotFleetRequests
--
--         , testDescribeDHCPOptions $
--             describeDHCPOptions
--
--         , testDescribeSpotPriceHistory $
--             describeSpotPriceHistory
--
--         , testStopInstances $
--             stopInstances
--
--         , testImportImage $
--             importImage
--
--         , testDeleteNetworkACLEntry $
--             deleteNetworkACLEntry
--
--         , testDisableVPCClassicLink $
--             disableVPCClassicLink
--
--         , testAuthorizeSecurityGroupIngress $
--             authorizeSecurityGroupIngress
--
--         , testBundleInstance $
--             bundleInstance
--
--         , testDescribeVPCEndpointServices $
--             describeVPCEndpointServices
--
--         , testReplaceNetworkACLAssociation $
--             replaceNetworkACLAssociation
--
--         , testCreateVPCPeeringConnection $
--             createVPCPeeringConnection
--
--         , testResetSnapshotAttribute $
--             resetSnapshotAttribute
--
--         , testDescribeAddresses $
--             describeAddresses
--
--         , testDescribeInternetGateways $
--             describeInternetGateways
--
--         , testReplaceRoute $
--             replaceRoute
--
--         , testCreateTags $
--             createTags
--
--         , testDescribeSubnets $
--             describeSubnets
--
--         , testDescribeNetworkInterfaces $
--             describeNetworkInterfaces
--
--         , testPurchaseReservedInstancesOffering $
--             purchaseReservedInstancesOffering
--
--         , testDescribeSnapshotAttribute $
--             describeSnapshotAttribute
--
--         , testCreateCustomerGateway $
--             createCustomerGateway
--
--         , testAttachInternetGateway $
--             attachInternetGateway
--
--         , testDeleteTags $
--             deleteTags
--
--         , testReplaceNetworkACLEntry $
--             replaceNetworkACLEntry
--
--         , testResetInstanceAttribute $
--             resetInstanceAttribute
--
--         , testDeleteRoute $
--             deleteRoute
--
--         , testDescribeVPNConnections $
--             describeVPNConnections
--
--         , testDescribeFlowLogs $
--             describeFlowLogs
--
--         , testDeleteSecurityGroup $
--             deleteSecurityGroup
--
--         , testDescribeReservedInstancesOfferings $
--             describeReservedInstancesOfferings
--
--         , testDeleteVPCPeeringConnection $
--             deleteVPCPeeringConnection
--
--         , testDescribeVPCEndpoints $
--             describeVPCEndpoints
--
--         , testDescribeInstanceAttribute $
--             describeInstanceAttribute
--
--         , testConfirmProductInstance $
--             confirmProductInstance
--
--         , testImportKeyPair $
--             importKeyPair
--
--         , testAttachNetworkInterface $
--             attachNetworkInterface
--
--         , testDescribeInstanceStatus $
--             describeInstanceStatus
--
--         , testCancelConversionTask $
--             cancelConversionTask
--
--         , testReportInstanceStatus $
--             reportInstanceStatus
--
--         , testAssociateDHCPOptions $
--             associateDHCPOptions
--
--         , testDescribeVPCs $
--             describeVPCs
--
--         , testRequestSpotInstances $
--             requestSpotInstances
--
--         , testModifyImageAttribute $
--             modifyImageAttribute
--
--         , testDescribeReservedInstances $
--             describeReservedInstances
--
--         , testAllocateAddress $
--             allocateAddress
--
--         , testRunInstances $
--             runInstances
--
--         , testCreateRouteTable $
--             createRouteTable
--
--         , testAttachVolume $
--             attachVolume
--
--         , testDescribeConversionTasks $
--             describeConversionTasks
--
--         , testRejectVPCPeeringConnection $
--             rejectVPCPeeringConnection
--
--         , testRevokeSecurityGroupIngress $
--             revokeSecurityGroupIngress
--
--         , testDescribeVolumes $
--             describeVolumes
--
--         , testDeleteVPNConnectionRoute $
--             deleteVPNConnectionRoute
--
--         , testModifyReservedInstances $
--             modifyReservedInstances
--
--         , testRegisterImage $
--             registerImage
--
--         , testModifyVPCEndpoint $
--             modifyVPCEndpoint
--
--         , testDeleteVPNGateway $
--             deleteVPNGateway
--
--         , testCreateVPC $
--             createVPC
--
--         , testDescribeMovingAddresses $
--             describeMovingAddresses
--
--         , testDescribeVolumeAttribute $
--             describeVolumeAttribute
--
--         , testMoveAddressToVPC $
--             moveAddressToVPC
--
--         , testGetPasswordData $
--             getPasswordData
--
--         , testCreateFlowLogs $
--             createFlowLogs
--
--         , testDescribeImportImageTasks $
--             describeImportImageTasks
--
--         , testDeleteNetworkACL $
--             deleteNetworkACL
--
--         , testDescribeSpotFleetRequests $
--             describeSpotFleetRequests
--
--         , testCopySnapshot $
--             copySnapshot
--
--         , testModifyVolumeAttribute $
--             modifyVolumeAttribute
--
--         , testDescribeVPCAttribute $
--             describeVPCAttribute
--
--         , testCreateVolume $
--             createVolume
--
--         , testDisassociateAddress $
--             disassociateAddress
--
--         , testDeleteVPC $
--             deleteVPC
--
--         , testDescribePrefixLists $
--             describePrefixLists
--
--         , testCreateInstanceExportTask $
--             createInstanceExportTask
--
--         , testDescribeSpotDatafeedSubscription $
--             describeSpotDatafeedSubscription
--
--         , testDetachVPNGateway $
--             detachVPNGateway
--
--         , testDescribeExportTasks $
--             describeExportTasks
--
--         , testDeletePlacementGroup $
--             deletePlacementGroup
--
--         , testCreateSubnet $
--             createSubnet
--
--         , testEnableVolumeIO $
--             enableVolumeIO
--
--         , testCancelExportTask $
--             cancelExportTask
--
--         , testRequestSpotFleet $
--             requestSpotFleet
--
--         , testDescribeInstances $
--             describeInstances
--
--         , testDescribeSecurityGroups $
--             describeSecurityGroups
--
--         , testDescribeVPCPeeringConnections $
--             describeVPCPeeringConnections
--
--         , testCreateNetworkInterface $
--             createNetworkInterface
--
--         , testAssociateAddress $
--             associateAddress
--
--         , testStartInstances $
--             startInstances
--
--         , testDescribeCustomerGateways $
--             describeCustomerGateways
--
--         , testResetNetworkInterfaceAttribute $
--             resetNetworkInterfaceAttribute
--
--         , testCreateVPNConnection $
--             createVPNConnection
--
--         , testDescribeSnapshots $
--             describeSnapshots
--
--         , testCreatePlacementGroup $
--             createPlacementGroup
--
--         , testReplaceRouteTableAssociation $
--             replaceRouteTableAssociation
--
--         , testDescribeNetworkInterfaceAttribute $
--             describeNetworkInterfaceAttribute
--
--         , testDescribeReservedInstancesListings $
--             describeReservedInstancesListings
--
--         , testDeleteNetworkInterface $
--             deleteNetworkInterface
--
--         , testDeleteInternetGateway $
--             deleteInternetGateway
--
--         , testDeleteSubnet $
--             deleteSubnet
--
--         , testCreateVPCEndpoint $
--             createVPCEndpoint
--
--         , testDescribeImportSnapshotTasks $
--             describeImportSnapshotTasks
--
--         , testCopyImage $
--             copyImage
--
--         , testDisassociateRouteTable $
--             disassociateRouteTable
--
--         , testUnmonitorInstances $
--             unmonitorInstances
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
--         , testAssignPrivateIPAddresses $
--             assignPrivateIPAddresses
--
--         , testDeleteSnapshot $
--             deleteSnapshot
--
--         , testDeleteCustomerGateway $
--             deleteCustomerGateway
--
--         , testModifyInstanceAttribute $
--             modifyInstanceAttribute
--
--         , testCreateSecurityGroup $
--             createSecurityGroup
--
--         , testCancelSpotInstanceRequests $
--             cancelSpotInstanceRequests
--
--         , testCreateRoute $
--             createRoute
--
--         , testCreateNetworkACLEntry $
--             createNetworkACLEntry
--
--         , testModifySnapshotAttribute $
--             modifySnapshotAttribute
--
--         , testEnableVGWRoutePropagation $
--             enableVGWRoutePropagation
--
--         , testCreateSnapshot $
--             createSnapshot
--
--         , testDescribeSpotFleetRequestHistory $
--             describeSpotFleetRequestHistory
--
--         , testDeleteSpotDatafeedSubscription $
--             deleteSpotDatafeedSubscription
--
--         , testDescribePlacementGroups $
--             describePlacementGroups
--
--         , testCreateReservedInstancesListing $
--             createReservedInstancesListing
--
--         , testEnableVPCClassicLink $
--             enableVPCClassicLink
--
--         , testDescribeKeyPairs $
--             describeKeyPairs
--
--         , testRebootInstances $
--             rebootInstances
--
--         , testAttachVPNGateway $
--             attachVPNGateway
--
--         , testCreateVPNConnectionRoute $
--             createVPNConnectionRoute
--
--         , testDescribeClassicLinkInstances $
--             describeClassicLinkInstances
--
--         , testTerminateInstances $
--             terminateInstances
--
--         , testCreateDHCPOptions $
--             createDHCPOptions
--
--         , testAssociateRouteTable $
--             associateRouteTable
--
--         , testCreateImage $
--             createImage
--
--         , testDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , testResetImageAttribute $
--             resetImageAttribute
--
--         , testDescribeNetworkACLs $
--             describeNetworkACLs
--
--         , testCancelImportTask $
--             cancelImportTask
--
--         , testGetConsoleOutput $
--             getConsoleOutput
--
--         , testUnassignPrivateIPAddresses $
--             unassignPrivateIPAddresses
--
--         , testDeleteRouteTable $
--             deleteRouteTable
--
--         , testDescribeImageAttribute $
--             describeImageAttribute
--
--         , testDeleteDHCPOptions $
--             deleteDHCPOptions
--
--         , testDescribeVPNGateways $
--             describeVPNGateways
--
--         , testDetachClassicLinkVPC $
--             detachClassicLinkVPC
--
--         , testDescribeReservedInstancesModifications $
--             describeReservedInstancesModifications
--
--         , testDescribeSpotInstanceRequests $
--             describeSpotInstanceRequests
--
--         , testMonitorInstances $
--             monitorInstances
--
--         , testDescribeRegions $
--             describeRegions
--
--         , testModifyVPCAttribute $
--             modifyVPCAttribute
--
--         , testDescribeSpotFleetInstances $
--             describeSpotFleetInstances
--
--         , testDescribeVolumeStatus $
--             describeVolumeStatus
--
--         , testDeleteVolume $
--             deleteVolume
--
--         , testDescribeImages $
--             describeImages
--
--         , testCreateKeyPair $
--             createKeyPair
--
--         , testRestoreAddressToClassic $
--             restoreAddressToClassic
--
--         , testDescribeAvailabilityZones $
--             describeAvailabilityZones
--
--         , testImportSnapshot $
--             importSnapshot
--
--         , testAcceptVPCPeeringConnection $
--             acceptVPCPeeringConnection
--
--         , testDescribeRouteTables $
--             describeRouteTables
--
--           ]

--     , testGroup "response"
--         [ testDetachNetworkInterfaceResponse $
--             detachNetworkInterfaceResponse
--
--         , testDeleteVPCEndpointsResponse $
--             deleteVPCEndpointsResponse
--
--         , testDeleteKeyPairResponse $
--             deleteKeyPairResponse
--
--         , testDeleteFlowLogsResponse $
--             deleteFlowLogsResponse
--
--         , testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testRevokeSecurityGroupEgressResponse $
--             revokeSecurityGroupEgressResponse
--
--         , testCreateVPNGatewayResponse $
--             createVPNGatewayResponse
--
--         , testDetachInternetGatewayResponse $
--             detachInternetGatewayResponse
--
--         , testCreateNetworkACLResponse $
--             createNetworkACLResponse
--
--         , testImportInstanceResponse $
--             importInstanceResponse
--
--         , testDescribeVPCClassicLinkResponse $
--             describeVPCClassicLinkResponse
--
--         , testDeleteVPNConnectionResponse $
--             deleteVPNConnectionResponse
--
--         , testAuthorizeSecurityGroupEgressResponse $
--             authorizeSecurityGroupEgressResponse
--
--         , testDescribeBundleTasksResponse $
--             describeBundleTasksResponse
--
--         , testCreateInternetGatewayResponse $
--             createInternetGatewayResponse
--
--         , testReleaseAddressResponse $
--             releaseAddressResponse
--
--         , testCancelBundleTaskResponse $
--             cancelBundleTaskResponse
--
--         , testModifyNetworkInterfaceAttributeResponse $
--             modifyNetworkInterfaceAttributeResponse
--
--         , testModifySubnetAttributeResponse $
--             modifySubnetAttributeResponse
--
--         , testDeregisterImageResponse $
--             deregisterImageResponse
--
--         , testDetachVolumeResponse $
--             volumeAttachment
--
--         , testCancelReservedInstancesListingResponse $
--             cancelReservedInstancesListingResponse
--
--         , testAttachClassicLinkVPCResponse $
--             attachClassicLinkVPCResponse
--
--         , testCancelSpotFleetRequestsResponse $
--             cancelSpotFleetRequestsResponse
--
--         , testDescribeDHCPOptionsResponse $
--             describeDHCPOptionsResponse
--
--         , testDescribeSpotPriceHistoryResponse $
--             describeSpotPriceHistoryResponse
--
--         , testStopInstancesResponse $
--             stopInstancesResponse
--
--         , testImportImageResponse $
--             importImageResponse
--
--         , testDeleteNetworkACLEntryResponse $
--             deleteNetworkACLEntryResponse
--
--         , testDisableVPCClassicLinkResponse $
--             disableVPCClassicLinkResponse
--
--         , testAuthorizeSecurityGroupIngressResponse $
--             authorizeSecurityGroupIngressResponse
--
--         , testBundleInstanceResponse $
--             bundleInstanceResponse
--
--         , testDescribeVPCEndpointServicesResponse $
--             describeVPCEndpointServicesResponse
--
--         , testReplaceNetworkACLAssociationResponse $
--             replaceNetworkACLAssociationResponse
--
--         , testCreateVPCPeeringConnectionResponse $
--             createVPCPeeringConnectionResponse
--
--         , testResetSnapshotAttributeResponse $
--             resetSnapshotAttributeResponse
--
--         , testDescribeAddressesResponse $
--             describeAddressesResponse
--
--         , testDescribeInternetGatewaysResponse $
--             describeInternetGatewaysResponse
--
--         , testReplaceRouteResponse $
--             replaceRouteResponse
--
--         , testCreateTagsResponse $
--             createTagsResponse
--
--         , testDescribeSubnetsResponse $
--             describeSubnetsResponse
--
--         , testDescribeNetworkInterfacesResponse $
--             describeNetworkInterfacesResponse
--
--         , testPurchaseReservedInstancesOfferingResponse $
--             purchaseReservedInstancesOfferingResponse
--
--         , testDescribeSnapshotAttributeResponse $
--             describeSnapshotAttributeResponse
--
--         , testCreateCustomerGatewayResponse $
--             createCustomerGatewayResponse
--
--         , testAttachInternetGatewayResponse $
--             attachInternetGatewayResponse
--
--         , testDeleteTagsResponse $
--             deleteTagsResponse
--
--         , testReplaceNetworkACLEntryResponse $
--             replaceNetworkACLEntryResponse
--
--         , testResetInstanceAttributeResponse $
--             resetInstanceAttributeResponse
--
--         , testDeleteRouteResponse $
--             deleteRouteResponse
--
--         , testDescribeVPNConnectionsResponse $
--             describeVPNConnectionsResponse
--
--         , testDescribeFlowLogsResponse $
--             describeFlowLogsResponse
--
--         , testDeleteSecurityGroupResponse $
--             deleteSecurityGroupResponse
--
--         , testDescribeReservedInstancesOfferingsResponse $
--             describeReservedInstancesOfferingsResponse
--
--         , testDeleteVPCPeeringConnectionResponse $
--             deleteVPCPeeringConnectionResponse
--
--         , testDescribeVPCEndpointsResponse $
--             describeVPCEndpointsResponse
--
--         , testDescribeInstanceAttributeResponse $
--             describeInstanceAttributeResponse
--
--         , testConfirmProductInstanceResponse $
--             confirmProductInstanceResponse
--
--         , testImportKeyPairResponse $
--             importKeyPairResponse
--
--         , testAttachNetworkInterfaceResponse $
--             attachNetworkInterfaceResponse
--
--         , testDescribeInstanceStatusResponse $
--             describeInstanceStatusResponse
--
--         , testCancelConversionTaskResponse $
--             cancelConversionTaskResponse
--
--         , testReportInstanceStatusResponse $
--             reportInstanceStatusResponse
--
--         , testAssociateDHCPOptionsResponse $
--             associateDHCPOptionsResponse
--
--         , testDescribeVPCsResponse $
--             describeVPCsResponse
--
--         , testRequestSpotInstancesResponse $
--             requestSpotInstancesResponse
--
--         , testModifyImageAttributeResponse $
--             modifyImageAttributeResponse
--
--         , testDescribeReservedInstancesResponse $
--             describeReservedInstancesResponse
--
--         , testAllocateAddressResponse $
--             allocateAddressResponse
--
--         , testRunInstancesResponse $
--             reservation
--
--         , testCreateRouteTableResponse $
--             createRouteTableResponse
--
--         , testAttachVolumeResponse $
--             volumeAttachment
--
--         , testDescribeConversionTasksResponse $
--             describeConversionTasksResponse
--
--         , testRejectVPCPeeringConnectionResponse $
--             rejectVPCPeeringConnectionResponse
--
--         , testRevokeSecurityGroupIngressResponse $
--             revokeSecurityGroupIngressResponse
--
--         , testDescribeVolumesResponse $
--             describeVolumesResponse
--
--         , testDeleteVPNConnectionRouteResponse $
--             deleteVPNConnectionRouteResponse
--
--         , testModifyReservedInstancesResponse $
--             modifyReservedInstancesResponse
--
--         , testRegisterImageResponse $
--             registerImageResponse
--
--         , testModifyVPCEndpointResponse $
--             modifyVPCEndpointResponse
--
--         , testDeleteVPNGatewayResponse $
--             deleteVPNGatewayResponse
--
--         , testCreateVPCResponse $
--             createVPCResponse
--
--         , testDescribeMovingAddressesResponse $
--             describeMovingAddressesResponse
--
--         , testDescribeVolumeAttributeResponse $
--             describeVolumeAttributeResponse
--
--         , testMoveAddressToVPCResponse $
--             moveAddressToVPCResponse
--
--         , testGetPasswordDataResponse $
--             getPasswordDataResponse
--
--         , testCreateFlowLogsResponse $
--             createFlowLogsResponse
--
--         , testDescribeImportImageTasksResponse $
--             describeImportImageTasksResponse
--
--         , testDeleteNetworkACLResponse $
--             deleteNetworkACLResponse
--
--         , testDescribeSpotFleetRequestsResponse $
--             describeSpotFleetRequestsResponse
--
--         , testCopySnapshotResponse $
--             copySnapshotResponse
--
--         , testModifyVolumeAttributeResponse $
--             modifyVolumeAttributeResponse
--
--         , testDescribeVPCAttributeResponse $
--             describeVPCAttributeResponse
--
--         , testCreateVolumeResponse $
--             volume
--
--         , testDisassociateAddressResponse $
--             disassociateAddressResponse
--
--         , testDeleteVPCResponse $
--             deleteVPCResponse
--
--         , testDescribePrefixListsResponse $
--             describePrefixListsResponse
--
--         , testCreateInstanceExportTaskResponse $
--             createInstanceExportTaskResponse
--
--         , testDescribeSpotDatafeedSubscriptionResponse $
--             describeSpotDatafeedSubscriptionResponse
--
--         , testDetachVPNGatewayResponse $
--             detachVPNGatewayResponse
--
--         , testDescribeExportTasksResponse $
--             describeExportTasksResponse
--
--         , testDeletePlacementGroupResponse $
--             deletePlacementGroupResponse
--
--         , testCreateSubnetResponse $
--             createSubnetResponse
--
--         , testEnableVolumeIOResponse $
--             enableVolumeIOResponse
--
--         , testCancelExportTaskResponse $
--             cancelExportTaskResponse
--
--         , testRequestSpotFleetResponse $
--             requestSpotFleetResponse
--
--         , testDescribeInstancesResponse $
--             describeInstancesResponse
--
--         , testDescribeSecurityGroupsResponse $
--             describeSecurityGroupsResponse
--
--         , testDescribeVPCPeeringConnectionsResponse $
--             describeVPCPeeringConnectionsResponse
--
--         , testCreateNetworkInterfaceResponse $
--             createNetworkInterfaceResponse
--
--         , testAssociateAddressResponse $
--             associateAddressResponse
--
--         , testStartInstancesResponse $
--             startInstancesResponse
--
--         , testDescribeCustomerGatewaysResponse $
--             describeCustomerGatewaysResponse
--
--         , testResetNetworkInterfaceAttributeResponse $
--             resetNetworkInterfaceAttributeResponse
--
--         , testCreateVPNConnectionResponse $
--             createVPNConnectionResponse
--
--         , testDescribeSnapshotsResponse $
--             describeSnapshotsResponse
--
--         , testCreatePlacementGroupResponse $
--             createPlacementGroupResponse
--
--         , testReplaceRouteTableAssociationResponse $
--             replaceRouteTableAssociationResponse
--
--         , testDescribeNetworkInterfaceAttributeResponse $
--             describeNetworkInterfaceAttributeResponse
--
--         , testDescribeReservedInstancesListingsResponse $
--             describeReservedInstancesListingsResponse
--
--         , testDeleteNetworkInterfaceResponse $
--             deleteNetworkInterfaceResponse
--
--         , testDeleteInternetGatewayResponse $
--             deleteInternetGatewayResponse
--
--         , testDeleteSubnetResponse $
--             deleteSubnetResponse
--
--         , testCreateVPCEndpointResponse $
--             createVPCEndpointResponse
--
--         , testDescribeImportSnapshotTasksResponse $
--             describeImportSnapshotTasksResponse
--
--         , testCopyImageResponse $
--             copyImageResponse
--
--         , testDisassociateRouteTableResponse $
--             disassociateRouteTableResponse
--
--         , testUnmonitorInstancesResponse $
--             unmonitorInstancesResponse
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
--         , testAssignPrivateIPAddressesResponse $
--             assignPrivateIPAddressesResponse
--
--         , testDeleteSnapshotResponse $
--             deleteSnapshotResponse
--
--         , testDeleteCustomerGatewayResponse $
--             deleteCustomerGatewayResponse
--
--         , testModifyInstanceAttributeResponse $
--             modifyInstanceAttributeResponse
--
--         , testCreateSecurityGroupResponse $
--             createSecurityGroupResponse
--
--         , testCancelSpotInstanceRequestsResponse $
--             cancelSpotInstanceRequestsResponse
--
--         , testCreateRouteResponse $
--             createRouteResponse
--
--         , testCreateNetworkACLEntryResponse $
--             createNetworkACLEntryResponse
--
--         , testModifySnapshotAttributeResponse $
--             modifySnapshotAttributeResponse
--
--         , testEnableVGWRoutePropagationResponse $
--             enableVGWRoutePropagationResponse
--
--         , testCreateSnapshotResponse $
--             snapshot
--
--         , testDescribeSpotFleetRequestHistoryResponse $
--             describeSpotFleetRequestHistoryResponse
--
--         , testDeleteSpotDatafeedSubscriptionResponse $
--             deleteSpotDatafeedSubscriptionResponse
--
--         , testDescribePlacementGroupsResponse $
--             describePlacementGroupsResponse
--
--         , testCreateReservedInstancesListingResponse $
--             createReservedInstancesListingResponse
--
--         , testEnableVPCClassicLinkResponse $
--             enableVPCClassicLinkResponse
--
--         , testDescribeKeyPairsResponse $
--             describeKeyPairsResponse
--
--         , testRebootInstancesResponse $
--             rebootInstancesResponse
--
--         , testAttachVPNGatewayResponse $
--             attachVPNGatewayResponse
--
--         , testCreateVPNConnectionRouteResponse $
--             createVPNConnectionRouteResponse
--
--         , testDescribeClassicLinkInstancesResponse $
--             describeClassicLinkInstancesResponse
--
--         , testTerminateInstancesResponse $
--             terminateInstancesResponse
--
--         , testCreateDHCPOptionsResponse $
--             createDHCPOptionsResponse
--
--         , testAssociateRouteTableResponse $
--             associateRouteTableResponse
--
--         , testCreateImageResponse $
--             createImageResponse
--
--         , testDescribeAccountAttributesResponse $
--             describeAccountAttributesResponse
--
--         , testResetImageAttributeResponse $
--             resetImageAttributeResponse
--
--         , testDescribeNetworkACLsResponse $
--             describeNetworkACLsResponse
--
--         , testCancelImportTaskResponse $
--             cancelImportTaskResponse
--
--         , testGetConsoleOutputResponse $
--             getConsoleOutputResponse
--
--         , testUnassignPrivateIPAddressesResponse $
--             unassignPrivateIPAddressesResponse
--
--         , testDeleteRouteTableResponse $
--             deleteRouteTableResponse
--
--         , testDescribeImageAttributeResponse $
--             describeImageAttributeResponse
--
--         , testDeleteDHCPOptionsResponse $
--             deleteDHCPOptionsResponse
--
--         , testDescribeVPNGatewaysResponse $
--             describeVPNGatewaysResponse
--
--         , testDetachClassicLinkVPCResponse $
--             detachClassicLinkVPCResponse
--
--         , testDescribeReservedInstancesModificationsResponse $
--             describeReservedInstancesModificationsResponse
--
--         , testDescribeSpotInstanceRequestsResponse $
--             describeSpotInstanceRequestsResponse
--
--         , testMonitorInstancesResponse $
--             monitorInstancesResponse
--
--         , testDescribeRegionsResponse $
--             describeRegionsResponse
--
--         , testModifyVPCAttributeResponse $
--             modifyVPCAttributeResponse
--
--         , testDescribeSpotFleetInstancesResponse $
--             describeSpotFleetInstancesResponse
--
--         , testDescribeVolumeStatusResponse $
--             describeVolumeStatusResponse
--
--         , testDeleteVolumeResponse $
--             deleteVolumeResponse
--
--         , testDescribeImagesResponse $
--             describeImagesResponse
--
--         , testCreateKeyPairResponse $
--             createKeyPairResponse
--
--         , testRestoreAddressToClassicResponse $
--             restoreAddressToClassicResponse
--
--         , testDescribeAvailabilityZonesResponse $
--             describeAvailabilityZonesResponse
--
--         , testImportSnapshotResponse $
--             importSnapshotResponse
--
--         , testAcceptVPCPeeringConnectionResponse $
--             acceptVPCPeeringConnectionResponse
--
--         , testDescribeRouteTablesResponse $
--             describeRouteTablesResponse
--
--           ]
--     ]

-- Requests

testDetachNetworkInterface :: DetachNetworkInterface -> TestTree
testDetachNetworkInterface = undefined

testDeleteVPCEndpoints :: DeleteVPCEndpoints -> TestTree
testDeleteVPCEndpoints = undefined

testDeleteKeyPair :: DeleteKeyPair -> TestTree
testDeleteKeyPair = undefined

testDeleteFlowLogs :: DeleteFlowLogs -> TestTree
testDeleteFlowLogs = undefined

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = undefined

testRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
testRevokeSecurityGroupEgress = undefined

testCreateVPNGateway :: CreateVPNGateway -> TestTree
testCreateVPNGateway = undefined

testDetachInternetGateway :: DetachInternetGateway -> TestTree
testDetachInternetGateway = undefined

testCreateNetworkACL :: CreateNetworkACL -> TestTree
testCreateNetworkACL = undefined

testImportInstance :: ImportInstance -> TestTree
testImportInstance = undefined

testDescribeVPCClassicLink :: DescribeVPCClassicLink -> TestTree
testDescribeVPCClassicLink = undefined

testDeleteVPNConnection :: DeleteVPNConnection -> TestTree
testDeleteVPNConnection = undefined

testAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
testAuthorizeSecurityGroupEgress = undefined

testDescribeBundleTasks :: DescribeBundleTasks -> TestTree
testDescribeBundleTasks = undefined

testCreateInternetGateway :: CreateInternetGateway -> TestTree
testCreateInternetGateway = undefined

testReleaseAddress :: ReleaseAddress -> TestTree
testReleaseAddress = undefined

testCancelBundleTask :: CancelBundleTask -> TestTree
testCancelBundleTask = undefined

testModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
testModifyNetworkInterfaceAttribute = undefined

testModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
testModifySubnetAttribute = undefined

testDeregisterImage :: DeregisterImage -> TestTree
testDeregisterImage = undefined

testDetachVolume :: DetachVolume -> TestTree
testDetachVolume = undefined

testCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
testCancelReservedInstancesListing = undefined

testAttachClassicLinkVPC :: AttachClassicLinkVPC -> TestTree
testAttachClassicLinkVPC = undefined

testCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
testCancelSpotFleetRequests = undefined

testDescribeDHCPOptions :: DescribeDHCPOptions -> TestTree
testDescribeDHCPOptions = undefined

testDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
testDescribeSpotPriceHistory = undefined

testStopInstances :: StopInstances -> TestTree
testStopInstances = undefined

testImportImage :: ImportImage -> TestTree
testImportImage = undefined

testDeleteNetworkACLEntry :: DeleteNetworkACLEntry -> TestTree
testDeleteNetworkACLEntry = undefined

testDisableVPCClassicLink :: DisableVPCClassicLink -> TestTree
testDisableVPCClassicLink = undefined

testAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
testAuthorizeSecurityGroupIngress = undefined

testBundleInstance :: BundleInstance -> TestTree
testBundleInstance = undefined

testDescribeVPCEndpointServices :: DescribeVPCEndpointServices -> TestTree
testDescribeVPCEndpointServices = undefined

testReplaceNetworkACLAssociation :: ReplaceNetworkACLAssociation -> TestTree
testReplaceNetworkACLAssociation = undefined

testCreateVPCPeeringConnection :: CreateVPCPeeringConnection -> TestTree
testCreateVPCPeeringConnection = undefined

testResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
testResetSnapshotAttribute = undefined

testDescribeAddresses :: DescribeAddresses -> TestTree
testDescribeAddresses = undefined

testDescribeInternetGateways :: DescribeInternetGateways -> TestTree
testDescribeInternetGateways = undefined

testReplaceRoute :: ReplaceRoute -> TestTree
testReplaceRoute = undefined

testCreateTags :: CreateTags -> TestTree
testCreateTags = undefined

testDescribeSubnets :: DescribeSubnets -> TestTree
testDescribeSubnets = undefined

testDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
testDescribeNetworkInterfaces = undefined

testPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
testPurchaseReservedInstancesOffering = undefined

testDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
testDescribeSnapshotAttribute = undefined

testCreateCustomerGateway :: CreateCustomerGateway -> TestTree
testCreateCustomerGateway = undefined

testAttachInternetGateway :: AttachInternetGateway -> TestTree
testAttachInternetGateway = undefined

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = undefined

testReplaceNetworkACLEntry :: ReplaceNetworkACLEntry -> TestTree
testReplaceNetworkACLEntry = undefined

testResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
testResetInstanceAttribute = undefined

testDeleteRoute :: DeleteRoute -> TestTree
testDeleteRoute = undefined

testDescribeVPNConnections :: DescribeVPNConnections -> TestTree
testDescribeVPNConnections = undefined

testDescribeFlowLogs :: DescribeFlowLogs -> TestTree
testDescribeFlowLogs = undefined

testDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
testDeleteSecurityGroup = undefined

testDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
testDescribeReservedInstancesOfferings = undefined

testDeleteVPCPeeringConnection :: DeleteVPCPeeringConnection -> TestTree
testDeleteVPCPeeringConnection = undefined

testDescribeVPCEndpoints :: DescribeVPCEndpoints -> TestTree
testDescribeVPCEndpoints = undefined

testDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
testDescribeInstanceAttribute = undefined

testConfirmProductInstance :: ConfirmProductInstance -> TestTree
testConfirmProductInstance = undefined

testImportKeyPair :: ImportKeyPair -> TestTree
testImportKeyPair = undefined

testAttachNetworkInterface :: AttachNetworkInterface -> TestTree
testAttachNetworkInterface = undefined

testDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
testDescribeInstanceStatus = undefined

testCancelConversionTask :: CancelConversionTask -> TestTree
testCancelConversionTask = undefined

testReportInstanceStatus :: ReportInstanceStatus -> TestTree
testReportInstanceStatus = undefined

testAssociateDHCPOptions :: AssociateDHCPOptions -> TestTree
testAssociateDHCPOptions = undefined

testDescribeVPCs :: DescribeVPCs -> TestTree
testDescribeVPCs = undefined

testRequestSpotInstances :: RequestSpotInstances -> TestTree
testRequestSpotInstances = undefined

testModifyImageAttribute :: ModifyImageAttribute -> TestTree
testModifyImageAttribute = undefined

testDescribeReservedInstances :: DescribeReservedInstances -> TestTree
testDescribeReservedInstances = undefined

testAllocateAddress :: AllocateAddress -> TestTree
testAllocateAddress = undefined

testRunInstances :: RunInstances -> TestTree
testRunInstances = undefined

testCreateRouteTable :: CreateRouteTable -> TestTree
testCreateRouteTable = undefined

testAttachVolume :: AttachVolume -> TestTree
testAttachVolume = undefined

testDescribeConversionTasks :: DescribeConversionTasks -> TestTree
testDescribeConversionTasks = undefined

testRejectVPCPeeringConnection :: RejectVPCPeeringConnection -> TestTree
testRejectVPCPeeringConnection = undefined

testRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
testRevokeSecurityGroupIngress = undefined

testDescribeVolumes :: DescribeVolumes -> TestTree
testDescribeVolumes = undefined

testDeleteVPNConnectionRoute :: DeleteVPNConnectionRoute -> TestTree
testDeleteVPNConnectionRoute = undefined

testModifyReservedInstances :: ModifyReservedInstances -> TestTree
testModifyReservedInstances = undefined

testRegisterImage :: RegisterImage -> TestTree
testRegisterImage = undefined

testModifyVPCEndpoint :: ModifyVPCEndpoint -> TestTree
testModifyVPCEndpoint = undefined

testDeleteVPNGateway :: DeleteVPNGateway -> TestTree
testDeleteVPNGateway = undefined

testCreateVPC :: CreateVPC -> TestTree
testCreateVPC = undefined

testDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
testDescribeMovingAddresses = undefined

testDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
testDescribeVolumeAttribute = undefined

testMoveAddressToVPC :: MoveAddressToVPC -> TestTree
testMoveAddressToVPC = undefined

testGetPasswordData :: GetPasswordData -> TestTree
testGetPasswordData = undefined

testCreateFlowLogs :: CreateFlowLogs -> TestTree
testCreateFlowLogs = undefined

testDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
testDescribeImportImageTasks = undefined

testDeleteNetworkACL :: DeleteNetworkACL -> TestTree
testDeleteNetworkACL = undefined

testDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
testDescribeSpotFleetRequests = undefined

testCopySnapshot :: CopySnapshot -> TestTree
testCopySnapshot = undefined

testModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
testModifyVolumeAttribute = undefined

testDescribeVPCAttribute :: DescribeVPCAttribute -> TestTree
testDescribeVPCAttribute = undefined

testCreateVolume :: CreateVolume -> TestTree
testCreateVolume = undefined

testDisassociateAddress :: DisassociateAddress -> TestTree
testDisassociateAddress = undefined

testDeleteVPC :: DeleteVPC -> TestTree
testDeleteVPC = undefined

testDescribePrefixLists :: DescribePrefixLists -> TestTree
testDescribePrefixLists = undefined

testCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
testCreateInstanceExportTask = undefined

testDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
testDescribeSpotDatafeedSubscription = undefined

testDetachVPNGateway :: DetachVPNGateway -> TestTree
testDetachVPNGateway = undefined

testDescribeExportTasks :: DescribeExportTasks -> TestTree
testDescribeExportTasks = undefined

testDeletePlacementGroup :: DeletePlacementGroup -> TestTree
testDeletePlacementGroup = undefined

testCreateSubnet :: CreateSubnet -> TestTree
testCreateSubnet = undefined

testEnableVolumeIO :: EnableVolumeIO -> TestTree
testEnableVolumeIO = undefined

testCancelExportTask :: CancelExportTask -> TestTree
testCancelExportTask = undefined

testRequestSpotFleet :: RequestSpotFleet -> TestTree
testRequestSpotFleet = undefined

testDescribeInstances :: DescribeInstances -> TestTree
testDescribeInstances = undefined

testDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
testDescribeSecurityGroups = undefined

testDescribeVPCPeeringConnections :: DescribeVPCPeeringConnections -> TestTree
testDescribeVPCPeeringConnections = undefined

testCreateNetworkInterface :: CreateNetworkInterface -> TestTree
testCreateNetworkInterface = undefined

testAssociateAddress :: AssociateAddress -> TestTree
testAssociateAddress = undefined

testStartInstances :: StartInstances -> TestTree
testStartInstances = undefined

testDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
testDescribeCustomerGateways = undefined

testResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
testResetNetworkInterfaceAttribute = undefined

testCreateVPNConnection :: CreateVPNConnection -> TestTree
testCreateVPNConnection = undefined

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = undefined

testCreatePlacementGroup :: CreatePlacementGroup -> TestTree
testCreatePlacementGroup = undefined

testReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
testReplaceRouteTableAssociation = undefined

testDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
testDescribeNetworkInterfaceAttribute = undefined

testDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
testDescribeReservedInstancesListings = undefined

testDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
testDeleteNetworkInterface = undefined

testDeleteInternetGateway :: DeleteInternetGateway -> TestTree
testDeleteInternetGateway = undefined

testDeleteSubnet :: DeleteSubnet -> TestTree
testDeleteSubnet = undefined

testCreateVPCEndpoint :: CreateVPCEndpoint -> TestTree
testCreateVPCEndpoint = undefined

testDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
testDescribeImportSnapshotTasks = undefined

testCopyImage :: CopyImage -> TestTree
testCopyImage = undefined

testDisassociateRouteTable :: DisassociateRouteTable -> TestTree
testDisassociateRouteTable = undefined

testUnmonitorInstances :: UnmonitorInstances -> TestTree
testUnmonitorInstances = undefined

testImportVolume :: ImportVolume -> TestTree
testImportVolume = undefined

testDisableVGWRoutePropagation :: DisableVGWRoutePropagation -> TestTree
testDisableVGWRoutePropagation = undefined

testCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscription -> TestTree
testCreateSpotDatafeedSubscription = undefined

testAssignPrivateIPAddresses :: AssignPrivateIPAddresses -> TestTree
testAssignPrivateIPAddresses = undefined

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = undefined

testDeleteCustomerGateway :: DeleteCustomerGateway -> TestTree
testDeleteCustomerGateway = undefined

testModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
testModifyInstanceAttribute = undefined

testCreateSecurityGroup :: CreateSecurityGroup -> TestTree
testCreateSecurityGroup = undefined

testCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
testCancelSpotInstanceRequests = undefined

testCreateRoute :: CreateRoute -> TestTree
testCreateRoute = undefined

testCreateNetworkACLEntry :: CreateNetworkACLEntry -> TestTree
testCreateNetworkACLEntry = undefined

testModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
testModifySnapshotAttribute = undefined

testEnableVGWRoutePropagation :: EnableVGWRoutePropagation -> TestTree
testEnableVGWRoutePropagation = undefined

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = undefined

testDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
testDescribeSpotFleetRequestHistory = undefined

testDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
testDeleteSpotDatafeedSubscription = undefined

testDescribePlacementGroups :: DescribePlacementGroups -> TestTree
testDescribePlacementGroups = undefined

testCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
testCreateReservedInstancesListing = undefined

testEnableVPCClassicLink :: EnableVPCClassicLink -> TestTree
testEnableVPCClassicLink = undefined

testDescribeKeyPairs :: DescribeKeyPairs -> TestTree
testDescribeKeyPairs = undefined

testRebootInstances :: RebootInstances -> TestTree
testRebootInstances = undefined

testAttachVPNGateway :: AttachVPNGateway -> TestTree
testAttachVPNGateway = undefined

testCreateVPNConnectionRoute :: CreateVPNConnectionRoute -> TestTree
testCreateVPNConnectionRoute = undefined

testDescribeClassicLinkInstances :: DescribeClassicLinkInstances -> TestTree
testDescribeClassicLinkInstances = undefined

testTerminateInstances :: TerminateInstances -> TestTree
testTerminateInstances = undefined

testCreateDHCPOptions :: CreateDHCPOptions -> TestTree
testCreateDHCPOptions = undefined

testAssociateRouteTable :: AssociateRouteTable -> TestTree
testAssociateRouteTable = undefined

testCreateImage :: CreateImage -> TestTree
testCreateImage = undefined

testDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
testDescribeAccountAttributes = undefined

testResetImageAttribute :: ResetImageAttribute -> TestTree
testResetImageAttribute = undefined

testDescribeNetworkACLs :: DescribeNetworkACLs -> TestTree
testDescribeNetworkACLs = undefined

testCancelImportTask :: CancelImportTask -> TestTree
testCancelImportTask = undefined

testGetConsoleOutput :: GetConsoleOutput -> TestTree
testGetConsoleOutput = undefined

testUnassignPrivateIPAddresses :: UnassignPrivateIPAddresses -> TestTree
testUnassignPrivateIPAddresses = undefined

testDeleteRouteTable :: DeleteRouteTable -> TestTree
testDeleteRouteTable = undefined

testDescribeImageAttribute :: DescribeImageAttribute -> TestTree
testDescribeImageAttribute = undefined

testDeleteDHCPOptions :: DeleteDHCPOptions -> TestTree
testDeleteDHCPOptions = undefined

testDescribeVPNGateways :: DescribeVPNGateways -> TestTree
testDescribeVPNGateways = undefined

testDetachClassicLinkVPC :: DetachClassicLinkVPC -> TestTree
testDetachClassicLinkVPC = undefined

testDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
testDescribeReservedInstancesModifications = undefined

testDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
testDescribeSpotInstanceRequests = undefined

testMonitorInstances :: MonitorInstances -> TestTree
testMonitorInstances = undefined

testDescribeRegions :: DescribeRegions -> TestTree
testDescribeRegions = undefined

testModifyVPCAttribute :: ModifyVPCAttribute -> TestTree
testModifyVPCAttribute = undefined

testDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
testDescribeSpotFleetInstances = undefined

testDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
testDescribeVolumeStatus = undefined

testDeleteVolume :: DeleteVolume -> TestTree
testDeleteVolume = undefined

testDescribeImages :: DescribeImages -> TestTree
testDescribeImages = undefined

testCreateKeyPair :: CreateKeyPair -> TestTree
testCreateKeyPair = undefined

testRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
testRestoreAddressToClassic = undefined

testDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
testDescribeAvailabilityZones = undefined

testImportSnapshot :: ImportSnapshot -> TestTree
testImportSnapshot = undefined

testAcceptVPCPeeringConnection :: AcceptVPCPeeringConnection -> TestTree
testAcceptVPCPeeringConnection = undefined

testDescribeRouteTables :: DescribeRouteTables -> TestTree
testDescribeRouteTables = undefined

-- Responses

testDetachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse -> TestTree
testDetachNetworkInterfaceResponse = resp
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse"
    (Proxy :: Proxy DetachNetworkInterface)

testDeleteVPCEndpointsResponse :: DeleteVPCEndpointsResponse -> TestTree
testDeleteVPCEndpointsResponse = resp
    "DeleteVPCEndpointsResponse"
    "fixture/DeleteVPCEndpointsResponse"
    (Proxy :: Proxy DeleteVPCEndpoints)

testDeleteKeyPairResponse :: DeleteKeyPairResponse -> TestTree
testDeleteKeyPairResponse = resp
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse"
    (Proxy :: Proxy DeleteKeyPair)

testDeleteFlowLogsResponse :: DeleteFlowLogsResponse -> TestTree
testDeleteFlowLogsResponse = resp
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse"
    (Proxy :: Proxy DeleteFlowLogs)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = resp
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

testRevokeSecurityGroupEgressResponse :: RevokeSecurityGroupEgressResponse -> TestTree
testRevokeSecurityGroupEgressResponse = resp
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse"
    (Proxy :: Proxy RevokeSecurityGroupEgress)

testCreateVPNGatewayResponse :: CreateVPNGatewayResponse -> TestTree
testCreateVPNGatewayResponse = resp
    "CreateVPNGatewayResponse"
    "fixture/CreateVPNGatewayResponse"
    (Proxy :: Proxy CreateVPNGateway)

testDetachInternetGatewayResponse :: DetachInternetGatewayResponse -> TestTree
testDetachInternetGatewayResponse = resp
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse"
    (Proxy :: Proxy DetachInternetGateway)

testCreateNetworkACLResponse :: CreateNetworkACLResponse -> TestTree
testCreateNetworkACLResponse = resp
    "CreateNetworkACLResponse"
    "fixture/CreateNetworkACLResponse"
    (Proxy :: Proxy CreateNetworkACL)

testImportInstanceResponse :: ImportInstanceResponse -> TestTree
testImportInstanceResponse = resp
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse"
    (Proxy :: Proxy ImportInstance)

testDescribeVPCClassicLinkResponse :: DescribeVPCClassicLinkResponse -> TestTree
testDescribeVPCClassicLinkResponse = resp
    "DescribeVPCClassicLinkResponse"
    "fixture/DescribeVPCClassicLinkResponse"
    (Proxy :: Proxy DescribeVPCClassicLink)

testDeleteVPNConnectionResponse :: DeleteVPNConnectionResponse -> TestTree
testDeleteVPNConnectionResponse = resp
    "DeleteVPNConnectionResponse"
    "fixture/DeleteVPNConnectionResponse"
    (Proxy :: Proxy DeleteVPNConnection)

testAuthorizeSecurityGroupEgressResponse :: AuthorizeSecurityGroupEgressResponse -> TestTree
testAuthorizeSecurityGroupEgressResponse = resp
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse"
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

testDescribeBundleTasksResponse :: DescribeBundleTasksResponse -> TestTree
testDescribeBundleTasksResponse = resp
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse"
    (Proxy :: Proxy DescribeBundleTasks)

testCreateInternetGatewayResponse :: CreateInternetGatewayResponse -> TestTree
testCreateInternetGatewayResponse = resp
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse"
    (Proxy :: Proxy CreateInternetGateway)

testReleaseAddressResponse :: ReleaseAddressResponse -> TestTree
testReleaseAddressResponse = resp
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse"
    (Proxy :: Proxy ReleaseAddress)

testCancelBundleTaskResponse :: CancelBundleTaskResponse -> TestTree
testCancelBundleTaskResponse = resp
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse"
    (Proxy :: Proxy CancelBundleTask)

testModifyNetworkInterfaceAttributeResponse :: ModifyNetworkInterfaceAttributeResponse -> TestTree
testModifyNetworkInterfaceAttributeResponse = resp
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

testModifySubnetAttributeResponse :: ModifySubnetAttributeResponse -> TestTree
testModifySubnetAttributeResponse = resp
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse"
    (Proxy :: Proxy ModifySubnetAttribute)

testDeregisterImageResponse :: DeregisterImageResponse -> TestTree
testDeregisterImageResponse = resp
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse"
    (Proxy :: Proxy DeregisterImage)

testDetachVolumeResponse :: VolumeAttachment -> TestTree
testDetachVolumeResponse = resp
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse"
    (Proxy :: Proxy DetachVolume)

testCancelReservedInstancesListingResponse :: CancelReservedInstancesListingResponse -> TestTree
testCancelReservedInstancesListingResponse = resp
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse"
    (Proxy :: Proxy CancelReservedInstancesListing)

testAttachClassicLinkVPCResponse :: AttachClassicLinkVPCResponse -> TestTree
testAttachClassicLinkVPCResponse = resp
    "AttachClassicLinkVPCResponse"
    "fixture/AttachClassicLinkVPCResponse"
    (Proxy :: Proxy AttachClassicLinkVPC)

testCancelSpotFleetRequestsResponse :: CancelSpotFleetRequestsResponse -> TestTree
testCancelSpotFleetRequestsResponse = resp
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse"
    (Proxy :: Proxy CancelSpotFleetRequests)

testDescribeDHCPOptionsResponse :: DescribeDHCPOptionsResponse -> TestTree
testDescribeDHCPOptionsResponse = resp
    "DescribeDHCPOptionsResponse"
    "fixture/DescribeDHCPOptionsResponse"
    (Proxy :: Proxy DescribeDHCPOptions)

testDescribeSpotPriceHistoryResponse :: DescribeSpotPriceHistoryResponse -> TestTree
testDescribeSpotPriceHistoryResponse = resp
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse"
    (Proxy :: Proxy DescribeSpotPriceHistory)

testStopInstancesResponse :: StopInstancesResponse -> TestTree
testStopInstancesResponse = resp
    "StopInstancesResponse"
    "fixture/StopInstancesResponse"
    (Proxy :: Proxy StopInstances)

testImportImageResponse :: ImportImageResponse -> TestTree
testImportImageResponse = resp
    "ImportImageResponse"
    "fixture/ImportImageResponse"
    (Proxy :: Proxy ImportImage)

testDeleteNetworkACLEntryResponse :: DeleteNetworkACLEntryResponse -> TestTree
testDeleteNetworkACLEntryResponse = resp
    "DeleteNetworkACLEntryResponse"
    "fixture/DeleteNetworkACLEntryResponse"
    (Proxy :: Proxy DeleteNetworkACLEntry)

testDisableVPCClassicLinkResponse :: DisableVPCClassicLinkResponse -> TestTree
testDisableVPCClassicLinkResponse = resp
    "DisableVPCClassicLinkResponse"
    "fixture/DisableVPCClassicLinkResponse"
    (Proxy :: Proxy DisableVPCClassicLink)

testAuthorizeSecurityGroupIngressResponse :: AuthorizeSecurityGroupIngressResponse -> TestTree
testAuthorizeSecurityGroupIngressResponse = resp
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

testBundleInstanceResponse :: BundleInstanceResponse -> TestTree
testBundleInstanceResponse = resp
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse"
    (Proxy :: Proxy BundleInstance)

testDescribeVPCEndpointServicesResponse :: DescribeVPCEndpointServicesResponse -> TestTree
testDescribeVPCEndpointServicesResponse = resp
    "DescribeVPCEndpointServicesResponse"
    "fixture/DescribeVPCEndpointServicesResponse"
    (Proxy :: Proxy DescribeVPCEndpointServices)

testReplaceNetworkACLAssociationResponse :: ReplaceNetworkACLAssociationResponse -> TestTree
testReplaceNetworkACLAssociationResponse = resp
    "ReplaceNetworkACLAssociationResponse"
    "fixture/ReplaceNetworkACLAssociationResponse"
    (Proxy :: Proxy ReplaceNetworkACLAssociation)

testCreateVPCPeeringConnectionResponse :: CreateVPCPeeringConnectionResponse -> TestTree
testCreateVPCPeeringConnectionResponse = resp
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse"
    (Proxy :: Proxy CreateVPCPeeringConnection)

testResetSnapshotAttributeResponse :: ResetSnapshotAttributeResponse -> TestTree
testResetSnapshotAttributeResponse = resp
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse"
    (Proxy :: Proxy ResetSnapshotAttribute)

testDescribeAddressesResponse :: DescribeAddressesResponse -> TestTree
testDescribeAddressesResponse = resp
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse"
    (Proxy :: Proxy DescribeAddresses)

testDescribeInternetGatewaysResponse :: DescribeInternetGatewaysResponse -> TestTree
testDescribeInternetGatewaysResponse = resp
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse"
    (Proxy :: Proxy DescribeInternetGateways)

testReplaceRouteResponse :: ReplaceRouteResponse -> TestTree
testReplaceRouteResponse = resp
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse"
    (Proxy :: Proxy ReplaceRoute)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = resp
    "CreateTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

testDescribeSubnetsResponse :: DescribeSubnetsResponse -> TestTree
testDescribeSubnetsResponse = resp
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse"
    (Proxy :: Proxy DescribeSubnets)

testDescribeNetworkInterfacesResponse :: DescribeNetworkInterfacesResponse -> TestTree
testDescribeNetworkInterfacesResponse = resp
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse"
    (Proxy :: Proxy DescribeNetworkInterfaces)

testPurchaseReservedInstancesOfferingResponse :: PurchaseReservedInstancesOfferingResponse -> TestTree
testPurchaseReservedInstancesOfferingResponse = resp
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

testDescribeSnapshotAttributeResponse :: DescribeSnapshotAttributeResponse -> TestTree
testDescribeSnapshotAttributeResponse = resp
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse"
    (Proxy :: Proxy DescribeSnapshotAttribute)

testCreateCustomerGatewayResponse :: CreateCustomerGatewayResponse -> TestTree
testCreateCustomerGatewayResponse = resp
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse"
    (Proxy :: Proxy CreateCustomerGateway)

testAttachInternetGatewayResponse :: AttachInternetGatewayResponse -> TestTree
testAttachInternetGatewayResponse = resp
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse"
    (Proxy :: Proxy AttachInternetGateway)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = resp
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

testReplaceNetworkACLEntryResponse :: ReplaceNetworkACLEntryResponse -> TestTree
testReplaceNetworkACLEntryResponse = resp
    "ReplaceNetworkACLEntryResponse"
    "fixture/ReplaceNetworkACLEntryResponse"
    (Proxy :: Proxy ReplaceNetworkACLEntry)

testResetInstanceAttributeResponse :: ResetInstanceAttributeResponse -> TestTree
testResetInstanceAttributeResponse = resp
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse"
    (Proxy :: Proxy ResetInstanceAttribute)

testDeleteRouteResponse :: DeleteRouteResponse -> TestTree
testDeleteRouteResponse = resp
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse"
    (Proxy :: Proxy DeleteRoute)

testDescribeVPNConnectionsResponse :: DescribeVPNConnectionsResponse -> TestTree
testDescribeVPNConnectionsResponse = resp
    "DescribeVPNConnectionsResponse"
    "fixture/DescribeVPNConnectionsResponse"
    (Proxy :: Proxy DescribeVPNConnections)

testDescribeFlowLogsResponse :: DescribeFlowLogsResponse -> TestTree
testDescribeFlowLogsResponse = resp
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse"
    (Proxy :: Proxy DescribeFlowLogs)

testDeleteSecurityGroupResponse :: DeleteSecurityGroupResponse -> TestTree
testDeleteSecurityGroupResponse = resp
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse"
    (Proxy :: Proxy DeleteSecurityGroup)

testDescribeReservedInstancesOfferingsResponse :: DescribeReservedInstancesOfferingsResponse -> TestTree
testDescribeReservedInstancesOfferingsResponse = resp
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

testDeleteVPCPeeringConnectionResponse :: DeleteVPCPeeringConnectionResponse -> TestTree
testDeleteVPCPeeringConnectionResponse = resp
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse"
    (Proxy :: Proxy DeleteVPCPeeringConnection)

testDescribeVPCEndpointsResponse :: DescribeVPCEndpointsResponse -> TestTree
testDescribeVPCEndpointsResponse = resp
    "DescribeVPCEndpointsResponse"
    "fixture/DescribeVPCEndpointsResponse"
    (Proxy :: Proxy DescribeVPCEndpoints)

testDescribeInstanceAttributeResponse :: DescribeInstanceAttributeResponse -> TestTree
testDescribeInstanceAttributeResponse = resp
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse"
    (Proxy :: Proxy DescribeInstanceAttribute)

testConfirmProductInstanceResponse :: ConfirmProductInstanceResponse -> TestTree
testConfirmProductInstanceResponse = resp
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse"
    (Proxy :: Proxy ConfirmProductInstance)

testImportKeyPairResponse :: ImportKeyPairResponse -> TestTree
testImportKeyPairResponse = resp
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse"
    (Proxy :: Proxy ImportKeyPair)

testAttachNetworkInterfaceResponse :: AttachNetworkInterfaceResponse -> TestTree
testAttachNetworkInterfaceResponse = resp
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse"
    (Proxy :: Proxy AttachNetworkInterface)

testDescribeInstanceStatusResponse :: DescribeInstanceStatusResponse -> TestTree
testDescribeInstanceStatusResponse = resp
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse"
    (Proxy :: Proxy DescribeInstanceStatus)

testCancelConversionTaskResponse :: CancelConversionTaskResponse -> TestTree
testCancelConversionTaskResponse = resp
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse"
    (Proxy :: Proxy CancelConversionTask)

testReportInstanceStatusResponse :: ReportInstanceStatusResponse -> TestTree
testReportInstanceStatusResponse = resp
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse"
    (Proxy :: Proxy ReportInstanceStatus)

testAssociateDHCPOptionsResponse :: AssociateDHCPOptionsResponse -> TestTree
testAssociateDHCPOptionsResponse = resp
    "AssociateDHCPOptionsResponse"
    "fixture/AssociateDHCPOptionsResponse"
    (Proxy :: Proxy AssociateDHCPOptions)

testDescribeVPCsResponse :: DescribeVPCsResponse -> TestTree
testDescribeVPCsResponse = resp
    "DescribeVPCsResponse"
    "fixture/DescribeVPCsResponse"
    (Proxy :: Proxy DescribeVPCs)

testRequestSpotInstancesResponse :: RequestSpotInstancesResponse -> TestTree
testRequestSpotInstancesResponse = resp
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse"
    (Proxy :: Proxy RequestSpotInstances)

testModifyImageAttributeResponse :: ModifyImageAttributeResponse -> TestTree
testModifyImageAttributeResponse = resp
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse"
    (Proxy :: Proxy ModifyImageAttribute)

testDescribeReservedInstancesResponse :: DescribeReservedInstancesResponse -> TestTree
testDescribeReservedInstancesResponse = resp
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse"
    (Proxy :: Proxy DescribeReservedInstances)

testAllocateAddressResponse :: AllocateAddressResponse -> TestTree
testAllocateAddressResponse = resp
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse"
    (Proxy :: Proxy AllocateAddress)

testRunInstancesResponse :: Reservation -> TestTree
testRunInstancesResponse = resp
    "RunInstancesResponse"
    "fixture/RunInstancesResponse"
    (Proxy :: Proxy RunInstances)

testCreateRouteTableResponse :: CreateRouteTableResponse -> TestTree
testCreateRouteTableResponse = resp
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse"
    (Proxy :: Proxy CreateRouteTable)

testAttachVolumeResponse :: VolumeAttachment -> TestTree
testAttachVolumeResponse = resp
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse"
    (Proxy :: Proxy AttachVolume)

testDescribeConversionTasksResponse :: DescribeConversionTasksResponse -> TestTree
testDescribeConversionTasksResponse = resp
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse"
    (Proxy :: Proxy DescribeConversionTasks)

testRejectVPCPeeringConnectionResponse :: RejectVPCPeeringConnectionResponse -> TestTree
testRejectVPCPeeringConnectionResponse = resp
    "RejectVPCPeeringConnectionResponse"
    "fixture/RejectVPCPeeringConnectionResponse"
    (Proxy :: Proxy RejectVPCPeeringConnection)

testRevokeSecurityGroupIngressResponse :: RevokeSecurityGroupIngressResponse -> TestTree
testRevokeSecurityGroupIngressResponse = resp
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeSecurityGroupIngress)

testDescribeVolumesResponse :: DescribeVolumesResponse -> TestTree
testDescribeVolumesResponse = resp
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse"
    (Proxy :: Proxy DescribeVolumes)

testDeleteVPNConnectionRouteResponse :: DeleteVPNConnectionRouteResponse -> TestTree
testDeleteVPNConnectionRouteResponse = resp
    "DeleteVPNConnectionRouteResponse"
    "fixture/DeleteVPNConnectionRouteResponse"
    (Proxy :: Proxy DeleteVPNConnectionRoute)

testModifyReservedInstancesResponse :: ModifyReservedInstancesResponse -> TestTree
testModifyReservedInstancesResponse = resp
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse"
    (Proxy :: Proxy ModifyReservedInstances)

testRegisterImageResponse :: RegisterImageResponse -> TestTree
testRegisterImageResponse = resp
    "RegisterImageResponse"
    "fixture/RegisterImageResponse"
    (Proxy :: Proxy RegisterImage)

testModifyVPCEndpointResponse :: ModifyVPCEndpointResponse -> TestTree
testModifyVPCEndpointResponse = resp
    "ModifyVPCEndpointResponse"
    "fixture/ModifyVPCEndpointResponse"
    (Proxy :: Proxy ModifyVPCEndpoint)

testDeleteVPNGatewayResponse :: DeleteVPNGatewayResponse -> TestTree
testDeleteVPNGatewayResponse = resp
    "DeleteVPNGatewayResponse"
    "fixture/DeleteVPNGatewayResponse"
    (Proxy :: Proxy DeleteVPNGateway)

testCreateVPCResponse :: CreateVPCResponse -> TestTree
testCreateVPCResponse = resp
    "CreateVPCResponse"
    "fixture/CreateVPCResponse"
    (Proxy :: Proxy CreateVPC)

testDescribeMovingAddressesResponse :: DescribeMovingAddressesResponse -> TestTree
testDescribeMovingAddressesResponse = resp
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse"
    (Proxy :: Proxy DescribeMovingAddresses)

testDescribeVolumeAttributeResponse :: DescribeVolumeAttributeResponse -> TestTree
testDescribeVolumeAttributeResponse = resp
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse"
    (Proxy :: Proxy DescribeVolumeAttribute)

testMoveAddressToVPCResponse :: MoveAddressToVPCResponse -> TestTree
testMoveAddressToVPCResponse = resp
    "MoveAddressToVPCResponse"
    "fixture/MoveAddressToVPCResponse"
    (Proxy :: Proxy MoveAddressToVPC)

testGetPasswordDataResponse :: GetPasswordDataResponse -> TestTree
testGetPasswordDataResponse = resp
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse"
    (Proxy :: Proxy GetPasswordData)

testCreateFlowLogsResponse :: CreateFlowLogsResponse -> TestTree
testCreateFlowLogsResponse = resp
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse"
    (Proxy :: Proxy CreateFlowLogs)

testDescribeImportImageTasksResponse :: DescribeImportImageTasksResponse -> TestTree
testDescribeImportImageTasksResponse = resp
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse"
    (Proxy :: Proxy DescribeImportImageTasks)

testDeleteNetworkACLResponse :: DeleteNetworkACLResponse -> TestTree
testDeleteNetworkACLResponse = resp
    "DeleteNetworkACLResponse"
    "fixture/DeleteNetworkACLResponse"
    (Proxy :: Proxy DeleteNetworkACL)

testDescribeSpotFleetRequestsResponse :: DescribeSpotFleetRequestsResponse -> TestTree
testDescribeSpotFleetRequestsResponse = resp
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse"
    (Proxy :: Proxy DescribeSpotFleetRequests)

testCopySnapshotResponse :: CopySnapshotResponse -> TestTree
testCopySnapshotResponse = resp
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse"
    (Proxy :: Proxy CopySnapshot)

testModifyVolumeAttributeResponse :: ModifyVolumeAttributeResponse -> TestTree
testModifyVolumeAttributeResponse = resp
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse"
    (Proxy :: Proxy ModifyVolumeAttribute)

testDescribeVPCAttributeResponse :: DescribeVPCAttributeResponse -> TestTree
testDescribeVPCAttributeResponse = resp
    "DescribeVPCAttributeResponse"
    "fixture/DescribeVPCAttributeResponse"
    (Proxy :: Proxy DescribeVPCAttribute)

testCreateVolumeResponse :: Volume -> TestTree
testCreateVolumeResponse = resp
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse"
    (Proxy :: Proxy CreateVolume)

testDisassociateAddressResponse :: DisassociateAddressResponse -> TestTree
testDisassociateAddressResponse = resp
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse"
    (Proxy :: Proxy DisassociateAddress)

testDeleteVPCResponse :: DeleteVPCResponse -> TestTree
testDeleteVPCResponse = resp
    "DeleteVPCResponse"
    "fixture/DeleteVPCResponse"
    (Proxy :: Proxy DeleteVPC)

testDescribePrefixListsResponse :: DescribePrefixListsResponse -> TestTree
testDescribePrefixListsResponse = resp
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse"
    (Proxy :: Proxy DescribePrefixLists)

testCreateInstanceExportTaskResponse :: CreateInstanceExportTaskResponse -> TestTree
testCreateInstanceExportTaskResponse = resp
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse"
    (Proxy :: Proxy CreateInstanceExportTask)

testDescribeSpotDatafeedSubscriptionResponse :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
testDescribeSpotDatafeedSubscriptionResponse = resp
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

testDetachVPNGatewayResponse :: DetachVPNGatewayResponse -> TestTree
testDetachVPNGatewayResponse = resp
    "DetachVPNGatewayResponse"
    "fixture/DetachVPNGatewayResponse"
    (Proxy :: Proxy DetachVPNGateway)

testDescribeExportTasksResponse :: DescribeExportTasksResponse -> TestTree
testDescribeExportTasksResponse = resp
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse"
    (Proxy :: Proxy DescribeExportTasks)

testDeletePlacementGroupResponse :: DeletePlacementGroupResponse -> TestTree
testDeletePlacementGroupResponse = resp
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse"
    (Proxy :: Proxy DeletePlacementGroup)

testCreateSubnetResponse :: CreateSubnetResponse -> TestTree
testCreateSubnetResponse = resp
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse"
    (Proxy :: Proxy CreateSubnet)

testEnableVolumeIOResponse :: EnableVolumeIOResponse -> TestTree
testEnableVolumeIOResponse = resp
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse"
    (Proxy :: Proxy EnableVolumeIO)

testCancelExportTaskResponse :: CancelExportTaskResponse -> TestTree
testCancelExportTaskResponse = resp
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse"
    (Proxy :: Proxy CancelExportTask)

testRequestSpotFleetResponse :: RequestSpotFleetResponse -> TestTree
testRequestSpotFleetResponse = resp
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse"
    (Proxy :: Proxy RequestSpotFleet)

testDescribeInstancesResponse :: DescribeInstancesResponse -> TestTree
testDescribeInstancesResponse = resp
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse"
    (Proxy :: Proxy DescribeInstances)

testDescribeSecurityGroupsResponse :: DescribeSecurityGroupsResponse -> TestTree
testDescribeSecurityGroupsResponse = resp
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse"
    (Proxy :: Proxy DescribeSecurityGroups)

testDescribeVPCPeeringConnectionsResponse :: DescribeVPCPeeringConnectionsResponse -> TestTree
testDescribeVPCPeeringConnectionsResponse = resp
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse"
    (Proxy :: Proxy DescribeVPCPeeringConnections)

testCreateNetworkInterfaceResponse :: CreateNetworkInterfaceResponse -> TestTree
testCreateNetworkInterfaceResponse = resp
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse"
    (Proxy :: Proxy CreateNetworkInterface)

testAssociateAddressResponse :: AssociateAddressResponse -> TestTree
testAssociateAddressResponse = resp
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse"
    (Proxy :: Proxy AssociateAddress)

testStartInstancesResponse :: StartInstancesResponse -> TestTree
testStartInstancesResponse = resp
    "StartInstancesResponse"
    "fixture/StartInstancesResponse"
    (Proxy :: Proxy StartInstances)

testDescribeCustomerGatewaysResponse :: DescribeCustomerGatewaysResponse -> TestTree
testDescribeCustomerGatewaysResponse = resp
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse"
    (Proxy :: Proxy DescribeCustomerGateways)

testResetNetworkInterfaceAttributeResponse :: ResetNetworkInterfaceAttributeResponse -> TestTree
testResetNetworkInterfaceAttributeResponse = resp
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

testCreateVPNConnectionResponse :: CreateVPNConnectionResponse -> TestTree
testCreateVPNConnectionResponse = resp
    "CreateVPNConnectionResponse"
    "fixture/CreateVPNConnectionResponse"
    (Proxy :: Proxy CreateVPNConnection)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = resp
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

testCreatePlacementGroupResponse :: CreatePlacementGroupResponse -> TestTree
testCreatePlacementGroupResponse = resp
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse"
    (Proxy :: Proxy CreatePlacementGroup)

testReplaceRouteTableAssociationResponse :: ReplaceRouteTableAssociationResponse -> TestTree
testReplaceRouteTableAssociationResponse = resp
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse"
    (Proxy :: Proxy ReplaceRouteTableAssociation)

testDescribeNetworkInterfaceAttributeResponse :: DescribeNetworkInterfaceAttributeResponse -> TestTree
testDescribeNetworkInterfaceAttributeResponse = resp
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

testDescribeReservedInstancesListingsResponse :: DescribeReservedInstancesListingsResponse -> TestTree
testDescribeReservedInstancesListingsResponse = resp
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse"
    (Proxy :: Proxy DescribeReservedInstancesListings)

testDeleteNetworkInterfaceResponse :: DeleteNetworkInterfaceResponse -> TestTree
testDeleteNetworkInterfaceResponse = resp
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse"
    (Proxy :: Proxy DeleteNetworkInterface)

testDeleteInternetGatewayResponse :: DeleteInternetGatewayResponse -> TestTree
testDeleteInternetGatewayResponse = resp
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse"
    (Proxy :: Proxy DeleteInternetGateway)

testDeleteSubnetResponse :: DeleteSubnetResponse -> TestTree
testDeleteSubnetResponse = resp
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse"
    (Proxy :: Proxy DeleteSubnet)

testCreateVPCEndpointResponse :: CreateVPCEndpointResponse -> TestTree
testCreateVPCEndpointResponse = resp
    "CreateVPCEndpointResponse"
    "fixture/CreateVPCEndpointResponse"
    (Proxy :: Proxy CreateVPCEndpoint)

testDescribeImportSnapshotTasksResponse :: DescribeImportSnapshotTasksResponse -> TestTree
testDescribeImportSnapshotTasksResponse = resp
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse"
    (Proxy :: Proxy DescribeImportSnapshotTasks)

testCopyImageResponse :: CopyImageResponse -> TestTree
testCopyImageResponse = resp
    "CopyImageResponse"
    "fixture/CopyImageResponse"
    (Proxy :: Proxy CopyImage)

testDisassociateRouteTableResponse :: DisassociateRouteTableResponse -> TestTree
testDisassociateRouteTableResponse = resp
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse"
    (Proxy :: Proxy DisassociateRouteTable)

testUnmonitorInstancesResponse :: UnmonitorInstancesResponse -> TestTree
testUnmonitorInstancesResponse = resp
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse"
    (Proxy :: Proxy UnmonitorInstances)

testImportVolumeResponse :: ImportVolumeResponse -> TestTree
testImportVolumeResponse = resp
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse"
    (Proxy :: Proxy ImportVolume)

testDisableVGWRoutePropagationResponse :: DisableVGWRoutePropagationResponse -> TestTree
testDisableVGWRoutePropagationResponse = resp
    "DisableVGWRoutePropagationResponse"
    "fixture/DisableVGWRoutePropagationResponse"
    (Proxy :: Proxy DisableVGWRoutePropagation)

testCreateSpotDatafeedSubscriptionResponse :: CreateSpotDatafeedSubscriptionResponse -> TestTree
testCreateSpotDatafeedSubscriptionResponse = resp
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

testAssignPrivateIPAddressesResponse :: AssignPrivateIPAddressesResponse -> TestTree
testAssignPrivateIPAddressesResponse = resp
    "AssignPrivateIPAddressesResponse"
    "fixture/AssignPrivateIPAddressesResponse"
    (Proxy :: Proxy AssignPrivateIPAddresses)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = resp
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

testDeleteCustomerGatewayResponse :: DeleteCustomerGatewayResponse -> TestTree
testDeleteCustomerGatewayResponse = resp
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse"
    (Proxy :: Proxy DeleteCustomerGateway)

testModifyInstanceAttributeResponse :: ModifyInstanceAttributeResponse -> TestTree
testModifyInstanceAttributeResponse = resp
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse"
    (Proxy :: Proxy ModifyInstanceAttribute)

testCreateSecurityGroupResponse :: CreateSecurityGroupResponse -> TestTree
testCreateSecurityGroupResponse = resp
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse"
    (Proxy :: Proxy CreateSecurityGroup)

testCancelSpotInstanceRequestsResponse :: CancelSpotInstanceRequestsResponse -> TestTree
testCancelSpotInstanceRequestsResponse = resp
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse"
    (Proxy :: Proxy CancelSpotInstanceRequests)

testCreateRouteResponse :: CreateRouteResponse -> TestTree
testCreateRouteResponse = resp
    "CreateRouteResponse"
    "fixture/CreateRouteResponse"
    (Proxy :: Proxy CreateRoute)

testCreateNetworkACLEntryResponse :: CreateNetworkACLEntryResponse -> TestTree
testCreateNetworkACLEntryResponse = resp
    "CreateNetworkACLEntryResponse"
    "fixture/CreateNetworkACLEntryResponse"
    (Proxy :: Proxy CreateNetworkACLEntry)

testModifySnapshotAttributeResponse :: ModifySnapshotAttributeResponse -> TestTree
testModifySnapshotAttributeResponse = resp
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse"
    (Proxy :: Proxy ModifySnapshotAttribute)

testEnableVGWRoutePropagationResponse :: EnableVGWRoutePropagationResponse -> TestTree
testEnableVGWRoutePropagationResponse = resp
    "EnableVGWRoutePropagationResponse"
    "fixture/EnableVGWRoutePropagationResponse"
    (Proxy :: Proxy EnableVGWRoutePropagation)

testCreateSnapshotResponse :: Snapshot -> TestTree
testCreateSnapshotResponse = resp
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

testDescribeSpotFleetRequestHistoryResponse :: DescribeSpotFleetRequestHistoryResponse -> TestTree
testDescribeSpotFleetRequestHistoryResponse = resp
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse"
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

testDeleteSpotDatafeedSubscriptionResponse :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
testDeleteSpotDatafeedSubscriptionResponse = resp
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

testDescribePlacementGroupsResponse :: DescribePlacementGroupsResponse -> TestTree
testDescribePlacementGroupsResponse = resp
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse"
    (Proxy :: Proxy DescribePlacementGroups)

testCreateReservedInstancesListingResponse :: CreateReservedInstancesListingResponse -> TestTree
testCreateReservedInstancesListingResponse = resp
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse"
    (Proxy :: Proxy CreateReservedInstancesListing)

testEnableVPCClassicLinkResponse :: EnableVPCClassicLinkResponse -> TestTree
testEnableVPCClassicLinkResponse = resp
    "EnableVPCClassicLinkResponse"
    "fixture/EnableVPCClassicLinkResponse"
    (Proxy :: Proxy EnableVPCClassicLink)

testDescribeKeyPairsResponse :: DescribeKeyPairsResponse -> TestTree
testDescribeKeyPairsResponse = resp
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse"
    (Proxy :: Proxy DescribeKeyPairs)

testRebootInstancesResponse :: RebootInstancesResponse -> TestTree
testRebootInstancesResponse = resp
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse"
    (Proxy :: Proxy RebootInstances)

testAttachVPNGatewayResponse :: AttachVPNGatewayResponse -> TestTree
testAttachVPNGatewayResponse = resp
    "AttachVPNGatewayResponse"
    "fixture/AttachVPNGatewayResponse"
    (Proxy :: Proxy AttachVPNGateway)

testCreateVPNConnectionRouteResponse :: CreateVPNConnectionRouteResponse -> TestTree
testCreateVPNConnectionRouteResponse = resp
    "CreateVPNConnectionRouteResponse"
    "fixture/CreateVPNConnectionRouteResponse"
    (Proxy :: Proxy CreateVPNConnectionRoute)

testDescribeClassicLinkInstancesResponse :: DescribeClassicLinkInstancesResponse -> TestTree
testDescribeClassicLinkInstancesResponse = resp
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse"
    (Proxy :: Proxy DescribeClassicLinkInstances)

testTerminateInstancesResponse :: TerminateInstancesResponse -> TestTree
testTerminateInstancesResponse = resp
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse"
    (Proxy :: Proxy TerminateInstances)

testCreateDHCPOptionsResponse :: CreateDHCPOptionsResponse -> TestTree
testCreateDHCPOptionsResponse = resp
    "CreateDHCPOptionsResponse"
    "fixture/CreateDHCPOptionsResponse"
    (Proxy :: Proxy CreateDHCPOptions)

testAssociateRouteTableResponse :: AssociateRouteTableResponse -> TestTree
testAssociateRouteTableResponse = resp
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse"
    (Proxy :: Proxy AssociateRouteTable)

testCreateImageResponse :: CreateImageResponse -> TestTree
testCreateImageResponse = resp
    "CreateImageResponse"
    "fixture/CreateImageResponse"
    (Proxy :: Proxy CreateImage)

testDescribeAccountAttributesResponse :: DescribeAccountAttributesResponse -> TestTree
testDescribeAccountAttributesResponse = resp
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse"
    (Proxy :: Proxy DescribeAccountAttributes)

testResetImageAttributeResponse :: ResetImageAttributeResponse -> TestTree
testResetImageAttributeResponse = resp
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse"
    (Proxy :: Proxy ResetImageAttribute)

testDescribeNetworkACLsResponse :: DescribeNetworkACLsResponse -> TestTree
testDescribeNetworkACLsResponse = resp
    "DescribeNetworkACLsResponse"
    "fixture/DescribeNetworkACLsResponse"
    (Proxy :: Proxy DescribeNetworkACLs)

testCancelImportTaskResponse :: CancelImportTaskResponse -> TestTree
testCancelImportTaskResponse = resp
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse"
    (Proxy :: Proxy CancelImportTask)

testGetConsoleOutputResponse :: GetConsoleOutputResponse -> TestTree
testGetConsoleOutputResponse = resp
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse"
    (Proxy :: Proxy GetConsoleOutput)

testUnassignPrivateIPAddressesResponse :: UnassignPrivateIPAddressesResponse -> TestTree
testUnassignPrivateIPAddressesResponse = resp
    "UnassignPrivateIPAddressesResponse"
    "fixture/UnassignPrivateIPAddressesResponse"
    (Proxy :: Proxy UnassignPrivateIPAddresses)

testDeleteRouteTableResponse :: DeleteRouteTableResponse -> TestTree
testDeleteRouteTableResponse = resp
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse"
    (Proxy :: Proxy DeleteRouteTable)

testDescribeImageAttributeResponse :: DescribeImageAttributeResponse -> TestTree
testDescribeImageAttributeResponse = resp
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse"
    (Proxy :: Proxy DescribeImageAttribute)

testDeleteDHCPOptionsResponse :: DeleteDHCPOptionsResponse -> TestTree
testDeleteDHCPOptionsResponse = resp
    "DeleteDHCPOptionsResponse"
    "fixture/DeleteDHCPOptionsResponse"
    (Proxy :: Proxy DeleteDHCPOptions)

testDescribeVPNGatewaysResponse :: DescribeVPNGatewaysResponse -> TestTree
testDescribeVPNGatewaysResponse = resp
    "DescribeVPNGatewaysResponse"
    "fixture/DescribeVPNGatewaysResponse"
    (Proxy :: Proxy DescribeVPNGateways)

testDetachClassicLinkVPCResponse :: DetachClassicLinkVPCResponse -> TestTree
testDetachClassicLinkVPCResponse = resp
    "DetachClassicLinkVPCResponse"
    "fixture/DetachClassicLinkVPCResponse"
    (Proxy :: Proxy DetachClassicLinkVPC)

testDescribeReservedInstancesModificationsResponse :: DescribeReservedInstancesModificationsResponse -> TestTree
testDescribeReservedInstancesModificationsResponse = resp
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse"
    (Proxy :: Proxy DescribeReservedInstancesModifications)

testDescribeSpotInstanceRequestsResponse :: DescribeSpotInstanceRequestsResponse -> TestTree
testDescribeSpotInstanceRequestsResponse = resp
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse"
    (Proxy :: Proxy DescribeSpotInstanceRequests)

testMonitorInstancesResponse :: MonitorInstancesResponse -> TestTree
testMonitorInstancesResponse = resp
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse"
    (Proxy :: Proxy MonitorInstances)

testDescribeRegionsResponse :: DescribeRegionsResponse -> TestTree
testDescribeRegionsResponse = resp
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse"
    (Proxy :: Proxy DescribeRegions)

testModifyVPCAttributeResponse :: ModifyVPCAttributeResponse -> TestTree
testModifyVPCAttributeResponse = resp
    "ModifyVPCAttributeResponse"
    "fixture/ModifyVPCAttributeResponse"
    (Proxy :: Proxy ModifyVPCAttribute)

testDescribeSpotFleetInstancesResponse :: DescribeSpotFleetInstancesResponse -> TestTree
testDescribeSpotFleetInstancesResponse = resp
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse"
    (Proxy :: Proxy DescribeSpotFleetInstances)

testDescribeVolumeStatusResponse :: DescribeVolumeStatusResponse -> TestTree
testDescribeVolumeStatusResponse = resp
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse"
    (Proxy :: Proxy DescribeVolumeStatus)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = resp
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse"
    (Proxy :: Proxy DeleteVolume)

testDescribeImagesResponse :: DescribeImagesResponse -> TestTree
testDescribeImagesResponse = resp
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse"
    (Proxy :: Proxy DescribeImages)

testCreateKeyPairResponse :: CreateKeyPairResponse -> TestTree
testCreateKeyPairResponse = resp
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse"
    (Proxy :: Proxy CreateKeyPair)

testRestoreAddressToClassicResponse :: RestoreAddressToClassicResponse -> TestTree
testRestoreAddressToClassicResponse = resp
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse"
    (Proxy :: Proxy RestoreAddressToClassic)

testDescribeAvailabilityZonesResponse :: DescribeAvailabilityZonesResponse -> TestTree
testDescribeAvailabilityZonesResponse = resp
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse"
    (Proxy :: Proxy DescribeAvailabilityZones)

testImportSnapshotResponse :: ImportSnapshotResponse -> TestTree
testImportSnapshotResponse = resp
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse"
    (Proxy :: Proxy ImportSnapshot)

testAcceptVPCPeeringConnectionResponse :: AcceptVPCPeeringConnectionResponse -> TestTree
testAcceptVPCPeeringConnectionResponse = resp
    "AcceptVPCPeeringConnectionResponse"
    "fixture/AcceptVPCPeeringConnectionResponse"
    (Proxy :: Proxy AcceptVPCPeeringConnection)

testDescribeRouteTablesResponse :: DescribeRouteTablesResponse -> TestTree
testDescribeRouteTablesResponse = resp
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse"
    (Proxy :: Proxy DescribeRouteTables)

instance Out AcceptVPCPeeringConnection
instance Out AcceptVPCPeeringConnectionResponse
instance Out AccountAttribute
instance Out AccountAttributeName
instance Out AccountAttributeValue
instance Out ActiveInstance
instance Out Address
instance Out AddressStatus
instance Out AllocateAddress
instance Out AllocateAddressResponse
instance Out ArchitectureValues
instance Out AssignPrivateIPAddresses
instance Out AssignPrivateIPAddressesResponse
instance Out AssociateAddress
instance Out AssociateAddressResponse
instance Out AssociateDHCPOptions
instance Out AssociateDHCPOptionsResponse
instance Out AssociateRouteTable
instance Out AssociateRouteTableResponse
instance Out AttachClassicLinkVPC
instance Out AttachClassicLinkVPCResponse
instance Out AttachInternetGateway
instance Out AttachInternetGatewayResponse
instance Out AttachNetworkInterface
instance Out AttachNetworkInterfaceResponse
instance Out AttachVPNGateway
instance Out AttachVPNGatewayResponse
instance Out AttachVolume
instance Out AttachmentStatus
instance Out AttributeBooleanValue
instance Out AttributeValue
instance Out AuthorizeSecurityGroupEgress
instance Out AuthorizeSecurityGroupEgressResponse
instance Out AuthorizeSecurityGroupIngress
instance Out AuthorizeSecurityGroupIngressResponse
instance Out AvailabilityZone
instance Out AvailabilityZoneMessage
instance Out AvailabilityZoneState
instance Out BatchState
instance Out BlobAttributeValue
instance Out BlockDeviceMapping
instance Out BundleInstance
instance Out BundleInstanceResponse
instance Out BundleTask
instance Out BundleTaskError
instance Out BundleTaskState
instance Out CancelBatchErrorCode
instance Out CancelBundleTask
instance Out CancelBundleTaskResponse
instance Out CancelConversionTask
instance Out CancelConversionTaskResponse
instance Out CancelExportTask
instance Out CancelExportTaskResponse
instance Out CancelImportTask
instance Out CancelImportTaskResponse
instance Out CancelReservedInstancesListing
instance Out CancelReservedInstancesListingResponse
instance Out CancelSpotFleetRequests
instance Out CancelSpotFleetRequestsError
instance Out CancelSpotFleetRequestsErrorItem
instance Out CancelSpotFleetRequestsResponse
instance Out CancelSpotFleetRequestsSuccessItem
instance Out CancelSpotInstanceRequestState
instance Out CancelSpotInstanceRequests
instance Out CancelSpotInstanceRequestsResponse
instance Out CancelledSpotInstanceRequest
instance Out ClassicLinkInstance
instance Out ClientData
instance Out ConfirmProductInstance
instance Out ConfirmProductInstanceResponse
instance Out ContainerFormat
instance Out ConversionTask
instance Out ConversionTaskState
instance Out CopyImage
instance Out CopyImageResponse
instance Out CopySnapshot
instance Out CopySnapshotResponse
instance Out CreateCustomerGateway
instance Out CreateCustomerGatewayResponse
instance Out CreateDHCPOptions
instance Out CreateDHCPOptionsResponse
instance Out CreateFlowLogs
instance Out CreateFlowLogsResponse
instance Out CreateImage
instance Out CreateImageResponse
instance Out CreateInstanceExportTask
instance Out CreateInstanceExportTaskResponse
instance Out CreateInternetGateway
instance Out CreateInternetGatewayResponse
instance Out CreateKeyPair
instance Out CreateKeyPairResponse
instance Out CreateNetworkACL
instance Out CreateNetworkACLEntry
instance Out CreateNetworkACLEntryResponse
instance Out CreateNetworkACLResponse
instance Out CreateNetworkInterface
instance Out CreateNetworkInterfaceResponse
instance Out CreatePlacementGroup
instance Out CreatePlacementGroupResponse
instance Out CreateReservedInstancesListing
instance Out CreateReservedInstancesListingResponse
instance Out CreateRoute
instance Out CreateRouteResponse
instance Out CreateRouteTable
instance Out CreateRouteTableResponse
instance Out CreateSecurityGroup
instance Out CreateSecurityGroupResponse
instance Out CreateSnapshot
instance Out CreateSpotDatafeedSubscription
instance Out CreateSpotDatafeedSubscriptionResponse
instance Out CreateSubnet
instance Out CreateSubnetResponse
instance Out CreateTags
instance Out CreateTagsResponse
instance Out CreateVPC
instance Out CreateVPCEndpoint
instance Out CreateVPCEndpointResponse
instance Out CreateVPCPeeringConnection
instance Out CreateVPCPeeringConnectionResponse
instance Out CreateVPCResponse
instance Out CreateVPNConnection
instance Out CreateVPNConnectionResponse
instance Out CreateVPNConnectionRoute
instance Out CreateVPNConnectionRouteResponse
instance Out CreateVPNGateway
instance Out CreateVPNGatewayResponse
instance Out CreateVolume
instance Out CreateVolumePermission
instance Out CreateVolumePermissionModifications
instance Out CurrencyCodeValues
instance Out CustomerGateway
instance Out DHCPConfiguration
instance Out DHCPOptions
instance Out DatafeedSubscriptionState
instance Out DeleteCustomerGateway
instance Out DeleteCustomerGatewayResponse
instance Out DeleteDHCPOptions
instance Out DeleteDHCPOptionsResponse
instance Out DeleteFlowLogs
instance Out DeleteFlowLogsResponse
instance Out DeleteInternetGateway
instance Out DeleteInternetGatewayResponse
instance Out DeleteKeyPair
instance Out DeleteKeyPairResponse
instance Out DeleteNetworkACL
instance Out DeleteNetworkACLEntry
instance Out DeleteNetworkACLEntryResponse
instance Out DeleteNetworkACLResponse
instance Out DeleteNetworkInterface
instance Out DeleteNetworkInterfaceResponse
instance Out DeletePlacementGroup
instance Out DeletePlacementGroupResponse
instance Out DeleteRoute
instance Out DeleteRouteResponse
instance Out DeleteRouteTable
instance Out DeleteRouteTableResponse
instance Out DeleteSecurityGroup
instance Out DeleteSecurityGroupResponse
instance Out DeleteSnapshot
instance Out DeleteSnapshotResponse
instance Out DeleteSpotDatafeedSubscription
instance Out DeleteSpotDatafeedSubscriptionResponse
instance Out DeleteSubnet
instance Out DeleteSubnetResponse
instance Out DeleteTags
instance Out DeleteTagsResponse
instance Out DeleteVPC
instance Out DeleteVPCEndpoints
instance Out DeleteVPCEndpointsResponse
instance Out DeleteVPCPeeringConnection
instance Out DeleteVPCPeeringConnectionResponse
instance Out DeleteVPCResponse
instance Out DeleteVPNConnection
instance Out DeleteVPNConnectionResponse
instance Out DeleteVPNConnectionRoute
instance Out DeleteVPNConnectionRouteResponse
instance Out DeleteVPNGateway
instance Out DeleteVPNGatewayResponse
instance Out DeleteVolume
instance Out DeleteVolumeResponse
instance Out DeregisterImage
instance Out DeregisterImageResponse
instance Out DescribeAccountAttributes
instance Out DescribeAccountAttributesResponse
instance Out DescribeAddresses
instance Out DescribeAddressesResponse
instance Out DescribeAvailabilityZones
instance Out DescribeAvailabilityZonesResponse
instance Out DescribeBundleTasks
instance Out DescribeBundleTasksResponse
instance Out DescribeClassicLinkInstances
instance Out DescribeClassicLinkInstancesResponse
instance Out DescribeConversionTasks
instance Out DescribeConversionTasksResponse
instance Out DescribeCustomerGateways
instance Out DescribeCustomerGatewaysResponse
instance Out DescribeDHCPOptions
instance Out DescribeDHCPOptionsResponse
instance Out DescribeExportTasks
instance Out DescribeExportTasksResponse
instance Out DescribeFlowLogs
instance Out DescribeFlowLogsResponse
instance Out DescribeImageAttribute
instance Out DescribeImageAttributeResponse
instance Out DescribeImages
instance Out DescribeImagesResponse
instance Out DescribeImportImageTasks
instance Out DescribeImportImageTasksResponse
instance Out DescribeImportSnapshotTasks
instance Out DescribeImportSnapshotTasksResponse
instance Out DescribeInstanceAttribute
instance Out DescribeInstanceAttributeResponse
instance Out DescribeInstanceStatus
instance Out DescribeInstanceStatusResponse
instance Out DescribeInstances
instance Out DescribeInstancesResponse
instance Out DescribeInternetGateways
instance Out DescribeInternetGatewaysResponse
instance Out DescribeKeyPairs
instance Out DescribeKeyPairsResponse
instance Out DescribeMovingAddresses
instance Out DescribeMovingAddressesResponse
instance Out DescribeNetworkACLs
instance Out DescribeNetworkACLsResponse
instance Out DescribeNetworkInterfaceAttribute
instance Out DescribeNetworkInterfaceAttributeResponse
instance Out DescribeNetworkInterfaces
instance Out DescribeNetworkInterfacesResponse
instance Out DescribePlacementGroups
instance Out DescribePlacementGroupsResponse
instance Out DescribePrefixLists
instance Out DescribePrefixListsResponse
instance Out DescribeRegions
instance Out DescribeRegionsResponse
instance Out DescribeReservedInstances
instance Out DescribeReservedInstancesListings
instance Out DescribeReservedInstancesListingsResponse
instance Out DescribeReservedInstancesModifications
instance Out DescribeReservedInstancesModificationsResponse
instance Out DescribeReservedInstancesOfferings
instance Out DescribeReservedInstancesOfferingsResponse
instance Out DescribeReservedInstancesResponse
instance Out DescribeRouteTables
instance Out DescribeRouteTablesResponse
instance Out DescribeSecurityGroups
instance Out DescribeSecurityGroupsResponse
instance Out DescribeSnapshotAttribute
instance Out DescribeSnapshotAttributeResponse
instance Out DescribeSnapshots
instance Out DescribeSnapshotsResponse
instance Out DescribeSpotDatafeedSubscription
instance Out DescribeSpotDatafeedSubscriptionResponse
instance Out DescribeSpotFleetInstances
instance Out DescribeSpotFleetInstancesResponse
instance Out DescribeSpotFleetRequestHistory
instance Out DescribeSpotFleetRequestHistoryResponse
instance Out DescribeSpotFleetRequests
instance Out DescribeSpotFleetRequestsResponse
instance Out DescribeSpotInstanceRequests
instance Out DescribeSpotInstanceRequestsResponse
instance Out DescribeSpotPriceHistory
instance Out DescribeSpotPriceHistoryResponse
instance Out DescribeSubnets
instance Out DescribeSubnetsResponse
instance Out DescribeTags
instance Out DescribeTagsResponse
instance Out DescribeVPCAttribute
instance Out DescribeVPCAttributeResponse
instance Out DescribeVPCClassicLink
instance Out DescribeVPCClassicLinkResponse
instance Out DescribeVPCEndpointServices
instance Out DescribeVPCEndpointServicesResponse
instance Out DescribeVPCEndpoints
instance Out DescribeVPCEndpointsResponse
instance Out DescribeVPCPeeringConnections
instance Out DescribeVPCPeeringConnectionsResponse
instance Out DescribeVPCs
instance Out DescribeVPCsResponse
instance Out DescribeVPNConnections
instance Out DescribeVPNConnectionsResponse
instance Out DescribeVPNGateways
instance Out DescribeVPNGatewaysResponse
instance Out DescribeVolumeAttribute
instance Out DescribeVolumeAttributeResponse
instance Out DescribeVolumeStatus
instance Out DescribeVolumeStatusResponse
instance Out DescribeVolumes
instance Out DescribeVolumesResponse
instance Out DetachClassicLinkVPC
instance Out DetachClassicLinkVPCResponse
instance Out DetachInternetGateway
instance Out DetachInternetGatewayResponse
instance Out DetachNetworkInterface
instance Out DetachNetworkInterfaceResponse
instance Out DetachVPNGateway
instance Out DetachVPNGatewayResponse
instance Out DetachVolume
instance Out DeviceType
instance Out DisableVGWRoutePropagation
instance Out DisableVGWRoutePropagationResponse
instance Out DisableVPCClassicLink
instance Out DisableVPCClassicLinkResponse
instance Out DisassociateAddress
instance Out DisassociateAddressResponse
instance Out DisassociateRouteTable
instance Out DisassociateRouteTableResponse
instance Out DiskImage
instance Out DiskImageDescription
instance Out DiskImageDetail
instance Out DiskImageFormat
instance Out DiskImageVolumeDescription
instance Out DomainType
instance Out EBSBlockDevice
instance Out EBSInstanceBlockDevice
instance Out EBSInstanceBlockDeviceSpecification
instance Out EnableVGWRoutePropagation
instance Out EnableVGWRoutePropagationResponse
instance Out EnableVPCClassicLink
instance Out EnableVPCClassicLinkResponse
instance Out EnableVolumeIO
instance Out EnableVolumeIOResponse
instance Out EventCode
instance Out EventInformation
instance Out EventType
instance Out ExportEnvironment
instance Out ExportTask
instance Out ExportTaskState
instance Out ExportToS3Task
instance Out ExportToS3TaskSpecification
instance Out Filter
instance Out FlowLog
instance Out FlowLogsResourceType
instance Out GatewayType
instance Out GetConsoleOutput
instance Out GetConsoleOutputResponse
instance Out GetPasswordData
instance Out GetPasswordDataResponse
instance Out GroupIdentifier
instance Out HistoryRecord
instance Out HypervisorType
instance Out IAMInstanceProfile
instance Out IAMInstanceProfileSpecification
instance Out ICMPTypeCode
instance Out IPPermission
instance Out IPRange
instance Out Image
instance Out ImageAttributeName
instance Out ImageDiskContainer
instance Out ImageState
instance Out ImageTypeValues
instance Out ImportImage
instance Out ImportImageResponse
instance Out ImportImageTask
instance Out ImportInstance
instance Out ImportInstanceLaunchSpecification
instance Out ImportInstanceResponse
instance Out ImportInstanceTaskDetails
instance Out ImportInstanceVolumeDetailItem
instance Out ImportKeyPair
instance Out ImportKeyPairResponse
instance Out ImportSnapshot
instance Out ImportSnapshotResponse
instance Out ImportSnapshotTask
instance Out ImportVolume
instance Out ImportVolumeResponse
instance Out ImportVolumeTaskDetails
instance Out Instance
instance Out InstanceAttributeName
instance Out InstanceBlockDeviceMapping
instance Out InstanceBlockDeviceMappingSpecification
instance Out InstanceCount
instance Out InstanceExportDetails
instance Out InstanceLifecycleType
instance Out InstanceMonitoring
instance Out InstanceNetworkInterface
instance Out InstanceNetworkInterfaceAssociation
instance Out InstanceNetworkInterfaceAttachment
instance Out InstanceNetworkInterfaceSpecification
instance Out InstancePrivateIPAddress
instance Out InstanceState
instance Out InstanceStateChange
instance Out InstanceStateName
instance Out InstanceStatus
instance Out InstanceStatusDetails
instance Out InstanceStatusEvent
instance Out InstanceStatusSummary
instance Out InstanceType
instance Out InternetGateway
instance Out InternetGatewayAttachment
instance Out KeyPairInfo
instance Out LaunchPermission
instance Out LaunchPermissionModifications
instance Out LaunchSpecification
instance Out ListingState
instance Out ListingStatus
instance Out ModifyImageAttribute
instance Out ModifyImageAttributeResponse
instance Out ModifyInstanceAttribute
instance Out ModifyInstanceAttributeResponse
instance Out ModifyNetworkInterfaceAttribute
instance Out ModifyNetworkInterfaceAttributeResponse
instance Out ModifyReservedInstances
instance Out ModifyReservedInstancesResponse
instance Out ModifySnapshotAttribute
instance Out ModifySnapshotAttributeName
instance Out ModifySnapshotAttributeResponse
instance Out ModifySubnetAttribute
instance Out ModifySubnetAttributeResponse
instance Out ModifyVPCAttribute
instance Out ModifyVPCAttributeResponse
instance Out ModifyVPCEndpoint
instance Out ModifyVPCEndpointResponse
instance Out ModifyVolumeAttribute
instance Out ModifyVolumeAttributeResponse
instance Out MonitorInstances
instance Out MonitorInstancesResponse
instance Out Monitoring
instance Out MonitoringState
instance Out MoveAddressToVPC
instance Out MoveAddressToVPCResponse
instance Out MoveStatus
instance Out MovingAddressStatus
instance Out NetworkACL
instance Out NetworkACLAssociation
instance Out NetworkACLEntry
instance Out NetworkInterface
instance Out NetworkInterfaceAssociation
instance Out NetworkInterfaceAttachment
instance Out NetworkInterfaceAttachmentChanges
instance Out NetworkInterfaceAttribute
instance Out NetworkInterfacePrivateIPAddress
instance Out NetworkInterfaceStatus
instance Out NewDHCPConfiguration
instance Out OfferingTypeValues
instance Out PermissionGroup
instance Out Placement
instance Out PlacementGroup
instance Out PlacementGroupState
instance Out PlacementStrategy
instance Out PlatformValues
instance Out PortRange
instance Out PrefixList
instance Out PrefixListId
instance Out PriceSchedule
instance Out PriceScheduleSpecification
instance Out PricingDetail
instance Out PrivateIPAddressSpecification
instance Out ProductCode
instance Out ProductCodeValues
instance Out PropagatingVGW
instance Out PurchaseReservedInstancesOffering
instance Out PurchaseReservedInstancesOfferingResponse
instance Out RIProductDescription
instance Out RebootInstances
instance Out RebootInstancesResponse
instance Out RecurringCharge
instance Out RecurringChargeFrequency
instance Out RegionInfo
instance Out RegisterImage
instance Out RegisterImageResponse
instance Out RejectVPCPeeringConnection
instance Out RejectVPCPeeringConnectionResponse
instance Out ReleaseAddress
instance Out ReleaseAddressResponse
instance Out ReplaceNetworkACLAssociation
instance Out ReplaceNetworkACLAssociationResponse
instance Out ReplaceNetworkACLEntry
instance Out ReplaceNetworkACLEntryResponse
instance Out ReplaceRoute
instance Out ReplaceRouteResponse
instance Out ReplaceRouteTableAssociation
instance Out ReplaceRouteTableAssociationResponse
instance Out ReportInstanceReasonCodes
instance Out ReportInstanceStatus
instance Out ReportInstanceStatusResponse
instance Out ReportStatusType
instance Out RequestSpotFleet
instance Out RequestSpotFleetResponse
instance Out RequestSpotInstances
instance Out RequestSpotInstancesResponse
instance Out RequestSpotLaunchSpecification
instance Out Reservation
instance Out ReservedInstanceLimitPrice
instance Out ReservedInstanceState
instance Out ReservedInstances
instance Out ReservedInstancesConfiguration
instance Out ReservedInstancesId
instance Out ReservedInstancesListing
instance Out ReservedInstancesModification
instance Out ReservedInstancesModificationResult
instance Out ReservedInstancesOffering
instance Out ResetImageAttribute
instance Out ResetImageAttributeName
instance Out ResetImageAttributeResponse
instance Out ResetInstanceAttribute
instance Out ResetInstanceAttributeResponse
instance Out ResetNetworkInterfaceAttribute
instance Out ResetNetworkInterfaceAttributeResponse
instance Out ResetSnapshotAttribute
instance Out ResetSnapshotAttributeResponse
instance Out ResourceType
instance Out RestoreAddressToClassic
instance Out RestoreAddressToClassicResponse
instance Out RevokeSecurityGroupEgress
instance Out RevokeSecurityGroupEgressResponse
instance Out RevokeSecurityGroupIngress
instance Out RevokeSecurityGroupIngressResponse
instance Out Route
instance Out RouteOrigin
instance Out RouteState
instance Out RouteTable
instance Out RouteTableAssociation
instance Out RuleAction
instance Out RunInstances
instance Out RunInstancesMonitoringEnabled
instance Out S3Storage
instance Out SecurityGroup
instance Out ShutdownBehavior
instance Out Snapshot
instance Out SnapshotAttributeName
instance Out SnapshotDetail
instance Out SnapshotDiskContainer
instance Out SnapshotState
instance Out SnapshotTaskDetail
instance Out SpotDatafeedSubscription
instance Out SpotFleetRequestConfig
instance Out SpotFleetRequestConfigData
instance Out SpotInstanceRequest
instance Out SpotInstanceState
instance Out SpotInstanceStateFault
instance Out SpotInstanceStatus
instance Out SpotInstanceType
instance Out SpotPlacement
instance Out SpotPrice
instance Out StartInstances
instance Out StartInstancesResponse
instance Out State
instance Out StateReason
instance Out StatusName
instance Out StatusType
instance Out StopInstances
instance Out StopInstancesResponse
instance Out Storage
instance Out Subnet
instance Out SubnetState
instance Out SummaryStatus
instance Out Tag
instance Out TagDescription
instance Out TelemetryStatus
instance Out Tenancy
instance Out TerminateInstances
instance Out TerminateInstancesResponse
instance Out TrafficType
instance Out UnassignPrivateIPAddresses
instance Out UnassignPrivateIPAddressesResponse
instance Out UnmonitorInstances
instance Out UnmonitorInstancesResponse
instance Out UnsuccessfulItem
instance Out UnsuccessfulItemError
instance Out UserBucket
instance Out UserBucketDetails
instance Out UserData
instance Out UserIdGroupPair
instance Out VGWTelemetry
instance Out VPC
instance Out VPCAttachment
instance Out VPCAttributeName
instance Out VPCClassicLink
instance Out VPCEndpoint
instance Out VPCPeeringConnection
instance Out VPCPeeringConnectionStateReason
instance Out VPCPeeringConnectionStateReasonCode
instance Out VPCPeeringConnectionVPCInfo
instance Out VPCState
instance Out VPNConnection
instance Out VPNConnectionOptions
instance Out VPNConnectionOptionsSpecification
instance Out VPNGateway
instance Out VPNState
instance Out VPNStaticRoute
instance Out VPNStaticRouteSource
instance Out VirtualizationType
instance Out Volume
instance Out VolumeAttachment
instance Out VolumeAttachmentState
instance Out VolumeAttributeName
instance Out VolumeDetail
instance Out VolumeState
instance Out VolumeStatusAction
instance Out VolumeStatusDetails
instance Out VolumeStatusEvent
instance Out VolumeStatusInfo
instance Out VolumeStatusInfoStatus
instance Out VolumeStatusItem
instance Out VolumeStatusName
instance Out VolumeType
