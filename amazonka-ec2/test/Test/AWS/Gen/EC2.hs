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
testDetachNetworkInterface = req
    "DetachNetworkInterface"
    "fixture/DetachNetworkInterface"

testDeleteVPCEndpoints :: DeleteVPCEndpoints -> TestTree
testDeleteVPCEndpoints = req
    "DeleteVPCEndpoints"
    "fixture/DeleteVPCEndpoints"

testDeleteKeyPair :: DeleteKeyPair -> TestTree
testDeleteKeyPair = req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair"

testDeleteFlowLogs :: DeleteFlowLogs -> TestTree
testDeleteFlowLogs = req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags"

testRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
testRevokeSecurityGroupEgress = req
    "RevokeSecurityGroupEgress"
    "fixture/RevokeSecurityGroupEgress"

testCreateVPNGateway :: CreateVPNGateway -> TestTree
testCreateVPNGateway = req
    "CreateVPNGateway"
    "fixture/CreateVPNGateway"

testDetachInternetGateway :: DetachInternetGateway -> TestTree
testDetachInternetGateway = req
    "DetachInternetGateway"
    "fixture/DetachInternetGateway"

testCreateNetworkACL :: CreateNetworkACL -> TestTree
testCreateNetworkACL = req
    "CreateNetworkACL"
    "fixture/CreateNetworkACL"

testImportInstance :: ImportInstance -> TestTree
testImportInstance = req
    "ImportInstance"
    "fixture/ImportInstance"

testDescribeVPCClassicLink :: DescribeVPCClassicLink -> TestTree
testDescribeVPCClassicLink = req
    "DescribeVPCClassicLink"
    "fixture/DescribeVPCClassicLink"

testDeleteVPNConnection :: DeleteVPNConnection -> TestTree
testDeleteVPNConnection = req
    "DeleteVPNConnection"
    "fixture/DeleteVPNConnection"

testAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
testAuthorizeSecurityGroupEgress = req
    "AuthorizeSecurityGroupEgress"
    "fixture/AuthorizeSecurityGroupEgress"

testDescribeBundleTasks :: DescribeBundleTasks -> TestTree
testDescribeBundleTasks = req
    "DescribeBundleTasks"
    "fixture/DescribeBundleTasks"

testCreateInternetGateway :: CreateInternetGateway -> TestTree
testCreateInternetGateway = req
    "CreateInternetGateway"
    "fixture/CreateInternetGateway"

testReleaseAddress :: ReleaseAddress -> TestTree
testReleaseAddress = req
    "ReleaseAddress"
    "fixture/ReleaseAddress"

testCancelBundleTask :: CancelBundleTask -> TestTree
testCancelBundleTask = req
    "CancelBundleTask"
    "fixture/CancelBundleTask"

testModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
testModifyNetworkInterfaceAttribute = req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute"

testModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
testModifySubnetAttribute = req
    "ModifySubnetAttribute"
    "fixture/ModifySubnetAttribute"

testDeregisterImage :: DeregisterImage -> TestTree
testDeregisterImage = req
    "DeregisterImage"
    "fixture/DeregisterImage"

testDetachVolume :: DetachVolume -> TestTree
testDetachVolume = req
    "DetachVolume"
    "fixture/DetachVolume"

testCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
testCancelReservedInstancesListing = req
    "CancelReservedInstancesListing"
    "fixture/CancelReservedInstancesListing"

testAttachClassicLinkVPC :: AttachClassicLinkVPC -> TestTree
testAttachClassicLinkVPC = req
    "AttachClassicLinkVPC"
    "fixture/AttachClassicLinkVPC"

testCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
testCancelSpotFleetRequests = req
    "CancelSpotFleetRequests"
    "fixture/CancelSpotFleetRequests"

testDescribeDHCPOptions :: DescribeDHCPOptions -> TestTree
testDescribeDHCPOptions = req
    "DescribeDHCPOptions"
    "fixture/DescribeDHCPOptions"

testDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
testDescribeSpotPriceHistory = req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory"

testStopInstances :: StopInstances -> TestTree
testStopInstances = req
    "StopInstances"
    "fixture/StopInstances"

testImportImage :: ImportImage -> TestTree
testImportImage = req
    "ImportImage"
    "fixture/ImportImage"

testDeleteNetworkACLEntry :: DeleteNetworkACLEntry -> TestTree
testDeleteNetworkACLEntry = req
    "DeleteNetworkACLEntry"
    "fixture/DeleteNetworkACLEntry"

testDisableVPCClassicLink :: DisableVPCClassicLink -> TestTree
testDisableVPCClassicLink = req
    "DisableVPCClassicLink"
    "fixture/DisableVPCClassicLink"

testAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
testAuthorizeSecurityGroupIngress = req
    "AuthorizeSecurityGroupIngress"
    "fixture/AuthorizeSecurityGroupIngress"

testBundleInstance :: BundleInstance -> TestTree
testBundleInstance = req
    "BundleInstance"
    "fixture/BundleInstance"

testDescribeVPCEndpointServices :: DescribeVPCEndpointServices -> TestTree
testDescribeVPCEndpointServices = req
    "DescribeVPCEndpointServices"
    "fixture/DescribeVPCEndpointServices"

testReplaceNetworkACLAssociation :: ReplaceNetworkACLAssociation -> TestTree
testReplaceNetworkACLAssociation = req
    "ReplaceNetworkACLAssociation"
    "fixture/ReplaceNetworkACLAssociation"

testCreateVPCPeeringConnection :: CreateVPCPeeringConnection -> TestTree
testCreateVPCPeeringConnection = req
    "CreateVPCPeeringConnection"
    "fixture/CreateVPCPeeringConnection"

testResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
testResetSnapshotAttribute = req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute"

testDescribeAddresses :: DescribeAddresses -> TestTree
testDescribeAddresses = req
    "DescribeAddresses"
    "fixture/DescribeAddresses"

testDescribeInternetGateways :: DescribeInternetGateways -> TestTree
testDescribeInternetGateways = req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways"

testReplaceRoute :: ReplaceRoute -> TestTree
testReplaceRoute = req
    "ReplaceRoute"
    "fixture/ReplaceRoute"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags"

testDescribeSubnets :: DescribeSubnets -> TestTree
testDescribeSubnets = req
    "DescribeSubnets"
    "fixture/DescribeSubnets"

testDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
testDescribeNetworkInterfaces = req
    "DescribeNetworkInterfaces"
    "fixture/DescribeNetworkInterfaces"

testPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
testPurchaseReservedInstancesOffering = req
    "PurchaseReservedInstancesOffering"
    "fixture/PurchaseReservedInstancesOffering"

testDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
testDescribeSnapshotAttribute = req
    "DescribeSnapshotAttribute"
    "fixture/DescribeSnapshotAttribute"

testCreateCustomerGateway :: CreateCustomerGateway -> TestTree
testCreateCustomerGateway = req
    "CreateCustomerGateway"
    "fixture/CreateCustomerGateway"

testAttachInternetGateway :: AttachInternetGateway -> TestTree
testAttachInternetGateway = req
    "AttachInternetGateway"
    "fixture/AttachInternetGateway"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags"

testReplaceNetworkACLEntry :: ReplaceNetworkACLEntry -> TestTree
testReplaceNetworkACLEntry = req
    "ReplaceNetworkACLEntry"
    "fixture/ReplaceNetworkACLEntry"

testResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
testResetInstanceAttribute = req
    "ResetInstanceAttribute"
    "fixture/ResetInstanceAttribute"

testDeleteRoute :: DeleteRoute -> TestTree
testDeleteRoute = req
    "DeleteRoute"
    "fixture/DeleteRoute"

testDescribeVPNConnections :: DescribeVPNConnections -> TestTree
testDescribeVPNConnections = req
    "DescribeVPNConnections"
    "fixture/DescribeVPNConnections"

testDescribeFlowLogs :: DescribeFlowLogs -> TestTree
testDescribeFlowLogs = req
    "DescribeFlowLogs"
    "fixture/DescribeFlowLogs"

testDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
testDeleteSecurityGroup = req
    "DeleteSecurityGroup"
    "fixture/DeleteSecurityGroup"

testDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
testDescribeReservedInstancesOfferings = req
    "DescribeReservedInstancesOfferings"
    "fixture/DescribeReservedInstancesOfferings"

testDeleteVPCPeeringConnection :: DeleteVPCPeeringConnection -> TestTree
testDeleteVPCPeeringConnection = req
    "DeleteVPCPeeringConnection"
    "fixture/DeleteVPCPeeringConnection"

testDescribeVPCEndpoints :: DescribeVPCEndpoints -> TestTree
testDescribeVPCEndpoints = req
    "DescribeVPCEndpoints"
    "fixture/DescribeVPCEndpoints"

testDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
testDescribeInstanceAttribute = req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute"

testConfirmProductInstance :: ConfirmProductInstance -> TestTree
testConfirmProductInstance = req
    "ConfirmProductInstance"
    "fixture/ConfirmProductInstance"

testImportKeyPair :: ImportKeyPair -> TestTree
testImportKeyPair = req
    "ImportKeyPair"
    "fixture/ImportKeyPair"

testAttachNetworkInterface :: AttachNetworkInterface -> TestTree
testAttachNetworkInterface = req
    "AttachNetworkInterface"
    "fixture/AttachNetworkInterface"

testDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
testDescribeInstanceStatus = req
    "DescribeInstanceStatus"
    "fixture/DescribeInstanceStatus"

testCancelConversionTask :: CancelConversionTask -> TestTree
testCancelConversionTask = req
    "CancelConversionTask"
    "fixture/CancelConversionTask"

testReportInstanceStatus :: ReportInstanceStatus -> TestTree
testReportInstanceStatus = req
    "ReportInstanceStatus"
    "fixture/ReportInstanceStatus"

testAssociateDHCPOptions :: AssociateDHCPOptions -> TestTree
testAssociateDHCPOptions = req
    "AssociateDHCPOptions"
    "fixture/AssociateDHCPOptions"

testDescribeVPCs :: DescribeVPCs -> TestTree
testDescribeVPCs = req
    "DescribeVPCs"
    "fixture/DescribeVPCs"

testRequestSpotInstances :: RequestSpotInstances -> TestTree
testRequestSpotInstances = req
    "RequestSpotInstances"
    "fixture/RequestSpotInstances"

testModifyImageAttribute :: ModifyImageAttribute -> TestTree
testModifyImageAttribute = req
    "ModifyImageAttribute"
    "fixture/ModifyImageAttribute"

testDescribeReservedInstances :: DescribeReservedInstances -> TestTree
testDescribeReservedInstances = req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances"

testAllocateAddress :: AllocateAddress -> TestTree
testAllocateAddress = req
    "AllocateAddress"
    "fixture/AllocateAddress"

testRunInstances :: RunInstances -> TestTree
testRunInstances = req
    "RunInstances"
    "fixture/RunInstances"

testCreateRouteTable :: CreateRouteTable -> TestTree
testCreateRouteTable = req
    "CreateRouteTable"
    "fixture/CreateRouteTable"

testAttachVolume :: AttachVolume -> TestTree
testAttachVolume = req
    "AttachVolume"
    "fixture/AttachVolume"

testDescribeConversionTasks :: DescribeConversionTasks -> TestTree
testDescribeConversionTasks = req
    "DescribeConversionTasks"
    "fixture/DescribeConversionTasks"

testRejectVPCPeeringConnection :: RejectVPCPeeringConnection -> TestTree
testRejectVPCPeeringConnection = req
    "RejectVPCPeeringConnection"
    "fixture/RejectVPCPeeringConnection"

testRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
testRevokeSecurityGroupIngress = req
    "RevokeSecurityGroupIngress"
    "fixture/RevokeSecurityGroupIngress"

testDescribeVolumes :: DescribeVolumes -> TestTree
testDescribeVolumes = req
    "DescribeVolumes"
    "fixture/DescribeVolumes"

testDeleteVPNConnectionRoute :: DeleteVPNConnectionRoute -> TestTree
testDeleteVPNConnectionRoute = req
    "DeleteVPNConnectionRoute"
    "fixture/DeleteVPNConnectionRoute"

testModifyReservedInstances :: ModifyReservedInstances -> TestTree
testModifyReservedInstances = req
    "ModifyReservedInstances"
    "fixture/ModifyReservedInstances"

testRegisterImage :: RegisterImage -> TestTree
testRegisterImage = req
    "RegisterImage"
    "fixture/RegisterImage"

testModifyVPCEndpoint :: ModifyVPCEndpoint -> TestTree
testModifyVPCEndpoint = req
    "ModifyVPCEndpoint"
    "fixture/ModifyVPCEndpoint"

testDeleteVPNGateway :: DeleteVPNGateway -> TestTree
testDeleteVPNGateway = req
    "DeleteVPNGateway"
    "fixture/DeleteVPNGateway"

testCreateVPC :: CreateVPC -> TestTree
testCreateVPC = req
    "CreateVPC"
    "fixture/CreateVPC"

testDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
testDescribeMovingAddresses = req
    "DescribeMovingAddresses"
    "fixture/DescribeMovingAddresses"

testDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
testDescribeVolumeAttribute = req
    "DescribeVolumeAttribute"
    "fixture/DescribeVolumeAttribute"

testMoveAddressToVPC :: MoveAddressToVPC -> TestTree
testMoveAddressToVPC = req
    "MoveAddressToVPC"
    "fixture/MoveAddressToVPC"

testGetPasswordData :: GetPasswordData -> TestTree
testGetPasswordData = req
    "GetPasswordData"
    "fixture/GetPasswordData"

testCreateFlowLogs :: CreateFlowLogs -> TestTree
testCreateFlowLogs = req
    "CreateFlowLogs"
    "fixture/CreateFlowLogs"

testDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
testDescribeImportImageTasks = req
    "DescribeImportImageTasks"
    "fixture/DescribeImportImageTasks"

testDeleteNetworkACL :: DeleteNetworkACL -> TestTree
testDeleteNetworkACL = req
    "DeleteNetworkACL"
    "fixture/DeleteNetworkACL"

testDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
testDescribeSpotFleetRequests = req
    "DescribeSpotFleetRequests"
    "fixture/DescribeSpotFleetRequests"

testCopySnapshot :: CopySnapshot -> TestTree
testCopySnapshot = req
    "CopySnapshot"
    "fixture/CopySnapshot"

testModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
testModifyVolumeAttribute = req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute"

testDescribeVPCAttribute :: DescribeVPCAttribute -> TestTree
testDescribeVPCAttribute = req
    "DescribeVPCAttribute"
    "fixture/DescribeVPCAttribute"

testCreateVolume :: CreateVolume -> TestTree
testCreateVolume = req
    "CreateVolume"
    "fixture/CreateVolume"

testDisassociateAddress :: DisassociateAddress -> TestTree
testDisassociateAddress = req
    "DisassociateAddress"
    "fixture/DisassociateAddress"

testDeleteVPC :: DeleteVPC -> TestTree
testDeleteVPC = req
    "DeleteVPC"
    "fixture/DeleteVPC"

testDescribePrefixLists :: DescribePrefixLists -> TestTree
testDescribePrefixLists = req
    "DescribePrefixLists"
    "fixture/DescribePrefixLists"

testCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
testCreateInstanceExportTask = req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask"

testDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
testDescribeSpotDatafeedSubscription = req
    "DescribeSpotDatafeedSubscription"
    "fixture/DescribeSpotDatafeedSubscription"

testDetachVPNGateway :: DetachVPNGateway -> TestTree
testDetachVPNGateway = req
    "DetachVPNGateway"
    "fixture/DetachVPNGateway"

testDescribeExportTasks :: DescribeExportTasks -> TestTree
testDescribeExportTasks = req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks"

testDeletePlacementGroup :: DeletePlacementGroup -> TestTree
testDeletePlacementGroup = req
    "DeletePlacementGroup"
    "fixture/DeletePlacementGroup"

testCreateSubnet :: CreateSubnet -> TestTree
testCreateSubnet = req
    "CreateSubnet"
    "fixture/CreateSubnet"

testEnableVolumeIO :: EnableVolumeIO -> TestTree
testEnableVolumeIO = req
    "EnableVolumeIO"
    "fixture/EnableVolumeIO"

testCancelExportTask :: CancelExportTask -> TestTree
testCancelExportTask = req
    "CancelExportTask"
    "fixture/CancelExportTask"

testRequestSpotFleet :: RequestSpotFleet -> TestTree
testRequestSpotFleet = req
    "RequestSpotFleet"
    "fixture/RequestSpotFleet"

testDescribeInstances :: DescribeInstances -> TestTree
testDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances"

testDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
testDescribeSecurityGroups = req
    "DescribeSecurityGroups"
    "fixture/DescribeSecurityGroups"

testDescribeVPCPeeringConnections :: DescribeVPCPeeringConnections -> TestTree
testDescribeVPCPeeringConnections = req
    "DescribeVPCPeeringConnections"
    "fixture/DescribeVPCPeeringConnections"

testCreateNetworkInterface :: CreateNetworkInterface -> TestTree
testCreateNetworkInterface = req
    "CreateNetworkInterface"
    "fixture/CreateNetworkInterface"

testAssociateAddress :: AssociateAddress -> TestTree
testAssociateAddress = req
    "AssociateAddress"
    "fixture/AssociateAddress"

testStartInstances :: StartInstances -> TestTree
testStartInstances = req
    "StartInstances"
    "fixture/StartInstances"

testDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
testDescribeCustomerGateways = req
    "DescribeCustomerGateways"
    "fixture/DescribeCustomerGateways"

testResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
testResetNetworkInterfaceAttribute = req
    "ResetNetworkInterfaceAttribute"
    "fixture/ResetNetworkInterfaceAttribute"

testCreateVPNConnection :: CreateVPNConnection -> TestTree
testCreateVPNConnection = req
    "CreateVPNConnection"
    "fixture/CreateVPNConnection"

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots"

testCreatePlacementGroup :: CreatePlacementGroup -> TestTree
testCreatePlacementGroup = req
    "CreatePlacementGroup"
    "fixture/CreatePlacementGroup"

testReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
testReplaceRouteTableAssociation = req
    "ReplaceRouteTableAssociation"
    "fixture/ReplaceRouteTableAssociation"

testDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
testDescribeNetworkInterfaceAttribute = req
    "DescribeNetworkInterfaceAttribute"
    "fixture/DescribeNetworkInterfaceAttribute"

testDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
testDescribeReservedInstancesListings = req
    "DescribeReservedInstancesListings"
    "fixture/DescribeReservedInstancesListings"

testDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
testDeleteNetworkInterface = req
    "DeleteNetworkInterface"
    "fixture/DeleteNetworkInterface"

testDeleteInternetGateway :: DeleteInternetGateway -> TestTree
testDeleteInternetGateway = req
    "DeleteInternetGateway"
    "fixture/DeleteInternetGateway"

testDeleteSubnet :: DeleteSubnet -> TestTree
testDeleteSubnet = req
    "DeleteSubnet"
    "fixture/DeleteSubnet"

testCreateVPCEndpoint :: CreateVPCEndpoint -> TestTree
testCreateVPCEndpoint = req
    "CreateVPCEndpoint"
    "fixture/CreateVPCEndpoint"

testDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
testDescribeImportSnapshotTasks = req
    "DescribeImportSnapshotTasks"
    "fixture/DescribeImportSnapshotTasks"

testCopyImage :: CopyImage -> TestTree
testCopyImage = req
    "CopyImage"
    "fixture/CopyImage"

testDisassociateRouteTable :: DisassociateRouteTable -> TestTree
testDisassociateRouteTable = req
    "DisassociateRouteTable"
    "fixture/DisassociateRouteTable"

testUnmonitorInstances :: UnmonitorInstances -> TestTree
testUnmonitorInstances = req
    "UnmonitorInstances"
    "fixture/UnmonitorInstances"

testImportVolume :: ImportVolume -> TestTree
testImportVolume = req
    "ImportVolume"
    "fixture/ImportVolume"

testDisableVGWRoutePropagation :: DisableVGWRoutePropagation -> TestTree
testDisableVGWRoutePropagation = req
    "DisableVGWRoutePropagation"
    "fixture/DisableVGWRoutePropagation"

testCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscription -> TestTree
testCreateSpotDatafeedSubscription = req
    "CreateSpotDatafeedSubscription"
    "fixture/CreateSpotDatafeedSubscription"

testAssignPrivateIPAddresses :: AssignPrivateIPAddresses -> TestTree
testAssignPrivateIPAddresses = req
    "AssignPrivateIPAddresses"
    "fixture/AssignPrivateIPAddresses"

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot"

testDeleteCustomerGateway :: DeleteCustomerGateway -> TestTree
testDeleteCustomerGateway = req
    "DeleteCustomerGateway"
    "fixture/DeleteCustomerGateway"

testModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
testModifyInstanceAttribute = req
    "ModifyInstanceAttribute"
    "fixture/ModifyInstanceAttribute"

testCreateSecurityGroup :: CreateSecurityGroup -> TestTree
testCreateSecurityGroup = req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup"

testCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
testCancelSpotInstanceRequests = req
    "CancelSpotInstanceRequests"
    "fixture/CancelSpotInstanceRequests"

testCreateRoute :: CreateRoute -> TestTree
testCreateRoute = req
    "CreateRoute"
    "fixture/CreateRoute"

testCreateNetworkACLEntry :: CreateNetworkACLEntry -> TestTree
testCreateNetworkACLEntry = req
    "CreateNetworkACLEntry"
    "fixture/CreateNetworkACLEntry"

testModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
testModifySnapshotAttribute = req
    "ModifySnapshotAttribute"
    "fixture/ModifySnapshotAttribute"

testEnableVGWRoutePropagation :: EnableVGWRoutePropagation -> TestTree
testEnableVGWRoutePropagation = req
    "EnableVGWRoutePropagation"
    "fixture/EnableVGWRoutePropagation"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot"

testDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
testDescribeSpotFleetRequestHistory = req
    "DescribeSpotFleetRequestHistory"
    "fixture/DescribeSpotFleetRequestHistory"

testDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
testDeleteSpotDatafeedSubscription = req
    "DeleteSpotDatafeedSubscription"
    "fixture/DeleteSpotDatafeedSubscription"

testDescribePlacementGroups :: DescribePlacementGroups -> TestTree
testDescribePlacementGroups = req
    "DescribePlacementGroups"
    "fixture/DescribePlacementGroups"

testCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
testCreateReservedInstancesListing = req
    "CreateReservedInstancesListing"
    "fixture/CreateReservedInstancesListing"

testEnableVPCClassicLink :: EnableVPCClassicLink -> TestTree
testEnableVPCClassicLink = req
    "EnableVPCClassicLink"
    "fixture/EnableVPCClassicLink"

testDescribeKeyPairs :: DescribeKeyPairs -> TestTree
testDescribeKeyPairs = req
    "DescribeKeyPairs"
    "fixture/DescribeKeyPairs"

testRebootInstances :: RebootInstances -> TestTree
testRebootInstances = req
    "RebootInstances"
    "fixture/RebootInstances"

testAttachVPNGateway :: AttachVPNGateway -> TestTree
testAttachVPNGateway = req
    "AttachVPNGateway"
    "fixture/AttachVPNGateway"

testCreateVPNConnectionRoute :: CreateVPNConnectionRoute -> TestTree
testCreateVPNConnectionRoute = req
    "CreateVPNConnectionRoute"
    "fixture/CreateVPNConnectionRoute"

testDescribeClassicLinkInstances :: DescribeClassicLinkInstances -> TestTree
testDescribeClassicLinkInstances = req
    "DescribeClassicLinkInstances"
    "fixture/DescribeClassicLinkInstances"

testTerminateInstances :: TerminateInstances -> TestTree
testTerminateInstances = req
    "TerminateInstances"
    "fixture/TerminateInstances"

testCreateDHCPOptions :: CreateDHCPOptions -> TestTree
testCreateDHCPOptions = req
    "CreateDHCPOptions"
    "fixture/CreateDHCPOptions"

testAssociateRouteTable :: AssociateRouteTable -> TestTree
testAssociateRouteTable = req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable"

testCreateImage :: CreateImage -> TestTree
testCreateImage = req
    "CreateImage"
    "fixture/CreateImage"

testDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
testDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes"

testResetImageAttribute :: ResetImageAttribute -> TestTree
testResetImageAttribute = req
    "ResetImageAttribute"
    "fixture/ResetImageAttribute"

testDescribeNetworkACLs :: DescribeNetworkACLs -> TestTree
testDescribeNetworkACLs = req
    "DescribeNetworkACLs"
    "fixture/DescribeNetworkACLs"

testCancelImportTask :: CancelImportTask -> TestTree
testCancelImportTask = req
    "CancelImportTask"
    "fixture/CancelImportTask"

testGetConsoleOutput :: GetConsoleOutput -> TestTree
testGetConsoleOutput = req
    "GetConsoleOutput"
    "fixture/GetConsoleOutput"

testUnassignPrivateIPAddresses :: UnassignPrivateIPAddresses -> TestTree
testUnassignPrivateIPAddresses = req
    "UnassignPrivateIPAddresses"
    "fixture/UnassignPrivateIPAddresses"

testDeleteRouteTable :: DeleteRouteTable -> TestTree
testDeleteRouteTable = req
    "DeleteRouteTable"
    "fixture/DeleteRouteTable"

testDescribeImageAttribute :: DescribeImageAttribute -> TestTree
testDescribeImageAttribute = req
    "DescribeImageAttribute"
    "fixture/DescribeImageAttribute"

testDeleteDHCPOptions :: DeleteDHCPOptions -> TestTree
testDeleteDHCPOptions = req
    "DeleteDHCPOptions"
    "fixture/DeleteDHCPOptions"

testDescribeVPNGateways :: DescribeVPNGateways -> TestTree
testDescribeVPNGateways = req
    "DescribeVPNGateways"
    "fixture/DescribeVPNGateways"

testDetachClassicLinkVPC :: DetachClassicLinkVPC -> TestTree
testDetachClassicLinkVPC = req
    "DetachClassicLinkVPC"
    "fixture/DetachClassicLinkVPC"

testDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
testDescribeReservedInstancesModifications = req
    "DescribeReservedInstancesModifications"
    "fixture/DescribeReservedInstancesModifications"

testDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
testDescribeSpotInstanceRequests = req
    "DescribeSpotInstanceRequests"
    "fixture/DescribeSpotInstanceRequests"

testMonitorInstances :: MonitorInstances -> TestTree
testMonitorInstances = req
    "MonitorInstances"
    "fixture/MonitorInstances"

testDescribeRegions :: DescribeRegions -> TestTree
testDescribeRegions = req
    "DescribeRegions"
    "fixture/DescribeRegions"

testModifyVPCAttribute :: ModifyVPCAttribute -> TestTree
testModifyVPCAttribute = req
    "ModifyVPCAttribute"
    "fixture/ModifyVPCAttribute"

testDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
testDescribeSpotFleetInstances = req
    "DescribeSpotFleetInstances"
    "fixture/DescribeSpotFleetInstances"

testDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
testDescribeVolumeStatus = req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus"

testDeleteVolume :: DeleteVolume -> TestTree
testDeleteVolume = req
    "DeleteVolume"
    "fixture/DeleteVolume"

testDescribeImages :: DescribeImages -> TestTree
testDescribeImages = req
    "DescribeImages"
    "fixture/DescribeImages"

testCreateKeyPair :: CreateKeyPair -> TestTree
testCreateKeyPair = req
    "CreateKeyPair"
    "fixture/CreateKeyPair"

testRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
testRestoreAddressToClassic = req
    "RestoreAddressToClassic"
    "fixture/RestoreAddressToClassic"

testDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
testDescribeAvailabilityZones = req
    "DescribeAvailabilityZones"
    "fixture/DescribeAvailabilityZones"

testImportSnapshot :: ImportSnapshot -> TestTree
testImportSnapshot = req
    "ImportSnapshot"
    "fixture/ImportSnapshot"

testAcceptVPCPeeringConnection :: AcceptVPCPeeringConnection -> TestTree
testAcceptVPCPeeringConnection = req
    "AcceptVPCPeeringConnection"
    "fixture/AcceptVPCPeeringConnection"

testDescribeRouteTables :: DescribeRouteTables -> TestTree
testDescribeRouteTables = req
    "DescribeRouteTables"
    "fixture/DescribeRouteTables"

-- Responses

testDetachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse -> TestTree
testDetachNetworkInterfaceResponse = res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse"
    (Proxy :: Proxy DetachNetworkInterface)

testDeleteVPCEndpointsResponse :: DeleteVPCEndpointsResponse -> TestTree
testDeleteVPCEndpointsResponse = res
    "DeleteVPCEndpointsResponse"
    "fixture/DeleteVPCEndpointsResponse"
    (Proxy :: Proxy DeleteVPCEndpoints)

testDeleteKeyPairResponse :: DeleteKeyPairResponse -> TestTree
testDeleteKeyPairResponse = res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse"
    (Proxy :: Proxy DeleteKeyPair)

testDeleteFlowLogsResponse :: DeleteFlowLogsResponse -> TestTree
testDeleteFlowLogsResponse = res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse"
    (Proxy :: Proxy DeleteFlowLogs)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

testRevokeSecurityGroupEgressResponse :: RevokeSecurityGroupEgressResponse -> TestTree
testRevokeSecurityGroupEgressResponse = res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse"
    (Proxy :: Proxy RevokeSecurityGroupEgress)

testCreateVPNGatewayResponse :: CreateVPNGatewayResponse -> TestTree
testCreateVPNGatewayResponse = res
    "CreateVPNGatewayResponse"
    "fixture/CreateVPNGatewayResponse"
    (Proxy :: Proxy CreateVPNGateway)

testDetachInternetGatewayResponse :: DetachInternetGatewayResponse -> TestTree
testDetachInternetGatewayResponse = res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse"
    (Proxy :: Proxy DetachInternetGateway)

testCreateNetworkACLResponse :: CreateNetworkACLResponse -> TestTree
testCreateNetworkACLResponse = res
    "CreateNetworkACLResponse"
    "fixture/CreateNetworkACLResponse"
    (Proxy :: Proxy CreateNetworkACL)

testImportInstanceResponse :: ImportInstanceResponse -> TestTree
testImportInstanceResponse = res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse"
    (Proxy :: Proxy ImportInstance)

testDescribeVPCClassicLinkResponse :: DescribeVPCClassicLinkResponse -> TestTree
testDescribeVPCClassicLinkResponse = res
    "DescribeVPCClassicLinkResponse"
    "fixture/DescribeVPCClassicLinkResponse"
    (Proxy :: Proxy DescribeVPCClassicLink)

testDeleteVPNConnectionResponse :: DeleteVPNConnectionResponse -> TestTree
testDeleteVPNConnectionResponse = res
    "DeleteVPNConnectionResponse"
    "fixture/DeleteVPNConnectionResponse"
    (Proxy :: Proxy DeleteVPNConnection)

testAuthorizeSecurityGroupEgressResponse :: AuthorizeSecurityGroupEgressResponse -> TestTree
testAuthorizeSecurityGroupEgressResponse = res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse"
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

testDescribeBundleTasksResponse :: DescribeBundleTasksResponse -> TestTree
testDescribeBundleTasksResponse = res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse"
    (Proxy :: Proxy DescribeBundleTasks)

testCreateInternetGatewayResponse :: CreateInternetGatewayResponse -> TestTree
testCreateInternetGatewayResponse = res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse"
    (Proxy :: Proxy CreateInternetGateway)

testReleaseAddressResponse :: ReleaseAddressResponse -> TestTree
testReleaseAddressResponse = res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse"
    (Proxy :: Proxy ReleaseAddress)

testCancelBundleTaskResponse :: CancelBundleTaskResponse -> TestTree
testCancelBundleTaskResponse = res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse"
    (Proxy :: Proxy CancelBundleTask)

testModifyNetworkInterfaceAttributeResponse :: ModifyNetworkInterfaceAttributeResponse -> TestTree
testModifyNetworkInterfaceAttributeResponse = res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

testModifySubnetAttributeResponse :: ModifySubnetAttributeResponse -> TestTree
testModifySubnetAttributeResponse = res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse"
    (Proxy :: Proxy ModifySubnetAttribute)

testDeregisterImageResponse :: DeregisterImageResponse -> TestTree
testDeregisterImageResponse = res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse"
    (Proxy :: Proxy DeregisterImage)

testDetachVolumeResponse :: VolumeAttachment -> TestTree
testDetachVolumeResponse = res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse"
    (Proxy :: Proxy DetachVolume)

testCancelReservedInstancesListingResponse :: CancelReservedInstancesListingResponse -> TestTree
testCancelReservedInstancesListingResponse = res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse"
    (Proxy :: Proxy CancelReservedInstancesListing)

testAttachClassicLinkVPCResponse :: AttachClassicLinkVPCResponse -> TestTree
testAttachClassicLinkVPCResponse = res
    "AttachClassicLinkVPCResponse"
    "fixture/AttachClassicLinkVPCResponse"
    (Proxy :: Proxy AttachClassicLinkVPC)

testCancelSpotFleetRequestsResponse :: CancelSpotFleetRequestsResponse -> TestTree
testCancelSpotFleetRequestsResponse = res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse"
    (Proxy :: Proxy CancelSpotFleetRequests)

testDescribeDHCPOptionsResponse :: DescribeDHCPOptionsResponse -> TestTree
testDescribeDHCPOptionsResponse = res
    "DescribeDHCPOptionsResponse"
    "fixture/DescribeDHCPOptionsResponse"
    (Proxy :: Proxy DescribeDHCPOptions)

testDescribeSpotPriceHistoryResponse :: DescribeSpotPriceHistoryResponse -> TestTree
testDescribeSpotPriceHistoryResponse = res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse"
    (Proxy :: Proxy DescribeSpotPriceHistory)

testStopInstancesResponse :: StopInstancesResponse -> TestTree
testStopInstancesResponse = res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse"
    (Proxy :: Proxy StopInstances)

testImportImageResponse :: ImportImageResponse -> TestTree
testImportImageResponse = res
    "ImportImageResponse"
    "fixture/ImportImageResponse"
    (Proxy :: Proxy ImportImage)

testDeleteNetworkACLEntryResponse :: DeleteNetworkACLEntryResponse -> TestTree
testDeleteNetworkACLEntryResponse = res
    "DeleteNetworkACLEntryResponse"
    "fixture/DeleteNetworkACLEntryResponse"
    (Proxy :: Proxy DeleteNetworkACLEntry)

testDisableVPCClassicLinkResponse :: DisableVPCClassicLinkResponse -> TestTree
testDisableVPCClassicLinkResponse = res
    "DisableVPCClassicLinkResponse"
    "fixture/DisableVPCClassicLinkResponse"
    (Proxy :: Proxy DisableVPCClassicLink)

testAuthorizeSecurityGroupIngressResponse :: AuthorizeSecurityGroupIngressResponse -> TestTree
testAuthorizeSecurityGroupIngressResponse = res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

testBundleInstanceResponse :: BundleInstanceResponse -> TestTree
testBundleInstanceResponse = res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse"
    (Proxy :: Proxy BundleInstance)

testDescribeVPCEndpointServicesResponse :: DescribeVPCEndpointServicesResponse -> TestTree
testDescribeVPCEndpointServicesResponse = res
    "DescribeVPCEndpointServicesResponse"
    "fixture/DescribeVPCEndpointServicesResponse"
    (Proxy :: Proxy DescribeVPCEndpointServices)

testReplaceNetworkACLAssociationResponse :: ReplaceNetworkACLAssociationResponse -> TestTree
testReplaceNetworkACLAssociationResponse = res
    "ReplaceNetworkACLAssociationResponse"
    "fixture/ReplaceNetworkACLAssociationResponse"
    (Proxy :: Proxy ReplaceNetworkACLAssociation)

testCreateVPCPeeringConnectionResponse :: CreateVPCPeeringConnectionResponse -> TestTree
testCreateVPCPeeringConnectionResponse = res
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse"
    (Proxy :: Proxy CreateVPCPeeringConnection)

testResetSnapshotAttributeResponse :: ResetSnapshotAttributeResponse -> TestTree
testResetSnapshotAttributeResponse = res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse"
    (Proxy :: Proxy ResetSnapshotAttribute)

testDescribeAddressesResponse :: DescribeAddressesResponse -> TestTree
testDescribeAddressesResponse = res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse"
    (Proxy :: Proxy DescribeAddresses)

testDescribeInternetGatewaysResponse :: DescribeInternetGatewaysResponse -> TestTree
testDescribeInternetGatewaysResponse = res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse"
    (Proxy :: Proxy DescribeInternetGateways)

testReplaceRouteResponse :: ReplaceRouteResponse -> TestTree
testReplaceRouteResponse = res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse"
    (Proxy :: Proxy ReplaceRoute)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

testDescribeSubnetsResponse :: DescribeSubnetsResponse -> TestTree
testDescribeSubnetsResponse = res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse"
    (Proxy :: Proxy DescribeSubnets)

testDescribeNetworkInterfacesResponse :: DescribeNetworkInterfacesResponse -> TestTree
testDescribeNetworkInterfacesResponse = res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse"
    (Proxy :: Proxy DescribeNetworkInterfaces)

testPurchaseReservedInstancesOfferingResponse :: PurchaseReservedInstancesOfferingResponse -> TestTree
testPurchaseReservedInstancesOfferingResponse = res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

testDescribeSnapshotAttributeResponse :: DescribeSnapshotAttributeResponse -> TestTree
testDescribeSnapshotAttributeResponse = res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse"
    (Proxy :: Proxy DescribeSnapshotAttribute)

testCreateCustomerGatewayResponse :: CreateCustomerGatewayResponse -> TestTree
testCreateCustomerGatewayResponse = res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse"
    (Proxy :: Proxy CreateCustomerGateway)

testAttachInternetGatewayResponse :: AttachInternetGatewayResponse -> TestTree
testAttachInternetGatewayResponse = res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse"
    (Proxy :: Proxy AttachInternetGateway)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

testReplaceNetworkACLEntryResponse :: ReplaceNetworkACLEntryResponse -> TestTree
testReplaceNetworkACLEntryResponse = res
    "ReplaceNetworkACLEntryResponse"
    "fixture/ReplaceNetworkACLEntryResponse"
    (Proxy :: Proxy ReplaceNetworkACLEntry)

testResetInstanceAttributeResponse :: ResetInstanceAttributeResponse -> TestTree
testResetInstanceAttributeResponse = res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse"
    (Proxy :: Proxy ResetInstanceAttribute)

testDeleteRouteResponse :: DeleteRouteResponse -> TestTree
testDeleteRouteResponse = res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse"
    (Proxy :: Proxy DeleteRoute)

testDescribeVPNConnectionsResponse :: DescribeVPNConnectionsResponse -> TestTree
testDescribeVPNConnectionsResponse = res
    "DescribeVPNConnectionsResponse"
    "fixture/DescribeVPNConnectionsResponse"
    (Proxy :: Proxy DescribeVPNConnections)

testDescribeFlowLogsResponse :: DescribeFlowLogsResponse -> TestTree
testDescribeFlowLogsResponse = res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse"
    (Proxy :: Proxy DescribeFlowLogs)

testDeleteSecurityGroupResponse :: DeleteSecurityGroupResponse -> TestTree
testDeleteSecurityGroupResponse = res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse"
    (Proxy :: Proxy DeleteSecurityGroup)

testDescribeReservedInstancesOfferingsResponse :: DescribeReservedInstancesOfferingsResponse -> TestTree
testDescribeReservedInstancesOfferingsResponse = res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

testDeleteVPCPeeringConnectionResponse :: DeleteVPCPeeringConnectionResponse -> TestTree
testDeleteVPCPeeringConnectionResponse = res
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse"
    (Proxy :: Proxy DeleteVPCPeeringConnection)

testDescribeVPCEndpointsResponse :: DescribeVPCEndpointsResponse -> TestTree
testDescribeVPCEndpointsResponse = res
    "DescribeVPCEndpointsResponse"
    "fixture/DescribeVPCEndpointsResponse"
    (Proxy :: Proxy DescribeVPCEndpoints)

testDescribeInstanceAttributeResponse :: DescribeInstanceAttributeResponse -> TestTree
testDescribeInstanceAttributeResponse = res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse"
    (Proxy :: Proxy DescribeInstanceAttribute)

testConfirmProductInstanceResponse :: ConfirmProductInstanceResponse -> TestTree
testConfirmProductInstanceResponse = res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse"
    (Proxy :: Proxy ConfirmProductInstance)

testImportKeyPairResponse :: ImportKeyPairResponse -> TestTree
testImportKeyPairResponse = res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse"
    (Proxy :: Proxy ImportKeyPair)

testAttachNetworkInterfaceResponse :: AttachNetworkInterfaceResponse -> TestTree
testAttachNetworkInterfaceResponse = res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse"
    (Proxy :: Proxy AttachNetworkInterface)

testDescribeInstanceStatusResponse :: DescribeInstanceStatusResponse -> TestTree
testDescribeInstanceStatusResponse = res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse"
    (Proxy :: Proxy DescribeInstanceStatus)

testCancelConversionTaskResponse :: CancelConversionTaskResponse -> TestTree
testCancelConversionTaskResponse = res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse"
    (Proxy :: Proxy CancelConversionTask)

testReportInstanceStatusResponse :: ReportInstanceStatusResponse -> TestTree
testReportInstanceStatusResponse = res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse"
    (Proxy :: Proxy ReportInstanceStatus)

testAssociateDHCPOptionsResponse :: AssociateDHCPOptionsResponse -> TestTree
testAssociateDHCPOptionsResponse = res
    "AssociateDHCPOptionsResponse"
    "fixture/AssociateDHCPOptionsResponse"
    (Proxy :: Proxy AssociateDHCPOptions)

testDescribeVPCsResponse :: DescribeVPCsResponse -> TestTree
testDescribeVPCsResponse = res
    "DescribeVPCsResponse"
    "fixture/DescribeVPCsResponse"
    (Proxy :: Proxy DescribeVPCs)

testRequestSpotInstancesResponse :: RequestSpotInstancesResponse -> TestTree
testRequestSpotInstancesResponse = res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse"
    (Proxy :: Proxy RequestSpotInstances)

testModifyImageAttributeResponse :: ModifyImageAttributeResponse -> TestTree
testModifyImageAttributeResponse = res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse"
    (Proxy :: Proxy ModifyImageAttribute)

testDescribeReservedInstancesResponse :: DescribeReservedInstancesResponse -> TestTree
testDescribeReservedInstancesResponse = res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse"
    (Proxy :: Proxy DescribeReservedInstances)

testAllocateAddressResponse :: AllocateAddressResponse -> TestTree
testAllocateAddressResponse = res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse"
    (Proxy :: Proxy AllocateAddress)

testRunInstancesResponse :: Reservation -> TestTree
testRunInstancesResponse = res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse"
    (Proxy :: Proxy RunInstances)

testCreateRouteTableResponse :: CreateRouteTableResponse -> TestTree
testCreateRouteTableResponse = res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse"
    (Proxy :: Proxy CreateRouteTable)

testAttachVolumeResponse :: VolumeAttachment -> TestTree
testAttachVolumeResponse = res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse"
    (Proxy :: Proxy AttachVolume)

testDescribeConversionTasksResponse :: DescribeConversionTasksResponse -> TestTree
testDescribeConversionTasksResponse = res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse"
    (Proxy :: Proxy DescribeConversionTasks)

testRejectVPCPeeringConnectionResponse :: RejectVPCPeeringConnectionResponse -> TestTree
testRejectVPCPeeringConnectionResponse = res
    "RejectVPCPeeringConnectionResponse"
    "fixture/RejectVPCPeeringConnectionResponse"
    (Proxy :: Proxy RejectVPCPeeringConnection)

testRevokeSecurityGroupIngressResponse :: RevokeSecurityGroupIngressResponse -> TestTree
testRevokeSecurityGroupIngressResponse = res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeSecurityGroupIngress)

testDescribeVolumesResponse :: DescribeVolumesResponse -> TestTree
testDescribeVolumesResponse = res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse"
    (Proxy :: Proxy DescribeVolumes)

testDeleteVPNConnectionRouteResponse :: DeleteVPNConnectionRouteResponse -> TestTree
testDeleteVPNConnectionRouteResponse = res
    "DeleteVPNConnectionRouteResponse"
    "fixture/DeleteVPNConnectionRouteResponse"
    (Proxy :: Proxy DeleteVPNConnectionRoute)

testModifyReservedInstancesResponse :: ModifyReservedInstancesResponse -> TestTree
testModifyReservedInstancesResponse = res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse"
    (Proxy :: Proxy ModifyReservedInstances)

testRegisterImageResponse :: RegisterImageResponse -> TestTree
testRegisterImageResponse = res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse"
    (Proxy :: Proxy RegisterImage)

testModifyVPCEndpointResponse :: ModifyVPCEndpointResponse -> TestTree
testModifyVPCEndpointResponse = res
    "ModifyVPCEndpointResponse"
    "fixture/ModifyVPCEndpointResponse"
    (Proxy :: Proxy ModifyVPCEndpoint)

testDeleteVPNGatewayResponse :: DeleteVPNGatewayResponse -> TestTree
testDeleteVPNGatewayResponse = res
    "DeleteVPNGatewayResponse"
    "fixture/DeleteVPNGatewayResponse"
    (Proxy :: Proxy DeleteVPNGateway)

testCreateVPCResponse :: CreateVPCResponse -> TestTree
testCreateVPCResponse = res
    "CreateVPCResponse"
    "fixture/CreateVPCResponse"
    (Proxy :: Proxy CreateVPC)

testDescribeMovingAddressesResponse :: DescribeMovingAddressesResponse -> TestTree
testDescribeMovingAddressesResponse = res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse"
    (Proxy :: Proxy DescribeMovingAddresses)

testDescribeVolumeAttributeResponse :: DescribeVolumeAttributeResponse -> TestTree
testDescribeVolumeAttributeResponse = res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse"
    (Proxy :: Proxy DescribeVolumeAttribute)

testMoveAddressToVPCResponse :: MoveAddressToVPCResponse -> TestTree
testMoveAddressToVPCResponse = res
    "MoveAddressToVPCResponse"
    "fixture/MoveAddressToVPCResponse"
    (Proxy :: Proxy MoveAddressToVPC)

testGetPasswordDataResponse :: GetPasswordDataResponse -> TestTree
testGetPasswordDataResponse = res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse"
    (Proxy :: Proxy GetPasswordData)

testCreateFlowLogsResponse :: CreateFlowLogsResponse -> TestTree
testCreateFlowLogsResponse = res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse"
    (Proxy :: Proxy CreateFlowLogs)

testDescribeImportImageTasksResponse :: DescribeImportImageTasksResponse -> TestTree
testDescribeImportImageTasksResponse = res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse"
    (Proxy :: Proxy DescribeImportImageTasks)

testDeleteNetworkACLResponse :: DeleteNetworkACLResponse -> TestTree
testDeleteNetworkACLResponse = res
    "DeleteNetworkACLResponse"
    "fixture/DeleteNetworkACLResponse"
    (Proxy :: Proxy DeleteNetworkACL)

testDescribeSpotFleetRequestsResponse :: DescribeSpotFleetRequestsResponse -> TestTree
testDescribeSpotFleetRequestsResponse = res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse"
    (Proxy :: Proxy DescribeSpotFleetRequests)

testCopySnapshotResponse :: CopySnapshotResponse -> TestTree
testCopySnapshotResponse = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse"
    (Proxy :: Proxy CopySnapshot)

testModifyVolumeAttributeResponse :: ModifyVolumeAttributeResponse -> TestTree
testModifyVolumeAttributeResponse = res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse"
    (Proxy :: Proxy ModifyVolumeAttribute)

testDescribeVPCAttributeResponse :: DescribeVPCAttributeResponse -> TestTree
testDescribeVPCAttributeResponse = res
    "DescribeVPCAttributeResponse"
    "fixture/DescribeVPCAttributeResponse"
    (Proxy :: Proxy DescribeVPCAttribute)

testCreateVolumeResponse :: Volume -> TestTree
testCreateVolumeResponse = res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse"
    (Proxy :: Proxy CreateVolume)

testDisassociateAddressResponse :: DisassociateAddressResponse -> TestTree
testDisassociateAddressResponse = res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse"
    (Proxy :: Proxy DisassociateAddress)

testDeleteVPCResponse :: DeleteVPCResponse -> TestTree
testDeleteVPCResponse = res
    "DeleteVPCResponse"
    "fixture/DeleteVPCResponse"
    (Proxy :: Proxy DeleteVPC)

testDescribePrefixListsResponse :: DescribePrefixListsResponse -> TestTree
testDescribePrefixListsResponse = res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse"
    (Proxy :: Proxy DescribePrefixLists)

testCreateInstanceExportTaskResponse :: CreateInstanceExportTaskResponse -> TestTree
testCreateInstanceExportTaskResponse = res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse"
    (Proxy :: Proxy CreateInstanceExportTask)

testDescribeSpotDatafeedSubscriptionResponse :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
testDescribeSpotDatafeedSubscriptionResponse = res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

testDetachVPNGatewayResponse :: DetachVPNGatewayResponse -> TestTree
testDetachVPNGatewayResponse = res
    "DetachVPNGatewayResponse"
    "fixture/DetachVPNGatewayResponse"
    (Proxy :: Proxy DetachVPNGateway)

testDescribeExportTasksResponse :: DescribeExportTasksResponse -> TestTree
testDescribeExportTasksResponse = res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse"
    (Proxy :: Proxy DescribeExportTasks)

testDeletePlacementGroupResponse :: DeletePlacementGroupResponse -> TestTree
testDeletePlacementGroupResponse = res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse"
    (Proxy :: Proxy DeletePlacementGroup)

testCreateSubnetResponse :: CreateSubnetResponse -> TestTree
testCreateSubnetResponse = res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse"
    (Proxy :: Proxy CreateSubnet)

testEnableVolumeIOResponse :: EnableVolumeIOResponse -> TestTree
testEnableVolumeIOResponse = res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse"
    (Proxy :: Proxy EnableVolumeIO)

testCancelExportTaskResponse :: CancelExportTaskResponse -> TestTree
testCancelExportTaskResponse = res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse"
    (Proxy :: Proxy CancelExportTask)

testRequestSpotFleetResponse :: RequestSpotFleetResponse -> TestTree
testRequestSpotFleetResponse = res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse"
    (Proxy :: Proxy RequestSpotFleet)

testDescribeInstancesResponse :: DescribeInstancesResponse -> TestTree
testDescribeInstancesResponse = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse"
    (Proxy :: Proxy DescribeInstances)

testDescribeSecurityGroupsResponse :: DescribeSecurityGroupsResponse -> TestTree
testDescribeSecurityGroupsResponse = res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse"
    (Proxy :: Proxy DescribeSecurityGroups)

testDescribeVPCPeeringConnectionsResponse :: DescribeVPCPeeringConnectionsResponse -> TestTree
testDescribeVPCPeeringConnectionsResponse = res
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse"
    (Proxy :: Proxy DescribeVPCPeeringConnections)

testCreateNetworkInterfaceResponse :: CreateNetworkInterfaceResponse -> TestTree
testCreateNetworkInterfaceResponse = res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse"
    (Proxy :: Proxy CreateNetworkInterface)

testAssociateAddressResponse :: AssociateAddressResponse -> TestTree
testAssociateAddressResponse = res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse"
    (Proxy :: Proxy AssociateAddress)

testStartInstancesResponse :: StartInstancesResponse -> TestTree
testStartInstancesResponse = res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse"
    (Proxy :: Proxy StartInstances)

testDescribeCustomerGatewaysResponse :: DescribeCustomerGatewaysResponse -> TestTree
testDescribeCustomerGatewaysResponse = res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse"
    (Proxy :: Proxy DescribeCustomerGateways)

testResetNetworkInterfaceAttributeResponse :: ResetNetworkInterfaceAttributeResponse -> TestTree
testResetNetworkInterfaceAttributeResponse = res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

testCreateVPNConnectionResponse :: CreateVPNConnectionResponse -> TestTree
testCreateVPNConnectionResponse = res
    "CreateVPNConnectionResponse"
    "fixture/CreateVPNConnectionResponse"
    (Proxy :: Proxy CreateVPNConnection)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

testCreatePlacementGroupResponse :: CreatePlacementGroupResponse -> TestTree
testCreatePlacementGroupResponse = res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse"
    (Proxy :: Proxy CreatePlacementGroup)

testReplaceRouteTableAssociationResponse :: ReplaceRouteTableAssociationResponse -> TestTree
testReplaceRouteTableAssociationResponse = res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse"
    (Proxy :: Proxy ReplaceRouteTableAssociation)

testDescribeNetworkInterfaceAttributeResponse :: DescribeNetworkInterfaceAttributeResponse -> TestTree
testDescribeNetworkInterfaceAttributeResponse = res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

testDescribeReservedInstancesListingsResponse :: DescribeReservedInstancesListingsResponse -> TestTree
testDescribeReservedInstancesListingsResponse = res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse"
    (Proxy :: Proxy DescribeReservedInstancesListings)

testDeleteNetworkInterfaceResponse :: DeleteNetworkInterfaceResponse -> TestTree
testDeleteNetworkInterfaceResponse = res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse"
    (Proxy :: Proxy DeleteNetworkInterface)

testDeleteInternetGatewayResponse :: DeleteInternetGatewayResponse -> TestTree
testDeleteInternetGatewayResponse = res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse"
    (Proxy :: Proxy DeleteInternetGateway)

testDeleteSubnetResponse :: DeleteSubnetResponse -> TestTree
testDeleteSubnetResponse = res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse"
    (Proxy :: Proxy DeleteSubnet)

testCreateVPCEndpointResponse :: CreateVPCEndpointResponse -> TestTree
testCreateVPCEndpointResponse = res
    "CreateVPCEndpointResponse"
    "fixture/CreateVPCEndpointResponse"
    (Proxy :: Proxy CreateVPCEndpoint)

testDescribeImportSnapshotTasksResponse :: DescribeImportSnapshotTasksResponse -> TestTree
testDescribeImportSnapshotTasksResponse = res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse"
    (Proxy :: Proxy DescribeImportSnapshotTasks)

testCopyImageResponse :: CopyImageResponse -> TestTree
testCopyImageResponse = res
    "CopyImageResponse"
    "fixture/CopyImageResponse"
    (Proxy :: Proxy CopyImage)

testDisassociateRouteTableResponse :: DisassociateRouteTableResponse -> TestTree
testDisassociateRouteTableResponse = res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse"
    (Proxy :: Proxy DisassociateRouteTable)

testUnmonitorInstancesResponse :: UnmonitorInstancesResponse -> TestTree
testUnmonitorInstancesResponse = res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse"
    (Proxy :: Proxy UnmonitorInstances)

testImportVolumeResponse :: ImportVolumeResponse -> TestTree
testImportVolumeResponse = res
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse"
    (Proxy :: Proxy ImportVolume)

testDisableVGWRoutePropagationResponse :: DisableVGWRoutePropagationResponse -> TestTree
testDisableVGWRoutePropagationResponse = res
    "DisableVGWRoutePropagationResponse"
    "fixture/DisableVGWRoutePropagationResponse"
    (Proxy :: Proxy DisableVGWRoutePropagation)

testCreateSpotDatafeedSubscriptionResponse :: CreateSpotDatafeedSubscriptionResponse -> TestTree
testCreateSpotDatafeedSubscriptionResponse = res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

testAssignPrivateIPAddressesResponse :: AssignPrivateIPAddressesResponse -> TestTree
testAssignPrivateIPAddressesResponse = res
    "AssignPrivateIPAddressesResponse"
    "fixture/AssignPrivateIPAddressesResponse"
    (Proxy :: Proxy AssignPrivateIPAddresses)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

testDeleteCustomerGatewayResponse :: DeleteCustomerGatewayResponse -> TestTree
testDeleteCustomerGatewayResponse = res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse"
    (Proxy :: Proxy DeleteCustomerGateway)

testModifyInstanceAttributeResponse :: ModifyInstanceAttributeResponse -> TestTree
testModifyInstanceAttributeResponse = res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse"
    (Proxy :: Proxy ModifyInstanceAttribute)

testCreateSecurityGroupResponse :: CreateSecurityGroupResponse -> TestTree
testCreateSecurityGroupResponse = res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse"
    (Proxy :: Proxy CreateSecurityGroup)

testCancelSpotInstanceRequestsResponse :: CancelSpotInstanceRequestsResponse -> TestTree
testCancelSpotInstanceRequestsResponse = res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse"
    (Proxy :: Proxy CancelSpotInstanceRequests)

testCreateRouteResponse :: CreateRouteResponse -> TestTree
testCreateRouteResponse = res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse"
    (Proxy :: Proxy CreateRoute)

testCreateNetworkACLEntryResponse :: CreateNetworkACLEntryResponse -> TestTree
testCreateNetworkACLEntryResponse = res
    "CreateNetworkACLEntryResponse"
    "fixture/CreateNetworkACLEntryResponse"
    (Proxy :: Proxy CreateNetworkACLEntry)

testModifySnapshotAttributeResponse :: ModifySnapshotAttributeResponse -> TestTree
testModifySnapshotAttributeResponse = res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse"
    (Proxy :: Proxy ModifySnapshotAttribute)

testEnableVGWRoutePropagationResponse :: EnableVGWRoutePropagationResponse -> TestTree
testEnableVGWRoutePropagationResponse = res
    "EnableVGWRoutePropagationResponse"
    "fixture/EnableVGWRoutePropagationResponse"
    (Proxy :: Proxy EnableVGWRoutePropagation)

testCreateSnapshotResponse :: Snapshot -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

testDescribeSpotFleetRequestHistoryResponse :: DescribeSpotFleetRequestHistoryResponse -> TestTree
testDescribeSpotFleetRequestHistoryResponse = res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse"
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

testDeleteSpotDatafeedSubscriptionResponse :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
testDeleteSpotDatafeedSubscriptionResponse = res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

testDescribePlacementGroupsResponse :: DescribePlacementGroupsResponse -> TestTree
testDescribePlacementGroupsResponse = res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse"
    (Proxy :: Proxy DescribePlacementGroups)

testCreateReservedInstancesListingResponse :: CreateReservedInstancesListingResponse -> TestTree
testCreateReservedInstancesListingResponse = res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse"
    (Proxy :: Proxy CreateReservedInstancesListing)

testEnableVPCClassicLinkResponse :: EnableVPCClassicLinkResponse -> TestTree
testEnableVPCClassicLinkResponse = res
    "EnableVPCClassicLinkResponse"
    "fixture/EnableVPCClassicLinkResponse"
    (Proxy :: Proxy EnableVPCClassicLink)

testDescribeKeyPairsResponse :: DescribeKeyPairsResponse -> TestTree
testDescribeKeyPairsResponse = res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse"
    (Proxy :: Proxy DescribeKeyPairs)

testRebootInstancesResponse :: RebootInstancesResponse -> TestTree
testRebootInstancesResponse = res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse"
    (Proxy :: Proxy RebootInstances)

testAttachVPNGatewayResponse :: AttachVPNGatewayResponse -> TestTree
testAttachVPNGatewayResponse = res
    "AttachVPNGatewayResponse"
    "fixture/AttachVPNGatewayResponse"
    (Proxy :: Proxy AttachVPNGateway)

testCreateVPNConnectionRouteResponse :: CreateVPNConnectionRouteResponse -> TestTree
testCreateVPNConnectionRouteResponse = res
    "CreateVPNConnectionRouteResponse"
    "fixture/CreateVPNConnectionRouteResponse"
    (Proxy :: Proxy CreateVPNConnectionRoute)

testDescribeClassicLinkInstancesResponse :: DescribeClassicLinkInstancesResponse -> TestTree
testDescribeClassicLinkInstancesResponse = res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse"
    (Proxy :: Proxy DescribeClassicLinkInstances)

testTerminateInstancesResponse :: TerminateInstancesResponse -> TestTree
testTerminateInstancesResponse = res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse"
    (Proxy :: Proxy TerminateInstances)

testCreateDHCPOptionsResponse :: CreateDHCPOptionsResponse -> TestTree
testCreateDHCPOptionsResponse = res
    "CreateDHCPOptionsResponse"
    "fixture/CreateDHCPOptionsResponse"
    (Proxy :: Proxy CreateDHCPOptions)

testAssociateRouteTableResponse :: AssociateRouteTableResponse -> TestTree
testAssociateRouteTableResponse = res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse"
    (Proxy :: Proxy AssociateRouteTable)

testCreateImageResponse :: CreateImageResponse -> TestTree
testCreateImageResponse = res
    "CreateImageResponse"
    "fixture/CreateImageResponse"
    (Proxy :: Proxy CreateImage)

testDescribeAccountAttributesResponse :: DescribeAccountAttributesResponse -> TestTree
testDescribeAccountAttributesResponse = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse"
    (Proxy :: Proxy DescribeAccountAttributes)

testResetImageAttributeResponse :: ResetImageAttributeResponse -> TestTree
testResetImageAttributeResponse = res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse"
    (Proxy :: Proxy ResetImageAttribute)

testDescribeNetworkACLsResponse :: DescribeNetworkACLsResponse -> TestTree
testDescribeNetworkACLsResponse = res
    "DescribeNetworkACLsResponse"
    "fixture/DescribeNetworkACLsResponse"
    (Proxy :: Proxy DescribeNetworkACLs)

testCancelImportTaskResponse :: CancelImportTaskResponse -> TestTree
testCancelImportTaskResponse = res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse"
    (Proxy :: Proxy CancelImportTask)

testGetConsoleOutputResponse :: GetConsoleOutputResponse -> TestTree
testGetConsoleOutputResponse = res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse"
    (Proxy :: Proxy GetConsoleOutput)

testUnassignPrivateIPAddressesResponse :: UnassignPrivateIPAddressesResponse -> TestTree
testUnassignPrivateIPAddressesResponse = res
    "UnassignPrivateIPAddressesResponse"
    "fixture/UnassignPrivateIPAddressesResponse"
    (Proxy :: Proxy UnassignPrivateIPAddresses)

testDeleteRouteTableResponse :: DeleteRouteTableResponse -> TestTree
testDeleteRouteTableResponse = res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse"
    (Proxy :: Proxy DeleteRouteTable)

testDescribeImageAttributeResponse :: DescribeImageAttributeResponse -> TestTree
testDescribeImageAttributeResponse = res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse"
    (Proxy :: Proxy DescribeImageAttribute)

testDeleteDHCPOptionsResponse :: DeleteDHCPOptionsResponse -> TestTree
testDeleteDHCPOptionsResponse = res
    "DeleteDHCPOptionsResponse"
    "fixture/DeleteDHCPOptionsResponse"
    (Proxy :: Proxy DeleteDHCPOptions)

testDescribeVPNGatewaysResponse :: DescribeVPNGatewaysResponse -> TestTree
testDescribeVPNGatewaysResponse = res
    "DescribeVPNGatewaysResponse"
    "fixture/DescribeVPNGatewaysResponse"
    (Proxy :: Proxy DescribeVPNGateways)

testDetachClassicLinkVPCResponse :: DetachClassicLinkVPCResponse -> TestTree
testDetachClassicLinkVPCResponse = res
    "DetachClassicLinkVPCResponse"
    "fixture/DetachClassicLinkVPCResponse"
    (Proxy :: Proxy DetachClassicLinkVPC)

testDescribeReservedInstancesModificationsResponse :: DescribeReservedInstancesModificationsResponse -> TestTree
testDescribeReservedInstancesModificationsResponse = res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse"
    (Proxy :: Proxy DescribeReservedInstancesModifications)

testDescribeSpotInstanceRequestsResponse :: DescribeSpotInstanceRequestsResponse -> TestTree
testDescribeSpotInstanceRequestsResponse = res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse"
    (Proxy :: Proxy DescribeSpotInstanceRequests)

testMonitorInstancesResponse :: MonitorInstancesResponse -> TestTree
testMonitorInstancesResponse = res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse"
    (Proxy :: Proxy MonitorInstances)

testDescribeRegionsResponse :: DescribeRegionsResponse -> TestTree
testDescribeRegionsResponse = res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse"
    (Proxy :: Proxy DescribeRegions)

testModifyVPCAttributeResponse :: ModifyVPCAttributeResponse -> TestTree
testModifyVPCAttributeResponse = res
    "ModifyVPCAttributeResponse"
    "fixture/ModifyVPCAttributeResponse"
    (Proxy :: Proxy ModifyVPCAttribute)

testDescribeSpotFleetInstancesResponse :: DescribeSpotFleetInstancesResponse -> TestTree
testDescribeSpotFleetInstancesResponse = res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse"
    (Proxy :: Proxy DescribeSpotFleetInstances)

testDescribeVolumeStatusResponse :: DescribeVolumeStatusResponse -> TestTree
testDescribeVolumeStatusResponse = res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse"
    (Proxy :: Proxy DescribeVolumeStatus)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse"
    (Proxy :: Proxy DeleteVolume)

testDescribeImagesResponse :: DescribeImagesResponse -> TestTree
testDescribeImagesResponse = res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse"
    (Proxy :: Proxy DescribeImages)

testCreateKeyPairResponse :: CreateKeyPairResponse -> TestTree
testCreateKeyPairResponse = res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse"
    (Proxy :: Proxy CreateKeyPair)

testRestoreAddressToClassicResponse :: RestoreAddressToClassicResponse -> TestTree
testRestoreAddressToClassicResponse = res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse"
    (Proxy :: Proxy RestoreAddressToClassic)

testDescribeAvailabilityZonesResponse :: DescribeAvailabilityZonesResponse -> TestTree
testDescribeAvailabilityZonesResponse = res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse"
    (Proxy :: Proxy DescribeAvailabilityZones)

testImportSnapshotResponse :: ImportSnapshotResponse -> TestTree
testImportSnapshotResponse = res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse"
    (Proxy :: Proxy ImportSnapshot)

testAcceptVPCPeeringConnectionResponse :: AcceptVPCPeeringConnectionResponse -> TestTree
testAcceptVPCPeeringConnectionResponse = res
    "AcceptVPCPeeringConnectionResponse"
    "fixture/AcceptVPCPeeringConnectionResponse"
    (Proxy :: Proxy AcceptVPCPeeringConnection)

testDescribeRouteTablesResponse :: DescribeRouteTablesResponse -> TestTree
testDescribeRouteTablesResponse = res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse"
    (Proxy :: Proxy DescribeRouteTables)
