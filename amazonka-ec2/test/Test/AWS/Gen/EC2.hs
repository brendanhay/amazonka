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
    "fixture/DetachNetworkInterface.yaml"

testDeleteVPCEndpoints :: DeleteVPCEndpoints -> TestTree
testDeleteVPCEndpoints = req
    "DeleteVPCEndpoints"
    "fixture/DeleteVPCEndpoints.yaml"

testDeleteKeyPair :: DeleteKeyPair -> TestTree
testDeleteKeyPair = req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

testDeleteFlowLogs :: DeleteFlowLogs -> TestTree
testDeleteFlowLogs = req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs.yaml"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

testRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
testRevokeSecurityGroupEgress = req
    "RevokeSecurityGroupEgress"
    "fixture/RevokeSecurityGroupEgress.yaml"

testCreateVPNGateway :: CreateVPNGateway -> TestTree
testCreateVPNGateway = req
    "CreateVPNGateway"
    "fixture/CreateVPNGateway.yaml"

testDetachInternetGateway :: DetachInternetGateway -> TestTree
testDetachInternetGateway = req
    "DetachInternetGateway"
    "fixture/DetachInternetGateway.yaml"

testCreateNetworkACL :: CreateNetworkACL -> TestTree
testCreateNetworkACL = req
    "CreateNetworkACL"
    "fixture/CreateNetworkACL.yaml"

testImportInstance :: ImportInstance -> TestTree
testImportInstance = req
    "ImportInstance"
    "fixture/ImportInstance.yaml"

testDescribeVPCClassicLink :: DescribeVPCClassicLink -> TestTree
testDescribeVPCClassicLink = req
    "DescribeVPCClassicLink"
    "fixture/DescribeVPCClassicLink.yaml"

testDeleteVPNConnection :: DeleteVPNConnection -> TestTree
testDeleteVPNConnection = req
    "DeleteVPNConnection"
    "fixture/DeleteVPNConnection.yaml"

testAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
testAuthorizeSecurityGroupEgress = req
    "AuthorizeSecurityGroupEgress"
    "fixture/AuthorizeSecurityGroupEgress.yaml"

testDescribeBundleTasks :: DescribeBundleTasks -> TestTree
testDescribeBundleTasks = req
    "DescribeBundleTasks"
    "fixture/DescribeBundleTasks.yaml"

testCreateInternetGateway :: CreateInternetGateway -> TestTree
testCreateInternetGateway = req
    "CreateInternetGateway"
    "fixture/CreateInternetGateway.yaml"

testReleaseAddress :: ReleaseAddress -> TestTree
testReleaseAddress = req
    "ReleaseAddress"
    "fixture/ReleaseAddress.yaml"

testCancelBundleTask :: CancelBundleTask -> TestTree
testCancelBundleTask = req
    "CancelBundleTask"
    "fixture/CancelBundleTask.yaml"

testModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
testModifyNetworkInterfaceAttribute = req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute.yaml"

testModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
testModifySubnetAttribute = req
    "ModifySubnetAttribute"
    "fixture/ModifySubnetAttribute.yaml"

testDeregisterImage :: DeregisterImage -> TestTree
testDeregisterImage = req
    "DeregisterImage"
    "fixture/DeregisterImage.yaml"

testDetachVolume :: DetachVolume -> TestTree
testDetachVolume = req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

testCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
testCancelReservedInstancesListing = req
    "CancelReservedInstancesListing"
    "fixture/CancelReservedInstancesListing.yaml"

testAttachClassicLinkVPC :: AttachClassicLinkVPC -> TestTree
testAttachClassicLinkVPC = req
    "AttachClassicLinkVPC"
    "fixture/AttachClassicLinkVPC.yaml"

testCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
testCancelSpotFleetRequests = req
    "CancelSpotFleetRequests"
    "fixture/CancelSpotFleetRequests.yaml"

testDescribeDHCPOptions :: DescribeDHCPOptions -> TestTree
testDescribeDHCPOptions = req
    "DescribeDHCPOptions"
    "fixture/DescribeDHCPOptions.yaml"

testDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
testDescribeSpotPriceHistory = req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory.yaml"

testStopInstances :: StopInstances -> TestTree
testStopInstances = req
    "StopInstances"
    "fixture/StopInstances.yaml"

testImportImage :: ImportImage -> TestTree
testImportImage = req
    "ImportImage"
    "fixture/ImportImage.yaml"

testDeleteNetworkACLEntry :: DeleteNetworkACLEntry -> TestTree
testDeleteNetworkACLEntry = req
    "DeleteNetworkACLEntry"
    "fixture/DeleteNetworkACLEntry.yaml"

testDisableVPCClassicLink :: DisableVPCClassicLink -> TestTree
testDisableVPCClassicLink = req
    "DisableVPCClassicLink"
    "fixture/DisableVPCClassicLink.yaml"

testAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
testAuthorizeSecurityGroupIngress = req
    "AuthorizeSecurityGroupIngress"
    "fixture/AuthorizeSecurityGroupIngress.yaml"

testBundleInstance :: BundleInstance -> TestTree
testBundleInstance = req
    "BundleInstance"
    "fixture/BundleInstance.yaml"

testDescribeVPCEndpointServices :: DescribeVPCEndpointServices -> TestTree
testDescribeVPCEndpointServices = req
    "DescribeVPCEndpointServices"
    "fixture/DescribeVPCEndpointServices.yaml"

testReplaceNetworkACLAssociation :: ReplaceNetworkACLAssociation -> TestTree
testReplaceNetworkACLAssociation = req
    "ReplaceNetworkACLAssociation"
    "fixture/ReplaceNetworkACLAssociation.yaml"

testCreateVPCPeeringConnection :: CreateVPCPeeringConnection -> TestTree
testCreateVPCPeeringConnection = req
    "CreateVPCPeeringConnection"
    "fixture/CreateVPCPeeringConnection.yaml"

testResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
testResetSnapshotAttribute = req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute.yaml"

testDescribeAddresses :: DescribeAddresses -> TestTree
testDescribeAddresses = req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

testDescribeInternetGateways :: DescribeInternetGateways -> TestTree
testDescribeInternetGateways = req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways.yaml"

testReplaceRoute :: ReplaceRoute -> TestTree
testReplaceRoute = req
    "ReplaceRoute"
    "fixture/ReplaceRoute.yaml"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

testDescribeSubnets :: DescribeSubnets -> TestTree
testDescribeSubnets = req
    "DescribeSubnets"
    "fixture/DescribeSubnets.yaml"

testDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
testDescribeNetworkInterfaces = req
    "DescribeNetworkInterfaces"
    "fixture/DescribeNetworkInterfaces.yaml"

testPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
testPurchaseReservedInstancesOffering = req
    "PurchaseReservedInstancesOffering"
    "fixture/PurchaseReservedInstancesOffering.yaml"

testDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
testDescribeSnapshotAttribute = req
    "DescribeSnapshotAttribute"
    "fixture/DescribeSnapshotAttribute.yaml"

testCreateCustomerGateway :: CreateCustomerGateway -> TestTree
testCreateCustomerGateway = req
    "CreateCustomerGateway"
    "fixture/CreateCustomerGateway.yaml"

testAttachInternetGateway :: AttachInternetGateway -> TestTree
testAttachInternetGateway = req
    "AttachInternetGateway"
    "fixture/AttachInternetGateway.yaml"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

testReplaceNetworkACLEntry :: ReplaceNetworkACLEntry -> TestTree
testReplaceNetworkACLEntry = req
    "ReplaceNetworkACLEntry"
    "fixture/ReplaceNetworkACLEntry.yaml"

testResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
testResetInstanceAttribute = req
    "ResetInstanceAttribute"
    "fixture/ResetInstanceAttribute.yaml"

testDeleteRoute :: DeleteRoute -> TestTree
testDeleteRoute = req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

testDescribeVPNConnections :: DescribeVPNConnections -> TestTree
testDescribeVPNConnections = req
    "DescribeVPNConnections"
    "fixture/DescribeVPNConnections.yaml"

testDescribeFlowLogs :: DescribeFlowLogs -> TestTree
testDescribeFlowLogs = req
    "DescribeFlowLogs"
    "fixture/DescribeFlowLogs.yaml"

testDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
testDeleteSecurityGroup = req
    "DeleteSecurityGroup"
    "fixture/DeleteSecurityGroup.yaml"

testDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
testDescribeReservedInstancesOfferings = req
    "DescribeReservedInstancesOfferings"
    "fixture/DescribeReservedInstancesOfferings.yaml"

testDeleteVPCPeeringConnection :: DeleteVPCPeeringConnection -> TestTree
testDeleteVPCPeeringConnection = req
    "DeleteVPCPeeringConnection"
    "fixture/DeleteVPCPeeringConnection.yaml"

testDescribeVPCEndpoints :: DescribeVPCEndpoints -> TestTree
testDescribeVPCEndpoints = req
    "DescribeVPCEndpoints"
    "fixture/DescribeVPCEndpoints.yaml"

testDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
testDescribeInstanceAttribute = req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

testConfirmProductInstance :: ConfirmProductInstance -> TestTree
testConfirmProductInstance = req
    "ConfirmProductInstance"
    "fixture/ConfirmProductInstance.yaml"

testImportKeyPair :: ImportKeyPair -> TestTree
testImportKeyPair = req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

testAttachNetworkInterface :: AttachNetworkInterface -> TestTree
testAttachNetworkInterface = req
    "AttachNetworkInterface"
    "fixture/AttachNetworkInterface.yaml"

testDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
testDescribeInstanceStatus = req
    "DescribeInstanceStatus"
    "fixture/DescribeInstanceStatus.yaml"

testCancelConversionTask :: CancelConversionTask -> TestTree
testCancelConversionTask = req
    "CancelConversionTask"
    "fixture/CancelConversionTask.yaml"

testReportInstanceStatus :: ReportInstanceStatus -> TestTree
testReportInstanceStatus = req
    "ReportInstanceStatus"
    "fixture/ReportInstanceStatus.yaml"

testAssociateDHCPOptions :: AssociateDHCPOptions -> TestTree
testAssociateDHCPOptions = req
    "AssociateDHCPOptions"
    "fixture/AssociateDHCPOptions.yaml"

testDescribeVPCs :: DescribeVPCs -> TestTree
testDescribeVPCs = req
    "DescribeVPCs"
    "fixture/DescribeVPCs.yaml"

testRequestSpotInstances :: RequestSpotInstances -> TestTree
testRequestSpotInstances = req
    "RequestSpotInstances"
    "fixture/RequestSpotInstances.yaml"

testModifyImageAttribute :: ModifyImageAttribute -> TestTree
testModifyImageAttribute = req
    "ModifyImageAttribute"
    "fixture/ModifyImageAttribute.yaml"

testDescribeReservedInstances :: DescribeReservedInstances -> TestTree
testDescribeReservedInstances = req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances.yaml"

testAllocateAddress :: AllocateAddress -> TestTree
testAllocateAddress = req
    "AllocateAddress"
    "fixture/AllocateAddress.yaml"

testRunInstances :: RunInstances -> TestTree
testRunInstances = req
    "RunInstances"
    "fixture/RunInstances.yaml"

testCreateRouteTable :: CreateRouteTable -> TestTree
testCreateRouteTable = req
    "CreateRouteTable"
    "fixture/CreateRouteTable.yaml"

testAttachVolume :: AttachVolume -> TestTree
testAttachVolume = req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

testDescribeConversionTasks :: DescribeConversionTasks -> TestTree
testDescribeConversionTasks = req
    "DescribeConversionTasks"
    "fixture/DescribeConversionTasks.yaml"

testRejectVPCPeeringConnection :: RejectVPCPeeringConnection -> TestTree
testRejectVPCPeeringConnection = req
    "RejectVPCPeeringConnection"
    "fixture/RejectVPCPeeringConnection.yaml"

testRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
testRevokeSecurityGroupIngress = req
    "RevokeSecurityGroupIngress"
    "fixture/RevokeSecurityGroupIngress.yaml"

testDescribeVolumes :: DescribeVolumes -> TestTree
testDescribeVolumes = req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

testDeleteVPNConnectionRoute :: DeleteVPNConnectionRoute -> TestTree
testDeleteVPNConnectionRoute = req
    "DeleteVPNConnectionRoute"
    "fixture/DeleteVPNConnectionRoute.yaml"

testModifyReservedInstances :: ModifyReservedInstances -> TestTree
testModifyReservedInstances = req
    "ModifyReservedInstances"
    "fixture/ModifyReservedInstances.yaml"

testRegisterImage :: RegisterImage -> TestTree
testRegisterImage = req
    "RegisterImage"
    "fixture/RegisterImage.yaml"

testModifyVPCEndpoint :: ModifyVPCEndpoint -> TestTree
testModifyVPCEndpoint = req
    "ModifyVPCEndpoint"
    "fixture/ModifyVPCEndpoint.yaml"

testDeleteVPNGateway :: DeleteVPNGateway -> TestTree
testDeleteVPNGateway = req
    "DeleteVPNGateway"
    "fixture/DeleteVPNGateway.yaml"

testCreateVPC :: CreateVPC -> TestTree
testCreateVPC = req
    "CreateVPC"
    "fixture/CreateVPC.yaml"

testDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
testDescribeMovingAddresses = req
    "DescribeMovingAddresses"
    "fixture/DescribeMovingAddresses.yaml"

testDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
testDescribeVolumeAttribute = req
    "DescribeVolumeAttribute"
    "fixture/DescribeVolumeAttribute.yaml"

testMoveAddressToVPC :: MoveAddressToVPC -> TestTree
testMoveAddressToVPC = req
    "MoveAddressToVPC"
    "fixture/MoveAddressToVPC.yaml"

testGetPasswordData :: GetPasswordData -> TestTree
testGetPasswordData = req
    "GetPasswordData"
    "fixture/GetPasswordData.yaml"

testCreateFlowLogs :: CreateFlowLogs -> TestTree
testCreateFlowLogs = req
    "CreateFlowLogs"
    "fixture/CreateFlowLogs.yaml"

testDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
testDescribeImportImageTasks = req
    "DescribeImportImageTasks"
    "fixture/DescribeImportImageTasks.yaml"

testDeleteNetworkACL :: DeleteNetworkACL -> TestTree
testDeleteNetworkACL = req
    "DeleteNetworkACL"
    "fixture/DeleteNetworkACL.yaml"

testDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
testDescribeSpotFleetRequests = req
    "DescribeSpotFleetRequests"
    "fixture/DescribeSpotFleetRequests.yaml"

testCopySnapshot :: CopySnapshot -> TestTree
testCopySnapshot = req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

testModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
testModifyVolumeAttribute = req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute.yaml"

testDescribeVPCAttribute :: DescribeVPCAttribute -> TestTree
testDescribeVPCAttribute = req
    "DescribeVPCAttribute"
    "fixture/DescribeVPCAttribute.yaml"

testCreateVolume :: CreateVolume -> TestTree
testCreateVolume = req
    "CreateVolume"
    "fixture/CreateVolume.yaml"

testDisassociateAddress :: DisassociateAddress -> TestTree
testDisassociateAddress = req
    "DisassociateAddress"
    "fixture/DisassociateAddress.yaml"

testDeleteVPC :: DeleteVPC -> TestTree
testDeleteVPC = req
    "DeleteVPC"
    "fixture/DeleteVPC.yaml"

testDescribePrefixLists :: DescribePrefixLists -> TestTree
testDescribePrefixLists = req
    "DescribePrefixLists"
    "fixture/DescribePrefixLists.yaml"

testCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
testCreateInstanceExportTask = req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask.yaml"

testDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
testDescribeSpotDatafeedSubscription = req
    "DescribeSpotDatafeedSubscription"
    "fixture/DescribeSpotDatafeedSubscription.yaml"

testDetachVPNGateway :: DetachVPNGateway -> TestTree
testDetachVPNGateway = req
    "DetachVPNGateway"
    "fixture/DetachVPNGateway.yaml"

testDescribeExportTasks :: DescribeExportTasks -> TestTree
testDescribeExportTasks = req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

testDeletePlacementGroup :: DeletePlacementGroup -> TestTree
testDeletePlacementGroup = req
    "DeletePlacementGroup"
    "fixture/DeletePlacementGroup.yaml"

testCreateSubnet :: CreateSubnet -> TestTree
testCreateSubnet = req
    "CreateSubnet"
    "fixture/CreateSubnet.yaml"

testEnableVolumeIO :: EnableVolumeIO -> TestTree
testEnableVolumeIO = req
    "EnableVolumeIO"
    "fixture/EnableVolumeIO.yaml"

testCancelExportTask :: CancelExportTask -> TestTree
testCancelExportTask = req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

testRequestSpotFleet :: RequestSpotFleet -> TestTree
testRequestSpotFleet = req
    "RequestSpotFleet"
    "fixture/RequestSpotFleet.yaml"

testDescribeInstances :: DescribeInstances -> TestTree
testDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

testDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
testDescribeSecurityGroups = req
    "DescribeSecurityGroups"
    "fixture/DescribeSecurityGroups.yaml"

testDescribeVPCPeeringConnections :: DescribeVPCPeeringConnections -> TestTree
testDescribeVPCPeeringConnections = req
    "DescribeVPCPeeringConnections"
    "fixture/DescribeVPCPeeringConnections.yaml"

testCreateNetworkInterface :: CreateNetworkInterface -> TestTree
testCreateNetworkInterface = req
    "CreateNetworkInterface"
    "fixture/CreateNetworkInterface.yaml"

testAssociateAddress :: AssociateAddress -> TestTree
testAssociateAddress = req
    "AssociateAddress"
    "fixture/AssociateAddress.yaml"

testStartInstances :: StartInstances -> TestTree
testStartInstances = req
    "StartInstances"
    "fixture/StartInstances.yaml"

testDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
testDescribeCustomerGateways = req
    "DescribeCustomerGateways"
    "fixture/DescribeCustomerGateways.yaml"

testResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
testResetNetworkInterfaceAttribute = req
    "ResetNetworkInterfaceAttribute"
    "fixture/ResetNetworkInterfaceAttribute.yaml"

testCreateVPNConnection :: CreateVPNConnection -> TestTree
testCreateVPNConnection = req
    "CreateVPNConnection"
    "fixture/CreateVPNConnection.yaml"

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

testCreatePlacementGroup :: CreatePlacementGroup -> TestTree
testCreatePlacementGroup = req
    "CreatePlacementGroup"
    "fixture/CreatePlacementGroup.yaml"

testReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
testReplaceRouteTableAssociation = req
    "ReplaceRouteTableAssociation"
    "fixture/ReplaceRouteTableAssociation.yaml"

testDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
testDescribeNetworkInterfaceAttribute = req
    "DescribeNetworkInterfaceAttribute"
    "fixture/DescribeNetworkInterfaceAttribute.yaml"

testDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
testDescribeReservedInstancesListings = req
    "DescribeReservedInstancesListings"
    "fixture/DescribeReservedInstancesListings.yaml"

testDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
testDeleteNetworkInterface = req
    "DeleteNetworkInterface"
    "fixture/DeleteNetworkInterface.yaml"

testDeleteInternetGateway :: DeleteInternetGateway -> TestTree
testDeleteInternetGateway = req
    "DeleteInternetGateway"
    "fixture/DeleteInternetGateway.yaml"

testDeleteSubnet :: DeleteSubnet -> TestTree
testDeleteSubnet = req
    "DeleteSubnet"
    "fixture/DeleteSubnet.yaml"

testCreateVPCEndpoint :: CreateVPCEndpoint -> TestTree
testCreateVPCEndpoint = req
    "CreateVPCEndpoint"
    "fixture/CreateVPCEndpoint.yaml"

testDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
testDescribeImportSnapshotTasks = req
    "DescribeImportSnapshotTasks"
    "fixture/DescribeImportSnapshotTasks.yaml"

testCopyImage :: CopyImage -> TestTree
testCopyImage = req
    "CopyImage"
    "fixture/CopyImage.yaml"

testDisassociateRouteTable :: DisassociateRouteTable -> TestTree
testDisassociateRouteTable = req
    "DisassociateRouteTable"
    "fixture/DisassociateRouteTable.yaml"

testUnmonitorInstances :: UnmonitorInstances -> TestTree
testUnmonitorInstances = req
    "UnmonitorInstances"
    "fixture/UnmonitorInstances.yaml"

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

testAssignPrivateIPAddresses :: AssignPrivateIPAddresses -> TestTree
testAssignPrivateIPAddresses = req
    "AssignPrivateIPAddresses"
    "fixture/AssignPrivateIPAddresses.yaml"

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

testDeleteCustomerGateway :: DeleteCustomerGateway -> TestTree
testDeleteCustomerGateway = req
    "DeleteCustomerGateway"
    "fixture/DeleteCustomerGateway.yaml"

testModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
testModifyInstanceAttribute = req
    "ModifyInstanceAttribute"
    "fixture/ModifyInstanceAttribute.yaml"

testCreateSecurityGroup :: CreateSecurityGroup -> TestTree
testCreateSecurityGroup = req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

testCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
testCancelSpotInstanceRequests = req
    "CancelSpotInstanceRequests"
    "fixture/CancelSpotInstanceRequests.yaml"

testCreateRoute :: CreateRoute -> TestTree
testCreateRoute = req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

testCreateNetworkACLEntry :: CreateNetworkACLEntry -> TestTree
testCreateNetworkACLEntry = req
    "CreateNetworkACLEntry"
    "fixture/CreateNetworkACLEntry.yaml"

testModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
testModifySnapshotAttribute = req
    "ModifySnapshotAttribute"
    "fixture/ModifySnapshotAttribute.yaml"

testEnableVGWRoutePropagation :: EnableVGWRoutePropagation -> TestTree
testEnableVGWRoutePropagation = req
    "EnableVGWRoutePropagation"
    "fixture/EnableVGWRoutePropagation.yaml"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

testDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
testDescribeSpotFleetRequestHistory = req
    "DescribeSpotFleetRequestHistory"
    "fixture/DescribeSpotFleetRequestHistory.yaml"

testDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
testDeleteSpotDatafeedSubscription = req
    "DeleteSpotDatafeedSubscription"
    "fixture/DeleteSpotDatafeedSubscription.yaml"

testDescribePlacementGroups :: DescribePlacementGroups -> TestTree
testDescribePlacementGroups = req
    "DescribePlacementGroups"
    "fixture/DescribePlacementGroups.yaml"

testCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
testCreateReservedInstancesListing = req
    "CreateReservedInstancesListing"
    "fixture/CreateReservedInstancesListing.yaml"

testEnableVPCClassicLink :: EnableVPCClassicLink -> TestTree
testEnableVPCClassicLink = req
    "EnableVPCClassicLink"
    "fixture/EnableVPCClassicLink.yaml"

testDescribeKeyPairs :: DescribeKeyPairs -> TestTree
testDescribeKeyPairs = req
    "DescribeKeyPairs"
    "fixture/DescribeKeyPairs.yaml"

testRebootInstances :: RebootInstances -> TestTree
testRebootInstances = req
    "RebootInstances"
    "fixture/RebootInstances.yaml"

testAttachVPNGateway :: AttachVPNGateway -> TestTree
testAttachVPNGateway = req
    "AttachVPNGateway"
    "fixture/AttachVPNGateway.yaml"

testCreateVPNConnectionRoute :: CreateVPNConnectionRoute -> TestTree
testCreateVPNConnectionRoute = req
    "CreateVPNConnectionRoute"
    "fixture/CreateVPNConnectionRoute.yaml"

testDescribeClassicLinkInstances :: DescribeClassicLinkInstances -> TestTree
testDescribeClassicLinkInstances = req
    "DescribeClassicLinkInstances"
    "fixture/DescribeClassicLinkInstances.yaml"

testTerminateInstances :: TerminateInstances -> TestTree
testTerminateInstances = req
    "TerminateInstances"
    "fixture/TerminateInstances.yaml"

testCreateDHCPOptions :: CreateDHCPOptions -> TestTree
testCreateDHCPOptions = req
    "CreateDHCPOptions"
    "fixture/CreateDHCPOptions.yaml"

testAssociateRouteTable :: AssociateRouteTable -> TestTree
testAssociateRouteTable = req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable.yaml"

testCreateImage :: CreateImage -> TestTree
testCreateImage = req
    "CreateImage"
    "fixture/CreateImage.yaml"

testDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
testDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

testResetImageAttribute :: ResetImageAttribute -> TestTree
testResetImageAttribute = req
    "ResetImageAttribute"
    "fixture/ResetImageAttribute.yaml"

testDescribeNetworkACLs :: DescribeNetworkACLs -> TestTree
testDescribeNetworkACLs = req
    "DescribeNetworkACLs"
    "fixture/DescribeNetworkACLs.yaml"

testCancelImportTask :: CancelImportTask -> TestTree
testCancelImportTask = req
    "CancelImportTask"
    "fixture/CancelImportTask.yaml"

testGetConsoleOutput :: GetConsoleOutput -> TestTree
testGetConsoleOutput = req
    "GetConsoleOutput"
    "fixture/GetConsoleOutput.yaml"

testUnassignPrivateIPAddresses :: UnassignPrivateIPAddresses -> TestTree
testUnassignPrivateIPAddresses = req
    "UnassignPrivateIPAddresses"
    "fixture/UnassignPrivateIPAddresses.yaml"

testDeleteRouteTable :: DeleteRouteTable -> TestTree
testDeleteRouteTable = req
    "DeleteRouteTable"
    "fixture/DeleteRouteTable.yaml"

testDescribeImageAttribute :: DescribeImageAttribute -> TestTree
testDescribeImageAttribute = req
    "DescribeImageAttribute"
    "fixture/DescribeImageAttribute.yaml"

testDeleteDHCPOptions :: DeleteDHCPOptions -> TestTree
testDeleteDHCPOptions = req
    "DeleteDHCPOptions"
    "fixture/DeleteDHCPOptions.yaml"

testDescribeVPNGateways :: DescribeVPNGateways -> TestTree
testDescribeVPNGateways = req
    "DescribeVPNGateways"
    "fixture/DescribeVPNGateways.yaml"

testDetachClassicLinkVPC :: DetachClassicLinkVPC -> TestTree
testDetachClassicLinkVPC = req
    "DetachClassicLinkVPC"
    "fixture/DetachClassicLinkVPC.yaml"

testDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
testDescribeReservedInstancesModifications = req
    "DescribeReservedInstancesModifications"
    "fixture/DescribeReservedInstancesModifications.yaml"

testDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
testDescribeSpotInstanceRequests = req
    "DescribeSpotInstanceRequests"
    "fixture/DescribeSpotInstanceRequests.yaml"

testMonitorInstances :: MonitorInstances -> TestTree
testMonitorInstances = req
    "MonitorInstances"
    "fixture/MonitorInstances.yaml"

testDescribeRegions :: DescribeRegions -> TestTree
testDescribeRegions = req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

testModifyVPCAttribute :: ModifyVPCAttribute -> TestTree
testModifyVPCAttribute = req
    "ModifyVPCAttribute"
    "fixture/ModifyVPCAttribute.yaml"

testDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
testDescribeSpotFleetInstances = req
    "DescribeSpotFleetInstances"
    "fixture/DescribeSpotFleetInstances.yaml"

testDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
testDescribeVolumeStatus = req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus.yaml"

testDeleteVolume :: DeleteVolume -> TestTree
testDeleteVolume = req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

testDescribeImages :: DescribeImages -> TestTree
testDescribeImages = req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

testCreateKeyPair :: CreateKeyPair -> TestTree
testCreateKeyPair = req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

testRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
testRestoreAddressToClassic = req
    "RestoreAddressToClassic"
    "fixture/RestoreAddressToClassic.yaml"

testDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
testDescribeAvailabilityZones = req
    "DescribeAvailabilityZones"
    "fixture/DescribeAvailabilityZones.yaml"

testImportSnapshot :: ImportSnapshot -> TestTree
testImportSnapshot = req
    "ImportSnapshot"
    "fixture/ImportSnapshot.yaml"

testAcceptVPCPeeringConnection :: AcceptVPCPeeringConnection -> TestTree
testAcceptVPCPeeringConnection = req
    "AcceptVPCPeeringConnection"
    "fixture/AcceptVPCPeeringConnection.yaml"

testDescribeRouteTables :: DescribeRouteTables -> TestTree
testDescribeRouteTables = req
    "DescribeRouteTables"
    "fixture/DescribeRouteTables.yaml"

-- Responses

testDetachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse -> TestTree
testDetachNetworkInterfaceResponse = res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    eC2
    (Proxy :: Proxy DetachNetworkInterface)

testDeleteVPCEndpointsResponse :: DeleteVPCEndpointsResponse -> TestTree
testDeleteVPCEndpointsResponse = res
    "DeleteVPCEndpointsResponse"
    "fixture/DeleteVPCEndpointsResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPCEndpoints)

testDeleteKeyPairResponse :: DeleteKeyPairResponse -> TestTree
testDeleteKeyPairResponse = res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    eC2
    (Proxy :: Proxy DeleteKeyPair)

testDeleteFlowLogsResponse :: DeleteFlowLogsResponse -> TestTree
testDeleteFlowLogsResponse = res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    eC2
    (Proxy :: Proxy DeleteFlowLogs)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeTags)

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

testDetachInternetGatewayResponse :: DetachInternetGatewayResponse -> TestTree
testDetachInternetGatewayResponse = res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DetachInternetGateway)

testCreateNetworkACLResponse :: CreateNetworkACLResponse -> TestTree
testCreateNetworkACLResponse = res
    "CreateNetworkACLResponse"
    "fixture/CreateNetworkACLResponse.proto"
    eC2
    (Proxy :: Proxy CreateNetworkACL)

testImportInstanceResponse :: ImportInstanceResponse -> TestTree
testImportInstanceResponse = res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    eC2
    (Proxy :: Proxy ImportInstance)

testDescribeVPCClassicLinkResponse :: DescribeVPCClassicLinkResponse -> TestTree
testDescribeVPCClassicLinkResponse = res
    "DescribeVPCClassicLinkResponse"
    "fixture/DescribeVPCClassicLinkResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCClassicLink)

testDeleteVPNConnectionResponse :: DeleteVPNConnectionResponse -> TestTree
testDeleteVPNConnectionResponse = res
    "DeleteVPNConnectionResponse"
    "fixture/DeleteVPNConnectionResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPNConnection)

testAuthorizeSecurityGroupEgressResponse :: AuthorizeSecurityGroupEgressResponse -> TestTree
testAuthorizeSecurityGroupEgressResponse = res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    eC2
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

testDescribeBundleTasksResponse :: DescribeBundleTasksResponse -> TestTree
testDescribeBundleTasksResponse = res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeBundleTasks)

testCreateInternetGatewayResponse :: CreateInternetGatewayResponse -> TestTree
testCreateInternetGatewayResponse = res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    eC2
    (Proxy :: Proxy CreateInternetGateway)

testReleaseAddressResponse :: ReleaseAddressResponse -> TestTree
testReleaseAddressResponse = res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    eC2
    (Proxy :: Proxy ReleaseAddress)

testCancelBundleTaskResponse :: CancelBundleTaskResponse -> TestTree
testCancelBundleTaskResponse = res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse.proto"
    eC2
    (Proxy :: Proxy CancelBundleTask)

testModifyNetworkInterfaceAttributeResponse :: ModifyNetworkInterfaceAttributeResponse -> TestTree
testModifyNetworkInterfaceAttributeResponse = res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

testModifySubnetAttributeResponse :: ModifySubnetAttributeResponse -> TestTree
testModifySubnetAttributeResponse = res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifySubnetAttribute)

testDeregisterImageResponse :: DeregisterImageResponse -> TestTree
testDeregisterImageResponse = res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse.proto"
    eC2
    (Proxy :: Proxy DeregisterImage)

testDetachVolumeResponse :: VolumeAttachment -> TestTree
testDetachVolumeResponse = res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    eC2
    (Proxy :: Proxy DetachVolume)

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

testCancelSpotFleetRequestsResponse :: CancelSpotFleetRequestsResponse -> TestTree
testCancelSpotFleetRequestsResponse = res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    eC2
    (Proxy :: Proxy CancelSpotFleetRequests)

testDescribeDHCPOptionsResponse :: DescribeDHCPOptionsResponse -> TestTree
testDescribeDHCPOptionsResponse = res
    "DescribeDHCPOptionsResponse"
    "fixture/DescribeDHCPOptionsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeDHCPOptions)

testDescribeSpotPriceHistoryResponse :: DescribeSpotPriceHistoryResponse -> TestTree
testDescribeSpotPriceHistoryResponse = res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotPriceHistory)

testStopInstancesResponse :: StopInstancesResponse -> TestTree
testStopInstancesResponse = res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    eC2
    (Proxy :: Proxy StopInstances)

testImportImageResponse :: ImportImageResponse -> TestTree
testImportImageResponse = res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    eC2
    (Proxy :: Proxy ImportImage)

testDeleteNetworkACLEntryResponse :: DeleteNetworkACLEntryResponse -> TestTree
testDeleteNetworkACLEntryResponse = res
    "DeleteNetworkACLEntryResponse"
    "fixture/DeleteNetworkACLEntryResponse.proto"
    eC2
    (Proxy :: Proxy DeleteNetworkACLEntry)

testDisableVPCClassicLinkResponse :: DisableVPCClassicLinkResponse -> TestTree
testDisableVPCClassicLinkResponse = res
    "DisableVPCClassicLinkResponse"
    "fixture/DisableVPCClassicLinkResponse.proto"
    eC2
    (Proxy :: Proxy DisableVPCClassicLink)

testAuthorizeSecurityGroupIngressResponse :: AuthorizeSecurityGroupIngressResponse -> TestTree
testAuthorizeSecurityGroupIngressResponse = res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    eC2
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

testBundleInstanceResponse :: BundleInstanceResponse -> TestTree
testBundleInstanceResponse = res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    eC2
    (Proxy :: Proxy BundleInstance)

testDescribeVPCEndpointServicesResponse :: DescribeVPCEndpointServicesResponse -> TestTree
testDescribeVPCEndpointServicesResponse = res
    "DescribeVPCEndpointServicesResponse"
    "fixture/DescribeVPCEndpointServicesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCEndpointServices)

testReplaceNetworkACLAssociationResponse :: ReplaceNetworkACLAssociationResponse -> TestTree
testReplaceNetworkACLAssociationResponse = res
    "ReplaceNetworkACLAssociationResponse"
    "fixture/ReplaceNetworkACLAssociationResponse.proto"
    eC2
    (Proxy :: Proxy ReplaceNetworkACLAssociation)

testCreateVPCPeeringConnectionResponse :: CreateVPCPeeringConnectionResponse -> TestTree
testCreateVPCPeeringConnectionResponse = res
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPCPeeringConnection)

testResetSnapshotAttributeResponse :: ResetSnapshotAttributeResponse -> TestTree
testResetSnapshotAttributeResponse = res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ResetSnapshotAttribute)

testDescribeAddressesResponse :: DescribeAddressesResponse -> TestTree
testDescribeAddressesResponse = res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeAddresses)

testDescribeInternetGatewaysResponse :: DescribeInternetGatewaysResponse -> TestTree
testDescribeInternetGatewaysResponse = res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    eC2
    (Proxy :: Proxy DescribeInternetGateways)

testReplaceRouteResponse :: ReplaceRouteResponse -> TestTree
testReplaceRouteResponse = res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    eC2
    (Proxy :: Proxy ReplaceRoute)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    eC2
    (Proxy :: Proxy CreateTags)

testDescribeSubnetsResponse :: DescribeSubnetsResponse -> TestTree
testDescribeSubnetsResponse = res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSubnets)

testDescribeNetworkInterfacesResponse :: DescribeNetworkInterfacesResponse -> TestTree
testDescribeNetworkInterfacesResponse = res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeNetworkInterfaces)

testPurchaseReservedInstancesOfferingResponse :: PurchaseReservedInstancesOfferingResponse -> TestTree
testPurchaseReservedInstancesOfferingResponse = res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    eC2
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

testDescribeSnapshotAttributeResponse :: DescribeSnapshotAttributeResponse -> TestTree
testDescribeSnapshotAttributeResponse = res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSnapshotAttribute)

testCreateCustomerGatewayResponse :: CreateCustomerGatewayResponse -> TestTree
testCreateCustomerGatewayResponse = res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    eC2
    (Proxy :: Proxy CreateCustomerGateway)

testAttachInternetGatewayResponse :: AttachInternetGatewayResponse -> TestTree
testAttachInternetGatewayResponse = res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    eC2
    (Proxy :: Proxy AttachInternetGateway)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    eC2
    (Proxy :: Proxy DeleteTags)

testReplaceNetworkACLEntryResponse :: ReplaceNetworkACLEntryResponse -> TestTree
testReplaceNetworkACLEntryResponse = res
    "ReplaceNetworkACLEntryResponse"
    "fixture/ReplaceNetworkACLEntryResponse.proto"
    eC2
    (Proxy :: Proxy ReplaceNetworkACLEntry)

testResetInstanceAttributeResponse :: ResetInstanceAttributeResponse -> TestTree
testResetInstanceAttributeResponse = res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ResetInstanceAttribute)

testDeleteRouteResponse :: DeleteRouteResponse -> TestTree
testDeleteRouteResponse = res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    eC2
    (Proxy :: Proxy DeleteRoute)

testDescribeVPNConnectionsResponse :: DescribeVPNConnectionsResponse -> TestTree
testDescribeVPNConnectionsResponse = res
    "DescribeVPNConnectionsResponse"
    "fixture/DescribeVPNConnectionsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPNConnections)

testDescribeFlowLogsResponse :: DescribeFlowLogsResponse -> TestTree
testDescribeFlowLogsResponse = res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeFlowLogs)

testDeleteSecurityGroupResponse :: DeleteSecurityGroupResponse -> TestTree
testDeleteSecurityGroupResponse = res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    eC2
    (Proxy :: Proxy DeleteSecurityGroup)

testDescribeReservedInstancesOfferingsResponse :: DescribeReservedInstancesOfferingsResponse -> TestTree
testDescribeReservedInstancesOfferingsResponse = res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

testDeleteVPCPeeringConnectionResponse :: DeleteVPCPeeringConnectionResponse -> TestTree
testDeleteVPCPeeringConnectionResponse = res
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPCPeeringConnection)

testDescribeVPCEndpointsResponse :: DescribeVPCEndpointsResponse -> TestTree
testDescribeVPCEndpointsResponse = res
    "DescribeVPCEndpointsResponse"
    "fixture/DescribeVPCEndpointsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCEndpoints)

testDescribeInstanceAttributeResponse :: DescribeInstanceAttributeResponse -> TestTree
testDescribeInstanceAttributeResponse = res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeInstanceAttribute)

testConfirmProductInstanceResponse :: ConfirmProductInstanceResponse -> TestTree
testConfirmProductInstanceResponse = res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    eC2
    (Proxy :: Proxy ConfirmProductInstance)

testImportKeyPairResponse :: ImportKeyPairResponse -> TestTree
testImportKeyPairResponse = res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    eC2
    (Proxy :: Proxy ImportKeyPair)

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

testCancelConversionTaskResponse :: CancelConversionTaskResponse -> TestTree
testCancelConversionTaskResponse = res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse.proto"
    eC2
    (Proxy :: Proxy CancelConversionTask)

testReportInstanceStatusResponse :: ReportInstanceStatusResponse -> TestTree
testReportInstanceStatusResponse = res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    eC2
    (Proxy :: Proxy ReportInstanceStatus)

testAssociateDHCPOptionsResponse :: AssociateDHCPOptionsResponse -> TestTree
testAssociateDHCPOptionsResponse = res
    "AssociateDHCPOptionsResponse"
    "fixture/AssociateDHCPOptionsResponse.proto"
    eC2
    (Proxy :: Proxy AssociateDHCPOptions)

testDescribeVPCsResponse :: DescribeVPCsResponse -> TestTree
testDescribeVPCsResponse = res
    "DescribeVPCsResponse"
    "fixture/DescribeVPCsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCs)

testRequestSpotInstancesResponse :: RequestSpotInstancesResponse -> TestTree
testRequestSpotInstancesResponse = res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    eC2
    (Proxy :: Proxy RequestSpotInstances)

testModifyImageAttributeResponse :: ModifyImageAttributeResponse -> TestTree
testModifyImageAttributeResponse = res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyImageAttribute)

testDescribeReservedInstancesResponse :: DescribeReservedInstancesResponse -> TestTree
testDescribeReservedInstancesResponse = res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeReservedInstances)

testAllocateAddressResponse :: AllocateAddressResponse -> TestTree
testAllocateAddressResponse = res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    eC2
    (Proxy :: Proxy AllocateAddress)

testRunInstancesResponse :: Reservation -> TestTree
testRunInstancesResponse = res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    eC2
    (Proxy :: Proxy RunInstances)

testCreateRouteTableResponse :: CreateRouteTableResponse -> TestTree
testCreateRouteTableResponse = res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    eC2
    (Proxy :: Proxy CreateRouteTable)

testAttachVolumeResponse :: VolumeAttachment -> TestTree
testAttachVolumeResponse = res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    eC2
    (Proxy :: Proxy AttachVolume)

testDescribeConversionTasksResponse :: DescribeConversionTasksResponse -> TestTree
testDescribeConversionTasksResponse = res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeConversionTasks)

testRejectVPCPeeringConnectionResponse :: RejectVPCPeeringConnectionResponse -> TestTree
testRejectVPCPeeringConnectionResponse = res
    "RejectVPCPeeringConnectionResponse"
    "fixture/RejectVPCPeeringConnectionResponse.proto"
    eC2
    (Proxy :: Proxy RejectVPCPeeringConnection)

testRevokeSecurityGroupIngressResponse :: RevokeSecurityGroupIngressResponse -> TestTree
testRevokeSecurityGroupIngressResponse = res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    eC2
    (Proxy :: Proxy RevokeSecurityGroupIngress)

testDescribeVolumesResponse :: DescribeVolumesResponse -> TestTree
testDescribeVolumesResponse = res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVolumes)

testDeleteVPNConnectionRouteResponse :: DeleteVPNConnectionRouteResponse -> TestTree
testDeleteVPNConnectionRouteResponse = res
    "DeleteVPNConnectionRouteResponse"
    "fixture/DeleteVPNConnectionRouteResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPNConnectionRoute)

testModifyReservedInstancesResponse :: ModifyReservedInstancesResponse -> TestTree
testModifyReservedInstancesResponse = res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    eC2
    (Proxy :: Proxy ModifyReservedInstances)

testRegisterImageResponse :: RegisterImageResponse -> TestTree
testRegisterImageResponse = res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    eC2
    (Proxy :: Proxy RegisterImage)

testModifyVPCEndpointResponse :: ModifyVPCEndpointResponse -> TestTree
testModifyVPCEndpointResponse = res
    "ModifyVPCEndpointResponse"
    "fixture/ModifyVPCEndpointResponse.proto"
    eC2
    (Proxy :: Proxy ModifyVPCEndpoint)

testDeleteVPNGatewayResponse :: DeleteVPNGatewayResponse -> TestTree
testDeleteVPNGatewayResponse = res
    "DeleteVPNGatewayResponse"
    "fixture/DeleteVPNGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVPNGateway)

testCreateVPCResponse :: CreateVPCResponse -> TestTree
testCreateVPCResponse = res
    "CreateVPCResponse"
    "fixture/CreateVPCResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPC)

testDescribeMovingAddressesResponse :: DescribeMovingAddressesResponse -> TestTree
testDescribeMovingAddressesResponse = res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeMovingAddresses)

testDescribeVolumeAttributeResponse :: DescribeVolumeAttributeResponse -> TestTree
testDescribeVolumeAttributeResponse = res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVolumeAttribute)

testMoveAddressToVPCResponse :: MoveAddressToVPCResponse -> TestTree
testMoveAddressToVPCResponse = res
    "MoveAddressToVPCResponse"
    "fixture/MoveAddressToVPCResponse.proto"
    eC2
    (Proxy :: Proxy MoveAddressToVPC)

testGetPasswordDataResponse :: GetPasswordDataResponse -> TestTree
testGetPasswordDataResponse = res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse.proto"
    eC2
    (Proxy :: Proxy GetPasswordData)

testCreateFlowLogsResponse :: CreateFlowLogsResponse -> TestTree
testCreateFlowLogsResponse = res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    eC2
    (Proxy :: Proxy CreateFlowLogs)

testDescribeImportImageTasksResponse :: DescribeImportImageTasksResponse -> TestTree
testDescribeImportImageTasksResponse = res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeImportImageTasks)

testDeleteNetworkACLResponse :: DeleteNetworkACLResponse -> TestTree
testDeleteNetworkACLResponse = res
    "DeleteNetworkACLResponse"
    "fixture/DeleteNetworkACLResponse.proto"
    eC2
    (Proxy :: Proxy DeleteNetworkACL)

testDescribeSpotFleetRequestsResponse :: DescribeSpotFleetRequestsResponse -> TestTree
testDescribeSpotFleetRequestsResponse = res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotFleetRequests)

testCopySnapshotResponse :: CopySnapshotResponse -> TestTree
testCopySnapshotResponse = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    eC2
    (Proxy :: Proxy CopySnapshot)

testModifyVolumeAttributeResponse :: ModifyVolumeAttributeResponse -> TestTree
testModifyVolumeAttributeResponse = res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyVolumeAttribute)

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

testDescribePrefixListsResponse :: DescribePrefixListsResponse -> TestTree
testDescribePrefixListsResponse = res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    eC2
    (Proxy :: Proxy DescribePrefixLists)

testCreateInstanceExportTaskResponse :: CreateInstanceExportTaskResponse -> TestTree
testCreateInstanceExportTaskResponse = res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    eC2
    (Proxy :: Proxy CreateInstanceExportTask)

testDescribeSpotDatafeedSubscriptionResponse :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
testDescribeSpotDatafeedSubscriptionResponse = res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

testDetachVPNGatewayResponse :: DetachVPNGatewayResponse -> TestTree
testDetachVPNGatewayResponse = res
    "DetachVPNGatewayResponse"
    "fixture/DetachVPNGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DetachVPNGateway)

testDescribeExportTasksResponse :: DescribeExportTasksResponse -> TestTree
testDescribeExportTasksResponse = res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeExportTasks)

testDeletePlacementGroupResponse :: DeletePlacementGroupResponse -> TestTree
testDeletePlacementGroupResponse = res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    eC2
    (Proxy :: Proxy DeletePlacementGroup)

testCreateSubnetResponse :: CreateSubnetResponse -> TestTree
testCreateSubnetResponse = res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    eC2
    (Proxy :: Proxy CreateSubnet)

testEnableVolumeIOResponse :: EnableVolumeIOResponse -> TestTree
testEnableVolumeIOResponse = res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    eC2
    (Proxy :: Proxy EnableVolumeIO)

testCancelExportTaskResponse :: CancelExportTaskResponse -> TestTree
testCancelExportTaskResponse = res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    eC2
    (Proxy :: Proxy CancelExportTask)

testRequestSpotFleetResponse :: RequestSpotFleetResponse -> TestTree
testRequestSpotFleetResponse = res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    eC2
    (Proxy :: Proxy RequestSpotFleet)

testDescribeInstancesResponse :: DescribeInstancesResponse -> TestTree
testDescribeInstancesResponse = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeInstances)

testDescribeSecurityGroupsResponse :: DescribeSecurityGroupsResponse -> TestTree
testDescribeSecurityGroupsResponse = res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSecurityGroups)

testDescribeVPCPeeringConnectionsResponse :: DescribeVPCPeeringConnectionsResponse -> TestTree
testDescribeVPCPeeringConnectionsResponse = res
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPCPeeringConnections)

testCreateNetworkInterfaceResponse :: CreateNetworkInterfaceResponse -> TestTree
testCreateNetworkInterfaceResponse = res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse.proto"
    eC2
    (Proxy :: Proxy CreateNetworkInterface)

testAssociateAddressResponse :: AssociateAddressResponse -> TestTree
testAssociateAddressResponse = res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    eC2
    (Proxy :: Proxy AssociateAddress)

testStartInstancesResponse :: StartInstancesResponse -> TestTree
testStartInstancesResponse = res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    eC2
    (Proxy :: Proxy StartInstances)

testDescribeCustomerGatewaysResponse :: DescribeCustomerGatewaysResponse -> TestTree
testDescribeCustomerGatewaysResponse = res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    eC2
    (Proxy :: Proxy DescribeCustomerGateways)

testResetNetworkInterfaceAttributeResponse :: ResetNetworkInterfaceAttributeResponse -> TestTree
testResetNetworkInterfaceAttributeResponse = res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

testCreateVPNConnectionResponse :: CreateVPNConnectionResponse -> TestTree
testCreateVPNConnectionResponse = res
    "CreateVPNConnectionResponse"
    "fixture/CreateVPNConnectionResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPNConnection)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSnapshots)

testCreatePlacementGroupResponse :: CreatePlacementGroupResponse -> TestTree
testCreatePlacementGroupResponse = res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    eC2
    (Proxy :: Proxy CreatePlacementGroup)

testReplaceRouteTableAssociationResponse :: ReplaceRouteTableAssociationResponse -> TestTree
testReplaceRouteTableAssociationResponse = res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    eC2
    (Proxy :: Proxy ReplaceRouteTableAssociation)

testDescribeNetworkInterfaceAttributeResponse :: DescribeNetworkInterfaceAttributeResponse -> TestTree
testDescribeNetworkInterfaceAttributeResponse = res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

testDescribeReservedInstancesListingsResponse :: DescribeReservedInstancesListingsResponse -> TestTree
testDescribeReservedInstancesListingsResponse = res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeReservedInstancesListings)

testDeleteNetworkInterfaceResponse :: DeleteNetworkInterfaceResponse -> TestTree
testDeleteNetworkInterfaceResponse = res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    eC2
    (Proxy :: Proxy DeleteNetworkInterface)

testDeleteInternetGatewayResponse :: DeleteInternetGatewayResponse -> TestTree
testDeleteInternetGatewayResponse = res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DeleteInternetGateway)

testDeleteSubnetResponse :: DeleteSubnetResponse -> TestTree
testDeleteSubnetResponse = res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    eC2
    (Proxy :: Proxy DeleteSubnet)

testCreateVPCEndpointResponse :: CreateVPCEndpointResponse -> TestTree
testCreateVPCEndpointResponse = res
    "CreateVPCEndpointResponse"
    "fixture/CreateVPCEndpointResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPCEndpoint)

testDescribeImportSnapshotTasksResponse :: DescribeImportSnapshotTasksResponse -> TestTree
testDescribeImportSnapshotTasksResponse = res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    eC2
    (Proxy :: Proxy DescribeImportSnapshotTasks)

testCopyImageResponse :: CopyImageResponse -> TestTree
testCopyImageResponse = res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    eC2
    (Proxy :: Proxy CopyImage)

testDisassociateRouteTableResponse :: DisassociateRouteTableResponse -> TestTree
testDisassociateRouteTableResponse = res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    eC2
    (Proxy :: Proxy DisassociateRouteTable)

testUnmonitorInstancesResponse :: UnmonitorInstancesResponse -> TestTree
testUnmonitorInstancesResponse = res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    eC2
    (Proxy :: Proxy UnmonitorInstances)

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

testAssignPrivateIPAddressesResponse :: AssignPrivateIPAddressesResponse -> TestTree
testAssignPrivateIPAddressesResponse = res
    "AssignPrivateIPAddressesResponse"
    "fixture/AssignPrivateIPAddressesResponse.proto"
    eC2
    (Proxy :: Proxy AssignPrivateIPAddresses)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    eC2
    (Proxy :: Proxy DeleteSnapshot)

testDeleteCustomerGatewayResponse :: DeleteCustomerGatewayResponse -> TestTree
testDeleteCustomerGatewayResponse = res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse.proto"
    eC2
    (Proxy :: Proxy DeleteCustomerGateway)

testModifyInstanceAttributeResponse :: ModifyInstanceAttributeResponse -> TestTree
testModifyInstanceAttributeResponse = res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyInstanceAttribute)

testCreateSecurityGroupResponse :: CreateSecurityGroupResponse -> TestTree
testCreateSecurityGroupResponse = res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    eC2
    (Proxy :: Proxy CreateSecurityGroup)

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

testCreateNetworkACLEntryResponse :: CreateNetworkACLEntryResponse -> TestTree
testCreateNetworkACLEntryResponse = res
    "CreateNetworkACLEntryResponse"
    "fixture/CreateNetworkACLEntryResponse.proto"
    eC2
    (Proxy :: Proxy CreateNetworkACLEntry)

testModifySnapshotAttributeResponse :: ModifySnapshotAttributeResponse -> TestTree
testModifySnapshotAttributeResponse = res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifySnapshotAttribute)

testEnableVGWRoutePropagationResponse :: EnableVGWRoutePropagationResponse -> TestTree
testEnableVGWRoutePropagationResponse = res
    "EnableVGWRoutePropagationResponse"
    "fixture/EnableVGWRoutePropagationResponse.proto"
    eC2
    (Proxy :: Proxy EnableVGWRoutePropagation)

testCreateSnapshotResponse :: Snapshot -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    eC2
    (Proxy :: Proxy CreateSnapshot)

testDescribeSpotFleetRequestHistoryResponse :: DescribeSpotFleetRequestHistoryResponse -> TestTree
testDescribeSpotFleetRequestHistoryResponse = res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

testDeleteSpotDatafeedSubscriptionResponse :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
testDeleteSpotDatafeedSubscriptionResponse = res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    eC2
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

testDescribePlacementGroupsResponse :: DescribePlacementGroupsResponse -> TestTree
testDescribePlacementGroupsResponse = res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    eC2
    (Proxy :: Proxy DescribePlacementGroups)

testCreateReservedInstancesListingResponse :: CreateReservedInstancesListingResponse -> TestTree
testCreateReservedInstancesListingResponse = res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    eC2
    (Proxy :: Proxy CreateReservedInstancesListing)

testEnableVPCClassicLinkResponse :: EnableVPCClassicLinkResponse -> TestTree
testEnableVPCClassicLinkResponse = res
    "EnableVPCClassicLinkResponse"
    "fixture/EnableVPCClassicLinkResponse.proto"
    eC2
    (Proxy :: Proxy EnableVPCClassicLink)

testDescribeKeyPairsResponse :: DescribeKeyPairsResponse -> TestTree
testDescribeKeyPairsResponse = res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeKeyPairs)

testRebootInstancesResponse :: RebootInstancesResponse -> TestTree
testRebootInstancesResponse = res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    eC2
    (Proxy :: Proxy RebootInstances)

testAttachVPNGatewayResponse :: AttachVPNGatewayResponse -> TestTree
testAttachVPNGatewayResponse = res
    "AttachVPNGatewayResponse"
    "fixture/AttachVPNGatewayResponse.proto"
    eC2
    (Proxy :: Proxy AttachVPNGateway)

testCreateVPNConnectionRouteResponse :: CreateVPNConnectionRouteResponse -> TestTree
testCreateVPNConnectionRouteResponse = res
    "CreateVPNConnectionRouteResponse"
    "fixture/CreateVPNConnectionRouteResponse.proto"
    eC2
    (Proxy :: Proxy CreateVPNConnectionRoute)

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

testCreateDHCPOptionsResponse :: CreateDHCPOptionsResponse -> TestTree
testCreateDHCPOptionsResponse = res
    "CreateDHCPOptionsResponse"
    "fixture/CreateDHCPOptionsResponse.proto"
    eC2
    (Proxy :: Proxy CreateDHCPOptions)

testAssociateRouteTableResponse :: AssociateRouteTableResponse -> TestTree
testAssociateRouteTableResponse = res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    eC2
    (Proxy :: Proxy AssociateRouteTable)

testCreateImageResponse :: CreateImageResponse -> TestTree
testCreateImageResponse = res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    eC2
    (Proxy :: Proxy CreateImage)

testDescribeAccountAttributesResponse :: DescribeAccountAttributesResponse -> TestTree
testDescribeAccountAttributesResponse = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeAccountAttributes)

testResetImageAttributeResponse :: ResetImageAttributeResponse -> TestTree
testResetImageAttributeResponse = res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ResetImageAttribute)

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

testGetConsoleOutputResponse :: GetConsoleOutputResponse -> TestTree
testGetConsoleOutputResponse = res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    eC2
    (Proxy :: Proxy GetConsoleOutput)

testUnassignPrivateIPAddressesResponse :: UnassignPrivateIPAddressesResponse -> TestTree
testUnassignPrivateIPAddressesResponse = res
    "UnassignPrivateIPAddressesResponse"
    "fixture/UnassignPrivateIPAddressesResponse.proto"
    eC2
    (Proxy :: Proxy UnassignPrivateIPAddresses)

testDeleteRouteTableResponse :: DeleteRouteTableResponse -> TestTree
testDeleteRouteTableResponse = res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    eC2
    (Proxy :: Proxy DeleteRouteTable)

testDescribeImageAttributeResponse :: DescribeImageAttributeResponse -> TestTree
testDescribeImageAttributeResponse = res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    eC2
    (Proxy :: Proxy DescribeImageAttribute)

testDeleteDHCPOptionsResponse :: DeleteDHCPOptionsResponse -> TestTree
testDeleteDHCPOptionsResponse = res
    "DeleteDHCPOptionsResponse"
    "fixture/DeleteDHCPOptionsResponse.proto"
    eC2
    (Proxy :: Proxy DeleteDHCPOptions)

testDescribeVPNGatewaysResponse :: DescribeVPNGatewaysResponse -> TestTree
testDescribeVPNGatewaysResponse = res
    "DescribeVPNGatewaysResponse"
    "fixture/DescribeVPNGatewaysResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVPNGateways)

testDetachClassicLinkVPCResponse :: DetachClassicLinkVPCResponse -> TestTree
testDetachClassicLinkVPCResponse = res
    "DetachClassicLinkVPCResponse"
    "fixture/DetachClassicLinkVPCResponse.proto"
    eC2
    (Proxy :: Proxy DetachClassicLinkVPC)

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

testMonitorInstancesResponse :: MonitorInstancesResponse -> TestTree
testMonitorInstancesResponse = res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    eC2
    (Proxy :: Proxy MonitorInstances)

testDescribeRegionsResponse :: DescribeRegionsResponse -> TestTree
testDescribeRegionsResponse = res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    eC2
    (Proxy :: Proxy DescribeRegions)

testModifyVPCAttributeResponse :: ModifyVPCAttributeResponse -> TestTree
testModifyVPCAttributeResponse = res
    "ModifyVPCAttributeResponse"
    "fixture/ModifyVPCAttributeResponse.proto"
    eC2
    (Proxy :: Proxy ModifyVPCAttribute)

testDescribeSpotFleetInstancesResponse :: DescribeSpotFleetInstancesResponse -> TestTree
testDescribeSpotFleetInstancesResponse = res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeSpotFleetInstances)

testDescribeVolumeStatusResponse :: DescribeVolumeStatusResponse -> TestTree
testDescribeVolumeStatusResponse = res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    eC2
    (Proxy :: Proxy DescribeVolumeStatus)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    eC2
    (Proxy :: Proxy DeleteVolume)

testDescribeImagesResponse :: DescribeImagesResponse -> TestTree
testDescribeImagesResponse = res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeImages)

testCreateKeyPairResponse :: CreateKeyPairResponse -> TestTree
testCreateKeyPairResponse = res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    eC2
    (Proxy :: Proxy CreateKeyPair)

testRestoreAddressToClassicResponse :: RestoreAddressToClassicResponse -> TestTree
testRestoreAddressToClassicResponse = res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    eC2
    (Proxy :: Proxy RestoreAddressToClassic)

testDescribeAvailabilityZonesResponse :: DescribeAvailabilityZonesResponse -> TestTree
testDescribeAvailabilityZonesResponse = res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeAvailabilityZones)

testImportSnapshotResponse :: ImportSnapshotResponse -> TestTree
testImportSnapshotResponse = res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    eC2
    (Proxy :: Proxy ImportSnapshot)

testAcceptVPCPeeringConnectionResponse :: AcceptVPCPeeringConnectionResponse -> TestTree
testAcceptVPCPeeringConnectionResponse = res
    "AcceptVPCPeeringConnectionResponse"
    "fixture/AcceptVPCPeeringConnectionResponse.proto"
    eC2
    (Proxy :: Proxy AcceptVPCPeeringConnection)

testDescribeRouteTablesResponse :: DescribeRouteTablesResponse -> TestTree
testDescribeRouteTablesResponse = res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    eC2
    (Proxy :: Proxy DescribeRouteTables)
