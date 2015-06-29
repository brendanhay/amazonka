-- Module      : Test.AWS.Gen.EC2
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ detachNetworkInterfaceTest $
--             detachNetworkInterface
--
--         , deleteVPCEndpointsTest $
--             deleteVPCEndpoints
--
--         , deleteKeyPairTest $
--             deleteKeyPair
--
--         , deleteFlowLogsTest $
--             deleteFlowLogs
--
--         , describeTagsTest $
--             describeTags
--
--         , revokeSecurityGroupEgressTest $
--             revokeSecurityGroupEgress
--
--         , createVPNGatewayTest $
--             createVPNGateway
--
--         , detachInternetGatewayTest $
--             detachInternetGateway
--
--         , createNetworkACLTest $
--             createNetworkACL
--
--         , importInstanceTest $
--             importInstance
--
--         , describeVPCClassicLinkTest $
--             describeVPCClassicLink
--
--         , deleteVPNConnectionTest $
--             deleteVPNConnection
--
--         , authorizeSecurityGroupEgressTest $
--             authorizeSecurityGroupEgress
--
--         , describeBundleTasksTest $
--             describeBundleTasks
--
--         , createInternetGatewayTest $
--             createInternetGateway
--
--         , releaseAddressTest $
--             releaseAddress
--
--         , cancelBundleTaskTest $
--             cancelBundleTask
--
--         , modifyNetworkInterfaceAttributeTest $
--             modifyNetworkInterfaceAttribute
--
--         , modifySubnetAttributeTest $
--             modifySubnetAttribute
--
--         , deregisterImageTest $
--             deregisterImage
--
--         , detachVolumeTest $
--             detachVolume
--
--         , cancelReservedInstancesListingTest $
--             cancelReservedInstancesListing
--
--         , attachClassicLinkVPCTest $
--             attachClassicLinkVPC
--
--         , cancelSpotFleetRequestsTest $
--             cancelSpotFleetRequests
--
--         , describeDHCPOptionsTest $
--             describeDHCPOptions
--
--         , describeSpotPriceHistoryTest $
--             describeSpotPriceHistory
--
--         , stopInstancesTest $
--             stopInstances
--
--         , importImageTest $
--             importImage
--
--         , deleteNetworkACLEntryTest $
--             deleteNetworkACLEntry
--
--         , disableVPCClassicLinkTest $
--             disableVPCClassicLink
--
--         , authorizeSecurityGroupIngressTest $
--             authorizeSecurityGroupIngress
--
--         , bundleInstanceTest $
--             bundleInstance
--
--         , describeVPCEndpointServicesTest $
--             describeVPCEndpointServices
--
--         , replaceNetworkACLAssociationTest $
--             replaceNetworkACLAssociation
--
--         , createVPCPeeringConnectionTest $
--             createVPCPeeringConnection
--
--         , resetSnapshotAttributeTest $
--             resetSnapshotAttribute
--
--         , describeAddressesTest $
--             describeAddresses
--
--         , describeInternetGatewaysTest $
--             describeInternetGateways
--
--         , replaceRouteTest $
--             replaceRoute
--
--         , createTagsTest $
--             createTags
--
--         , describeSubnetsTest $
--             describeSubnets
--
--         , describeNetworkInterfacesTest $
--             describeNetworkInterfaces
--
--         , purchaseReservedInstancesOfferingTest $
--             purchaseReservedInstancesOffering
--
--         , describeSnapshotAttributeTest $
--             describeSnapshotAttribute
--
--         , createCustomerGatewayTest $
--             createCustomerGateway
--
--         , attachInternetGatewayTest $
--             attachInternetGateway
--
--         , deleteTagsTest $
--             deleteTags
--
--         , replaceNetworkACLEntryTest $
--             replaceNetworkACLEntry
--
--         , resetInstanceAttributeTest $
--             resetInstanceAttribute
--
--         , deleteRouteTest $
--             deleteRoute
--
--         , describeVPNConnectionsTest $
--             describeVPNConnections
--
--         , describeFlowLogsTest $
--             describeFlowLogs
--
--         , deleteSecurityGroupTest $
--             deleteSecurityGroup
--
--         , describeReservedInstancesOfferingsTest $
--             describeReservedInstancesOfferings
--
--         , deleteVPCPeeringConnectionTest $
--             deleteVPCPeeringConnection
--
--         , describeVPCEndpointsTest $
--             describeVPCEndpoints
--
--         , describeInstanceAttributeTest $
--             describeInstanceAttribute
--
--         , confirmProductInstanceTest $
--             confirmProductInstance
--
--         , importKeyPairTest $
--             importKeyPair
--
--         , attachNetworkInterfaceTest $
--             attachNetworkInterface
--
--         , describeInstanceStatusTest $
--             describeInstanceStatus
--
--         , cancelConversionTaskTest $
--             cancelConversionTask
--
--         , reportInstanceStatusTest $
--             reportInstanceStatus
--
--         , associateDHCPOptionsTest $
--             associateDHCPOptions
--
--         , describeVPCsTest $
--             describeVPCs
--
--         , requestSpotInstancesTest $
--             requestSpotInstances
--
--         , modifyImageAttributeTest $
--             modifyImageAttribute
--
--         , describeReservedInstancesTest $
--             describeReservedInstances
--
--         , allocateAddressTest $
--             allocateAddress
--
--         , runInstancesTest $
--             runInstances
--
--         , createRouteTableTest $
--             createRouteTable
--
--         , attachVolumeTest $
--             attachVolume
--
--         , describeConversionTasksTest $
--             describeConversionTasks
--
--         , rejectVPCPeeringConnectionTest $
--             rejectVPCPeeringConnection
--
--         , revokeSecurityGroupIngressTest $
--             revokeSecurityGroupIngress
--
--         , describeVolumesTest $
--             describeVolumes
--
--         , deleteVPNConnectionRouteTest $
--             deleteVPNConnectionRoute
--
--         , modifyReservedInstancesTest $
--             modifyReservedInstances
--
--         , registerImageTest $
--             registerImage
--
--         , modifyVPCEndpointTest $
--             modifyVPCEndpoint
--
--         , deleteVPNGatewayTest $
--             deleteVPNGateway
--
--         , createVPCTest $
--             createVPC
--
--         , describeMovingAddressesTest $
--             describeMovingAddresses
--
--         , describeVolumeAttributeTest $
--             describeVolumeAttribute
--
--         , moveAddressToVPCTest $
--             moveAddressToVPC
--
--         , getPasswordDataTest $
--             getPasswordData
--
--         , createFlowLogsTest $
--             createFlowLogs
--
--         , describeImportImageTasksTest $
--             describeImportImageTasks
--
--         , deleteNetworkACLTest $
--             deleteNetworkACL
--
--         , describeSpotFleetRequestsTest $
--             describeSpotFleetRequests
--
--         , copySnapshotTest $
--             copySnapshot
--
--         , modifyVolumeAttributeTest $
--             modifyVolumeAttribute
--
--         , describeVPCAttributeTest $
--             describeVPCAttribute
--
--         , createVolumeTest $
--             createVolume
--
--         , disassociateAddressTest $
--             disassociateAddress
--
--         , deleteVPCTest $
--             deleteVPC
--
--         , describePrefixListsTest $
--             describePrefixLists
--
--         , createInstanceExportTaskTest $
--             createInstanceExportTask
--
--         , describeSpotDatafeedSubscriptionTest $
--             describeSpotDatafeedSubscription
--
--         , detachVPNGatewayTest $
--             detachVPNGateway
--
--         , describeExportTasksTest $
--             describeExportTasks
--
--         , deletePlacementGroupTest $
--             deletePlacementGroup
--
--         , createSubnetTest $
--             createSubnet
--
--         , enableVolumeIOTest $
--             enableVolumeIO
--
--         , cancelExportTaskTest $
--             cancelExportTask
--
--         , requestSpotFleetTest $
--             requestSpotFleet
--
--         , describeInstancesTest $
--             describeInstances
--
--         , describeSecurityGroupsTest $
--             describeSecurityGroups
--
--         , describeVPCPeeringConnectionsTest $
--             describeVPCPeeringConnections
--
--         , createNetworkInterfaceTest $
--             createNetworkInterface
--
--         , associateAddressTest $
--             associateAddress
--
--         , startInstancesTest $
--             startInstances
--
--         , describeCustomerGatewaysTest $
--             describeCustomerGateways
--
--         , resetNetworkInterfaceAttributeTest $
--             resetNetworkInterfaceAttribute
--
--         , createVPNConnectionTest $
--             createVPNConnection
--
--         , describeSnapshotsTest $
--             describeSnapshots
--
--         , createPlacementGroupTest $
--             createPlacementGroup
--
--         , replaceRouteTableAssociationTest $
--             replaceRouteTableAssociation
--
--         , describeNetworkInterfaceAttributeTest $
--             describeNetworkInterfaceAttribute
--
--         , describeReservedInstancesListingsTest $
--             describeReservedInstancesListings
--
--         , deleteNetworkInterfaceTest $
--             deleteNetworkInterface
--
--         , deleteInternetGatewayTest $
--             deleteInternetGateway
--
--         , deleteSubnetTest $
--             deleteSubnet
--
--         , createVPCEndpointTest $
--             createVPCEndpoint
--
--         , describeImportSnapshotTasksTest $
--             describeImportSnapshotTasks
--
--         , copyImageTest $
--             copyImage
--
--         , disassociateRouteTableTest $
--             disassociateRouteTable
--
--         , unmonitorInstancesTest $
--             unmonitorInstances
--
--         , importVolumeTest $
--             importVolume
--
--         , disableVGWRoutePropagationTest $
--             disableVGWRoutePropagation
--
--         , createSpotDatafeedSubscriptionTest $
--             createSpotDatafeedSubscription
--
--         , assignPrivateIPAddressesTest $
--             assignPrivateIPAddresses
--
--         , deleteSnapshotTest $
--             deleteSnapshot
--
--         , deleteCustomerGatewayTest $
--             deleteCustomerGateway
--
--         , modifyInstanceAttributeTest $
--             modifyInstanceAttribute
--
--         , createSecurityGroupTest $
--             createSecurityGroup
--
--         , cancelSpotInstanceRequestsTest $
--             cancelSpotInstanceRequests
--
--         , createRouteTest $
--             createRoute
--
--         , createNetworkACLEntryTest $
--             createNetworkACLEntry
--
--         , modifySnapshotAttributeTest $
--             modifySnapshotAttribute
--
--         , enableVGWRoutePropagationTest $
--             enableVGWRoutePropagation
--
--         , createSnapshotTest $
--             createSnapshot
--
--         , describeSpotFleetRequestHistoryTest $
--             describeSpotFleetRequestHistory
--
--         , deleteSpotDatafeedSubscriptionTest $
--             deleteSpotDatafeedSubscription
--
--         , describePlacementGroupsTest $
--             describePlacementGroups
--
--         , createReservedInstancesListingTest $
--             createReservedInstancesListing
--
--         , enableVPCClassicLinkTest $
--             enableVPCClassicLink
--
--         , describeKeyPairsTest $
--             describeKeyPairs
--
--         , rebootInstancesTest $
--             rebootInstances
--
--         , attachVPNGatewayTest $
--             attachVPNGateway
--
--         , createVPNConnectionRouteTest $
--             createVPNConnectionRoute
--
--         , describeClassicLinkInstancesTest $
--             describeClassicLinkInstances
--
--         , terminateInstancesTest $
--             terminateInstances
--
--         , createDHCPOptionsTest $
--             createDHCPOptions
--
--         , associateRouteTableTest $
--             associateRouteTable
--
--         , createImageTest $
--             createImage
--
--         , describeAccountAttributesTest $
--             describeAccountAttributes
--
--         , resetImageAttributeTest $
--             resetImageAttribute
--
--         , describeNetworkACLsTest $
--             describeNetworkACLs
--
--         , cancelImportTaskTest $
--             cancelImportTask
--
--         , getConsoleOutputTest $
--             getConsoleOutput
--
--         , unassignPrivateIPAddressesTest $
--             unassignPrivateIPAddresses
--
--         , deleteRouteTableTest $
--             deleteRouteTable
--
--         , describeImageAttributeTest $
--             describeImageAttribute
--
--         , deleteDHCPOptionsTest $
--             deleteDHCPOptions
--
--         , describeVPNGatewaysTest $
--             describeVPNGateways
--
--         , detachClassicLinkVPCTest $
--             detachClassicLinkVPC
--
--         , describeReservedInstancesModificationsTest $
--             describeReservedInstancesModifications
--
--         , describeSpotInstanceRequestsTest $
--             describeSpotInstanceRequests
--
--         , monitorInstancesTest $
--             monitorInstances
--
--         , describeRegionsTest $
--             describeRegions
--
--         , modifyVPCAttributeTest $
--             modifyVPCAttribute
--
--         , describeSpotFleetInstancesTest $
--             describeSpotFleetInstances
--
--         , describeVolumeStatusTest $
--             describeVolumeStatus
--
--         , deleteVolumeTest $
--             deleteVolume
--
--         , describeImagesTest $
--             describeImages
--
--         , createKeyPairTest $
--             createKeyPair
--
--         , restoreAddressToClassicTest $
--             restoreAddressToClassic
--
--         , describeAvailabilityZonesTest $
--             describeAvailabilityZones
--
--         , importSnapshotTest $
--             importSnapshot
--
--         , acceptVPCPeeringConnectionTest $
--             acceptVPCPeeringConnection
--
--         , describeRouteTablesTest $
--             describeRouteTables
--
--           ]

--     , testGroup "response"
--         [ detachNetworkInterfaceResponseTest $
--             detachNetworkInterfaceResponse
--
--         , deleteVPCEndpointsResponseTest $
--             deleteVPCEndpointsResponse
--
--         , deleteKeyPairResponseTest $
--             deleteKeyPairResponse
--
--         , deleteFlowLogsResponseTest $
--             deleteFlowLogsResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , revokeSecurityGroupEgressResponseTest $
--             revokeSecurityGroupEgressResponse
--
--         , createVPNGatewayResponseTest $
--             createVPNGatewayResponse
--
--         , detachInternetGatewayResponseTest $
--             detachInternetGatewayResponse
--
--         , createNetworkACLResponseTest $
--             createNetworkACLResponse
--
--         , importInstanceResponseTest $
--             importInstanceResponse
--
--         , describeVPCClassicLinkResponseTest $
--             describeVPCClassicLinkResponse
--
--         , deleteVPNConnectionResponseTest $
--             deleteVPNConnectionResponse
--
--         , authorizeSecurityGroupEgressResponseTest $
--             authorizeSecurityGroupEgressResponse
--
--         , describeBundleTasksResponseTest $
--             describeBundleTasksResponse
--
--         , createInternetGatewayResponseTest $
--             createInternetGatewayResponse
--
--         , releaseAddressResponseTest $
--             releaseAddressResponse
--
--         , cancelBundleTaskResponseTest $
--             cancelBundleTaskResponse
--
--         , modifyNetworkInterfaceAttributeResponseTest $
--             modifyNetworkInterfaceAttributeResponse
--
--         , modifySubnetAttributeResponseTest $
--             modifySubnetAttributeResponse
--
--         , deregisterImageResponseTest $
--             deregisterImageResponse
--
--         , volumeAttachmentTest $
--             volumeAttachment
--
--         , cancelReservedInstancesListingResponseTest $
--             cancelReservedInstancesListingResponse
--
--         , attachClassicLinkVPCResponseTest $
--             attachClassicLinkVPCResponse
--
--         , cancelSpotFleetRequestsResponseTest $
--             cancelSpotFleetRequestsResponse
--
--         , describeDHCPOptionsResponseTest $
--             describeDHCPOptionsResponse
--
--         , describeSpotPriceHistoryResponseTest $
--             describeSpotPriceHistoryResponse
--
--         , stopInstancesResponseTest $
--             stopInstancesResponse
--
--         , importImageResponseTest $
--             importImageResponse
--
--         , deleteNetworkACLEntryResponseTest $
--             deleteNetworkACLEntryResponse
--
--         , disableVPCClassicLinkResponseTest $
--             disableVPCClassicLinkResponse
--
--         , authorizeSecurityGroupIngressResponseTest $
--             authorizeSecurityGroupIngressResponse
--
--         , bundleInstanceResponseTest $
--             bundleInstanceResponse
--
--         , describeVPCEndpointServicesResponseTest $
--             describeVPCEndpointServicesResponse
--
--         , replaceNetworkACLAssociationResponseTest $
--             replaceNetworkACLAssociationResponse
--
--         , createVPCPeeringConnectionResponseTest $
--             createVPCPeeringConnectionResponse
--
--         , resetSnapshotAttributeResponseTest $
--             resetSnapshotAttributeResponse
--
--         , describeAddressesResponseTest $
--             describeAddressesResponse
--
--         , describeInternetGatewaysResponseTest $
--             describeInternetGatewaysResponse
--
--         , replaceRouteResponseTest $
--             replaceRouteResponse
--
--         , createTagsResponseTest $
--             createTagsResponse
--
--         , describeSubnetsResponseTest $
--             describeSubnetsResponse
--
--         , describeNetworkInterfacesResponseTest $
--             describeNetworkInterfacesResponse
--
--         , purchaseReservedInstancesOfferingResponseTest $
--             purchaseReservedInstancesOfferingResponse
--
--         , describeSnapshotAttributeResponseTest $
--             describeSnapshotAttributeResponse
--
--         , createCustomerGatewayResponseTest $
--             createCustomerGatewayResponse
--
--         , attachInternetGatewayResponseTest $
--             attachInternetGatewayResponse
--
--         , deleteTagsResponseTest $
--             deleteTagsResponse
--
--         , replaceNetworkACLEntryResponseTest $
--             replaceNetworkACLEntryResponse
--
--         , resetInstanceAttributeResponseTest $
--             resetInstanceAttributeResponse
--
--         , deleteRouteResponseTest $
--             deleteRouteResponse
--
--         , describeVPNConnectionsResponseTest $
--             describeVPNConnectionsResponse
--
--         , describeFlowLogsResponseTest $
--             describeFlowLogsResponse
--
--         , deleteSecurityGroupResponseTest $
--             deleteSecurityGroupResponse
--
--         , describeReservedInstancesOfferingsResponseTest $
--             describeReservedInstancesOfferingsResponse
--
--         , deleteVPCPeeringConnectionResponseTest $
--             deleteVPCPeeringConnectionResponse
--
--         , describeVPCEndpointsResponseTest $
--             describeVPCEndpointsResponse
--
--         , describeInstanceAttributeResponseTest $
--             describeInstanceAttributeResponse
--
--         , confirmProductInstanceResponseTest $
--             confirmProductInstanceResponse
--
--         , importKeyPairResponseTest $
--             importKeyPairResponse
--
--         , attachNetworkInterfaceResponseTest $
--             attachNetworkInterfaceResponse
--
--         , describeInstanceStatusResponseTest $
--             describeInstanceStatusResponse
--
--         , cancelConversionTaskResponseTest $
--             cancelConversionTaskResponse
--
--         , reportInstanceStatusResponseTest $
--             reportInstanceStatusResponse
--
--         , associateDHCPOptionsResponseTest $
--             associateDHCPOptionsResponse
--
--         , describeVPCsResponseTest $
--             describeVPCsResponse
--
--         , requestSpotInstancesResponseTest $
--             requestSpotInstancesResponse
--
--         , modifyImageAttributeResponseTest $
--             modifyImageAttributeResponse
--
--         , describeReservedInstancesResponseTest $
--             describeReservedInstancesResponse
--
--         , allocateAddressResponseTest $
--             allocateAddressResponse
--
--         , reservationTest $
--             reservation
--
--         , createRouteTableResponseTest $
--             createRouteTableResponse
--
--         , volumeAttachmentTest $
--             volumeAttachment
--
--         , describeConversionTasksResponseTest $
--             describeConversionTasksResponse
--
--         , rejectVPCPeeringConnectionResponseTest $
--             rejectVPCPeeringConnectionResponse
--
--         , revokeSecurityGroupIngressResponseTest $
--             revokeSecurityGroupIngressResponse
--
--         , describeVolumesResponseTest $
--             describeVolumesResponse
--
--         , deleteVPNConnectionRouteResponseTest $
--             deleteVPNConnectionRouteResponse
--
--         , modifyReservedInstancesResponseTest $
--             modifyReservedInstancesResponse
--
--         , registerImageResponseTest $
--             registerImageResponse
--
--         , modifyVPCEndpointResponseTest $
--             modifyVPCEndpointResponse
--
--         , deleteVPNGatewayResponseTest $
--             deleteVPNGatewayResponse
--
--         , createVPCResponseTest $
--             createVPCResponse
--
--         , describeMovingAddressesResponseTest $
--             describeMovingAddressesResponse
--
--         , describeVolumeAttributeResponseTest $
--             describeVolumeAttributeResponse
--
--         , moveAddressToVPCResponseTest $
--             moveAddressToVPCResponse
--
--         , getPasswordDataResponseTest $
--             getPasswordDataResponse
--
--         , createFlowLogsResponseTest $
--             createFlowLogsResponse
--
--         , describeImportImageTasksResponseTest $
--             describeImportImageTasksResponse
--
--         , deleteNetworkACLResponseTest $
--             deleteNetworkACLResponse
--
--         , describeSpotFleetRequestsResponseTest $
--             describeSpotFleetRequestsResponse
--
--         , copySnapshotResponseTest $
--             copySnapshotResponse
--
--         , modifyVolumeAttributeResponseTest $
--             modifyVolumeAttributeResponse
--
--         , describeVPCAttributeResponseTest $
--             describeVPCAttributeResponse
--
--         , volumeTest $
--             volume
--
--         , disassociateAddressResponseTest $
--             disassociateAddressResponse
--
--         , deleteVPCResponseTest $
--             deleteVPCResponse
--
--         , describePrefixListsResponseTest $
--             describePrefixListsResponse
--
--         , createInstanceExportTaskResponseTest $
--             createInstanceExportTaskResponse
--
--         , describeSpotDatafeedSubscriptionResponseTest $
--             describeSpotDatafeedSubscriptionResponse
--
--         , detachVPNGatewayResponseTest $
--             detachVPNGatewayResponse
--
--         , describeExportTasksResponseTest $
--             describeExportTasksResponse
--
--         , deletePlacementGroupResponseTest $
--             deletePlacementGroupResponse
--
--         , createSubnetResponseTest $
--             createSubnetResponse
--
--         , enableVolumeIOResponseTest $
--             enableVolumeIOResponse
--
--         , cancelExportTaskResponseTest $
--             cancelExportTaskResponse
--
--         , requestSpotFleetResponseTest $
--             requestSpotFleetResponse
--
--         , describeInstancesResponseTest $
--             describeInstancesResponse
--
--         , describeSecurityGroupsResponseTest $
--             describeSecurityGroupsResponse
--
--         , describeVPCPeeringConnectionsResponseTest $
--             describeVPCPeeringConnectionsResponse
--
--         , createNetworkInterfaceResponseTest $
--             createNetworkInterfaceResponse
--
--         , associateAddressResponseTest $
--             associateAddressResponse
--
--         , startInstancesResponseTest $
--             startInstancesResponse
--
--         , describeCustomerGatewaysResponseTest $
--             describeCustomerGatewaysResponse
--
--         , resetNetworkInterfaceAttributeResponseTest $
--             resetNetworkInterfaceAttributeResponse
--
--         , createVPNConnectionResponseTest $
--             createVPNConnectionResponse
--
--         , describeSnapshotsResponseTest $
--             describeSnapshotsResponse
--
--         , createPlacementGroupResponseTest $
--             createPlacementGroupResponse
--
--         , replaceRouteTableAssociationResponseTest $
--             replaceRouteTableAssociationResponse
--
--         , describeNetworkInterfaceAttributeResponseTest $
--             describeNetworkInterfaceAttributeResponse
--
--         , describeReservedInstancesListingsResponseTest $
--             describeReservedInstancesListingsResponse
--
--         , deleteNetworkInterfaceResponseTest $
--             deleteNetworkInterfaceResponse
--
--         , deleteInternetGatewayResponseTest $
--             deleteInternetGatewayResponse
--
--         , deleteSubnetResponseTest $
--             deleteSubnetResponse
--
--         , createVPCEndpointResponseTest $
--             createVPCEndpointResponse
--
--         , describeImportSnapshotTasksResponseTest $
--             describeImportSnapshotTasksResponse
--
--         , copyImageResponseTest $
--             copyImageResponse
--
--         , disassociateRouteTableResponseTest $
--             disassociateRouteTableResponse
--
--         , unmonitorInstancesResponseTest $
--             unmonitorInstancesResponse
--
--         , importVolumeResponseTest $
--             importVolumeResponse
--
--         , disableVGWRoutePropagationResponseTest $
--             disableVGWRoutePropagationResponse
--
--         , createSpotDatafeedSubscriptionResponseTest $
--             createSpotDatafeedSubscriptionResponse
--
--         , assignPrivateIPAddressesResponseTest $
--             assignPrivateIPAddressesResponse
--
--         , deleteSnapshotResponseTest $
--             deleteSnapshotResponse
--
--         , deleteCustomerGatewayResponseTest $
--             deleteCustomerGatewayResponse
--
--         , modifyInstanceAttributeResponseTest $
--             modifyInstanceAttributeResponse
--
--         , createSecurityGroupResponseTest $
--             createSecurityGroupResponse
--
--         , cancelSpotInstanceRequestsResponseTest $
--             cancelSpotInstanceRequestsResponse
--
--         , createRouteResponseTest $
--             createRouteResponse
--
--         , createNetworkACLEntryResponseTest $
--             createNetworkACLEntryResponse
--
--         , modifySnapshotAttributeResponseTest $
--             modifySnapshotAttributeResponse
--
--         , enableVGWRoutePropagationResponseTest $
--             enableVGWRoutePropagationResponse
--
--         , snapshotTest $
--             snapshot
--
--         , describeSpotFleetRequestHistoryResponseTest $
--             describeSpotFleetRequestHistoryResponse
--
--         , deleteSpotDatafeedSubscriptionResponseTest $
--             deleteSpotDatafeedSubscriptionResponse
--
--         , describePlacementGroupsResponseTest $
--             describePlacementGroupsResponse
--
--         , createReservedInstancesListingResponseTest $
--             createReservedInstancesListingResponse
--
--         , enableVPCClassicLinkResponseTest $
--             enableVPCClassicLinkResponse
--
--         , describeKeyPairsResponseTest $
--             describeKeyPairsResponse
--
--         , rebootInstancesResponseTest $
--             rebootInstancesResponse
--
--         , attachVPNGatewayResponseTest $
--             attachVPNGatewayResponse
--
--         , createVPNConnectionRouteResponseTest $
--             createVPNConnectionRouteResponse
--
--         , describeClassicLinkInstancesResponseTest $
--             describeClassicLinkInstancesResponse
--
--         , terminateInstancesResponseTest $
--             terminateInstancesResponse
--
--         , createDHCPOptionsResponseTest $
--             createDHCPOptionsResponse
--
--         , associateRouteTableResponseTest $
--             associateRouteTableResponse
--
--         , createImageResponseTest $
--             createImageResponse
--
--         , describeAccountAttributesResponseTest $
--             describeAccountAttributesResponse
--
--         , resetImageAttributeResponseTest $
--             resetImageAttributeResponse
--
--         , describeNetworkACLsResponseTest $
--             describeNetworkACLsResponse
--
--         , cancelImportTaskResponseTest $
--             cancelImportTaskResponse
--
--         , getConsoleOutputResponseTest $
--             getConsoleOutputResponse
--
--         , unassignPrivateIPAddressesResponseTest $
--             unassignPrivateIPAddressesResponse
--
--         , deleteRouteTableResponseTest $
--             deleteRouteTableResponse
--
--         , describeImageAttributeResponseTest $
--             describeImageAttributeResponse
--
--         , deleteDHCPOptionsResponseTest $
--             deleteDHCPOptionsResponse
--
--         , describeVPNGatewaysResponseTest $
--             describeVPNGatewaysResponse
--
--         , detachClassicLinkVPCResponseTest $
--             detachClassicLinkVPCResponse
--
--         , describeReservedInstancesModificationsResponseTest $
--             describeReservedInstancesModificationsResponse
--
--         , describeSpotInstanceRequestsResponseTest $
--             describeSpotInstanceRequestsResponse
--
--         , monitorInstancesResponseTest $
--             monitorInstancesResponse
--
--         , describeRegionsResponseTest $
--             describeRegionsResponse
--
--         , modifyVPCAttributeResponseTest $
--             modifyVPCAttributeResponse
--
--         , describeSpotFleetInstancesResponseTest $
--             describeSpotFleetInstancesResponse
--
--         , describeVolumeStatusResponseTest $
--             describeVolumeStatusResponse
--
--         , deleteVolumeResponseTest $
--             deleteVolumeResponse
--
--         , describeImagesResponseTest $
--             describeImagesResponse
--
--         , createKeyPairResponseTest $
--             createKeyPairResponse
--
--         , restoreAddressToClassicResponseTest $
--             restoreAddressToClassicResponse
--
--         , describeAvailabilityZonesResponseTest $
--             describeAvailabilityZonesResponse
--
--         , importSnapshotResponseTest $
--             importSnapshotResponse
--
--         , acceptVPCPeeringConnectionResponseTest $
--             acceptVPCPeeringConnectionResponse
--
--         , describeRouteTablesResponseTest $
--             describeRouteTablesResponse
--
--           ]
--     ]

-- Requests

detachNetworkInterfaceTest :: DetachNetworkInterface -> TestTree
detachNetworkInterfaceTest = undefined

deleteVPCEndpointsTest :: DeleteVPCEndpoints -> TestTree
deleteVPCEndpointsTest = undefined

deleteKeyPairTest :: DeleteKeyPair -> TestTree
deleteKeyPairTest = undefined

deleteFlowLogsTest :: DeleteFlowLogs -> TestTree
deleteFlowLogsTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

revokeSecurityGroupEgressTest :: RevokeSecurityGroupEgress -> TestTree
revokeSecurityGroupEgressTest = undefined

createVPNGatewayTest :: CreateVPNGateway -> TestTree
createVPNGatewayTest = undefined

detachInternetGatewayTest :: DetachInternetGateway -> TestTree
detachInternetGatewayTest = undefined

createNetworkACLTest :: CreateNetworkACL -> TestTree
createNetworkACLTest = undefined

importInstanceTest :: ImportInstance -> TestTree
importInstanceTest = undefined

describeVPCClassicLinkTest :: DescribeVPCClassicLink -> TestTree
describeVPCClassicLinkTest = undefined

deleteVPNConnectionTest :: DeleteVPNConnection -> TestTree
deleteVPNConnectionTest = undefined

authorizeSecurityGroupEgressTest :: AuthorizeSecurityGroupEgress -> TestTree
authorizeSecurityGroupEgressTest = undefined

describeBundleTasksTest :: DescribeBundleTasks -> TestTree
describeBundleTasksTest = undefined

createInternetGatewayTest :: CreateInternetGateway -> TestTree
createInternetGatewayTest = undefined

releaseAddressTest :: ReleaseAddress -> TestTree
releaseAddressTest = undefined

cancelBundleTaskTest :: CancelBundleTask -> TestTree
cancelBundleTaskTest = undefined

modifyNetworkInterfaceAttributeTest :: ModifyNetworkInterfaceAttribute -> TestTree
modifyNetworkInterfaceAttributeTest = undefined

modifySubnetAttributeTest :: ModifySubnetAttribute -> TestTree
modifySubnetAttributeTest = undefined

deregisterImageTest :: DeregisterImage -> TestTree
deregisterImageTest = undefined

detachVolumeTest :: DetachVolume -> TestTree
detachVolumeTest = undefined

cancelReservedInstancesListingTest :: CancelReservedInstancesListing -> TestTree
cancelReservedInstancesListingTest = undefined

attachClassicLinkVPCTest :: AttachClassicLinkVPC -> TestTree
attachClassicLinkVPCTest = undefined

cancelSpotFleetRequestsTest :: CancelSpotFleetRequests -> TestTree
cancelSpotFleetRequestsTest = undefined

describeDHCPOptionsTest :: DescribeDHCPOptions -> TestTree
describeDHCPOptionsTest = undefined

describeSpotPriceHistoryTest :: DescribeSpotPriceHistory -> TestTree
describeSpotPriceHistoryTest = undefined

stopInstancesTest :: StopInstances -> TestTree
stopInstancesTest = undefined

importImageTest :: ImportImage -> TestTree
importImageTest = undefined

deleteNetworkACLEntryTest :: DeleteNetworkACLEntry -> TestTree
deleteNetworkACLEntryTest = undefined

disableVPCClassicLinkTest :: DisableVPCClassicLink -> TestTree
disableVPCClassicLinkTest = undefined

authorizeSecurityGroupIngressTest :: AuthorizeSecurityGroupIngress -> TestTree
authorizeSecurityGroupIngressTest = undefined

bundleInstanceTest :: BundleInstance -> TestTree
bundleInstanceTest = undefined

describeVPCEndpointServicesTest :: DescribeVPCEndpointServices -> TestTree
describeVPCEndpointServicesTest = undefined

replaceNetworkACLAssociationTest :: ReplaceNetworkACLAssociation -> TestTree
replaceNetworkACLAssociationTest = undefined

createVPCPeeringConnectionTest :: CreateVPCPeeringConnection -> TestTree
createVPCPeeringConnectionTest = undefined

resetSnapshotAttributeTest :: ResetSnapshotAttribute -> TestTree
resetSnapshotAttributeTest = undefined

describeAddressesTest :: DescribeAddresses -> TestTree
describeAddressesTest = undefined

describeInternetGatewaysTest :: DescribeInternetGateways -> TestTree
describeInternetGatewaysTest = undefined

replaceRouteTest :: ReplaceRoute -> TestTree
replaceRouteTest = undefined

createTagsTest :: CreateTags -> TestTree
createTagsTest = undefined

describeSubnetsTest :: DescribeSubnets -> TestTree
describeSubnetsTest = undefined

describeNetworkInterfacesTest :: DescribeNetworkInterfaces -> TestTree
describeNetworkInterfacesTest = undefined

purchaseReservedInstancesOfferingTest :: PurchaseReservedInstancesOffering -> TestTree
purchaseReservedInstancesOfferingTest = undefined

describeSnapshotAttributeTest :: DescribeSnapshotAttribute -> TestTree
describeSnapshotAttributeTest = undefined

createCustomerGatewayTest :: CreateCustomerGateway -> TestTree
createCustomerGatewayTest = undefined

attachInternetGatewayTest :: AttachInternetGateway -> TestTree
attachInternetGatewayTest = undefined

deleteTagsTest :: DeleteTags -> TestTree
deleteTagsTest = undefined

replaceNetworkACLEntryTest :: ReplaceNetworkACLEntry -> TestTree
replaceNetworkACLEntryTest = undefined

resetInstanceAttributeTest :: ResetInstanceAttribute -> TestTree
resetInstanceAttributeTest = undefined

deleteRouteTest :: DeleteRoute -> TestTree
deleteRouteTest = undefined

describeVPNConnectionsTest :: DescribeVPNConnections -> TestTree
describeVPNConnectionsTest = undefined

describeFlowLogsTest :: DescribeFlowLogs -> TestTree
describeFlowLogsTest = undefined

deleteSecurityGroupTest :: DeleteSecurityGroup -> TestTree
deleteSecurityGroupTest = undefined

describeReservedInstancesOfferingsTest :: DescribeReservedInstancesOfferings -> TestTree
describeReservedInstancesOfferingsTest = undefined

deleteVPCPeeringConnectionTest :: DeleteVPCPeeringConnection -> TestTree
deleteVPCPeeringConnectionTest = undefined

describeVPCEndpointsTest :: DescribeVPCEndpoints -> TestTree
describeVPCEndpointsTest = undefined

describeInstanceAttributeTest :: DescribeInstanceAttribute -> TestTree
describeInstanceAttributeTest = undefined

confirmProductInstanceTest :: ConfirmProductInstance -> TestTree
confirmProductInstanceTest = undefined

importKeyPairTest :: ImportKeyPair -> TestTree
importKeyPairTest = undefined

attachNetworkInterfaceTest :: AttachNetworkInterface -> TestTree
attachNetworkInterfaceTest = undefined

describeInstanceStatusTest :: DescribeInstanceStatus -> TestTree
describeInstanceStatusTest = undefined

cancelConversionTaskTest :: CancelConversionTask -> TestTree
cancelConversionTaskTest = undefined

reportInstanceStatusTest :: ReportInstanceStatus -> TestTree
reportInstanceStatusTest = undefined

associateDHCPOptionsTest :: AssociateDHCPOptions -> TestTree
associateDHCPOptionsTest = undefined

describeVPCsTest :: DescribeVPCs -> TestTree
describeVPCsTest = undefined

requestSpotInstancesTest :: RequestSpotInstances -> TestTree
requestSpotInstancesTest = undefined

modifyImageAttributeTest :: ModifyImageAttribute -> TestTree
modifyImageAttributeTest = undefined

describeReservedInstancesTest :: DescribeReservedInstances -> TestTree
describeReservedInstancesTest = undefined

allocateAddressTest :: AllocateAddress -> TestTree
allocateAddressTest = undefined

runInstancesTest :: RunInstances -> TestTree
runInstancesTest = undefined

createRouteTableTest :: CreateRouteTable -> TestTree
createRouteTableTest = undefined

attachVolumeTest :: AttachVolume -> TestTree
attachVolumeTest = undefined

describeConversionTasksTest :: DescribeConversionTasks -> TestTree
describeConversionTasksTest = undefined

rejectVPCPeeringConnectionTest :: RejectVPCPeeringConnection -> TestTree
rejectVPCPeeringConnectionTest = undefined

revokeSecurityGroupIngressTest :: RevokeSecurityGroupIngress -> TestTree
revokeSecurityGroupIngressTest = undefined

describeVolumesTest :: DescribeVolumes -> TestTree
describeVolumesTest = undefined

deleteVPNConnectionRouteTest :: DeleteVPNConnectionRoute -> TestTree
deleteVPNConnectionRouteTest = undefined

modifyReservedInstancesTest :: ModifyReservedInstances -> TestTree
modifyReservedInstancesTest = undefined

registerImageTest :: RegisterImage -> TestTree
registerImageTest = undefined

modifyVPCEndpointTest :: ModifyVPCEndpoint -> TestTree
modifyVPCEndpointTest = undefined

deleteVPNGatewayTest :: DeleteVPNGateway -> TestTree
deleteVPNGatewayTest = undefined

createVPCTest :: CreateVPC -> TestTree
createVPCTest = undefined

describeMovingAddressesTest :: DescribeMovingAddresses -> TestTree
describeMovingAddressesTest = undefined

describeVolumeAttributeTest :: DescribeVolumeAttribute -> TestTree
describeVolumeAttributeTest = undefined

moveAddressToVPCTest :: MoveAddressToVPC -> TestTree
moveAddressToVPCTest = undefined

getPasswordDataTest :: GetPasswordData -> TestTree
getPasswordDataTest = undefined

createFlowLogsTest :: CreateFlowLogs -> TestTree
createFlowLogsTest = undefined

describeImportImageTasksTest :: DescribeImportImageTasks -> TestTree
describeImportImageTasksTest = undefined

deleteNetworkACLTest :: DeleteNetworkACL -> TestTree
deleteNetworkACLTest = undefined

describeSpotFleetRequestsTest :: DescribeSpotFleetRequests -> TestTree
describeSpotFleetRequestsTest = undefined

copySnapshotTest :: CopySnapshot -> TestTree
copySnapshotTest = undefined

modifyVolumeAttributeTest :: ModifyVolumeAttribute -> TestTree
modifyVolumeAttributeTest = undefined

describeVPCAttributeTest :: DescribeVPCAttribute -> TestTree
describeVPCAttributeTest = undefined

createVolumeTest :: CreateVolume -> TestTree
createVolumeTest = undefined

disassociateAddressTest :: DisassociateAddress -> TestTree
disassociateAddressTest = undefined

deleteVPCTest :: DeleteVPC -> TestTree
deleteVPCTest = undefined

describePrefixListsTest :: DescribePrefixLists -> TestTree
describePrefixListsTest = undefined

createInstanceExportTaskTest :: CreateInstanceExportTask -> TestTree
createInstanceExportTaskTest = undefined

describeSpotDatafeedSubscriptionTest :: DescribeSpotDatafeedSubscription -> TestTree
describeSpotDatafeedSubscriptionTest = undefined

detachVPNGatewayTest :: DetachVPNGateway -> TestTree
detachVPNGatewayTest = undefined

describeExportTasksTest :: DescribeExportTasks -> TestTree
describeExportTasksTest = undefined

deletePlacementGroupTest :: DeletePlacementGroup -> TestTree
deletePlacementGroupTest = undefined

createSubnetTest :: CreateSubnet -> TestTree
createSubnetTest = undefined

enableVolumeIOTest :: EnableVolumeIO -> TestTree
enableVolumeIOTest = undefined

cancelExportTaskTest :: CancelExportTask -> TestTree
cancelExportTaskTest = undefined

requestSpotFleetTest :: RequestSpotFleet -> TestTree
requestSpotFleetTest = undefined

describeInstancesTest :: DescribeInstances -> TestTree
describeInstancesTest = undefined

describeSecurityGroupsTest :: DescribeSecurityGroups -> TestTree
describeSecurityGroupsTest = undefined

describeVPCPeeringConnectionsTest :: DescribeVPCPeeringConnections -> TestTree
describeVPCPeeringConnectionsTest = undefined

createNetworkInterfaceTest :: CreateNetworkInterface -> TestTree
createNetworkInterfaceTest = undefined

associateAddressTest :: AssociateAddress -> TestTree
associateAddressTest = undefined

startInstancesTest :: StartInstances -> TestTree
startInstancesTest = undefined

describeCustomerGatewaysTest :: DescribeCustomerGateways -> TestTree
describeCustomerGatewaysTest = undefined

resetNetworkInterfaceAttributeTest :: ResetNetworkInterfaceAttribute -> TestTree
resetNetworkInterfaceAttributeTest = undefined

createVPNConnectionTest :: CreateVPNConnection -> TestTree
createVPNConnectionTest = undefined

describeSnapshotsTest :: DescribeSnapshots -> TestTree
describeSnapshotsTest = undefined

createPlacementGroupTest :: CreatePlacementGroup -> TestTree
createPlacementGroupTest = undefined

replaceRouteTableAssociationTest :: ReplaceRouteTableAssociation -> TestTree
replaceRouteTableAssociationTest = undefined

describeNetworkInterfaceAttributeTest :: DescribeNetworkInterfaceAttribute -> TestTree
describeNetworkInterfaceAttributeTest = undefined

describeReservedInstancesListingsTest :: DescribeReservedInstancesListings -> TestTree
describeReservedInstancesListingsTest = undefined

deleteNetworkInterfaceTest :: DeleteNetworkInterface -> TestTree
deleteNetworkInterfaceTest = undefined

deleteInternetGatewayTest :: DeleteInternetGateway -> TestTree
deleteInternetGatewayTest = undefined

deleteSubnetTest :: DeleteSubnet -> TestTree
deleteSubnetTest = undefined

createVPCEndpointTest :: CreateVPCEndpoint -> TestTree
createVPCEndpointTest = undefined

describeImportSnapshotTasksTest :: DescribeImportSnapshotTasks -> TestTree
describeImportSnapshotTasksTest = undefined

copyImageTest :: CopyImage -> TestTree
copyImageTest = undefined

disassociateRouteTableTest :: DisassociateRouteTable -> TestTree
disassociateRouteTableTest = undefined

unmonitorInstancesTest :: UnmonitorInstances -> TestTree
unmonitorInstancesTest = undefined

importVolumeTest :: ImportVolume -> TestTree
importVolumeTest = undefined

disableVGWRoutePropagationTest :: DisableVGWRoutePropagation -> TestTree
disableVGWRoutePropagationTest = undefined

createSpotDatafeedSubscriptionTest :: CreateSpotDatafeedSubscription -> TestTree
createSpotDatafeedSubscriptionTest = undefined

assignPrivateIPAddressesTest :: AssignPrivateIPAddresses -> TestTree
assignPrivateIPAddressesTest = undefined

deleteSnapshotTest :: DeleteSnapshot -> TestTree
deleteSnapshotTest = undefined

deleteCustomerGatewayTest :: DeleteCustomerGateway -> TestTree
deleteCustomerGatewayTest = undefined

modifyInstanceAttributeTest :: ModifyInstanceAttribute -> TestTree
modifyInstanceAttributeTest = undefined

createSecurityGroupTest :: CreateSecurityGroup -> TestTree
createSecurityGroupTest = undefined

cancelSpotInstanceRequestsTest :: CancelSpotInstanceRequests -> TestTree
cancelSpotInstanceRequestsTest = undefined

createRouteTest :: CreateRoute -> TestTree
createRouteTest = undefined

createNetworkACLEntryTest :: CreateNetworkACLEntry -> TestTree
createNetworkACLEntryTest = undefined

modifySnapshotAttributeTest :: ModifySnapshotAttribute -> TestTree
modifySnapshotAttributeTest = undefined

enableVGWRoutePropagationTest :: EnableVGWRoutePropagation -> TestTree
enableVGWRoutePropagationTest = undefined

createSnapshotTest :: CreateSnapshot -> TestTree
createSnapshotTest = undefined

describeSpotFleetRequestHistoryTest :: DescribeSpotFleetRequestHistory -> TestTree
describeSpotFleetRequestHistoryTest = undefined

deleteSpotDatafeedSubscriptionTest :: DeleteSpotDatafeedSubscription -> TestTree
deleteSpotDatafeedSubscriptionTest = undefined

describePlacementGroupsTest :: DescribePlacementGroups -> TestTree
describePlacementGroupsTest = undefined

createReservedInstancesListingTest :: CreateReservedInstancesListing -> TestTree
createReservedInstancesListingTest = undefined

enableVPCClassicLinkTest :: EnableVPCClassicLink -> TestTree
enableVPCClassicLinkTest = undefined

describeKeyPairsTest :: DescribeKeyPairs -> TestTree
describeKeyPairsTest = undefined

rebootInstancesTest :: RebootInstances -> TestTree
rebootInstancesTest = undefined

attachVPNGatewayTest :: AttachVPNGateway -> TestTree
attachVPNGatewayTest = undefined

createVPNConnectionRouteTest :: CreateVPNConnectionRoute -> TestTree
createVPNConnectionRouteTest = undefined

describeClassicLinkInstancesTest :: DescribeClassicLinkInstances -> TestTree
describeClassicLinkInstancesTest = undefined

terminateInstancesTest :: TerminateInstances -> TestTree
terminateInstancesTest = undefined

createDHCPOptionsTest :: CreateDHCPOptions -> TestTree
createDHCPOptionsTest = undefined

associateRouteTableTest :: AssociateRouteTable -> TestTree
associateRouteTableTest = undefined

createImageTest :: CreateImage -> TestTree
createImageTest = undefined

describeAccountAttributesTest :: DescribeAccountAttributes -> TestTree
describeAccountAttributesTest = undefined

resetImageAttributeTest :: ResetImageAttribute -> TestTree
resetImageAttributeTest = undefined

describeNetworkACLsTest :: DescribeNetworkACLs -> TestTree
describeNetworkACLsTest = undefined

cancelImportTaskTest :: CancelImportTask -> TestTree
cancelImportTaskTest = undefined

getConsoleOutputTest :: GetConsoleOutput -> TestTree
getConsoleOutputTest = undefined

unassignPrivateIPAddressesTest :: UnassignPrivateIPAddresses -> TestTree
unassignPrivateIPAddressesTest = undefined

deleteRouteTableTest :: DeleteRouteTable -> TestTree
deleteRouteTableTest = undefined

describeImageAttributeTest :: DescribeImageAttribute -> TestTree
describeImageAttributeTest = undefined

deleteDHCPOptionsTest :: DeleteDHCPOptions -> TestTree
deleteDHCPOptionsTest = undefined

describeVPNGatewaysTest :: DescribeVPNGateways -> TestTree
describeVPNGatewaysTest = undefined

detachClassicLinkVPCTest :: DetachClassicLinkVPC -> TestTree
detachClassicLinkVPCTest = undefined

describeReservedInstancesModificationsTest :: DescribeReservedInstancesModifications -> TestTree
describeReservedInstancesModificationsTest = undefined

describeSpotInstanceRequestsTest :: DescribeSpotInstanceRequests -> TestTree
describeSpotInstanceRequestsTest = undefined

monitorInstancesTest :: MonitorInstances -> TestTree
monitorInstancesTest = undefined

describeRegionsTest :: DescribeRegions -> TestTree
describeRegionsTest = undefined

modifyVPCAttributeTest :: ModifyVPCAttribute -> TestTree
modifyVPCAttributeTest = undefined

describeSpotFleetInstancesTest :: DescribeSpotFleetInstances -> TestTree
describeSpotFleetInstancesTest = undefined

describeVolumeStatusTest :: DescribeVolumeStatus -> TestTree
describeVolumeStatusTest = undefined

deleteVolumeTest :: DeleteVolume -> TestTree
deleteVolumeTest = undefined

describeImagesTest :: DescribeImages -> TestTree
describeImagesTest = undefined

createKeyPairTest :: CreateKeyPair -> TestTree
createKeyPairTest = undefined

restoreAddressToClassicTest :: RestoreAddressToClassic -> TestTree
restoreAddressToClassicTest = undefined

describeAvailabilityZonesTest :: DescribeAvailabilityZones -> TestTree
describeAvailabilityZonesTest = undefined

importSnapshotTest :: ImportSnapshot -> TestTree
importSnapshotTest = undefined

acceptVPCPeeringConnectionTest :: AcceptVPCPeeringConnection -> TestTree
acceptVPCPeeringConnectionTest = undefined

describeRouteTablesTest :: DescribeRouteTables -> TestTree
describeRouteTablesTest = undefined

-- Responses

detachNetworkInterfaceResponseTest :: DetachNetworkInterfaceResponse -> TestTree
detachNetworkInterfaceResponseTest = resp
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse"
    (Proxy :: Proxy DetachNetworkInterface)

deleteVPCEndpointsResponseTest :: DeleteVPCEndpointsResponse -> TestTree
deleteVPCEndpointsResponseTest = resp
    "DeleteVPCEndpointsResponse"
    "fixture/DeleteVPCEndpointsResponse"
    (Proxy :: Proxy DeleteVPCEndpoints)

deleteKeyPairResponseTest :: DeleteKeyPairResponse -> TestTree
deleteKeyPairResponseTest = resp
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse"
    (Proxy :: Proxy DeleteKeyPair)

deleteFlowLogsResponseTest :: DeleteFlowLogsResponse -> TestTree
deleteFlowLogsResponseTest = resp
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse"
    (Proxy :: Proxy DeleteFlowLogs)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

revokeSecurityGroupEgressResponseTest :: RevokeSecurityGroupEgressResponse -> TestTree
revokeSecurityGroupEgressResponseTest = resp
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse"
    (Proxy :: Proxy RevokeSecurityGroupEgress)

createVPNGatewayResponseTest :: CreateVPNGatewayResponse -> TestTree
createVPNGatewayResponseTest = resp
    "CreateVPNGatewayResponse"
    "fixture/CreateVPNGatewayResponse"
    (Proxy :: Proxy CreateVPNGateway)

detachInternetGatewayResponseTest :: DetachInternetGatewayResponse -> TestTree
detachInternetGatewayResponseTest = resp
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse"
    (Proxy :: Proxy DetachInternetGateway)

createNetworkACLResponseTest :: CreateNetworkACLResponse -> TestTree
createNetworkACLResponseTest = resp
    "CreateNetworkACLResponse"
    "fixture/CreateNetworkACLResponse"
    (Proxy :: Proxy CreateNetworkACL)

importInstanceResponseTest :: ImportInstanceResponse -> TestTree
importInstanceResponseTest = resp
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse"
    (Proxy :: Proxy ImportInstance)

describeVPCClassicLinkResponseTest :: DescribeVPCClassicLinkResponse -> TestTree
describeVPCClassicLinkResponseTest = resp
    "DescribeVPCClassicLinkResponse"
    "fixture/DescribeVPCClassicLinkResponse"
    (Proxy :: Proxy DescribeVPCClassicLink)

deleteVPNConnectionResponseTest :: DeleteVPNConnectionResponse -> TestTree
deleteVPNConnectionResponseTest = resp
    "DeleteVPNConnectionResponse"
    "fixture/DeleteVPNConnectionResponse"
    (Proxy :: Proxy DeleteVPNConnection)

authorizeSecurityGroupEgressResponseTest :: AuthorizeSecurityGroupEgressResponse -> TestTree
authorizeSecurityGroupEgressResponseTest = resp
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse"
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

describeBundleTasksResponseTest :: DescribeBundleTasksResponse -> TestTree
describeBundleTasksResponseTest = resp
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse"
    (Proxy :: Proxy DescribeBundleTasks)

createInternetGatewayResponseTest :: CreateInternetGatewayResponse -> TestTree
createInternetGatewayResponseTest = resp
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse"
    (Proxy :: Proxy CreateInternetGateway)

releaseAddressResponseTest :: ReleaseAddressResponse -> TestTree
releaseAddressResponseTest = resp
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse"
    (Proxy :: Proxy ReleaseAddress)

cancelBundleTaskResponseTest :: CancelBundleTaskResponse -> TestTree
cancelBundleTaskResponseTest = resp
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse"
    (Proxy :: Proxy CancelBundleTask)

modifyNetworkInterfaceAttributeResponseTest :: ModifyNetworkInterfaceAttributeResponse -> TestTree
modifyNetworkInterfaceAttributeResponseTest = resp
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

modifySubnetAttributeResponseTest :: ModifySubnetAttributeResponse -> TestTree
modifySubnetAttributeResponseTest = resp
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse"
    (Proxy :: Proxy ModifySubnetAttribute)

deregisterImageResponseTest :: DeregisterImageResponse -> TestTree
deregisterImageResponseTest = resp
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse"
    (Proxy :: Proxy DeregisterImage)

volumeAttachmentTest :: VolumeAttachment -> TestTree
volumeAttachmentTest = resp
    "VolumeAttachment"
    "fixture/VolumeAttachment"
    (Proxy :: Proxy DetachVolume)

cancelReservedInstancesListingResponseTest :: CancelReservedInstancesListingResponse -> TestTree
cancelReservedInstancesListingResponseTest = resp
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse"
    (Proxy :: Proxy CancelReservedInstancesListing)

attachClassicLinkVPCResponseTest :: AttachClassicLinkVPCResponse -> TestTree
attachClassicLinkVPCResponseTest = resp
    "AttachClassicLinkVPCResponse"
    "fixture/AttachClassicLinkVPCResponse"
    (Proxy :: Proxy AttachClassicLinkVPC)

cancelSpotFleetRequestsResponseTest :: CancelSpotFleetRequestsResponse -> TestTree
cancelSpotFleetRequestsResponseTest = resp
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse"
    (Proxy :: Proxy CancelSpotFleetRequests)

describeDHCPOptionsResponseTest :: DescribeDHCPOptionsResponse -> TestTree
describeDHCPOptionsResponseTest = resp
    "DescribeDHCPOptionsResponse"
    "fixture/DescribeDHCPOptionsResponse"
    (Proxy :: Proxy DescribeDHCPOptions)

describeSpotPriceHistoryResponseTest :: DescribeSpotPriceHistoryResponse -> TestTree
describeSpotPriceHistoryResponseTest = resp
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse"
    (Proxy :: Proxy DescribeSpotPriceHistory)

stopInstancesResponseTest :: StopInstancesResponse -> TestTree
stopInstancesResponseTest = resp
    "StopInstancesResponse"
    "fixture/StopInstancesResponse"
    (Proxy :: Proxy StopInstances)

importImageResponseTest :: ImportImageResponse -> TestTree
importImageResponseTest = resp
    "ImportImageResponse"
    "fixture/ImportImageResponse"
    (Proxy :: Proxy ImportImage)

deleteNetworkACLEntryResponseTest :: DeleteNetworkACLEntryResponse -> TestTree
deleteNetworkACLEntryResponseTest = resp
    "DeleteNetworkACLEntryResponse"
    "fixture/DeleteNetworkACLEntryResponse"
    (Proxy :: Proxy DeleteNetworkACLEntry)

disableVPCClassicLinkResponseTest :: DisableVPCClassicLinkResponse -> TestTree
disableVPCClassicLinkResponseTest = resp
    "DisableVPCClassicLinkResponse"
    "fixture/DisableVPCClassicLinkResponse"
    (Proxy :: Proxy DisableVPCClassicLink)

authorizeSecurityGroupIngressResponseTest :: AuthorizeSecurityGroupIngressResponse -> TestTree
authorizeSecurityGroupIngressResponseTest = resp
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

bundleInstanceResponseTest :: BundleInstanceResponse -> TestTree
bundleInstanceResponseTest = resp
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse"
    (Proxy :: Proxy BundleInstance)

describeVPCEndpointServicesResponseTest :: DescribeVPCEndpointServicesResponse -> TestTree
describeVPCEndpointServicesResponseTest = resp
    "DescribeVPCEndpointServicesResponse"
    "fixture/DescribeVPCEndpointServicesResponse"
    (Proxy :: Proxy DescribeVPCEndpointServices)

replaceNetworkACLAssociationResponseTest :: ReplaceNetworkACLAssociationResponse -> TestTree
replaceNetworkACLAssociationResponseTest = resp
    "ReplaceNetworkACLAssociationResponse"
    "fixture/ReplaceNetworkACLAssociationResponse"
    (Proxy :: Proxy ReplaceNetworkACLAssociation)

createVPCPeeringConnectionResponseTest :: CreateVPCPeeringConnectionResponse -> TestTree
createVPCPeeringConnectionResponseTest = resp
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse"
    (Proxy :: Proxy CreateVPCPeeringConnection)

resetSnapshotAttributeResponseTest :: ResetSnapshotAttributeResponse -> TestTree
resetSnapshotAttributeResponseTest = resp
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse"
    (Proxy :: Proxy ResetSnapshotAttribute)

describeAddressesResponseTest :: DescribeAddressesResponse -> TestTree
describeAddressesResponseTest = resp
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse"
    (Proxy :: Proxy DescribeAddresses)

describeInternetGatewaysResponseTest :: DescribeInternetGatewaysResponse -> TestTree
describeInternetGatewaysResponseTest = resp
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse"
    (Proxy :: Proxy DescribeInternetGateways)

replaceRouteResponseTest :: ReplaceRouteResponse -> TestTree
replaceRouteResponseTest = resp
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse"
    (Proxy :: Proxy ReplaceRoute)

createTagsResponseTest :: CreateTagsResponse -> TestTree
createTagsResponseTest = resp
    "CreateTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

describeSubnetsResponseTest :: DescribeSubnetsResponse -> TestTree
describeSubnetsResponseTest = resp
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse"
    (Proxy :: Proxy DescribeSubnets)

describeNetworkInterfacesResponseTest :: DescribeNetworkInterfacesResponse -> TestTree
describeNetworkInterfacesResponseTest = resp
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse"
    (Proxy :: Proxy DescribeNetworkInterfaces)

purchaseReservedInstancesOfferingResponseTest :: PurchaseReservedInstancesOfferingResponse -> TestTree
purchaseReservedInstancesOfferingResponseTest = resp
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

describeSnapshotAttributeResponseTest :: DescribeSnapshotAttributeResponse -> TestTree
describeSnapshotAttributeResponseTest = resp
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse"
    (Proxy :: Proxy DescribeSnapshotAttribute)

createCustomerGatewayResponseTest :: CreateCustomerGatewayResponse -> TestTree
createCustomerGatewayResponseTest = resp
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse"
    (Proxy :: Proxy CreateCustomerGateway)

attachInternetGatewayResponseTest :: AttachInternetGatewayResponse -> TestTree
attachInternetGatewayResponseTest = resp
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse"
    (Proxy :: Proxy AttachInternetGateway)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

replaceNetworkACLEntryResponseTest :: ReplaceNetworkACLEntryResponse -> TestTree
replaceNetworkACLEntryResponseTest = resp
    "ReplaceNetworkACLEntryResponse"
    "fixture/ReplaceNetworkACLEntryResponse"
    (Proxy :: Proxy ReplaceNetworkACLEntry)

resetInstanceAttributeResponseTest :: ResetInstanceAttributeResponse -> TestTree
resetInstanceAttributeResponseTest = resp
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse"
    (Proxy :: Proxy ResetInstanceAttribute)

deleteRouteResponseTest :: DeleteRouteResponse -> TestTree
deleteRouteResponseTest = resp
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse"
    (Proxy :: Proxy DeleteRoute)

describeVPNConnectionsResponseTest :: DescribeVPNConnectionsResponse -> TestTree
describeVPNConnectionsResponseTest = resp
    "DescribeVPNConnectionsResponse"
    "fixture/DescribeVPNConnectionsResponse"
    (Proxy :: Proxy DescribeVPNConnections)

describeFlowLogsResponseTest :: DescribeFlowLogsResponse -> TestTree
describeFlowLogsResponseTest = resp
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse"
    (Proxy :: Proxy DescribeFlowLogs)

deleteSecurityGroupResponseTest :: DeleteSecurityGroupResponse -> TestTree
deleteSecurityGroupResponseTest = resp
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse"
    (Proxy :: Proxy DeleteSecurityGroup)

describeReservedInstancesOfferingsResponseTest :: DescribeReservedInstancesOfferingsResponse -> TestTree
describeReservedInstancesOfferingsResponseTest = resp
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

deleteVPCPeeringConnectionResponseTest :: DeleteVPCPeeringConnectionResponse -> TestTree
deleteVPCPeeringConnectionResponseTest = resp
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse"
    (Proxy :: Proxy DeleteVPCPeeringConnection)

describeVPCEndpointsResponseTest :: DescribeVPCEndpointsResponse -> TestTree
describeVPCEndpointsResponseTest = resp
    "DescribeVPCEndpointsResponse"
    "fixture/DescribeVPCEndpointsResponse"
    (Proxy :: Proxy DescribeVPCEndpoints)

describeInstanceAttributeResponseTest :: DescribeInstanceAttributeResponse -> TestTree
describeInstanceAttributeResponseTest = resp
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse"
    (Proxy :: Proxy DescribeInstanceAttribute)

confirmProductInstanceResponseTest :: ConfirmProductInstanceResponse -> TestTree
confirmProductInstanceResponseTest = resp
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse"
    (Proxy :: Proxy ConfirmProductInstance)

importKeyPairResponseTest :: ImportKeyPairResponse -> TestTree
importKeyPairResponseTest = resp
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse"
    (Proxy :: Proxy ImportKeyPair)

attachNetworkInterfaceResponseTest :: AttachNetworkInterfaceResponse -> TestTree
attachNetworkInterfaceResponseTest = resp
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse"
    (Proxy :: Proxy AttachNetworkInterface)

describeInstanceStatusResponseTest :: DescribeInstanceStatusResponse -> TestTree
describeInstanceStatusResponseTest = resp
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse"
    (Proxy :: Proxy DescribeInstanceStatus)

cancelConversionTaskResponseTest :: CancelConversionTaskResponse -> TestTree
cancelConversionTaskResponseTest = resp
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse"
    (Proxy :: Proxy CancelConversionTask)

reportInstanceStatusResponseTest :: ReportInstanceStatusResponse -> TestTree
reportInstanceStatusResponseTest = resp
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse"
    (Proxy :: Proxy ReportInstanceStatus)

associateDHCPOptionsResponseTest :: AssociateDHCPOptionsResponse -> TestTree
associateDHCPOptionsResponseTest = resp
    "AssociateDHCPOptionsResponse"
    "fixture/AssociateDHCPOptionsResponse"
    (Proxy :: Proxy AssociateDHCPOptions)

describeVPCsResponseTest :: DescribeVPCsResponse -> TestTree
describeVPCsResponseTest = resp
    "DescribeVPCsResponse"
    "fixture/DescribeVPCsResponse"
    (Proxy :: Proxy DescribeVPCs)

requestSpotInstancesResponseTest :: RequestSpotInstancesResponse -> TestTree
requestSpotInstancesResponseTest = resp
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse"
    (Proxy :: Proxy RequestSpotInstances)

modifyImageAttributeResponseTest :: ModifyImageAttributeResponse -> TestTree
modifyImageAttributeResponseTest = resp
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse"
    (Proxy :: Proxy ModifyImageAttribute)

describeReservedInstancesResponseTest :: DescribeReservedInstancesResponse -> TestTree
describeReservedInstancesResponseTest = resp
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse"
    (Proxy :: Proxy DescribeReservedInstances)

allocateAddressResponseTest :: AllocateAddressResponse -> TestTree
allocateAddressResponseTest = resp
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse"
    (Proxy :: Proxy AllocateAddress)

reservationTest :: Reservation -> TestTree
reservationTest = resp
    "Reservation"
    "fixture/Reservation"
    (Proxy :: Proxy RunInstances)

createRouteTableResponseTest :: CreateRouteTableResponse -> TestTree
createRouteTableResponseTest = resp
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse"
    (Proxy :: Proxy CreateRouteTable)

volumeAttachmentTest :: VolumeAttachment -> TestTree
volumeAttachmentTest = resp
    "VolumeAttachment"
    "fixture/VolumeAttachment"
    (Proxy :: Proxy AttachVolume)

describeConversionTasksResponseTest :: DescribeConversionTasksResponse -> TestTree
describeConversionTasksResponseTest = resp
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse"
    (Proxy :: Proxy DescribeConversionTasks)

rejectVPCPeeringConnectionResponseTest :: RejectVPCPeeringConnectionResponse -> TestTree
rejectVPCPeeringConnectionResponseTest = resp
    "RejectVPCPeeringConnectionResponse"
    "fixture/RejectVPCPeeringConnectionResponse"
    (Proxy :: Proxy RejectVPCPeeringConnection)

revokeSecurityGroupIngressResponseTest :: RevokeSecurityGroupIngressResponse -> TestTree
revokeSecurityGroupIngressResponseTest = resp
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeSecurityGroupIngress)

describeVolumesResponseTest :: DescribeVolumesResponse -> TestTree
describeVolumesResponseTest = resp
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse"
    (Proxy :: Proxy DescribeVolumes)

deleteVPNConnectionRouteResponseTest :: DeleteVPNConnectionRouteResponse -> TestTree
deleteVPNConnectionRouteResponseTest = resp
    "DeleteVPNConnectionRouteResponse"
    "fixture/DeleteVPNConnectionRouteResponse"
    (Proxy :: Proxy DeleteVPNConnectionRoute)

modifyReservedInstancesResponseTest :: ModifyReservedInstancesResponse -> TestTree
modifyReservedInstancesResponseTest = resp
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse"
    (Proxy :: Proxy ModifyReservedInstances)

registerImageResponseTest :: RegisterImageResponse -> TestTree
registerImageResponseTest = resp
    "RegisterImageResponse"
    "fixture/RegisterImageResponse"
    (Proxy :: Proxy RegisterImage)

modifyVPCEndpointResponseTest :: ModifyVPCEndpointResponse -> TestTree
modifyVPCEndpointResponseTest = resp
    "ModifyVPCEndpointResponse"
    "fixture/ModifyVPCEndpointResponse"
    (Proxy :: Proxy ModifyVPCEndpoint)

deleteVPNGatewayResponseTest :: DeleteVPNGatewayResponse -> TestTree
deleteVPNGatewayResponseTest = resp
    "DeleteVPNGatewayResponse"
    "fixture/DeleteVPNGatewayResponse"
    (Proxy :: Proxy DeleteVPNGateway)

createVPCResponseTest :: CreateVPCResponse -> TestTree
createVPCResponseTest = resp
    "CreateVPCResponse"
    "fixture/CreateVPCResponse"
    (Proxy :: Proxy CreateVPC)

describeMovingAddressesResponseTest :: DescribeMovingAddressesResponse -> TestTree
describeMovingAddressesResponseTest = resp
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse"
    (Proxy :: Proxy DescribeMovingAddresses)

describeVolumeAttributeResponseTest :: DescribeVolumeAttributeResponse -> TestTree
describeVolumeAttributeResponseTest = resp
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse"
    (Proxy :: Proxy DescribeVolumeAttribute)

moveAddressToVPCResponseTest :: MoveAddressToVPCResponse -> TestTree
moveAddressToVPCResponseTest = resp
    "MoveAddressToVPCResponse"
    "fixture/MoveAddressToVPCResponse"
    (Proxy :: Proxy MoveAddressToVPC)

getPasswordDataResponseTest :: GetPasswordDataResponse -> TestTree
getPasswordDataResponseTest = resp
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse"
    (Proxy :: Proxy GetPasswordData)

createFlowLogsResponseTest :: CreateFlowLogsResponse -> TestTree
createFlowLogsResponseTest = resp
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse"
    (Proxy :: Proxy CreateFlowLogs)

describeImportImageTasksResponseTest :: DescribeImportImageTasksResponse -> TestTree
describeImportImageTasksResponseTest = resp
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse"
    (Proxy :: Proxy DescribeImportImageTasks)

deleteNetworkACLResponseTest :: DeleteNetworkACLResponse -> TestTree
deleteNetworkACLResponseTest = resp
    "DeleteNetworkACLResponse"
    "fixture/DeleteNetworkACLResponse"
    (Proxy :: Proxy DeleteNetworkACL)

describeSpotFleetRequestsResponseTest :: DescribeSpotFleetRequestsResponse -> TestTree
describeSpotFleetRequestsResponseTest = resp
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse"
    (Proxy :: Proxy DescribeSpotFleetRequests)

copySnapshotResponseTest :: CopySnapshotResponse -> TestTree
copySnapshotResponseTest = resp
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse"
    (Proxy :: Proxy CopySnapshot)

modifyVolumeAttributeResponseTest :: ModifyVolumeAttributeResponse -> TestTree
modifyVolumeAttributeResponseTest = resp
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse"
    (Proxy :: Proxy ModifyVolumeAttribute)

describeVPCAttributeResponseTest :: DescribeVPCAttributeResponse -> TestTree
describeVPCAttributeResponseTest = resp
    "DescribeVPCAttributeResponse"
    "fixture/DescribeVPCAttributeResponse"
    (Proxy :: Proxy DescribeVPCAttribute)

volumeTest :: Volume -> TestTree
volumeTest = resp
    "Volume"
    "fixture/Volume"
    (Proxy :: Proxy CreateVolume)

disassociateAddressResponseTest :: DisassociateAddressResponse -> TestTree
disassociateAddressResponseTest = resp
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse"
    (Proxy :: Proxy DisassociateAddress)

deleteVPCResponseTest :: DeleteVPCResponse -> TestTree
deleteVPCResponseTest = resp
    "DeleteVPCResponse"
    "fixture/DeleteVPCResponse"
    (Proxy :: Proxy DeleteVPC)

describePrefixListsResponseTest :: DescribePrefixListsResponse -> TestTree
describePrefixListsResponseTest = resp
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse"
    (Proxy :: Proxy DescribePrefixLists)

createInstanceExportTaskResponseTest :: CreateInstanceExportTaskResponse -> TestTree
createInstanceExportTaskResponseTest = resp
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse"
    (Proxy :: Proxy CreateInstanceExportTask)

describeSpotDatafeedSubscriptionResponseTest :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
describeSpotDatafeedSubscriptionResponseTest = resp
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

detachVPNGatewayResponseTest :: DetachVPNGatewayResponse -> TestTree
detachVPNGatewayResponseTest = resp
    "DetachVPNGatewayResponse"
    "fixture/DetachVPNGatewayResponse"
    (Proxy :: Proxy DetachVPNGateway)

describeExportTasksResponseTest :: DescribeExportTasksResponse -> TestTree
describeExportTasksResponseTest = resp
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse"
    (Proxy :: Proxy DescribeExportTasks)

deletePlacementGroupResponseTest :: DeletePlacementGroupResponse -> TestTree
deletePlacementGroupResponseTest = resp
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse"
    (Proxy :: Proxy DeletePlacementGroup)

createSubnetResponseTest :: CreateSubnetResponse -> TestTree
createSubnetResponseTest = resp
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse"
    (Proxy :: Proxy CreateSubnet)

enableVolumeIOResponseTest :: EnableVolumeIOResponse -> TestTree
enableVolumeIOResponseTest = resp
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse"
    (Proxy :: Proxy EnableVolumeIO)

cancelExportTaskResponseTest :: CancelExportTaskResponse -> TestTree
cancelExportTaskResponseTest = resp
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse"
    (Proxy :: Proxy CancelExportTask)

requestSpotFleetResponseTest :: RequestSpotFleetResponse -> TestTree
requestSpotFleetResponseTest = resp
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse"
    (Proxy :: Proxy RequestSpotFleet)

describeInstancesResponseTest :: DescribeInstancesResponse -> TestTree
describeInstancesResponseTest = resp
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse"
    (Proxy :: Proxy DescribeInstances)

describeSecurityGroupsResponseTest :: DescribeSecurityGroupsResponse -> TestTree
describeSecurityGroupsResponseTest = resp
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse"
    (Proxy :: Proxy DescribeSecurityGroups)

describeVPCPeeringConnectionsResponseTest :: DescribeVPCPeeringConnectionsResponse -> TestTree
describeVPCPeeringConnectionsResponseTest = resp
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse"
    (Proxy :: Proxy DescribeVPCPeeringConnections)

createNetworkInterfaceResponseTest :: CreateNetworkInterfaceResponse -> TestTree
createNetworkInterfaceResponseTest = resp
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse"
    (Proxy :: Proxy CreateNetworkInterface)

associateAddressResponseTest :: AssociateAddressResponse -> TestTree
associateAddressResponseTest = resp
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse"
    (Proxy :: Proxy AssociateAddress)

startInstancesResponseTest :: StartInstancesResponse -> TestTree
startInstancesResponseTest = resp
    "StartInstancesResponse"
    "fixture/StartInstancesResponse"
    (Proxy :: Proxy StartInstances)

describeCustomerGatewaysResponseTest :: DescribeCustomerGatewaysResponse -> TestTree
describeCustomerGatewaysResponseTest = resp
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse"
    (Proxy :: Proxy DescribeCustomerGateways)

resetNetworkInterfaceAttributeResponseTest :: ResetNetworkInterfaceAttributeResponse -> TestTree
resetNetworkInterfaceAttributeResponseTest = resp
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

createVPNConnectionResponseTest :: CreateVPNConnectionResponse -> TestTree
createVPNConnectionResponseTest = resp
    "CreateVPNConnectionResponse"
    "fixture/CreateVPNConnectionResponse"
    (Proxy :: Proxy CreateVPNConnection)

describeSnapshotsResponseTest :: DescribeSnapshotsResponse -> TestTree
describeSnapshotsResponseTest = resp
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

createPlacementGroupResponseTest :: CreatePlacementGroupResponse -> TestTree
createPlacementGroupResponseTest = resp
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse"
    (Proxy :: Proxy CreatePlacementGroup)

replaceRouteTableAssociationResponseTest :: ReplaceRouteTableAssociationResponse -> TestTree
replaceRouteTableAssociationResponseTest = resp
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse"
    (Proxy :: Proxy ReplaceRouteTableAssociation)

describeNetworkInterfaceAttributeResponseTest :: DescribeNetworkInterfaceAttributeResponse -> TestTree
describeNetworkInterfaceAttributeResponseTest = resp
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

describeReservedInstancesListingsResponseTest :: DescribeReservedInstancesListingsResponse -> TestTree
describeReservedInstancesListingsResponseTest = resp
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse"
    (Proxy :: Proxy DescribeReservedInstancesListings)

deleteNetworkInterfaceResponseTest :: DeleteNetworkInterfaceResponse -> TestTree
deleteNetworkInterfaceResponseTest = resp
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse"
    (Proxy :: Proxy DeleteNetworkInterface)

deleteInternetGatewayResponseTest :: DeleteInternetGatewayResponse -> TestTree
deleteInternetGatewayResponseTest = resp
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse"
    (Proxy :: Proxy DeleteInternetGateway)

deleteSubnetResponseTest :: DeleteSubnetResponse -> TestTree
deleteSubnetResponseTest = resp
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse"
    (Proxy :: Proxy DeleteSubnet)

createVPCEndpointResponseTest :: CreateVPCEndpointResponse -> TestTree
createVPCEndpointResponseTest = resp
    "CreateVPCEndpointResponse"
    "fixture/CreateVPCEndpointResponse"
    (Proxy :: Proxy CreateVPCEndpoint)

describeImportSnapshotTasksResponseTest :: DescribeImportSnapshotTasksResponse -> TestTree
describeImportSnapshotTasksResponseTest = resp
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse"
    (Proxy :: Proxy DescribeImportSnapshotTasks)

copyImageResponseTest :: CopyImageResponse -> TestTree
copyImageResponseTest = resp
    "CopyImageResponse"
    "fixture/CopyImageResponse"
    (Proxy :: Proxy CopyImage)

disassociateRouteTableResponseTest :: DisassociateRouteTableResponse -> TestTree
disassociateRouteTableResponseTest = resp
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse"
    (Proxy :: Proxy DisassociateRouteTable)

unmonitorInstancesResponseTest :: UnmonitorInstancesResponse -> TestTree
unmonitorInstancesResponseTest = resp
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse"
    (Proxy :: Proxy UnmonitorInstances)

importVolumeResponseTest :: ImportVolumeResponse -> TestTree
importVolumeResponseTest = resp
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse"
    (Proxy :: Proxy ImportVolume)

disableVGWRoutePropagationResponseTest :: DisableVGWRoutePropagationResponse -> TestTree
disableVGWRoutePropagationResponseTest = resp
    "DisableVGWRoutePropagationResponse"
    "fixture/DisableVGWRoutePropagationResponse"
    (Proxy :: Proxy DisableVGWRoutePropagation)

createSpotDatafeedSubscriptionResponseTest :: CreateSpotDatafeedSubscriptionResponse -> TestTree
createSpotDatafeedSubscriptionResponseTest = resp
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

assignPrivateIPAddressesResponseTest :: AssignPrivateIPAddressesResponse -> TestTree
assignPrivateIPAddressesResponseTest = resp
    "AssignPrivateIPAddressesResponse"
    "fixture/AssignPrivateIPAddressesResponse"
    (Proxy :: Proxy AssignPrivateIPAddresses)

deleteSnapshotResponseTest :: DeleteSnapshotResponse -> TestTree
deleteSnapshotResponseTest = resp
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

deleteCustomerGatewayResponseTest :: DeleteCustomerGatewayResponse -> TestTree
deleteCustomerGatewayResponseTest = resp
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse"
    (Proxy :: Proxy DeleteCustomerGateway)

modifyInstanceAttributeResponseTest :: ModifyInstanceAttributeResponse -> TestTree
modifyInstanceAttributeResponseTest = resp
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse"
    (Proxy :: Proxy ModifyInstanceAttribute)

createSecurityGroupResponseTest :: CreateSecurityGroupResponse -> TestTree
createSecurityGroupResponseTest = resp
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse"
    (Proxy :: Proxy CreateSecurityGroup)

cancelSpotInstanceRequestsResponseTest :: CancelSpotInstanceRequestsResponse -> TestTree
cancelSpotInstanceRequestsResponseTest = resp
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse"
    (Proxy :: Proxy CancelSpotInstanceRequests)

createRouteResponseTest :: CreateRouteResponse -> TestTree
createRouteResponseTest = resp
    "CreateRouteResponse"
    "fixture/CreateRouteResponse"
    (Proxy :: Proxy CreateRoute)

createNetworkACLEntryResponseTest :: CreateNetworkACLEntryResponse -> TestTree
createNetworkACLEntryResponseTest = resp
    "CreateNetworkACLEntryResponse"
    "fixture/CreateNetworkACLEntryResponse"
    (Proxy :: Proxy CreateNetworkACLEntry)

modifySnapshotAttributeResponseTest :: ModifySnapshotAttributeResponse -> TestTree
modifySnapshotAttributeResponseTest = resp
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse"
    (Proxy :: Proxy ModifySnapshotAttribute)

enableVGWRoutePropagationResponseTest :: EnableVGWRoutePropagationResponse -> TestTree
enableVGWRoutePropagationResponseTest = resp
    "EnableVGWRoutePropagationResponse"
    "fixture/EnableVGWRoutePropagationResponse"
    (Proxy :: Proxy EnableVGWRoutePropagation)

snapshotTest :: Snapshot -> TestTree
snapshotTest = resp
    "Snapshot"
    "fixture/Snapshot"
    (Proxy :: Proxy CreateSnapshot)

describeSpotFleetRequestHistoryResponseTest :: DescribeSpotFleetRequestHistoryResponse -> TestTree
describeSpotFleetRequestHistoryResponseTest = resp
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse"
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

deleteSpotDatafeedSubscriptionResponseTest :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
deleteSpotDatafeedSubscriptionResponseTest = resp
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

describePlacementGroupsResponseTest :: DescribePlacementGroupsResponse -> TestTree
describePlacementGroupsResponseTest = resp
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse"
    (Proxy :: Proxy DescribePlacementGroups)

createReservedInstancesListingResponseTest :: CreateReservedInstancesListingResponse -> TestTree
createReservedInstancesListingResponseTest = resp
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse"
    (Proxy :: Proxy CreateReservedInstancesListing)

enableVPCClassicLinkResponseTest :: EnableVPCClassicLinkResponse -> TestTree
enableVPCClassicLinkResponseTest = resp
    "EnableVPCClassicLinkResponse"
    "fixture/EnableVPCClassicLinkResponse"
    (Proxy :: Proxy EnableVPCClassicLink)

describeKeyPairsResponseTest :: DescribeKeyPairsResponse -> TestTree
describeKeyPairsResponseTest = resp
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse"
    (Proxy :: Proxy DescribeKeyPairs)

rebootInstancesResponseTest :: RebootInstancesResponse -> TestTree
rebootInstancesResponseTest = resp
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse"
    (Proxy :: Proxy RebootInstances)

attachVPNGatewayResponseTest :: AttachVPNGatewayResponse -> TestTree
attachVPNGatewayResponseTest = resp
    "AttachVPNGatewayResponse"
    "fixture/AttachVPNGatewayResponse"
    (Proxy :: Proxy AttachVPNGateway)

createVPNConnectionRouteResponseTest :: CreateVPNConnectionRouteResponse -> TestTree
createVPNConnectionRouteResponseTest = resp
    "CreateVPNConnectionRouteResponse"
    "fixture/CreateVPNConnectionRouteResponse"
    (Proxy :: Proxy CreateVPNConnectionRoute)

describeClassicLinkInstancesResponseTest :: DescribeClassicLinkInstancesResponse -> TestTree
describeClassicLinkInstancesResponseTest = resp
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse"
    (Proxy :: Proxy DescribeClassicLinkInstances)

terminateInstancesResponseTest :: TerminateInstancesResponse -> TestTree
terminateInstancesResponseTest = resp
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse"
    (Proxy :: Proxy TerminateInstances)

createDHCPOptionsResponseTest :: CreateDHCPOptionsResponse -> TestTree
createDHCPOptionsResponseTest = resp
    "CreateDHCPOptionsResponse"
    "fixture/CreateDHCPOptionsResponse"
    (Proxy :: Proxy CreateDHCPOptions)

associateRouteTableResponseTest :: AssociateRouteTableResponse -> TestTree
associateRouteTableResponseTest = resp
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse"
    (Proxy :: Proxy AssociateRouteTable)

createImageResponseTest :: CreateImageResponse -> TestTree
createImageResponseTest = resp
    "CreateImageResponse"
    "fixture/CreateImageResponse"
    (Proxy :: Proxy CreateImage)

describeAccountAttributesResponseTest :: DescribeAccountAttributesResponse -> TestTree
describeAccountAttributesResponseTest = resp
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse"
    (Proxy :: Proxy DescribeAccountAttributes)

resetImageAttributeResponseTest :: ResetImageAttributeResponse -> TestTree
resetImageAttributeResponseTest = resp
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse"
    (Proxy :: Proxy ResetImageAttribute)

describeNetworkACLsResponseTest :: DescribeNetworkACLsResponse -> TestTree
describeNetworkACLsResponseTest = resp
    "DescribeNetworkACLsResponse"
    "fixture/DescribeNetworkACLsResponse"
    (Proxy :: Proxy DescribeNetworkACLs)

cancelImportTaskResponseTest :: CancelImportTaskResponse -> TestTree
cancelImportTaskResponseTest = resp
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse"
    (Proxy :: Proxy CancelImportTask)

getConsoleOutputResponseTest :: GetConsoleOutputResponse -> TestTree
getConsoleOutputResponseTest = resp
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse"
    (Proxy :: Proxy GetConsoleOutput)

unassignPrivateIPAddressesResponseTest :: UnassignPrivateIPAddressesResponse -> TestTree
unassignPrivateIPAddressesResponseTest = resp
    "UnassignPrivateIPAddressesResponse"
    "fixture/UnassignPrivateIPAddressesResponse"
    (Proxy :: Proxy UnassignPrivateIPAddresses)

deleteRouteTableResponseTest :: DeleteRouteTableResponse -> TestTree
deleteRouteTableResponseTest = resp
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse"
    (Proxy :: Proxy DeleteRouteTable)

describeImageAttributeResponseTest :: DescribeImageAttributeResponse -> TestTree
describeImageAttributeResponseTest = resp
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse"
    (Proxy :: Proxy DescribeImageAttribute)

deleteDHCPOptionsResponseTest :: DeleteDHCPOptionsResponse -> TestTree
deleteDHCPOptionsResponseTest = resp
    "DeleteDHCPOptionsResponse"
    "fixture/DeleteDHCPOptionsResponse"
    (Proxy :: Proxy DeleteDHCPOptions)

describeVPNGatewaysResponseTest :: DescribeVPNGatewaysResponse -> TestTree
describeVPNGatewaysResponseTest = resp
    "DescribeVPNGatewaysResponse"
    "fixture/DescribeVPNGatewaysResponse"
    (Proxy :: Proxy DescribeVPNGateways)

detachClassicLinkVPCResponseTest :: DetachClassicLinkVPCResponse -> TestTree
detachClassicLinkVPCResponseTest = resp
    "DetachClassicLinkVPCResponse"
    "fixture/DetachClassicLinkVPCResponse"
    (Proxy :: Proxy DetachClassicLinkVPC)

describeReservedInstancesModificationsResponseTest :: DescribeReservedInstancesModificationsResponse -> TestTree
describeReservedInstancesModificationsResponseTest = resp
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse"
    (Proxy :: Proxy DescribeReservedInstancesModifications)

describeSpotInstanceRequestsResponseTest :: DescribeSpotInstanceRequestsResponse -> TestTree
describeSpotInstanceRequestsResponseTest = resp
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse"
    (Proxy :: Proxy DescribeSpotInstanceRequests)

monitorInstancesResponseTest :: MonitorInstancesResponse -> TestTree
monitorInstancesResponseTest = resp
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse"
    (Proxy :: Proxy MonitorInstances)

describeRegionsResponseTest :: DescribeRegionsResponse -> TestTree
describeRegionsResponseTest = resp
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse"
    (Proxy :: Proxy DescribeRegions)

modifyVPCAttributeResponseTest :: ModifyVPCAttributeResponse -> TestTree
modifyVPCAttributeResponseTest = resp
    "ModifyVPCAttributeResponse"
    "fixture/ModifyVPCAttributeResponse"
    (Proxy :: Proxy ModifyVPCAttribute)

describeSpotFleetInstancesResponseTest :: DescribeSpotFleetInstancesResponse -> TestTree
describeSpotFleetInstancesResponseTest = resp
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse"
    (Proxy :: Proxy DescribeSpotFleetInstances)

describeVolumeStatusResponseTest :: DescribeVolumeStatusResponse -> TestTree
describeVolumeStatusResponseTest = resp
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse"
    (Proxy :: Proxy DescribeVolumeStatus)

deleteVolumeResponseTest :: DeleteVolumeResponse -> TestTree
deleteVolumeResponseTest = resp
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse"
    (Proxy :: Proxy DeleteVolume)

describeImagesResponseTest :: DescribeImagesResponse -> TestTree
describeImagesResponseTest = resp
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse"
    (Proxy :: Proxy DescribeImages)

createKeyPairResponseTest :: CreateKeyPairResponse -> TestTree
createKeyPairResponseTest = resp
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse"
    (Proxy :: Proxy CreateKeyPair)

restoreAddressToClassicResponseTest :: RestoreAddressToClassicResponse -> TestTree
restoreAddressToClassicResponseTest = resp
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse"
    (Proxy :: Proxy RestoreAddressToClassic)

describeAvailabilityZonesResponseTest :: DescribeAvailabilityZonesResponse -> TestTree
describeAvailabilityZonesResponseTest = resp
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse"
    (Proxy :: Proxy DescribeAvailabilityZones)

importSnapshotResponseTest :: ImportSnapshotResponse -> TestTree
importSnapshotResponseTest = resp
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse"
    (Proxy :: Proxy ImportSnapshot)

acceptVPCPeeringConnectionResponseTest :: AcceptVPCPeeringConnectionResponse -> TestTree
acceptVPCPeeringConnectionResponseTest = resp
    "AcceptVPCPeeringConnectionResponse"
    "fixture/AcceptVPCPeeringConnectionResponse"
    (Proxy :: Proxy AcceptVPCPeeringConnection)

describeRouteTablesResponseTest :: DescribeRouteTablesResponse -> TestTree
describeRouteTablesResponseTest = resp
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse"
    (Proxy :: Proxy DescribeRouteTables)
