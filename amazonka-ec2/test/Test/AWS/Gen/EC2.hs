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
--         [ acceptVPCPeeringConnectionTest $
--             acceptVPCPeeringConnection
--
--         , allocateAddressTest $
--             allocateAddress
--
--         , assignPrivateIPAddressesTest $
--             assignPrivateIPAddresses
--
--         , associateAddressTest $
--             associateAddress
--
--         , associateDHCPOptionsTest $
--             associateDHCPOptions
--
--         , associateRouteTableTest $
--             associateRouteTable
--
--         , attachClassicLinkVPCTest $
--             attachClassicLinkVPC
--
--         , attachInternetGatewayTest $
--             attachInternetGateway
--
--         , attachNetworkInterfaceTest $
--             attachNetworkInterface
--
--         , attachVPNGatewayTest $
--             attachVPNGateway
--
--         , attachVolumeTest $
--             attachVolume
--
--         , authorizeSecurityGroupEgressTest $
--             authorizeSecurityGroupEgress
--
--         , authorizeSecurityGroupIngressTest $
--             authorizeSecurityGroupIngress
--
--         , bundleInstanceTest $
--             bundleInstance
--
--         , cancelBundleTaskTest $
--             cancelBundleTask
--
--         , cancelConversionTaskTest $
--             cancelConversionTask
--
--         , cancelExportTaskTest $
--             cancelExportTask
--
--         , cancelImportTaskTest $
--             cancelImportTask
--
--         , cancelReservedInstancesListingTest $
--             cancelReservedInstancesListing
--
--         , cancelSpotFleetRequestsTest $
--             cancelSpotFleetRequests
--
--         , cancelSpotInstanceRequestsTest $
--             cancelSpotInstanceRequests
--
--         , confirmProductInstanceTest $
--             confirmProductInstance
--
--         , copyImageTest $
--             copyImage
--
--         , copySnapshotTest $
--             copySnapshot
--
--         , createCustomerGatewayTest $
--             createCustomerGateway
--
--         , createDHCPOptionsTest $
--             createDHCPOptions
--
--         , createFlowLogsTest $
--             createFlowLogs
--
--         , createImageTest $
--             createImage
--
--         , createInstanceExportTaskTest $
--             createInstanceExportTask
--
--         , createInternetGatewayTest $
--             createInternetGateway
--
--         , createKeyPairTest $
--             createKeyPair
--
--         , createNetworkACLTest $
--             createNetworkACL
--
--         , createNetworkACLEntryTest $
--             createNetworkACLEntry
--
--         , createNetworkInterfaceTest $
--             createNetworkInterface
--
--         , createPlacementGroupTest $
--             createPlacementGroup
--
--         , createReservedInstancesListingTest $
--             createReservedInstancesListing
--
--         , createRouteTest $
--             createRoute
--
--         , createRouteTableTest $
--             createRouteTable
--
--         , createSecurityGroupTest $
--             createSecurityGroup
--
--         , createSnapshotTest $
--             createSnapshot
--
--         , createSpotDatafeedSubscriptionTest $
--             createSpotDatafeedSubscription
--
--         , createSubnetTest $
--             createSubnet
--
--         , createTagsTest $
--             createTags
--
--         , createVPCTest $
--             createVPC
--
--         , createVPCEndpointTest $
--             createVPCEndpoint
--
--         , createVPCPeeringConnectionTest $
--             createVPCPeeringConnection
--
--         , createVPNConnectionTest $
--             createVPNConnection
--
--         , createVPNConnectionRouteTest $
--             createVPNConnectionRoute
--
--         , createVPNGatewayTest $
--             createVPNGateway
--
--         , createVolumeTest $
--             createVolume
--
--         , deleteCustomerGatewayTest $
--             deleteCustomerGateway
--
--         , deleteDHCPOptionsTest $
--             deleteDHCPOptions
--
--         , deleteFlowLogsTest $
--             deleteFlowLogs
--
--         , deleteInternetGatewayTest $
--             deleteInternetGateway
--
--         , deleteKeyPairTest $
--             deleteKeyPair
--
--         , deleteNetworkACLTest $
--             deleteNetworkACL
--
--         , deleteNetworkACLEntryTest $
--             deleteNetworkACLEntry
--
--         , deleteNetworkInterfaceTest $
--             deleteNetworkInterface
--
--         , deletePlacementGroupTest $
--             deletePlacementGroup
--
--         , deleteRouteTest $
--             deleteRoute
--
--         , deleteRouteTableTest $
--             deleteRouteTable
--
--         , deleteSecurityGroupTest $
--             deleteSecurityGroup
--
--         , deleteSnapshotTest $
--             deleteSnapshot
--
--         , deleteSpotDatafeedSubscriptionTest $
--             deleteSpotDatafeedSubscription
--
--         , deleteSubnetTest $
--             deleteSubnet
--
--         , deleteTagsTest $
--             deleteTags
--
--         , deleteVPCTest $
--             deleteVPC
--
--         , deleteVPCEndpointsTest $
--             deleteVPCEndpoints
--
--         , deleteVPCPeeringConnectionTest $
--             deleteVPCPeeringConnection
--
--         , deleteVPNConnectionTest $
--             deleteVPNConnection
--
--         , deleteVPNConnectionRouteTest $
--             deleteVPNConnectionRoute
--
--         , deleteVPNGatewayTest $
--             deleteVPNGateway
--
--         , deleteVolumeTest $
--             deleteVolume
--
--         , deregisterImageTest $
--             deregisterImage
--
--         , describeAccountAttributesTest $
--             describeAccountAttributes
--
--         , describeAddressesTest $
--             describeAddresses
--
--         , describeAvailabilityZonesTest $
--             describeAvailabilityZones
--
--         , describeBundleTasksTest $
--             describeBundleTasks
--
--         , describeClassicLinkInstancesTest $
--             describeClassicLinkInstances
--
--         , describeConversionTasksTest $
--             describeConversionTasks
--
--         , describeCustomerGatewaysTest $
--             describeCustomerGateways
--
--         , describeDHCPOptionsTest $
--             describeDHCPOptions
--
--         , describeExportTasksTest $
--             describeExportTasks
--
--         , describeFlowLogsTest $
--             describeFlowLogs
--
--         , describeImageAttributeTest $
--             describeImageAttribute
--
--         , describeImagesTest $
--             describeImages
--
--         , describeImportImageTasksTest $
--             describeImportImageTasks
--
--         , describeImportSnapshotTasksTest $
--             describeImportSnapshotTasks
--
--         , describeInstanceAttributeTest $
--             describeInstanceAttribute
--
--         , describeInstanceStatusTest $
--             describeInstanceStatus
--
--         , describeInstancesTest $
--             describeInstances
--
--         , describeInternetGatewaysTest $
--             describeInternetGateways
--
--         , describeKeyPairsTest $
--             describeKeyPairs
--
--         , describeMovingAddressesTest $
--             describeMovingAddresses
--
--         , describeNetworkACLsTest $
--             describeNetworkACLs
--
--         , describeNetworkInterfaceAttributeTest $
--             describeNetworkInterfaceAttribute
--
--         , describeNetworkInterfacesTest $
--             describeNetworkInterfaces
--
--         , describePlacementGroupsTest $
--             describePlacementGroups
--
--         , describePrefixListsTest $
--             describePrefixLists
--
--         , describeRegionsTest $
--             describeRegions
--
--         , describeReservedInstancesTest $
--             describeReservedInstances
--
--         , describeReservedInstancesListingsTest $
--             describeReservedInstancesListings
--
--         , describeReservedInstancesModificationsTest $
--             describeReservedInstancesModifications
--
--         , describeReservedInstancesOfferingsTest $
--             describeReservedInstancesOfferings
--
--         , describeRouteTablesTest $
--             describeRouteTables
--
--         , describeSecurityGroupsTest $
--             describeSecurityGroups
--
--         , describeSnapshotAttributeTest $
--             describeSnapshotAttribute
--
--         , describeSnapshotsTest $
--             describeSnapshots
--
--         , describeSpotDatafeedSubscriptionTest $
--             describeSpotDatafeedSubscription
--
--         , describeSpotFleetInstancesTest $
--             describeSpotFleetInstances
--
--         , describeSpotFleetRequestHistoryTest $
--             describeSpotFleetRequestHistory
--
--         , describeSpotFleetRequestsTest $
--             describeSpotFleetRequests
--
--         , describeSpotInstanceRequestsTest $
--             describeSpotInstanceRequests
--
--         , describeSpotPriceHistoryTest $
--             describeSpotPriceHistory
--
--         , describeSubnetsTest $
--             describeSubnets
--
--         , describeTagsTest $
--             describeTags
--
--         , describeVPCAttributeTest $
--             describeVPCAttribute
--
--         , describeVPCClassicLinkTest $
--             describeVPCClassicLink
--
--         , describeVPCEndpointServicesTest $
--             describeVPCEndpointServices
--
--         , describeVPCEndpointsTest $
--             describeVPCEndpoints
--
--         , describeVPCPeeringConnectionsTest $
--             describeVPCPeeringConnections
--
--         , describeVPCsTest $
--             describeVPCs
--
--         , describeVPNConnectionsTest $
--             describeVPNConnections
--
--         , describeVPNGatewaysTest $
--             describeVPNGateways
--
--         , describeVolumeAttributeTest $
--             describeVolumeAttribute
--
--         , describeVolumeStatusTest $
--             describeVolumeStatus
--
--         , describeVolumesTest $
--             describeVolumes
--
--         , detachClassicLinkVPCTest $
--             detachClassicLinkVPC
--
--         , detachInternetGatewayTest $
--             detachInternetGateway
--
--         , detachNetworkInterfaceTest $
--             detachNetworkInterface
--
--         , detachVPNGatewayTest $
--             detachVPNGateway
--
--         , detachVolumeTest $
--             detachVolume
--
--         , disableVGWRoutePropagationTest $
--             disableVGWRoutePropagation
--
--         , disableVPCClassicLinkTest $
--             disableVPCClassicLink
--
--         , disassociateAddressTest $
--             disassociateAddress
--
--         , disassociateRouteTableTest $
--             disassociateRouteTable
--
--         , enableVGWRoutePropagationTest $
--             enableVGWRoutePropagation
--
--         , enableVPCClassicLinkTest $
--             enableVPCClassicLink
--
--         , enableVolumeIOTest $
--             enableVolumeIO
--
--         , getConsoleOutputTest $
--             getConsoleOutput
--
--         , getPasswordDataTest $
--             getPasswordData
--
--         , importImageTest $
--             importImage
--
--         , importInstanceTest $
--             importInstance
--
--         , importKeyPairTest $
--             importKeyPair
--
--         , importSnapshotTest $
--             importSnapshot
--
--         , importVolumeTest $
--             importVolume
--
--         , modifyImageAttributeTest $
--             modifyImageAttribute
--
--         , modifyInstanceAttributeTest $
--             modifyInstanceAttribute
--
--         , modifyNetworkInterfaceAttributeTest $
--             modifyNetworkInterfaceAttribute
--
--         , modifyReservedInstancesTest $
--             modifyReservedInstances
--
--         , modifySnapshotAttributeTest $
--             modifySnapshotAttribute
--
--         , modifySubnetAttributeTest $
--             modifySubnetAttribute
--
--         , modifyVPCAttributeTest $
--             modifyVPCAttribute
--
--         , modifyVPCEndpointTest $
--             modifyVPCEndpoint
--
--         , modifyVolumeAttributeTest $
--             modifyVolumeAttribute
--
--         , monitorInstancesTest $
--             monitorInstances
--
--         , moveAddressToVPCTest $
--             moveAddressToVPC
--
--         , purchaseReservedInstancesOfferingTest $
--             purchaseReservedInstancesOffering
--
--         , rebootInstancesTest $
--             rebootInstances
--
--         , registerImageTest $
--             registerImage
--
--         , rejectVPCPeeringConnectionTest $
--             rejectVPCPeeringConnection
--
--         , releaseAddressTest $
--             releaseAddress
--
--         , replaceNetworkACLAssociationTest $
--             replaceNetworkACLAssociation
--
--         , replaceNetworkACLEntryTest $
--             replaceNetworkACLEntry
--
--         , replaceRouteTest $
--             replaceRoute
--
--         , replaceRouteTableAssociationTest $
--             replaceRouteTableAssociation
--
--         , reportInstanceStatusTest $
--             reportInstanceStatus
--
--         , requestSpotFleetTest $
--             requestSpotFleet
--
--         , requestSpotInstancesTest $
--             requestSpotInstances
--
--         , resetImageAttributeTest $
--             resetImageAttribute
--
--         , resetInstanceAttributeTest $
--             resetInstanceAttribute
--
--         , resetNetworkInterfaceAttributeTest $
--             resetNetworkInterfaceAttribute
--
--         , resetSnapshotAttributeTest $
--             resetSnapshotAttribute
--
--         , restoreAddressToClassicTest $
--             restoreAddressToClassic
--
--         , revokeSecurityGroupEgressTest $
--             revokeSecurityGroupEgress
--
--         , revokeSecurityGroupIngressTest $
--             revokeSecurityGroupIngress
--
--         , runInstancesTest $
--             runInstances
--
--         , startInstancesTest $
--             startInstances
--
--         , stopInstancesTest $
--             stopInstances
--
--         , terminateInstancesTest $
--             terminateInstances
--
--         , unassignPrivateIPAddressesTest $
--             unassignPrivateIPAddresses
--
--         , unmonitorInstancesTest $
--             unmonitorInstances
--
--           ]

--     , testGroup "response"
--         [ acceptVPCPeeringConnectionResponseTest $
--             acceptVPCPeeringConnectionResponse
--
--         , allocateAddressResponseTest $
--             allocateAddressResponse
--
--         , assignPrivateIPAddressesResponseTest $
--             assignPrivateIPAddressesResponse
--
--         , associateAddressResponseTest $
--             associateAddressResponse
--
--         , associateDHCPOptionsResponseTest $
--             associateDHCPOptionsResponse
--
--         , associateRouteTableResponseTest $
--             associateRouteTableResponse
--
--         , attachClassicLinkVPCResponseTest $
--             attachClassicLinkVPCResponse
--
--         , attachInternetGatewayResponseTest $
--             attachInternetGatewayResponse
--
--         , attachNetworkInterfaceResponseTest $
--             attachNetworkInterfaceResponse
--
--         , attachVPNGatewayResponseTest $
--             attachVPNGatewayResponse
--
--         , attachVolumeResponseTest $
--             volumeAttachment
--
--         , authorizeSecurityGroupEgressResponseTest $
--             authorizeSecurityGroupEgressResponse
--
--         , authorizeSecurityGroupIngressResponseTest $
--             authorizeSecurityGroupIngressResponse
--
--         , bundleInstanceResponseTest $
--             bundleInstanceResponse
--
--         , cancelBundleTaskResponseTest $
--             cancelBundleTaskResponse
--
--         , cancelConversionTaskResponseTest $
--             cancelConversionTaskResponse
--
--         , cancelExportTaskResponseTest $
--             cancelExportTaskResponse
--
--         , cancelImportTaskResponseTest $
--             cancelImportTaskResponse
--
--         , cancelReservedInstancesListingResponseTest $
--             cancelReservedInstancesListingResponse
--
--         , cancelSpotFleetRequestsResponseTest $
--             cancelSpotFleetRequestsResponse
--
--         , cancelSpotInstanceRequestsResponseTest $
--             cancelSpotInstanceRequestsResponse
--
--         , confirmProductInstanceResponseTest $
--             confirmProductInstanceResponse
--
--         , copyImageResponseTest $
--             copyImageResponse
--
--         , copySnapshotResponseTest $
--             copySnapshotResponse
--
--         , createCustomerGatewayResponseTest $
--             createCustomerGatewayResponse
--
--         , createDHCPOptionsResponseTest $
--             createDHCPOptionsResponse
--
--         , createFlowLogsResponseTest $
--             createFlowLogsResponse
--
--         , createImageResponseTest $
--             createImageResponse
--
--         , createInstanceExportTaskResponseTest $
--             createInstanceExportTaskResponse
--
--         , createInternetGatewayResponseTest $
--             createInternetGatewayResponse
--
--         , createKeyPairResponseTest $
--             createKeyPairResponse
--
--         , createNetworkACLResponseTest $
--             createNetworkACLResponse
--
--         , createNetworkACLEntryResponseTest $
--             createNetworkACLEntryResponse
--
--         , createNetworkInterfaceResponseTest $
--             createNetworkInterfaceResponse
--
--         , createPlacementGroupResponseTest $
--             createPlacementGroupResponse
--
--         , createReservedInstancesListingResponseTest $
--             createReservedInstancesListingResponse
--
--         , createRouteResponseTest $
--             createRouteResponse
--
--         , createRouteTableResponseTest $
--             createRouteTableResponse
--
--         , createSecurityGroupResponseTest $
--             createSecurityGroupResponse
--
--         , createSnapshotResponseTest $
--             snapshot
--
--         , createSpotDatafeedSubscriptionResponseTest $
--             createSpotDatafeedSubscriptionResponse
--
--         , createSubnetResponseTest $
--             createSubnetResponse
--
--         , createTagsResponseTest $
--             createTagsResponse
--
--         , createVPCResponseTest $
--             createVPCResponse
--
--         , createVPCEndpointResponseTest $
--             createVPCEndpointResponse
--
--         , createVPCPeeringConnectionResponseTest $
--             createVPCPeeringConnectionResponse
--
--         , createVPNConnectionResponseTest $
--             createVPNConnectionResponse
--
--         , createVPNConnectionRouteResponseTest $
--             createVPNConnectionRouteResponse
--
--         , createVPNGatewayResponseTest $
--             createVPNGatewayResponse
--
--         , createVolumeResponseTest $
--             volume
--
--         , deleteCustomerGatewayResponseTest $
--             deleteCustomerGatewayResponse
--
--         , deleteDHCPOptionsResponseTest $
--             deleteDHCPOptionsResponse
--
--         , deleteFlowLogsResponseTest $
--             deleteFlowLogsResponse
--
--         , deleteInternetGatewayResponseTest $
--             deleteInternetGatewayResponse
--
--         , deleteKeyPairResponseTest $
--             deleteKeyPairResponse
--
--         , deleteNetworkACLResponseTest $
--             deleteNetworkACLResponse
--
--         , deleteNetworkACLEntryResponseTest $
--             deleteNetworkACLEntryResponse
--
--         , deleteNetworkInterfaceResponseTest $
--             deleteNetworkInterfaceResponse
--
--         , deletePlacementGroupResponseTest $
--             deletePlacementGroupResponse
--
--         , deleteRouteResponseTest $
--             deleteRouteResponse
--
--         , deleteRouteTableResponseTest $
--             deleteRouteTableResponse
--
--         , deleteSecurityGroupResponseTest $
--             deleteSecurityGroupResponse
--
--         , deleteSnapshotResponseTest $
--             deleteSnapshotResponse
--
--         , deleteSpotDatafeedSubscriptionResponseTest $
--             deleteSpotDatafeedSubscriptionResponse
--
--         , deleteSubnetResponseTest $
--             deleteSubnetResponse
--
--         , deleteTagsResponseTest $
--             deleteTagsResponse
--
--         , deleteVPCResponseTest $
--             deleteVPCResponse
--
--         , deleteVPCEndpointsResponseTest $
--             deleteVPCEndpointsResponse
--
--         , deleteVPCPeeringConnectionResponseTest $
--             deleteVPCPeeringConnectionResponse
--
--         , deleteVPNConnectionResponseTest $
--             deleteVPNConnectionResponse
--
--         , deleteVPNConnectionRouteResponseTest $
--             deleteVPNConnectionRouteResponse
--
--         , deleteVPNGatewayResponseTest $
--             deleteVPNGatewayResponse
--
--         , deleteVolumeResponseTest $
--             deleteVolumeResponse
--
--         , deregisterImageResponseTest $
--             deregisterImageResponse
--
--         , describeAccountAttributesResponseTest $
--             describeAccountAttributesResponse
--
--         , describeAddressesResponseTest $
--             describeAddressesResponse
--
--         , describeAvailabilityZonesResponseTest $
--             describeAvailabilityZonesResponse
--
--         , describeBundleTasksResponseTest $
--             describeBundleTasksResponse
--
--         , describeClassicLinkInstancesResponseTest $
--             describeClassicLinkInstancesResponse
--
--         , describeConversionTasksResponseTest $
--             describeConversionTasksResponse
--
--         , describeCustomerGatewaysResponseTest $
--             describeCustomerGatewaysResponse
--
--         , describeDHCPOptionsResponseTest $
--             describeDHCPOptionsResponse
--
--         , describeExportTasksResponseTest $
--             describeExportTasksResponse
--
--         , describeFlowLogsResponseTest $
--             describeFlowLogsResponse
--
--         , describeImageAttributeResponseTest $
--             describeImageAttributeResponse
--
--         , describeImagesResponseTest $
--             describeImagesResponse
--
--         , describeImportImageTasksResponseTest $
--             describeImportImageTasksResponse
--
--         , describeImportSnapshotTasksResponseTest $
--             describeImportSnapshotTasksResponse
--
--         , describeInstanceAttributeResponseTest $
--             describeInstanceAttributeResponse
--
--         , describeInstanceStatusResponseTest $
--             describeInstanceStatusResponse
--
--         , describeInstancesResponseTest $
--             describeInstancesResponse
--
--         , describeInternetGatewaysResponseTest $
--             describeInternetGatewaysResponse
--
--         , describeKeyPairsResponseTest $
--             describeKeyPairsResponse
--
--         , describeMovingAddressesResponseTest $
--             describeMovingAddressesResponse
--
--         , describeNetworkACLsResponseTest $
--             describeNetworkACLsResponse
--
--         , describeNetworkInterfaceAttributeResponseTest $
--             describeNetworkInterfaceAttributeResponse
--
--         , describeNetworkInterfacesResponseTest $
--             describeNetworkInterfacesResponse
--
--         , describePlacementGroupsResponseTest $
--             describePlacementGroupsResponse
--
--         , describePrefixListsResponseTest $
--             describePrefixListsResponse
--
--         , describeRegionsResponseTest $
--             describeRegionsResponse
--
--         , describeReservedInstancesResponseTest $
--             describeReservedInstancesResponse
--
--         , describeReservedInstancesListingsResponseTest $
--             describeReservedInstancesListingsResponse
--
--         , describeReservedInstancesModificationsResponseTest $
--             describeReservedInstancesModificationsResponse
--
--         , describeReservedInstancesOfferingsResponseTest $
--             describeReservedInstancesOfferingsResponse
--
--         , describeRouteTablesResponseTest $
--             describeRouteTablesResponse
--
--         , describeSecurityGroupsResponseTest $
--             describeSecurityGroupsResponse
--
--         , describeSnapshotAttributeResponseTest $
--             describeSnapshotAttributeResponse
--
--         , describeSnapshotsResponseTest $
--             describeSnapshotsResponse
--
--         , describeSpotDatafeedSubscriptionResponseTest $
--             describeSpotDatafeedSubscriptionResponse
--
--         , describeSpotFleetInstancesResponseTest $
--             describeSpotFleetInstancesResponse
--
--         , describeSpotFleetRequestHistoryResponseTest $
--             describeSpotFleetRequestHistoryResponse
--
--         , describeSpotFleetRequestsResponseTest $
--             describeSpotFleetRequestsResponse
--
--         , describeSpotInstanceRequestsResponseTest $
--             describeSpotInstanceRequestsResponse
--
--         , describeSpotPriceHistoryResponseTest $
--             describeSpotPriceHistoryResponse
--
--         , describeSubnetsResponseTest $
--             describeSubnetsResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , describeVPCAttributeResponseTest $
--             describeVPCAttributeResponse
--
--         , describeVPCClassicLinkResponseTest $
--             describeVPCClassicLinkResponse
--
--         , describeVPCEndpointServicesResponseTest $
--             describeVPCEndpointServicesResponse
--
--         , describeVPCEndpointsResponseTest $
--             describeVPCEndpointsResponse
--
--         , describeVPCPeeringConnectionsResponseTest $
--             describeVPCPeeringConnectionsResponse
--
--         , describeVPCsResponseTest $
--             describeVPCsResponse
--
--         , describeVPNConnectionsResponseTest $
--             describeVPNConnectionsResponse
--
--         , describeVPNGatewaysResponseTest $
--             describeVPNGatewaysResponse
--
--         , describeVolumeAttributeResponseTest $
--             describeVolumeAttributeResponse
--
--         , describeVolumeStatusResponseTest $
--             describeVolumeStatusResponse
--
--         , describeVolumesResponseTest $
--             describeVolumesResponse
--
--         , detachClassicLinkVPCResponseTest $
--             detachClassicLinkVPCResponse
--
--         , detachInternetGatewayResponseTest $
--             detachInternetGatewayResponse
--
--         , detachNetworkInterfaceResponseTest $
--             detachNetworkInterfaceResponse
--
--         , detachVPNGatewayResponseTest $
--             detachVPNGatewayResponse
--
--         , detachVolumeResponseTest $
--             volumeAttachment
--
--         , disableVGWRoutePropagationResponseTest $
--             disableVGWRoutePropagationResponse
--
--         , disableVPCClassicLinkResponseTest $
--             disableVPCClassicLinkResponse
--
--         , disassociateAddressResponseTest $
--             disassociateAddressResponse
--
--         , disassociateRouteTableResponseTest $
--             disassociateRouteTableResponse
--
--         , enableVGWRoutePropagationResponseTest $
--             enableVGWRoutePropagationResponse
--
--         , enableVPCClassicLinkResponseTest $
--             enableVPCClassicLinkResponse
--
--         , enableVolumeIOResponseTest $
--             enableVolumeIOResponse
--
--         , getConsoleOutputResponseTest $
--             getConsoleOutputResponse
--
--         , getPasswordDataResponseTest $
--             getPasswordDataResponse
--
--         , importImageResponseTest $
--             importImageResponse
--
--         , importInstanceResponseTest $
--             importInstanceResponse
--
--         , importKeyPairResponseTest $
--             importKeyPairResponse
--
--         , importSnapshotResponseTest $
--             importSnapshotResponse
--
--         , importVolumeResponseTest $
--             importVolumeResponse
--
--         , modifyImageAttributeResponseTest $
--             modifyImageAttributeResponse
--
--         , modifyInstanceAttributeResponseTest $
--             modifyInstanceAttributeResponse
--
--         , modifyNetworkInterfaceAttributeResponseTest $
--             modifyNetworkInterfaceAttributeResponse
--
--         , modifyReservedInstancesResponseTest $
--             modifyReservedInstancesResponse
--
--         , modifySnapshotAttributeResponseTest $
--             modifySnapshotAttributeResponse
--
--         , modifySubnetAttributeResponseTest $
--             modifySubnetAttributeResponse
--
--         , modifyVPCAttributeResponseTest $
--             modifyVPCAttributeResponse
--
--         , modifyVPCEndpointResponseTest $
--             modifyVPCEndpointResponse
--
--         , modifyVolumeAttributeResponseTest $
--             modifyVolumeAttributeResponse
--
--         , monitorInstancesResponseTest $
--             monitorInstancesResponse
--
--         , moveAddressToVPCResponseTest $
--             moveAddressToVPCResponse
--
--         , purchaseReservedInstancesOfferingResponseTest $
--             purchaseReservedInstancesOfferingResponse
--
--         , rebootInstancesResponseTest $
--             rebootInstancesResponse
--
--         , registerImageResponseTest $
--             registerImageResponse
--
--         , rejectVPCPeeringConnectionResponseTest $
--             rejectVPCPeeringConnectionResponse
--
--         , releaseAddressResponseTest $
--             releaseAddressResponse
--
--         , replaceNetworkACLAssociationResponseTest $
--             replaceNetworkACLAssociationResponse
--
--         , replaceNetworkACLEntryResponseTest $
--             replaceNetworkACLEntryResponse
--
--         , replaceRouteResponseTest $
--             replaceRouteResponse
--
--         , replaceRouteTableAssociationResponseTest $
--             replaceRouteTableAssociationResponse
--
--         , reportInstanceStatusResponseTest $
--             reportInstanceStatusResponse
--
--         , requestSpotFleetResponseTest $
--             requestSpotFleetResponse
--
--         , requestSpotInstancesResponseTest $
--             requestSpotInstancesResponse
--
--         , resetImageAttributeResponseTest $
--             resetImageAttributeResponse
--
--         , resetInstanceAttributeResponseTest $
--             resetInstanceAttributeResponse
--
--         , resetNetworkInterfaceAttributeResponseTest $
--             resetNetworkInterfaceAttributeResponse
--
--         , resetSnapshotAttributeResponseTest $
--             resetSnapshotAttributeResponse
--
--         , restoreAddressToClassicResponseTest $
--             restoreAddressToClassicResponse
--
--         , revokeSecurityGroupEgressResponseTest $
--             revokeSecurityGroupEgressResponse
--
--         , revokeSecurityGroupIngressResponseTest $
--             revokeSecurityGroupIngressResponse
--
--         , runInstancesResponseTest $
--             reservation
--
--         , startInstancesResponseTest $
--             startInstancesResponse
--
--         , stopInstancesResponseTest $
--             stopInstancesResponse
--
--         , terminateInstancesResponseTest $
--             terminateInstancesResponse
--
--         , unassignPrivateIPAddressesResponseTest $
--             unassignPrivateIPAddressesResponse
--
--         , unmonitorInstancesResponseTest $
--             unmonitorInstancesResponse
--
--           ]
--     ]

-- Requests

acceptVPCPeeringConnectionTest :: AcceptVPCPeeringConnection -> TestTree
acceptVPCPeeringConnectionTest = undefined

allocateAddressTest :: AllocateAddress -> TestTree
allocateAddressTest = undefined

assignPrivateIPAddressesTest :: AssignPrivateIPAddresses -> TestTree
assignPrivateIPAddressesTest = undefined

associateAddressTest :: AssociateAddress -> TestTree
associateAddressTest = undefined

associateDHCPOptionsTest :: AssociateDHCPOptions -> TestTree
associateDHCPOptionsTest = undefined

associateRouteTableTest :: AssociateRouteTable -> TestTree
associateRouteTableTest = undefined

attachClassicLinkVPCTest :: AttachClassicLinkVPC -> TestTree
attachClassicLinkVPCTest = undefined

attachInternetGatewayTest :: AttachInternetGateway -> TestTree
attachInternetGatewayTest = undefined

attachNetworkInterfaceTest :: AttachNetworkInterface -> TestTree
attachNetworkInterfaceTest = undefined

attachVPNGatewayTest :: AttachVPNGateway -> TestTree
attachVPNGatewayTest = undefined

attachVolumeTest :: AttachVolume -> TestTree
attachVolumeTest = undefined

authorizeSecurityGroupEgressTest :: AuthorizeSecurityGroupEgress -> TestTree
authorizeSecurityGroupEgressTest = undefined

authorizeSecurityGroupIngressTest :: AuthorizeSecurityGroupIngress -> TestTree
authorizeSecurityGroupIngressTest = undefined

bundleInstanceTest :: BundleInstance -> TestTree
bundleInstanceTest = undefined

cancelBundleTaskTest :: CancelBundleTask -> TestTree
cancelBundleTaskTest = undefined

cancelConversionTaskTest :: CancelConversionTask -> TestTree
cancelConversionTaskTest = undefined

cancelExportTaskTest :: CancelExportTask -> TestTree
cancelExportTaskTest = undefined

cancelImportTaskTest :: CancelImportTask -> TestTree
cancelImportTaskTest = undefined

cancelReservedInstancesListingTest :: CancelReservedInstancesListing -> TestTree
cancelReservedInstancesListingTest = undefined

cancelSpotFleetRequestsTest :: CancelSpotFleetRequests -> TestTree
cancelSpotFleetRequestsTest = undefined

cancelSpotInstanceRequestsTest :: CancelSpotInstanceRequests -> TestTree
cancelSpotInstanceRequestsTest = undefined

confirmProductInstanceTest :: ConfirmProductInstance -> TestTree
confirmProductInstanceTest = undefined

copyImageTest :: CopyImage -> TestTree
copyImageTest = undefined

copySnapshotTest :: CopySnapshot -> TestTree
copySnapshotTest = undefined

createCustomerGatewayTest :: CreateCustomerGateway -> TestTree
createCustomerGatewayTest = undefined

createDHCPOptionsTest :: CreateDHCPOptions -> TestTree
createDHCPOptionsTest = undefined

createFlowLogsTest :: CreateFlowLogs -> TestTree
createFlowLogsTest = undefined

createImageTest :: CreateImage -> TestTree
createImageTest = undefined

createInstanceExportTaskTest :: CreateInstanceExportTask -> TestTree
createInstanceExportTaskTest = undefined

createInternetGatewayTest :: CreateInternetGateway -> TestTree
createInternetGatewayTest = undefined

createKeyPairTest :: CreateKeyPair -> TestTree
createKeyPairTest = undefined

createNetworkACLTest :: CreateNetworkACL -> TestTree
createNetworkACLTest = undefined

createNetworkACLEntryTest :: CreateNetworkACLEntry -> TestTree
createNetworkACLEntryTest = undefined

createNetworkInterfaceTest :: CreateNetworkInterface -> TestTree
createNetworkInterfaceTest = undefined

createPlacementGroupTest :: CreatePlacementGroup -> TestTree
createPlacementGroupTest = undefined

createReservedInstancesListingTest :: CreateReservedInstancesListing -> TestTree
createReservedInstancesListingTest = undefined

createRouteTest :: CreateRoute -> TestTree
createRouteTest = undefined

createRouteTableTest :: CreateRouteTable -> TestTree
createRouteTableTest = undefined

createSecurityGroupTest :: CreateSecurityGroup -> TestTree
createSecurityGroupTest = undefined

createSnapshotTest :: CreateSnapshot -> TestTree
createSnapshotTest = undefined

createSpotDatafeedSubscriptionTest :: CreateSpotDatafeedSubscription -> TestTree
createSpotDatafeedSubscriptionTest = undefined

createSubnetTest :: CreateSubnet -> TestTree
createSubnetTest = undefined

createTagsTest :: CreateTags -> TestTree
createTagsTest = undefined

createVPCTest :: CreateVPC -> TestTree
createVPCTest = undefined

createVPCEndpointTest :: CreateVPCEndpoint -> TestTree
createVPCEndpointTest = undefined

createVPCPeeringConnectionTest :: CreateVPCPeeringConnection -> TestTree
createVPCPeeringConnectionTest = undefined

createVPNConnectionTest :: CreateVPNConnection -> TestTree
createVPNConnectionTest = undefined

createVPNConnectionRouteTest :: CreateVPNConnectionRoute -> TestTree
createVPNConnectionRouteTest = undefined

createVPNGatewayTest :: CreateVPNGateway -> TestTree
createVPNGatewayTest = undefined

createVolumeTest :: CreateVolume -> TestTree
createVolumeTest = undefined

deleteCustomerGatewayTest :: DeleteCustomerGateway -> TestTree
deleteCustomerGatewayTest = undefined

deleteDHCPOptionsTest :: DeleteDHCPOptions -> TestTree
deleteDHCPOptionsTest = undefined

deleteFlowLogsTest :: DeleteFlowLogs -> TestTree
deleteFlowLogsTest = undefined

deleteInternetGatewayTest :: DeleteInternetGateway -> TestTree
deleteInternetGatewayTest = undefined

deleteKeyPairTest :: DeleteKeyPair -> TestTree
deleteKeyPairTest = undefined

deleteNetworkACLTest :: DeleteNetworkACL -> TestTree
deleteNetworkACLTest = undefined

deleteNetworkACLEntryTest :: DeleteNetworkACLEntry -> TestTree
deleteNetworkACLEntryTest = undefined

deleteNetworkInterfaceTest :: DeleteNetworkInterface -> TestTree
deleteNetworkInterfaceTest = undefined

deletePlacementGroupTest :: DeletePlacementGroup -> TestTree
deletePlacementGroupTest = undefined

deleteRouteTest :: DeleteRoute -> TestTree
deleteRouteTest = undefined

deleteRouteTableTest :: DeleteRouteTable -> TestTree
deleteRouteTableTest = undefined

deleteSecurityGroupTest :: DeleteSecurityGroup -> TestTree
deleteSecurityGroupTest = undefined

deleteSnapshotTest :: DeleteSnapshot -> TestTree
deleteSnapshotTest = undefined

deleteSpotDatafeedSubscriptionTest :: DeleteSpotDatafeedSubscription -> TestTree
deleteSpotDatafeedSubscriptionTest = undefined

deleteSubnetTest :: DeleteSubnet -> TestTree
deleteSubnetTest = undefined

deleteTagsTest :: DeleteTags -> TestTree
deleteTagsTest = undefined

deleteVPCTest :: DeleteVPC -> TestTree
deleteVPCTest = undefined

deleteVPCEndpointsTest :: DeleteVPCEndpoints -> TestTree
deleteVPCEndpointsTest = undefined

deleteVPCPeeringConnectionTest :: DeleteVPCPeeringConnection -> TestTree
deleteVPCPeeringConnectionTest = undefined

deleteVPNConnectionTest :: DeleteVPNConnection -> TestTree
deleteVPNConnectionTest = undefined

deleteVPNConnectionRouteTest :: DeleteVPNConnectionRoute -> TestTree
deleteVPNConnectionRouteTest = undefined

deleteVPNGatewayTest :: DeleteVPNGateway -> TestTree
deleteVPNGatewayTest = undefined

deleteVolumeTest :: DeleteVolume -> TestTree
deleteVolumeTest = undefined

deregisterImageTest :: DeregisterImage -> TestTree
deregisterImageTest = undefined

describeAccountAttributesTest :: DescribeAccountAttributes -> TestTree
describeAccountAttributesTest = undefined

describeAddressesTest :: DescribeAddresses -> TestTree
describeAddressesTest = undefined

describeAvailabilityZonesTest :: DescribeAvailabilityZones -> TestTree
describeAvailabilityZonesTest = undefined

describeBundleTasksTest :: DescribeBundleTasks -> TestTree
describeBundleTasksTest = undefined

describeClassicLinkInstancesTest :: DescribeClassicLinkInstances -> TestTree
describeClassicLinkInstancesTest = undefined

describeConversionTasksTest :: DescribeConversionTasks -> TestTree
describeConversionTasksTest = undefined

describeCustomerGatewaysTest :: DescribeCustomerGateways -> TestTree
describeCustomerGatewaysTest = undefined

describeDHCPOptionsTest :: DescribeDHCPOptions -> TestTree
describeDHCPOptionsTest = undefined

describeExportTasksTest :: DescribeExportTasks -> TestTree
describeExportTasksTest = undefined

describeFlowLogsTest :: DescribeFlowLogs -> TestTree
describeFlowLogsTest = undefined

describeImageAttributeTest :: DescribeImageAttribute -> TestTree
describeImageAttributeTest = undefined

describeImagesTest :: DescribeImages -> TestTree
describeImagesTest = undefined

describeImportImageTasksTest :: DescribeImportImageTasks -> TestTree
describeImportImageTasksTest = undefined

describeImportSnapshotTasksTest :: DescribeImportSnapshotTasks -> TestTree
describeImportSnapshotTasksTest = undefined

describeInstanceAttributeTest :: DescribeInstanceAttribute -> TestTree
describeInstanceAttributeTest = undefined

describeInstanceStatusTest :: DescribeInstanceStatus -> TestTree
describeInstanceStatusTest = undefined

describeInstancesTest :: DescribeInstances -> TestTree
describeInstancesTest = undefined

describeInternetGatewaysTest :: DescribeInternetGateways -> TestTree
describeInternetGatewaysTest = undefined

describeKeyPairsTest :: DescribeKeyPairs -> TestTree
describeKeyPairsTest = undefined

describeMovingAddressesTest :: DescribeMovingAddresses -> TestTree
describeMovingAddressesTest = undefined

describeNetworkACLsTest :: DescribeNetworkACLs -> TestTree
describeNetworkACLsTest = undefined

describeNetworkInterfaceAttributeTest :: DescribeNetworkInterfaceAttribute -> TestTree
describeNetworkInterfaceAttributeTest = undefined

describeNetworkInterfacesTest :: DescribeNetworkInterfaces -> TestTree
describeNetworkInterfacesTest = undefined

describePlacementGroupsTest :: DescribePlacementGroups -> TestTree
describePlacementGroupsTest = undefined

describePrefixListsTest :: DescribePrefixLists -> TestTree
describePrefixListsTest = undefined

describeRegionsTest :: DescribeRegions -> TestTree
describeRegionsTest = undefined

describeReservedInstancesTest :: DescribeReservedInstances -> TestTree
describeReservedInstancesTest = undefined

describeReservedInstancesListingsTest :: DescribeReservedInstancesListings -> TestTree
describeReservedInstancesListingsTest = undefined

describeReservedInstancesModificationsTest :: DescribeReservedInstancesModifications -> TestTree
describeReservedInstancesModificationsTest = undefined

describeReservedInstancesOfferingsTest :: DescribeReservedInstancesOfferings -> TestTree
describeReservedInstancesOfferingsTest = undefined

describeRouteTablesTest :: DescribeRouteTables -> TestTree
describeRouteTablesTest = undefined

describeSecurityGroupsTest :: DescribeSecurityGroups -> TestTree
describeSecurityGroupsTest = undefined

describeSnapshotAttributeTest :: DescribeSnapshotAttribute -> TestTree
describeSnapshotAttributeTest = undefined

describeSnapshotsTest :: DescribeSnapshots -> TestTree
describeSnapshotsTest = undefined

describeSpotDatafeedSubscriptionTest :: DescribeSpotDatafeedSubscription -> TestTree
describeSpotDatafeedSubscriptionTest = undefined

describeSpotFleetInstancesTest :: DescribeSpotFleetInstances -> TestTree
describeSpotFleetInstancesTest = undefined

describeSpotFleetRequestHistoryTest :: DescribeSpotFleetRequestHistory -> TestTree
describeSpotFleetRequestHistoryTest = undefined

describeSpotFleetRequestsTest :: DescribeSpotFleetRequests -> TestTree
describeSpotFleetRequestsTest = undefined

describeSpotInstanceRequestsTest :: DescribeSpotInstanceRequests -> TestTree
describeSpotInstanceRequestsTest = undefined

describeSpotPriceHistoryTest :: DescribeSpotPriceHistory -> TestTree
describeSpotPriceHistoryTest = undefined

describeSubnetsTest :: DescribeSubnets -> TestTree
describeSubnetsTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

describeVPCAttributeTest :: DescribeVPCAttribute -> TestTree
describeVPCAttributeTest = undefined

describeVPCClassicLinkTest :: DescribeVPCClassicLink -> TestTree
describeVPCClassicLinkTest = undefined

describeVPCEndpointServicesTest :: DescribeVPCEndpointServices -> TestTree
describeVPCEndpointServicesTest = undefined

describeVPCEndpointsTest :: DescribeVPCEndpoints -> TestTree
describeVPCEndpointsTest = undefined

describeVPCPeeringConnectionsTest :: DescribeVPCPeeringConnections -> TestTree
describeVPCPeeringConnectionsTest = undefined

describeVPCsTest :: DescribeVPCs -> TestTree
describeVPCsTest = undefined

describeVPNConnectionsTest :: DescribeVPNConnections -> TestTree
describeVPNConnectionsTest = undefined

describeVPNGatewaysTest :: DescribeVPNGateways -> TestTree
describeVPNGatewaysTest = undefined

describeVolumeAttributeTest :: DescribeVolumeAttribute -> TestTree
describeVolumeAttributeTest = undefined

describeVolumeStatusTest :: DescribeVolumeStatus -> TestTree
describeVolumeStatusTest = undefined

describeVolumesTest :: DescribeVolumes -> TestTree
describeVolumesTest = undefined

detachClassicLinkVPCTest :: DetachClassicLinkVPC -> TestTree
detachClassicLinkVPCTest = undefined

detachInternetGatewayTest :: DetachInternetGateway -> TestTree
detachInternetGatewayTest = undefined

detachNetworkInterfaceTest :: DetachNetworkInterface -> TestTree
detachNetworkInterfaceTest = undefined

detachVPNGatewayTest :: DetachVPNGateway -> TestTree
detachVPNGatewayTest = undefined

detachVolumeTest :: DetachVolume -> TestTree
detachVolumeTest = undefined

disableVGWRoutePropagationTest :: DisableVGWRoutePropagation -> TestTree
disableVGWRoutePropagationTest = undefined

disableVPCClassicLinkTest :: DisableVPCClassicLink -> TestTree
disableVPCClassicLinkTest = undefined

disassociateAddressTest :: DisassociateAddress -> TestTree
disassociateAddressTest = undefined

disassociateRouteTableTest :: DisassociateRouteTable -> TestTree
disassociateRouteTableTest = undefined

enableVGWRoutePropagationTest :: EnableVGWRoutePropagation -> TestTree
enableVGWRoutePropagationTest = undefined

enableVPCClassicLinkTest :: EnableVPCClassicLink -> TestTree
enableVPCClassicLinkTest = undefined

enableVolumeIOTest :: EnableVolumeIO -> TestTree
enableVolumeIOTest = undefined

getConsoleOutputTest :: GetConsoleOutput -> TestTree
getConsoleOutputTest = undefined

getPasswordDataTest :: GetPasswordData -> TestTree
getPasswordDataTest = undefined

importImageTest :: ImportImage -> TestTree
importImageTest = undefined

importInstanceTest :: ImportInstance -> TestTree
importInstanceTest = undefined

importKeyPairTest :: ImportKeyPair -> TestTree
importKeyPairTest = undefined

importSnapshotTest :: ImportSnapshot -> TestTree
importSnapshotTest = undefined

importVolumeTest :: ImportVolume -> TestTree
importVolumeTest = undefined

modifyImageAttributeTest :: ModifyImageAttribute -> TestTree
modifyImageAttributeTest = undefined

modifyInstanceAttributeTest :: ModifyInstanceAttribute -> TestTree
modifyInstanceAttributeTest = undefined

modifyNetworkInterfaceAttributeTest :: ModifyNetworkInterfaceAttribute -> TestTree
modifyNetworkInterfaceAttributeTest = undefined

modifyReservedInstancesTest :: ModifyReservedInstances -> TestTree
modifyReservedInstancesTest = undefined

modifySnapshotAttributeTest :: ModifySnapshotAttribute -> TestTree
modifySnapshotAttributeTest = undefined

modifySubnetAttributeTest :: ModifySubnetAttribute -> TestTree
modifySubnetAttributeTest = undefined

modifyVPCAttributeTest :: ModifyVPCAttribute -> TestTree
modifyVPCAttributeTest = undefined

modifyVPCEndpointTest :: ModifyVPCEndpoint -> TestTree
modifyVPCEndpointTest = undefined

modifyVolumeAttributeTest :: ModifyVolumeAttribute -> TestTree
modifyVolumeAttributeTest = undefined

monitorInstancesTest :: MonitorInstances -> TestTree
monitorInstancesTest = undefined

moveAddressToVPCTest :: MoveAddressToVPC -> TestTree
moveAddressToVPCTest = undefined

purchaseReservedInstancesOfferingTest :: PurchaseReservedInstancesOffering -> TestTree
purchaseReservedInstancesOfferingTest = undefined

rebootInstancesTest :: RebootInstances -> TestTree
rebootInstancesTest = undefined

registerImageTest :: RegisterImage -> TestTree
registerImageTest = undefined

rejectVPCPeeringConnectionTest :: RejectVPCPeeringConnection -> TestTree
rejectVPCPeeringConnectionTest = undefined

releaseAddressTest :: ReleaseAddress -> TestTree
releaseAddressTest = undefined

replaceNetworkACLAssociationTest :: ReplaceNetworkACLAssociation -> TestTree
replaceNetworkACLAssociationTest = undefined

replaceNetworkACLEntryTest :: ReplaceNetworkACLEntry -> TestTree
replaceNetworkACLEntryTest = undefined

replaceRouteTest :: ReplaceRoute -> TestTree
replaceRouteTest = undefined

replaceRouteTableAssociationTest :: ReplaceRouteTableAssociation -> TestTree
replaceRouteTableAssociationTest = undefined

reportInstanceStatusTest :: ReportInstanceStatus -> TestTree
reportInstanceStatusTest = undefined

requestSpotFleetTest :: RequestSpotFleet -> TestTree
requestSpotFleetTest = undefined

requestSpotInstancesTest :: RequestSpotInstances -> TestTree
requestSpotInstancesTest = undefined

resetImageAttributeTest :: ResetImageAttribute -> TestTree
resetImageAttributeTest = undefined

resetInstanceAttributeTest :: ResetInstanceAttribute -> TestTree
resetInstanceAttributeTest = undefined

resetNetworkInterfaceAttributeTest :: ResetNetworkInterfaceAttribute -> TestTree
resetNetworkInterfaceAttributeTest = undefined

resetSnapshotAttributeTest :: ResetSnapshotAttribute -> TestTree
resetSnapshotAttributeTest = undefined

restoreAddressToClassicTest :: RestoreAddressToClassic -> TestTree
restoreAddressToClassicTest = undefined

revokeSecurityGroupEgressTest :: RevokeSecurityGroupEgress -> TestTree
revokeSecurityGroupEgressTest = undefined

revokeSecurityGroupIngressTest :: RevokeSecurityGroupIngress -> TestTree
revokeSecurityGroupIngressTest = undefined

runInstancesTest :: RunInstances -> TestTree
runInstancesTest = undefined

startInstancesTest :: StartInstances -> TestTree
startInstancesTest = undefined

stopInstancesTest :: StopInstances -> TestTree
stopInstancesTest = undefined

terminateInstancesTest :: TerminateInstances -> TestTree
terminateInstancesTest = undefined

unassignPrivateIPAddressesTest :: UnassignPrivateIPAddresses -> TestTree
unassignPrivateIPAddressesTest = undefined

unmonitorInstancesTest :: UnmonitorInstances -> TestTree
unmonitorInstancesTest = undefined

-- Responses

acceptVPCPeeringConnectionResponseTest :: AcceptVPCPeeringConnectionResponse -> TestTree
acceptVPCPeeringConnectionResponseTest = resp
    "acceptVPCPeeringConnectionResponse"
    "fixture/AcceptVPCPeeringConnectionResponse"
    (Proxy :: Proxy AcceptVPCPeeringConnection)

allocateAddressResponseTest :: AllocateAddressResponse -> TestTree
allocateAddressResponseTest = resp
    "allocateAddressResponse"
    "fixture/AllocateAddressResponse"
    (Proxy :: Proxy AllocateAddress)

assignPrivateIPAddressesResponseTest :: AssignPrivateIPAddressesResponse -> TestTree
assignPrivateIPAddressesResponseTest = resp
    "assignPrivateIPAddressesResponse"
    "fixture/AssignPrivateIPAddressesResponse"
    (Proxy :: Proxy AssignPrivateIPAddresses)

associateAddressResponseTest :: AssociateAddressResponse -> TestTree
associateAddressResponseTest = resp
    "associateAddressResponse"
    "fixture/AssociateAddressResponse"
    (Proxy :: Proxy AssociateAddress)

associateDHCPOptionsResponseTest :: AssociateDHCPOptionsResponse -> TestTree
associateDHCPOptionsResponseTest = resp
    "associateDHCPOptionsResponse"
    "fixture/AssociateDHCPOptionsResponse"
    (Proxy :: Proxy AssociateDHCPOptions)

associateRouteTableResponseTest :: AssociateRouteTableResponse -> TestTree
associateRouteTableResponseTest = resp
    "associateRouteTableResponse"
    "fixture/AssociateRouteTableResponse"
    (Proxy :: Proxy AssociateRouteTable)

attachClassicLinkVPCResponseTest :: AttachClassicLinkVPCResponse -> TestTree
attachClassicLinkVPCResponseTest = resp
    "attachClassicLinkVPCResponse"
    "fixture/AttachClassicLinkVPCResponse"
    (Proxy :: Proxy AttachClassicLinkVPC)

attachInternetGatewayResponseTest :: AttachInternetGatewayResponse -> TestTree
attachInternetGatewayResponseTest = resp
    "attachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse"
    (Proxy :: Proxy AttachInternetGateway)

attachNetworkInterfaceResponseTest :: AttachNetworkInterfaceResponse -> TestTree
attachNetworkInterfaceResponseTest = resp
    "attachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse"
    (Proxy :: Proxy AttachNetworkInterface)

attachVPNGatewayResponseTest :: AttachVPNGatewayResponse -> TestTree
attachVPNGatewayResponseTest = resp
    "attachVPNGatewayResponse"
    "fixture/AttachVPNGatewayResponse"
    (Proxy :: Proxy AttachVPNGateway)

attachVolumeResponseTest :: VolumeAttachment -> TestTree
attachVolumeResponseTest = resp
    "attachVolumeResponse"
    "fixture/VolumeAttachment"
    (Proxy :: Proxy AttachVolume)

authorizeSecurityGroupEgressResponseTest :: AuthorizeSecurityGroupEgressResponse -> TestTree
authorizeSecurityGroupEgressResponseTest = resp
    "authorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse"
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

authorizeSecurityGroupIngressResponseTest :: AuthorizeSecurityGroupIngressResponse -> TestTree
authorizeSecurityGroupIngressResponseTest = resp
    "authorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

bundleInstanceResponseTest :: BundleInstanceResponse -> TestTree
bundleInstanceResponseTest = resp
    "bundleInstanceResponse"
    "fixture/BundleInstanceResponse"
    (Proxy :: Proxy BundleInstance)

cancelBundleTaskResponseTest :: CancelBundleTaskResponse -> TestTree
cancelBundleTaskResponseTest = resp
    "cancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse"
    (Proxy :: Proxy CancelBundleTask)

cancelConversionTaskResponseTest :: CancelConversionTaskResponse -> TestTree
cancelConversionTaskResponseTest = resp
    "cancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse"
    (Proxy :: Proxy CancelConversionTask)

cancelExportTaskResponseTest :: CancelExportTaskResponse -> TestTree
cancelExportTaskResponseTest = resp
    "cancelExportTaskResponse"
    "fixture/CancelExportTaskResponse"
    (Proxy :: Proxy CancelExportTask)

cancelImportTaskResponseTest :: CancelImportTaskResponse -> TestTree
cancelImportTaskResponseTest = resp
    "cancelImportTaskResponse"
    "fixture/CancelImportTaskResponse"
    (Proxy :: Proxy CancelImportTask)

cancelReservedInstancesListingResponseTest :: CancelReservedInstancesListingResponse -> TestTree
cancelReservedInstancesListingResponseTest = resp
    "cancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse"
    (Proxy :: Proxy CancelReservedInstancesListing)

cancelSpotFleetRequestsResponseTest :: CancelSpotFleetRequestsResponse -> TestTree
cancelSpotFleetRequestsResponseTest = resp
    "cancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse"
    (Proxy :: Proxy CancelSpotFleetRequests)

cancelSpotInstanceRequestsResponseTest :: CancelSpotInstanceRequestsResponse -> TestTree
cancelSpotInstanceRequestsResponseTest = resp
    "cancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse"
    (Proxy :: Proxy CancelSpotInstanceRequests)

confirmProductInstanceResponseTest :: ConfirmProductInstanceResponse -> TestTree
confirmProductInstanceResponseTest = resp
    "confirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse"
    (Proxy :: Proxy ConfirmProductInstance)

copyImageResponseTest :: CopyImageResponse -> TestTree
copyImageResponseTest = resp
    "copyImageResponse"
    "fixture/CopyImageResponse"
    (Proxy :: Proxy CopyImage)

copySnapshotResponseTest :: CopySnapshotResponse -> TestTree
copySnapshotResponseTest = resp
    "copySnapshotResponse"
    "fixture/CopySnapshotResponse"
    (Proxy :: Proxy CopySnapshot)

createCustomerGatewayResponseTest :: CreateCustomerGatewayResponse -> TestTree
createCustomerGatewayResponseTest = resp
    "createCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse"
    (Proxy :: Proxy CreateCustomerGateway)

createDHCPOptionsResponseTest :: CreateDHCPOptionsResponse -> TestTree
createDHCPOptionsResponseTest = resp
    "createDHCPOptionsResponse"
    "fixture/CreateDHCPOptionsResponse"
    (Proxy :: Proxy CreateDHCPOptions)

createFlowLogsResponseTest :: CreateFlowLogsResponse -> TestTree
createFlowLogsResponseTest = resp
    "createFlowLogsResponse"
    "fixture/CreateFlowLogsResponse"
    (Proxy :: Proxy CreateFlowLogs)

createImageResponseTest :: CreateImageResponse -> TestTree
createImageResponseTest = resp
    "createImageResponse"
    "fixture/CreateImageResponse"
    (Proxy :: Proxy CreateImage)

createInstanceExportTaskResponseTest :: CreateInstanceExportTaskResponse -> TestTree
createInstanceExportTaskResponseTest = resp
    "createInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse"
    (Proxy :: Proxy CreateInstanceExportTask)

createInternetGatewayResponseTest :: CreateInternetGatewayResponse -> TestTree
createInternetGatewayResponseTest = resp
    "createInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse"
    (Proxy :: Proxy CreateInternetGateway)

createKeyPairResponseTest :: CreateKeyPairResponse -> TestTree
createKeyPairResponseTest = resp
    "createKeyPairResponse"
    "fixture/CreateKeyPairResponse"
    (Proxy :: Proxy CreateKeyPair)

createNetworkACLResponseTest :: CreateNetworkACLResponse -> TestTree
createNetworkACLResponseTest = resp
    "createNetworkACLResponse"
    "fixture/CreateNetworkACLResponse"
    (Proxy :: Proxy CreateNetworkACL)

createNetworkACLEntryResponseTest :: CreateNetworkACLEntryResponse -> TestTree
createNetworkACLEntryResponseTest = resp
    "createNetworkACLEntryResponse"
    "fixture/CreateNetworkACLEntryResponse"
    (Proxy :: Proxy CreateNetworkACLEntry)

createNetworkInterfaceResponseTest :: CreateNetworkInterfaceResponse -> TestTree
createNetworkInterfaceResponseTest = resp
    "createNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse"
    (Proxy :: Proxy CreateNetworkInterface)

createPlacementGroupResponseTest :: CreatePlacementGroupResponse -> TestTree
createPlacementGroupResponseTest = resp
    "createPlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse"
    (Proxy :: Proxy CreatePlacementGroup)

createReservedInstancesListingResponseTest :: CreateReservedInstancesListingResponse -> TestTree
createReservedInstancesListingResponseTest = resp
    "createReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse"
    (Proxy :: Proxy CreateReservedInstancesListing)

createRouteResponseTest :: CreateRouteResponse -> TestTree
createRouteResponseTest = resp
    "createRouteResponse"
    "fixture/CreateRouteResponse"
    (Proxy :: Proxy CreateRoute)

createRouteTableResponseTest :: CreateRouteTableResponse -> TestTree
createRouteTableResponseTest = resp
    "createRouteTableResponse"
    "fixture/CreateRouteTableResponse"
    (Proxy :: Proxy CreateRouteTable)

createSecurityGroupResponseTest :: CreateSecurityGroupResponse -> TestTree
createSecurityGroupResponseTest = resp
    "createSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse"
    (Proxy :: Proxy CreateSecurityGroup)

createSnapshotResponseTest :: Snapshot -> TestTree
createSnapshotResponseTest = resp
    "createSnapshotResponse"
    "fixture/Snapshot"
    (Proxy :: Proxy CreateSnapshot)

createSpotDatafeedSubscriptionResponseTest :: CreateSpotDatafeedSubscriptionResponse -> TestTree
createSpotDatafeedSubscriptionResponseTest = resp
    "createSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

createSubnetResponseTest :: CreateSubnetResponse -> TestTree
createSubnetResponseTest = resp
    "createSubnetResponse"
    "fixture/CreateSubnetResponse"
    (Proxy :: Proxy CreateSubnet)

createTagsResponseTest :: CreateTagsResponse -> TestTree
createTagsResponseTest = resp
    "createTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

createVPCResponseTest :: CreateVPCResponse -> TestTree
createVPCResponseTest = resp
    "createVPCResponse"
    "fixture/CreateVPCResponse"
    (Proxy :: Proxy CreateVPC)

createVPCEndpointResponseTest :: CreateVPCEndpointResponse -> TestTree
createVPCEndpointResponseTest = resp
    "createVPCEndpointResponse"
    "fixture/CreateVPCEndpointResponse"
    (Proxy :: Proxy CreateVPCEndpoint)

createVPCPeeringConnectionResponseTest :: CreateVPCPeeringConnectionResponse -> TestTree
createVPCPeeringConnectionResponseTest = resp
    "createVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse"
    (Proxy :: Proxy CreateVPCPeeringConnection)

createVPNConnectionResponseTest :: CreateVPNConnectionResponse -> TestTree
createVPNConnectionResponseTest = resp
    "createVPNConnectionResponse"
    "fixture/CreateVPNConnectionResponse"
    (Proxy :: Proxy CreateVPNConnection)

createVPNConnectionRouteResponseTest :: CreateVPNConnectionRouteResponse -> TestTree
createVPNConnectionRouteResponseTest = resp
    "createVPNConnectionRouteResponse"
    "fixture/CreateVPNConnectionRouteResponse"
    (Proxy :: Proxy CreateVPNConnectionRoute)

createVPNGatewayResponseTest :: CreateVPNGatewayResponse -> TestTree
createVPNGatewayResponseTest = resp
    "createVPNGatewayResponse"
    "fixture/CreateVPNGatewayResponse"
    (Proxy :: Proxy CreateVPNGateway)

createVolumeResponseTest :: Volume -> TestTree
createVolumeResponseTest = resp
    "createVolumeResponse"
    "fixture/Volume"
    (Proxy :: Proxy CreateVolume)

deleteCustomerGatewayResponseTest :: DeleteCustomerGatewayResponse -> TestTree
deleteCustomerGatewayResponseTest = resp
    "deleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse"
    (Proxy :: Proxy DeleteCustomerGateway)

deleteDHCPOptionsResponseTest :: DeleteDHCPOptionsResponse -> TestTree
deleteDHCPOptionsResponseTest = resp
    "deleteDHCPOptionsResponse"
    "fixture/DeleteDHCPOptionsResponse"
    (Proxy :: Proxy DeleteDHCPOptions)

deleteFlowLogsResponseTest :: DeleteFlowLogsResponse -> TestTree
deleteFlowLogsResponseTest = resp
    "deleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse"
    (Proxy :: Proxy DeleteFlowLogs)

deleteInternetGatewayResponseTest :: DeleteInternetGatewayResponse -> TestTree
deleteInternetGatewayResponseTest = resp
    "deleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse"
    (Proxy :: Proxy DeleteInternetGateway)

deleteKeyPairResponseTest :: DeleteKeyPairResponse -> TestTree
deleteKeyPairResponseTest = resp
    "deleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse"
    (Proxy :: Proxy DeleteKeyPair)

deleteNetworkACLResponseTest :: DeleteNetworkACLResponse -> TestTree
deleteNetworkACLResponseTest = resp
    "deleteNetworkACLResponse"
    "fixture/DeleteNetworkACLResponse"
    (Proxy :: Proxy DeleteNetworkACL)

deleteNetworkACLEntryResponseTest :: DeleteNetworkACLEntryResponse -> TestTree
deleteNetworkACLEntryResponseTest = resp
    "deleteNetworkACLEntryResponse"
    "fixture/DeleteNetworkACLEntryResponse"
    (Proxy :: Proxy DeleteNetworkACLEntry)

deleteNetworkInterfaceResponseTest :: DeleteNetworkInterfaceResponse -> TestTree
deleteNetworkInterfaceResponseTest = resp
    "deleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse"
    (Proxy :: Proxy DeleteNetworkInterface)

deletePlacementGroupResponseTest :: DeletePlacementGroupResponse -> TestTree
deletePlacementGroupResponseTest = resp
    "deletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse"
    (Proxy :: Proxy DeletePlacementGroup)

deleteRouteResponseTest :: DeleteRouteResponse -> TestTree
deleteRouteResponseTest = resp
    "deleteRouteResponse"
    "fixture/DeleteRouteResponse"
    (Proxy :: Proxy DeleteRoute)

deleteRouteTableResponseTest :: DeleteRouteTableResponse -> TestTree
deleteRouteTableResponseTest = resp
    "deleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse"
    (Proxy :: Proxy DeleteRouteTable)

deleteSecurityGroupResponseTest :: DeleteSecurityGroupResponse -> TestTree
deleteSecurityGroupResponseTest = resp
    "deleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse"
    (Proxy :: Proxy DeleteSecurityGroup)

deleteSnapshotResponseTest :: DeleteSnapshotResponse -> TestTree
deleteSnapshotResponseTest = resp
    "deleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

deleteSpotDatafeedSubscriptionResponseTest :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
deleteSpotDatafeedSubscriptionResponseTest = resp
    "deleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

deleteSubnetResponseTest :: DeleteSubnetResponse -> TestTree
deleteSubnetResponseTest = resp
    "deleteSubnetResponse"
    "fixture/DeleteSubnetResponse"
    (Proxy :: Proxy DeleteSubnet)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "deleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

deleteVPCResponseTest :: DeleteVPCResponse -> TestTree
deleteVPCResponseTest = resp
    "deleteVPCResponse"
    "fixture/DeleteVPCResponse"
    (Proxy :: Proxy DeleteVPC)

deleteVPCEndpointsResponseTest :: DeleteVPCEndpointsResponse -> TestTree
deleteVPCEndpointsResponseTest = resp
    "deleteVPCEndpointsResponse"
    "fixture/DeleteVPCEndpointsResponse"
    (Proxy :: Proxy DeleteVPCEndpoints)

deleteVPCPeeringConnectionResponseTest :: DeleteVPCPeeringConnectionResponse -> TestTree
deleteVPCPeeringConnectionResponseTest = resp
    "deleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse"
    (Proxy :: Proxy DeleteVPCPeeringConnection)

deleteVPNConnectionResponseTest :: DeleteVPNConnectionResponse -> TestTree
deleteVPNConnectionResponseTest = resp
    "deleteVPNConnectionResponse"
    "fixture/DeleteVPNConnectionResponse"
    (Proxy :: Proxy DeleteVPNConnection)

deleteVPNConnectionRouteResponseTest :: DeleteVPNConnectionRouteResponse -> TestTree
deleteVPNConnectionRouteResponseTest = resp
    "deleteVPNConnectionRouteResponse"
    "fixture/DeleteVPNConnectionRouteResponse"
    (Proxy :: Proxy DeleteVPNConnectionRoute)

deleteVPNGatewayResponseTest :: DeleteVPNGatewayResponse -> TestTree
deleteVPNGatewayResponseTest = resp
    "deleteVPNGatewayResponse"
    "fixture/DeleteVPNGatewayResponse"
    (Proxy :: Proxy DeleteVPNGateway)

deleteVolumeResponseTest :: DeleteVolumeResponse -> TestTree
deleteVolumeResponseTest = resp
    "deleteVolumeResponse"
    "fixture/DeleteVolumeResponse"
    (Proxy :: Proxy DeleteVolume)

deregisterImageResponseTest :: DeregisterImageResponse -> TestTree
deregisterImageResponseTest = resp
    "deregisterImageResponse"
    "fixture/DeregisterImageResponse"
    (Proxy :: Proxy DeregisterImage)

describeAccountAttributesResponseTest :: DescribeAccountAttributesResponse -> TestTree
describeAccountAttributesResponseTest = resp
    "describeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse"
    (Proxy :: Proxy DescribeAccountAttributes)

describeAddressesResponseTest :: DescribeAddressesResponse -> TestTree
describeAddressesResponseTest = resp
    "describeAddressesResponse"
    "fixture/DescribeAddressesResponse"
    (Proxy :: Proxy DescribeAddresses)

describeAvailabilityZonesResponseTest :: DescribeAvailabilityZonesResponse -> TestTree
describeAvailabilityZonesResponseTest = resp
    "describeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse"
    (Proxy :: Proxy DescribeAvailabilityZones)

describeBundleTasksResponseTest :: DescribeBundleTasksResponse -> TestTree
describeBundleTasksResponseTest = resp
    "describeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse"
    (Proxy :: Proxy DescribeBundleTasks)

describeClassicLinkInstancesResponseTest :: DescribeClassicLinkInstancesResponse -> TestTree
describeClassicLinkInstancesResponseTest = resp
    "describeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse"
    (Proxy :: Proxy DescribeClassicLinkInstances)

describeConversionTasksResponseTest :: DescribeConversionTasksResponse -> TestTree
describeConversionTasksResponseTest = resp
    "describeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse"
    (Proxy :: Proxy DescribeConversionTasks)

describeCustomerGatewaysResponseTest :: DescribeCustomerGatewaysResponse -> TestTree
describeCustomerGatewaysResponseTest = resp
    "describeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse"
    (Proxy :: Proxy DescribeCustomerGateways)

describeDHCPOptionsResponseTest :: DescribeDHCPOptionsResponse -> TestTree
describeDHCPOptionsResponseTest = resp
    "describeDHCPOptionsResponse"
    "fixture/DescribeDHCPOptionsResponse"
    (Proxy :: Proxy DescribeDHCPOptions)

describeExportTasksResponseTest :: DescribeExportTasksResponse -> TestTree
describeExportTasksResponseTest = resp
    "describeExportTasksResponse"
    "fixture/DescribeExportTasksResponse"
    (Proxy :: Proxy DescribeExportTasks)

describeFlowLogsResponseTest :: DescribeFlowLogsResponse -> TestTree
describeFlowLogsResponseTest = resp
    "describeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse"
    (Proxy :: Proxy DescribeFlowLogs)

describeImageAttributeResponseTest :: DescribeImageAttributeResponse -> TestTree
describeImageAttributeResponseTest = resp
    "describeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse"
    (Proxy :: Proxy DescribeImageAttribute)

describeImagesResponseTest :: DescribeImagesResponse -> TestTree
describeImagesResponseTest = resp
    "describeImagesResponse"
    "fixture/DescribeImagesResponse"
    (Proxy :: Proxy DescribeImages)

describeImportImageTasksResponseTest :: DescribeImportImageTasksResponse -> TestTree
describeImportImageTasksResponseTest = resp
    "describeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse"
    (Proxy :: Proxy DescribeImportImageTasks)

describeImportSnapshotTasksResponseTest :: DescribeImportSnapshotTasksResponse -> TestTree
describeImportSnapshotTasksResponseTest = resp
    "describeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse"
    (Proxy :: Proxy DescribeImportSnapshotTasks)

describeInstanceAttributeResponseTest :: DescribeInstanceAttributeResponse -> TestTree
describeInstanceAttributeResponseTest = resp
    "describeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse"
    (Proxy :: Proxy DescribeInstanceAttribute)

describeInstanceStatusResponseTest :: DescribeInstanceStatusResponse -> TestTree
describeInstanceStatusResponseTest = resp
    "describeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse"
    (Proxy :: Proxy DescribeInstanceStatus)

describeInstancesResponseTest :: DescribeInstancesResponse -> TestTree
describeInstancesResponseTest = resp
    "describeInstancesResponse"
    "fixture/DescribeInstancesResponse"
    (Proxy :: Proxy DescribeInstances)

describeInternetGatewaysResponseTest :: DescribeInternetGatewaysResponse -> TestTree
describeInternetGatewaysResponseTest = resp
    "describeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse"
    (Proxy :: Proxy DescribeInternetGateways)

describeKeyPairsResponseTest :: DescribeKeyPairsResponse -> TestTree
describeKeyPairsResponseTest = resp
    "describeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse"
    (Proxy :: Proxy DescribeKeyPairs)

describeMovingAddressesResponseTest :: DescribeMovingAddressesResponse -> TestTree
describeMovingAddressesResponseTest = resp
    "describeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse"
    (Proxy :: Proxy DescribeMovingAddresses)

describeNetworkACLsResponseTest :: DescribeNetworkACLsResponse -> TestTree
describeNetworkACLsResponseTest = resp
    "describeNetworkACLsResponse"
    "fixture/DescribeNetworkACLsResponse"
    (Proxy :: Proxy DescribeNetworkACLs)

describeNetworkInterfaceAttributeResponseTest :: DescribeNetworkInterfaceAttributeResponse -> TestTree
describeNetworkInterfaceAttributeResponseTest = resp
    "describeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

describeNetworkInterfacesResponseTest :: DescribeNetworkInterfacesResponse -> TestTree
describeNetworkInterfacesResponseTest = resp
    "describeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse"
    (Proxy :: Proxy DescribeNetworkInterfaces)

describePlacementGroupsResponseTest :: DescribePlacementGroupsResponse -> TestTree
describePlacementGroupsResponseTest = resp
    "describePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse"
    (Proxy :: Proxy DescribePlacementGroups)

describePrefixListsResponseTest :: DescribePrefixListsResponse -> TestTree
describePrefixListsResponseTest = resp
    "describePrefixListsResponse"
    "fixture/DescribePrefixListsResponse"
    (Proxy :: Proxy DescribePrefixLists)

describeRegionsResponseTest :: DescribeRegionsResponse -> TestTree
describeRegionsResponseTest = resp
    "describeRegionsResponse"
    "fixture/DescribeRegionsResponse"
    (Proxy :: Proxy DescribeRegions)

describeReservedInstancesResponseTest :: DescribeReservedInstancesResponse -> TestTree
describeReservedInstancesResponseTest = resp
    "describeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse"
    (Proxy :: Proxy DescribeReservedInstances)

describeReservedInstancesListingsResponseTest :: DescribeReservedInstancesListingsResponse -> TestTree
describeReservedInstancesListingsResponseTest = resp
    "describeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse"
    (Proxy :: Proxy DescribeReservedInstancesListings)

describeReservedInstancesModificationsResponseTest :: DescribeReservedInstancesModificationsResponse -> TestTree
describeReservedInstancesModificationsResponseTest = resp
    "describeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse"
    (Proxy :: Proxy DescribeReservedInstancesModifications)

describeReservedInstancesOfferingsResponseTest :: DescribeReservedInstancesOfferingsResponse -> TestTree
describeReservedInstancesOfferingsResponseTest = resp
    "describeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

describeRouteTablesResponseTest :: DescribeRouteTablesResponse -> TestTree
describeRouteTablesResponseTest = resp
    "describeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse"
    (Proxy :: Proxy DescribeRouteTables)

describeSecurityGroupsResponseTest :: DescribeSecurityGroupsResponse -> TestTree
describeSecurityGroupsResponseTest = resp
    "describeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse"
    (Proxy :: Proxy DescribeSecurityGroups)

describeSnapshotAttributeResponseTest :: DescribeSnapshotAttributeResponse -> TestTree
describeSnapshotAttributeResponseTest = resp
    "describeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse"
    (Proxy :: Proxy DescribeSnapshotAttribute)

describeSnapshotsResponseTest :: DescribeSnapshotsResponse -> TestTree
describeSnapshotsResponseTest = resp
    "describeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

describeSpotDatafeedSubscriptionResponseTest :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
describeSpotDatafeedSubscriptionResponseTest = resp
    "describeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse"
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

describeSpotFleetInstancesResponseTest :: DescribeSpotFleetInstancesResponse -> TestTree
describeSpotFleetInstancesResponseTest = resp
    "describeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse"
    (Proxy :: Proxy DescribeSpotFleetInstances)

describeSpotFleetRequestHistoryResponseTest :: DescribeSpotFleetRequestHistoryResponse -> TestTree
describeSpotFleetRequestHistoryResponseTest = resp
    "describeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse"
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

describeSpotFleetRequestsResponseTest :: DescribeSpotFleetRequestsResponse -> TestTree
describeSpotFleetRequestsResponseTest = resp
    "describeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse"
    (Proxy :: Proxy DescribeSpotFleetRequests)

describeSpotInstanceRequestsResponseTest :: DescribeSpotInstanceRequestsResponse -> TestTree
describeSpotInstanceRequestsResponseTest = resp
    "describeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse"
    (Proxy :: Proxy DescribeSpotInstanceRequests)

describeSpotPriceHistoryResponseTest :: DescribeSpotPriceHistoryResponse -> TestTree
describeSpotPriceHistoryResponseTest = resp
    "describeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse"
    (Proxy :: Proxy DescribeSpotPriceHistory)

describeSubnetsResponseTest :: DescribeSubnetsResponse -> TestTree
describeSubnetsResponseTest = resp
    "describeSubnetsResponse"
    "fixture/DescribeSubnetsResponse"
    (Proxy :: Proxy DescribeSubnets)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "describeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

describeVPCAttributeResponseTest :: DescribeVPCAttributeResponse -> TestTree
describeVPCAttributeResponseTest = resp
    "describeVPCAttributeResponse"
    "fixture/DescribeVPCAttributeResponse"
    (Proxy :: Proxy DescribeVPCAttribute)

describeVPCClassicLinkResponseTest :: DescribeVPCClassicLinkResponse -> TestTree
describeVPCClassicLinkResponseTest = resp
    "describeVPCClassicLinkResponse"
    "fixture/DescribeVPCClassicLinkResponse"
    (Proxy :: Proxy DescribeVPCClassicLink)

describeVPCEndpointServicesResponseTest :: DescribeVPCEndpointServicesResponse -> TestTree
describeVPCEndpointServicesResponseTest = resp
    "describeVPCEndpointServicesResponse"
    "fixture/DescribeVPCEndpointServicesResponse"
    (Proxy :: Proxy DescribeVPCEndpointServices)

describeVPCEndpointsResponseTest :: DescribeVPCEndpointsResponse -> TestTree
describeVPCEndpointsResponseTest = resp
    "describeVPCEndpointsResponse"
    "fixture/DescribeVPCEndpointsResponse"
    (Proxy :: Proxy DescribeVPCEndpoints)

describeVPCPeeringConnectionsResponseTest :: DescribeVPCPeeringConnectionsResponse -> TestTree
describeVPCPeeringConnectionsResponseTest = resp
    "describeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse"
    (Proxy :: Proxy DescribeVPCPeeringConnections)

describeVPCsResponseTest :: DescribeVPCsResponse -> TestTree
describeVPCsResponseTest = resp
    "describeVPCsResponse"
    "fixture/DescribeVPCsResponse"
    (Proxy :: Proxy DescribeVPCs)

describeVPNConnectionsResponseTest :: DescribeVPNConnectionsResponse -> TestTree
describeVPNConnectionsResponseTest = resp
    "describeVPNConnectionsResponse"
    "fixture/DescribeVPNConnectionsResponse"
    (Proxy :: Proxy DescribeVPNConnections)

describeVPNGatewaysResponseTest :: DescribeVPNGatewaysResponse -> TestTree
describeVPNGatewaysResponseTest = resp
    "describeVPNGatewaysResponse"
    "fixture/DescribeVPNGatewaysResponse"
    (Proxy :: Proxy DescribeVPNGateways)

describeVolumeAttributeResponseTest :: DescribeVolumeAttributeResponse -> TestTree
describeVolumeAttributeResponseTest = resp
    "describeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse"
    (Proxy :: Proxy DescribeVolumeAttribute)

describeVolumeStatusResponseTest :: DescribeVolumeStatusResponse -> TestTree
describeVolumeStatusResponseTest = resp
    "describeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse"
    (Proxy :: Proxy DescribeVolumeStatus)

describeVolumesResponseTest :: DescribeVolumesResponse -> TestTree
describeVolumesResponseTest = resp
    "describeVolumesResponse"
    "fixture/DescribeVolumesResponse"
    (Proxy :: Proxy DescribeVolumes)

detachClassicLinkVPCResponseTest :: DetachClassicLinkVPCResponse -> TestTree
detachClassicLinkVPCResponseTest = resp
    "detachClassicLinkVPCResponse"
    "fixture/DetachClassicLinkVPCResponse"
    (Proxy :: Proxy DetachClassicLinkVPC)

detachInternetGatewayResponseTest :: DetachInternetGatewayResponse -> TestTree
detachInternetGatewayResponseTest = resp
    "detachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse"
    (Proxy :: Proxy DetachInternetGateway)

detachNetworkInterfaceResponseTest :: DetachNetworkInterfaceResponse -> TestTree
detachNetworkInterfaceResponseTest = resp
    "detachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse"
    (Proxy :: Proxy DetachNetworkInterface)

detachVPNGatewayResponseTest :: DetachVPNGatewayResponse -> TestTree
detachVPNGatewayResponseTest = resp
    "detachVPNGatewayResponse"
    "fixture/DetachVPNGatewayResponse"
    (Proxy :: Proxy DetachVPNGateway)

detachVolumeResponseTest :: VolumeAttachment -> TestTree
detachVolumeResponseTest = resp
    "detachVolumeResponse"
    "fixture/VolumeAttachment"
    (Proxy :: Proxy DetachVolume)

disableVGWRoutePropagationResponseTest :: DisableVGWRoutePropagationResponse -> TestTree
disableVGWRoutePropagationResponseTest = resp
    "disableVGWRoutePropagationResponse"
    "fixture/DisableVGWRoutePropagationResponse"
    (Proxy :: Proxy DisableVGWRoutePropagation)

disableVPCClassicLinkResponseTest :: DisableVPCClassicLinkResponse -> TestTree
disableVPCClassicLinkResponseTest = resp
    "disableVPCClassicLinkResponse"
    "fixture/DisableVPCClassicLinkResponse"
    (Proxy :: Proxy DisableVPCClassicLink)

disassociateAddressResponseTest :: DisassociateAddressResponse -> TestTree
disassociateAddressResponseTest = resp
    "disassociateAddressResponse"
    "fixture/DisassociateAddressResponse"
    (Proxy :: Proxy DisassociateAddress)

disassociateRouteTableResponseTest :: DisassociateRouteTableResponse -> TestTree
disassociateRouteTableResponseTest = resp
    "disassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse"
    (Proxy :: Proxy DisassociateRouteTable)

enableVGWRoutePropagationResponseTest :: EnableVGWRoutePropagationResponse -> TestTree
enableVGWRoutePropagationResponseTest = resp
    "enableVGWRoutePropagationResponse"
    "fixture/EnableVGWRoutePropagationResponse"
    (Proxy :: Proxy EnableVGWRoutePropagation)

enableVPCClassicLinkResponseTest :: EnableVPCClassicLinkResponse -> TestTree
enableVPCClassicLinkResponseTest = resp
    "enableVPCClassicLinkResponse"
    "fixture/EnableVPCClassicLinkResponse"
    (Proxy :: Proxy EnableVPCClassicLink)

enableVolumeIOResponseTest :: EnableVolumeIOResponse -> TestTree
enableVolumeIOResponseTest = resp
    "enableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse"
    (Proxy :: Proxy EnableVolumeIO)

getConsoleOutputResponseTest :: GetConsoleOutputResponse -> TestTree
getConsoleOutputResponseTest = resp
    "getConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse"
    (Proxy :: Proxy GetConsoleOutput)

getPasswordDataResponseTest :: GetPasswordDataResponse -> TestTree
getPasswordDataResponseTest = resp
    "getPasswordDataResponse"
    "fixture/GetPasswordDataResponse"
    (Proxy :: Proxy GetPasswordData)

importImageResponseTest :: ImportImageResponse -> TestTree
importImageResponseTest = resp
    "importImageResponse"
    "fixture/ImportImageResponse"
    (Proxy :: Proxy ImportImage)

importInstanceResponseTest :: ImportInstanceResponse -> TestTree
importInstanceResponseTest = resp
    "importInstanceResponse"
    "fixture/ImportInstanceResponse"
    (Proxy :: Proxy ImportInstance)

importKeyPairResponseTest :: ImportKeyPairResponse -> TestTree
importKeyPairResponseTest = resp
    "importKeyPairResponse"
    "fixture/ImportKeyPairResponse"
    (Proxy :: Proxy ImportKeyPair)

importSnapshotResponseTest :: ImportSnapshotResponse -> TestTree
importSnapshotResponseTest = resp
    "importSnapshotResponse"
    "fixture/ImportSnapshotResponse"
    (Proxy :: Proxy ImportSnapshot)

importVolumeResponseTest :: ImportVolumeResponse -> TestTree
importVolumeResponseTest = resp
    "importVolumeResponse"
    "fixture/ImportVolumeResponse"
    (Proxy :: Proxy ImportVolume)

modifyImageAttributeResponseTest :: ModifyImageAttributeResponse -> TestTree
modifyImageAttributeResponseTest = resp
    "modifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse"
    (Proxy :: Proxy ModifyImageAttribute)

modifyInstanceAttributeResponseTest :: ModifyInstanceAttributeResponse -> TestTree
modifyInstanceAttributeResponseTest = resp
    "modifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse"
    (Proxy :: Proxy ModifyInstanceAttribute)

modifyNetworkInterfaceAttributeResponseTest :: ModifyNetworkInterfaceAttributeResponse -> TestTree
modifyNetworkInterfaceAttributeResponseTest = resp
    "modifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

modifyReservedInstancesResponseTest :: ModifyReservedInstancesResponse -> TestTree
modifyReservedInstancesResponseTest = resp
    "modifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse"
    (Proxy :: Proxy ModifyReservedInstances)

modifySnapshotAttributeResponseTest :: ModifySnapshotAttributeResponse -> TestTree
modifySnapshotAttributeResponseTest = resp
    "modifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse"
    (Proxy :: Proxy ModifySnapshotAttribute)

modifySubnetAttributeResponseTest :: ModifySubnetAttributeResponse -> TestTree
modifySubnetAttributeResponseTest = resp
    "modifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse"
    (Proxy :: Proxy ModifySubnetAttribute)

modifyVPCAttributeResponseTest :: ModifyVPCAttributeResponse -> TestTree
modifyVPCAttributeResponseTest = resp
    "modifyVPCAttributeResponse"
    "fixture/ModifyVPCAttributeResponse"
    (Proxy :: Proxy ModifyVPCAttribute)

modifyVPCEndpointResponseTest :: ModifyVPCEndpointResponse -> TestTree
modifyVPCEndpointResponseTest = resp
    "modifyVPCEndpointResponse"
    "fixture/ModifyVPCEndpointResponse"
    (Proxy :: Proxy ModifyVPCEndpoint)

modifyVolumeAttributeResponseTest :: ModifyVolumeAttributeResponse -> TestTree
modifyVolumeAttributeResponseTest = resp
    "modifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse"
    (Proxy :: Proxy ModifyVolumeAttribute)

monitorInstancesResponseTest :: MonitorInstancesResponse -> TestTree
monitorInstancesResponseTest = resp
    "monitorInstancesResponse"
    "fixture/MonitorInstancesResponse"
    (Proxy :: Proxy MonitorInstances)

moveAddressToVPCResponseTest :: MoveAddressToVPCResponse -> TestTree
moveAddressToVPCResponseTest = resp
    "moveAddressToVPCResponse"
    "fixture/MoveAddressToVPCResponse"
    (Proxy :: Proxy MoveAddressToVPC)

purchaseReservedInstancesOfferingResponseTest :: PurchaseReservedInstancesOfferingResponse -> TestTree
purchaseReservedInstancesOfferingResponseTest = resp
    "purchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

rebootInstancesResponseTest :: RebootInstancesResponse -> TestTree
rebootInstancesResponseTest = resp
    "rebootInstancesResponse"
    "fixture/RebootInstancesResponse"
    (Proxy :: Proxy RebootInstances)

registerImageResponseTest :: RegisterImageResponse -> TestTree
registerImageResponseTest = resp
    "registerImageResponse"
    "fixture/RegisterImageResponse"
    (Proxy :: Proxy RegisterImage)

rejectVPCPeeringConnectionResponseTest :: RejectVPCPeeringConnectionResponse -> TestTree
rejectVPCPeeringConnectionResponseTest = resp
    "rejectVPCPeeringConnectionResponse"
    "fixture/RejectVPCPeeringConnectionResponse"
    (Proxy :: Proxy RejectVPCPeeringConnection)

releaseAddressResponseTest :: ReleaseAddressResponse -> TestTree
releaseAddressResponseTest = resp
    "releaseAddressResponse"
    "fixture/ReleaseAddressResponse"
    (Proxy :: Proxy ReleaseAddress)

replaceNetworkACLAssociationResponseTest :: ReplaceNetworkACLAssociationResponse -> TestTree
replaceNetworkACLAssociationResponseTest = resp
    "replaceNetworkACLAssociationResponse"
    "fixture/ReplaceNetworkACLAssociationResponse"
    (Proxy :: Proxy ReplaceNetworkACLAssociation)

replaceNetworkACLEntryResponseTest :: ReplaceNetworkACLEntryResponse -> TestTree
replaceNetworkACLEntryResponseTest = resp
    "replaceNetworkACLEntryResponse"
    "fixture/ReplaceNetworkACLEntryResponse"
    (Proxy :: Proxy ReplaceNetworkACLEntry)

replaceRouteResponseTest :: ReplaceRouteResponse -> TestTree
replaceRouteResponseTest = resp
    "replaceRouteResponse"
    "fixture/ReplaceRouteResponse"
    (Proxy :: Proxy ReplaceRoute)

replaceRouteTableAssociationResponseTest :: ReplaceRouteTableAssociationResponse -> TestTree
replaceRouteTableAssociationResponseTest = resp
    "replaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse"
    (Proxy :: Proxy ReplaceRouteTableAssociation)

reportInstanceStatusResponseTest :: ReportInstanceStatusResponse -> TestTree
reportInstanceStatusResponseTest = resp
    "reportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse"
    (Proxy :: Proxy ReportInstanceStatus)

requestSpotFleetResponseTest :: RequestSpotFleetResponse -> TestTree
requestSpotFleetResponseTest = resp
    "requestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse"
    (Proxy :: Proxy RequestSpotFleet)

requestSpotInstancesResponseTest :: RequestSpotInstancesResponse -> TestTree
requestSpotInstancesResponseTest = resp
    "requestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse"
    (Proxy :: Proxy RequestSpotInstances)

resetImageAttributeResponseTest :: ResetImageAttributeResponse -> TestTree
resetImageAttributeResponseTest = resp
    "resetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse"
    (Proxy :: Proxy ResetImageAttribute)

resetInstanceAttributeResponseTest :: ResetInstanceAttributeResponse -> TestTree
resetInstanceAttributeResponseTest = resp
    "resetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse"
    (Proxy :: Proxy ResetInstanceAttribute)

resetNetworkInterfaceAttributeResponseTest :: ResetNetworkInterfaceAttributeResponse -> TestTree
resetNetworkInterfaceAttributeResponseTest = resp
    "resetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse"
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

resetSnapshotAttributeResponseTest :: ResetSnapshotAttributeResponse -> TestTree
resetSnapshotAttributeResponseTest = resp
    "resetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse"
    (Proxy :: Proxy ResetSnapshotAttribute)

restoreAddressToClassicResponseTest :: RestoreAddressToClassicResponse -> TestTree
restoreAddressToClassicResponseTest = resp
    "restoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse"
    (Proxy :: Proxy RestoreAddressToClassic)

revokeSecurityGroupEgressResponseTest :: RevokeSecurityGroupEgressResponse -> TestTree
revokeSecurityGroupEgressResponseTest = resp
    "revokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse"
    (Proxy :: Proxy RevokeSecurityGroupEgress)

revokeSecurityGroupIngressResponseTest :: RevokeSecurityGroupIngressResponse -> TestTree
revokeSecurityGroupIngressResponseTest = resp
    "revokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeSecurityGroupIngress)

runInstancesResponseTest :: Reservation -> TestTree
runInstancesResponseTest = resp
    "runInstancesResponse"
    "fixture/Reservation"
    (Proxy :: Proxy RunInstances)

startInstancesResponseTest :: StartInstancesResponse -> TestTree
startInstancesResponseTest = resp
    "startInstancesResponse"
    "fixture/StartInstancesResponse"
    (Proxy :: Proxy StartInstances)

stopInstancesResponseTest :: StopInstancesResponse -> TestTree
stopInstancesResponseTest = resp
    "stopInstancesResponse"
    "fixture/StopInstancesResponse"
    (Proxy :: Proxy StopInstances)

terminateInstancesResponseTest :: TerminateInstancesResponse -> TestTree
terminateInstancesResponseTest = resp
    "terminateInstancesResponse"
    "fixture/TerminateInstancesResponse"
    (Proxy :: Proxy TerminateInstances)

unassignPrivateIPAddressesResponseTest :: UnassignPrivateIPAddressesResponse -> TestTree
unassignPrivateIPAddressesResponseTest = resp
    "unassignPrivateIPAddressesResponse"
    "fixture/UnassignPrivateIPAddressesResponse"
    (Proxy :: Proxy UnassignPrivateIPAddresses)

unmonitorInstancesResponseTest :: UnmonitorInstancesResponse -> TestTree
unmonitorInstancesResponseTest = resp
    "unmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse"
    (Proxy :: Proxy UnmonitorInstances)
