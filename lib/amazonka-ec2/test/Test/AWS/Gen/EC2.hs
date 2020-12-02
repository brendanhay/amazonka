{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EC2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestImportInstance $
--             importInstance
--
--         , requestRevokeSecurityGroupEgress $
--             revokeSecurityGroupEgress
--
--         , requestCreateNetworkInterfacePermission $
--             createNetworkInterfacePermission
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
--         , requestDeleteFlowLogs $
--             deleteFlowLogs
--
--         , requestDescribeVPCClassicLink $
--             describeVPCClassicLink
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
--         , requestAcceptReservedInstancesExchangeQuote $
--             acceptReservedInstancesExchangeQuote
--
--         , requestReleaseAddress $
--             releaseAddress
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
--         , requestDeregisterImage $
--             deregisterImage
--
--         , requestDeleteVPCEndpointConnectionNotifications $
--             deleteVPCEndpointConnectionNotifications
--
--         , requestModifyNetworkInterfaceAttribute $
--             modifyNetworkInterfaceAttribute
--
--         , requestModifyVPCTenancy $
--             modifyVPCTenancy
--
--         , requestCancelReservedInstancesListing $
--             cancelReservedInstancesListing
--
--         , requestAttachClassicLinkVPC $
--             attachClassicLinkVPC
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
--         , requestCopyFpgaImage $
--             copyFpgaImage
--
--         , requestStopInstances $
--             stopInstances
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
--         , requestAuthorizeSecurityGroupIngress $
--             authorizeSecurityGroupIngress
--
--         , requestCreateVPCPeeringConnection $
--             createVPCPeeringConnection
--
--         , requestDescribeSubnets $
--             describeSubnets
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
--         , requestDeleteRoute $
--             deleteRoute
--
--         , requestReplaceNetworkACLEntry $
--             replaceNetworkACLEntry
--
--         , requestDescribeVPCEndpoints $
--             describeVPCEndpoints
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
--         , requestDeleteSecurityGroup $
--             deleteSecurityGroup
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
--         , requestDescribeVPCEndpointConnectionNotifications $
--             describeVPCEndpointConnectionNotifications
--
--         , requestRunInstances $
--             runInstances
--
--         , requestAssociateDHCPOptions $
--             associateDHCPOptions
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
--         , requestDisableVPCClassicLinkDNSSupport $
--             disableVPCClassicLinkDNSSupport
--
--         , requestDescribeVolumesModifications $
--             describeVolumesModifications
--
--         , requestCreateFpgaImage $
--             createFpgaImage
--
--         , requestAcceptVPCEndpointConnections $
--             acceptVPCEndpointConnections
--
--         , requestGetLaunchTemplateData $
--             getLaunchTemplateData
--
--         , requestAllocateAddress $
--             allocateAddress
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
--         , requestReportInstanceStatus $
--             reportInstanceStatus
--
--         , requestAttachVolume $
--             attachVolume
--
--         , requestRequestSpotInstances $
--             requestSpotInstances
--
--         , requestDescribeHostReservationOfferings $
--             describeHostReservationOfferings
--
--         , requestResetFpgaImageAttribute $
--             resetFpgaImageAttribute
--
--         , requestDescribeVolumes $
--             describeVolumes
--
--         , requestRejectVPCPeeringConnection $
--             rejectVPCPeeringConnection
--
--         , requestDeleteVPNConnectionRoute $
--             deleteVPNConnectionRoute
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
--         , requestRegisterImage $
--             registerImage
--
--         , requestModifyFleet $
--             modifyFleet
--
--         , requestRevokeSecurityGroupIngress $
--             revokeSecurityGroupIngress
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
--         , requestDescribeScheduledInstances $
--             describeScheduledInstances
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
--         , requestDeleteNetworkACL $
--             deleteNetworkACL
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
--         , requestDescribeImportImageTasks $
--             describeImportImageTasks
--
--         , requestDescribeVolumeAttribute $
--             describeVolumeAttribute
--
--         , requestDescribeMovingAddresses $
--             describeMovingAddresses
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
--         , requestDisassociateAddress $
--             disassociateAddress
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
--         , requestModifyVolumeAttribute $
--             modifyVolumeAttribute
--
--         , requestDescribeSpotDatafeedSubscription $
--             describeSpotDatafeedSubscription
--
--         , requestDescribePrefixLists $
--             describePrefixLists
--
--         , requestDeletePlacementGroup $
--             deletePlacementGroup
--
--         , requestRequestSpotFleet $
--             requestSpotFleet
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
--         , requestDescribeExportTasks $
--             describeExportTasks
--
--         , requestModifySpotFleetRequest $
--             modifySpotFleetRequest
--
--         , requestDetachVPNGateway $
--             detachVPNGateway
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
--         , requestDeleteNetworkInterface $
--             deleteNetworkInterface
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
--         , requestDescribeAggregateIdFormat $
--             describeAggregateIdFormat
--
--         , requestDescribeSnapshots $
--             describeSnapshots
--
--         , requestAssociateAddress $
--             associateAddress
--
--         , requestDescribeNetworkInterfaceAttribute $
--             describeNetworkInterfaceAttribute
--
--         , requestReplaceIAMInstanceProfileAssociation $
--             replaceIAMInstanceProfileAssociation
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
--         , requestCreateFleet $
--             createFleet
--
--         , requestDeleteNatGateway $
--             deleteNatGateway
--
--         , requestDescribeImportSnapshotTasks $
--             describeImportSnapshotTasks
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
--         , requestUnmonitorInstances $
--             unmonitorInstances
--
--         , requestCreateSecurityGroup $
--             createSecurityGroup
--
--         , requestImportVolume $
--             importVolume
--
--         , requestDisableVGWRoutePropagation $
--             disableVGWRoutePropagation
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
--         , requestModifyInstanceAttribute $
--             modifyInstanceAttribute
--
--         , requestDeleteCustomerGateway $
--             deleteCustomerGateway
--
--         , requestDisassociateIAMInstanceProfile $
--             disassociateIAMInstanceProfile
--
--         , requestDisassociateRouteTable $
--             disassociateRouteTable
--
--         , requestGetConsoleScreenshot $
--             getConsoleScreenshot
--
--         , requestAssignIPv6Addresses $
--             assignIPv6Addresses
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
--         , requestDescribeStaleSecurityGroups $
--             describeStaleSecurityGroups
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
--         , requestCreateNetworkACLEntry $
--             createNetworkACLEntry
--
--         , requestCreateReservedInstancesListing $
--             createReservedInstancesListing
--
--         , requestAttachVPNGateway $
--             attachVPNGateway
--
--         , requestModifyVPCEndpointServicePermissions $
--             modifyVPCEndpointServicePermissions
--
--         , requestCreateDHCPOptions $
--             createDHCPOptions
--
--         , requestDescribeAccountAttributes $
--             describeAccountAttributes
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
--         , requestUnassignIPv6Addresses $
--             unassignIPv6Addresses
--
--         , requestAssociateIAMInstanceProfile $
--             associateIAMInstanceProfile
--
--         , requestDeleteEgressOnlyInternetGateway $
--             deleteEgressOnlyInternetGateway
--
--         , requestPurchaseHostReservation $
--             purchaseHostReservation
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
--         , requestDescribeKeyPairs $
--             describeKeyPairs
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
--         , requestDeleteRouteTable $
--             deleteRouteTable
--
--         , requestResetImageAttribute $
--             resetImageAttribute
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
--         , requestUnassignPrivateIPAddresses $
--             unassignPrivateIPAddresses
--
--         , requestDescribeNetworkInterfacePermissions $
--             describeNetworkInterfacePermissions
--
--         , requestDescribeVPCEndpointServicePermissions $
--             describeVPCEndpointServicePermissions
--
--         , requestDeleteDHCPOptions $
--             deleteDHCPOptions
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
--         , requestCreateKeyPair $
--             createKeyPair
--
--         , requestGetReservedInstancesExchangeQuote $
--             getReservedInstancesExchangeQuote
--
--         , requestDeleteVolume $
--             deleteVolume
--
--         , requestDeleteVPCEndpointServiceConfigurations $
--             deleteVPCEndpointServiceConfigurations
--
--         , requestDescribeSpotFleetInstances $
--             describeSpotFleetInstances
--
--           ]

--     , testGroup "response"
--         [ responseImportInstance $
--             importInstanceResponse
--
--         , responseRevokeSecurityGroupEgress $
--             revokeSecurityGroupEgressResponse
--
--         , responseCreateNetworkInterfacePermission $
--             createNetworkInterfacePermissionResponse
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
--         , responseDeleteFlowLogs $
--             deleteFlowLogsResponse
--
--         , responseDescribeVPCClassicLink $
--             describeVPCClassicLinkResponse
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
--         , responseAcceptReservedInstancesExchangeQuote $
--             acceptReservedInstancesExchangeQuoteResponse
--
--         , responseReleaseAddress $
--             releaseAddressResponse
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
--         , responseDeregisterImage $
--             deregisterImageResponse
--
--         , responseDeleteVPCEndpointConnectionNotifications $
--             deleteVPCEndpointConnectionNotificationsResponse
--
--         , responseModifyNetworkInterfaceAttribute $
--             modifyNetworkInterfaceAttributeResponse
--
--         , responseModifyVPCTenancy $
--             modifyVPCTenancyResponse
--
--         , responseCancelReservedInstancesListing $
--             cancelReservedInstancesListingResponse
--
--         , responseAttachClassicLinkVPC $
--             attachClassicLinkVPCResponse
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
--         , responseCopyFpgaImage $
--             copyFpgaImageResponse
--
--         , responseStopInstances $
--             stopInstancesResponse
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
--         , responseAuthorizeSecurityGroupIngress $
--             authorizeSecurityGroupIngressResponse
--
--         , responseCreateVPCPeeringConnection $
--             createVPCPeeringConnectionResponse
--
--         , responseDescribeSubnets $
--             describeSubnetsResponse
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
--         , responseDeleteRoute $
--             deleteRouteResponse
--
--         , responseReplaceNetworkACLEntry $
--             replaceNetworkACLEntryResponse
--
--         , responseDescribeVPCEndpoints $
--             describeVPCEndpointsResponse
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
--         , responseDeleteSecurityGroup $
--             deleteSecurityGroupResponse
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
--         , responseDescribeVPCEndpointConnectionNotifications $
--             describeVPCEndpointConnectionNotificationsResponse
--
--         , responseRunInstances $
--             reservation
--
--         , responseAssociateDHCPOptions $
--             associateDHCPOptionsResponse
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
--         , responseDisableVPCClassicLinkDNSSupport $
--             disableVPCClassicLinkDNSSupportResponse
--
--         , responseDescribeVolumesModifications $
--             describeVolumesModificationsResponse
--
--         , responseCreateFpgaImage $
--             createFpgaImageResponse
--
--         , responseAcceptVPCEndpointConnections $
--             acceptVPCEndpointConnectionsResponse
--
--         , responseGetLaunchTemplateData $
--             getLaunchTemplateDataResponse
--
--         , responseAllocateAddress $
--             allocateAddressResponse
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
--         , responseReportInstanceStatus $
--             reportInstanceStatusResponse
--
--         , responseAttachVolume $
--             volumeAttachment
--
--         , responseRequestSpotInstances $
--             requestSpotInstancesResponse
--
--         , responseDescribeHostReservationOfferings $
--             describeHostReservationOfferingsResponse
--
--         , responseResetFpgaImageAttribute $
--             resetFpgaImageAttributeResponse
--
--         , responseDescribeVolumes $
--             describeVolumesResponse
--
--         , responseRejectVPCPeeringConnection $
--             rejectVPCPeeringConnectionResponse
--
--         , responseDeleteVPNConnectionRoute $
--             deleteVPNConnectionRouteResponse
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
--         , responseRegisterImage $
--             registerImageResponse
--
--         , responseModifyFleet $
--             modifyFleetResponse
--
--         , responseRevokeSecurityGroupIngress $
--             revokeSecurityGroupIngressResponse
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
--         , responseDescribeScheduledInstances $
--             describeScheduledInstancesResponse
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
--         , responseDeleteNetworkACL $
--             deleteNetworkACLResponse
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
--         , responseDescribeImportImageTasks $
--             describeImportImageTasksResponse
--
--         , responseDescribeVolumeAttribute $
--             describeVolumeAttributeResponse
--
--         , responseDescribeMovingAddresses $
--             describeMovingAddressesResponse
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
--         , responseDisassociateAddress $
--             disassociateAddressResponse
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
--         , responseModifyVolumeAttribute $
--             modifyVolumeAttributeResponse
--
--         , responseDescribeSpotDatafeedSubscription $
--             describeSpotDatafeedSubscriptionResponse
--
--         , responseDescribePrefixLists $
--             describePrefixListsResponse
--
--         , responseDeletePlacementGroup $
--             deletePlacementGroupResponse
--
--         , responseRequestSpotFleet $
--             requestSpotFleetResponse
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
--         , responseDescribeExportTasks $
--             describeExportTasksResponse
--
--         , responseModifySpotFleetRequest $
--             modifySpotFleetRequestResponse
--
--         , responseDetachVPNGateway $
--             detachVPNGatewayResponse
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
--         , responseDeleteNetworkInterface $
--             deleteNetworkInterfaceResponse
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
--         , responseDescribeAggregateIdFormat $
--             describeAggregateIdFormatResponse
--
--         , responseDescribeSnapshots $
--             describeSnapshotsResponse
--
--         , responseAssociateAddress $
--             associateAddressResponse
--
--         , responseDescribeNetworkInterfaceAttribute $
--             describeNetworkInterfaceAttributeResponse
--
--         , responseReplaceIAMInstanceProfileAssociation $
--             replaceIAMInstanceProfileAssociationResponse
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
--         , responseCreateFleet $
--             createFleetResponse
--
--         , responseDeleteNatGateway $
--             deleteNatGatewayResponse
--
--         , responseDescribeImportSnapshotTasks $
--             describeImportSnapshotTasksResponse
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
--         , responseUnmonitorInstances $
--             unmonitorInstancesResponse
--
--         , responseCreateSecurityGroup $
--             createSecurityGroupResponse
--
--         , responseImportVolume $
--             importVolumeResponse
--
--         , responseDisableVGWRoutePropagation $
--             disableVGWRoutePropagationResponse
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
--         , responseModifyInstanceAttribute $
--             modifyInstanceAttributeResponse
--
--         , responseDeleteCustomerGateway $
--             deleteCustomerGatewayResponse
--
--         , responseDisassociateIAMInstanceProfile $
--             disassociateIAMInstanceProfileResponse
--
--         , responseDisassociateRouteTable $
--             disassociateRouteTableResponse
--
--         , responseGetConsoleScreenshot $
--             getConsoleScreenshotResponse
--
--         , responseAssignIPv6Addresses $
--             assignIPv6AddressesResponse
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
--         , responseDescribeStaleSecurityGroups $
--             describeStaleSecurityGroupsResponse
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
--         , responseCreateNetworkACLEntry $
--             createNetworkACLEntryResponse
--
--         , responseCreateReservedInstancesListing $
--             createReservedInstancesListingResponse
--
--         , responseAttachVPNGateway $
--             attachVPNGatewayResponse
--
--         , responseModifyVPCEndpointServicePermissions $
--             modifyVPCEndpointServicePermissionsResponse
--
--         , responseCreateDHCPOptions $
--             createDHCPOptionsResponse
--
--         , responseDescribeAccountAttributes $
--             describeAccountAttributesResponse
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
--         , responseUnassignIPv6Addresses $
--             unassignIPv6AddressesResponse
--
--         , responseAssociateIAMInstanceProfile $
--             associateIAMInstanceProfileResponse
--
--         , responseDeleteEgressOnlyInternetGateway $
--             deleteEgressOnlyInternetGatewayResponse
--
--         , responsePurchaseHostReservation $
--             purchaseHostReservationResponse
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
--         , responseDescribeKeyPairs $
--             describeKeyPairsResponse
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
--         , responseDeleteRouteTable $
--             deleteRouteTableResponse
--
--         , responseResetImageAttribute $
--             resetImageAttributeResponse
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
--         , responseUnassignPrivateIPAddresses $
--             unassignPrivateIPAddressesResponse
--
--         , responseDescribeNetworkInterfacePermissions $
--             describeNetworkInterfacePermissionsResponse
--
--         , responseDescribeVPCEndpointServicePermissions $
--             describeVPCEndpointServicePermissionsResponse
--
--         , responseDeleteDHCPOptions $
--             deleteDHCPOptionsResponse
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
--         , responseCreateKeyPair $
--             createKeyPairResponse
--
--         , responseGetReservedInstancesExchangeQuote $
--             getReservedInstancesExchangeQuoteResponse
--
--         , responseDeleteVolume $
--             deleteVolumeResponse
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

requestImportInstance :: ImportInstance -> TestTree
requestImportInstance = req
    "ImportInstance"
    "fixture/ImportInstance.yaml"

requestRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
requestRevokeSecurityGroupEgress = req
    "RevokeSecurityGroupEgress"
    "fixture/RevokeSecurityGroupEgress.yaml"

requestCreateNetworkInterfacePermission :: CreateNetworkInterfacePermission -> TestTree
requestCreateNetworkInterfacePermission = req
    "CreateNetworkInterfacePermission"
    "fixture/CreateNetworkInterfacePermission.yaml"

requestDeleteLaunchTemplate :: DeleteLaunchTemplate -> TestTree
requestDeleteLaunchTemplate = req
    "DeleteLaunchTemplate"
    "fixture/DeleteLaunchTemplate.yaml"

requestRejectVPCEndpointConnections :: RejectVPCEndpointConnections -> TestTree
requestRejectVPCEndpointConnections = req
    "RejectVPCEndpointConnections"
    "fixture/RejectVPCEndpointConnections.yaml"

requestCreateVPNGateway :: CreateVPNGateway -> TestTree
requestCreateVPNGateway = req
    "CreateVPNGateway"
    "fixture/CreateVPNGateway.yaml"

requestCreateNetworkACL :: CreateNetworkACL -> TestTree
requestCreateNetworkACL = req
    "CreateNetworkACL"
    "fixture/CreateNetworkACL.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair = req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestDescribeSecurityGroupReferences :: DescribeSecurityGroupReferences -> TestTree
requestDescribeSecurityGroupReferences = req
    "DescribeSecurityGroupReferences"
    "fixture/DescribeSecurityGroupReferences.yaml"

requestDeleteFleets :: DeleteFleets -> TestTree
requestDeleteFleets = req
    "DeleteFleets"
    "fixture/DeleteFleets.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsIngress = req
    "UpdateSecurityGroupRuleDescriptionsIngress"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngress.yaml"

requestDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlock -> TestTree
requestDisassociateSubnetCidrBlock = req
    "DisassociateSubnetCidrBlock"
    "fixture/DisassociateSubnetCidrBlock.yaml"

requestDetachNetworkInterface :: DetachNetworkInterface -> TestTree
requestDetachNetworkInterface = req
    "DetachNetworkInterface"
    "fixture/DetachNetworkInterface.yaml"

requestDetachInternetGateway :: DetachInternetGateway -> TestTree
requestDetachInternetGateway = req
    "DetachInternetGateway"
    "fixture/DetachInternetGateway.yaml"

requestDeleteVPCEndpoints :: DeleteVPCEndpoints -> TestTree
requestDeleteVPCEndpoints = req
    "DeleteVPCEndpoints"
    "fixture/DeleteVPCEndpoints.yaml"

requestDeleteFlowLogs :: DeleteFlowLogs -> TestTree
requestDeleteFlowLogs = req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs.yaml"

requestDescribeVPCClassicLink :: DescribeVPCClassicLink -> TestTree
requestDescribeVPCClassicLink = req
    "DescribeVPCClassicLink"
    "fixture/DescribeVPCClassicLink.yaml"

requestModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
requestModifySubnetAttribute = req
    "ModifySubnetAttribute"
    "fixture/ModifySubnetAttribute.yaml"

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume = req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecifications -> TestTree
requestDescribeInstanceCreditSpecifications = req
    "DescribeInstanceCreditSpecifications"
    "fixture/DescribeInstanceCreditSpecifications.yaml"

requestCancelBundleTask :: CancelBundleTask -> TestTree
requestCancelBundleTask = req
    "CancelBundleTask"
    "fixture/CancelBundleTask.yaml"

requestAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuote -> TestTree
requestAcceptReservedInstancesExchangeQuote = req
    "AcceptReservedInstancesExchangeQuote"
    "fixture/AcceptReservedInstancesExchangeQuote.yaml"

requestReleaseAddress :: ReleaseAddress -> TestTree
requestReleaseAddress = req
    "ReleaseAddress"
    "fixture/ReleaseAddress.yaml"

requestCreateInternetGateway :: CreateInternetGateway -> TestTree
requestCreateInternetGateway = req
    "CreateInternetGateway"
    "fixture/CreateInternetGateway.yaml"

requestDeleteVPNConnection :: DeleteVPNConnection -> TestTree
requestDeleteVPNConnection = req
    "DeleteVPNConnection"
    "fixture/DeleteVPNConnection.yaml"

requestDescribeBundleTasks :: DescribeBundleTasks -> TestTree
requestDescribeBundleTasks = req
    "DescribeBundleTasks"
    "fixture/DescribeBundleTasks.yaml"

requestAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
requestAuthorizeSecurityGroupEgress = req
    "AuthorizeSecurityGroupEgress"
    "fixture/AuthorizeSecurityGroupEgress.yaml"

requestDeregisterImage :: DeregisterImage -> TestTree
requestDeregisterImage = req
    "DeregisterImage"
    "fixture/DeregisterImage.yaml"

requestDeleteVPCEndpointConnectionNotifications :: DeleteVPCEndpointConnectionNotifications -> TestTree
requestDeleteVPCEndpointConnectionNotifications = req
    "DeleteVPCEndpointConnectionNotifications"
    "fixture/DeleteVPCEndpointConnectionNotifications.yaml"

requestModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
requestModifyNetworkInterfaceAttribute = req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute.yaml"

requestModifyVPCTenancy :: ModifyVPCTenancy -> TestTree
requestModifyVPCTenancy = req
    "ModifyVPCTenancy"
    "fixture/ModifyVPCTenancy.yaml"

requestCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
requestCancelReservedInstancesListing = req
    "CancelReservedInstancesListing"
    "fixture/CancelReservedInstancesListing.yaml"

requestAttachClassicLinkVPC :: AttachClassicLinkVPC -> TestTree
requestAttachClassicLinkVPC = req
    "AttachClassicLinkVPC"
    "fixture/AttachClassicLinkVPC.yaml"

requestDescribeVPCClassicLinkDNSSupport :: DescribeVPCClassicLinkDNSSupport -> TestTree
requestDescribeVPCClassicLinkDNSSupport = req
    "DescribeVPCClassicLinkDNSSupport"
    "fixture/DescribeVPCClassicLinkDNSSupport.yaml"

requestAssociateSubnetCidrBlock :: AssociateSubnetCidrBlock -> TestTree
requestAssociateSubnetCidrBlock = req
    "AssociateSubnetCidrBlock"
    "fixture/AssociateSubnetCidrBlock.yaml"

requestRunScheduledInstances :: RunScheduledInstances -> TestTree
requestRunScheduledInstances = req
    "RunScheduledInstances"
    "fixture/RunScheduledInstances.yaml"

requestCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
requestCancelSpotFleetRequests = req
    "CancelSpotFleetRequests"
    "fixture/CancelSpotFleetRequests.yaml"

requestDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
requestDescribeSpotPriceHistory = req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory.yaml"

requestDescribeDHCPOptions :: DescribeDHCPOptions -> TestTree
requestDescribeDHCPOptions = req
    "DescribeDHCPOptions"
    "fixture/DescribeDHCPOptions.yaml"

requestImportImage :: ImportImage -> TestTree
requestImportImage = req
    "ImportImage"
    "fixture/ImportImage.yaml"

requestCopyFpgaImage :: CopyFpgaImage -> TestTree
requestCopyFpgaImage = req
    "CopyFpgaImage"
    "fixture/CopyFpgaImage.yaml"

requestStopInstances :: StopInstances -> TestTree
requestStopInstances = req
    "StopInstances"
    "fixture/StopInstances.yaml"

requestModifyLaunchTemplate :: ModifyLaunchTemplate -> TestTree
requestModifyLaunchTemplate = req
    "ModifyLaunchTemplate"
    "fixture/ModifyLaunchTemplate.yaml"

requestModifyVPCEndpointConnectionNotification :: ModifyVPCEndpointConnectionNotification -> TestTree
requestModifyVPCEndpointConnectionNotification = req
    "ModifyVPCEndpointConnectionNotification"
    "fixture/ModifyVPCEndpointConnectionNotification.yaml"

requestDescribeInternetGateways :: DescribeInternetGateways -> TestTree
requestDescribeInternetGateways = req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways.yaml"

requestDisableVPCClassicLink :: DisableVPCClassicLink -> TestTree
requestDisableVPCClassicLink = req
    "DisableVPCClassicLink"
    "fixture/DisableVPCClassicLink.yaml"

requestDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersions -> TestTree
requestDeleteLaunchTemplateVersions = req
    "DeleteLaunchTemplateVersions"
    "fixture/DeleteLaunchTemplateVersions.yaml"

requestBundleInstance :: BundleInstance -> TestTree
requestBundleInstance = req
    "BundleInstance"
    "fixture/BundleInstance.yaml"

requestDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
requestDescribeNetworkInterfaces = req
    "DescribeNetworkInterfaces"
    "fixture/DescribeNetworkInterfaces.yaml"

requestReplaceNetworkACLAssociation :: ReplaceNetworkACLAssociation -> TestTree
requestReplaceNetworkACLAssociation = req
    "ReplaceNetworkACLAssociation"
    "fixture/ReplaceNetworkACLAssociation.yaml"

requestDescribeNatGateways :: DescribeNatGateways -> TestTree
requestDescribeNatGateways = req
    "DescribeNatGateways"
    "fixture/DescribeNatGateways.yaml"

requestDescribeAddresses :: DescribeAddresses -> TestTree
requestDescribeAddresses = req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

requestDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
requestDescribeSnapshotAttribute = req
    "DescribeSnapshotAttribute"
    "fixture/DescribeSnapshotAttribute.yaml"

requestDescribeIdentityIdFormat :: DescribeIdentityIdFormat -> TestTree
requestDescribeIdentityIdFormat = req
    "DescribeIdentityIdFormat"
    "fixture/DescribeIdentityIdFormat.yaml"

requestReplaceRoute :: ReplaceRoute -> TestTree
requestReplaceRoute = req
    "ReplaceRoute"
    "fixture/ReplaceRoute.yaml"

requestDescribeVPCEndpointServices :: DescribeVPCEndpointServices -> TestTree
requestDescribeVPCEndpointServices = req
    "DescribeVPCEndpointServices"
    "fixture/DescribeVPCEndpointServices.yaml"

requestAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
requestAuthorizeSecurityGroupIngress = req
    "AuthorizeSecurityGroupIngress"
    "fixture/AuthorizeSecurityGroupIngress.yaml"

requestCreateVPCPeeringConnection :: CreateVPCPeeringConnection -> TestTree
requestCreateVPCPeeringConnection = req
    "CreateVPCPeeringConnection"
    "fixture/CreateVPCPeeringConnection.yaml"

requestDescribeSubnets :: DescribeSubnets -> TestTree
requestDescribeSubnets = req
    "DescribeSubnets"
    "fixture/DescribeSubnets.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
requestPurchaseReservedInstancesOffering = req
    "PurchaseReservedInstancesOffering"
    "fixture/PurchaseReservedInstancesOffering.yaml"

requestDeleteNetworkACLEntry :: DeleteNetworkACLEntry -> TestTree
requestDeleteNetworkACLEntry = req
    "DeleteNetworkACLEntry"
    "fixture/DeleteNetworkACLEntry.yaml"

requestResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
requestResetSnapshotAttribute = req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute.yaml"

requestDescribeVPNConnections :: DescribeVPNConnections -> TestTree
requestDescribeVPNConnections = req
    "DescribeVPNConnections"
    "fixture/DescribeVPNConnections.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute = req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestReplaceNetworkACLEntry :: ReplaceNetworkACLEntry -> TestTree
requestReplaceNetworkACLEntry = req
    "ReplaceNetworkACLEntry"
    "fixture/ReplaceNetworkACLEntry.yaml"

requestDescribeVPCEndpoints :: DescribeVPCEndpoints -> TestTree
requestDescribeVPCEndpoints = req
    "DescribeVPCEndpoints"
    "fixture/DescribeVPCEndpoints.yaml"

requestResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
requestResetInstanceAttribute = req
    "ResetInstanceAttribute"
    "fixture/ResetInstanceAttribute.yaml"

requestModifyIdentityIdFormat :: ModifyIdentityIdFormat -> TestTree
requestModifyIdentityIdFormat = req
    "ModifyIdentityIdFormat"
    "fixture/ModifyIdentityIdFormat.yaml"

requestAttachNetworkInterface :: AttachNetworkInterface -> TestTree
requestAttachNetworkInterface = req
    "AttachNetworkInterface"
    "fixture/AttachNetworkInterface.yaml"

requestDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
requestDescribeInstanceStatus = req
    "DescribeInstanceStatus"
    "fixture/DescribeInstanceStatus.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair = req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestConfirmProductInstance :: ConfirmProductInstance -> TestTree
requestConfirmProductInstance = req
    "ConfirmProductInstance"
    "fixture/ConfirmProductInstance.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute = req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
requestDescribeReservedInstancesOfferings = req
    "DescribeReservedInstancesOfferings"
    "fixture/DescribeReservedInstancesOfferings.yaml"

requestCreateCustomerGateway :: CreateCustomerGateway -> TestTree
requestCreateCustomerGateway = req
    "CreateCustomerGateway"
    "fixture/CreateCustomerGateway.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets = req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
requestDeleteSecurityGroup = req
    "DeleteSecurityGroup"
    "fixture/DeleteSecurityGroup.yaml"

requestDeleteVPCPeeringConnection :: DeleteVPCPeeringConnection -> TestTree
requestDeleteVPCPeeringConnection = req
    "DeleteVPCPeeringConnection"
    "fixture/DeleteVPCPeeringConnection.yaml"

requestAttachInternetGateway :: AttachInternetGateway -> TestTree
requestAttachInternetGateway = req
    "AttachInternetGateway"
    "fixture/AttachInternetGateway.yaml"

requestModifyInstancePlacement :: ModifyInstancePlacement -> TestTree
requestModifyInstancePlacement = req
    "ModifyInstancePlacement"
    "fixture/ModifyInstancePlacement.yaml"

requestDescribeFlowLogs :: DescribeFlowLogs -> TestTree
requestDescribeFlowLogs = req
    "DescribeFlowLogs"
    "fixture/DescribeFlowLogs.yaml"

requestDescribeVPCEndpointConnectionNotifications :: DescribeVPCEndpointConnectionNotifications -> TestTree
requestDescribeVPCEndpointConnectionNotifications = req
    "DescribeVPCEndpointConnectionNotifications"
    "fixture/DescribeVPCEndpointConnectionNotifications.yaml"

requestRunInstances :: RunInstances -> TestTree
requestRunInstances = req
    "RunInstances"
    "fixture/RunInstances.yaml"

requestAssociateDHCPOptions :: AssociateDHCPOptions -> TestTree
requestAssociateDHCPOptions = req
    "AssociateDHCPOptions"
    "fixture/AssociateDHCPOptions.yaml"

requestDescribeReservedInstances :: DescribeReservedInstances -> TestTree
requestDescribeReservedInstances = req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances.yaml"

requestDescribeIdFormat :: DescribeIdFormat -> TestTree
requestDescribeIdFormat = req
    "DescribeIdFormat"
    "fixture/DescribeIdFormat.yaml"

requestDescribeVPCs :: DescribeVPCs -> TestTree
requestDescribeVPCs = req
    "DescribeVPCs"
    "fixture/DescribeVPCs.yaml"

requestDescribeConversionTasks :: DescribeConversionTasks -> TestTree
requestDescribeConversionTasks = req
    "DescribeConversionTasks"
    "fixture/DescribeConversionTasks.yaml"

requestCreateLaunchTemplateVersion :: CreateLaunchTemplateVersion -> TestTree
requestCreateLaunchTemplateVersion = req
    "CreateLaunchTemplateVersion"
    "fixture/CreateLaunchTemplateVersion.yaml"

requestDisableVPCClassicLinkDNSSupport :: DisableVPCClassicLinkDNSSupport -> TestTree
requestDisableVPCClassicLinkDNSSupport = req
    "DisableVPCClassicLinkDNSSupport"
    "fixture/DisableVPCClassicLinkDNSSupport.yaml"

requestDescribeVolumesModifications :: DescribeVolumesModifications -> TestTree
requestDescribeVolumesModifications = req
    "DescribeVolumesModifications"
    "fixture/DescribeVolumesModifications.yaml"

requestCreateFpgaImage :: CreateFpgaImage -> TestTree
requestCreateFpgaImage = req
    "CreateFpgaImage"
    "fixture/CreateFpgaImage.yaml"

requestAcceptVPCEndpointConnections :: AcceptVPCEndpointConnections -> TestTree
requestAcceptVPCEndpointConnections = req
    "AcceptVPCEndpointConnections"
    "fixture/AcceptVPCEndpointConnections.yaml"

requestGetLaunchTemplateData :: GetLaunchTemplateData -> TestTree
requestGetLaunchTemplateData = req
    "GetLaunchTemplateData"
    "fixture/GetLaunchTemplateData.yaml"

requestAllocateAddress :: AllocateAddress -> TestTree
requestAllocateAddress = req
    "AllocateAddress"
    "fixture/AllocateAddress.yaml"

requestCancelConversionTask :: CancelConversionTask -> TestTree
requestCancelConversionTask = req
    "CancelConversionTask"
    "fixture/CancelConversionTask.yaml"

requestModifyImageAttribute :: ModifyImageAttribute -> TestTree
requestModifyImageAttribute = req
    "ModifyImageAttribute"
    "fixture/ModifyImageAttribute.yaml"

requestCreateRouteTable :: CreateRouteTable -> TestTree
requestCreateRouteTable = req
    "CreateRouteTable"
    "fixture/CreateRouteTable.yaml"

requestReportInstanceStatus :: ReportInstanceStatus -> TestTree
requestReportInstanceStatus = req
    "ReportInstanceStatus"
    "fixture/ReportInstanceStatus.yaml"

requestAttachVolume :: AttachVolume -> TestTree
requestAttachVolume = req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

requestRequestSpotInstances :: RequestSpotInstances -> TestTree
requestRequestSpotInstances = req
    "RequestSpotInstances"
    "fixture/RequestSpotInstances.yaml"

requestDescribeHostReservationOfferings :: DescribeHostReservationOfferings -> TestTree
requestDescribeHostReservationOfferings = req
    "DescribeHostReservationOfferings"
    "fixture/DescribeHostReservationOfferings.yaml"

requestResetFpgaImageAttribute :: ResetFpgaImageAttribute -> TestTree
requestResetFpgaImageAttribute = req
    "ResetFpgaImageAttribute"
    "fixture/ResetFpgaImageAttribute.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes = req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestRejectVPCPeeringConnection :: RejectVPCPeeringConnection -> TestTree
requestRejectVPCPeeringConnection = req
    "RejectVPCPeeringConnection"
    "fixture/RejectVPCPeeringConnection.yaml"

requestDeleteVPNConnectionRoute :: DeleteVPNConnectionRoute -> TestTree
requestDeleteVPNConnectionRoute = req
    "DeleteVPNConnectionRoute"
    "fixture/DeleteVPNConnectionRoute.yaml"

requestModifyVPCEndpoint :: ModifyVPCEndpoint -> TestTree
requestModifyVPCEndpoint = req
    "ModifyVPCEndpoint"
    "fixture/ModifyVPCEndpoint.yaml"

requestDescribeFpgaImageAttribute :: DescribeFpgaImageAttribute -> TestTree
requestDescribeFpgaImageAttribute = req
    "DescribeFpgaImageAttribute"
    "fixture/DescribeFpgaImageAttribute.yaml"

requestAllocateHosts :: AllocateHosts -> TestTree
requestAllocateHosts = req
    "AllocateHosts"
    "fixture/AllocateHosts.yaml"

requestRegisterImage :: RegisterImage -> TestTree
requestRegisterImage = req
    "RegisterImage"
    "fixture/RegisterImage.yaml"

requestModifyFleet :: ModifyFleet -> TestTree
requestModifyFleet = req
    "ModifyFleet"
    "fixture/ModifyFleet.yaml"

requestRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
requestRevokeSecurityGroupIngress = req
    "RevokeSecurityGroupIngress"
    "fixture/RevokeSecurityGroupIngress.yaml"

requestDescribeHostReservations :: DescribeHostReservations -> TestTree
requestDescribeHostReservations = req
    "DescribeHostReservations"
    "fixture/DescribeHostReservations.yaml"

requestUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsEgress = req
    "UpdateSecurityGroupRuleDescriptionsEgress"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgress.yaml"

requestEnableVPCClassicLinkDNSSupport :: EnableVPCClassicLinkDNSSupport -> TestTree
requestEnableVPCClassicLinkDNSSupport = req
    "EnableVPCClassicLinkDNSSupport"
    "fixture/EnableVPCClassicLinkDNSSupport.yaml"

requestDescribeVPCEndpointConnections :: DescribeVPCEndpointConnections -> TestTree
requestDescribeVPCEndpointConnections = req
    "DescribeVPCEndpointConnections"
    "fixture/DescribeVPCEndpointConnections.yaml"

requestModifyReservedInstances :: ModifyReservedInstances -> TestTree
requestModifyReservedInstances = req
    "ModifyReservedInstances"
    "fixture/ModifyReservedInstances.yaml"

requestDeleteFpgaImage :: DeleteFpgaImage -> TestTree
requestDeleteFpgaImage = req
    "DeleteFpgaImage"
    "fixture/DeleteFpgaImage.yaml"

requestDescribeScheduledInstances :: DescribeScheduledInstances -> TestTree
requestDescribeScheduledInstances = req
    "DescribeScheduledInstances"
    "fixture/DescribeScheduledInstances.yaml"

requestCreateFlowLogs :: CreateFlowLogs -> TestTree
requestCreateFlowLogs = req
    "CreateFlowLogs"
    "fixture/CreateFlowLogs.yaml"

requestDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
requestDescribeSpotFleetRequests = req
    "DescribeSpotFleetRequests"
    "fixture/DescribeSpotFleetRequests.yaml"

requestMoveAddressToVPC :: MoveAddressToVPC -> TestTree
requestMoveAddressToVPC = req
    "MoveAddressToVPC"
    "fixture/MoveAddressToVPC.yaml"

requestDescribeFleetInstances :: DescribeFleetInstances -> TestTree
requestDescribeFleetInstances = req
    "DescribeFleetInstances"
    "fixture/DescribeFleetInstances.yaml"

requestDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersions -> TestTree
requestDescribeLaunchTemplateVersions = req
    "DescribeLaunchTemplateVersions"
    "fixture/DescribeLaunchTemplateVersions.yaml"

requestModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecification -> TestTree
requestModifyInstanceCreditSpecification = req
    "ModifyInstanceCreditSpecification"
    "fixture/ModifyInstanceCreditSpecification.yaml"

requestDescribePrincipalIdFormat :: DescribePrincipalIdFormat -> TestTree
requestDescribePrincipalIdFormat = req
    "DescribePrincipalIdFormat"
    "fixture/DescribePrincipalIdFormat.yaml"

requestDeleteNetworkACL :: DeleteNetworkACL -> TestTree
requestDeleteNetworkACL = req
    "DeleteNetworkACL"
    "fixture/DeleteNetworkACL.yaml"

requestCreateLaunchTemplate :: CreateLaunchTemplate -> TestTree
requestCreateLaunchTemplate = req
    "CreateLaunchTemplate"
    "fixture/CreateLaunchTemplate.yaml"

requestCreateVPCEndpointConnectionNotification :: CreateVPCEndpointConnectionNotification -> TestTree
requestCreateVPCEndpointConnectionNotification = req
    "CreateVPCEndpointConnectionNotification"
    "fixture/CreateVPCEndpointConnectionNotification.yaml"

requestDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermission -> TestTree
requestDeleteNetworkInterfacePermission = req
    "DeleteNetworkInterfacePermission"
    "fixture/DeleteNetworkInterfacePermission.yaml"

requestDeleteVPNGateway :: DeleteVPNGateway -> TestTree
requestDeleteVPNGateway = req
    "DeleteVPNGateway"
    "fixture/DeleteVPNGateway.yaml"

requestDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
requestDescribeImportImageTasks = req
    "DescribeImportImageTasks"
    "fixture/DescribeImportImageTasks.yaml"

requestDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
requestDescribeVolumeAttribute = req
    "DescribeVolumeAttribute"
    "fixture/DescribeVolumeAttribute.yaml"

requestDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
requestDescribeMovingAddresses = req
    "DescribeMovingAddresses"
    "fixture/DescribeMovingAddresses.yaml"

requestGetPasswordData :: GetPasswordData -> TestTree
requestGetPasswordData = req
    "GetPasswordData"
    "fixture/GetPasswordData.yaml"

requestCreateVPC :: CreateVPC -> TestTree
requestCreateVPC = req
    "CreateVPC"
    "fixture/CreateVPC.yaml"

requestModifyVPCPeeringConnectionOptions :: ModifyVPCPeeringConnectionOptions -> TestTree
requestModifyVPCPeeringConnectionOptions = req
    "ModifyVPCPeeringConnectionOptions"
    "fixture/ModifyVPCPeeringConnectionOptions.yaml"

requestDescribeFpgaImages :: DescribeFpgaImages -> TestTree
requestDescribeFpgaImages = req
    "DescribeFpgaImages"
    "fixture/DescribeFpgaImages.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot = req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestDisassociateAddress :: DisassociateAddress -> TestTree
requestDisassociateAddress = req
    "DisassociateAddress"
    "fixture/DisassociateAddress.yaml"

requestDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGateways -> TestTree
requestDescribeEgressOnlyInternetGateways = req
    "DescribeEgressOnlyInternetGateways"
    "fixture/DescribeEgressOnlyInternetGateways.yaml"

requestDeleteVPC :: DeleteVPC -> TestTree
requestDeleteVPC = req
    "DeleteVPC"
    "fixture/DeleteVPC.yaml"

requestCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
requestCreateInstanceExportTask = req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask.yaml"

requestAssociateVPCCidrBlock :: AssociateVPCCidrBlock -> TestTree
requestAssociateVPCCidrBlock = req
    "AssociateVPCCidrBlock"
    "fixture/AssociateVPCCidrBlock.yaml"

requestDescribeVPCAttribute :: DescribeVPCAttribute -> TestTree
requestDescribeVPCAttribute = req
    "DescribeVPCAttribute"
    "fixture/DescribeVPCAttribute.yaml"

requestCreateVolume :: CreateVolume -> TestTree
requestCreateVolume = req
    "CreateVolume"
    "fixture/CreateVolume.yaml"

requestCreateDefaultSubnet :: CreateDefaultSubnet -> TestTree
requestCreateDefaultSubnet = req
    "CreateDefaultSubnet"
    "fixture/CreateDefaultSubnet.yaml"

requestDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailability -> TestTree
requestDescribeScheduledInstanceAvailability = req
    "DescribeScheduledInstanceAvailability"
    "fixture/DescribeScheduledInstanceAvailability.yaml"

requestModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
requestModifyVolumeAttribute = req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute.yaml"

requestDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
requestDescribeSpotDatafeedSubscription = req
    "DescribeSpotDatafeedSubscription"
    "fixture/DescribeSpotDatafeedSubscription.yaml"

requestDescribePrefixLists :: DescribePrefixLists -> TestTree
requestDescribePrefixLists = req
    "DescribePrefixLists"
    "fixture/DescribePrefixLists.yaml"

requestDeletePlacementGroup :: DeletePlacementGroup -> TestTree
requestDeletePlacementGroup = req
    "DeletePlacementGroup"
    "fixture/DeletePlacementGroup.yaml"

requestRequestSpotFleet :: RequestSpotFleet -> TestTree
requestRequestSpotFleet = req
    "RequestSpotFleet"
    "fixture/RequestSpotFleet.yaml"

requestCreateSubnet :: CreateSubnet -> TestTree
requestCreateSubnet = req
    "CreateSubnet"
    "fixture/CreateSubnet.yaml"

requestCreateNetworkInterface :: CreateNetworkInterface -> TestTree
requestCreateNetworkInterface = req
    "CreateNetworkInterface"
    "fixture/CreateNetworkInterface.yaml"

requestDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
requestDescribeSecurityGroups = req
    "DescribeSecurityGroups"
    "fixture/DescribeSecurityGroups.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks = req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestModifySpotFleetRequest :: ModifySpotFleetRequest -> TestTree
requestModifySpotFleetRequest = req
    "ModifySpotFleetRequest"
    "fixture/ModifySpotFleetRequest.yaml"

requestDetachVPNGateway :: DetachVPNGateway -> TestTree
requestDetachVPNGateway = req
    "DetachVPNGateway"
    "fixture/DetachVPNGateway.yaml"

requestGetHostReservationPurchasePreview :: GetHostReservationPurchasePreview -> TestTree
requestGetHostReservationPurchasePreview = req
    "GetHostReservationPurchasePreview"
    "fixture/GetHostReservationPurchasePreview.yaml"

requestEnableVolumeIO :: EnableVolumeIO -> TestTree
requestEnableVolumeIO = req
    "EnableVolumeIO"
    "fixture/EnableVolumeIO.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestCreateNatGateway :: CreateNatGateway -> TestTree
requestCreateNatGateway = req
    "CreateNatGateway"
    "fixture/CreateNatGateway.yaml"

requestDescribeVPCPeeringConnections :: DescribeVPCPeeringConnections -> TestTree
requestDescribeVPCPeeringConnections = req
    "DescribeVPCPeeringConnections"
    "fixture/DescribeVPCPeeringConnections.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask = req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestCreateVPCEndpointServiceConfiguration :: CreateVPCEndpointServiceConfiguration -> TestTree
requestCreateVPCEndpointServiceConfiguration = req
    "CreateVPCEndpointServiceConfiguration"
    "fixture/CreateVPCEndpointServiceConfiguration.yaml"

requestCreateDefaultVPC :: CreateDefaultVPC -> TestTree
requestCreateDefaultVPC = req
    "CreateDefaultVPC"
    "fixture/CreateDefaultVPC.yaml"

requestDisassociateVPCCidrBlock :: DisassociateVPCCidrBlock -> TestTree
requestDisassociateVPCCidrBlock = req
    "DisassociateVPCCidrBlock"
    "fixture/DisassociateVPCCidrBlock.yaml"

requestDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
requestDeleteNetworkInterface = req
    "DeleteNetworkInterface"
    "fixture/DeleteNetworkInterface.yaml"

requestReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
requestReplaceRouteTableAssociation = req
    "ReplaceRouteTableAssociation"
    "fixture/ReplaceRouteTableAssociation.yaml"

requestStartInstances :: StartInstances -> TestTree
requestStartInstances = req
    "StartInstances"
    "fixture/StartInstances.yaml"

requestCreatePlacementGroup :: CreatePlacementGroup -> TestTree
requestCreatePlacementGroup = req
    "CreatePlacementGroup"
    "fixture/CreatePlacementGroup.yaml"

requestDescribeAggregateIdFormat :: DescribeAggregateIdFormat -> TestTree
requestDescribeAggregateIdFormat = req
    "DescribeAggregateIdFormat"
    "fixture/DescribeAggregateIdFormat.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestAssociateAddress :: AssociateAddress -> TestTree
requestAssociateAddress = req
    "AssociateAddress"
    "fixture/AssociateAddress.yaml"

requestDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
requestDescribeNetworkInterfaceAttribute = req
    "DescribeNetworkInterfaceAttribute"
    "fixture/DescribeNetworkInterfaceAttribute.yaml"

requestReplaceIAMInstanceProfileAssociation :: ReplaceIAMInstanceProfileAssociation -> TestTree
requestReplaceIAMInstanceProfileAssociation = req
    "ReplaceIAMInstanceProfileAssociation"
    "fixture/ReplaceIAMInstanceProfileAssociation.yaml"

requestReleaseHosts :: ReleaseHosts -> TestTree
requestReleaseHosts = req
    "ReleaseHosts"
    "fixture/ReleaseHosts.yaml"

requestResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
requestResetNetworkInterfaceAttribute = req
    "ResetNetworkInterfaceAttribute"
    "fixture/ResetNetworkInterfaceAttribute.yaml"

requestDeleteInternetGateway :: DeleteInternetGateway -> TestTree
requestDeleteInternetGateway = req
    "DeleteInternetGateway"
    "fixture/DeleteInternetGateway.yaml"

requestDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
requestDescribeReservedInstancesListings = req
    "DescribeReservedInstancesListings"
    "fixture/DescribeReservedInstancesListings.yaml"

requestCreateVPNConnection :: CreateVPNConnection -> TestTree
requestCreateVPNConnection = req
    "CreateVPNConnection"
    "fixture/CreateVPNConnection.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet = req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestDeleteNatGateway :: DeleteNatGateway -> TestTree
requestDeleteNatGateway = req
    "DeleteNatGateway"
    "fixture/DeleteNatGateway.yaml"

requestDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
requestDescribeImportSnapshotTasks = req
    "DescribeImportSnapshotTasks"
    "fixture/DescribeImportSnapshotTasks.yaml"

requestDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
requestDescribeCustomerGateways = req
    "DescribeCustomerGateways"
    "fixture/DescribeCustomerGateways.yaml"

requestDeleteSubnet :: DeleteSubnet -> TestTree
requestDeleteSubnet = req
    "DeleteSubnet"
    "fixture/DeleteSubnet.yaml"

requestCopyImage :: CopyImage -> TestTree
requestCopyImage = req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestCreateVPCEndpoint :: CreateVPCEndpoint -> TestTree
requestCreateVPCEndpoint = req
    "CreateVPCEndpoint"
    "fixture/CreateVPCEndpoint.yaml"

requestUnmonitorInstances :: UnmonitorInstances -> TestTree
requestUnmonitorInstances = req
    "UnmonitorInstances"
    "fixture/UnmonitorInstances.yaml"

requestCreateSecurityGroup :: CreateSecurityGroup -> TestTree
requestCreateSecurityGroup = req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

requestImportVolume :: ImportVolume -> TestTree
requestImportVolume = req
    "ImportVolume"
    "fixture/ImportVolume.yaml"

requestDisableVGWRoutePropagation :: DisableVGWRoutePropagation -> TestTree
requestDisableVGWRoutePropagation = req
    "DisableVGWRoutePropagation"
    "fixture/DisableVGWRoutePropagation.yaml"

requestCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscription -> TestTree
requestCreateSpotDatafeedSubscription = req
    "CreateSpotDatafeedSubscription"
    "fixture/CreateSpotDatafeedSubscription.yaml"

requestCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
requestCancelSpotInstanceRequests = req
    "CancelSpotInstanceRequests"
    "fixture/CancelSpotInstanceRequests.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute = req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestDescribeVPCEndpointServiceConfigurations :: DescribeVPCEndpointServiceConfigurations -> TestTree
requestDescribeVPCEndpointServiceConfigurations = req
    "DescribeVPCEndpointServiceConfigurations"
    "fixture/DescribeVPCEndpointServiceConfigurations.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestAssignPrivateIPAddresses :: AssignPrivateIPAddresses -> TestTree
requestAssignPrivateIPAddresses = req
    "AssignPrivateIPAddresses"
    "fixture/AssignPrivateIPAddresses.yaml"

requestModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
requestModifyInstanceAttribute = req
    "ModifyInstanceAttribute"
    "fixture/ModifyInstanceAttribute.yaml"

requestDeleteCustomerGateway :: DeleteCustomerGateway -> TestTree
requestDeleteCustomerGateway = req
    "DeleteCustomerGateway"
    "fixture/DeleteCustomerGateway.yaml"

requestDisassociateIAMInstanceProfile :: DisassociateIAMInstanceProfile -> TestTree
requestDisassociateIAMInstanceProfile = req
    "DisassociateIAMInstanceProfile"
    "fixture/DisassociateIAMInstanceProfile.yaml"

requestDisassociateRouteTable :: DisassociateRouteTable -> TestTree
requestDisassociateRouteTable = req
    "DisassociateRouteTable"
    "fixture/DisassociateRouteTable.yaml"

requestGetConsoleScreenshot :: GetConsoleScreenshot -> TestTree
requestGetConsoleScreenshot = req
    "GetConsoleScreenshot"
    "fixture/GetConsoleScreenshot.yaml"

requestAssignIPv6Addresses :: AssignIPv6Addresses -> TestTree
requestAssignIPv6Addresses = req
    "AssignIPv6Addresses"
    "fixture/AssignIPv6Addresses.yaml"

requestDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
requestDeleteSpotDatafeedSubscription = req
    "DeleteSpotDatafeedSubscription"
    "fixture/DeleteSpotDatafeedSubscription.yaml"

requestModifyVolume :: ModifyVolume -> TestTree
requestModifyVolume = req
    "ModifyVolume"
    "fixture/ModifyVolume.yaml"

requestEnableVPCClassicLink :: EnableVPCClassicLink -> TestTree
requestEnableVPCClassicLink = req
    "EnableVPCClassicLink"
    "fixture/EnableVPCClassicLink.yaml"

requestDescribePlacementGroups :: DescribePlacementGroups -> TestTree
requestDescribePlacementGroups = req
    "DescribePlacementGroups"
    "fixture/DescribePlacementGroups.yaml"

requestDescribeStaleSecurityGroups :: DescribeStaleSecurityGroups -> TestTree
requestDescribeStaleSecurityGroups = req
    "DescribeStaleSecurityGroups"
    "fixture/DescribeStaleSecurityGroups.yaml"

requestPurchaseScheduledInstances :: PurchaseScheduledInstances -> TestTree
requestPurchaseScheduledInstances = req
    "PurchaseScheduledInstances"
    "fixture/PurchaseScheduledInstances.yaml"

requestEnableVGWRoutePropagation :: EnableVGWRoutePropagation -> TestTree
requestEnableVGWRoutePropagation = req
    "EnableVGWRoutePropagation"
    "fixture/EnableVGWRoutePropagation.yaml"

requestDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
requestDescribeSpotFleetRequestHistory = req
    "DescribeSpotFleetRequestHistory"
    "fixture/DescribeSpotFleetRequestHistory.yaml"

requestModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
requestModifySnapshotAttribute = req
    "ModifySnapshotAttribute"
    "fixture/ModifySnapshotAttribute.yaml"

requestDescribeIAMInstanceProfileAssociations :: DescribeIAMInstanceProfileAssociations -> TestTree
requestDescribeIAMInstanceProfileAssociations = req
    "DescribeIAMInstanceProfileAssociations"
    "fixture/DescribeIAMInstanceProfileAssociations.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateNetworkACLEntry :: CreateNetworkACLEntry -> TestTree
requestCreateNetworkACLEntry = req
    "CreateNetworkACLEntry"
    "fixture/CreateNetworkACLEntry.yaml"

requestCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
requestCreateReservedInstancesListing = req
    "CreateReservedInstancesListing"
    "fixture/CreateReservedInstancesListing.yaml"

requestAttachVPNGateway :: AttachVPNGateway -> TestTree
requestAttachVPNGateway = req
    "AttachVPNGateway"
    "fixture/AttachVPNGateway.yaml"

requestModifyVPCEndpointServicePermissions :: ModifyVPCEndpointServicePermissions -> TestTree
requestModifyVPCEndpointServicePermissions = req
    "ModifyVPCEndpointServicePermissions"
    "fixture/ModifyVPCEndpointServicePermissions.yaml"

requestCreateDHCPOptions :: CreateDHCPOptions -> TestTree
requestCreateDHCPOptions = req
    "CreateDHCPOptions"
    "fixture/CreateDHCPOptions.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestModifyFpgaImageAttribute :: ModifyFpgaImageAttribute -> TestTree
requestModifyFpgaImageAttribute = req
    "ModifyFpgaImageAttribute"
    "fixture/ModifyFpgaImageAttribute.yaml"

requestModifyHosts :: ModifyHosts -> TestTree
requestModifyHosts = req
    "ModifyHosts"
    "fixture/ModifyHosts.yaml"

requestRebootInstances :: RebootInstances -> TestTree
requestRebootInstances = req
    "RebootInstances"
    "fixture/RebootInstances.yaml"

requestModifyVPCEndpointServiceConfiguration :: ModifyVPCEndpointServiceConfiguration -> TestTree
requestModifyVPCEndpointServiceConfiguration = req
    "ModifyVPCEndpointServiceConfiguration"
    "fixture/ModifyVPCEndpointServiceConfiguration.yaml"

requestUnassignIPv6Addresses :: UnassignIPv6Addresses -> TestTree
requestUnassignIPv6Addresses = req
    "UnassignIPv6Addresses"
    "fixture/UnassignIPv6Addresses.yaml"

requestAssociateIAMInstanceProfile :: AssociateIAMInstanceProfile -> TestTree
requestAssociateIAMInstanceProfile = req
    "AssociateIAMInstanceProfile"
    "fixture/AssociateIAMInstanceProfile.yaml"

requestDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGateway -> TestTree
requestDeleteEgressOnlyInternetGateway = req
    "DeleteEgressOnlyInternetGateway"
    "fixture/DeleteEgressOnlyInternetGateway.yaml"

requestPurchaseHostReservation :: PurchaseHostReservation -> TestTree
requestPurchaseHostReservation = req
    "PurchaseHostReservation"
    "fixture/PurchaseHostReservation.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage = req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestDescribeClassicLinkInstances :: DescribeClassicLinkInstances -> TestTree
requestDescribeClassicLinkInstances = req
    "DescribeClassicLinkInstances"
    "fixture/DescribeClassicLinkInstances.yaml"

requestTerminateInstances :: TerminateInstances -> TestTree
requestTerminateInstances = req
    "TerminateInstances"
    "fixture/TerminateInstances.yaml"

requestDescribeKeyPairs :: DescribeKeyPairs -> TestTree
requestDescribeKeyPairs = req
    "DescribeKeyPairs"
    "fixture/DescribeKeyPairs.yaml"

requestDescribeLaunchTemplates :: DescribeLaunchTemplates -> TestTree
requestDescribeLaunchTemplates = req
    "DescribeLaunchTemplates"
    "fixture/DescribeLaunchTemplates.yaml"

requestCreateVPNConnectionRoute :: CreateVPNConnectionRoute -> TestTree
requestCreateVPNConnectionRoute = req
    "CreateVPNConnectionRoute"
    "fixture/CreateVPNConnectionRoute.yaml"

requestAssociateRouteTable :: AssociateRouteTable -> TestTree
requestAssociateRouteTable = req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable.yaml"

requestDescribeVPNGateways :: DescribeVPNGateways -> TestTree
requestDescribeVPNGateways = req
    "DescribeVPNGateways"
    "fixture/DescribeVPNGateways.yaml"

requestGetConsoleOutput :: GetConsoleOutput -> TestTree
requestGetConsoleOutput = req
    "GetConsoleOutput"
    "fixture/GetConsoleOutput.yaml"

requestDescribeHosts :: DescribeHosts -> TestTree
requestDescribeHosts = req
    "DescribeHosts"
    "fixture/DescribeHosts.yaml"

requestDescribeImageAttribute :: DescribeImageAttribute -> TestTree
requestDescribeImageAttribute = req
    "DescribeImageAttribute"
    "fixture/DescribeImageAttribute.yaml"

requestModifyIdFormat :: ModifyIdFormat -> TestTree
requestModifyIdFormat = req
    "ModifyIdFormat"
    "fixture/ModifyIdFormat.yaml"

requestDeleteRouteTable :: DeleteRouteTable -> TestTree
requestDeleteRouteTable = req
    "DeleteRouteTable"
    "fixture/DeleteRouteTable.yaml"

requestResetImageAttribute :: ResetImageAttribute -> TestTree
requestResetImageAttribute = req
    "ResetImageAttribute"
    "fixture/ResetImageAttribute.yaml"

requestCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGateway -> TestTree
requestCreateEgressOnlyInternetGateway = req
    "CreateEgressOnlyInternetGateway"
    "fixture/CreateEgressOnlyInternetGateway.yaml"

requestDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
requestDescribeReservedInstancesModifications = req
    "DescribeReservedInstancesModifications"
    "fixture/DescribeReservedInstancesModifications.yaml"

requestDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
requestDescribeSpotInstanceRequests = req
    "DescribeSpotInstanceRequests"
    "fixture/DescribeSpotInstanceRequests.yaml"

requestUnassignPrivateIPAddresses :: UnassignPrivateIPAddresses -> TestTree
requestUnassignPrivateIPAddresses = req
    "UnassignPrivateIPAddresses"
    "fixture/UnassignPrivateIPAddresses.yaml"

requestDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissions -> TestTree
requestDescribeNetworkInterfacePermissions = req
    "DescribeNetworkInterfacePermissions"
    "fixture/DescribeNetworkInterfacePermissions.yaml"

requestDescribeVPCEndpointServicePermissions :: DescribeVPCEndpointServicePermissions -> TestTree
requestDescribeVPCEndpointServicePermissions = req
    "DescribeVPCEndpointServicePermissions"
    "fixture/DescribeVPCEndpointServicePermissions.yaml"

requestDeleteDHCPOptions :: DeleteDHCPOptions -> TestTree
requestDeleteDHCPOptions = req
    "DeleteDHCPOptions"
    "fixture/DeleteDHCPOptions.yaml"

requestDescribeNetworkACLs :: DescribeNetworkACLs -> TestTree
requestDescribeNetworkACLs = req
    "DescribeNetworkACLs"
    "fixture/DescribeNetworkACLs.yaml"

requestCancelImportTask :: CancelImportTask -> TestTree
requestCancelImportTask = req
    "CancelImportTask"
    "fixture/CancelImportTask.yaml"

requestDetachClassicLinkVPC :: DetachClassicLinkVPC -> TestTree
requestDetachClassicLinkVPC = req
    "DetachClassicLinkVPC"
    "fixture/DetachClassicLinkVPC.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions = req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestMonitorInstances :: MonitorInstances -> TestTree
requestMonitorInstances = req
    "MonitorInstances"
    "fixture/MonitorInstances.yaml"

requestAcceptVPCPeeringConnection :: AcceptVPCPeeringConnection -> TestTree
requestAcceptVPCPeeringConnection = req
    "AcceptVPCPeeringConnection"
    "fixture/AcceptVPCPeeringConnection.yaml"

requestImportSnapshot :: ImportSnapshot -> TestTree
requestImportSnapshot = req
    "ImportSnapshot"
    "fixture/ImportSnapshot.yaml"

requestDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
requestDescribeVolumeStatus = req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus.yaml"

requestDescribeRouteTables :: DescribeRouteTables -> TestTree
requestDescribeRouteTables = req
    "DescribeRouteTables"
    "fixture/DescribeRouteTables.yaml"

requestDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
requestDescribeAvailabilityZones = req
    "DescribeAvailabilityZones"
    "fixture/DescribeAvailabilityZones.yaml"

requestModifyVPCAttribute :: ModifyVPCAttribute -> TestTree
requestModifyVPCAttribute = req
    "ModifyVPCAttribute"
    "fixture/ModifyVPCAttribute.yaml"

requestDescribeFleetHistory :: DescribeFleetHistory -> TestTree
requestDescribeFleetHistory = req
    "DescribeFleetHistory"
    "fixture/DescribeFleetHistory.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages = req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestDescribeElasticGpus :: DescribeElasticGpus -> TestTree
requestDescribeElasticGpus = req
    "DescribeElasticGpus"
    "fixture/DescribeElasticGpus.yaml"

requestRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
requestRestoreAddressToClassic = req
    "RestoreAddressToClassic"
    "fixture/RestoreAddressToClassic.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair = req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuote -> TestTree
requestGetReservedInstancesExchangeQuote = req
    "GetReservedInstancesExchangeQuote"
    "fixture/GetReservedInstancesExchangeQuote.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume = req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestDeleteVPCEndpointServiceConfigurations :: DeleteVPCEndpointServiceConfigurations -> TestTree
requestDeleteVPCEndpointServiceConfigurations = req
    "DeleteVPCEndpointServiceConfigurations"
    "fixture/DeleteVPCEndpointServiceConfigurations.yaml"

requestDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
requestDescribeSpotFleetInstances = req
    "DescribeSpotFleetInstances"
    "fixture/DescribeSpotFleetInstances.yaml"

-- Responses

responseImportInstance :: ImportInstanceResponse -> TestTree
responseImportInstance = res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    ec2
    (Proxy :: Proxy ImportInstance)

responseRevokeSecurityGroupEgress :: RevokeSecurityGroupEgressResponse -> TestTree
responseRevokeSecurityGroupEgress = res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse.proto"
    ec2
    (Proxy :: Proxy RevokeSecurityGroupEgress)

responseCreateNetworkInterfacePermission :: CreateNetworkInterfacePermissionResponse -> TestTree
responseCreateNetworkInterfacePermission = res
    "CreateNetworkInterfacePermissionResponse"
    "fixture/CreateNetworkInterfacePermissionResponse.proto"
    ec2
    (Proxy :: Proxy CreateNetworkInterfacePermission)

responseDeleteLaunchTemplate :: DeleteLaunchTemplateResponse -> TestTree
responseDeleteLaunchTemplate = res
    "DeleteLaunchTemplateResponse"
    "fixture/DeleteLaunchTemplateResponse.proto"
    ec2
    (Proxy :: Proxy DeleteLaunchTemplate)

responseRejectVPCEndpointConnections :: RejectVPCEndpointConnectionsResponse -> TestTree
responseRejectVPCEndpointConnections = res
    "RejectVPCEndpointConnectionsResponse"
    "fixture/RejectVPCEndpointConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy RejectVPCEndpointConnections)

responseCreateVPNGateway :: CreateVPNGatewayResponse -> TestTree
responseCreateVPNGateway = res
    "CreateVPNGatewayResponse"
    "fixture/CreateVPNGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPNGateway)

responseCreateNetworkACL :: CreateNetworkACLResponse -> TestTree
responseCreateNetworkACL = res
    "CreateNetworkACLResponse"
    "fixture/CreateNetworkACLResponse.proto"
    ec2
    (Proxy :: Proxy CreateNetworkACL)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair = res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    ec2
    (Proxy :: Proxy DeleteKeyPair)

responseDescribeSecurityGroupReferences :: DescribeSecurityGroupReferencesResponse -> TestTree
responseDescribeSecurityGroupReferences = res
    "DescribeSecurityGroupReferencesResponse"
    "fixture/DescribeSecurityGroupReferencesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSecurityGroupReferences)

responseDeleteFleets :: DeleteFleetsResponse -> TestTree
responseDeleteFleets = res
    "DeleteFleetsResponse"
    "fixture/DeleteFleetsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteFleets)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTags)

responseUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsIngress = res
    "UpdateSecurityGroupRuleDescriptionsIngressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngressResponse.proto"
    ec2
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsIngress)

responseDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlockResponse -> TestTree
responseDisassociateSubnetCidrBlock = res
    "DisassociateSubnetCidrBlockResponse"
    "fixture/DisassociateSubnetCidrBlockResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateSubnetCidrBlock)

responseDetachNetworkInterface :: DetachNetworkInterfaceResponse -> TestTree
responseDetachNetworkInterface = res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    ec2
    (Proxy :: Proxy DetachNetworkInterface)

responseDetachInternetGateway :: DetachInternetGatewayResponse -> TestTree
responseDetachInternetGateway = res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DetachInternetGateway)

responseDeleteVPCEndpoints :: DeleteVPCEndpointsResponse -> TestTree
responseDeleteVPCEndpoints = res
    "DeleteVPCEndpointsResponse"
    "fixture/DeleteVPCEndpointsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPCEndpoints)

responseDeleteFlowLogs :: DeleteFlowLogsResponse -> TestTree
responseDeleteFlowLogs = res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteFlowLogs)

responseDescribeVPCClassicLink :: DescribeVPCClassicLinkResponse -> TestTree
responseDescribeVPCClassicLink = res
    "DescribeVPCClassicLinkResponse"
    "fixture/DescribeVPCClassicLinkResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCClassicLink)

responseModifySubnetAttribute :: ModifySubnetAttributeResponse -> TestTree
responseModifySubnetAttribute = res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifySubnetAttribute)

responseDetachVolume :: VolumeAttachment -> TestTree
responseDetachVolume = res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    ec2
    (Proxy :: Proxy DetachVolume)

responseDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecificationsResponse -> TestTree
responseDescribeInstanceCreditSpecifications = res
    "DescribeInstanceCreditSpecificationsResponse"
    "fixture/DescribeInstanceCreditSpecificationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceCreditSpecifications)

responseCancelBundleTask :: CancelBundleTaskResponse -> TestTree
responseCancelBundleTask = res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse.proto"
    ec2
    (Proxy :: Proxy CancelBundleTask)

responseAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuoteResponse -> TestTree
responseAcceptReservedInstancesExchangeQuote = res
    "AcceptReservedInstancesExchangeQuoteResponse"
    "fixture/AcceptReservedInstancesExchangeQuoteResponse.proto"
    ec2
    (Proxy :: Proxy AcceptReservedInstancesExchangeQuote)

responseReleaseAddress :: ReleaseAddressResponse -> TestTree
responseReleaseAddress = res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    ec2
    (Proxy :: Proxy ReleaseAddress)

responseCreateInternetGateway :: CreateInternetGatewayResponse -> TestTree
responseCreateInternetGateway = res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateInternetGateway)

responseDeleteVPNConnection :: DeleteVPNConnectionResponse -> TestTree
responseDeleteVPNConnection = res
    "DeleteVPNConnectionResponse"
    "fixture/DeleteVPNConnectionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPNConnection)

responseDescribeBundleTasks :: DescribeBundleTasksResponse -> TestTree
responseDescribeBundleTasks = res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeBundleTasks)

responseAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgressResponse -> TestTree
responseAuthorizeSecurityGroupEgress = res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    ec2
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

responseDeregisterImage :: DeregisterImageResponse -> TestTree
responseDeregisterImage = res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse.proto"
    ec2
    (Proxy :: Proxy DeregisterImage)

responseDeleteVPCEndpointConnectionNotifications :: DeleteVPCEndpointConnectionNotificationsResponse -> TestTree
responseDeleteVPCEndpointConnectionNotifications = res
    "DeleteVPCEndpointConnectionNotificationsResponse"
    "fixture/DeleteVPCEndpointConnectionNotificationsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPCEndpointConnectionNotifications)

responseModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttributeResponse -> TestTree
responseModifyNetworkInterfaceAttribute = res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

responseModifyVPCTenancy :: ModifyVPCTenancyResponse -> TestTree
responseModifyVPCTenancy = res
    "ModifyVPCTenancyResponse"
    "fixture/ModifyVPCTenancyResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCTenancy)

responseCancelReservedInstancesListing :: CancelReservedInstancesListingResponse -> TestTree
responseCancelReservedInstancesListing = res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse.proto"
    ec2
    (Proxy :: Proxy CancelReservedInstancesListing)

responseAttachClassicLinkVPC :: AttachClassicLinkVPCResponse -> TestTree
responseAttachClassicLinkVPC = res
    "AttachClassicLinkVPCResponse"
    "fixture/AttachClassicLinkVPCResponse.proto"
    ec2
    (Proxy :: Proxy AttachClassicLinkVPC)

responseDescribeVPCClassicLinkDNSSupport :: DescribeVPCClassicLinkDNSSupportResponse -> TestTree
responseDescribeVPCClassicLinkDNSSupport = res
    "DescribeVPCClassicLinkDNSSupportResponse"
    "fixture/DescribeVPCClassicLinkDNSSupportResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCClassicLinkDNSSupport)

responseAssociateSubnetCidrBlock :: AssociateSubnetCidrBlockResponse -> TestTree
responseAssociateSubnetCidrBlock = res
    "AssociateSubnetCidrBlockResponse"
    "fixture/AssociateSubnetCidrBlockResponse.proto"
    ec2
    (Proxy :: Proxy AssociateSubnetCidrBlock)

responseRunScheduledInstances :: RunScheduledInstancesResponse -> TestTree
responseRunScheduledInstances = res
    "RunScheduledInstancesResponse"
    "fixture/RunScheduledInstancesResponse.proto"
    ec2
    (Proxy :: Proxy RunScheduledInstances)

responseCancelSpotFleetRequests :: CancelSpotFleetRequestsResponse -> TestTree
responseCancelSpotFleetRequests = res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    ec2
    (Proxy :: Proxy CancelSpotFleetRequests)

responseDescribeSpotPriceHistory :: DescribeSpotPriceHistoryResponse -> TestTree
responseDescribeSpotPriceHistory = res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotPriceHistory)

responseDescribeDHCPOptions :: DescribeDHCPOptionsResponse -> TestTree
responseDescribeDHCPOptions = res
    "DescribeDHCPOptionsResponse"
    "fixture/DescribeDHCPOptionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeDHCPOptions)

responseImportImage :: ImportImageResponse -> TestTree
responseImportImage = res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    ec2
    (Proxy :: Proxy ImportImage)

responseCopyFpgaImage :: CopyFpgaImageResponse -> TestTree
responseCopyFpgaImage = res
    "CopyFpgaImageResponse"
    "fixture/CopyFpgaImageResponse.proto"
    ec2
    (Proxy :: Proxy CopyFpgaImage)

responseStopInstances :: StopInstancesResponse -> TestTree
responseStopInstances = res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    ec2
    (Proxy :: Proxy StopInstances)

responseModifyLaunchTemplate :: ModifyLaunchTemplateResponse -> TestTree
responseModifyLaunchTemplate = res
    "ModifyLaunchTemplateResponse"
    "fixture/ModifyLaunchTemplateResponse.proto"
    ec2
    (Proxy :: Proxy ModifyLaunchTemplate)

responseModifyVPCEndpointConnectionNotification :: ModifyVPCEndpointConnectionNotificationResponse -> TestTree
responseModifyVPCEndpointConnectionNotification = res
    "ModifyVPCEndpointConnectionNotificationResponse"
    "fixture/ModifyVPCEndpointConnectionNotificationResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCEndpointConnectionNotification)

responseDescribeInternetGateways :: DescribeInternetGatewaysResponse -> TestTree
responseDescribeInternetGateways = res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInternetGateways)

responseDisableVPCClassicLink :: DisableVPCClassicLinkResponse -> TestTree
responseDisableVPCClassicLink = res
    "DisableVPCClassicLinkResponse"
    "fixture/DisableVPCClassicLinkResponse.proto"
    ec2
    (Proxy :: Proxy DisableVPCClassicLink)

responseDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> TestTree
responseDeleteLaunchTemplateVersions = res
    "DeleteLaunchTemplateVersionsResponse"
    "fixture/DeleteLaunchTemplateVersionsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteLaunchTemplateVersions)

responseBundleInstance :: BundleInstanceResponse -> TestTree
responseBundleInstance = res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    ec2
    (Proxy :: Proxy BundleInstance)

responseDescribeNetworkInterfaces :: DescribeNetworkInterfacesResponse -> TestTree
responseDescribeNetworkInterfaces = res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNetworkInterfaces)

responseReplaceNetworkACLAssociation :: ReplaceNetworkACLAssociationResponse -> TestTree
responseReplaceNetworkACLAssociation = res
    "ReplaceNetworkACLAssociationResponse"
    "fixture/ReplaceNetworkACLAssociationResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceNetworkACLAssociation)

responseDescribeNatGateways :: DescribeNatGatewaysResponse -> TestTree
responseDescribeNatGateways = res
    "DescribeNatGatewaysResponse"
    "fixture/DescribeNatGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNatGateways)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses = res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeAddresses)

responseDescribeSnapshotAttribute :: DescribeSnapshotAttributeResponse -> TestTree
responseDescribeSnapshotAttribute = res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSnapshotAttribute)

responseDescribeIdentityIdFormat :: DescribeIdentityIdFormatResponse -> TestTree
responseDescribeIdentityIdFormat = res
    "DescribeIdentityIdFormatResponse"
    "fixture/DescribeIdentityIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy DescribeIdentityIdFormat)

responseReplaceRoute :: ReplaceRouteResponse -> TestTree
responseReplaceRoute = res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceRoute)

responseDescribeVPCEndpointServices :: DescribeVPCEndpointServicesResponse -> TestTree
responseDescribeVPCEndpointServices = res
    "DescribeVPCEndpointServicesResponse"
    "fixture/DescribeVPCEndpointServicesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointServices)

responseAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngressResponse -> TestTree
responseAuthorizeSecurityGroupIngress = res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    ec2
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

responseCreateVPCPeeringConnection :: CreateVPCPeeringConnectionResponse -> TestTree
responseCreateVPCPeeringConnection = res
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPCPeeringConnection)

responseDescribeSubnets :: DescribeSubnetsResponse -> TestTree
responseDescribeSubnets = res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSubnets)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    ec2
    (Proxy :: Proxy CreateTags)

responsePurchaseReservedInstancesOffering :: PurchaseReservedInstancesOfferingResponse -> TestTree
responsePurchaseReservedInstancesOffering = res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    ec2
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

responseDeleteNetworkACLEntry :: DeleteNetworkACLEntryResponse -> TestTree
responseDeleteNetworkACLEntry = res
    "DeleteNetworkACLEntryResponse"
    "fixture/DeleteNetworkACLEntryResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNetworkACLEntry)

responseResetSnapshotAttribute :: ResetSnapshotAttributeResponse -> TestTree
responseResetSnapshotAttribute = res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetSnapshotAttribute)

responseDescribeVPNConnections :: DescribeVPNConnectionsResponse -> TestTree
responseDescribeVPNConnections = res
    "DescribeVPNConnectionsResponse"
    "fixture/DescribeVPNConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPNConnections)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute = res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    ec2
    (Proxy :: Proxy DeleteRoute)

responseReplaceNetworkACLEntry :: ReplaceNetworkACLEntryResponse -> TestTree
responseReplaceNetworkACLEntry = res
    "ReplaceNetworkACLEntryResponse"
    "fixture/ReplaceNetworkACLEntryResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceNetworkACLEntry)

responseDescribeVPCEndpoints :: DescribeVPCEndpointsResponse -> TestTree
responseDescribeVPCEndpoints = res
    "DescribeVPCEndpointsResponse"
    "fixture/DescribeVPCEndpointsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpoints)

responseResetInstanceAttribute :: ResetInstanceAttributeResponse -> TestTree
responseResetInstanceAttribute = res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetInstanceAttribute)

responseModifyIdentityIdFormat :: ModifyIdentityIdFormatResponse -> TestTree
responseModifyIdentityIdFormat = res
    "ModifyIdentityIdFormatResponse"
    "fixture/ModifyIdentityIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy ModifyIdentityIdFormat)

responseAttachNetworkInterface :: AttachNetworkInterfaceResponse -> TestTree
responseAttachNetworkInterface = res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse.proto"
    ec2
    (Proxy :: Proxy AttachNetworkInterface)

responseDescribeInstanceStatus :: DescribeInstanceStatusResponse -> TestTree
responseDescribeInstanceStatus = res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceStatus)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair = res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    ec2
    (Proxy :: Proxy ImportKeyPair)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTags)

responseConfirmProductInstance :: ConfirmProductInstanceResponse -> TestTree
responseConfirmProductInstance = res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    ec2
    (Proxy :: Proxy ConfirmProductInstance)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute = res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceAttribute)

responseDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferingsResponse -> TestTree
responseDescribeReservedInstancesOfferings = res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

responseCreateCustomerGateway :: CreateCustomerGatewayResponse -> TestTree
responseCreateCustomerGateway = res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateCustomerGateway)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets = res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFleets)

responseDeleteSecurityGroup :: DeleteSecurityGroupResponse -> TestTree
responseDeleteSecurityGroup = res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    ec2
    (Proxy :: Proxy DeleteSecurityGroup)

responseDeleteVPCPeeringConnection :: DeleteVPCPeeringConnectionResponse -> TestTree
responseDeleteVPCPeeringConnection = res
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPCPeeringConnection)

responseAttachInternetGateway :: AttachInternetGatewayResponse -> TestTree
responseAttachInternetGateway = res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy AttachInternetGateway)

responseModifyInstancePlacement :: ModifyInstancePlacementResponse -> TestTree
responseModifyInstancePlacement = res
    "ModifyInstancePlacementResponse"
    "fixture/ModifyInstancePlacementResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstancePlacement)

responseDescribeFlowLogs :: DescribeFlowLogsResponse -> TestTree
responseDescribeFlowLogs = res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFlowLogs)

responseDescribeVPCEndpointConnectionNotifications :: DescribeVPCEndpointConnectionNotificationsResponse -> TestTree
responseDescribeVPCEndpointConnectionNotifications = res
    "DescribeVPCEndpointConnectionNotificationsResponse"
    "fixture/DescribeVPCEndpointConnectionNotificationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointConnectionNotifications)

responseRunInstances :: Reservation -> TestTree
responseRunInstances = res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    ec2
    (Proxy :: Proxy RunInstances)

responseAssociateDHCPOptions :: AssociateDHCPOptionsResponse -> TestTree
responseAssociateDHCPOptions = res
    "AssociateDHCPOptionsResponse"
    "fixture/AssociateDHCPOptionsResponse.proto"
    ec2
    (Proxy :: Proxy AssociateDHCPOptions)

responseDescribeReservedInstances :: DescribeReservedInstancesResponse -> TestTree
responseDescribeReservedInstances = res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeReservedInstances)

responseDescribeIdFormat :: DescribeIdFormatResponse -> TestTree
responseDescribeIdFormat = res
    "DescribeIdFormatResponse"
    "fixture/DescribeIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy DescribeIdFormat)

responseDescribeVPCs :: DescribeVPCsResponse -> TestTree
responseDescribeVPCs = res
    "DescribeVPCsResponse"
    "fixture/DescribeVPCsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCs)

responseDescribeConversionTasks :: DescribeConversionTasksResponse -> TestTree
responseDescribeConversionTasks = res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeConversionTasks)

responseCreateLaunchTemplateVersion :: CreateLaunchTemplateVersionResponse -> TestTree
responseCreateLaunchTemplateVersion = res
    "CreateLaunchTemplateVersionResponse"
    "fixture/CreateLaunchTemplateVersionResponse.proto"
    ec2
    (Proxy :: Proxy CreateLaunchTemplateVersion)

responseDisableVPCClassicLinkDNSSupport :: DisableVPCClassicLinkDNSSupportResponse -> TestTree
responseDisableVPCClassicLinkDNSSupport = res
    "DisableVPCClassicLinkDNSSupportResponse"
    "fixture/DisableVPCClassicLinkDNSSupportResponse.proto"
    ec2
    (Proxy :: Proxy DisableVPCClassicLinkDNSSupport)

responseDescribeVolumesModifications :: DescribeVolumesModificationsResponse -> TestTree
responseDescribeVolumesModifications = res
    "DescribeVolumesModificationsResponse"
    "fixture/DescribeVolumesModificationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVolumesModifications)

responseCreateFpgaImage :: CreateFpgaImageResponse -> TestTree
responseCreateFpgaImage = res
    "CreateFpgaImageResponse"
    "fixture/CreateFpgaImageResponse.proto"
    ec2
    (Proxy :: Proxy CreateFpgaImage)

responseAcceptVPCEndpointConnections :: AcceptVPCEndpointConnectionsResponse -> TestTree
responseAcceptVPCEndpointConnections = res
    "AcceptVPCEndpointConnectionsResponse"
    "fixture/AcceptVPCEndpointConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy AcceptVPCEndpointConnections)

responseGetLaunchTemplateData :: GetLaunchTemplateDataResponse -> TestTree
responseGetLaunchTemplateData = res
    "GetLaunchTemplateDataResponse"
    "fixture/GetLaunchTemplateDataResponse.proto"
    ec2
    (Proxy :: Proxy GetLaunchTemplateData)

responseAllocateAddress :: AllocateAddressResponse -> TestTree
responseAllocateAddress = res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    ec2
    (Proxy :: Proxy AllocateAddress)

responseCancelConversionTask :: CancelConversionTaskResponse -> TestTree
responseCancelConversionTask = res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse.proto"
    ec2
    (Proxy :: Proxy CancelConversionTask)

responseModifyImageAttribute :: ModifyImageAttributeResponse -> TestTree
responseModifyImageAttribute = res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyImageAttribute)

responseCreateRouteTable :: CreateRouteTableResponse -> TestTree
responseCreateRouteTable = res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy CreateRouteTable)

responseReportInstanceStatus :: ReportInstanceStatusResponse -> TestTree
responseReportInstanceStatus = res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    ec2
    (Proxy :: Proxy ReportInstanceStatus)

responseAttachVolume :: VolumeAttachment -> TestTree
responseAttachVolume = res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    ec2
    (Proxy :: Proxy AttachVolume)

responseRequestSpotInstances :: RequestSpotInstancesResponse -> TestTree
responseRequestSpotInstances = res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    ec2
    (Proxy :: Proxy RequestSpotInstances)

responseDescribeHostReservationOfferings :: DescribeHostReservationOfferingsResponse -> TestTree
responseDescribeHostReservationOfferings = res
    "DescribeHostReservationOfferingsResponse"
    "fixture/DescribeHostReservationOfferingsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeHostReservationOfferings)

responseResetFpgaImageAttribute :: ResetFpgaImageAttributeResponse -> TestTree
responseResetFpgaImageAttribute = res
    "ResetFpgaImageAttributeResponse"
    "fixture/ResetFpgaImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetFpgaImageAttribute)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes = res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVolumes)

responseRejectVPCPeeringConnection :: RejectVPCPeeringConnectionResponse -> TestTree
responseRejectVPCPeeringConnection = res
    "RejectVPCPeeringConnectionResponse"
    "fixture/RejectVPCPeeringConnectionResponse.proto"
    ec2
    (Proxy :: Proxy RejectVPCPeeringConnection)

responseDeleteVPNConnectionRoute :: DeleteVPNConnectionRouteResponse -> TestTree
responseDeleteVPNConnectionRoute = res
    "DeleteVPNConnectionRouteResponse"
    "fixture/DeleteVPNConnectionRouteResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPNConnectionRoute)

responseModifyVPCEndpoint :: ModifyVPCEndpointResponse -> TestTree
responseModifyVPCEndpoint = res
    "ModifyVPCEndpointResponse"
    "fixture/ModifyVPCEndpointResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCEndpoint)

responseDescribeFpgaImageAttribute :: DescribeFpgaImageAttributeResponse -> TestTree
responseDescribeFpgaImageAttribute = res
    "DescribeFpgaImageAttributeResponse"
    "fixture/DescribeFpgaImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFpgaImageAttribute)

responseAllocateHosts :: AllocateHostsResponse -> TestTree
responseAllocateHosts = res
    "AllocateHostsResponse"
    "fixture/AllocateHostsResponse.proto"
    ec2
    (Proxy :: Proxy AllocateHosts)

responseRegisterImage :: RegisterImageResponse -> TestTree
responseRegisterImage = res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    ec2
    (Proxy :: Proxy RegisterImage)

responseModifyFleet :: ModifyFleetResponse -> TestTree
responseModifyFleet = res
    "ModifyFleetResponse"
    "fixture/ModifyFleetResponse.proto"
    ec2
    (Proxy :: Proxy ModifyFleet)

responseRevokeSecurityGroupIngress :: RevokeSecurityGroupIngressResponse -> TestTree
responseRevokeSecurityGroupIngress = res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    ec2
    (Proxy :: Proxy RevokeSecurityGroupIngress)

responseDescribeHostReservations :: DescribeHostReservationsResponse -> TestTree
responseDescribeHostReservations = res
    "DescribeHostReservationsResponse"
    "fixture/DescribeHostReservationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeHostReservations)

responseUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsEgress = res
    "UpdateSecurityGroupRuleDescriptionsEgressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgressResponse.proto"
    ec2
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsEgress)

responseEnableVPCClassicLinkDNSSupport :: EnableVPCClassicLinkDNSSupportResponse -> TestTree
responseEnableVPCClassicLinkDNSSupport = res
    "EnableVPCClassicLinkDNSSupportResponse"
    "fixture/EnableVPCClassicLinkDNSSupportResponse.proto"
    ec2
    (Proxy :: Proxy EnableVPCClassicLinkDNSSupport)

responseDescribeVPCEndpointConnections :: DescribeVPCEndpointConnectionsResponse -> TestTree
responseDescribeVPCEndpointConnections = res
    "DescribeVPCEndpointConnectionsResponse"
    "fixture/DescribeVPCEndpointConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointConnections)

responseModifyReservedInstances :: ModifyReservedInstancesResponse -> TestTree
responseModifyReservedInstances = res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    ec2
    (Proxy :: Proxy ModifyReservedInstances)

responseDeleteFpgaImage :: DeleteFpgaImageResponse -> TestTree
responseDeleteFpgaImage = res
    "DeleteFpgaImageResponse"
    "fixture/DeleteFpgaImageResponse.proto"
    ec2
    (Proxy :: Proxy DeleteFpgaImage)

responseDescribeScheduledInstances :: DescribeScheduledInstancesResponse -> TestTree
responseDescribeScheduledInstances = res
    "DescribeScheduledInstancesResponse"
    "fixture/DescribeScheduledInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeScheduledInstances)

responseCreateFlowLogs :: CreateFlowLogsResponse -> TestTree
responseCreateFlowLogs = res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    ec2
    (Proxy :: Proxy CreateFlowLogs)

responseDescribeSpotFleetRequests :: DescribeSpotFleetRequestsResponse -> TestTree
responseDescribeSpotFleetRequests = res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotFleetRequests)

responseMoveAddressToVPC :: MoveAddressToVPCResponse -> TestTree
responseMoveAddressToVPC = res
    "MoveAddressToVPCResponse"
    "fixture/MoveAddressToVPCResponse.proto"
    ec2
    (Proxy :: Proxy MoveAddressToVPC)

responseDescribeFleetInstances :: DescribeFleetInstancesResponse -> TestTree
responseDescribeFleetInstances = res
    "DescribeFleetInstancesResponse"
    "fixture/DescribeFleetInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFleetInstances)

responseDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersionsResponse -> TestTree
responseDescribeLaunchTemplateVersions = res
    "DescribeLaunchTemplateVersionsResponse"
    "fixture/DescribeLaunchTemplateVersionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLaunchTemplateVersions)

responseModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecificationResponse -> TestTree
responseModifyInstanceCreditSpecification = res
    "ModifyInstanceCreditSpecificationResponse"
    "fixture/ModifyInstanceCreditSpecificationResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstanceCreditSpecification)

responseDescribePrincipalIdFormat :: DescribePrincipalIdFormatResponse -> TestTree
responseDescribePrincipalIdFormat = res
    "DescribePrincipalIdFormatResponse"
    "fixture/DescribePrincipalIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy DescribePrincipalIdFormat)

responseDeleteNetworkACL :: DeleteNetworkACLResponse -> TestTree
responseDeleteNetworkACL = res
    "DeleteNetworkACLResponse"
    "fixture/DeleteNetworkACLResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNetworkACL)

responseCreateLaunchTemplate :: CreateLaunchTemplateResponse -> TestTree
responseCreateLaunchTemplate = res
    "CreateLaunchTemplateResponse"
    "fixture/CreateLaunchTemplateResponse.proto"
    ec2
    (Proxy :: Proxy CreateLaunchTemplate)

responseCreateVPCEndpointConnectionNotification :: CreateVPCEndpointConnectionNotificationResponse -> TestTree
responseCreateVPCEndpointConnectionNotification = res
    "CreateVPCEndpointConnectionNotificationResponse"
    "fixture/CreateVPCEndpointConnectionNotificationResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPCEndpointConnectionNotification)

responseDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermissionResponse -> TestTree
responseDeleteNetworkInterfacePermission = res
    "DeleteNetworkInterfacePermissionResponse"
    "fixture/DeleteNetworkInterfacePermissionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNetworkInterfacePermission)

responseDeleteVPNGateway :: DeleteVPNGatewayResponse -> TestTree
responseDeleteVPNGateway = res
    "DeleteVPNGatewayResponse"
    "fixture/DeleteVPNGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPNGateway)

responseDescribeImportImageTasks :: DescribeImportImageTasksResponse -> TestTree
responseDescribeImportImageTasks = res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeImportImageTasks)

responseDescribeVolumeAttribute :: DescribeVolumeAttributeResponse -> TestTree
responseDescribeVolumeAttribute = res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVolumeAttribute)

responseDescribeMovingAddresses :: DescribeMovingAddressesResponse -> TestTree
responseDescribeMovingAddresses = res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeMovingAddresses)

responseGetPasswordData :: GetPasswordDataResponse -> TestTree
responseGetPasswordData = res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse.proto"
    ec2
    (Proxy :: Proxy GetPasswordData)

responseCreateVPC :: CreateVPCResponse -> TestTree
responseCreateVPC = res
    "CreateVPCResponse"
    "fixture/CreateVPCResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPC)

responseModifyVPCPeeringConnectionOptions :: ModifyVPCPeeringConnectionOptionsResponse -> TestTree
responseModifyVPCPeeringConnectionOptions = res
    "ModifyVPCPeeringConnectionOptionsResponse"
    "fixture/ModifyVPCPeeringConnectionOptionsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCPeeringConnectionOptions)

responseDescribeFpgaImages :: DescribeFpgaImagesResponse -> TestTree
responseDescribeFpgaImages = res
    "DescribeFpgaImagesResponse"
    "fixture/DescribeFpgaImagesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFpgaImages)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    ec2
    (Proxy :: Proxy CopySnapshot)

responseDisassociateAddress :: DisassociateAddressResponse -> TestTree
responseDisassociateAddress = res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateAddress)

responseDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGatewaysResponse -> TestTree
responseDescribeEgressOnlyInternetGateways = res
    "DescribeEgressOnlyInternetGatewaysResponse"
    "fixture/DescribeEgressOnlyInternetGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeEgressOnlyInternetGateways)

responseDeleteVPC :: DeleteVPCResponse -> TestTree
responseDeleteVPC = res
    "DeleteVPCResponse"
    "fixture/DeleteVPCResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPC)

responseCreateInstanceExportTask :: CreateInstanceExportTaskResponse -> TestTree
responseCreateInstanceExportTask = res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    ec2
    (Proxy :: Proxy CreateInstanceExportTask)

responseAssociateVPCCidrBlock :: AssociateVPCCidrBlockResponse -> TestTree
responseAssociateVPCCidrBlock = res
    "AssociateVPCCidrBlockResponse"
    "fixture/AssociateVPCCidrBlockResponse.proto"
    ec2
    (Proxy :: Proxy AssociateVPCCidrBlock)

responseDescribeVPCAttribute :: DescribeVPCAttributeResponse -> TestTree
responseDescribeVPCAttribute = res
    "DescribeVPCAttributeResponse"
    "fixture/DescribeVPCAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCAttribute)

responseCreateVolume :: Volume -> TestTree
responseCreateVolume = res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    ec2
    (Proxy :: Proxy CreateVolume)

responseCreateDefaultSubnet :: CreateDefaultSubnetResponse -> TestTree
responseCreateDefaultSubnet = res
    "CreateDefaultSubnetResponse"
    "fixture/CreateDefaultSubnetResponse.proto"
    ec2
    (Proxy :: Proxy CreateDefaultSubnet)

responseDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailabilityResponse -> TestTree
responseDescribeScheduledInstanceAvailability = res
    "DescribeScheduledInstanceAvailabilityResponse"
    "fixture/DescribeScheduledInstanceAvailabilityResponse.proto"
    ec2
    (Proxy :: Proxy DescribeScheduledInstanceAvailability)

responseModifyVolumeAttribute :: ModifyVolumeAttributeResponse -> TestTree
responseModifyVolumeAttribute = res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVolumeAttribute)

responseDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
responseDescribeSpotDatafeedSubscription = res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

responseDescribePrefixLists :: DescribePrefixListsResponse -> TestTree
responseDescribePrefixLists = res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    ec2
    (Proxy :: Proxy DescribePrefixLists)

responseDeletePlacementGroup :: DeletePlacementGroupResponse -> TestTree
responseDeletePlacementGroup = res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    ec2
    (Proxy :: Proxy DeletePlacementGroup)

responseRequestSpotFleet :: RequestSpotFleetResponse -> TestTree
responseRequestSpotFleet = res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    ec2
    (Proxy :: Proxy RequestSpotFleet)

responseCreateSubnet :: CreateSubnetResponse -> TestTree
responseCreateSubnet = res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    ec2
    (Proxy :: Proxy CreateSubnet)

responseCreateNetworkInterface :: CreateNetworkInterfaceResponse -> TestTree
responseCreateNetworkInterface = res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse.proto"
    ec2
    (Proxy :: Proxy CreateNetworkInterface)

responseDescribeSecurityGroups :: DescribeSecurityGroupsResponse -> TestTree
responseDescribeSecurityGroups = res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSecurityGroups)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks = res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeExportTasks)

responseModifySpotFleetRequest :: ModifySpotFleetRequestResponse -> TestTree
responseModifySpotFleetRequest = res
    "ModifySpotFleetRequestResponse"
    "fixture/ModifySpotFleetRequestResponse.proto"
    ec2
    (Proxy :: Proxy ModifySpotFleetRequest)

responseDetachVPNGateway :: DetachVPNGatewayResponse -> TestTree
responseDetachVPNGateway = res
    "DetachVPNGatewayResponse"
    "fixture/DetachVPNGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DetachVPNGateway)

responseGetHostReservationPurchasePreview :: GetHostReservationPurchasePreviewResponse -> TestTree
responseGetHostReservationPurchasePreview = res
    "GetHostReservationPurchasePreviewResponse"
    "fixture/GetHostReservationPurchasePreviewResponse.proto"
    ec2
    (Proxy :: Proxy GetHostReservationPurchasePreview)

responseEnableVolumeIO :: EnableVolumeIOResponse -> TestTree
responseEnableVolumeIO = res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    ec2
    (Proxy :: Proxy EnableVolumeIO)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstances)

responseCreateNatGateway :: CreateNatGatewayResponse -> TestTree
responseCreateNatGateway = res
    "CreateNatGatewayResponse"
    "fixture/CreateNatGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateNatGateway)

responseDescribeVPCPeeringConnections :: DescribeVPCPeeringConnectionsResponse -> TestTree
responseDescribeVPCPeeringConnections = res
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCPeeringConnections)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask = res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    ec2
    (Proxy :: Proxy CancelExportTask)

responseCreateVPCEndpointServiceConfiguration :: CreateVPCEndpointServiceConfigurationResponse -> TestTree
responseCreateVPCEndpointServiceConfiguration = res
    "CreateVPCEndpointServiceConfigurationResponse"
    "fixture/CreateVPCEndpointServiceConfigurationResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPCEndpointServiceConfiguration)

responseCreateDefaultVPC :: CreateDefaultVPCResponse -> TestTree
responseCreateDefaultVPC = res
    "CreateDefaultVPCResponse"
    "fixture/CreateDefaultVPCResponse.proto"
    ec2
    (Proxy :: Proxy CreateDefaultVPC)

responseDisassociateVPCCidrBlock :: DisassociateVPCCidrBlockResponse -> TestTree
responseDisassociateVPCCidrBlock = res
    "DisassociateVPCCidrBlockResponse"
    "fixture/DisassociateVPCCidrBlockResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateVPCCidrBlock)

responseDeleteNetworkInterface :: DeleteNetworkInterfaceResponse -> TestTree
responseDeleteNetworkInterface = res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNetworkInterface)

responseReplaceRouteTableAssociation :: ReplaceRouteTableAssociationResponse -> TestTree
responseReplaceRouteTableAssociation = res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceRouteTableAssociation)

responseStartInstances :: StartInstancesResponse -> TestTree
responseStartInstances = res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    ec2
    (Proxy :: Proxy StartInstances)

responseCreatePlacementGroup :: CreatePlacementGroupResponse -> TestTree
responseCreatePlacementGroup = res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    ec2
    (Proxy :: Proxy CreatePlacementGroup)

responseDescribeAggregateIdFormat :: DescribeAggregateIdFormatResponse -> TestTree
responseDescribeAggregateIdFormat = res
    "DescribeAggregateIdFormatResponse"
    "fixture/DescribeAggregateIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy DescribeAggregateIdFormat)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSnapshots)

responseAssociateAddress :: AssociateAddressResponse -> TestTree
responseAssociateAddress = res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    ec2
    (Proxy :: Proxy AssociateAddress)

responseDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttributeResponse -> TestTree
responseDescribeNetworkInterfaceAttribute = res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

responseReplaceIAMInstanceProfileAssociation :: ReplaceIAMInstanceProfileAssociationResponse -> TestTree
responseReplaceIAMInstanceProfileAssociation = res
    "ReplaceIAMInstanceProfileAssociationResponse"
    "fixture/ReplaceIAMInstanceProfileAssociationResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceIAMInstanceProfileAssociation)

responseReleaseHosts :: ReleaseHostsResponse -> TestTree
responseReleaseHosts = res
    "ReleaseHostsResponse"
    "fixture/ReleaseHostsResponse.proto"
    ec2
    (Proxy :: Proxy ReleaseHosts)

responseResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttributeResponse -> TestTree
responseResetNetworkInterfaceAttribute = res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

responseDeleteInternetGateway :: DeleteInternetGatewayResponse -> TestTree
responseDeleteInternetGateway = res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteInternetGateway)

responseDescribeReservedInstancesListings :: DescribeReservedInstancesListingsResponse -> TestTree
responseDescribeReservedInstancesListings = res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeReservedInstancesListings)

responseCreateVPNConnection :: CreateVPNConnectionResponse -> TestTree
responseCreateVPNConnection = res
    "CreateVPNConnectionResponse"
    "fixture/CreateVPNConnectionResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPNConnection)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet = res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    ec2
    (Proxy :: Proxy CreateFleet)

responseDeleteNatGateway :: DeleteNatGatewayResponse -> TestTree
responseDeleteNatGateway = res
    "DeleteNatGatewayResponse"
    "fixture/DeleteNatGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNatGateway)

responseDescribeImportSnapshotTasks :: DescribeImportSnapshotTasksResponse -> TestTree
responseDescribeImportSnapshotTasks = res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeImportSnapshotTasks)

responseDescribeCustomerGateways :: DescribeCustomerGatewaysResponse -> TestTree
responseDescribeCustomerGateways = res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeCustomerGateways)

responseDeleteSubnet :: DeleteSubnetResponse -> TestTree
responseDeleteSubnet = res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    ec2
    (Proxy :: Proxy DeleteSubnet)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage = res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    ec2
    (Proxy :: Proxy CopyImage)

responseCreateVPCEndpoint :: CreateVPCEndpointResponse -> TestTree
responseCreateVPCEndpoint = res
    "CreateVPCEndpointResponse"
    "fixture/CreateVPCEndpointResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPCEndpoint)

responseUnmonitorInstances :: UnmonitorInstancesResponse -> TestTree
responseUnmonitorInstances = res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    ec2
    (Proxy :: Proxy UnmonitorInstances)

responseCreateSecurityGroup :: CreateSecurityGroupResponse -> TestTree
responseCreateSecurityGroup = res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    ec2
    (Proxy :: Proxy CreateSecurityGroup)

responseImportVolume :: ImportVolumeResponse -> TestTree
responseImportVolume = res
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse.proto"
    ec2
    (Proxy :: Proxy ImportVolume)

responseDisableVGWRoutePropagation :: DisableVGWRoutePropagationResponse -> TestTree
responseDisableVGWRoutePropagation = res
    "DisableVGWRoutePropagationResponse"
    "fixture/DisableVGWRoutePropagationResponse.proto"
    ec2
    (Proxy :: Proxy DisableVGWRoutePropagation)

responseCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscriptionResponse -> TestTree
responseCreateSpotDatafeedSubscription = res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse.proto"
    ec2
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

responseCancelSpotInstanceRequests :: CancelSpotInstanceRequestsResponse -> TestTree
responseCancelSpotInstanceRequests = res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse.proto"
    ec2
    (Proxy :: Proxy CancelSpotInstanceRequests)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute = res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    ec2
    (Proxy :: Proxy CreateRoute)

responseDescribeVPCEndpointServiceConfigurations :: DescribeVPCEndpointServiceConfigurationsResponse -> TestTree
responseDescribeVPCEndpointServiceConfigurations = res
    "DescribeVPCEndpointServiceConfigurationsResponse"
    "fixture/DescribeVPCEndpointServiceConfigurationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointServiceConfigurations)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    ec2
    (Proxy :: Proxy DeleteSnapshot)

responseAssignPrivateIPAddresses :: AssignPrivateIPAddressesResponse -> TestTree
responseAssignPrivateIPAddresses = res
    "AssignPrivateIPAddressesResponse"
    "fixture/AssignPrivateIPAddressesResponse.proto"
    ec2
    (Proxy :: Proxy AssignPrivateIPAddresses)

responseModifyInstanceAttribute :: ModifyInstanceAttributeResponse -> TestTree
responseModifyInstanceAttribute = res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstanceAttribute)

responseDeleteCustomerGateway :: DeleteCustomerGatewayResponse -> TestTree
responseDeleteCustomerGateway = res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteCustomerGateway)

responseDisassociateIAMInstanceProfile :: DisassociateIAMInstanceProfileResponse -> TestTree
responseDisassociateIAMInstanceProfile = res
    "DisassociateIAMInstanceProfileResponse"
    "fixture/DisassociateIAMInstanceProfileResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateIAMInstanceProfile)

responseDisassociateRouteTable :: DisassociateRouteTableResponse -> TestTree
responseDisassociateRouteTable = res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateRouteTable)

responseGetConsoleScreenshot :: GetConsoleScreenshotResponse -> TestTree
responseGetConsoleScreenshot = res
    "GetConsoleScreenshotResponse"
    "fixture/GetConsoleScreenshotResponse.proto"
    ec2
    (Proxy :: Proxy GetConsoleScreenshot)

responseAssignIPv6Addresses :: AssignIPv6AddressesResponse -> TestTree
responseAssignIPv6Addresses = res
    "AssignIPv6AddressesResponse"
    "fixture/AssignIPv6AddressesResponse.proto"
    ec2
    (Proxy :: Proxy AssignIPv6Addresses)

responseDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
responseDeleteSpotDatafeedSubscription = res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

responseModifyVolume :: ModifyVolumeResponse -> TestTree
responseModifyVolume = res
    "ModifyVolumeResponse"
    "fixture/ModifyVolumeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVolume)

responseEnableVPCClassicLink :: EnableVPCClassicLinkResponse -> TestTree
responseEnableVPCClassicLink = res
    "EnableVPCClassicLinkResponse"
    "fixture/EnableVPCClassicLinkResponse.proto"
    ec2
    (Proxy :: Proxy EnableVPCClassicLink)

responseDescribePlacementGroups :: DescribePlacementGroupsResponse -> TestTree
responseDescribePlacementGroups = res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    ec2
    (Proxy :: Proxy DescribePlacementGroups)

responseDescribeStaleSecurityGroups :: DescribeStaleSecurityGroupsResponse -> TestTree
responseDescribeStaleSecurityGroups = res
    "DescribeStaleSecurityGroupsResponse"
    "fixture/DescribeStaleSecurityGroupsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeStaleSecurityGroups)

responsePurchaseScheduledInstances :: PurchaseScheduledInstancesResponse -> TestTree
responsePurchaseScheduledInstances = res
    "PurchaseScheduledInstancesResponse"
    "fixture/PurchaseScheduledInstancesResponse.proto"
    ec2
    (Proxy :: Proxy PurchaseScheduledInstances)

responseEnableVGWRoutePropagation :: EnableVGWRoutePropagationResponse -> TestTree
responseEnableVGWRoutePropagation = res
    "EnableVGWRoutePropagationResponse"
    "fixture/EnableVGWRoutePropagationResponse.proto"
    ec2
    (Proxy :: Proxy EnableVGWRoutePropagation)

responseDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistoryResponse -> TestTree
responseDescribeSpotFleetRequestHistory = res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

responseModifySnapshotAttribute :: ModifySnapshotAttributeResponse -> TestTree
responseModifySnapshotAttribute = res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifySnapshotAttribute)

responseDescribeIAMInstanceProfileAssociations :: DescribeIAMInstanceProfileAssociationsResponse -> TestTree
responseDescribeIAMInstanceProfileAssociations = res
    "DescribeIAMInstanceProfileAssociationsResponse"
    "fixture/DescribeIAMInstanceProfileAssociationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeIAMInstanceProfileAssociations)

responseCreateSnapshot :: Snapshot -> TestTree
responseCreateSnapshot = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    ec2
    (Proxy :: Proxy CreateSnapshot)

responseCreateNetworkACLEntry :: CreateNetworkACLEntryResponse -> TestTree
responseCreateNetworkACLEntry = res
    "CreateNetworkACLEntryResponse"
    "fixture/CreateNetworkACLEntryResponse.proto"
    ec2
    (Proxy :: Proxy CreateNetworkACLEntry)

responseCreateReservedInstancesListing :: CreateReservedInstancesListingResponse -> TestTree
responseCreateReservedInstancesListing = res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    ec2
    (Proxy :: Proxy CreateReservedInstancesListing)

responseAttachVPNGateway :: AttachVPNGatewayResponse -> TestTree
responseAttachVPNGateway = res
    "AttachVPNGatewayResponse"
    "fixture/AttachVPNGatewayResponse.proto"
    ec2
    (Proxy :: Proxy AttachVPNGateway)

responseModifyVPCEndpointServicePermissions :: ModifyVPCEndpointServicePermissionsResponse -> TestTree
responseModifyVPCEndpointServicePermissions = res
    "ModifyVPCEndpointServicePermissionsResponse"
    "fixture/ModifyVPCEndpointServicePermissionsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCEndpointServicePermissions)

responseCreateDHCPOptions :: CreateDHCPOptionsResponse -> TestTree
responseCreateDHCPOptions = res
    "CreateDHCPOptionsResponse"
    "fixture/CreateDHCPOptionsResponse.proto"
    ec2
    (Proxy :: Proxy CreateDHCPOptions)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeAccountAttributes)

responseModifyFpgaImageAttribute :: ModifyFpgaImageAttributeResponse -> TestTree
responseModifyFpgaImageAttribute = res
    "ModifyFpgaImageAttributeResponse"
    "fixture/ModifyFpgaImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyFpgaImageAttribute)

responseModifyHosts :: ModifyHostsResponse -> TestTree
responseModifyHosts = res
    "ModifyHostsResponse"
    "fixture/ModifyHostsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyHosts)

responseRebootInstances :: RebootInstancesResponse -> TestTree
responseRebootInstances = res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    ec2
    (Proxy :: Proxy RebootInstances)

responseModifyVPCEndpointServiceConfiguration :: ModifyVPCEndpointServiceConfigurationResponse -> TestTree
responseModifyVPCEndpointServiceConfiguration = res
    "ModifyVPCEndpointServiceConfigurationResponse"
    "fixture/ModifyVPCEndpointServiceConfigurationResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCEndpointServiceConfiguration)

responseUnassignIPv6Addresses :: UnassignIPv6AddressesResponse -> TestTree
responseUnassignIPv6Addresses = res
    "UnassignIPv6AddressesResponse"
    "fixture/UnassignIPv6AddressesResponse.proto"
    ec2
    (Proxy :: Proxy UnassignIPv6Addresses)

responseAssociateIAMInstanceProfile :: AssociateIAMInstanceProfileResponse -> TestTree
responseAssociateIAMInstanceProfile = res
    "AssociateIAMInstanceProfileResponse"
    "fixture/AssociateIAMInstanceProfileResponse.proto"
    ec2
    (Proxy :: Proxy AssociateIAMInstanceProfile)

responseDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGatewayResponse -> TestTree
responseDeleteEgressOnlyInternetGateway = res
    "DeleteEgressOnlyInternetGatewayResponse"
    "fixture/DeleteEgressOnlyInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteEgressOnlyInternetGateway)

responsePurchaseHostReservation :: PurchaseHostReservationResponse -> TestTree
responsePurchaseHostReservation = res
    "PurchaseHostReservationResponse"
    "fixture/PurchaseHostReservationResponse.proto"
    ec2
    (Proxy :: Proxy PurchaseHostReservation)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage = res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    ec2
    (Proxy :: Proxy CreateImage)

responseDescribeClassicLinkInstances :: DescribeClassicLinkInstancesResponse -> TestTree
responseDescribeClassicLinkInstances = res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeClassicLinkInstances)

responseTerminateInstances :: TerminateInstancesResponse -> TestTree
responseTerminateInstances = res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse.proto"
    ec2
    (Proxy :: Proxy TerminateInstances)

responseDescribeKeyPairs :: DescribeKeyPairsResponse -> TestTree
responseDescribeKeyPairs = res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeKeyPairs)

responseDescribeLaunchTemplates :: DescribeLaunchTemplatesResponse -> TestTree
responseDescribeLaunchTemplates = res
    "DescribeLaunchTemplatesResponse"
    "fixture/DescribeLaunchTemplatesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLaunchTemplates)

responseCreateVPNConnectionRoute :: CreateVPNConnectionRouteResponse -> TestTree
responseCreateVPNConnectionRoute = res
    "CreateVPNConnectionRouteResponse"
    "fixture/CreateVPNConnectionRouteResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPNConnectionRoute)

responseAssociateRouteTable :: AssociateRouteTableResponse -> TestTree
responseAssociateRouteTable = res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy AssociateRouteTable)

responseDescribeVPNGateways :: DescribeVPNGatewaysResponse -> TestTree
responseDescribeVPNGateways = res
    "DescribeVPNGatewaysResponse"
    "fixture/DescribeVPNGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPNGateways)

responseGetConsoleOutput :: GetConsoleOutputResponse -> TestTree
responseGetConsoleOutput = res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    ec2
    (Proxy :: Proxy GetConsoleOutput)

responseDescribeHosts :: DescribeHostsResponse -> TestTree
responseDescribeHosts = res
    "DescribeHostsResponse"
    "fixture/DescribeHostsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeHosts)

responseDescribeImageAttribute :: DescribeImageAttributeResponse -> TestTree
responseDescribeImageAttribute = res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeImageAttribute)

responseModifyIdFormat :: ModifyIdFormatResponse -> TestTree
responseModifyIdFormat = res
    "ModifyIdFormatResponse"
    "fixture/ModifyIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy ModifyIdFormat)

responseDeleteRouteTable :: DeleteRouteTableResponse -> TestTree
responseDeleteRouteTable = res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy DeleteRouteTable)

responseResetImageAttribute :: ResetImageAttributeResponse -> TestTree
responseResetImageAttribute = res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetImageAttribute)

responseCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGatewayResponse -> TestTree
responseCreateEgressOnlyInternetGateway = res
    "CreateEgressOnlyInternetGatewayResponse"
    "fixture/CreateEgressOnlyInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateEgressOnlyInternetGateway)

responseDescribeReservedInstancesModifications :: DescribeReservedInstancesModificationsResponse -> TestTree
responseDescribeReservedInstancesModifications = res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeReservedInstancesModifications)

responseDescribeSpotInstanceRequests :: DescribeSpotInstanceRequestsResponse -> TestTree
responseDescribeSpotInstanceRequests = res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotInstanceRequests)

responseUnassignPrivateIPAddresses :: UnassignPrivateIPAddressesResponse -> TestTree
responseUnassignPrivateIPAddresses = res
    "UnassignPrivateIPAddressesResponse"
    "fixture/UnassignPrivateIPAddressesResponse.proto"
    ec2
    (Proxy :: Proxy UnassignPrivateIPAddresses)

responseDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissionsResponse -> TestTree
responseDescribeNetworkInterfacePermissions = res
    "DescribeNetworkInterfacePermissionsResponse"
    "fixture/DescribeNetworkInterfacePermissionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNetworkInterfacePermissions)

responseDescribeVPCEndpointServicePermissions :: DescribeVPCEndpointServicePermissionsResponse -> TestTree
responseDescribeVPCEndpointServicePermissions = res
    "DescribeVPCEndpointServicePermissionsResponse"
    "fixture/DescribeVPCEndpointServicePermissionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointServicePermissions)

responseDeleteDHCPOptions :: DeleteDHCPOptionsResponse -> TestTree
responseDeleteDHCPOptions = res
    "DeleteDHCPOptionsResponse"
    "fixture/DeleteDHCPOptionsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteDHCPOptions)

responseDescribeNetworkACLs :: DescribeNetworkACLsResponse -> TestTree
responseDescribeNetworkACLs = res
    "DescribeNetworkACLsResponse"
    "fixture/DescribeNetworkACLsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNetworkACLs)

responseCancelImportTask :: CancelImportTaskResponse -> TestTree
responseCancelImportTask = res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse.proto"
    ec2
    (Proxy :: Proxy CancelImportTask)

responseDetachClassicLinkVPC :: DetachClassicLinkVPCResponse -> TestTree
responseDetachClassicLinkVPC = res
    "DetachClassicLinkVPCResponse"
    "fixture/DetachClassicLinkVPCResponse.proto"
    ec2
    (Proxy :: Proxy DetachClassicLinkVPC)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions = res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeRegions)

responseMonitorInstances :: MonitorInstancesResponse -> TestTree
responseMonitorInstances = res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    ec2
    (Proxy :: Proxy MonitorInstances)

responseAcceptVPCPeeringConnection :: AcceptVPCPeeringConnectionResponse -> TestTree
responseAcceptVPCPeeringConnection = res
    "AcceptVPCPeeringConnectionResponse"
    "fixture/AcceptVPCPeeringConnectionResponse.proto"
    ec2
    (Proxy :: Proxy AcceptVPCPeeringConnection)

responseImportSnapshot :: ImportSnapshotResponse -> TestTree
responseImportSnapshot = res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    ec2
    (Proxy :: Proxy ImportSnapshot)

responseDescribeVolumeStatus :: DescribeVolumeStatusResponse -> TestTree
responseDescribeVolumeStatus = res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVolumeStatus)

responseDescribeRouteTables :: DescribeRouteTablesResponse -> TestTree
responseDescribeRouteTables = res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeRouteTables)

responseDescribeAvailabilityZones :: DescribeAvailabilityZonesResponse -> TestTree
responseDescribeAvailabilityZones = res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeAvailabilityZones)

responseModifyVPCAttribute :: ModifyVPCAttributeResponse -> TestTree
responseModifyVPCAttribute = res
    "ModifyVPCAttributeResponse"
    "fixture/ModifyVPCAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCAttribute)

responseDescribeFleetHistory :: DescribeFleetHistoryResponse -> TestTree
responseDescribeFleetHistory = res
    "DescribeFleetHistoryResponse"
    "fixture/DescribeFleetHistoryResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFleetHistory)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages = res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeImages)

responseDescribeElasticGpus :: DescribeElasticGpusResponse -> TestTree
responseDescribeElasticGpus = res
    "DescribeElasticGpusResponse"
    "fixture/DescribeElasticGpusResponse.proto"
    ec2
    (Proxy :: Proxy DescribeElasticGpus)

responseRestoreAddressToClassic :: RestoreAddressToClassicResponse -> TestTree
responseRestoreAddressToClassic = res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    ec2
    (Proxy :: Proxy RestoreAddressToClassic)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair = res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    ec2
    (Proxy :: Proxy CreateKeyPair)

responseGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuoteResponse -> TestTree
responseGetReservedInstancesExchangeQuote = res
    "GetReservedInstancesExchangeQuoteResponse"
    "fixture/GetReservedInstancesExchangeQuoteResponse.proto"
    ec2
    (Proxy :: Proxy GetReservedInstancesExchangeQuote)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVolume)

responseDeleteVPCEndpointServiceConfigurations :: DeleteVPCEndpointServiceConfigurationsResponse -> TestTree
responseDeleteVPCEndpointServiceConfigurations = res
    "DeleteVPCEndpointServiceConfigurationsResponse"
    "fixture/DeleteVPCEndpointServiceConfigurationsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPCEndpointServiceConfigurations)

responseDescribeSpotFleetInstances :: DescribeSpotFleetInstancesResponse -> TestTree
responseDescribeSpotFleetInstances = res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotFleetInstances)
