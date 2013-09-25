{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.EC2
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.EC2 (tests) where

import Network.AWS.EC2
import Test.Common

tests :: [Test]
tests = (:[]) $ testVersion ec2Version
    [ testGroup "Requests"
        [ testProperty "AllocateAddress"                     (prop :: TRq AllocateAddress)
        -- , testProperty "AssignPrivateIpAddresses"            (prop :: TRq AssignPrivateIpAddresses)
        -- , testProperty "AssociateAddress"                    (prop :: TRq AssociateAddress)
        -- , testProperty "AssociateDhcpOptions"                (prop :: TRq AssociateDhcpOptions)
        -- , testProperty "AssociateRouteTable"                 (prop :: TRq AssociateRouteTable)
        -- , testProperty "AttachInternetGateway"               (prop :: TRq AttachInternetGateway)
        -- , testProperty "AttachNetworkInterface"              (prop :: TRq AttachNetworkInterface)
        -- , testProperty "AttachVolume"                        (prop :: TRq AttachVolume)
        -- , testProperty "AttachVpnGateway"                    (prop :: TRq AttachVpnGateway)
        -- , testProperty "AuthorizeSecurityGroupEgress"        (prop :: TRq AuthorizeSecurityGroupEgress)
        -- , testProperty "AuthorizeSecurityGroupIngress"       (prop :: TRq AuthorizeSecurityGroupIngress)
        -- , testProperty "BundleInstance"                      (prop :: TRq BundleInstance)
        -- , testProperty "CancelBundleTask"                    (prop :: TRq CancelBundleTask)
        -- , testProperty "CancelConversionTask"                (prop :: TRq CancelConversionTask)
        -- , testProperty "CancelExportTask"                    (prop :: TRq CancelExportTask)
        -- , testProperty "CancelReservedInstancesListing"      (prop :: TRq CancelReservedInstancesListing)
        -- , testProperty "CancelSpotInstanceRequests"          (prop :: TRq CancelSpotInstanceRequests)

        -- , testProperty "ConfirmProductInstance"             (prop :: TRq ConfirmProductInstance)
        -- , testProperty "CopyImage"                          (prop :: TRq CopyImage)
        -- , testProperty "CopySnapshot"                       (prop :: TRq CopySnapshot)
        -- , testProperty "CreateCustomerGateway"              (prop :: TRq CreateCustomerGateway)
        -- , testProperty "CreateDhcpOptions"                  (prop :: TRq CreateDhcpOptions)
        -- , testProperty "CreateImage"                        (prop :: TRq CreateImage)
        -- , testProperty "CreateInstanceExportTask"           (prop :: TRq CreateInstanceExportTask)
        -- , testProperty "CreateInternetGateway"              (prop :: TRq CreateInternetGateway)
        -- , testProperty "CreateKeyPair"                      (prop :: TRq CreateKeyPair)
        -- , testProperty "CreateNetworkAcl"                   (prop :: TRq CreateNetworkAcl)
        -- , testProperty "CreateNetworkAclEntry"              (prop :: TRq CreateNetworkAclEntry)
        -- , testProperty "CreateNetworkInterface"             (prop :: TRq CreateNetworkInterface)
        -- , testProperty "CreatePlacementGroup"               (prop :: TRq CreatePlacementGroup)
        -- , testProperty "CreateReservedInstancesListing"     (prop :: TRq CreateReservedInstancesListing)
        -- , testProperty "CreateRoute"                        (prop :: TRq CreateRoute)
        -- , testProperty "CreateRouteTable"                   (prop :: TRq CreateRouteTable)
        , testProperty "CreateSecurityGroup"                (prop :: TRq CreateSecurityGroup)
        -- , testProperty "CreateSnapshot"                     (prop :: TRq CreateSnapshot)
        -- , testProperty "CreateSpotDatafeedSubscription"     (prop :: TRq CreateSpotDatafeedSubscription)
        -- , testProperty "CreateSubnet"                       (prop :: TRq CreateSubnet)
        -- , testProperty "CreateTags"                         (prop :: TRq CreateTags)
        -- , testProperty "CreateVolume"                       (prop :: TRq CreateVolume)
        -- , testProperty "CreateVpc"                          (prop :: TRq CreateVpc)
        -- , testProperty "CreateVpnConnection"                (prop :: TRq CreateVpnConnection)
        -- , testProperty "CreateVpnConnectionRoute"           (prop :: TRq CreateVpnConnectionRoute)
        -- , testProperty "CreateVpnGateway"                   (prop :: TRq CreateVpnGateway)
        -- , testProperty "DeleteCustomerGateway"              (prop :: TRq DeleteCustomerGateway)
        -- , testProperty "DeleteDhcpOptions"                  (prop :: TRq DeleteDhcpOptions)
        -- , testProperty "DeleteInternetGateway"              (prop :: TRq DeleteInternetGateway)
        -- , testProperty "DeleteKeyPair"                      (prop :: TRq DeleteKeyPair)
        -- , testProperty "DeleteNetworkAcl"                   (prop :: TRq DeleteNetworkAcl)
        -- , testProperty "DeleteNetworkAclEntry"              (prop :: TRq DeleteNetworkAclEntry)
        -- , testProperty "DeleteNetworkInterface"             (prop :: TRq DeleteNetworkInterface)
        -- , testProperty "DeletePlacementGroup"               (prop :: TRq DeletePlacementGroup)
        -- , testProperty "DeleteRoute"                        (prop :: TRq DeleteRoute)
        -- , testProperty "DeleteRouteTable"                   (prop :: TRq DeleteRouteTable)
        , testProperty "DeleteSecurityGroup"                (prop :: TRq DeleteSecurityGroup)
        -- , testProperty "DeleteSnapshot"                     (prop :: TRq DeleteSnapshot)
        -- , testProperty "DeleteSpotDatafeedSubscription"     (prop :: TRq DeleteSpotDatafeedSubscription)
        -- , testProperty "DeleteSubnet"                       (prop :: TRq DeleteSubnet)
        -- , testProperty "DeleteTags"                         (prop :: TRq DeleteTags)
        -- , testProperty "DeleteVolume"                       (prop :: TRq DeleteVolume)
        -- , testProperty "DeleteVpc"                          (prop :: TRq DeleteVpc)
        -- , testProperty "DeleteVpnConnection"                (prop :: TRq DeleteVpnConnection)
        -- , testProperty "DeleteVpnConnectionRoute"           (prop :: TRq DeleteVpnConnectionRoute)
        -- , testProperty "DeleteVpnGateway"                   (prop :: TRq DeleteVpnGateway)
        -- , testProperty "DeregisterImage"                    (prop :: TRq DeregisterImage)
        -- , testProperty "DescribeAccountAttributes"          (prop :: TRq DescribeAccountAttributes)
        -- , testProperty "DescribeAddresses"                  (prop :: TRq DescribeAddresses)
        -- , testProperty "DescribeAvailabilityZones"          (prop :: TRq DescribeAvailabilityZones)
        -- , testProperty "DescribeBundleTasks"                (prop :: TRq DescribeBundleTasks)
        -- , testProperty "DescribeConversionTasks"            (prop :: TRq DescribeConversionTasks)
        -- , testProperty "DescribeCustomerGateways"           (prop :: TRq DescribeCustomerGateways)
        -- , testProperty "DescribeDhcpOptions"                (prop :: TRq DescribeDhcpOptions)
        -- , testProperty "DescribeExportTasks"                (prop :: TRq DescribeExportTasks)
        -- , testProperty "DescribeImageAttribute"             (prop :: TRq DescribeImageAttribute)
        , testProperty "DescribeImages"                     (prop :: TRq DescribeImages)
        -- , testProperty "DescribeInstanceAttribute"          (prop :: TRq DescribeInstanceAttribute)
        , testProperty "DescribeInstances"                  (prop :: TRq DescribeInstances)
        -- , testProperty "DescribeInstanceStatus"             (prop :: TRq DescribeInstanceStatus)
        -- , testProperty "DescribeInternetGateways"           (prop :: TRq DescribeInternetGateways)
        -- , testProperty "DescribeKeyPairs"                   (prop :: TRq DescribeKeyPairs)
        -- , testProperty "DescribeNetworkAcls"                (prop :: TRq DescribeNetworkAcls)
        -- , testProperty "DescribeNetworkInterfaceAttribute"  (prop :: TRq DescribeNetworkInterfaceAttribute)
        -- , testProperty "DescribeNetworkInterfaces"          (prop :: TRq DescribeNetworkInterfaces)
        -- , testProperty "DescribePlacementGroups"            (prop :: TRq DescribePlacementGroups)
        -- , testProperty "DescribeRegions"                    (prop :: TRq DescribeRegions)
        -- , testProperty "DescribeReservedInstances"          (prop :: TRq DescribeReservedInstances)
        -- , testProperty "DescribeReservedInstancesListings"  (prop :: TRq DescribeReservedInstancesListings)
        -- , testProperty "DescribeReservedInstancesOfferings" (prop :: TRq DescribeReservedInstancesOfferings)
        -- , testProperty "DescribeRouteTables"                (prop :: TRq DescribeRouteTables)
        , testProperty "DescribeSecurityGroups"             (prop :: TRq DescribeSecurityGroups)
        -- , testProperty "DescribeSnapshotAttribute"          (prop :: TRq DescribeSnapshotAttribute)
        -- , testProperty "DescribeSnapshots"                  (prop :: TRq DescribeSnapshots)
        -- , testProperty "DescribeSpotDatafeedSubscription"   (prop :: TRq DescribeSpotDatafeedSubscription)
        -- , testProperty "DescribeSpotInstanceRequests"       (prop :: TRq DescribeSpotInstanceRequests)
        -- , testProperty "DescribeSpotPriceHistory"           (prop :: TRq DescribeSpotPriceHistory)
        -- , testProperty "DescribeSubnets"                    (prop :: TRq DescribeSubnets)
        , testProperty "DescribeTags"                       (prop :: TRq DescribeTags)
        -- , testProperty "DescribeVolumeAttribute"            (prop :: TRq DescribeVolumeAttribute)
        -- , testProperty "DescribeVolumes"                    (prop :: TRq DescribeVolumes)
        -- , testProperty "DescribeVolumeStatus"               (prop :: TRq DescribeVolumeStatus)
        -- , testProperty "DescribeVpcAttribute"               (prop :: TRq DescribeVpcAttribute)
        -- , testProperty "DescribeVpcs"                       (prop :: TRq DescribeVpcs)
        -- , testProperty "DescribeVpnConnections"             (prop :: TRq DescribeVpnConnections)
        -- , testProperty "DescribeVpnGateways"                (prop :: TRq DescribeVpnGateways)
        -- , testProperty "DetachInternetGateway"              (prop :: TRq DetachInternetGateway)
        -- , testProperty "DetachNetworkInterface"             (prop :: TRq DetachNetworkInterface)
        -- , testProperty "DetachVolume"                       (prop :: TRq DetachVolume)
        -- , testProperty "DetachVpnGateway"                   (prop :: TRq DetachVpnGateway)
        -- , testProperty "DisableVgwRoutePropagation"         (prop :: TRq DisableVgwRoutePropagation)
        -- , testProperty "DisassociateAddress"                (prop :: TRq DisassociateAddress)
        -- , testProperty "DisassociateRouteTable"             (prop :: TRq DisassociateRouteTable)
        -- , testProperty "EnableVgwRoutePropagation"          (prop :: TRq EnableVgwRoutePropagation)
        -- , testProperty "EnableVolumeIO"                     (prop :: TRq EnableVolumeIO)
        -- , testProperty "GetConsoleOutput"                   (prop :: TRq GetConsoleOutput)
        -- , testProperty "GetPasswordData"                    (prop :: TRq GetPasswordData)
        -- , testProperty "ImportInstance"                     (prop :: TRq ImportInstance)
        -- , testProperty "ImportKeyPair"                      (prop :: TRq ImportKeyPair)
        -- , testProperty "ImportVolume"                       (prop :: TRq ImportVolume)
        -- , testProperty "ModifyImageAttribute"               (prop :: TRq ModifyImageAttribute)
        -- , testProperty "ModifyInstanceAttribute"            (prop :: TRq ModifyInstanceAttribute)
        -- , testProperty "ModifyNetworkInterfaceAttribute"    (prop :: TRq ModifyNetworkInterfaceAttribute)
        -- , testProperty "ModifySnapshotAttribute"            (prop :: TRq ModifySnapshotAttribute)
        -- , testProperty "ModifyVolumeAttribute"              (prop :: TRq ModifyVolumeAttribute)
        -- , testProperty "ModifyVpcAttribute"                 (prop :: TRq ModifyVpcAttribute)
        -- , testProperty "MonitorInstances"                   (prop :: TRq MonitorInstances)
        -- , testProperty "PurchaseReservedInstancesOffering"  (prop :: TRq PurchaseReservedInstancesOffering)
        -- , testProperty "RebootInstances"                    (prop :: TRq RebootInstances)
        -- , testProperty "RegisterImage"                      (prop :: TRq RegisterImage)
        -- , testProperty "ReleaseAddress"                     (prop :: TRq ReleaseAddress)
        -- , testProperty "ReplaceNetworkAclAssociation"       (prop :: TRq ReplaceNetworkAclAssociation)
        -- , testProperty "ReplaceNetworkAclEntry"             (prop :: TRq ReplaceNetworkAclEntry)
        -- , testProperty "ReplaceRoute"                       (prop :: TRq ReplaceRoute)
        -- , testProperty "ReplaceRouteTableAssociation"       (prop :: TRq ReplaceRouteTableAssociation)
        -- , testProperty "ReportInstanceStatus"               (prop :: TRq ReportInstanceStatus)
        -- , testProperty "RequestSpotInstances"               (prop :: TRq RequestSpotInstances)
        -- , testProperty "ResetImageAttribute"                (prop :: TRq ResetImageAttribute)
        -- , testProperty "ResetInstanceAttribute"             (prop :: TRq ResetInstanceAttribute)
        -- , testProperty "ResetNetworkInterfaceAttribute"     (prop :: TRq ResetNetworkInterfaceAttribute)
        -- , testProperty "ResetSnapshotAttribute"             (prop :: TRq ResetSnapshotAttribute)
        , testProperty "RevokeSecurityGroupEgress"          (prop :: TRq RevokeSecurityGroupEgress)
        , testProperty "RevokeSecurityGroupIngress"         (prop :: TRq RevokeSecurityGroupIngress)
        -- , testProperty "RunInstances"                       (prop :: TRq RunInstances)
        -- , testProperty "StartInstances"                     (prop :: TRq StartInstances)
        -- , testProperty "StopInstances"                      (prop :: TRq StopInstances)
        -- , testProperty "TerminateInstances"                 (prop :: TRq TerminateInstances)
        -- , testProperty "UnassignPrivateIpAddresses"         (prop :: TRq UnassignPrivateIpAddresses)
        -- , testProperty "UnmonitorInstances"                 (prop :: TRq UnmonitorInstances)
      ]

    , testGroup "Responses"
        [ testProperty "AllocateAddressResponse"                (prop :: TRs AllocateAddressResponse)
        -- , testProperty "AssignPrivateIpAddressesResponse"       (prop :: TRs AssignPrivateIpAddressesResponse)
        -- , testProperty "AssociateAddressResponse"               (prop :: TRs AssociateAddressResponse)
        -- , testProperty "AssociateDhcpOptionsResponse"           (prop :: TRs AssociateDhcpOptionsResponse)
        -- , testProperty "AssociateRouteTableResponse"            (prop :: TRs AssociateRouteTableResponse)
        -- , testProperty "AttachInternetGatewayResponse"          (prop :: TRs AttachInternetGatewayResponse)
        -- , testProperty "AttachNetworkInterfaceResponse"         (prop :: TRs AttachNetworkInterfaceResponse)
        -- , testProperty "AttachVolumeResponse"                   (prop :: TRs AttachVolumeResponse)
        -- , testProperty "AttachVpnGatewayResponse"               (prop :: TRs AttachVpnGatewayResponse)
        -- , testProperty "AuthorizeSecurityGroupEgressResponse"   (prop :: TRs AuthorizeSecurityGroupEgressResponse)
        -- , testProperty "AuthorizeSecurityGroupIngressResponse"  (prop :: TRs AuthorizeSecurityGroupIngressResponse)
        -- , testProperty "BundleInstanceResponse"                 (prop :: TRs BundleInstanceResponse)
        -- , testProperty "CancelBundleTaskResponse"               (prop :: TRs CancelBundleTaskResponse)
        -- , testProperty "CancelConversionTaskResponse"           (prop :: TRs CancelConversionTaskResponse)
        -- , testProperty "CancelExportTaskResponse"               (prop :: TRs CancelExportTaskResponse)
        -- , testProperty "CancelReservedInstancesListingResponse" (prop :: TRs CancelReservedInstancesListingResponse)
        -- , testProperty "CancelSpotInstanceRequestsResponse"     (prop :: TRs CancelSpotInstanceRequestsResponse)
        -- , testProperty "ConfirmProductInstanceResponse"             (prop :: TRs ConfirmProductInstanceResponse)
        -- , testProperty "CopyImageResponse"                          (prop :: TRs CopyImageResponse)
        -- , testProperty "CopySnapshotResponse"                       (prop :: TRs CopySnapshotResponse)
        -- , testProperty "CreateCustomerGatewayResponse"              (prop :: TRs CreateCustomerGatewayResponse)
        -- , testProperty "CreateDhcpOptionsResponse"                  (prop :: TRs CreateDhcpOptionsResponse)
        -- , testProperty "CreateImageResponse"                        (prop :: TRs CreateImageResponse)
        -- , testProperty "CreateInstanceExportTaskResponse"           (prop :: TRs CreateInstanceExportTaskResponse)
        -- , testProperty "CreateInternetGatewayResponse"              (prop :: TRs CreateInternetGatewayResponse)
        -- , testProperty "CreateKeyPairResponse"                      (prop :: TRs CreateKeyPairResponse)
        -- , testProperty "CreateNetworkAclResponse"                   (prop :: TRs CreateNetworkAclResponse)
        -- , testProperty "CreateNetworkAclEntryResponse"              (prop :: TRs CreateNetworkAclEntryResponse)
        -- , testProperty "CreateNetworkInterfaceResponse"             (prop :: TRs CreateNetworkInterfaceResponse)
        -- , testProperty "CreatePlacementGroupResponse"               (prop :: TRs CreatePlacementGroupResponse)
        -- , testProperty "CreateReservedInstancesListingResponse"     (prop :: TRs CreateReservedInstancesListingResponse)
        -- , testProperty "CreateRouteResponse"                        (prop :: TRs CreateRouteResponse)
        -- , testProperty "CreateRouteTableResponse"                   (prop :: TRs CreateRouteTableResponse)
        , testProperty "CreateSecurityGroupResponse"                (prop :: TRs CreateSecurityGroupResponse)
        -- , testProperty "CreateSnapshotResponse"                     (prop :: TRs CreateSnapshotResponse)
        -- , testProperty "CreateSpotDatafeedSubscriptionResponse"     (prop :: TRs CreateSpotDatafeedSubscriptionResponse)
        -- , testProperty "CreateSubnetResponse"                       (prop :: TRs CreateSubnetResponse)
        -- , testProperty "CreateTagsResponse"                         (prop :: TRs CreateTagsResponse)
        -- , testProperty "CreateVolumeResponse"                       (prop :: TRs CreateVolumeResponse)
        -- , testProperty "CreateVpcResponse"                          (prop :: TRs CreateVpcResponse)
        -- , testProperty "CreateVpnConnectionResponse"                (prop :: TRs CreateVpnConnectionResponse)
        -- , testProperty "CreateVpnConnectionRouteResponse"           (prop :: TRs CreateVpnConnectionRouteResponse)
        -- , testProperty "CreateVpnGatewayResponse"                   (prop :: TRs CreateVpnGatewayResponse)
        -- , testProperty "DeleteCustomerGatewayResponse"              (prop :: TRs DeleteCustomerGatewayResponse)
        -- , testProperty "DeleteDhcpOptionsResponse"                  (prop :: TRs DeleteDhcpOptionsResponse)
        -- , testProperty "DeleteInternetGatewayResponse"              (prop :: TRs DeleteInternetGatewayResponse)
        -- , testProperty "DeleteKeyPairResponse"                      (prop :: TRs DeleteKeyPairResponse)
        -- , testProperty "DeleteNetworkAclResponse"                   (prop :: TRs DeleteNetworkAclResponse)
        -- , testProperty "DeleteNetworkAclEntryResponse"              (prop :: TRs DeleteNetworkAclEntryResponse)
        -- , testProperty "DeleteNetworkInterfaceResponse"             (prop :: TRs DeleteNetworkInterfaceResponse)
        -- , testProperty "DeletePlacementGroupResponse"               (prop :: TRs DeletePlacementGroupResponse)
        -- , testProperty "DeleteRouteResponse"                        (prop :: TRs DeleteRouteResponse)
        -- , testProperty "DeleteRouteTableResponse"                   (prop :: TRs DeleteRouteTableResponse)
        , testProperty "DeleteSecurityGroupResponse"                (prop :: TRs DeleteSecurityGroupResponse)
        -- , testProperty "DeleteSnapshotResponse"                     (prop :: TRs DeleteSnapshotResponse)
        -- , testProperty "DeleteSpotDatafeedSubscriptionResponse"     (prop :: TRs DeleteSpotDatafeedSubscriptionResponse)
        -- , testProperty "DeleteSubnetResponse"                       (prop :: TRs DeleteSubnetResponse)
        -- , testProperty "DeleteTagsResponse"                         (prop :: TRs DeleteTagsResponse)
        -- , testProperty "DeleteVolumeResponse"                       (prop :: TRs DeleteVolumeResponse)
        -- , testProperty "DeleteVpcResponse"                          (prop :: TRs DeleteVpcResponse)
        -- , testProperty "DeleteVpnConnectionResponse"                (prop :: TRs DeleteVpnConnectionResponse)
        -- , testProperty "DeleteVpnConnectionRouteResponse"           (prop :: TRs DeleteVpnConnectionRouteResponse)
        -- , testProperty "DeleteVpnGatewayResponse"                   (prop :: TRs DeleteVpnGatewayResponse)
        -- , testProperty "DeregisterImageResponse"                    (prop :: TRs DeregisterImageResponse)
        -- , testProperty "DescribeAccountAttributesResponse"          (prop :: TRs DescribeAccountAttributesResponse)
        -- , testProperty "DescribeAddressesResponse"                  (prop :: TRs DescribeAddressesResponse)
        -- , testProperty "DescribeAvailabilityZonesResponse"          (prop :: TRs DescribeAvailabilityZonesResponse)
        -- , testProperty "DescribeBundleTasksResponse"                (prop :: TRs DescribeBundleTasksResponse)
        -- , testProperty "DescribeConversionTasksResponse"            (prop :: TRs DescribeConversionTasksResponse)
        -- , testProperty "DescribeCustomerGatewaysResponse"           (prop :: TRs DescribeCustomerGatewaysResponse)
        -- , testProperty "DescribeDhcpOptionsResponse"                (prop :: TRs DescribeDhcpOptionsResponse)
        -- , testProperty "DescribeExportTasksResponse"                (prop :: TRs DescribeExportTasksResponse)
        -- , testProperty "DescribeImageAttributeResponse"             (prop :: TRs DescribeImageAttributeResponse)
        , testProperty "DescribeImagesResponse"                     (prop :: TRs DescribeImagesResponse)
        -- , testProperty "DescribeInstanceAttributeResponse"          (prop :: TRs DescribeInstanceAttributeResponse)
        , testProperty "DescribeInstancesResponse"                  (prop :: TRs DescribeInstancesResponse)
        -- , testProperty "DescribeInstanceStatusResponse"             (prop :: TRs DescribeInstanceStatusResponse)
        -- , testProperty "DescribeInternetGatewaysResponse"           (prop :: TRs DescribeInternetGatewaysResponse)
        -- , testProperty "DescribeKeyPairsResponse"                   (prop :: TRs DescribeKeyPairsResponse)
        -- , testProperty "DescribeNetworkAclsResponse"                (prop :: TRs DescribeNetworkAclsResponse)
        -- , testProperty "DescribeNetworkInterfaceAttributeResponse"  (prop :: TRs DescribeNetworkInterfaceAttributeResponse)
        -- , testProperty "DescribeNetworkInterfacesResponse"          (prop :: TRs DescribeNetworkInterfacesResponse)
        -- , testProperty "DescribePlacementGroupsResponse"            (prop :: TRs DescribePlacementGroupsResponse)
        -- , testProperty "DescribeRegionsResponse"                    (prop :: TRs DescribeRegionsResponse)
        -- , testProperty "DescribeReservedInstancesResponse"          (prop :: TRs DescribeReservedInstancesResponse)
        -- , testProperty "DescribeReservedInstancesListingsResponse"  (prop :: TRs DescribeReservedInstancesListingsResponse)
        -- , testProperty "DescribeReservedInstancesOfferingsResponse" (prop :: TRs DescribeReservedInstancesOfferingsResponse)
        -- , testProperty "DescribeRouteTablesResponse"                (prop :: TRs DescribeRouteTablesResponse)
        , testProperty "DescribeSecurityGroupsResponse"             (prop :: TRs DescribeSecurityGroupsResponse)
        -- , testProperty "DescribeSnapshotAttributeResponse"          (prop :: TRs DescribeSnapshotAttributeResponse)
        -- , testProperty "DescribeSnapshotsResponse"                  (prop :: TRs DescribeSnapshotsResponse)
        -- , testProperty "DescribeSpotDatafeedSubscriptionResponse"   (prop :: TRs DescribeSpotDatafeedSubscriptionResponse)
        -- , testProperty "DescribeSpotInstanceRequestsResponse"       (prop :: TRs DescribeSpotInstanceRequestsResponse)
        -- , testProperty "DescribeSpotPriceHistoryResponse"           (prop :: TRs DescribeSpotPriceHistoryResponse)
        -- , testProperty "DescribeSubnetsResponse"                    (prop :: TRs DescribeSubnetsResponse)
        , testProperty "DescribeTagsResponse"                       (prop :: TRs DescribeTagsResponse)
        -- , testProperty "DescribeVolumeAttributeResponse"            (prop :: TRs DescribeVolumeAttributeResponse)
        -- , testProperty "DescribeVolumesResponse"                    (prop :: TRs DescribeVolumesResponse)
        -- , testProperty "DescribeVolumeStatusResponse"               (prop :: TRs DescribeVolumeStatusResponse)
        -- , testProperty "DescribeVpcAttributeResponse"               (prop :: TRs DescribeVpcAttributeResponse)
        -- , testProperty "DescribeVpcsResponse"                       (prop :: TRs DescribeVpcsResponse)
        -- , testProperty "DescribeVpnConnectionsResponse"             (prop :: TRs DescribeVpnConnectionsResponse)
        -- , testProperty "DescribeVpnGatewaysResponse"                (prop :: TRs DescribeVpnGatewaysResponse)
        -- , testProperty "DetachInternetGatewayResponse"              (prop :: TRs DetachInternetGatewayResponse)
        -- , testProperty "DetachNetworkInterfaceResponse"             (prop :: TRs DetachNetworkInterfaceResponse)
        -- , testProperty "DetachVolumeResponse"                       (prop :: TRs DetachVolumeResponse)
        -- , testProperty "DetachVpnGatewayResponse"                   (prop :: TRs DetachVpnGatewayResponse)
        -- , testProperty "DisableVgwRoutePropagationResponse"         (prop :: TRs DisableVgwRoutePropagationResponse)
        -- , testProperty "DisassociateAddressResponse"                (prop :: TRs DisassociateAddressResponse)
        -- , testProperty "DisassociateRouteTableResponse"             (prop :: TRs DisassociateRouteTableResponse)
        -- , testProperty "EnableVgwRoutePropagationResponse"          (prop :: TRs EnableVgwRoutePropagationResponse)
        -- , testProperty "EnableVolumeIOResponse"                     (prop :: TRs EnableVolumeIOResponse)
        -- , testProperty "GetConsoleOutputResponse"                   (prop :: TRs GetConsoleOutputResponse)
        -- , testProperty "GetPasswordDataResponse"                    (prop :: TRs GetPasswordDataResponse)
        -- , testProperty "ImportInstanceResponse"                     (prop :: TRs ImportInstanceResponse)
        -- , testProperty "ImportKeyPairResponse"                      (prop :: TRs ImportKeyPairResponse)
        -- , testProperty "ImportVolumeResponse"                       (prop :: TRs ImportVolumeResponse)
        -- , testProperty "ModifyImageAttributeResponse"               (prop :: TRs ModifyImageAttributeResponse)
        -- , testProperty "ModifyInstanceAttributeResponse"            (prop :: TRs ModifyInstanceAttributeResponse)
        -- , testProperty "ModifyNetworkInterfaceAttributeResponse"    (prop :: TRs ModifyNetworkInterfaceAttributeResponse)
        -- , testProperty "ModifySnapshotAttributeResponse"            (prop :: TRs ModifySnapshotAttributeResponse)
        -- , testProperty "ModifyVolumeAttributeResponse"              (prop :: TRs ModifyVolumeAttributeResponse)
        -- , testProperty "ModifyVpcAttributeResponse"                 (prop :: TRs ModifyVpcAttributeResponse)
        -- , testProperty "MonitorInstancesResponse"                   (prop :: TRs MonitorInstancesResponse)
        -- , testProperty "PurchaseReservedInstancesOfferingResponse"  (prop :: TRs PurchaseReservedInstancesOfferingResponse)
        -- , testProperty "RebootInstancesResponse"                    (prop :: TRs RebootInstancesResponse)
        -- , testProperty "RegisterImageResponse"                      (prop :: TRs RegisterImageResponse)
        -- , testProperty "ReleaseAddressResponse"                     (prop :: TRs ReleaseAddressResponse)
        -- , testProperty "ReplaceNetworkAclAssociationResponse"       (prop :: TRs ReplaceNetworkAclAssociationResponse)
        -- , testProperty "ReplaceNetworkAclEntryResponse"             (prop :: TRs ReplaceNetworkAclEntryResponse)
        -- , testProperty "ReplaceRouteResponse"                       (prop :: TRs ReplaceRouteResponse)
        -- , testProperty "ReplaceRouteTableAssociationResponse"       (prop :: TRs ReplaceRouteTableAssociationResponse)
        -- , testProperty "ReportInstanceStatusResponse"               (prop :: TRs ReportInstanceStatusResponse)
        -- , testProperty "RequestSpotInstancesResponse"               (prop :: TRs RequestSpotInstancesResponse)
        -- , testProperty "ResetImageAttributeResponse"                (prop :: TRs ResetImageAttributeResponse)
        -- , testProperty "ResetInstanceAttributeResponse"             (prop :: TRs ResetInstanceAttributeResponse)
        -- , testProperty "ResetNetworkInterfaceAttributeResponse"     (prop :: TRs ResetNetworkInterfaceAttributeResponse)
        -- , testProperty "ResetSnapshotAttributeResponse"             (prop :: TRs ResetSnapshotAttributeResponse)
        , testProperty "RevokeSecurityGroupEgressResponse"          (prop :: TRs RevokeSecurityGroupEgressResponse)
        , testProperty "RevokeSecurityGroupIngressResponse"         (prop :: TRs RevokeSecurityGroupIngressResponse)
        -- , testProperty "RunInstancesResponse"                       (prop :: TRs RunInstancesResponse)
        -- , testProperty "StartInstancesResponse"                     (prop :: TRs StartInstancesResponse)
        -- , testProperty "StopInstancesResponse"                      (prop :: TRs StopInstancesResponse)
        -- , testProperty "TerminateInstancesResponse"                 (prop :: TRs TerminateInstancesResponse)
        -- , testProperty "UnassignPrivateIpAddressesResponse"         (prop :: TRs UnassignPrivateIpAddressesResponse)
        -- , testProperty "UnmonitorInstancesResponse"                 (prop :: TRs UnmonitorInstancesResponse)
        ]
    ]

instance ToJSON AddressDomain where
    toJSON = stringify

instance ToJSON VolumeStatus where
    toJSON = stringify

instance ToJSON BundleInstanceState where
    toJSON = stringify

instance ToJSON TagResourceType where
    toJSON = stringify

instance ToJSON Protocol where
    toJSON = stringify

$(deriveArbitrary
   [ ''AddressDomain
   , ''BundleInstanceState
   , ''TagResourceType
   , ''VolumeStatus
   , ''Protocol
   ])

$(deriveDependency
    [ ''Attachment
    , ''BlockDeviceMappingItemType
    , ''BundleInstanceS3Storage
    , ''BundleInstanceTask
    , ''BundleInstanceTaskError
    , ''BundleInstanceTaskStorage
    , ''CancelSpotInstanceRequestsResponseSetItemType
    , ''DescribeImagesResponseItemType
    , ''DescribeReservedInstancesListingsResponseSetItemType
    , ''EbsBlockDeviceType
    , ''EbsInstanceBlockDeviceMappingResponseType
    , ''Filter
    , ''Filters
    , ''GroupItemType
    , ''IamInstanceProfileResponseType
    , ''InstanceBlockDeviceMappingResponseItemType
    , ''InstanceCountsSetItemType
    , ''InstanceMonitoringStateType
    , ''InstanceNetworkInterfaceAssociationType
    , ''InstanceNetworkInterfaceAttachmentType
    , ''InstanceNetworkInterfaceSetItemType
    , ''InstancePrivateIpAddressesSetItemType
    , ''InstanceStateType
    , ''IpPermission
    , ''IpRange
    , ''PlacementResponseType
    , ''PriceScheduleSetItemType
    , ''ProductCodesSetItemType
    , ''ReservationInfoType
    , ''ResourceTagSetItemType
    , ''RunningInstancesItemType
    , ''SecurityGroupItemType
    , ''StateReasonType
    , ''TagFilter
    , ''TagSetItemType
    , ''UserIdGroupPair
    ])

$(deriveProperty "test/resources/EC2"
    [ ''AllocateAddress
    , ''AllocateAddressResponse
    , ''AssignPrivateIpAddresses
    , ''AssignPrivateIpAddressesResponse
    , ''AssociateAddress
    , ''AssociateAddressResponse
    , ''AssociateDhcpOptions
    , ''AssociateDhcpOptionsResponse
    , ''AssociateRouteTable
    , ''AssociateRouteTableResponse
    , ''AttachInternetGateway
    , ''AttachInternetGatewayResponse
    , ''AttachNetworkInterface
    , ''AttachNetworkInterfaceResponse
    , ''AttachVolume
    , ''AttachVolumeResponse
    , ''AttachVpnGateway
    , ''AttachVpnGatewayResponse
    , ''AuthorizeSecurityGroupEgress
    , ''AuthorizeSecurityGroupEgressResponse
    , ''AuthorizeSecurityGroupIngress
    , ''AuthorizeSecurityGroupIngressResponse
    , ''BundleInstance
    , ''BundleInstanceResponse
    , ''CancelBundleTask
    , ''CancelBundleTaskResponse
    , ''CancelConversionTask
    , ''CancelConversionTaskResponse
    , ''CancelExportTask
    , ''CancelExportTaskResponse
    , ''CancelReservedInstancesListing
    , ''CancelReservedInstancesListingResponse
    , ''CancelSpotInstanceRequests
    , ''CancelSpotInstanceRequestsResponse
    -- , ''ConfirmProductInstance
    -- , ''ConfirmProductInstanceResponse
    -- , ''CopyImage
    -- , ''CopyImageResponse
    -- , ''CopySnapshot
    -- , ''CopySnapshotResponse
    -- , ''CreateCustomerGateway
    -- , ''CreateCustomerGatewayResponse
    -- , ''CreateDhcpOptions
    -- , ''CreateDhcpOptionsResponse
    -- , ''CreateImage
    -- , ''CreateImageResponse
    -- , ''CreateInstanceExportTask
    -- , ''CreateInstanceExportTaskResponse
    -- , ''CreateInternetGateway
    -- , ''CreateInternetGatewayResponse
    -- , ''CreateKeyPair
    -- , ''CreateKeyPairResponse
    -- , ''CreateNetworkAcl
    -- , ''CreateNetworkAclResponse
    -- , ''CreateNetworkAclEntry
    -- , ''CreateNetworkAclEntryResponse
    -- , ''CreateNetworkInterface
    -- , ''CreateNetworkInterfaceResponse
    -- , ''CreatePlacementGroup
    -- , ''CreatePlacementGroupResponse
    -- , ''CreateReservedInstancesListing
    -- , ''CreateReservedInstancesListingResponse
    -- , ''CreateRoute
    -- , ''CreateRouteResponse
    -- , ''CreateRouteTable
    -- , ''CreateRouteTableResponse
    , ''CreateSecurityGroup
    , ''CreateSecurityGroupResponse
    -- , ''CreateSnapshot
    -- , ''CreateSnapshotResponse
    -- , ''CreateSpotDatafeedSubscription
    -- , ''CreateSpotDatafeedSubscriptionResponse
    -- , ''CreateSubnet
    -- , ''CreateSubnetResponse
    -- , ''CreateTags
    -- , ''CreateTagsResponse
    -- , ''CreateVolume
    -- , ''CreateVolumeResponse
    -- , ''CreateVpc
    -- , ''CreateVpcResponse
    -- , ''CreateVpnConnection
    -- , ''CreateVpnConnectionResponse
    -- , ''CreateVpnConnectionRoute
    -- , ''CreateVpnConnectionRouteResponse
    -- , ''CreateVpnGateway
    -- , ''CreateVpnGatewayResponse
    -- , ''DeleteCustomerGateway
    -- , ''DeleteCustomerGatewayResponse
    -- , ''DeleteDhcpOptions
    -- , ''DeleteDhcpOptionsResponse
    -- , ''DeleteInternetGateway
    -- , ''DeleteInternetGatewayResponse
    -- , ''DeleteKeyPair
    -- , ''DeleteKeyPairResponse
    -- , ''DeleteNetworkAcl
    -- , ''DeleteNetworkAclResponse
    -- , ''DeleteNetworkAclEntry
    -- , ''DeleteNetworkAclEntryResponse
    -- , ''DeleteNetworkInterface
    -- , ''DeleteNetworkInterfaceResponse
    -- , ''DeletePlacementGroup
    -- , ''DeletePlacementGroupResponse
    -- , ''DeleteRoute
    -- , ''DeleteRouteResponse
    -- , ''DeleteRouteTable
    -- , ''DeleteRouteTableResponse
    , ''DeleteSecurityGroup
    , ''DeleteSecurityGroupResponse
    -- , ''DeleteSnapshot
    -- , ''DeleteSnapshotResponse
    -- , ''DeleteSpotDatafeedSubscription
    -- , ''DeleteSpotDatafeedSubscriptionResponse
    -- , ''DeleteSubnet
    -- , ''DeleteSubnetResponse
    -- , ''DeleteTags
    -- , ''DeleteTagsResponse
    -- , ''DeleteVolume
    -- , ''DeleteVolumeResponse
    -- , ''DeleteVpc
    -- , ''DeleteVpcResponse
    -- , ''DeleteVpnConnection
    -- , ''DeleteVpnConnectionResponse
    -- , ''DeleteVpnConnectionRoute
    -- , ''DeleteVpnConnectionRouteResponse
    -- , ''DeleteVpnGateway
    -- , ''DeleteVpnGatewayResponse
    -- , ''DeregisterImage
    -- , ''DeregisterImageResponse
    -- , ''DescribeAccountAttributes
    -- , ''DescribeAccountAttributesResponse
    -- , ''DescribeAddresses
    -- , ''DescribeAddressesResponse
    -- , ''DescribeAvailabilityZones
    -- , ''DescribeAvailabilityZonesResponse
    -- , ''DescribeBundleTasks
    -- , ''DescribeBundleTasksResponse
    -- , ''DescribeConversionTasks
    -- , ''DescribeConversionTasksResponse
    -- , ''DescribeCustomerGateways
    -- , ''DescribeCustomerGatewaysResponse
    -- , ''DescribeDhcpOptions
    -- , ''DescribeDhcpOptionsResponse
    -- , ''DescribeExportTasks
    -- , ''DescribeExportTasksResponse
    -- , ''DescribeImageAttribute
    -- , ''DescribeImageAttributeResponse
    , ''DescribeImages
    , ''DescribeImagesResponse
    -- , ''DescribeInstanceAttribute
    -- , ''DescribeInstanceAttributeResponse
    , ''DescribeInstances
    , ''DescribeInstancesResponse
    -- , ''DescribeInstanceStatus
    -- , ''DescribeInstanceStatusResponse
    -- , ''DescribeInternetGateways
    -- , ''DescribeInternetGatewaysResponse
    -- , ''DescribeKeyPairs
    -- , ''DescribeKeyPairsResponse
    -- , ''DescribeNetworkAcls
    -- , ''DescribeNetworkAclsResponse
    -- , ''DescribeNetworkInterfaceAttribute
    -- , ''DescribeNetworkInterfaceAttributeResponse
    -- , ''DescribeNetworkInterfaces
    -- , ''DescribeNetworkInterfacesResponse
    -- , ''DescribePlacementGroups
    -- , ''DescribePlacementGroupsResponse
    -- , ''DescribeRegions
    -- , ''DescribeRegionsResponse
    -- , ''DescribeReservedInstances
    -- , ''DescribeReservedInstancesResponse
    -- , ''DescribeReservedInstancesListings
    -- , ''DescribeReservedInstancesListingsResponse
    -- , ''DescribeReservedInstancesOfferings
    -- , ''DescribeReservedInstancesOfferingsResponse
    -- , ''DescribeRouteTables
    -- , ''DescribeRouteTablesResponse
    , ''DescribeSecurityGroups
    , ''DescribeSecurityGroupsResponse
    -- , ''DescribeSnapshotAttribute
    -- , ''DescribeSnapshotAttributeResponse
    -- , ''DescribeSnapshots
    -- , ''DescribeSnapshotsResponse
    -- , ''DescribeSpotDatafeedSubscription
    -- , ''DescribeSpotDatafeedSubscriptionResponse
    -- , ''DescribeSpotInstanceRequests
    -- , ''DescribeSpotInstanceRequestsResponse
    -- , ''DescribeSpotPriceHistory
    -- , ''DescribeSpotPriceHistoryResponse
    -- , ''DescribeSubnets
    -- , ''DescribeSubnetsResponse
    , ''DescribeTags
    , ''DescribeTagsResponse
    -- , ''DescribeVolumeAttribute
    -- , ''DescribeVolumeAttributeResponse
    -- , ''DescribeVolumes
    -- , ''DescribeVolumesResponse
    -- , ''DescribeVolumeStatus
    -- , ''DescribeVolumeStatusResponse
    -- , ''DescribeVpcAttribute
    -- , ''DescribeVpcAttributeResponse
    -- , ''DescribeVpcs
    -- , ''DescribeVpcsResponse
    -- , ''DescribeVpnConnections
    -- , ''DescribeVpnConnectionsResponse
    -- , ''DescribeVpnGateways
    -- , ''DescribeVpnGatewaysResponse
    -- , ''DetachInternetGateway
    -- , ''DetachInternetGatewayResponse
    -- , ''DetachNetworkInterface
    -- , ''DetachNetworkInterfaceResponse
    -- , ''DetachVolume
    -- , ''DetachVolumeResponse
    -- , ''DetachVpnGateway
    -- , ''DetachVpnGatewayResponse
    -- , ''DisableVgwRoutePropagation
    -- , ''DisableVgwRoutePropagationResponse
    -- , ''DisassociateAddress
    -- , ''DisassociateAddressResponse
    -- , ''DisassociateRouteTable
    -- , ''DisassociateRouteTableResponse
    -- , ''EnableVgwRoutePropagation
    -- , ''EnableVgwRoutePropagationResponse
    -- , ''EnableVolumeIO
    -- , ''EnableVolumeIOResponse
    -- , ''GetConsoleOutput
    -- , ''GetConsoleOutputResponse
    -- , ''GetPasswordData
    -- , ''GetPasswordDataResponse
    -- , ''ImportInstance
    -- , ''ImportInstanceResponse
    -- , ''ImportKeyPair
    -- , ''ImportKeyPairResponse
    -- , ''ImportVolume
    -- , ''ImportVolumeResponse
    -- , ''ModifyImageAttribute
    -- , ''ModifyImageAttributeResponse
    -- , ''ModifyInstanceAttribute
    -- , ''ModifyInstanceAttributeResponse
    -- , ''ModifyNetworkInterfaceAttribute
    -- , ''ModifyNetworkInterfaceAttributeResponse
    -- , ''ModifySnapshotAttribute
    -- , ''ModifySnapshotAttributeResponse
    -- , ''ModifyVolumeAttribute
    -- , ''ModifyVolumeAttributeResponse
    -- , ''ModifyVpcAttribute
    -- , ''ModifyVpcAttributeResponse
    -- , ''MonitorInstances
    -- , ''MonitorInstancesResponse
    -- , ''PurchaseReservedInstancesOffering
    -- , ''PurchaseReservedInstancesOfferingResponse
    -- , ''RebootInstances
    -- , ''RebootInstancesResponse
    -- , ''RegisterImage
    -- , ''RegisterImageResponse
    -- , ''ReleaseAddress
    -- , ''ReleaseAddressResponse
    -- , ''ReplaceNetworkAclAssociation
    -- , ''ReplaceNetworkAclAssociationResponse
    -- , ''ReplaceNetworkAclEntry
    -- , ''ReplaceNetworkAclEntryResponse
    -- , ''ReplaceRoute
    -- , ''ReplaceRouteResponse
    -- , ''ReplaceRouteTableAssociation
    -- , ''ReplaceRouteTableAssociationResponse
    -- , ''ReportInstanceStatus
    -- , ''ReportInstanceStatusResponse
    -- , ''RequestSpotInstances
    -- , ''RequestSpotInstancesResponse
    -- , ''ResetImageAttribute
    -- , ''ResetImageAttributeResponse
    -- , ''ResetInstanceAttribute
    -- , ''ResetInstanceAttributeResponse
    -- , ''ResetNetworkInterfaceAttribute
    -- , ''ResetNetworkInterfaceAttributeResponse
    -- , ''ResetSnapshotAttribute
    -- , ''ResetSnapshotAttributeResponse
    , ''RevokeSecurityGroupEgress
    , ''RevokeSecurityGroupEgressResponse
    , ''RevokeSecurityGroupIngress
    , ''RevokeSecurityGroupIngressResponse
    -- , ''RunInstances
    -- , ''RunInstancesResponse
    -- , ''StartInstances
    -- , ''StartInstancesResponse
    -- , ''StopInstances
    -- , ''StopInstancesResponse
    -- , ''TerminateInstances
    -- , ''TerminateInstancesResponse
    -- , ''UnassignPrivateIpAddresses
    -- , ''UnassignPrivateIpAddressesResponse
    -- , ''UnmonitorInstances
    -- , ''UnmonitorInstancesResponse
    ])
