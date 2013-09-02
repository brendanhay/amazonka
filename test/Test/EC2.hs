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

import qualified Data.Text       as Text
import           Network.AWS.EC2
import           Test.Common

tests :: [Test]
tests = (:[]) $ testVersion ec2Version
    [ testGroup "Requests"
        [ testProperty "AllocateAddress"                     (prop :: Rq AllocateAddress)
        , testProperty "AssignPrivateIpAddresses"            (prop :: Rq AssignPrivateIpAddresses)
        , testProperty "AssociateAddress"                    (prop :: Rq AssociateAddress)
        , testProperty "AssociateDhcpOptions"                (prop :: Rq AssociateDhcpOptions)
        , testProperty "AssociateRouteTable"                 (prop :: Rq AssociateRouteTable)
        , testProperty "AttachInternetGateway"               (prop :: Rq AttachInternetGateway)
        , testProperty "AttachNetworkInterface"              (prop :: Rq AttachNetworkInterface)
        , testProperty "AttachVolume"                        (prop :: Rq AttachVolume)
        , testProperty "AttachVpnGateway"                    (prop :: Rq AttachVpnGateway)
        , testProperty "AuthorizeSecurityGroupEgress"        (prop :: Rq AuthorizeSecurityGroupEgress)
        , testProperty "AuthorizeSecurityGroupIngress"       (prop :: Rq AuthorizeSecurityGroupIngress)
        , testProperty "BundleInstance"                      (prop :: Rq BundleInstance)
        , testProperty "CancelBundleTask"                    (prop :: Rq CancelBundleTask)
        , testProperty "CancelConversionTask"                (prop :: Rq CancelConversionTask)
        , testProperty "CancelExportTask"                    (prop :: Rq CancelExportTask)
        , testProperty "CancelReservedInstancesListing"      (prop :: Rq CancelReservedInstancesListing)
        , testProperty "CancelSpotInstanceRequests"          (prop :: Rq CancelSpotInstanceRequests)
        -- , testProperty "ConfirmProductInstance"             (prop :: Rq ConfirmProductInstance)
        -- , testProperty "CopyImage"                          (prop :: Rq CopyImage)
        -- , testProperty "CopySnapshot"                       (prop :: Rq CopySnapshot)
        -- , testProperty "CreateCustomerGateway"              (prop :: Rq CreateCustomerGateway)
        -- , testProperty "CreateDhcpOptions"                  (prop :: Rq CreateDhcpOptions)
        -- , testProperty "CreateImage"                        (prop :: Rq CreateImage)
        -- , testProperty "CreateInstanceExportTask"           (prop :: Rq CreateInstanceExportTask)
        -- , testProperty "CreateInternetGateway"              (prop :: Rq CreateInternetGateway)
        -- , testProperty "CreateKeyPair"                      (prop :: Rq CreateKeyPair)
        -- , testProperty "CreateNetworkAcl"                   (prop :: Rq CreateNetworkAcl)
        -- , testProperty "CreateNetworkAclEntry"              (prop :: Rq CreateNetworkAclEntry)
        -- , testProperty "CreateNetworkInterface"             (prop :: Rq CreateNetworkInterface)
        -- , testProperty "CreatePlacementGroup"               (prop :: Rq CreatePlacementGroup)
        -- , testProperty "CreateReservedInstancesListing"     (prop :: Rq CreateReservedInstancesListing)
        -- , testProperty "CreateRoute"                        (prop :: Rq CreateRoute)
        -- , testProperty "CreateRouteTable"                   (prop :: Rq CreateRouteTable)
        -- , testProperty "CreateSecurityGroup"                (prop :: Rq CreateSecurityGroup)
        -- , testProperty "CreateSnapshot"                     (prop :: Rq CreateSnapshot)
        -- , testProperty "CreateSpotDatafeedSubscription"     (prop :: Rq CreateSpotDatafeedSubscription)
        -- , testProperty "CreateSubnet"                       (prop :: Rq CreateSubnet)
        -- , testProperty "CreateTags"                         (prop :: Rq CreateTags)
        -- , testProperty "CreateVolume"                       (prop :: Rq CreateVolume)
        -- , testProperty "CreateVpc"                          (prop :: Rq CreateVpc)
        -- , testProperty "CreateVpnConnection"                (prop :: Rq CreateVpnConnection)
        -- , testProperty "CreateVpnConnectionRoute"           (prop :: Rq CreateVpnConnectionRoute)
        -- , testProperty "CreateVpnGateway"                   (prop :: Rq CreateVpnGateway)
        -- , testProperty "DeleteCustomerGateway"              (prop :: Rq DeleteCustomerGateway)
        -- , testProperty "DeleteDhcpOptions"                  (prop :: Rq DeleteDhcpOptions)
        -- , testProperty "DeleteInternetGateway"              (prop :: Rq DeleteInternetGateway)
        -- , testProperty "DeleteKeyPair"                      (prop :: Rq DeleteKeyPair)
        -- , testProperty "DeleteNetworkAcl"                   (prop :: Rq DeleteNetworkAcl)
        -- , testProperty "DeleteNetworkAclEntry"              (prop :: Rq DeleteNetworkAclEntry)
        -- , testProperty "DeleteNetworkInterface"             (prop :: Rq DeleteNetworkInterface)
        -- , testProperty "DeletePlacementGroup"               (prop :: Rq DeletePlacementGroup)
        -- , testProperty "DeleteRoute"                        (prop :: Rq DeleteRoute)
        -- , testProperty "DeleteRouteTable"                   (prop :: Rq DeleteRouteTable)
        -- , testProperty "DeleteSecurityGroup"                (prop :: Rq DeleteSecurityGroup)
        -- , testProperty "DeleteSnapshot"                     (prop :: Rq DeleteSnapshot)
        -- , testProperty "DeleteSpotDatafeedSubscription"     (prop :: Rq DeleteSpotDatafeedSubscription)
        -- , testProperty "DeleteSubnet"                       (prop :: Rq DeleteSubnet)
        -- , testProperty "DeleteTags"                         (prop :: Rq DeleteTags)
        -- , testProperty "DeleteVolume"                       (prop :: Rq DeleteVolume)
        -- , testProperty "DeleteVpc"                          (prop :: Rq DeleteVpc)
        -- , testProperty "DeleteVpnConnection"                (prop :: Rq DeleteVpnConnection)
        -- , testProperty "DeleteVpnConnectionRoute"           (prop :: Rq DeleteVpnConnectionRoute)
        -- , testProperty "DeleteVpnGateway"                   (prop :: Rq DeleteVpnGateway)
        -- , testProperty "DeregisterImage"                    (prop :: Rq DeregisterImage)
        -- , testProperty "DescribeAccountAttributes"          (prop :: Rq DescribeAccountAttributes)
        -- , testProperty "DescribeAddresses"                  (prop :: Rq DescribeAddresses)
        -- , testProperty "DescribeAvailabilityZones"          (prop :: Rq DescribeAvailabilityZones)
        -- , testProperty "DescribeBundleTasks"                (prop :: Rq DescribeBundleTasks)
        -- , testProperty "DescribeConversionTasks"            (prop :: Rq DescribeConversionTasks)
        -- , testProperty "DescribeCustomerGateways"           (prop :: Rq DescribeCustomerGateways)
        -- , testProperty "DescribeDhcpOptions"                (prop :: Rq DescribeDhcpOptions)
        -- , testProperty "DescribeExportTasks"                (prop :: Rq DescribeExportTasks)
        -- , testProperty "DescribeImageAttribute"             (prop :: Rq DescribeImageAttribute)
        -- , testProperty "DescribeImages"                     (prop :: Rq DescribeImages)
        -- , testProperty "DescribeInstanceAttribute"          (prop :: Rq DescribeInstanceAttribute)
        -- , testProperty "DescribeInstances"                  (prop :: Rq DescribeInstances)
        -- , testProperty "DescribeInstanceStatus"             (prop :: Rq DescribeInstanceStatus)
        -- , testProperty "DescribeInternetGateways"           (prop :: Rq DescribeInternetGateways)
        -- , testProperty "DescribeKeyPairs"                   (prop :: Rq DescribeKeyPairs)
        -- , testProperty "DescribeNetworkAcls"                (prop :: Rq DescribeNetworkAcls)
        -- , testProperty "DescribeNetworkInterfaceAttribute"  (prop :: Rq DescribeNetworkInterfaceAttribute)
        -- , testProperty "DescribeNetworkInterfaces"          (prop :: Rq DescribeNetworkInterfaces)
        -- , testProperty "DescribePlacementGroups"            (prop :: Rq DescribePlacementGroups)
        -- , testProperty "DescribeRegions"                    (prop :: Rq DescribeRegions)
        -- , testProperty "DescribeReservedInstances"          (prop :: Rq DescribeReservedInstances)
        -- , testProperty "DescribeReservedInstancesListings"  (prop :: Rq DescribeReservedInstancesListings)
        -- , testProperty "DescribeReservedInstancesOfferings" (prop :: Rq DescribeReservedInstancesOfferings)
        -- , testProperty "DescribeRouteTables"                (prop :: Rq DescribeRouteTables)
        -- , testProperty "DescribeSecurityGroups"             (prop :: Rq DescribeSecurityGroups)
        -- , testProperty "DescribeSnapshotAttribute"          (prop :: Rq DescribeSnapshotAttribute)
        -- , testProperty "DescribeSnapshots"                  (prop :: Rq DescribeSnapshots)
        -- , testProperty "DescribeSpotDatafeedSubscription"   (prop :: Rq DescribeSpotDatafeedSubscription)
        -- , testProperty "DescribeSpotInstanceRequests"       (prop :: Rq DescribeSpotInstanceRequests)
        -- , testProperty "DescribeSpotPriceHistory"           (prop :: Rq DescribeSpotPriceHistory)
        -- , testProperty "DescribeSubnets"                    (prop :: Rq DescribeSubnets)
        -- , testProperty "DescribeTags"                       (prop :: Rq DescribeTags)
        -- , testProperty "DescribeVolumeAttribute"            (prop :: Rq DescribeVolumeAttribute)
        -- , testProperty "DescribeVolumes"                    (prop :: Rq DescribeVolumes)
        -- , testProperty "DescribeVolumeStatus"               (prop :: Rq DescribeVolumeStatus)
        -- , testProperty "DescribeVpcAttribute"               (prop :: Rq DescribeVpcAttribute)
        -- , testProperty "DescribeVpcs"                       (prop :: Rq DescribeVpcs)
        -- , testProperty "DescribeVpnConnections"             (prop :: Rq DescribeVpnConnections)
        -- , testProperty "DescribeVpnGateways"                (prop :: Rq DescribeVpnGateways)
        -- , testProperty "DetachInternetGateway"              (prop :: Rq DetachInternetGateway)
        -- , testProperty "DetachNetworkInterface"             (prop :: Rq DetachNetworkInterface)
        -- , testProperty "DetachVolume"                       (prop :: Rq DetachVolume)
        -- , testProperty "DetachVpnGateway"                   (prop :: Rq DetachVpnGateway)
        -- , testProperty "DisableVgwRoutePropagation"         (prop :: Rq DisableVgwRoutePropagation)
        -- , testProperty "DisassociateAddress"                (prop :: Rq DisassociateAddress)
        -- , testProperty "DisassociateRouteTable"             (prop :: Rq DisassociateRouteTable)
        -- , testProperty "EnableVgwRoutePropagation"          (prop :: Rq EnableVgwRoutePropagation)
        -- , testProperty "EnableVolumeIO"                     (prop :: Rq EnableVolumeIO)
        -- , testProperty "GetConsoleOutput"                   (prop :: Rq GetConsoleOutput)
        -- , testProperty "GetPasswordData"                    (prop :: Rq GetPasswordData)
        -- , testProperty "ImportInstance"                     (prop :: Rq ImportInstance)
        -- , testProperty "ImportKeyPair"                      (prop :: Rq ImportKeyPair)
        -- , testProperty "ImportVolume"                       (prop :: Rq ImportVolume)
        -- , testProperty "ModifyImageAttribute"               (prop :: Rq ModifyImageAttribute)
        -- , testProperty "ModifyInstanceAttribute"            (prop :: Rq ModifyInstanceAttribute)
        -- , testProperty "ModifyNetworkInterfaceAttribute"    (prop :: Rq ModifyNetworkInterfaceAttribute)
        -- , testProperty "ModifySnapshotAttribute"            (prop :: Rq ModifySnapshotAttribute)
        -- , testProperty "ModifyVolumeAttribute"              (prop :: Rq ModifyVolumeAttribute)
        -- , testProperty "ModifyVpcAttribute"                 (prop :: Rq ModifyVpcAttribute)
        -- , testProperty "MonitorInstances"                   (prop :: Rq MonitorInstances)
        -- , testProperty "PurchaseReservedInstancesOffering"  (prop :: Rq PurchaseReservedInstancesOffering)
        -- , testProperty "RebootInstances"                    (prop :: Rq RebootInstances)
        -- , testProperty "RegisterImage"                      (prop :: Rq RegisterImage)
        -- , testProperty "ReleaseAddress"                     (prop :: Rq ReleaseAddress)
        -- , testProperty "ReplaceNetworkAclAssociation"       (prop :: Rq ReplaceNetworkAclAssociation)
        -- , testProperty "ReplaceNetworkAclEntry"             (prop :: Rq ReplaceNetworkAclEntry)
        -- , testProperty "ReplaceRoute"                       (prop :: Rq ReplaceRoute)
        -- , testProperty "ReplaceRouteTableAssociation"       (prop :: Rq ReplaceRouteTableAssociation)
        -- , testProperty "ReportInstanceStatus"               (prop :: Rq ReportInstanceStatus)
        -- , testProperty "RequestSpotInstances"               (prop :: Rq RequestSpotInstances)
        -- , testProperty "ResetImageAttribute"                (prop :: Rq ResetImageAttribute)
        -- , testProperty "ResetInstanceAttribute"             (prop :: Rq ResetInstanceAttribute)
        -- , testProperty "ResetNetworkInterfaceAttribute"     (prop :: Rq ResetNetworkInterfaceAttribute)
        -- , testProperty "ResetSnapshotAttribute"             (prop :: Rq ResetSnapshotAttribute)
        -- , testProperty "RevokeSecurityGroupEgress"          (prop :: Rq RevokeSecurityGroupEgress)
        -- , testProperty "RevokeSecurityGroupIngress"         (prop :: Rq RevokeSecurityGroupIngress)
        -- , testProperty "RunInstances"                       (prop :: Rq RunInstances)
        -- , testProperty "StartInstances"                     (prop :: Rq StartInstances)
        -- , testProperty "StopInstances"                      (prop :: Rq StopInstances)
        -- , testProperty "TerminateInstances"                 (prop :: Rq TerminateInstances)
        -- , testProperty "UnassignPrivateIpAddresses"         (prop :: Rq UnassignPrivateIpAddresses)
        -- , testProperty "UnmonitorInstances"                 (prop :: Rq UnmonitorInstances)
      ]

    , testGroup "Responses"
        [ testProperty "AllocateAddressResponse"                (prop :: Rs AllocateAddressResponse)
        , testProperty "AssignPrivateIpAddressesResponse"       (prop :: Rs AssignPrivateIpAddressesResponse)
        , testProperty "AssociateAddressResponse"               (prop :: Rs AssociateAddressResponse)
        , testProperty "AssociateDhcpOptionsResponse"           (prop :: Rs AssociateDhcpOptionsResponse)
        , testProperty "AssociateRouteTableResponse"            (prop :: Rs AssociateRouteTableResponse)
        , testProperty "AttachInternetGatewayResponse"          (prop :: Rs AttachInternetGatewayResponse)
        , testProperty "AttachNetworkInterfaceResponse"         (prop :: Rs AttachNetworkInterfaceResponse)
        , testProperty "AttachVolumeResponse"                   (prop :: Rs AttachVolumeResponse)
        , testProperty "AttachVpnGatewayResponse"               (prop :: Rs AttachVpnGatewayResponse)
        , testProperty "AuthorizeSecurityGroupEgressResponse"   (prop :: Rs AuthorizeSecurityGroupEgressResponse)
        , testProperty "AuthorizeSecurityGroupIngressResponse"  (prop :: Rs AuthorizeSecurityGroupIngressResponse)
        , testProperty "BundleInstanceResponse"                 (prop :: Rs BundleInstanceResponse)
        , testProperty "CancelBundleTaskResponse"               (prop :: Rs CancelBundleTaskResponse)
        , testProperty "CancelConversionTaskResponse"           (prop :: Rs CancelConversionTaskResponse)
        , testProperty "CancelExportTaskResponse"               (prop :: Rs CancelExportTaskResponse)
        , testProperty "CancelReservedInstancesListingResponse" (prop :: Rs CancelReservedInstancesListingResponse)
        , testProperty "CancelSpotInstanceRequestsResponse"     (prop :: Rs CancelSpotInstanceRequestsResponse)
        -- , testProperty "ConfirmProductInstanceResponse"             (prop :: Rs ConfirmProductInstanceResponse)
        -- , testProperty "CopyImageResponse"                          (prop :: Rs CopyImageResponse)
        -- , testProperty "CopySnapshotResponse"                       (prop :: Rs CopySnapshotResponse)
        -- , testProperty "CreateCustomerGatewayResponse"              (prop :: Rs CreateCustomerGatewayResponse)
        -- , testProperty "CreateDhcpOptionsResponse"                  (prop :: Rs CreateDhcpOptionsResponse)
        -- , testProperty "CreateImageResponse"                        (prop :: Rs CreateImageResponse)
        -- , testProperty "CreateInstanceExportTaskResponse"           (prop :: Rs CreateInstanceExportTaskResponse)
        -- , testProperty "CreateInternetGatewayResponse"              (prop :: Rs CreateInternetGatewayResponse)
        -- , testProperty "CreateKeyPairResponse"                      (prop :: Rs CreateKeyPairResponse)
        -- , testProperty "CreateNetworkAclResponse"                   (prop :: Rs CreateNetworkAclResponse)
        -- , testProperty "CreateNetworkAclEntryResponse"              (prop :: Rs CreateNetworkAclEntryResponse)
        -- , testProperty "CreateNetworkInterfaceResponse"             (prop :: Rs CreateNetworkInterfaceResponse)
        -- , testProperty "CreatePlacementGroupResponse"               (prop :: Rs CreatePlacementGroupResponse)
        -- , testProperty "CreateReservedInstancesListingResponse"     (prop :: Rs CreateReservedInstancesListingResponse)
        -- , testProperty "CreateRouteResponse"                        (prop :: Rs CreateRouteResponse)
        -- , testProperty "CreateRouteTableResponse"                   (prop :: Rs CreateRouteTableResponse)
        -- , testProperty "CreateSecurityGroupResponse"                (prop :: Rs CreateSecurityGroupResponse)
        -- , testProperty "CreateSnapshotResponse"                     (prop :: Rs CreateSnapshotResponse)
        -- , testProperty "CreateSpotDatafeedSubscriptionResponse"     (prop :: Rs CreateSpotDatafeedSubscriptionResponse)
        -- , testProperty "CreateSubnetResponse"                       (prop :: Rs CreateSubnetResponse)
        -- , testProperty "CreateTagsResponse"                         (prop :: Rs CreateTagsResponse)
        -- , testProperty "CreateVolumeResponse"                       (prop :: Rs CreateVolumeResponse)
        -- , testProperty "CreateVpcResponse"                          (prop :: Rs CreateVpcResponse)
        -- , testProperty "CreateVpnConnectionResponse"                (prop :: Rs CreateVpnConnectionResponse)
        -- , testProperty "CreateVpnConnectionRouteResponse"           (prop :: Rs CreateVpnConnectionRouteResponse)
        -- , testProperty "CreateVpnGatewayResponse"                   (prop :: Rs CreateVpnGatewayResponse)
        -- , testProperty "DeleteCustomerGatewayResponse"              (prop :: Rs DeleteCustomerGatewayResponse)
        -- , testProperty "DeleteDhcpOptionsResponse"                  (prop :: Rs DeleteDhcpOptionsResponse)
        -- , testProperty "DeleteInternetGatewayResponse"              (prop :: Rs DeleteInternetGatewayResponse)
        -- , testProperty "DeleteKeyPairResponse"                      (prop :: Rs DeleteKeyPairResponse)
        -- , testProperty "DeleteNetworkAclResponse"                   (prop :: Rs DeleteNetworkAclResponse)
        -- , testProperty "DeleteNetworkAclEntryResponse"              (prop :: Rs DeleteNetworkAclEntryResponse)
        -- , testProperty "DeleteNetworkInterfaceResponse"             (prop :: Rs DeleteNetworkInterfaceResponse)
        -- , testProperty "DeletePlacementGroupResponse"               (prop :: Rs DeletePlacementGroupResponse)
        -- , testProperty "DeleteRouteResponse"                        (prop :: Rs DeleteRouteResponse)
        -- , testProperty "DeleteRouteTableResponse"                   (prop :: Rs DeleteRouteTableResponse)
        -- , testProperty "DeleteSecurityGroupResponse"                (prop :: Rs DeleteSecurityGroupResponse)
        -- , testProperty "DeleteSnapshotResponse"                     (prop :: Rs DeleteSnapshotResponse)
        -- , testProperty "DeleteSpotDatafeedSubscriptionResponse"     (prop :: Rs DeleteSpotDatafeedSubscriptionResponse)
        -- , testProperty "DeleteSubnetResponse"                       (prop :: Rs DeleteSubnetResponse)
        -- , testProperty "DeleteTagsResponse"                         (prop :: Rs DeleteTagsResponse)
        -- , testProperty "DeleteVolumeResponse"                       (prop :: Rs DeleteVolumeResponse)
        -- , testProperty "DeleteVpcResponse"                          (prop :: Rs DeleteVpcResponse)
        -- , testProperty "DeleteVpnConnectionResponse"                (prop :: Rs DeleteVpnConnectionResponse)
        -- , testProperty "DeleteVpnConnectionRouteResponse"           (prop :: Rs DeleteVpnConnectionRouteResponse)
        -- , testProperty "DeleteVpnGatewayResponse"                   (prop :: Rs DeleteVpnGatewayResponse)
        -- , testProperty "DeregisterImageResponse"                    (prop :: Rs DeregisterImageResponse)
        -- , testProperty "DescribeAccountAttributesResponse"          (prop :: Rs DescribeAccountAttributesResponse)
        -- , testProperty "DescribeAddressesResponse"                  (prop :: Rs DescribeAddressesResponse)
        -- , testProperty "DescribeAvailabilityZonesResponse"          (prop :: Rs DescribeAvailabilityZonesResponse)
        -- , testProperty "DescribeBundleTasksResponse"                (prop :: Rs DescribeBundleTasksResponse)
        -- , testProperty "DescribeConversionTasksResponse"            (prop :: Rs DescribeConversionTasksResponse)
        -- , testProperty "DescribeCustomerGatewaysResponse"           (prop :: Rs DescribeCustomerGatewaysResponse)
        -- , testProperty "DescribeDhcpOptionsResponse"                (prop :: Rs DescribeDhcpOptionsResponse)
        -- , testProperty "DescribeExportTasksResponse"                (prop :: Rs DescribeExportTasksResponse)
        -- , testProperty "DescribeImageAttributeResponse"             (prop :: Rs DescribeImageAttributeResponse)
        -- , testProperty "DescribeImagesResponse"                     (prop :: Rs DescribeImagesResponse)
        -- , testProperty "DescribeInstanceAttributeResponse"          (prop :: Rs DescribeInstanceAttributeResponse)
        -- , testProperty "DescribeInstancesResponse"                  (prop :: Rs DescribeInstancesResponse)
        -- , testProperty "DescribeInstanceStatusResponse"             (prop :: Rs DescribeInstanceStatusResponse)
        -- , testProperty "DescribeInternetGatewaysResponse"           (prop :: Rs DescribeInternetGatewaysResponse)
        -- , testProperty "DescribeKeyPairsResponse"                   (prop :: Rs DescribeKeyPairsResponse)
        -- , testProperty "DescribeNetworkAclsResponse"                (prop :: Rs DescribeNetworkAclsResponse)
        -- , testProperty "DescribeNetworkInterfaceAttributeResponse"  (prop :: Rs DescribeNetworkInterfaceAttributeResponse)
        -- , testProperty "DescribeNetworkInterfacesResponse"          (prop :: Rs DescribeNetworkInterfacesResponse)
        -- , testProperty "DescribePlacementGroupsResponse"            (prop :: Rs DescribePlacementGroupsResponse)
        -- , testProperty "DescribeRegionsResponse"                    (prop :: Rs DescribeRegionsResponse)
        -- , testProperty "DescribeReservedInstancesResponse"          (prop :: Rs DescribeReservedInstancesResponse)
        -- , testProperty "DescribeReservedInstancesListingsResponse"  (prop :: Rs DescribeReservedInstancesListingsResponse)
        -- , testProperty "DescribeReservedInstancesOfferingsResponse" (prop :: Rs DescribeReservedInstancesOfferingsResponse)
        -- , testProperty "DescribeRouteTablesResponse"                (prop :: Rs DescribeRouteTablesResponse)
        -- , testProperty "DescribeSecurityGroupsResponse"             (prop :: Rs DescribeSecurityGroupsResponse)
        -- , testProperty "DescribeSnapshotAttributeResponse"          (prop :: Rs DescribeSnapshotAttributeResponse)
        -- , testProperty "DescribeSnapshotsResponse"                  (prop :: Rs DescribeSnapshotsResponse)
        -- , testProperty "DescribeSpotDatafeedSubscriptionResponse"   (prop :: Rs DescribeSpotDatafeedSubscriptionResponse)
        -- , testProperty "DescribeSpotInstanceRequestsResponse"       (prop :: Rs DescribeSpotInstanceRequestsResponse)
        -- , testProperty "DescribeSpotPriceHistoryResponse"           (prop :: Rs DescribeSpotPriceHistoryResponse)
        -- , testProperty "DescribeSubnetsResponse"                    (prop :: Rs DescribeSubnetsResponse)
        -- , testProperty "DescribeTagsResponse"                       (prop :: Rs DescribeTagsResponse)
        -- , testProperty "DescribeVolumeAttributeResponse"            (prop :: Rs DescribeVolumeAttributeResponse)
        -- , testProperty "DescribeVolumesResponse"                    (prop :: Rs DescribeVolumesResponse)
        -- , testProperty "DescribeVolumeStatusResponse"               (prop :: Rs DescribeVolumeStatusResponse)
        -- , testProperty "DescribeVpcAttributeResponse"               (prop :: Rs DescribeVpcAttributeResponse)
        -- , testProperty "DescribeVpcsResponse"                       (prop :: Rs DescribeVpcsResponse)
        -- , testProperty "DescribeVpnConnectionsResponse"             (prop :: Rs DescribeVpnConnectionsResponse)
        -- , testProperty "DescribeVpnGatewaysResponse"                (prop :: Rs DescribeVpnGatewaysResponse)
        -- , testProperty "DetachInternetGatewayResponse"              (prop :: Rs DetachInternetGatewayResponse)
        -- , testProperty "DetachNetworkInterfaceResponse"             (prop :: Rs DetachNetworkInterfaceResponse)
        -- , testProperty "DetachVolumeResponse"                       (prop :: Rs DetachVolumeResponse)
        -- , testProperty "DetachVpnGatewayResponse"                   (prop :: Rs DetachVpnGatewayResponse)
        -- , testProperty "DisableVgwRoutePropagationResponse"         (prop :: Rs DisableVgwRoutePropagationResponse)
        -- , testProperty "DisassociateAddressResponse"                (prop :: Rs DisassociateAddressResponse)
        -- , testProperty "DisassociateRouteTableResponse"             (prop :: Rs DisassociateRouteTableResponse)
        -- , testProperty "EnableVgwRoutePropagationResponse"          (prop :: Rs EnableVgwRoutePropagationResponse)
        -- , testProperty "EnableVolumeIOResponse"                     (prop :: Rs EnableVolumeIOResponse)
        -- , testProperty "GetConsoleOutputResponse"                   (prop :: Rs GetConsoleOutputResponse)
        -- , testProperty "GetPasswordDataResponse"                    (prop :: Rs GetPasswordDataResponse)
        -- , testProperty "ImportInstanceResponse"                     (prop :: Rs ImportInstanceResponse)
        -- , testProperty "ImportKeyPairResponse"                      (prop :: Rs ImportKeyPairResponse)
        -- , testProperty "ImportVolumeResponse"                       (prop :: Rs ImportVolumeResponse)
        -- , testProperty "ModifyImageAttributeResponse"               (prop :: Rs ModifyImageAttributeResponse)
        -- , testProperty "ModifyInstanceAttributeResponse"            (prop :: Rs ModifyInstanceAttributeResponse)
        -- , testProperty "ModifyNetworkInterfaceAttributeResponse"    (prop :: Rs ModifyNetworkInterfaceAttributeResponse)
        -- , testProperty "ModifySnapshotAttributeResponse"            (prop :: Rs ModifySnapshotAttributeResponse)
        -- , testProperty "ModifyVolumeAttributeResponse"              (prop :: Rs ModifyVolumeAttributeResponse)
        -- , testProperty "ModifyVpcAttributeResponse"                 (prop :: Rs ModifyVpcAttributeResponse)
        -- , testProperty "MonitorInstancesResponse"                   (prop :: Rs MonitorInstancesResponse)
        -- , testProperty "PurchaseReservedInstancesOfferingResponse"  (prop :: Rs PurchaseReservedInstancesOfferingResponse)
        -- , testProperty "RebootInstancesResponse"                    (prop :: Rs RebootInstancesResponse)
        -- , testProperty "RegisterImageResponse"                      (prop :: Rs RegisterImageResponse)
        -- , testProperty "ReleaseAddressResponse"                     (prop :: Rs ReleaseAddressResponse)
        -- , testProperty "ReplaceNetworkAclAssociationResponse"       (prop :: Rs ReplaceNetworkAclAssociationResponse)
        -- , testProperty "ReplaceNetworkAclEntryResponse"             (prop :: Rs ReplaceNetworkAclEntryResponse)
        -- , testProperty "ReplaceRouteResponse"                       (prop :: Rs ReplaceRouteResponse)
        -- , testProperty "ReplaceRouteTableAssociationResponse"       (prop :: Rs ReplaceRouteTableAssociationResponse)
        -- , testProperty "ReportInstanceStatusResponse"               (prop :: Rs ReportInstanceStatusResponse)
        -- , testProperty "RequestSpotInstancesResponse"               (prop :: Rs RequestSpotInstancesResponse)
        -- , testProperty "ResetImageAttributeResponse"                (prop :: Rs ResetImageAttributeResponse)
        -- , testProperty "ResetInstanceAttributeResponse"             (prop :: Rs ResetInstanceAttributeResponse)
        -- , testProperty "ResetNetworkInterfaceAttributeResponse"     (prop :: Rs ResetNetworkInterfaceAttributeResponse)
        -- , testProperty "ResetSnapshotAttributeResponse"             (prop :: Rs ResetSnapshotAttributeResponse)
        -- , testProperty "RevokeSecurityGroupEgressResponse"          (prop :: Rs RevokeSecurityGroupEgressResponse)
        -- , testProperty "RevokeSecurityGroupIngressResponse"         (prop :: Rs RevokeSecurityGroupIngressResponse)
        -- , testProperty "RunInstancesResponse"                       (prop :: Rs RunInstancesResponse)
        -- , testProperty "StartInstancesResponse"                     (prop :: Rs StartInstancesResponse)
        -- , testProperty "StopInstancesResponse"                      (prop :: Rs StopInstancesResponse)
        -- , testProperty "TerminateInstancesResponse"                 (prop :: Rs TerminateInstancesResponse)
        -- , testProperty "UnassignPrivateIpAddressesResponse"         (prop :: Rs UnassignPrivateIpAddressesResponse)
        -- , testProperty "UnmonitorInstancesResponse"                 (prop :: Rs UnmonitorInstancesResponse)
        ]
    ]

instance ToJSON AddressDomain where
    toJSON = String . Text.pack . show

instance ToJSON VolumeStatus where
    toJSON = String . Text.pack . show

instance ToJSON BundleInstanceState where
    toJSON = String . Text.pack . show

$(deriveArbitrary
   [ ''AddressDomain
   , ''VolumeStatus
   , ''BundleInstanceState
   ])

$(deriveDependency
    [ ''Attachment
    , ''IpPermission
    , ''IpRange
    , ''UserIdGroupPair
    , ''BundleInstanceS3Storage
    , ''BundleInstanceTaskStorage
    , ''BundleInstanceTaskError
    , ''BundleInstanceTask
    , ''DescribeReservedInstancesListingsResponseSetItemType
    , ''CancelSpotInstanceRequestsResponseSetItemType
    , ''ResourceTagSetItemType
    , ''PriceScheduleSetItemType
    , ''InstanceCountsSetItemType
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
    -- , ''CreateSecurityGroup
    -- , ''CreateSecurityGroupResponse
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
    -- , ''DeleteSecurityGroup
    -- , ''DeleteSecurityGroupResponse
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
    -- , ''DescribeImages
    -- , ''DescribeImagesResponse
    -- , ''DescribeInstanceAttribute
    -- , ''DescribeInstanceAttributeResponse
    -- , ''DescribeInstances
    -- , ''DescribeInstancesResponse
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
    -- , ''DescribeSecurityGroups
    -- , ''DescribeSecurityGroupsResponse
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
    -- , ''DescribeTags
    -- , ''DescribeTagsResponse
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
    -- , ''RevokeSecurityGroupEgress
    -- , ''RevokeSecurityGroupEgressResponse
    -- , ''RevokeSecurityGroupIngress
    -- , ''RevokeSecurityGroupIngressResponse
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
