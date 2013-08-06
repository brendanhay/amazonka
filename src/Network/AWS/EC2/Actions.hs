{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.EC2.Actions
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Actions where

--
-- Internal
--

data Action
    = ActivateLicense
--    | AllocateAddress
    | AssignPrivateIpAddresses
    | AssociateAddress
    | AssociateDHCPOptions
    | AssociateRouteTable
    | AttachInternetGateway
    | AttachNetworkInterface
    | AttachVolume
    | AttachVPNGateway
    | AuthorizeSecurityGroupEgress
    | AuthorizeSecurityGroupIngress
    | BundleInstance
    | CancelBundleTask
    | CancelConversionTask
    | CancelExportTask
    | CancelReservedInstancesListing
    | CancelSpotInstanceRequests
    | ConfirmProductInstance
    | CopyImage
    | CopySnapshot
    | CreateCustomerGateway
    | CreateDHCPOptions
    | CreateImage
    | CreateInstanceExportTask
    | CreateInternetGateway
    | CreateKeyPair
    | CreateNetworkAcl
    | CreateNetworkAclEntry
    | CreateNetworkInterface
    | CreatePlacementGroup
    | CreateReservedInstancesListing
    | CreateRoute
    | CreateRouteTable
    | CreateSecurityGroup
    | CreateSnapshot
    | CreateSpotDatafeedSubscription
    | CreateSubnet
    | CreateTags
    | CreateVolume
    | CreateVPC
    | CreateVPNConnection
    | CreateVPNConnectionRoute
    | CreateVPNGateway
    | DeactivateLicense
    | DeleteCustomerGateway
    | DeleteDHCPOptions
    | DeleteInternetGateway
    | DeleteKeyPair
    | DeleteNetworkAcl
    | DeleteNetworkAclEntry
    | DeleteNetworkInterface
    | DeletePlacementGroup
    | DeleteRoute
    | DeleteRouteTable
    | DeleteSecurityGroup
    | DeleteSnapshot
    | DeleteSpotDatafeedSubscription
    | DeleteSubnet
    | DeleteTags
    | DeleteVolume
    | DeleteVPC
    | DeleteVPNConnection
    | DeleteVPNConnectionRoute
    | DeleteVPNGateway
    | DeregisterImage
    | DescribeAccountAttributes
    | DescribeAddresses
    | DescribeAvailabilityZones
    | DescribeBundleTasks
    | DescribeConversionTasks
    | DescribeCustomerGateways
    | DescribeDHCPOptions
    | DescribeExportTasks
    | DescribeImageAttribute
    | DescribeImages
    | DescribeInstanceAttribute
--    | DescribeInstances
    | DescribeInstanceStatus
    | DescribeInternetGateways
    | DescribeKeyPairs
    | DescribeLicenses
    | DescribeNetworkAcls
    | DescribeNetworkInterfaceAttribute
    | DescribeNetworkInterfaces
    | DescribePlacementGroups
    | DescribeRegions
    | DescribeReservedInstances
    | DescribeReservedInstancesListings
    | DescribeReservedInstancesOfferings
    | DescribeRouteTables
    | DescribeSecurityGroups
    | DescribeSnapshotAttribute
    | DescribeSnapshots
    | DescribeSpotDatafeedSubscription
    | DescribeSpotInstanceRequests
    | DescribeSpotPriceHistory
    | DescribeSubnets
    | DescribeTags
    | DescribeVolumeAttribute
    | DescribeVolumes
    | DescribeVolumeStatus
    | DescribeVPCAttribute
    | DescribeVPCs
    | DescribeVPNConnections
    | DescribeVPNGateways
    | DetachInternetGateway
    | DetachNetworkInterface
    | DetachVolume
    | DetachVPNGateway
    | DisableVGWRoutePropagation
    | DisassociateAddress
    | DisassociateRouteTable
    | EnableVGWRoutePropagation
    | EnableVolumeIO
    | GetConsoleOutput
    | GetPasswordData
    | ImportInstance
    | ImportKeyPair
    | ImportVolume
    | ModifyImageAttribute
    | ModifyInstanceAttribute
    | ModifyNetworkInterfaceAttribute
    | ModifySnapshotAttribute
    | ModifyVolumeAttribute
    | ModifyVPCAttribute
    | MonitorInstances
    | PurchaseReservedInstancesOffering
    | RebootInstances
    | RegisterImage
    | ReleaseAddress
    | ReplaceNetworkAclAssociation
    | ReplaceNetworkAclEntry
    | ReplaceRoute
    | ReplaceRouteTableAssociation
    | ReportInstanceStatus
    | RequestSpotInstances
    | ResetImageAttribute
    | ResetInstanceAttribute
    | ResetNetworkInterfaceAttribute
    | ResetSnapshotAttribute
    | RevokeSecurityGroupEgress
    | RevokeSecurityGroupIngress
    | RunInstances
    | StartInstances
    | StopInstances
    | TerminateInstances
    | UnassignPrivateIpAddresses
    | UnmonitorInstances
