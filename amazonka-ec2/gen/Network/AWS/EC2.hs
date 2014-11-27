-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Elastic Compute Cloud (Amazon EC2) is a web service that provides
-- resizable compute capacity in the cloud. It is designed to make web-scale
-- computing easier for developers. Amazon EC2’s simple web service interface
-- allows you to obtain and configure capacity with minimal friction. It
-- provides you with complete control of your computing resources and lets you
-- run on Amazon’s proven computing environment. Amazon EC2 reduces the time
-- required to obtain and boot new server instances to minutes, allowing you to
-- quickly scale capacity, both up and down, as your computing requirements
-- change. Amazon EC2 changes the economics of computing by allowing you to pay
-- only for capacity that you actually use. Amazon EC2 provides developers the
-- tools to build failure resilient applications and isolate themselves from
-- common failure scenarios.
module Network.AWS.EC2
    ( module Network.AWS.EC2.AcceptVpcPeeringConnection
    , module Network.AWS.EC2.AllocateAddress
    , module Network.AWS.EC2.AssignPrivateIpAddresses
    , module Network.AWS.EC2.AssociateAddress
    , module Network.AWS.EC2.AssociateDhcpOptions
    , module Network.AWS.EC2.AssociateRouteTable
    , module Network.AWS.EC2.AttachInternetGateway
    , module Network.AWS.EC2.AttachNetworkInterface
    , module Network.AWS.EC2.AttachVolume
    , module Network.AWS.EC2.AttachVpnGateway
    , module Network.AWS.EC2.AuthorizeSecurityGroupEgress
    , module Network.AWS.EC2.AuthorizeSecurityGroupIngress
    , module Network.AWS.EC2.BundleInstance
    , module Network.AWS.EC2.CancelBundleTask
    , module Network.AWS.EC2.CancelConversionTask
    , module Network.AWS.EC2.CancelExportTask
    , module Network.AWS.EC2.CancelReservedInstancesListing
    , module Network.AWS.EC2.CancelSpotInstanceRequests
    , module Network.AWS.EC2.ConfirmProductInstance
    , module Network.AWS.EC2.CopyImage
    , module Network.AWS.EC2.CopySnapshot
    , module Network.AWS.EC2.CreateCustomerGateway
    , module Network.AWS.EC2.CreateDhcpOptions
    , module Network.AWS.EC2.CreateImage
    , module Network.AWS.EC2.CreateInstanceExportTask
    , module Network.AWS.EC2.CreateInternetGateway
    , module Network.AWS.EC2.CreateKeyPair
    , module Network.AWS.EC2.CreateNetworkAcl
    , module Network.AWS.EC2.CreateNetworkAclEntry
    , module Network.AWS.EC2.CreateNetworkInterface
    , module Network.AWS.EC2.CreatePlacementGroup
    , module Network.AWS.EC2.CreateReservedInstancesListing
    , module Network.AWS.EC2.CreateRoute
    , module Network.AWS.EC2.CreateRouteTable
    , module Network.AWS.EC2.CreateSecurityGroup
    , module Network.AWS.EC2.CreateSnapshot
    , module Network.AWS.EC2.CreateSpotDatafeedSubscription
    , module Network.AWS.EC2.CreateSubnet
    , module Network.AWS.EC2.CreateTags
    , module Network.AWS.EC2.CreateVolume
    , module Network.AWS.EC2.CreateVpc
    , module Network.AWS.EC2.CreateVpcPeeringConnection
    , module Network.AWS.EC2.CreateVpnConnection
    , module Network.AWS.EC2.CreateVpnConnectionRoute
    , module Network.AWS.EC2.CreateVpnGateway
    , module Network.AWS.EC2.DeleteCustomerGateway
    , module Network.AWS.EC2.DeleteDhcpOptions
    , module Network.AWS.EC2.DeleteInternetGateway
    , module Network.AWS.EC2.DeleteKeyPair
    , module Network.AWS.EC2.DeleteNetworkAcl
    , module Network.AWS.EC2.DeleteNetworkAclEntry
    , module Network.AWS.EC2.DeleteNetworkInterface
    , module Network.AWS.EC2.DeletePlacementGroup
    , module Network.AWS.EC2.DeleteRoute
    , module Network.AWS.EC2.DeleteRouteTable
    , module Network.AWS.EC2.DeleteSecurityGroup
    , module Network.AWS.EC2.DeleteSnapshot
    , module Network.AWS.EC2.DeleteSpotDatafeedSubscription
    , module Network.AWS.EC2.DeleteSubnet
    , module Network.AWS.EC2.DeleteTags
    , module Network.AWS.EC2.DeleteVolume
    , module Network.AWS.EC2.DeleteVpc
    , module Network.AWS.EC2.DeleteVpcPeeringConnection
    , module Network.AWS.EC2.DeleteVpnConnection
    , module Network.AWS.EC2.DeleteVpnConnectionRoute
    , module Network.AWS.EC2.DeleteVpnGateway
    , module Network.AWS.EC2.DeregisterImage
    , module Network.AWS.EC2.DescribeAccountAttributes
    , module Network.AWS.EC2.DescribeAddresses
    , module Network.AWS.EC2.DescribeAvailabilityZones
    , module Network.AWS.EC2.DescribeBundleTasks
    , module Network.AWS.EC2.DescribeConversionTasks
    , module Network.AWS.EC2.DescribeCustomerGateways
    , module Network.AWS.EC2.DescribeDhcpOptions
    , module Network.AWS.EC2.DescribeExportTasks
    , module Network.AWS.EC2.DescribeImageAttribute
    , module Network.AWS.EC2.DescribeImages
    , module Network.AWS.EC2.DescribeInstanceAttribute
    , module Network.AWS.EC2.DescribeInstanceStatus
    , module Network.AWS.EC2.DescribeInstances
    , module Network.AWS.EC2.DescribeInternetGateways
    , module Network.AWS.EC2.DescribeKeyPairs
    , module Network.AWS.EC2.DescribeNetworkAcls
    , module Network.AWS.EC2.DescribeNetworkInterfaceAttribute
    , module Network.AWS.EC2.DescribeNetworkInterfaces
    , module Network.AWS.EC2.DescribePlacementGroups
    , module Network.AWS.EC2.DescribeRegions
    , module Network.AWS.EC2.DescribeReservedInstances
    , module Network.AWS.EC2.DescribeReservedInstancesListings
    , module Network.AWS.EC2.DescribeReservedInstancesModifications
    , module Network.AWS.EC2.DescribeReservedInstancesOfferings
    , module Network.AWS.EC2.DescribeRouteTables
    , module Network.AWS.EC2.DescribeSecurityGroups
    , module Network.AWS.EC2.DescribeSnapshotAttribute
    , module Network.AWS.EC2.DescribeSnapshots
    , module Network.AWS.EC2.DescribeSpotDatafeedSubscription
    , module Network.AWS.EC2.DescribeSpotInstanceRequests
    , module Network.AWS.EC2.DescribeSpotPriceHistory
    , module Network.AWS.EC2.DescribeSubnets
    , module Network.AWS.EC2.DescribeTags
    , module Network.AWS.EC2.DescribeVolumeAttribute
    , module Network.AWS.EC2.DescribeVolumeStatus
    , module Network.AWS.EC2.DescribeVolumes
    , module Network.AWS.EC2.DescribeVpcAttribute
    , module Network.AWS.EC2.DescribeVpcPeeringConnections
    , module Network.AWS.EC2.DescribeVpcs
    , module Network.AWS.EC2.DescribeVpnConnections
    , module Network.AWS.EC2.DescribeVpnGateways
    , module Network.AWS.EC2.DetachInternetGateway
    , module Network.AWS.EC2.DetachNetworkInterface
    , module Network.AWS.EC2.DetachVolume
    , module Network.AWS.EC2.DetachVpnGateway
    , module Network.AWS.EC2.DisableVgwRoutePropagation
    , module Network.AWS.EC2.DisassociateAddress
    , module Network.AWS.EC2.DisassociateRouteTable
    , module Network.AWS.EC2.EnableVgwRoutePropagation
    , module Network.AWS.EC2.EnableVolumeIO
    , module Network.AWS.EC2.GetConsoleOutput
    , module Network.AWS.EC2.GetPasswordData
    , module Network.AWS.EC2.ImportInstance
    , module Network.AWS.EC2.ImportKeyPair
    , module Network.AWS.EC2.ImportVolume
    , module Network.AWS.EC2.ModifyImageAttribute
    , module Network.AWS.EC2.ModifyInstanceAttribute
    , module Network.AWS.EC2.ModifyNetworkInterfaceAttribute
    , module Network.AWS.EC2.ModifyReservedInstances
    , module Network.AWS.EC2.ModifySnapshotAttribute
    , module Network.AWS.EC2.ModifySubnetAttribute
    , module Network.AWS.EC2.ModifyVolumeAttribute
    , module Network.AWS.EC2.ModifyVpcAttribute
    , module Network.AWS.EC2.MonitorInstances
    , module Network.AWS.EC2.PurchaseReservedInstancesOffering
    , module Network.AWS.EC2.RebootInstances
    , module Network.AWS.EC2.RegisterImage
    , module Network.AWS.EC2.RejectVpcPeeringConnection
    , module Network.AWS.EC2.ReleaseAddress
    , module Network.AWS.EC2.ReplaceNetworkAclAssociation
    , module Network.AWS.EC2.ReplaceNetworkAclEntry
    , module Network.AWS.EC2.ReplaceRoute
    , module Network.AWS.EC2.ReplaceRouteTableAssociation
    , module Network.AWS.EC2.ReportInstanceStatus
    , module Network.AWS.EC2.RequestSpotInstances
    , module Network.AWS.EC2.ResetImageAttribute
    , module Network.AWS.EC2.ResetInstanceAttribute
    , module Network.AWS.EC2.ResetNetworkInterfaceAttribute
    , module Network.AWS.EC2.ResetSnapshotAttribute
    , module Network.AWS.EC2.RevokeSecurityGroupEgress
    , module Network.AWS.EC2.RevokeSecurityGroupIngress
    , module Network.AWS.EC2.RunInstances
    , module Network.AWS.EC2.StartInstances
    , module Network.AWS.EC2.StopInstances
    , module Network.AWS.EC2.TerminateInstances
    , module Network.AWS.EC2.Types
    , module Network.AWS.EC2.UnassignPrivateIpAddresses
    , module Network.AWS.EC2.UnmonitorInstances
    ) where

import Network.AWS.EC2.AcceptVpcPeeringConnection
import Network.AWS.EC2.AllocateAddress
import Network.AWS.EC2.AssignPrivateIpAddresses
import Network.AWS.EC2.AssociateAddress
import Network.AWS.EC2.AssociateDhcpOptions
import Network.AWS.EC2.AssociateRouteTable
import Network.AWS.EC2.AttachInternetGateway
import Network.AWS.EC2.AttachNetworkInterface
import Network.AWS.EC2.AttachVolume
import Network.AWS.EC2.AttachVpnGateway
import Network.AWS.EC2.AuthorizeSecurityGroupEgress
import Network.AWS.EC2.AuthorizeSecurityGroupIngress
import Network.AWS.EC2.BundleInstance
import Network.AWS.EC2.CancelBundleTask
import Network.AWS.EC2.CancelConversionTask
import Network.AWS.EC2.CancelExportTask
import Network.AWS.EC2.CancelReservedInstancesListing
import Network.AWS.EC2.CancelSpotInstanceRequests
import Network.AWS.EC2.ConfirmProductInstance
import Network.AWS.EC2.CopyImage
import Network.AWS.EC2.CopySnapshot
import Network.AWS.EC2.CreateCustomerGateway
import Network.AWS.EC2.CreateDhcpOptions
import Network.AWS.EC2.CreateImage
import Network.AWS.EC2.CreateInstanceExportTask
import Network.AWS.EC2.CreateInternetGateway
import Network.AWS.EC2.CreateKeyPair
import Network.AWS.EC2.CreateNetworkAcl
import Network.AWS.EC2.CreateNetworkAclEntry
import Network.AWS.EC2.CreateNetworkInterface
import Network.AWS.EC2.CreatePlacementGroup
import Network.AWS.EC2.CreateReservedInstancesListing
import Network.AWS.EC2.CreateRoute
import Network.AWS.EC2.CreateRouteTable
import Network.AWS.EC2.CreateSecurityGroup
import Network.AWS.EC2.CreateSnapshot
import Network.AWS.EC2.CreateSpotDatafeedSubscription
import Network.AWS.EC2.CreateSubnet
import Network.AWS.EC2.CreateTags
import Network.AWS.EC2.CreateVolume
import Network.AWS.EC2.CreateVpc
import Network.AWS.EC2.CreateVpcPeeringConnection
import Network.AWS.EC2.CreateVpnConnection
import Network.AWS.EC2.CreateVpnConnectionRoute
import Network.AWS.EC2.CreateVpnGateway
import Network.AWS.EC2.DeleteCustomerGateway
import Network.AWS.EC2.DeleteDhcpOptions
import Network.AWS.EC2.DeleteInternetGateway
import Network.AWS.EC2.DeleteKeyPair
import Network.AWS.EC2.DeleteNetworkAcl
import Network.AWS.EC2.DeleteNetworkAclEntry
import Network.AWS.EC2.DeleteNetworkInterface
import Network.AWS.EC2.DeletePlacementGroup
import Network.AWS.EC2.DeleteRoute
import Network.AWS.EC2.DeleteRouteTable
import Network.AWS.EC2.DeleteSecurityGroup
import Network.AWS.EC2.DeleteSnapshot
import Network.AWS.EC2.DeleteSpotDatafeedSubscription
import Network.AWS.EC2.DeleteSubnet
import Network.AWS.EC2.DeleteTags
import Network.AWS.EC2.DeleteVolume
import Network.AWS.EC2.DeleteVpc
import Network.AWS.EC2.DeleteVpcPeeringConnection
import Network.AWS.EC2.DeleteVpnConnection
import Network.AWS.EC2.DeleteVpnConnectionRoute
import Network.AWS.EC2.DeleteVpnGateway
import Network.AWS.EC2.DeregisterImage
import Network.AWS.EC2.DescribeAccountAttributes
import Network.AWS.EC2.DescribeAddresses
import Network.AWS.EC2.DescribeAvailabilityZones
import Network.AWS.EC2.DescribeBundleTasks
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeCustomerGateways
import Network.AWS.EC2.DescribeDhcpOptions
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.DescribeImageAttribute
import Network.AWS.EC2.DescribeImages
import Network.AWS.EC2.DescribeInstanceAttribute
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInternetGateways
import Network.AWS.EC2.DescribeKeyPairs
import Network.AWS.EC2.DescribeNetworkAcls
import Network.AWS.EC2.DescribeNetworkInterfaceAttribute
import Network.AWS.EC2.DescribeNetworkInterfaces
import Network.AWS.EC2.DescribePlacementGroups
import Network.AWS.EC2.DescribeRegions
import Network.AWS.EC2.DescribeReservedInstances
import Network.AWS.EC2.DescribeReservedInstancesListings
import Network.AWS.EC2.DescribeReservedInstancesModifications
import Network.AWS.EC2.DescribeReservedInstancesOfferings
import Network.AWS.EC2.DescribeRouteTables
import Network.AWS.EC2.DescribeSecurityGroups
import Network.AWS.EC2.DescribeSnapshotAttribute
import Network.AWS.EC2.DescribeSnapshots
import Network.AWS.EC2.DescribeSpotDatafeedSubscription
import Network.AWS.EC2.DescribeSpotInstanceRequests
import Network.AWS.EC2.DescribeSpotPriceHistory
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.DescribeTags
import Network.AWS.EC2.DescribeVolumeAttribute
import Network.AWS.EC2.DescribeVolumeStatus
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVpcAttribute
import Network.AWS.EC2.DescribeVpcPeeringConnections
import Network.AWS.EC2.DescribeVpcs
import Network.AWS.EC2.DescribeVpnConnections
import Network.AWS.EC2.DescribeVpnGateways
import Network.AWS.EC2.DetachInternetGateway
import Network.AWS.EC2.DetachNetworkInterface
import Network.AWS.EC2.DetachVolume
import Network.AWS.EC2.DetachVpnGateway
import Network.AWS.EC2.DisableVgwRoutePropagation
import Network.AWS.EC2.DisassociateAddress
import Network.AWS.EC2.DisassociateRouteTable
import Network.AWS.EC2.EnableVgwRoutePropagation
import Network.AWS.EC2.EnableVolumeIO
import Network.AWS.EC2.GetConsoleOutput
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.ImportInstance
import Network.AWS.EC2.ImportKeyPair
import Network.AWS.EC2.ImportVolume
import Network.AWS.EC2.ModifyImageAttribute
import Network.AWS.EC2.ModifyInstanceAttribute
import Network.AWS.EC2.ModifyNetworkInterfaceAttribute
import Network.AWS.EC2.ModifyReservedInstances
import Network.AWS.EC2.ModifySnapshotAttribute
import Network.AWS.EC2.ModifySubnetAttribute
import Network.AWS.EC2.ModifyVolumeAttribute
import Network.AWS.EC2.ModifyVpcAttribute
import Network.AWS.EC2.MonitorInstances
import Network.AWS.EC2.PurchaseReservedInstancesOffering
import Network.AWS.EC2.RebootInstances
import Network.AWS.EC2.RegisterImage
import Network.AWS.EC2.RejectVpcPeeringConnection
import Network.AWS.EC2.ReleaseAddress
import Network.AWS.EC2.ReplaceNetworkAclAssociation
import Network.AWS.EC2.ReplaceNetworkAclEntry
import Network.AWS.EC2.ReplaceRoute
import Network.AWS.EC2.ReplaceRouteTableAssociation
import Network.AWS.EC2.ReportInstanceStatus
import Network.AWS.EC2.RequestSpotInstances
import Network.AWS.EC2.ResetImageAttribute
import Network.AWS.EC2.ResetInstanceAttribute
import Network.AWS.EC2.ResetNetworkInterfaceAttribute
import Network.AWS.EC2.ResetSnapshotAttribute
import Network.AWS.EC2.RevokeSecurityGroupEgress
import Network.AWS.EC2.RevokeSecurityGroupIngress
import Network.AWS.EC2.RunInstances
import Network.AWS.EC2.StartInstances
import Network.AWS.EC2.StopInstances
import Network.AWS.EC2.TerminateInstances
import Network.AWS.EC2.Types
import Network.AWS.EC2.UnassignPrivateIpAddresses
import Network.AWS.EC2.UnmonitorInstances
