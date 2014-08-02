-- Module      : Network.AWS.EC2.V2014_06_15
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic Compute Cloud (Amazon EC2) is a web service that provides
-- resizable compute capacity in the cloud. It is designed to make web-scale
-- computing easier for developers. Amazon EC2’s simple web service interface
-- allows you to obtain and configure capacity with minimal friction. It
-- provides you with complete control of your computing resources and lets you
-- run on Amazon’s proven computing environment. Amazon EC2 reduces the time
-- required to obtain and boot new server instances to minutes, allowing you
-- to quickly scale capacity, both up and down, as your computing requirements
-- change. Amazon EC2 changes the economics of computing by allowing you to
-- pay only for capacity that you actually use. Amazon EC2 provides developers
-- the tools to build failure resilient applications and isolate themselves
-- from common failure scenarios.
module Network.AWS.EC2.V2014_06_15 (module Export) where

import Network.AWS.EC2.V2014_06_15.AcceptVpcPeeringConnection as Export
import Network.AWS.EC2.V2014_06_15.AllocateAddress as Export
import Network.AWS.EC2.V2014_06_15.AssignPrivateIpAddresses as Export
import Network.AWS.EC2.V2014_06_15.AssociateAddress as Export
import Network.AWS.EC2.V2014_06_15.AssociateDhcpOptions as Export
import Network.AWS.EC2.V2014_06_15.AssociateRouteTable as Export
import Network.AWS.EC2.V2014_06_15.AttachInternetGateway as Export
import Network.AWS.EC2.V2014_06_15.AttachNetworkInterface as Export
import Network.AWS.EC2.V2014_06_15.AttachVolume as Export
import Network.AWS.EC2.V2014_06_15.AttachVpnGateway as Export
import Network.AWS.EC2.V2014_06_15.AuthorizeSecurityGroupEgress as Export
import Network.AWS.EC2.V2014_06_15.AuthorizeSecurityGroupIngress as Export
import Network.AWS.EC2.V2014_06_15.BundleInstance as Export
import Network.AWS.EC2.V2014_06_15.CancelBundleTask as Export
import Network.AWS.EC2.V2014_06_15.CancelConversionTask as Export
import Network.AWS.EC2.V2014_06_15.CancelExportTask as Export
import Network.AWS.EC2.V2014_06_15.CancelReservedInstancesListing as Export
import Network.AWS.EC2.V2014_06_15.CancelSpotInstanceRequests as Export
import Network.AWS.EC2.V2014_06_15.ConfirmProductInstance as Export
import Network.AWS.EC2.V2014_06_15.CopyImage as Export
import Network.AWS.EC2.V2014_06_15.CopySnapshot as Export
import Network.AWS.EC2.V2014_06_15.CreateCustomerGateway as Export
import Network.AWS.EC2.V2014_06_15.CreateDhcpOptions as Export
import Network.AWS.EC2.V2014_06_15.CreateImage as Export
import Network.AWS.EC2.V2014_06_15.CreateInstanceExportTask as Export
import Network.AWS.EC2.V2014_06_15.CreateInternetGateway as Export
import Network.AWS.EC2.V2014_06_15.CreateKeyPair as Export
import Network.AWS.EC2.V2014_06_15.CreateNetworkAcl as Export
import Network.AWS.EC2.V2014_06_15.CreateNetworkAclEntry as Export
import Network.AWS.EC2.V2014_06_15.CreateNetworkInterface as Export
import Network.AWS.EC2.V2014_06_15.CreatePlacementGroup as Export
import Network.AWS.EC2.V2014_06_15.CreateReservedInstancesListing as Export
import Network.AWS.EC2.V2014_06_15.CreateRoute as Export
import Network.AWS.EC2.V2014_06_15.CreateRouteTable as Export
import Network.AWS.EC2.V2014_06_15.CreateSecurityGroup as Export
import Network.AWS.EC2.V2014_06_15.CreateSnapshot as Export
import Network.AWS.EC2.V2014_06_15.CreateSpotDatafeedSubscription as Export
import Network.AWS.EC2.V2014_06_15.CreateSubnet as Export
import Network.AWS.EC2.V2014_06_15.CreateTags as Export
import Network.AWS.EC2.V2014_06_15.CreateVolume as Export
import Network.AWS.EC2.V2014_06_15.CreateVpc as Export
import Network.AWS.EC2.V2014_06_15.CreateVpcPeeringConnection as Export
import Network.AWS.EC2.V2014_06_15.CreateVpnConnection as Export
import Network.AWS.EC2.V2014_06_15.CreateVpnConnectionRoute as Export
import Network.AWS.EC2.V2014_06_15.CreateVpnGateway as Export
import Network.AWS.EC2.V2014_06_15.DeleteCustomerGateway as Export
import Network.AWS.EC2.V2014_06_15.DeleteDhcpOptions as Export
import Network.AWS.EC2.V2014_06_15.DeleteInternetGateway as Export
import Network.AWS.EC2.V2014_06_15.DeleteKeyPair as Export
import Network.AWS.EC2.V2014_06_15.DeleteNetworkAcl as Export
import Network.AWS.EC2.V2014_06_15.DeleteNetworkAclEntry as Export
import Network.AWS.EC2.V2014_06_15.DeleteNetworkInterface as Export
import Network.AWS.EC2.V2014_06_15.DeletePlacementGroup as Export
import Network.AWS.EC2.V2014_06_15.DeleteRoute as Export
import Network.AWS.EC2.V2014_06_15.DeleteRouteTable as Export
import Network.AWS.EC2.V2014_06_15.DeleteSecurityGroup as Export
import Network.AWS.EC2.V2014_06_15.DeleteSnapshot as Export
import Network.AWS.EC2.V2014_06_15.DeleteSpotDatafeedSubscription as Export
import Network.AWS.EC2.V2014_06_15.DeleteSubnet as Export
import Network.AWS.EC2.V2014_06_15.DeleteTags as Export
import Network.AWS.EC2.V2014_06_15.DeleteVolume as Export
import Network.AWS.EC2.V2014_06_15.DeleteVpc as Export
import Network.AWS.EC2.V2014_06_15.DeleteVpcPeeringConnection as Export
import Network.AWS.EC2.V2014_06_15.DeleteVpnConnection as Export
import Network.AWS.EC2.V2014_06_15.DeleteVpnConnectionRoute as Export
import Network.AWS.EC2.V2014_06_15.DeleteVpnGateway as Export
import Network.AWS.EC2.V2014_06_15.DeregisterImage as Export
import Network.AWS.EC2.V2014_06_15.DescribeAccountAttributes as Export
import Network.AWS.EC2.V2014_06_15.DescribeAddresses as Export
import Network.AWS.EC2.V2014_06_15.DescribeAvailabilityZones as Export
import Network.AWS.EC2.V2014_06_15.DescribeBundleTasks as Export
import Network.AWS.EC2.V2014_06_15.DescribeConversionTasks as Export
import Network.AWS.EC2.V2014_06_15.DescribeCustomerGateways as Export
import Network.AWS.EC2.V2014_06_15.DescribeDhcpOptions as Export
import Network.AWS.EC2.V2014_06_15.DescribeExportTasks as Export
import Network.AWS.EC2.V2014_06_15.DescribeImageAttribute as Export
import Network.AWS.EC2.V2014_06_15.DescribeImages as Export
import Network.AWS.EC2.V2014_06_15.DescribeInstanceAttribute as Export
import Network.AWS.EC2.V2014_06_15.DescribeInstanceStatus as Export
import Network.AWS.EC2.V2014_06_15.DescribeInstances as Export
import Network.AWS.EC2.V2014_06_15.DescribeInternetGateways as Export
import Network.AWS.EC2.V2014_06_15.DescribeKeyPairs as Export
import Network.AWS.EC2.V2014_06_15.DescribeNetworkAcls as Export
import Network.AWS.EC2.V2014_06_15.DescribeNetworkInterfaceAttribute as Export
import Network.AWS.EC2.V2014_06_15.DescribeNetworkInterfaces as Export
import Network.AWS.EC2.V2014_06_15.DescribePlacementGroups as Export
import Network.AWS.EC2.V2014_06_15.DescribeRegions as Export
import Network.AWS.EC2.V2014_06_15.DescribeReservedInstances as Export
import Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesListings as Export
import Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesModifications as Export
import Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesOfferings as Export
import Network.AWS.EC2.V2014_06_15.DescribeRouteTables as Export
import Network.AWS.EC2.V2014_06_15.DescribeSecurityGroups as Export
import Network.AWS.EC2.V2014_06_15.DescribeSnapshotAttribute as Export
import Network.AWS.EC2.V2014_06_15.DescribeSnapshots as Export
import Network.AWS.EC2.V2014_06_15.DescribeSpotDatafeedSubscription as Export
import Network.AWS.EC2.V2014_06_15.DescribeSpotInstanceRequests as Export
import Network.AWS.EC2.V2014_06_15.DescribeSpotPriceHistory as Export
import Network.AWS.EC2.V2014_06_15.DescribeSubnets as Export
import Network.AWS.EC2.V2014_06_15.DescribeTags as Export
import Network.AWS.EC2.V2014_06_15.DescribeVolumeAttribute as Export
import Network.AWS.EC2.V2014_06_15.DescribeVolumeStatus as Export
import Network.AWS.EC2.V2014_06_15.DescribeVolumes as Export
import Network.AWS.EC2.V2014_06_15.DescribeVpcAttribute as Export
import Network.AWS.EC2.V2014_06_15.DescribeVpcPeeringConnections as Export
import Network.AWS.EC2.V2014_06_15.DescribeVpcs as Export
import Network.AWS.EC2.V2014_06_15.DescribeVpnConnections as Export
import Network.AWS.EC2.V2014_06_15.DescribeVpnGateways as Export
import Network.AWS.EC2.V2014_06_15.DetachInternetGateway as Export
import Network.AWS.EC2.V2014_06_15.DetachNetworkInterface as Export
import Network.AWS.EC2.V2014_06_15.DetachVolume as Export
import Network.AWS.EC2.V2014_06_15.DetachVpnGateway as Export
import Network.AWS.EC2.V2014_06_15.DisableVgwRoutePropagation as Export
import Network.AWS.EC2.V2014_06_15.DisassociateAddress as Export
import Network.AWS.EC2.V2014_06_15.DisassociateRouteTable as Export
import Network.AWS.EC2.V2014_06_15.EnableVgwRoutePropagation as Export
import Network.AWS.EC2.V2014_06_15.EnableVolumeIO as Export
import Network.AWS.EC2.V2014_06_15.GetConsoleOutput as Export
import Network.AWS.EC2.V2014_06_15.GetPasswordData as Export
import Network.AWS.EC2.V2014_06_15.ImportInstance as Export
import Network.AWS.EC2.V2014_06_15.ImportKeyPair as Export
import Network.AWS.EC2.V2014_06_15.ImportVolume as Export
import Network.AWS.EC2.V2014_06_15.ModifyImageAttribute as Export
import Network.AWS.EC2.V2014_06_15.ModifyInstanceAttribute as Export
import Network.AWS.EC2.V2014_06_15.ModifyNetworkInterfaceAttribute as Export
import Network.AWS.EC2.V2014_06_15.ModifyReservedInstances as Export
import Network.AWS.EC2.V2014_06_15.ModifySnapshotAttribute as Export
import Network.AWS.EC2.V2014_06_15.ModifySubnetAttribute as Export
import Network.AWS.EC2.V2014_06_15.ModifyVolumeAttribute as Export
import Network.AWS.EC2.V2014_06_15.ModifyVpcAttribute as Export
import Network.AWS.EC2.V2014_06_15.MonitorInstances as Export
import Network.AWS.EC2.V2014_06_15.PurchaseReservedInstancesOffering as Export
import Network.AWS.EC2.V2014_06_15.RebootInstances as Export
import Network.AWS.EC2.V2014_06_15.RegisterImage as Export
import Network.AWS.EC2.V2014_06_15.RejectVpcPeeringConnection as Export
import Network.AWS.EC2.V2014_06_15.ReleaseAddress as Export
import Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclAssociation as Export
import Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclEntry as Export
import Network.AWS.EC2.V2014_06_15.ReplaceRoute as Export
import Network.AWS.EC2.V2014_06_15.ReplaceRouteTableAssociation as Export
import Network.AWS.EC2.V2014_06_15.ReportInstanceStatus as Export
import Network.AWS.EC2.V2014_06_15.RequestSpotInstances as Export
import Network.AWS.EC2.V2014_06_15.ResetImageAttribute as Export
import Network.AWS.EC2.V2014_06_15.ResetInstanceAttribute as Export
import Network.AWS.EC2.V2014_06_15.ResetNetworkInterfaceAttribute as Export
import Network.AWS.EC2.V2014_06_15.ResetSnapshotAttribute as Export
import Network.AWS.EC2.V2014_06_15.RevokeSecurityGroupEgress as Export
import Network.AWS.EC2.V2014_06_15.RevokeSecurityGroupIngress as Export
import Network.AWS.EC2.V2014_06_15.RunInstances as Export
import Network.AWS.EC2.V2014_06_15.StartInstances as Export
import Network.AWS.EC2.V2014_06_15.StopInstances as Export
import Network.AWS.EC2.V2014_06_15.TerminateInstances as Export
import Network.AWS.EC2.V2014_06_15.Types as Export
import Network.AWS.EC2.V2014_06_15.UnassignPrivateIpAddresses as Export
import Network.AWS.EC2.V2014_06_15.UnmonitorInstances as Export
