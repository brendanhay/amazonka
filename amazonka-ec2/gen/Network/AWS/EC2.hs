-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Elastic Compute Cloud
--
-- Amazon Elastic Compute Cloud (Amazon EC2) provides resizable computing
-- capacity in the Amazon Web Services (AWS) cloud. Using Amazon EC2
-- eliminates your need to invest in hardware up front, so you can develop
-- and deploy applications faster.
module Network.AWS.EC2
    ( module Export
    ) where

import           Network.AWS.EC2.AcceptVPCPeeringConnection             as Export
import           Network.AWS.EC2.AllocateAddress                        as Export
import           Network.AWS.EC2.AssignPrivateIPAddresses               as Export
import           Network.AWS.EC2.AssociateAddress                       as Export
import           Network.AWS.EC2.AssociateDHCPOptions                   as Export
import           Network.AWS.EC2.AssociateRouteTable                    as Export
import           Network.AWS.EC2.AttachClassicLinkVPC                   as Export
import           Network.AWS.EC2.AttachInternetGateway                  as Export
import           Network.AWS.EC2.AttachNetworkInterface                 as Export
import           Network.AWS.EC2.AttachVolume                           as Export
import           Network.AWS.EC2.AttachVPNGateway                       as Export
import           Network.AWS.EC2.AuthorizeSecurityGroupEgress           as Export
import           Network.AWS.EC2.AuthorizeSecurityGroupIngress          as Export
import           Network.AWS.EC2.BundleInstance                         as Export
import           Network.AWS.EC2.CancelBundleTask                       as Export
import           Network.AWS.EC2.CancelConversionTask                   as Export
import           Network.AWS.EC2.CancelExportTask                       as Export
import           Network.AWS.EC2.CancelImportTask                       as Export
import           Network.AWS.EC2.CancelReservedInstancesListing         as Export
import           Network.AWS.EC2.CancelSpotFleetRequests                as Export
import           Network.AWS.EC2.CancelSpotInstanceRequests             as Export
import           Network.AWS.EC2.ConfirmProductInstance                 as Export
import           Network.AWS.EC2.CopyImage                              as Export
import           Network.AWS.EC2.CopySnapshot                           as Export
import           Network.AWS.EC2.CreateCustomerGateway                  as Export
import           Network.AWS.EC2.CreateDHCPOptions                      as Export
import           Network.AWS.EC2.CreateFlowLogs                         as Export
import           Network.AWS.EC2.CreateImage                            as Export
import           Network.AWS.EC2.CreateInstanceExportTask               as Export
import           Network.AWS.EC2.CreateInternetGateway                  as Export
import           Network.AWS.EC2.CreateKeyPair                          as Export
import           Network.AWS.EC2.CreateNetworkACL                       as Export
import           Network.AWS.EC2.CreateNetworkACLEntry                  as Export
import           Network.AWS.EC2.CreateNetworkInterface                 as Export
import           Network.AWS.EC2.CreatePlacementGroup                   as Export
import           Network.AWS.EC2.CreateReservedInstancesListing         as Export
import           Network.AWS.EC2.CreateRoute                            as Export
import           Network.AWS.EC2.CreateRouteTable                       as Export
import           Network.AWS.EC2.CreateSecurityGroup                    as Export
import           Network.AWS.EC2.CreateSnapshot                         as Export
import           Network.AWS.EC2.CreateSpotDatafeedSubscription         as Export
import           Network.AWS.EC2.CreateSubnet                           as Export
import           Network.AWS.EC2.CreateTags                             as Export
import           Network.AWS.EC2.CreateVolume                           as Export
import           Network.AWS.EC2.CreateVPC                              as Export
import           Network.AWS.EC2.CreateVPCEndpoint                      as Export
import           Network.AWS.EC2.CreateVPCPeeringConnection             as Export
import           Network.AWS.EC2.CreateVPNConnection                    as Export
import           Network.AWS.EC2.CreateVPNConnectionRoute               as Export
import           Network.AWS.EC2.CreateVPNGateway                       as Export
import           Network.AWS.EC2.DeleteCustomerGateway                  as Export
import           Network.AWS.EC2.DeleteDHCPOptions                      as Export
import           Network.AWS.EC2.DeleteFlowLogs                         as Export
import           Network.AWS.EC2.DeleteInternetGateway                  as Export
import           Network.AWS.EC2.DeleteKeyPair                          as Export
import           Network.AWS.EC2.DeleteNetworkACL                       as Export
import           Network.AWS.EC2.DeleteNetworkACLEntry                  as Export
import           Network.AWS.EC2.DeleteNetworkInterface                 as Export
import           Network.AWS.EC2.DeletePlacementGroup                   as Export
import           Network.AWS.EC2.DeleteRoute                            as Export
import           Network.AWS.EC2.DeleteRouteTable                       as Export
import           Network.AWS.EC2.DeleteSecurityGroup                    as Export
import           Network.AWS.EC2.DeleteSnapshot                         as Export
import           Network.AWS.EC2.DeleteSpotDatafeedSubscription         as Export
import           Network.AWS.EC2.DeleteSubnet                           as Export
import           Network.AWS.EC2.DeleteTags                             as Export
import           Network.AWS.EC2.DeleteVolume                           as Export
import           Network.AWS.EC2.DeleteVPC                              as Export
import           Network.AWS.EC2.DeleteVPCEndpoints                     as Export
import           Network.AWS.EC2.DeleteVPCPeeringConnection             as Export
import           Network.AWS.EC2.DeleteVPNConnection                    as Export
import           Network.AWS.EC2.DeleteVPNConnectionRoute               as Export
import           Network.AWS.EC2.DeleteVPNGateway                       as Export
import           Network.AWS.EC2.DeregisterImage                        as Export
import           Network.AWS.EC2.DescribeAccountAttributes              as Export
import           Network.AWS.EC2.DescribeAddresses                      as Export
import           Network.AWS.EC2.DescribeAvailabilityZones              as Export
import           Network.AWS.EC2.DescribeBundleTasks                    as Export
import           Network.AWS.EC2.DescribeClassicLinkInstances           as Export
import           Network.AWS.EC2.DescribeConversionTasks                as Export
import           Network.AWS.EC2.DescribeCustomerGateways               as Export
import           Network.AWS.EC2.DescribeDHCPOptions                    as Export
import           Network.AWS.EC2.DescribeExportTasks                    as Export
import           Network.AWS.EC2.DescribeFlowLogs                       as Export
import           Network.AWS.EC2.DescribeImageAttribute                 as Export
import           Network.AWS.EC2.DescribeImages                         as Export
import           Network.AWS.EC2.DescribeImportImageTasks               as Export
import           Network.AWS.EC2.DescribeImportSnapshotTasks            as Export
import           Network.AWS.EC2.DescribeInstanceAttribute              as Export
import           Network.AWS.EC2.DescribeInstances                      as Export
import           Network.AWS.EC2.DescribeInstanceStatus                 as Export
import           Network.AWS.EC2.DescribeInternetGateways               as Export
import           Network.AWS.EC2.DescribeKeyPairs                       as Export
import           Network.AWS.EC2.DescribeMovingAddresses                as Export
import           Network.AWS.EC2.DescribeNetworkACLs                    as Export
import           Network.AWS.EC2.DescribeNetworkInterfaceAttribute      as Export
import           Network.AWS.EC2.DescribeNetworkInterfaces              as Export
import           Network.AWS.EC2.DescribePlacementGroups                as Export
import           Network.AWS.EC2.DescribePrefixLists                    as Export
import           Network.AWS.EC2.DescribeRegions                        as Export
import           Network.AWS.EC2.DescribeReservedInstances              as Export
import           Network.AWS.EC2.DescribeReservedInstancesListings      as Export
import           Network.AWS.EC2.DescribeReservedInstancesModifications as Export
import           Network.AWS.EC2.DescribeReservedInstancesOfferings     as Export
import           Network.AWS.EC2.DescribeRouteTables                    as Export
import           Network.AWS.EC2.DescribeSecurityGroups                 as Export
import           Network.AWS.EC2.DescribeSnapshotAttribute              as Export
import           Network.AWS.EC2.DescribeSnapshots                      as Export
import           Network.AWS.EC2.DescribeSpotDatafeedSubscription       as Export
import           Network.AWS.EC2.DescribeSpotFleetInstances             as Export
import           Network.AWS.EC2.DescribeSpotFleetRequestHistory        as Export
import           Network.AWS.EC2.DescribeSpotFleetRequests              as Export
import           Network.AWS.EC2.DescribeSpotInstanceRequests           as Export
import           Network.AWS.EC2.DescribeSpotPriceHistory               as Export
import           Network.AWS.EC2.DescribeSubnets                        as Export
import           Network.AWS.EC2.DescribeTags                           as Export
import           Network.AWS.EC2.DescribeVolumeAttribute                as Export
import           Network.AWS.EC2.DescribeVolumes                        as Export
import           Network.AWS.EC2.DescribeVolumeStatus                   as Export
import           Network.AWS.EC2.DescribeVPCAttribute                   as Export
import           Network.AWS.EC2.DescribeVPCClassicLink                 as Export
import           Network.AWS.EC2.DescribeVPCEndpoints                   as Export
import           Network.AWS.EC2.DescribeVPCEndpointServices            as Export
import           Network.AWS.EC2.DescribeVPCPeeringConnections          as Export
import           Network.AWS.EC2.DescribeVPCs                           as Export
import           Network.AWS.EC2.DescribeVPNConnections                 as Export
import           Network.AWS.EC2.DescribeVPNGateways                    as Export
import           Network.AWS.EC2.DetachClassicLinkVPC                   as Export
import           Network.AWS.EC2.DetachInternetGateway                  as Export
import           Network.AWS.EC2.DetachNetworkInterface                 as Export
import           Network.AWS.EC2.DetachVolume                           as Export
import           Network.AWS.EC2.DetachVPNGateway                       as Export
import           Network.AWS.EC2.DisableVGWRoutePropagation             as Export
import           Network.AWS.EC2.DisableVPCClassicLink                  as Export
import           Network.AWS.EC2.DisassociateAddress                    as Export
import           Network.AWS.EC2.DisassociateRouteTable                 as Export
import           Network.AWS.EC2.EnableVGWRoutePropagation              as Export
import           Network.AWS.EC2.EnableVolumeIO                         as Export
import           Network.AWS.EC2.EnableVPCClassicLink                   as Export
import           Network.AWS.EC2.GetConsoleOutput                       as Export
import           Network.AWS.EC2.GetPasswordData                        as Export
import           Network.AWS.EC2.ImportImage                            as Export
import           Network.AWS.EC2.ImportInstance                         as Export
import           Network.AWS.EC2.ImportKeyPair                          as Export
import           Network.AWS.EC2.ImportSnapshot                         as Export
import           Network.AWS.EC2.ImportVolume                           as Export
import           Network.AWS.EC2.ModifyImageAttribute                   as Export
import           Network.AWS.EC2.ModifyInstanceAttribute                as Export
import           Network.AWS.EC2.ModifyNetworkInterfaceAttribute        as Export
import           Network.AWS.EC2.ModifyReservedInstances                as Export
import           Network.AWS.EC2.ModifySnapshotAttribute                as Export
import           Network.AWS.EC2.ModifySubnetAttribute                  as Export
import           Network.AWS.EC2.ModifyVolumeAttribute                  as Export
import           Network.AWS.EC2.ModifyVPCAttribute                     as Export
import           Network.AWS.EC2.ModifyVPCEndpoint                      as Export
import           Network.AWS.EC2.MonitorInstances                       as Export
import           Network.AWS.EC2.MoveAddressToVPC                       as Export
import           Network.AWS.EC2.PurchaseReservedInstancesOffering      as Export
import           Network.AWS.EC2.RebootInstances                        as Export
import           Network.AWS.EC2.RegisterImage                          as Export
import           Network.AWS.EC2.RejectVPCPeeringConnection             as Export
import           Network.AWS.EC2.ReleaseAddress                         as Export
import           Network.AWS.EC2.ReplaceNetworkACLAssociation           as Export
import           Network.AWS.EC2.ReplaceNetworkACLEntry                 as Export
import           Network.AWS.EC2.ReplaceRoute                           as Export
import           Network.AWS.EC2.ReplaceRouteTableAssociation           as Export
import           Network.AWS.EC2.ReportInstanceStatus                   as Export
import           Network.AWS.EC2.RequestSpotFleet                       as Export
import           Network.AWS.EC2.RequestSpotInstances                   as Export
import           Network.AWS.EC2.ResetImageAttribute                    as Export
import           Network.AWS.EC2.ResetInstanceAttribute                 as Export
import           Network.AWS.EC2.ResetNetworkInterfaceAttribute         as Export
import           Network.AWS.EC2.ResetSnapshotAttribute                 as Export
import           Network.AWS.EC2.RestoreAddressToClassic                as Export
import           Network.AWS.EC2.RevokeSecurityGroupEgress              as Export
import           Network.AWS.EC2.RevokeSecurityGroupIngress             as Export
import           Network.AWS.EC2.RunInstances                           as Export
import           Network.AWS.EC2.StartInstances                         as Export
import           Network.AWS.EC2.StopInstances                          as Export
import           Network.AWS.EC2.TerminateInstances                     as Export
import           Network.AWS.EC2.Types                                  as Export
import           Network.AWS.EC2.UnassignPrivateIPAddresses             as Export
import           Network.AWS.EC2.UnmonitorInstances                     as Export
import           Network.AWS.EC2.Waiters                                as Export
