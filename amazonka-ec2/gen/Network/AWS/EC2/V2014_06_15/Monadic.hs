{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.EC2.V2014_06_15.Monadic
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
--
-- The 'State' operator variants from "Control.Lens.Setter" such as '.='
-- can be used to modify any additional request parameters before sending.
module Network.AWS.EC2.V2014_06_15.Monadic
    (
    -- * AcceptVpcPeeringConnection
    -- $AcceptVpcPeeringConnection
      acceptVpcPeeringConnection
    , acceptVpcPeeringConnectionCatch

    -- * AllocateAddress
    -- $AllocateAddress
    , allocateAddress
    , allocateAddressCatch

    -- * AssignPrivateIpAddresses
    -- $AssignPrivateIpAddresses
    , assignPrivateIpAddresses
    , assignPrivateIpAddressesCatch

    -- * AssociateAddress
    -- $AssociateAddress
    , associateAddress
    , associateAddressCatch

    -- * AssociateDhcpOptions
    -- $AssociateDhcpOptions
    , associateDhcpOptions
    , associateDhcpOptionsCatch

    -- * AssociateRouteTable
    -- $AssociateRouteTable
    , associateRouteTable
    , associateRouteTableCatch

    -- * AttachInternetGateway
    -- $AttachInternetGateway
    , attachInternetGateway
    , attachInternetGatewayCatch

    -- * AttachNetworkInterface
    -- $AttachNetworkInterface
    , attachNetworkInterface
    , attachNetworkInterfaceCatch

    -- * AttachVolume
    -- $AttachVolume
    , attachVolume
    , attachVolumeCatch

    -- * AttachVpnGateway
    -- $AttachVpnGateway
    , attachVpnGateway
    , attachVpnGatewayCatch

    -- * AuthorizeSecurityGroupEgress
    -- $AuthorizeSecurityGroupEgress
    , authorizeSecurityGroupEgress
    , authorizeSecurityGroupEgressCatch

    -- * AuthorizeSecurityGroupIngress
    -- $AuthorizeSecurityGroupIngress
    , authorizeSecurityGroupIngress
    , authorizeSecurityGroupIngressCatch

    -- * BundleInstance
    -- $BundleInstance
    , bundleInstance
    , bundleInstanceCatch

    -- * CancelBundleTask
    -- $CancelBundleTask
    , cancelBundleTask
    , cancelBundleTaskCatch

    -- * CancelConversionTask
    -- $CancelConversionTask
    , cancelConversionTask
    , cancelConversionTaskCatch

    -- * CancelExportTask
    -- $CancelExportTask
    , cancelExportTask
    , cancelExportTaskCatch

    -- * CancelReservedInstancesListing
    -- $CancelReservedInstancesListing
    , cancelReservedInstancesListing
    , cancelReservedInstancesListingCatch

    -- * CancelSpotInstanceRequests
    -- $CancelSpotInstanceRequests
    , cancelSpotInstanceRequests
    , cancelSpotInstanceRequestsCatch

    -- * ConfirmProductInstance
    -- $ConfirmProductInstance
    , confirmProductInstance
    , confirmProductInstanceCatch

    -- * CopyImage
    -- $CopyImage
    , copyImage
    , copyImageCatch

    -- * CopySnapshot
    -- $CopySnapshot
    , copySnapshot
    , copySnapshotCatch

    -- * CreateCustomerGateway
    -- $CreateCustomerGateway
    , createCustomerGateway
    , createCustomerGatewayCatch

    -- * CreateDhcpOptions
    -- $CreateDhcpOptions
    , createDhcpOptions
    , createDhcpOptionsCatch

    -- * CreateImage
    -- $CreateImage
    , createImage
    , createImageCatch

    -- * CreateInstanceExportTask
    -- $CreateInstanceExportTask
    , createInstanceExportTask
    , createInstanceExportTaskCatch

    -- * CreateInternetGateway
    -- $CreateInternetGateway
    , createInternetGateway
    , createInternetGatewayCatch

    -- * CreateKeyPair
    -- $CreateKeyPair
    , createKeyPair
    , createKeyPairCatch

    -- * CreateNetworkAcl
    -- $CreateNetworkAcl
    , createNetworkAcl
    , createNetworkAclCatch

    -- * CreateNetworkAclEntry
    -- $CreateNetworkAclEntry
    , createNetworkAclEntry
    , createNetworkAclEntryCatch

    -- * CreateNetworkInterface
    -- $CreateNetworkInterface
    , createNetworkInterface
    , createNetworkInterfaceCatch

    -- * CreatePlacementGroup
    -- $CreatePlacementGroup
    , createPlacementGroup
    , createPlacementGroupCatch

    -- * CreateReservedInstancesListing
    -- $CreateReservedInstancesListing
    , createReservedInstancesListing
    , createReservedInstancesListingCatch

    -- * CreateRoute
    -- $CreateRoute
    , createRoute
    , createRouteCatch

    -- * CreateRouteTable
    -- $CreateRouteTable
    , createRouteTable
    , createRouteTableCatch

    -- * CreateSecurityGroup
    -- $CreateSecurityGroup
    , createSecurityGroup
    , createSecurityGroupCatch

    -- * CreateSnapshot
    -- $CreateSnapshot
    , createSnapshot
    , createSnapshotCatch

    -- * CreateSpotDatafeedSubscription
    -- $CreateSpotDatafeedSubscription
    , createSpotDatafeedSubscription
    , createSpotDatafeedSubscriptionCatch

    -- * CreateSubnet
    -- $CreateSubnet
    , createSubnet
    , createSubnetCatch

    -- * CreateTags
    -- $CreateTags
    , createTags
    , createTagsCatch

    -- * CreateVolume
    -- $CreateVolume
    , createVolume
    , createVolumeCatch

    -- * CreateVpc
    -- $CreateVpc
    , createVpc
    , createVpcCatch

    -- * CreateVpcPeeringConnection
    -- $CreateVpcPeeringConnection
    , createVpcPeeringConnection
    , createVpcPeeringConnectionCatch

    -- * CreateVpnConnection
    -- $CreateVpnConnection
    , createVpnConnection
    , createVpnConnectionCatch

    -- * CreateVpnConnectionRoute
    -- $CreateVpnConnectionRoute
    , createVpnConnectionRoute
    , createVpnConnectionRouteCatch

    -- * CreateVpnGateway
    -- $CreateVpnGateway
    , createVpnGateway
    , createVpnGatewayCatch

    -- * DeleteCustomerGateway
    -- $DeleteCustomerGateway
    , deleteCustomerGateway
    , deleteCustomerGatewayCatch

    -- * DeleteDhcpOptions
    -- $DeleteDhcpOptions
    , deleteDhcpOptions
    , deleteDhcpOptionsCatch

    -- * DeleteInternetGateway
    -- $DeleteInternetGateway
    , deleteInternetGateway
    , deleteInternetGatewayCatch

    -- * DeleteKeyPair
    -- $DeleteKeyPair
    , deleteKeyPair
    , deleteKeyPairCatch

    -- * DeleteNetworkAcl
    -- $DeleteNetworkAcl
    , deleteNetworkAcl
    , deleteNetworkAclCatch

    -- * DeleteNetworkAclEntry
    -- $DeleteNetworkAclEntry
    , deleteNetworkAclEntry
    , deleteNetworkAclEntryCatch

    -- * DeleteNetworkInterface
    -- $DeleteNetworkInterface
    , deleteNetworkInterface
    , deleteNetworkInterfaceCatch

    -- * DeletePlacementGroup
    -- $DeletePlacementGroup
    , deletePlacementGroup
    , deletePlacementGroupCatch

    -- * DeleteRoute
    -- $DeleteRoute
    , deleteRoute
    , deleteRouteCatch

    -- * DeleteRouteTable
    -- $DeleteRouteTable
    , deleteRouteTable
    , deleteRouteTableCatch

    -- * DeleteSecurityGroup
    -- $DeleteSecurityGroup
    , deleteSecurityGroup
    , deleteSecurityGroupCatch

    -- * DeleteSnapshot
    -- $DeleteSnapshot
    , deleteSnapshot
    , deleteSnapshotCatch

    -- * DeleteSpotDatafeedSubscription
    -- $DeleteSpotDatafeedSubscription
    , deleteSpotDatafeedSubscription
    , deleteSpotDatafeedSubscriptionCatch

    -- * DeleteSubnet
    -- $DeleteSubnet
    , deleteSubnet
    , deleteSubnetCatch

    -- * DeleteTags
    -- $DeleteTags
    , deleteTags
    , deleteTagsCatch

    -- * DeleteVolume
    -- $DeleteVolume
    , deleteVolume
    , deleteVolumeCatch

    -- * DeleteVpc
    -- $DeleteVpc
    , deleteVpc
    , deleteVpcCatch

    -- * DeleteVpcPeeringConnection
    -- $DeleteVpcPeeringConnection
    , deleteVpcPeeringConnection
    , deleteVpcPeeringConnectionCatch

    -- * DeleteVpnConnection
    -- $DeleteVpnConnection
    , deleteVpnConnection
    , deleteVpnConnectionCatch

    -- * DeleteVpnConnectionRoute
    -- $DeleteVpnConnectionRoute
    , deleteVpnConnectionRoute
    , deleteVpnConnectionRouteCatch

    -- * DeleteVpnGateway
    -- $DeleteVpnGateway
    , deleteVpnGateway
    , deleteVpnGatewayCatch

    -- * DeregisterImage
    -- $DeregisterImage
    , deregisterImage
    , deregisterImageCatch

    -- * DescribeAccountAttributes
    -- $DescribeAccountAttributes
    , describeAccountAttributes
    , describeAccountAttributesCatch

    -- * DescribeAddresses
    -- $DescribeAddresses
    , describeAddresses
    , describeAddressesCatch

    -- * DescribeAvailabilityZones
    -- $DescribeAvailabilityZones
    , describeAvailabilityZones
    , describeAvailabilityZonesCatch

    -- * DescribeBundleTasks
    -- $DescribeBundleTasks
    , describeBundleTasks
    , describeBundleTasksCatch

    -- * DescribeConversionTasks
    -- $DescribeConversionTasks
    , describeConversionTasks
    , describeConversionTasksCatch

    -- * DescribeCustomerGateways
    -- $DescribeCustomerGateways
    , describeCustomerGateways
    , describeCustomerGatewaysCatch

    -- * DescribeDhcpOptions
    -- $DescribeDhcpOptions
    , describeDhcpOptions
    , describeDhcpOptionsCatch

    -- * DescribeExportTasks
    -- $DescribeExportTasks
    , describeExportTasks
    , describeExportTasksCatch

    -- * DescribeImageAttribute
    -- $DescribeImageAttribute
    , describeImageAttribute
    , describeImageAttributeCatch

    -- * DescribeImages
    -- $DescribeImages
    , describeImages
    , describeImagesCatch

    -- * DescribeInstanceAttribute
    -- $DescribeInstanceAttribute
    , describeInstanceAttribute
    , describeInstanceAttributeCatch

    -- * DescribeInstanceStatus
    -- $DescribeInstanceStatus
    , describeInstanceStatus
    , describeInstanceStatusCatch

    -- * DescribeInstances
    -- $DescribeInstances
    , describeInstances
    , describeInstancesCatch

    -- * DescribeInternetGateways
    -- $DescribeInternetGateways
    , describeInternetGateways
    , describeInternetGatewaysCatch

    -- * DescribeKeyPairs
    -- $DescribeKeyPairs
    , describeKeyPairs
    , describeKeyPairsCatch

    -- * DescribeNetworkAcls
    -- $DescribeNetworkAcls
    , describeNetworkAcls
    , describeNetworkAclsCatch

    -- * DescribeNetworkInterfaceAttribute
    -- $DescribeNetworkInterfaceAttribute
    , describeNetworkInterfaceAttribute
    , describeNetworkInterfaceAttributeCatch

    -- * DescribeNetworkInterfaces
    -- $DescribeNetworkInterfaces
    , describeNetworkInterfaces
    , describeNetworkInterfacesCatch

    -- * DescribePlacementGroups
    -- $DescribePlacementGroups
    , describePlacementGroups
    , describePlacementGroupsCatch

    -- * DescribeRegions
    -- $DescribeRegions
    , describeRegions
    , describeRegionsCatch

    -- * DescribeReservedInstances
    -- $DescribeReservedInstances
    , describeReservedInstances
    , describeReservedInstancesCatch

    -- * DescribeReservedInstancesListings
    -- $DescribeReservedInstancesListings
    , describeReservedInstancesListings
    , describeReservedInstancesListingsCatch

    -- * DescribeReservedInstancesModifications
    -- $DescribeReservedInstancesModifications
    , describeReservedInstancesModifications
    , describeReservedInstancesModificationsCatch

    -- * DescribeReservedInstancesOfferings
    -- $DescribeReservedInstancesOfferings
    , describeReservedInstancesOfferings
    , describeReservedInstancesOfferingsCatch

    -- * DescribeRouteTables
    -- $DescribeRouteTables
    , describeRouteTables
    , describeRouteTablesCatch

    -- * DescribeSecurityGroups
    -- $DescribeSecurityGroups
    , describeSecurityGroups
    , describeSecurityGroupsCatch

    -- * DescribeSnapshotAttribute
    -- $DescribeSnapshotAttribute
    , describeSnapshotAttribute
    , describeSnapshotAttributeCatch

    -- * DescribeSnapshots
    -- $DescribeSnapshots
    , describeSnapshots
    , describeSnapshotsCatch

    -- * DescribeSpotDatafeedSubscription
    -- $DescribeSpotDatafeedSubscription
    , describeSpotDatafeedSubscription
    , describeSpotDatafeedSubscriptionCatch

    -- * DescribeSpotInstanceRequests
    -- $DescribeSpotInstanceRequests
    , describeSpotInstanceRequests
    , describeSpotInstanceRequestsCatch

    -- * DescribeSpotPriceHistory
    -- $DescribeSpotPriceHistory
    , describeSpotPriceHistory
    , describeSpotPriceHistoryCatch

    -- * DescribeSubnets
    -- $DescribeSubnets
    , describeSubnets
    , describeSubnetsCatch

    -- * DescribeTags
    -- $DescribeTags
    , describeTags
    , describeTagsCatch

    -- * DescribeVolumeAttribute
    -- $DescribeVolumeAttribute
    , describeVolumeAttribute
    , describeVolumeAttributeCatch

    -- * DescribeVolumeStatus
    -- $DescribeVolumeStatus
    , describeVolumeStatus
    , describeVolumeStatusCatch

    -- * DescribeVolumes
    -- $DescribeVolumes
    , describeVolumes
    , describeVolumesCatch

    -- * DescribeVpcAttribute
    -- $DescribeVpcAttribute
    , describeVpcAttribute
    , describeVpcAttributeCatch

    -- * DescribeVpcPeeringConnections
    -- $DescribeVpcPeeringConnections
    , describeVpcPeeringConnections
    , describeVpcPeeringConnectionsCatch

    -- * DescribeVpcs
    -- $DescribeVpcs
    , describeVpcs
    , describeVpcsCatch

    -- * DescribeVpnConnections
    -- $DescribeVpnConnections
    , describeVpnConnections
    , describeVpnConnectionsCatch

    -- * DescribeVpnGateways
    -- $DescribeVpnGateways
    , describeVpnGateways
    , describeVpnGatewaysCatch

    -- * DetachInternetGateway
    -- $DetachInternetGateway
    , detachInternetGateway
    , detachInternetGatewayCatch

    -- * DetachNetworkInterface
    -- $DetachNetworkInterface
    , detachNetworkInterface
    , detachNetworkInterfaceCatch

    -- * DetachVolume
    -- $DetachVolume
    , detachVolume
    , detachVolumeCatch

    -- * DetachVpnGateway
    -- $DetachVpnGateway
    , detachVpnGateway
    , detachVpnGatewayCatch

    -- * DisableVgwRoutePropagation
    -- $DisableVgwRoutePropagation
    , disableVgwRoutePropagation
    , disableVgwRoutePropagationCatch

    -- * DisassociateAddress
    -- $DisassociateAddress
    , disassociateAddress
    , disassociateAddressCatch

    -- * DisassociateRouteTable
    -- $DisassociateRouteTable
    , disassociateRouteTable
    , disassociateRouteTableCatch

    -- * EnableVgwRoutePropagation
    -- $EnableVgwRoutePropagation
    , enableVgwRoutePropagation
    , enableVgwRoutePropagationCatch

    -- * EnableVolumeIO
    -- $EnableVolumeIO
    , enableVolumeIO
    , enableVolumeIOCatch

    -- * GetConsoleOutput
    -- $GetConsoleOutput
    , getConsoleOutput
    , getConsoleOutputCatch

    -- * GetPasswordData
    -- $GetPasswordData
    , getPasswordData
    , getPasswordDataCatch

    -- * ImportInstance
    -- $ImportInstance
    , importInstance
    , importInstanceCatch

    -- * ImportKeyPair
    -- $ImportKeyPair
    , importKeyPair
    , importKeyPairCatch

    -- * ImportVolume
    -- $ImportVolume
    , importVolume
    , importVolumeCatch

    -- * ModifyImageAttribute
    -- $ModifyImageAttribute
    , modifyImageAttribute
    , modifyImageAttributeCatch

    -- * ModifyInstanceAttribute
    -- $ModifyInstanceAttribute
    , modifyInstanceAttribute
    , modifyInstanceAttributeCatch

    -- * ModifyNetworkInterfaceAttribute
    -- $ModifyNetworkInterfaceAttribute
    , modifyNetworkInterfaceAttribute
    , modifyNetworkInterfaceAttributeCatch

    -- * ModifyReservedInstances
    -- $ModifyReservedInstances
    , modifyReservedInstances
    , modifyReservedInstancesCatch

    -- * ModifySnapshotAttribute
    -- $ModifySnapshotAttribute
    , modifySnapshotAttribute
    , modifySnapshotAttributeCatch

    -- * ModifySubnetAttribute
    -- $ModifySubnetAttribute
    , modifySubnetAttribute
    , modifySubnetAttributeCatch

    -- * ModifyVolumeAttribute
    -- $ModifyVolumeAttribute
    , modifyVolumeAttribute
    , modifyVolumeAttributeCatch

    -- * ModifyVpcAttribute
    -- $ModifyVpcAttribute
    , modifyVpcAttribute
    , modifyVpcAttributeCatch

    -- * MonitorInstances
    -- $MonitorInstances
    , monitorInstances
    , monitorInstancesCatch

    -- * PurchaseReservedInstancesOffering
    -- $PurchaseReservedInstancesOffering
    , purchaseReservedInstancesOffering
    , purchaseReservedInstancesOfferingCatch

    -- * RebootInstances
    -- $RebootInstances
    , rebootInstances
    , rebootInstancesCatch

    -- * RegisterImage
    -- $RegisterImage
    , registerImage
    , registerImageCatch

    -- * RejectVpcPeeringConnection
    -- $RejectVpcPeeringConnection
    , rejectVpcPeeringConnection
    , rejectVpcPeeringConnectionCatch

    -- * ReleaseAddress
    -- $ReleaseAddress
    , releaseAddress
    , releaseAddressCatch

    -- * ReplaceNetworkAclAssociation
    -- $ReplaceNetworkAclAssociation
    , replaceNetworkAclAssociation
    , replaceNetworkAclAssociationCatch

    -- * ReplaceNetworkAclEntry
    -- $ReplaceNetworkAclEntry
    , replaceNetworkAclEntry
    , replaceNetworkAclEntryCatch

    -- * ReplaceRoute
    -- $ReplaceRoute
    , replaceRoute
    , replaceRouteCatch

    -- * ReplaceRouteTableAssociation
    -- $ReplaceRouteTableAssociation
    , replaceRouteTableAssociation
    , replaceRouteTableAssociationCatch

    -- * ReportInstanceStatus
    -- $ReportInstanceStatus
    , reportInstanceStatus
    , reportInstanceStatusCatch

    -- * RequestSpotInstances
    -- $RequestSpotInstances
    , requestSpotInstances
    , requestSpotInstancesCatch

    -- * ResetImageAttribute
    -- $ResetImageAttribute
    , resetImageAttribute
    , resetImageAttributeCatch

    -- * ResetInstanceAttribute
    -- $ResetInstanceAttribute
    , resetInstanceAttribute
    , resetInstanceAttributeCatch

    -- * ResetNetworkInterfaceAttribute
    -- $ResetNetworkInterfaceAttribute
    , resetNetworkInterfaceAttribute
    , resetNetworkInterfaceAttributeCatch

    -- * ResetSnapshotAttribute
    -- $ResetSnapshotAttribute
    , resetSnapshotAttribute
    , resetSnapshotAttributeCatch

    -- * RevokeSecurityGroupEgress
    -- $RevokeSecurityGroupEgress
    , revokeSecurityGroupEgress
    , revokeSecurityGroupEgressCatch

    -- * RevokeSecurityGroupIngress
    -- $RevokeSecurityGroupIngress
    , revokeSecurityGroupIngress
    , revokeSecurityGroupIngressCatch

    -- * RunInstances
    -- $RunInstances
    , runInstances
    , runInstancesCatch

    -- * StartInstances
    -- $StartInstances
    , startInstances
    , startInstancesCatch

    -- * StopInstances
    -- $StopInstances
    , stopInstances
    , stopInstancesCatch

    -- * TerminateInstances
    -- $TerminateInstances
    , terminateInstances
    , terminateInstancesCatch

    -- * UnassignPrivateIpAddresses
    -- $UnassignPrivateIpAddresses
    , unassignPrivateIpAddresses
    , unassignPrivateIpAddressesCatch

    -- * UnmonitorInstances
    -- $UnmonitorInstances
    , unmonitorInstances
    , unmonitorInstancesCatch

    -- * Re-exported
    , module AWS
    , module Network.AWS.EC2.V2014_06_15
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.EC2.V2014_06_15

type ServiceErr = EC2Error


-- $AcceptVpcPeeringConnection
-- Accept a VPC peering connection request. To accept a request, the VPC
-- peering connection must be in the pending-acceptance state, and you must be
-- the owner of the peer VPC. Use the DescribeVpcPeeringConnections request to
-- view your outstanding VPC peering connection requests. Example This example
-- accepts the specified VPC peering connection request.
-- https://ec2.amazonaws.com/?Action=AcceptVpcPeeringConnection
-- &amp;vpcPeeringConnectionId=pcx-1a2b3c4d &amp;AUTHPARAMS
-- &lt;AcceptVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcPeeringConnection&gt;
-- &lt;vpcPeeringConnectionId&gt;pcx-1a2b3c4d&lt;/vpcPeeringConnectionId&gt;
-- &lt;requesterVpcInfo&gt; &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.0.0/28&lt;/cidrBlock&gt; &lt;/requesterVpcInfo&gt;
-- &lt;accepterVpcInfo&gt; &lt;ownerId&gt;777788889999&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-111aaa22&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.1.0/28&lt;/cidrBlock&gt; &lt;/accepterVpcInfo&gt;
-- &lt;status&gt; &lt;code&gt;active&lt;/code&gt;
-- &lt;message&gt;Active&lt;/message&gt; &lt;/status&gt; &lt;tagSet/&gt;
-- &lt;/vpcPeeringConnection&gt; &lt;/AcceptVpcPeeringConnectionResponse&gt;".
--
-- See: 'Network.AWS.EC2.V2014_06_15.AcceptVpcPeeringConnection'

acceptVpcPeeringConnection :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => State AcceptVpcPeeringConnection a
    -> m AcceptVpcPeeringConnectionResponse
acceptVpcPeeringConnection s =
    send (mkAcceptVpcPeeringConnection &~ s)

acceptVpcPeeringConnectionCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => State AcceptVpcPeeringConnection a
    -> m (Either ServiceErr AcceptVpcPeeringConnectionResponse)
acceptVpcPeeringConnectionCatch s =
    sendCatch (mkAcceptVpcPeeringConnection &~ s)

-- $AllocateAddress
-- Acquires an Elastic IP address. An Elastic IP address is for use either in
-- the EC2-Classic platform or in a VPC. For more information, see Elastic IP
-- Addresses in the Amazon Elastic Compute Cloud User Guide. Example for
-- EC2-Classic This example request allocates an Elastic IP address for use
-- with instances in EC2-Classic.
-- https://ec2.amazonaws.com/?Action=AllocateAddress &amp;AUTHPARAMS
-- &lt;AllocateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;publicIp&gt;192.0.2.1&lt;/publicIp&gt;
-- &lt;domain&gt;standard&lt;/domain&gt; &lt;/AllocateAddressResponse&gt;
-- Example for EC2-VPC This example request allocates an Elastic IP address
-- for use with instances in a VPC.
-- https://ec2.amazonaws.com/?Action=AllocateAddress Domain=vpc
-- &amp;AUTHPARAMS &lt;AllocateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;publicIp&gt;198.51.100.1&lt;/publicIp&gt;
-- &lt;domain&gt;vpc&lt;/domain&gt;
-- &lt;allocationId&gt;eipalloc-5723d13e&lt;/allocationId&gt;
-- &lt;/AllocateAddressResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AllocateAddress'

allocateAddress :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => State AllocateAddress a
    -> m AllocateAddressResponse
allocateAddress s =
    send (mkAllocateAddress &~ s)

allocateAddressCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => State AllocateAddress a
    -> m (Either ServiceErr AllocateAddressResponse)
allocateAddressCatch s =
    sendCatch (mkAllocateAddress &~ s)

-- $AssignPrivateIpAddresses
-- Assigns one or more secondary private IP addresses to the specified network
-- interface. You can specify one or more specific secondary IP addresses, or
-- you can specify the number of secondary IP addresses to be automatically
-- assigned within the subnet's CIDR block range. The number of secondary IP
-- addresses that you can assign to an instance varies by instance type. For
-- information about instance types, see Instance Types in the Amazon Elastic
-- Compute Cloud User Guide. For more information about Elastic IP addresses,
-- see Elastic IP Addresses in the Amazon Elastic Compute Cloud User Guide.
-- AssignPrivateIpAddresses is available only in EC2-VPC. Example 1 This
-- example assigns two secondary private IP addresses (10.0.2.1 and 10.0.2.11)
-- to the specified network interface.
-- https://ec2.amazonaws.com/?Action=AssignPrivateIpAddresses
-- &amp;NetworkInterfaceId=eni-d83388b1 &amp;PrivateIpAddress.0=10.0.2.1
-- &amp;PrivateIpAddress.1=10.0.2.11 &amp;AUTHPARAMS
-- &lt;AssignPrivateIpAddresses
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssignPrivateIpAddresses&gt; Example
-- 2 This example assigns two secondary private IP addresses to the specified
-- network interface. Amazon EC2 automatically assigns these IP addresses from
-- the available IP addresses within the subnet's CIDR block range.
-- https://ec2.amazonaws.com/?Action=AssignPrivateIpAddresses
-- &amp;NetworkInterfaceId=eni-d83388b1 &amp;SecondaryPrivateIpAddressCount=2
-- &amp;AUTHPARAMS &lt;AssignPrivateIpAddresses
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssignPrivateIpAddresses&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AssignPrivateIpAddresses'

assignPrivateIpAddresses :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'apiaNetworkInterfaceId'
    -> State AssignPrivateIpAddresses a
    -> m AssignPrivateIpAddressesResponse
assignPrivateIpAddresses p1 s =
    send $ (mkAssignPrivateIpAddresses p1) &~ s

assignPrivateIpAddressesCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'apiaNetworkInterfaceId'
    -> State AssignPrivateIpAddresses a
    -> m (Either ServiceErr AssignPrivateIpAddressesResponse)
assignPrivateIpAddressesCatch p1 s =
    sendCatch $ (mkAssignPrivateIpAddresses p1) &~ s

-- $AssociateAddress
-- Associates an Elastic IP address with an instance or a network interface.
-- An Elastic IP address is for use in either the EC2-Classic platform or in a
-- VPC. For more information, see Elastic IP Addresses in the Amazon Elastic
-- Compute Cloud User Guide. [EC2-Classic, default VPC] If the Elastic IP
-- address is already associated with a different instance, it is
-- disassociated from that instance and associated with the specified
-- instance. [EC2-VPC] If you don't specify a private IP address, the Elastic
-- IP address is associated with the primary IP address. If the Elastic IP
-- address is already associated with a different instance or a network
-- interface, you get an error unless you allow reassociation. This is an
-- idempotent operation. If you perform the operation more than once, Amazon
-- EC2 doesn't return an error. Example for EC2-Classic This example request
-- associates an Elastic IP address with an instance in EC2-Classic.
-- https://ec2.amazonaws.com/?Action=AssociateAddress
-- &amp;InstanceId=i-2ea64347 &amp;PublicIp=192.0.2.1 &amp;AUTHPARAMS
-- &lt;AssociateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssociateAddressResponse&gt; Example
-- for EC2-VPC This example request associates a Elastic IP address with an
-- instance in a VPC. The AllowReassignment parameter allows the Elastic IP
-- address to be associated with the specified instance even if it's already
-- associated with a different instance or a network interface.
-- https://ec2.amazonaws.com/?Action=AssociateAddress
-- &amp;InstanceId=i-4fd2431a &amp;AllocationId=eipalloc-5723d13e
-- &amp;AllowReassignment=true &amp;AUTHPARAMS &lt;AssociateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;associationId&gt;eipassoc-fc5ca095&lt;/associationId&gt;
-- &lt;/AssociateAddressResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AssociateAddress'

associateAddress :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => State AssociateAddress a
    -> m AssociateAddressResponse
associateAddress s =
    send (mkAssociateAddress &~ s)

associateAddressCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => State AssociateAddress a
    -> m (Either ServiceErr AssociateAddressResponse)
associateAddressCatch s =
    sendCatch (mkAssociateAddress &~ s)

-- $AssociateDhcpOptions
-- Associates a set of DHCP options (that you've previously created) with the
-- specified VPC, or associates no DHCP options with the VPC. After you
-- associate the options with the VPC, any existing instances and all new
-- instances that you launch in that VPC use the options. You don't need to
-- restart or relaunch the instances. They automatically pick up the changes
-- within a few hours, depending on how frequently the instance renews its
-- DHCP lease. You can explicitly renew the lease using the operating system
-- on the instance. For more information, see DHCP Options Sets in the Amazon
-- Virtual Private Cloud User Guide. Example 1 This example associates the
-- DHCP options with the ID dopt-7a8b9c2d with the VPC with the ID
-- vpc-1a2b3c4d. https://ec2.amazonaws.com/?Action=AssociateDhcpOptions
-- &amp;DhcpOptionsId=dopt-7a8b9c2d &amp;VpcId=vpc-1a2b3c4d &amp;AUTHPARAMS
-- &lt;AssociateDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssociateDhcpOptionsResponse&gt;
-- Example 2 This example changes the VPC with the ID vpc-1a2b3c4d to have no
-- associated DHCP options set.
-- https://ec2.amazonaws.com/?Action=AssociateDhcpOptions
-- &amp;DhcpOptionsId=default &amp;VpcId=vpc-1a2b3c4d &amp;AUTHPARAMS
-- &lt;AssociateDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssociateDhcpOptionsResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AssociateDhcpOptions'

associateDhcpOptions :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'adoDhcpOptionsId'
    -> Text -- ^ 'adoVpcId'
    -> State AssociateDhcpOptions a
    -> m AssociateDhcpOptionsResponse
associateDhcpOptions p1 p2 s =
    send $ (mkAssociateDhcpOptions p1 p2) &~ s

associateDhcpOptionsCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'adoDhcpOptionsId'
    -> Text -- ^ 'adoVpcId'
    -> State AssociateDhcpOptions a
    -> m (Either ServiceErr AssociateDhcpOptionsResponse)
associateDhcpOptionsCatch p1 p2 s =
    sendCatch $ (mkAssociateDhcpOptions p1 p2) &~ s

-- $AssociateRouteTable
-- Associates a subnet with a route table. The subnet and route table must be
-- in the same VPC. This association causes traffic originating from the
-- subnet to be routed according to the routes in the route table. The action
-- returns an association ID, which you need in order to disassociate the
-- route table from the subnet later. A route table can be associated with
-- multiple subnets. For more information about route tables, see Route Tables
-- in the Amazon Virtual Private Cloud User Guide. Example This example
-- associates a route table with the ID rtb-e4ad488d with a subnet with the ID
-- subnet-15ad487c. https://ec2.amazonaws.com/?Action=AssociateRouteTable
-- &amp;RouteTableId=rtb-e4ad488d &amp;SubnetId=subnet-15ad487c
-- &lt;AssociateRouteTableResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;associationId&gt;rtbassoc-f8ad4891&lt;/associationId&gt;
-- &lt;/AssociateRouteTableResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AssociateRouteTable'

associateRouteTable :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'artSubnetId'
    -> Text -- ^ 'artRouteTableId'
    -> State AssociateRouteTable a
    -> m AssociateRouteTableResponse
associateRouteTable p1 p2 s =
    send $ (mkAssociateRouteTable p1 p2) &~ s

associateRouteTableCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'artSubnetId'
    -> Text -- ^ 'artRouteTableId'
    -> State AssociateRouteTable a
    -> m (Either ServiceErr AssociateRouteTableResponse)
associateRouteTableCatch p1 p2 s =
    sendCatch $ (mkAssociateRouteTable p1 p2) &~ s

-- $AttachInternetGateway
-- Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, see the Amazon Virtual Private Cloud User Guide. Example This
-- example attaches the Internet gateway with the ID igw-eaad4883 to the VPC
-- with the ID vpc-11ad4878.
-- https://ec2.amazonaws.com/?Action=AttachInternetGateway
-- &amp;InternetGatewayId=igw-eaad4883 &amp;VpcId=vpc-11ad4878 &amp;AUTHPARAMS
-- &lt;AttachInternetGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AttachInternetGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AttachInternetGateway'

attachInternetGateway :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'aigInternetGatewayId'
    -> Text -- ^ 'aigVpcId'
    -> State AttachInternetGateway a
    -> m AttachInternetGatewayResponse
attachInternetGateway p1 p2 s =
    send $ (mkAttachInternetGateway p1 p2) &~ s

attachInternetGatewayCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'aigInternetGatewayId'
    -> Text -- ^ 'aigVpcId'
    -> State AttachInternetGateway a
    -> m (Either ServiceErr AttachInternetGatewayResponse)
attachInternetGatewayCatch p1 p2 s =
    sendCatch $ (mkAttachInternetGateway p1 p2) &~ s

-- $AttachNetworkInterface
-- Attaches a network interface to an instance. Example This example attaches
-- the specified network interface to the specified instance.
-- https://ec2.amazonaws.com/?Action=AttachNetworkInterface &amp;DeviceIndex=1
-- &amp;InstanceId=i-9cc316fe &amp;NetworkInterfaceId=eni-ffda3197
-- &amp;AUTHPARAMS &lt;AttachNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;ace8cd1e-e685-4e44-90fb-92014d907212&lt;/requestId&gt;
-- &lt;attachmentId&gt;eni-attach-d94b09b0&lt;/attachmentId&gt;
-- &lt;/AttachNetworkInterfaceResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AttachNetworkInterface'

attachNetworkInterface :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'aniNetworkInterfaceId'
    -> Text -- ^ 'aniInstanceId'
    -> Integer -- ^ 'aniDeviceIndex'
    -> State AttachNetworkInterface a
    -> m AttachNetworkInterfaceResponse
attachNetworkInterface p1 p2 p3 s =
    send $ (mkAttachNetworkInterface p1 p2 p3) &~ s

attachNetworkInterfaceCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'aniNetworkInterfaceId'
    -> Text -- ^ 'aniInstanceId'
    -> Integer -- ^ 'aniDeviceIndex'
    -> State AttachNetworkInterface a
    -> m (Either ServiceErr AttachNetworkInterfaceResponse)
attachNetworkInterfaceCatch p1 p2 p3 s =
    sendCatch $ (mkAttachNetworkInterface p1 p2 p3) &~ s

-- $AttachVolume
-- Attaches an Amazon EBS volume to a running or stopped instance and exposes
-- it to the instance with the specified device name. Encrypted Amazon EBS
-- volumes may only be attached to instances that support Amazon EBS
-- encryption. For more information, see Amazon EBS Encryption in the Amazon
-- Elastic Compute Cloud User Guide. For a list of supported device names, see
-- Attaching an Amazon EBS Volume to an Instance. Any device names that aren't
-- reserved for instance store volumes can be used for Amazon EBS volumes. For
-- more information, see Amazon EC2 Instance Store in the Amazon Elastic
-- Compute Cloud User Guide. If a volume has an AWS Marketplace product code:
-- The volume can only be attached as the root device of a stopped instance.
-- You must be subscribed to the AWS Marketplace code that is on the volume.
-- The configuration (instance type, operating system) of the instance must
-- support that specific AWS Marketplace code. For example, you cannot take a
-- volume from a Windows instance and attach it to a Linux instance. AWS
-- Marketplace product codes are copied from the volume to the instance. For
-- an overview of the AWS Marketplace, see
-- https://aws.amazon.com/marketplace/help/200900000. For more information
-- about how to use the AWS Marketplace, see AWS Marketplace. For more
-- information about Amazon EBS volumes, see Attaching Amazon EBS Volumes in
-- the Amazon Elastic Compute Cloud User Guide. Example 1 This example request
-- attaches the volume with the ID vol-1a2b3c4d to the instance with the ID
-- i-1a2b3c4d and exposes it as /dev/sdh.
-- https://ec2.amazonaws.com/?Action=AttachVolume &amp;VolumeId=vol-1a2b3c4d
-- &amp;InstanceId=i-1a2b3c4d &amp;Device=/dev/sdh &amp;AUTHPARAMS
-- &lt;AttachVolumeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;instanceId&gt;i-1a2b3c4d&lt;/instanceId&gt;
-- &lt;device&gt;/dev/sdh&lt;/device&gt;
-- &lt;status&gt;attaching&lt;/status&gt;
-- &lt;attachTime&gt;YYYY-MM-DDTHH:MM:SS.000Z&lt;/attachTime&gt;
-- &lt;/AttachVolumeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AttachVolume'

attachVolume :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'avVolumeId'
    -> Text -- ^ 'avInstanceId'
    -> Text -- ^ 'avDevice'
    -> State AttachVolume a
    -> m AttachVolumeResponse
attachVolume p1 p2 p3 s =
    send $ (mkAttachVolume p1 p2 p3) &~ s

attachVolumeCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'avVolumeId'
    -> Text -- ^ 'avInstanceId'
    -> Text -- ^ 'avDevice'
    -> State AttachVolume a
    -> m (Either ServiceErr AttachVolumeResponse)
attachVolumeCatch p1 p2 p3 s =
    sendCatch $ (mkAttachVolume p1 p2 p3) &~ s

-- $AttachVpnGateway
-- Attaches a virtual private gateway to a VPC. For more information, see
-- Adding a Hardware Virtual Private Gateway to Your VPC in the Amazon Virtual
-- Private Cloud User Guide. Example This example attaches the virtual private
-- gateway with the ID vgw-8db04f81 to the VPC with the ID vpc-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=AttachVpnGateway
-- &amp;VpnGatewayId=vgw-8db04f81 &amp;VpcId=vpc-1a2b3c4d &amp;AUTHPARAMS
-- &lt;AttachVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;attachment&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;state&gt;attaching&lt;/state&gt; &lt;/attachment&gt;
-- &lt;/AttachVpnGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AttachVpnGateway'

attachVpnGateway :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'avgVpnGatewayId'
    -> Text -- ^ 'avgVpcId'
    -> State AttachVpnGateway a
    -> m AttachVpnGatewayResponse
attachVpnGateway p1 p2 s =
    send $ (mkAttachVpnGateway p1 p2) &~ s

attachVpnGatewayCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'avgVpnGatewayId'
    -> Text -- ^ 'avgVpcId'
    -> State AttachVpnGateway a
    -> m (Either ServiceErr AttachVpnGatewayResponse)
attachVpnGatewayCatch p1 p2 s =
    sendCatch $ (mkAttachVpnGateway p1 p2) &~ s

-- $AuthorizeSecurityGroupEgress
-- Adds one or more egress rules to a security group for use with a VPC.
-- Specifically, this action permits instances to send traffic to one or more
-- CIDR IP address ranges, or to one or more security groups for the same VPC.
-- You can have up to 50 rules per security group (covering both ingress and
-- egress rules). A security group is for use with instances either in the
-- EC2-Classic platform or in a specific VPC. This action doesn't apply to
-- security groups for use in EC2-Classic. For more information, see Security
-- Groups for Your VPC in the Amazon Virtual Private Cloud User Guide. Each
-- rule consists of the protocol (for example, TCP), plus either a CIDR range
-- or a source group. For the TCP and UDP protocols, you must also specify the
-- destination port or port range. For the ICMP protocol, you must also
-- specify the ICMP type and code. You can use -1 for the type or code to mean
-- all types or all codes. Rule changes are propagated to affected instances
-- as quickly as possible. However, a small delay might occur. Example 1 This
-- example request grants your security group with the ID sg-1a2b3c4d access
-- to the 192.0.2.0/24 and 198.51.100.0/24 address ranges on TCP port 80.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupEgress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=192.0.2.0/24
-- &amp;IpPermissions.1.IpRanges.2.CidrIp=198.51.100.0/24 &amp;AUTHPARAMS
-- &lt;AuthorizeSecurityGroupEgressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/AuthorizeSecurityGroupEgressResponse&gt; Example 2 This example
-- request grants egress access from the security group with the ID
-- sg-1a2b3c4d to the security group with the ID sg-9a8d7f5c on TCP port 1433.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupEgress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=1433 &amp;IpPermissions.1.ToPort=1433
-- &amp;IpPermissions.1.Groups.1.GroupId=sg-9a8d7f5c &amp;AUTHPARAMS
-- &lt;AuthorizeSecurityGroupEgressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/AuthorizeSecurityGroupEgressResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.AuthorizeSecurityGroupEgress'

authorizeSecurityGroupEgress :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => Text -- ^ 'asgeGroupId'
    -> State AuthorizeSecurityGroupEgress a
    -> m AuthorizeSecurityGroupEgressResponse
authorizeSecurityGroupEgress p1 s =
    send $ (mkAuthorizeSecurityGroupEgress p1) &~ s

authorizeSecurityGroupEgressCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'asgeGroupId'
    -> State AuthorizeSecurityGroupEgress a
    -> m (Either ServiceErr AuthorizeSecurityGroupEgressResponse)
authorizeSecurityGroupEgressCatch p1 s =
    sendCatch $ (mkAuthorizeSecurityGroupEgress p1) &~ s

-- $AuthorizeSecurityGroupIngress
-- Adds one or more ingress rules to a security group. EC2-Classic: You can
-- have up to 100 rules per group. EC2-VPC: You can have up to 50 rules per
-- group (covering both ingress and egress rules). Rule changes are propagated
-- to instances within the security group as quickly as possible. However, a
-- small delay might occur. [EC2-Classic] This action gives one or more CIDR
-- IP address ranges permission to access a security group in your account, or
-- gives one or more security groups (called the source groups) permission to
-- access a security group for your account. A source group can be for your
-- own AWS account, or another. [EC2-VPC] This action gives one or more CIDR
-- IP address ranges permission to access a security group in your VPC, or
-- gives one or more other security groups (called the source groups)
-- permission to access a security group for your VPC. The security groups
-- must all be for the same VPC. Example 1 This example request grants TCP
-- port 80 access from the 192.0.2.0/24 and 198.51.100.0/24 address ranges to
-- the security group for EC2-Classic named websrv.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupIngress
-- &amp;GroupName=websrv &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=192.0.2.0/24
-- &amp;IpPermissions.1.IpRanges.2.CidrIp=198.51.100.0/24 &amp;AUTHPARAMS
-- Example 2 This example request grants TCP port 80 access from the source
-- group for EC2-Classic named OtherAccountGroup (in AWS account 123456789012)
-- to the security group for EC2-Classic named websrv.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupIngress
-- &amp;GroupName=websrv &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.Groups.1.GroupName=OtherAccountGroup
-- &amp;IpPermissions.1.Groups.1.UserId=123456789012 &amp;AUTHPARAMS Example 3
-- This example request grants TCP port 80 access from the source group named
-- OtherGroupInMyVPC (with the ID sg-2a2b3c4d) to the security group named
-- VpcWebServers (with the ID sg-1a2b3c4d). In EC2-VPC, you must use the
-- security group IDs in a request, not the security group names. In this
-- example, your AWS account ID is 123456789012.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupIngress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.Groups.1.GroupId=sg-2a2b3c4d
-- &amp;IpPermissions.1.Groups.1.UserId=123456789012 &amp;AUTHPARAMS Example 4
-- This example request grants your local system the ability to use SSH (port
-- 22) to connect to any instance in the security group named default.
-- https://ec2.amazonaws.com/ ?Action=AuthorizeSecurityGroupIngress
-- &amp;GroupName=default &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=22 &amp;IpPermissions.1.ToPort=22
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=your-local-system's-public-ip-address/32
-- &amp;AUTHPARAMS Example 5 This example request grants your local system the
-- ability to use Remote Desktop (port 3389) to connect to any instance in the
-- security group named default. https://ec2.amazonaws.com/
-- ?Action=AuthorizeSecurityGroupIngress &amp;GroupName=default
-- &amp;IpPermissions.1.IpProtocol=tcp &amp;IpPermissions.1.FromPort=3389
-- &amp;IpPermissions.1.ToPort=3389
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=your-local-system's-public-ip-address/32.
-- 
--
-- See: 'Network.AWS.EC2.V2014_06_15.AuthorizeSecurityGroupIngress'

authorizeSecurityGroupIngress :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 )
    => State AuthorizeSecurityGroupIngress a
    -> m AuthorizeSecurityGroupIngressResponse
authorizeSecurityGroupIngress s =
    send (mkAuthorizeSecurityGroupIngress &~ s)

authorizeSecurityGroupIngressCatch :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadReader Env m
                                      )
    => State AuthorizeSecurityGroupIngress a
    -> m (Either ServiceErr AuthorizeSecurityGroupIngressResponse)
authorizeSecurityGroupIngressCatch s =
    sendCatch (mkAuthorizeSecurityGroupIngress &~ s)

-- $BundleInstance
-- Bundles an Amazon instance store-backed Windows instance. During bundling,
-- only the root device volume (C:\) is bundled. Data on other instance store
-- volumes is not preserved. This procedure is not applicable for Linux/Unix
-- instances or Windows instances that are backed by Amazon EBS. For more
-- information, see Creating an Instance Store-Backed Windows AMI. Example
-- This example request bundles the specified instance. Before you specify a
-- value for your access key ID, review and follow the guidance in Best
-- Practices for Managing AWS Access Keys.
-- https://ec2.amazonaws.com/?Action=BundleInstance &amp;InstanceId=i-e468cd8d
-- &amp;Storage.S3.AWSAccessKeyId='AKIAIOSFODNN7EXAMPLE'
-- &amp;Storage.S3.Bucket=myawsbucket &amp;Storage.S3.Prefix=winami
-- &amp;Storage.S3.UploadPolicy=eyJleHBpcmF0aW9uIjogIjIwMDgtMDgtMzBUMDg6NDk6MD
-- laIiwiY29uZGl0aW9ucyI6IFt7ImJ1Y2tldCI6ICJteS1idWNrZXQifSxbInN0YXJ0cy13aXRoIiwgI
-- iRrZXkiLCAibXktbmV3LWltYWdlIl0seyJhY2wiOiAiZWMyLWJ1bmRsZS1yZWFkIn1dfEXAMPLE
-- &amp;Storage.S3.UploadPolicySignature=fh5tyyyQD8W4COEthj3nlGNEXAMPLE
-- &amp;AUTHPARAMS &lt;BundleInstanceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;bundleInstanceTask&gt; &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;bundleId&gt;bun-c1a540a8&lt;/bundleId&gt;
-- &lt;state&gt;bundling&lt;/state&gt;
-- &lt;startTime&gt;2008-10-07T11:41:50.000Z&lt;/startTime&gt;
-- &lt;updateTime&gt;2008-10-07T11:51:50.000Z&lt;/updateTime&gt;
-- &lt;progress&gt;70%&lt;/progress&gt; &lt;storage&gt; &lt;S3&gt;
-- &lt;bucket&gt;myawsbucket&lt;/bucket&gt;
-- &lt;prefix&gt;winami&lt;/prefix&gt; &lt;/S3&gt; &lt;/storage&gt;
-- &lt;/bundleInstanceTask&gt; &lt;/BundleInstanceResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.BundleInstance'

bundleInstance :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'biInstanceId'
    -> Storage -- ^ 'biStorage'
    -> State BundleInstance a
    -> m BundleInstanceResponse
bundleInstance p1 p2 s =
    send $ (mkBundleInstance p1 p2) &~ s

bundleInstanceCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'biInstanceId'
    -> Storage -- ^ 'biStorage'
    -> State BundleInstance a
    -> m (Either ServiceErr BundleInstanceResponse)
bundleInstanceCatch p1 p2 s =
    sendCatch $ (mkBundleInstance p1 p2) &~ s

-- $CancelBundleTask
-- Cancels a bundling operation for an instance store-backed Windows instance.
-- Example This example request cancels the specified bundle task.
-- https://ec2.amazonaws.com/?Action=CancelBundleTask
-- &amp;BundleId=bun-cla322b9 &amp;AUTHPARAMS &lt;CancelBundleTaskResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;bundleInstanceTask&gt; &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;bundleId&gt;bun-cla322b9&lt;/bundleId&gt;
-- &lt;state&gt;canceling&lt;/state&gt;
-- &lt;startTime&gt;2008-10-07T11:41:50.000Z&lt;/startTime&gt;
-- &lt;updateTime&gt;2008-10-07T11:51:50.000Z&lt;/updateTime&gt;
-- &lt;progress&gt;20%&lt;/progress&gt; &lt;storage&gt; &lt;S3&gt;
-- &lt;bucket&gt;myawsbucket&lt;/bucket&gt;
-- &lt;prefix&gt;my-new-image&lt;/prefix&gt; &lt;/S3&gt; &lt;/storage&gt;
-- &lt;/bundleInstanceTask&gt; &lt;/CancelBundleTaskResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CancelBundleTask'

cancelBundleTask :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'cbtBundleId'
    -> State CancelBundleTask a
    -> m CancelBundleTaskResponse
cancelBundleTask p1 s =
    send $ (mkCancelBundleTask p1) &~ s

cancelBundleTaskCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'cbtBundleId'
    -> State CancelBundleTask a
    -> m (Either ServiceErr CancelBundleTaskResponse)
cancelBundleTaskCatch p1 s =
    sendCatch $ (mkCancelBundleTask p1) &~ s

-- $CancelConversionTask
-- Cancels an active conversion task. The task can be the import of an
-- instance or volume. The action removes all artifacts of the conversion,
-- including a partially uploaded volume or instance. If the conversion is
-- complete or is in the process of transferring the final disk image, the
-- command fails and returns an exception. For more information, see Using the
-- Command Line Tools to Import Your Virtual Machine to Amazon EC2 in the
-- Amazon Elastic Compute Cloud User Guide. Example This example request
-- cancels the conversion task with the ID import-i-fh95npoc.
-- https://ec2.amazonaws.com/?Action=CancelConversionTask
-- &amp;ConversionTaskId=import-i-fh95npoc &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE true.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CancelConversionTask'

cancelConversionTask :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'cctConversionTaskId'
    -> State CancelConversionTask a
    -> m CancelConversionTaskResponse
cancelConversionTask p1 s =
    send $ (mkCancelConversionTask p1) &~ s

cancelConversionTaskCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'cctConversionTaskId'
    -> State CancelConversionTask a
    -> m (Either ServiceErr CancelConversionTaskResponse)
cancelConversionTaskCatch p1 s =
    sendCatch $ (mkCancelConversionTask p1) &~ s

-- $CancelExportTask
-- Cancels an active export task. The request removes all artifacts of the
-- export, including any partially-created Amazon S3 objects. If the export
-- task is complete or is in the process of transferring the final disk image,
-- the command fails and returns an error. Example This example request
-- cancels the export task with the ID export-i-1234wxyz.
-- https://ec2.amazonaws.com/?Action=CancelExportTask
-- &amp;exportTaskId=export-i-1234wxyz &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE true.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CancelExportTask'

cancelExportTask :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'cetExportTaskId'
    -> State CancelExportTask a
    -> m CancelExportTaskResponse
cancelExportTask p1 s =
    send $ (mkCancelExportTask p1) &~ s

cancelExportTaskCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'cetExportTaskId'
    -> State CancelExportTask a
    -> m (Either ServiceErr CancelExportTaskResponse)
cancelExportTaskCatch p1 s =
    sendCatch $ (mkCancelExportTask p1) &~ s

-- $CancelReservedInstancesListing
-- Cancels the specified Reserved Instance listing in the Reserved Instance
-- Marketplace. For more information, see Reserved Instance Marketplace in the
-- Amazon Elastic Compute Cloud User Guide. Example This example request
-- cancels a Reserved Instance listing in the Reserved Instance Marketplace.
-- https://ec2.amazonaws.com/?Action=CancelReservedInstancesListing
-- &amp;ReservedInstancesListingId=3ebe97b5-f273-43b6-a204-7a18cEXAMPLE
-- &amp;AUTHPARAMS bec2cf62-98ef-434a-8a15-886fcexample
-- 3ebe97b5-f273-43b6-a204-7a18cEXAMPLE e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- 2012-07-12T16:55:28.000Z 2012-07-12T16:55:28.000Z cancelled CANCELLED
-- Available 0 Sold 0 Cancelled 1 Pending 0 5 166.64 USD false 4 133.32 USD
-- false 3 99.99 USD false 2 66.66 USD false 1 33.33 USD false
-- XqJIt1342112125076.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CancelReservedInstancesListing'

cancelReservedInstancesListing :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'crilReservedInstancesListingId'
    -> State CancelReservedInstancesListing a
    -> m CancelReservedInstancesListingResponse
cancelReservedInstancesListing p1 s =
    send $ (mkCancelReservedInstancesListing p1) &~ s

cancelReservedInstancesListingCatch :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadReader Env m
                                       )
    => Text -- ^ 'crilReservedInstancesListingId'
    -> State CancelReservedInstancesListing a
    -> m (Either ServiceErr CancelReservedInstancesListingResponse)
cancelReservedInstancesListingCatch p1 s =
    sendCatch $ (mkCancelReservedInstancesListing p1) &~ s

-- $CancelSpotInstanceRequests
-- Cancels one or more Spot Instance requests. Spot Instances are instances
-- that Amazon EC2 starts on your behalf when the maximum price that you
-- specify exceeds the current Spot Price. Amazon EC2 periodically sets the
-- Spot Price based on available Spot Instance capacity and current Spot
-- Instance requests. For more information about Spot Instances, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide. Canceling a Spot
-- Instance request does not terminate running Spot Instances associated with
-- the request. Example This example cancels the specified Spot Instance
-- request. https://ec2.amazonaws.com/?Action=CancelSpotInstanceRequests
-- &amp;SpotInstanceRequestId.1=sir-1a2b3c4d &amp;AUTHPARAMS
-- &lt;CancelSpotInstanceRequestsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;spotInstanceRequestSet&gt; &lt;item&gt;
-- &lt;spotInstanceRequestId&gt;sir-1a2b3c4d&lt;/spotInstanceRequestId&gt;
-- &lt;state&gt;cancelled&lt;/state&gt; &lt;/item&gt;
-- &lt;/spotInstanceRequestSet&gt;
-- &lt;/CancelSpotInstanceRequestsResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CancelSpotInstanceRequests'

cancelSpotInstanceRequests :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => [Text] -- ^ 'csirSpotInstanceRequestIds'
    -> State CancelSpotInstanceRequests a
    -> m CancelSpotInstanceRequestsResponse
cancelSpotInstanceRequests p1 s =
    send $ (mkCancelSpotInstanceRequests p1) &~ s

cancelSpotInstanceRequestsCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => [Text] -- ^ 'csirSpotInstanceRequestIds'
    -> State CancelSpotInstanceRequests a
    -> m (Either ServiceErr CancelSpotInstanceRequestsResponse)
cancelSpotInstanceRequestsCatch p1 s =
    sendCatch $ (mkCancelSpotInstanceRequests p1) &~ s

-- $ConfirmProductInstance
-- Determines whether a product code is associated with an instance. This
-- action can only be used by the owner of the product code. It is useful when
-- a product code owner needs to verify whether another user's instance is
-- eligible for support. Example This example determines whether the specified
-- product code is associated with the specified instance.
-- https://ec2.amazonaws.com/?Action=ConfirmProductInstance
-- &amp;ProductCode=774F4FF8 &amp;InstanceId=i-10a64379 &amp;AUTHPARAMS
-- &lt;ConfirmProductInstanceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;ownerId&gt;111122223333&lt;/ownerId&gt;
-- &lt;/ConfirmProductInstanceResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ConfirmProductInstance'

confirmProductInstance :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'cpiProductCode'
    -> Text -- ^ 'cpiInstanceId'
    -> State ConfirmProductInstance a
    -> m ConfirmProductInstanceResponse
confirmProductInstance p1 p2 s =
    send $ (mkConfirmProductInstance p1 p2) &~ s

confirmProductInstanceCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'cpiProductCode'
    -> Text -- ^ 'cpiInstanceId'
    -> State ConfirmProductInstance a
    -> m (Either ServiceErr ConfirmProductInstanceResponse)
confirmProductInstanceCatch p1 p2 s =
    sendCatch $ (mkConfirmProductInstance p1 p2) &~ s

-- $CopyImage
-- Initiates the copy of an AMI from the specified source region to the region
-- in which the request was made. You specify the destination region by using
-- its endpoint when making the request. AMIs that use encrypted Amazon EBS
-- snapshots cannot be copied with this method. For more information, see
-- Copying AMIs in the Amazon Elastic Compute Cloud User Guide. Example This
-- example request copies the AMI in us-west-2 with the ID ami-1a2b3c4d,
-- naming the new AMI My-Standard-AMI.
-- https://ec2.amazonaws.com/?Action=CopyImage &amp;SourceRegion=us-west-2
-- &amp;SourceImageId=ami-1a2b3c4d &amp;Name=My-Standard-AMI
-- &amp;Description=This%20is%20the%20new%20version%20of%20My-Standard-AMI
-- &amp;ClientToken=550e8400-e29b-41d4-a716-446655440000 &amp;AUTHPARAMS
-- &lt;CopyImageResponse xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;60bc441d-fa2c-494d-b155-5d6a3EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-4d3c2b1a&lt;/imageId&gt; &lt;/CopyImageResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CopyImage'

copyImage :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'ciSourceRegion'
    -> Text -- ^ 'ciSourceImageId'
    -> State CopyImage a
    -> m CopyImageResponse
copyImage p1 p2 s =
    send $ (mkCopyImage p1 p2) &~ s

copyImageCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'ciSourceRegion'
    -> Text -- ^ 'ciSourceImageId'
    -> State CopyImage a
    -> m (Either ServiceErr CopyImageResponse)
copyImageCatch p1 p2 s =
    sendCatch $ (mkCopyImage p1 p2) &~ s

-- $CopySnapshot
-- Copies a point-in-time snapshot of an Amazon EBS volume and stores it in
-- Amazon S3. You can copy the snapshot within the same region or from one
-- region to another. You can use the snapshot to create Amazon EBS volumes or
-- Amazon Machine Images (AMIs). The snapshot is copied to the regional
-- endpoint that you send the HTTP request to. Copies of encrypted Amazon EBS
-- snapshots remain encrypted. Copies of unencrypted snapshots remain
-- unencrypted. For more information, see Copying an Amazon EBS Snapshot in
-- the Amazon Elastic Compute Cloud User Guide. Example This example request
-- copies the snapshot in the us-west-1 region with the ID snap-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=CopySnapshot &amp;SourceRegion=us-west-1
-- &amp;SourceSnapshotId=snap-1a2b3c4d &amp;Description=My_snapshot
-- &amp;AUTHPARAMS &lt;CopySnapshotResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;60bc441d-fa2c-494d-b155-5d6a3EXAMPLE&lt;/requestId&gt;
-- &lt;snapshotId&gt;snap-2a2b3c4d&lt;/snapshotId&gt;
-- &lt;/CopySnapshotResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CopySnapshot'

copySnapshot :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'csSourceRegion'
    -> Text -- ^ 'csSourceSnapshotId'
    -> State CopySnapshot a
    -> m CopySnapshotResponse
copySnapshot p1 p2 s =
    send $ (mkCopySnapshot p1 p2) &~ s

copySnapshotCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'csSourceRegion'
    -> Text -- ^ 'csSourceSnapshotId'
    -> State CopySnapshot a
    -> m (Either ServiceErr CopySnapshotResponse)
copySnapshotCatch p1 p2 s =
    sendCatch $ (mkCopySnapshot p1 p2) &~ s

-- $CreateCustomerGateway
-- Provides information to AWS about your VPN customer gateway device. The
-- customer gateway is the appliance at your end of the VPN connection. (The
-- device on the AWS side of the VPN connection is the virtual private
-- gateway.) You must provide the Internet-routable IP address of the customer
-- gateway's external interface. The IP address must be static and can't be
-- behind a device performing network address translation (NAT). For devices
-- that use Border Gateway Protocol (BGP), you can also provide the device's
-- BGP Autonomous System Number (ASN). You can use an existing ASN assigned to
-- your network. If you don't have an ASN already, you can use a private ASN
-- (in the 64512 - 65534 range). Amazon EC2 supports all 2-byte ASN numbers in
-- the range of 1 - 65534, with the exception of 7224, which is reserved in
-- the us-east-1 region, and 9059, which is reserved in the eu-west-1 region.
-- For more information about VPN customer gateways, see Adding a Hardware
-- Virtual Private Gateway to Your VPC in the Amazon Virtual Private Cloud
-- User Guide. Example This example passes information to AWS about the
-- customer gateway with the IP address 12.1.2.3 and BGP ASN 65534.
-- https://ec2.amazonaws.com/?Action=CreateCustomerGateway &amp;Type=ipsec.1
-- &amp;IpAddress=12.1.2.3 &amp;BgpAsn=65534 &amp;AUTHPARAMS
-- &lt;CreateCustomerGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;customerGateway&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;ipAddress&gt;12.1.2.3&lt;/ipAddress&gt;
-- &lt;bgpAsn&gt;65534&lt;/bgpAsn&gt; &lt;tagSet/&gt; &lt;/customerGateway&gt;
-- &lt;/CreateCustomerGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateCustomerGateway'

createCustomerGateway :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => GatewayType -- ^ 'ccgType'
    -> Text -- ^ 'ccgPublicIp'
    -> Integer -- ^ 'ccgBgpAsn'
    -> State CreateCustomerGateway a
    -> m CreateCustomerGatewayResponse
createCustomerGateway p1 p2 p3 s =
    send $ (mkCreateCustomerGateway p1 p2 p3) &~ s

createCustomerGatewayCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => GatewayType -- ^ 'ccgType'
    -> Text -- ^ 'ccgPublicIp'
    -> Integer -- ^ 'ccgBgpAsn'
    -> State CreateCustomerGateway a
    -> m (Either ServiceErr CreateCustomerGatewayResponse)
createCustomerGatewayCatch p1 p2 p3 s =
    sendCatch $ (mkCreateCustomerGateway p1 p2 p3) &~ s

-- $CreateDhcpOptions
-- Creates a set of DHCP options for your VPC. After creating the set, you
-- must associate it with the VPC, causing all existing and new instances that
-- you launch in the VPC to use this set of DHCP options. The following are
-- the individual DHCP options you can specify. For more information about the
-- options, see RFC 2132. domain-name-servers - The IP addresses of up to four
-- domain name servers, or AmazonProvidedDNS. The default DHCP option set
-- specifies AmazonProvidedDNS. If specifying more than one domain name
-- server, specify the IP addresses in a single parameter, separated by
-- commas. domain-name - If you're using AmazonProvidedDNS in us-east-1,
-- specify ec2.internal. If you're using AmazonProvidedDNS in another region,
-- specify region.compute.internal (for example,
-- ap-northeast-1.compute.internal). Otherwise, specify a domain name (for
-- example, MyCompany.com). If specifying more than one domain name, separate
-- them with spaces. ntp-servers - The IP addresses of up to four Network Time
-- Protocol (NTP) servers. netbios-name-servers - The IP addresses of up to
-- four NetBIOS name servers. netbios-node-type - The NetBIOS node type (1, 2,
-- 4, or 8). We recommend that you specify 2 (broadcast and multicast are not
-- currently supported). For more information about these node types, see RFC
-- 2132. For more information about DHCP options, see DHCP Options Sets in the
-- Amazon Virtual Private Cloud User Guide. Example This example creates a set
-- of DHCP options with a domain name example.com and two DNS servers
-- (10.2.5.1 and 10.2.5.2). The DNS servers' IP addresses are specified in a
-- single parameter, separated by commas, to preserve the order in which they
-- are specified. https://ec2.amazonaws.com/?Action=CreateDhcpOptions
-- &amp;DhcpConfiguration.1.Key=domain-name
-- &amp;DhcpConfiguration.1.Value.1=example.com
-- &amp;DhcpConfiguration.2.Key=domain-name-servers
-- &amp;DhcpConfiguration.2.Value.1=10.2.5.1,10.2.5.2 &amp;AUTHPARAMS
-- &lt;CreateDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;dhcpOptions&gt;
-- &lt;dhcpOptionsId&gt;dopt-7a8b9c2d&lt;/dhcpOptionsId&gt;
-- &lt;dhcpConfigurationSet&gt; &lt;item&gt;
-- &lt;key&gt;domain-name&lt;/key&gt; &lt;valueSet&gt; &lt;item&gt;
-- &lt;value&gt;example.com&lt;/value&gt; &lt;/item&gt; &lt;/valueSet&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;key&gt;domain-name-servers&lt;/key&gt;
-- &lt;valueSet&gt; &lt;item&gt; &lt;value&gt;10.2.5.1&lt;/value&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;value&gt;10.2.5.2&lt;/value&gt;
-- &lt;/item&gt; &lt;/valueSet&gt; &lt;/item&gt; &lt;/dhcpConfigurationSet&gt;
-- &lt;tagSet/&gt; &lt;/dhcpOptions&gt; &lt;/CreateDhcpOptionsResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateDhcpOptions'

createDhcpOptions :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => [DhcpConfiguration] -- ^ 'cdoDhcpConfigurations'
    -> State CreateDhcpOptions a
    -> m CreateDhcpOptionsResponse
createDhcpOptions p1 s =
    send $ (mkCreateDhcpOptions p1) &~ s

createDhcpOptionsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => [DhcpConfiguration] -- ^ 'cdoDhcpConfigurations'
    -> State CreateDhcpOptions a
    -> m (Either ServiceErr CreateDhcpOptionsResponse)
createDhcpOptionsCatch p1 s =
    sendCatch $ (mkCreateDhcpOptions p1) &~ s

-- $CreateImage
-- Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that is
-- either running or stopped. If you customized your instance with instance
-- store volumes or EBS volumes in addition to the root device volume, the new
-- AMI contains block device mapping information for those volumes. When you
-- launch an instance from this new AMI, the instance automatically launches
-- with those additional volumes. For more information, see Creating Amazon
-- EBS-Backed Linux AMIs in the Amazon Elastic Compute Cloud User Guide.
-- Example This example request creates an AMI from the specified instance.
-- https://ec2.amazonaws.com/?Action=CreateImage
-- &amp;Description=Standard+Web+Server+v1.0 &amp;InstanceId=i-10a64379
-- &amp;Name=standard-web-server-v1.0 &amp;AUTHPARAMS &lt;CreateImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-4fa54026&lt;/imageId&gt; &lt;/CreateImageResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateImage'

createImage :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'ci1InstanceId'
    -> Text -- ^ 'ci1Name'
    -> State CreateImage a
    -> m CreateImageResponse
createImage p1 p2 s =
    send $ (mkCreateImage p1 p2) &~ s

createImageCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'ci1InstanceId'
    -> Text -- ^ 'ci1Name'
    -> State CreateImage a
    -> m (Either ServiceErr CreateImageResponse)
createImageCatch p1 p2 s =
    sendCatch $ (mkCreateImage p1 p2) &~ s

-- $CreateInstanceExportTask
-- Exports a running or stopped instance to an Amazon S3 bucket. For
-- information about the supported operating systems, image formats, and known
-- limitations for the types of instances you can export, see Exporting EC2
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example request creates an Export VM task that makes a Windows instance
-- available as an OVA.
-- https://ec2.amazonaws.com/?Action=CreateInstanceExportTask
-- &amp;Description=Example%20for%20docs &amp;InstanceId=i-12345678
-- &amp;TargetEnvironment=VMWare &amp;ExportToS3.DiskImageFormat=VMDK
-- &amp;ExportToS3.ContainerFormat=OVA
-- &amp;ExportToS3.S3bucket=my-bucket-for-exported-vm
-- &amp;ExportToS3.S3prefix=my-exports/ &amp;AUTHPARAMS
-- &lt;CreateInstanceExportTaskResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;exportTask&gt;
-- &lt;exportTaskId&gt;export-i-1234wxyz&lt;/exportTaskId&gt;
-- &lt;description&gt;Example for docs&lt;/description&gt;
-- &lt;state&gt;active&lt;/state&gt;
-- &lt;statusMessage&gt;Running&lt;/statusMessage&gt; &lt;instanceExport&gt;
-- &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;targetEnvironment&gt;VMWare&lt;/targetEnvironment&gt;
-- &lt;/instanceExport&gt; &lt;exportToS3&gt;
-- &lt;diskImageFormat&gt;VMDK&lt;/diskImageFormat&gt;
-- &lt;containerFormat&gt;OVA&lt;/containerFormat&gt;
-- &lt;s3Bucket&gt;my-bucket-for-exported-vm&lt;/s3Bucket&gt;
-- &lt;s3Key&gt;my-exports/ export-i-1234wxyz .ova&lt;/s3Key&gt;
-- &lt;/exportToS3&gt; &lt;/exportTask&gt;
-- &lt;/CreateInstanceExportTaskResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateInstanceExportTask'

createInstanceExportTask :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'cietInstanceId'
    -> State CreateInstanceExportTask a
    -> m CreateInstanceExportTaskResponse
createInstanceExportTask p2 s =
    send $ (mkCreateInstanceExportTask p2) &~ s

createInstanceExportTaskCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'cietInstanceId'
    -> State CreateInstanceExportTask a
    -> m (Either ServiceErr CreateInstanceExportTaskResponse)
createInstanceExportTaskCatch p2 s =
    sendCatch $ (mkCreateInstanceExportTask p2) &~ s

-- $CreateInternetGateway
-- Creates an Internet gateway for use with a VPC. After creating the Internet
-- gateway, you attach it to a VPC using AttachInternetGateway. For more
-- information about your VPC and Internet gateway, see the Amazon Virtual
-- Private Cloud User Guide. Example This example creates an Internet gateway.
-- https://ec2.amazonaws.com/?Action=CreateInternetGateway &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE igw-eaad4883.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateInternetGateway'

createInternetGateway :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => State CreateInternetGateway a
    -> m CreateInternetGatewayResponse
createInternetGateway s =
    send (mkCreateInternetGateway &~ s)

createInternetGatewayCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => State CreateInternetGateway a
    -> m (Either ServiceErr CreateInternetGatewayResponse)
createInternetGatewayCatch s =
    sendCatch (mkCreateInternetGateway &~ s)

-- $CreateKeyPair
-- Creates a 2048-bit RSA key pair with the specified name. Amazon EC2 stores
-- the public key and displays the private key for you to save to a file. The
-- private key is returned as an unencrypted PEM encoded PKCS#8 private key.
-- If a key with the specified name already exists, Amazon EC2 returns an
-- error. You can have up to five thousand key pairs per region. For more
-- information about key pairs, see Key Pairs in the Amazon Elastic Compute
-- Cloud User Guide. Example This example request creates a key pair named
-- my-key-pair. https://ec2.amazonaws.com/?Action=CreateKeyPair
-- &amp;KeyName=my-key-pair &amp;AUTHPARAMS &lt;CreateKeyPairResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt; my-key-pair
-- 1f:51:ae:28:bf:89:e9:d8:1f:25:5d:37:2d:7d:b8:ca:9f:f5:f1:6f ---- BEGIN RSA
-- PRIVATE KEY ----
-- MIICiTCCAfICCQD6m7oRw0uXOjANBgkqhkiG9w0BAQUFADCBiDELMAkGA1UEBhMC
-- VVMxCzAJBgNVBAgTAldBMRAwDgYDVQQHEwdTZWF0dGxlMQ8wDQYDVQQKEwZBbWF6
-- b24xFDASBgNVBAsTC0lBTSBDb25zb2xlMRIwEAYDVQQDEwlUZXN0Q2lsYWMxHzAd
-- BgkqhkiG9w0BCQEWEG5vb25lQGFtYXpvbi5jb20wHhcNMTEwNDI1MjA0NTIxWhcN
-- MTIwNDI0MjA0NTIxWjCBiDELMAkGA1UEBhMCVVMxCzAJBgNVBAgTAldBMRAwDgYD
-- VQQHEwdTZWF0dGxlMQ8wDQYDVQQKEwZBbWF6b24xFDASBgNVBAsTC0lBTSBDb25z
-- b2xlMRIwEAYDVQQDEwlUZXN0Q2lsYWMxHzAdBgkqhkiG9w0BCQEWEG5vb25lQGFt
-- YXpvbi5jb20wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAMaK0dn+a4GmWIWJ
-- 21uUSfwfEvySWtC2XADZ4nB+BLYgVIk60CpiwsZ3G93vUEIO3IyNoH/f0wYK8m9T
-- rDHudUZg3qX4waLG5M43q7Wgc/MbQITxOUSQv7c7ugFFDzQGBzZswY6786m86gpE
-- Ibb3OhjZnzcvQAaRHhdlQWIMm2nrAgMBAAEwDQYJKoZIhvcNAQEFBQADgYEAtCu4
-- nUhVVxYUntneD9+h8Mg9q6q+auNKyExzyLwaxlAoo7TJHidbtS4J5iNmZgXL0Fkb
-- FFBjvSfpJIlJ00zbhNYS5f6GuoEDmFJl0ZxBHjJnyp378OD8uTs7fLvjx79LjSTb
-- NYiytVbZPQUQ5Yaxu2jXnimvw3rrszlaEXAMPLE -----END RSA PRIVATE KEY-----
-- Saving the File Create a file named my-key-pair.pem and paste the entire
-- key from the response into this file. Keep this file in a safe place; it is
-- required to decrypt login information when you connect to an instance that
-- you launched using this key pair. If you're using an SSH client on a Linux
-- computer to connect to your instance, use the following command to set the
-- permissions of your private key file so that only you can read it. chmod
-- 400 my-key-pair.pem.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateKeyPair'

createKeyPair :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'ckpKeyName'
    -> State CreateKeyPair a
    -> m CreateKeyPairResponse
createKeyPair p1 s =
    send $ (mkCreateKeyPair p1) &~ s

createKeyPairCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ckpKeyName'
    -> State CreateKeyPair a
    -> m (Either ServiceErr CreateKeyPairResponse)
createKeyPairCatch p1 s =
    sendCatch $ (mkCreateKeyPair p1) &~ s

-- $CreateNetworkAcl
-- Creates a network ACL in a VPC. Network ACLs provide an optional layer of
-- security (in addition to security groups) for the instances in your VPC.
-- For more information about network ACLs, see Network ACLs in the Amazon
-- Virtual Private Cloud User Guide. Example This example creates a network
-- ACL in the specified VPC. The response includes a default entry for egress,
-- and another for ingress, each with a very high rule number. These are the
-- last entries we process to decide whether traffic is allowed in or out of
-- an associated subnet. If the traffic doesn't match any rules with a lower
-- rule number, then these default entries ultimately deny the traffic.
-- https://ec2.amazonaws.com/?Action=CreateNetworkAcl &amp;VpcId=vpc-11ad4878
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE acl-5fb85d36
-- vpc-11ad4878 false 32767 all deny true 0.0.0.0/0 32767 all deny false
-- 0.0.0.0/0.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateNetworkAcl'

createNetworkAcl :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'cnaVpcId'
    -> State CreateNetworkAcl a
    -> m CreateNetworkAclResponse
createNetworkAcl p1 s =
    send $ (mkCreateNetworkAcl p1) &~ s

createNetworkAclCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'cnaVpcId'
    -> State CreateNetworkAcl a
    -> m (Either ServiceErr CreateNetworkAclResponse)
createNetworkAclCatch p1 s =
    sendCatch $ (mkCreateNetworkAcl p1) &~ s

-- $CreateNetworkAclEntry
-- Creates an entry (a rule) in a network ACL with the specified rule number.
-- Each network ACL has a set of numbered ingress rules and a separate set of
-- numbered egress rules. When determining whether a packet should be allowed
-- in or out of a subnet associated with the ACL, we process the entries in
-- the ACL according to the rule numbers, in ascending order. Each network ACL
-- has a set of ingress rules and a separate set of egress rules. We recommend
-- that you leave room between the rule numbers (for example, 100, 110, 120,
-- ...), and not number them one right after the other (for example, 101, 102,
-- 103, ...). This makes it easier to add a rule between existing ones without
-- having to renumber the rules. After you add an entry, you can't modify it;
-- you must either replace it, or create an entry and delete the old one. For
-- more information about network ACLs, see Network ACLs in the Amazon Virtual
-- Private Cloud User Guide. Example This example creates an entry with rule
-- number 110 in the network ACL with the ID acl-2cb85d45. The rule allows
-- ingress traffic from anywhere (0.0.0.0/0) on UDP port 53 into any
-- associated subnet. https://ec2.amazonaws.com/?Action=CreateNetworkAclEntry
-- &amp;NetworkAclId=acl-2cb85d45 &amp;RuleNumber=110 &amp;Protocol=udp
-- &amp;RuleAction=allow &amp;Egress=false &amp;CidrBlock=0.0.0.0/0
-- &amp;PortRange.From=53 &amp;PortRange.To=53 &amp;AUTHPARAMS
-- &lt;CreateNetworkAclEntryResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/CreateNetworkAclEntryResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateNetworkAclEntry'

createNetworkAclEntry :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'cnaeNetworkAclId'
    -> Integer -- ^ 'cnaeRuleNumber'
    -> Text -- ^ 'cnaeProtocol'
    -> RuleAction -- ^ 'cnaeRuleAction'
    -> Bool -- ^ 'cnaeEgress'
    -> Text -- ^ 'cnaeCidrBlock'
    -> State CreateNetworkAclEntry a
    -> m CreateNetworkAclEntryResponse
createNetworkAclEntry p1 p2 p3 p4 p5 p6 s =
    send $ (mkCreateNetworkAclEntry p1 p2 p3 p4 p5 p6) &~ s

createNetworkAclEntryCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'cnaeNetworkAclId'
    -> Integer -- ^ 'cnaeRuleNumber'
    -> Text -- ^ 'cnaeProtocol'
    -> RuleAction -- ^ 'cnaeRuleAction'
    -> Bool -- ^ 'cnaeEgress'
    -> Text -- ^ 'cnaeCidrBlock'
    -> State CreateNetworkAclEntry a
    -> m (Either ServiceErr CreateNetworkAclEntryResponse)
createNetworkAclEntryCatch p1 p2 p3 p4 p5 p6 s =
    sendCatch $ (mkCreateNetworkAclEntry p1 p2 p3 p4 p5 p6) &~ s

-- $CreateNetworkInterface
-- Creates a network interface in the specified subnet. For more information
-- about network interfaces, see Elastic Network Interfaces in the Amazon
-- Elastic Compute Cloud User Guide. Example 1 This example creates a network
-- interface in the specified subnet with a primary IP address that is
-- automatically selected by Amazon EC2.
-- https://ec2.amazonaws.com/?Action=CreateNetworkInterface
-- &amp;SubnetId=subnet-b2a249da &amp;AUTHPARAMS
-- &lt;CreateNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;8dbe591e-5a22-48cb-b948-dd0aadd55adf&lt;/requestId&gt;
-- &lt;networkInterface&gt;
-- &lt;networkInterfaceId&gt;eni-cfca76a6&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-b2a249da&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-c31dafaa&lt;/vpcId&gt;
-- &lt;availabilityZone&gt;ap-southeast-1b&lt;/availabilityZone&gt;
-- &lt;description/&gt; &lt;ownerId&gt;251839141158&lt;/ownerId&gt;
-- &lt;requesterManaged&gt;false&lt;/requesterManaged&gt;
-- &lt;status&gt;available&lt;/status&gt;
-- &lt;macAddress&gt;02:74:b0:72:79:61&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.2.157&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;groupName&gt;default&lt;/groupName&gt; &lt;/item&gt; &lt;/groupSet&gt;
-- &lt;tagSet/&gt; &lt;privateIpAddressesSet&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.157&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;/item&gt;
-- &lt;/privateIpAddressesSet&gt; &lt;/networkInterface&gt;
-- &lt;/CreateNetworkInterfaceResponse&gt; Example 2 This example creates a
-- network interface in the specified subnet with a primary IP address of
-- 10.0.2.140 and four secondary private IP addresses that are automatically
-- selected by Amazon EC2.
-- https://ec2.amazonaws.com/?Action=CreateNetworkInterface
-- &amp;PrivateIpAddresses.0.Primary=true
-- &amp;PrivateIpAddresses.0.PrivateIpAddress=10.0.2.140
-- &amp;SecondaryPrivateIpAddressCount=4 &amp;SubnetId=subnet-a61dafcf
-- &amp;AUTHPARAMS &lt;CreateNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;bd78c839-0895-4fac-a17f-98b559b6b630&lt;/requestId&gt;
-- &lt;networkInterface&gt;
-- &lt;networkInterfaceId&gt;eni-1bcb7772&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-a61dafcf&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-c31dafaa&lt;/vpcId&gt;
-- &lt;availabilityZone&gt;ap-southeast-1b&lt;/availabilityZone&gt;
-- &lt;description/&gt; &lt;ownerId&gt;251839141158&lt;/ownerId&gt;
-- &lt;requesterManaged&gt;false&lt;/requesterManaged&gt;
-- &lt;status&gt;pending&lt;/status&gt;
-- &lt;macAddress&gt;02:74:b0:70:7f:1a&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.2.140&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;groupName&gt;default&lt;/groupName&gt; &lt;/item&gt; &lt;/groupSet&gt;
-- &lt;tagSet/&gt; &lt;privateIpAddressesSet&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.140&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.172&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.169&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.170&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.171&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt;
-- &lt;/privateIpAddressesSet&gt; &lt;/networkInterface&gt;
-- &lt;/CreateNetworkInterfaceResponse&gt; Example 3 This example creates a
-- network interface with a primary private IP address of 10.0.2.130 and two
-- secondary IP addresses of 10.0.2.132 and 10.0.2.133.
-- https://ec2.amazonaws.com/?Action=CreateNetworkInterface
-- &amp;PrivateIpAddresses.0.Primary=true
-- &amp;PrivateIpAddresses.0.PrivateIpAddress=10.0.2.130
-- &amp;PrivateIpAddresses.1.Primary=false
-- &amp;PrivateIpAddresses.1.PrivateIpAddress=10.0.2.132
-- &amp;PrivateIpAddresses.2.Primary=false
-- &amp;PrivateIpAddresses.2.PrivateIpAddress=10.0.2.133
-- &amp;SubnetId=subnet-a61dafcf &amp;AUTHPARAMS
-- &lt;CreateNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;a9565f4c-f928-4113-859b-905886d11658&lt;/requestId&gt;
-- &lt;networkInterface&gt;
-- &lt;networkInterfaceId&gt;eni-41c47828&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-a61dafcf&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-c31dafaa&lt;/vpcId&gt;
-- &lt;availabilityZone&gt;ap-southeast-1b&lt;/availabilityZone&gt;
-- &lt;description/&gt; &lt;ownerId&gt;251839141158&lt;/ownerId&gt;
-- &lt;requesterManaged&gt;false&lt;/requesterManaged&gt;
-- &lt;status&gt;pending&lt;/status&gt;
-- &lt;macAddress&gt;02:74:b0:78:bf:ab&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.2.130&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-188d9f74&lt;/groupId&gt;
-- &lt;groupName&gt;default&lt;/groupName&gt; &lt;/item&gt; &lt;/groupSet&gt;
-- &lt;tagSet/&gt; &lt;privateIpAddressesSet&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.130&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.133&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.132&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt;
-- &lt;/privateIpAddressesSet&gt; &lt;/networkInterface&gt;
-- &lt;/CreateNetworkInterfaceResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateNetworkInterface'

createNetworkInterface :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'cniSubnetId'
    -> State CreateNetworkInterface a
    -> m CreateNetworkInterfaceResponse
createNetworkInterface p1 s =
    send $ (mkCreateNetworkInterface p1) &~ s

createNetworkInterfaceCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'cniSubnetId'
    -> State CreateNetworkInterface a
    -> m (Either ServiceErr CreateNetworkInterfaceResponse)
createNetworkInterfaceCatch p1 s =
    sendCatch $ (mkCreateNetworkInterface p1) &~ s

-- $CreatePlacementGroup
-- Creates a placement group that you launch cluster instances into. You must
-- give the group a name that's unique within the scope of your account. For
-- more information about placement groups and cluster instances, see Cluster
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example creates a placement group named XYZ-cluster.
-- https://ec2.amazonaws.com/?Action=CreatePlacementGroup
-- &amp;GroupName=XYZ-cluster &amp;Strategy=cluster &amp;AUTHPARAMS
-- &lt;CreatePlacementGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/CreatePlacementGroupResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreatePlacementGroup'

createPlacementGroup :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'cpgGroupName'
    -> PlacementStrategy -- ^ 'cpgStrategy'
    -> State CreatePlacementGroup a
    -> m CreatePlacementGroupResponse
createPlacementGroup p1 p2 s =
    send $ (mkCreatePlacementGroup p1 p2) &~ s

createPlacementGroupCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'cpgGroupName'
    -> PlacementStrategy -- ^ 'cpgStrategy'
    -> State CreatePlacementGroup a
    -> m (Either ServiceErr CreatePlacementGroupResponse)
createPlacementGroupCatch p1 p2 s =
    sendCatch $ (mkCreatePlacementGroup p1 p2) &~ s

-- $CreateReservedInstancesListing
-- Creates a listing for Amazon EC2 Reserved Instances to be sold in the
-- Reserved Instance Marketplace. You can submit one Reserved Instance listing
-- at a time. For more information, see Reserved Instance Marketplace in the
-- Amazon Elastic Compute Cloud User Guide. Example This example creates a
-- Reserved Instance Marketplace listing from the specified Reserved Instance,
-- which has 11 months remaining in its term. In this example, we set the
-- upfront price at $2.50, and the price drops over the course of the 11-month
-- term if the instance is still not sold.
-- https://ec2.amazonaws.com/?Action=CreateReservedInstancesListing
-- &amp;ClientToken=myIdempToken1 &amp;InstanceCount=1
-- &amp;PriceSchedules.0.Price=2.5 &amp;PriceSchedules.0.Term=11
-- &amp;PriceSchedules.1.Price=2.0 &amp;PriceSchedules.1.Term=8
-- &amp;PriceSchedules.2.Price=1.5 &amp;PriceSchedules.2.Term=5
-- &amp;PriceSchedules.3.Price=0.7 &amp;PriceSchedules.3.Term=3
-- &amp;PriceSchedules.4.Price=0.1 &amp;PriceSchedules.4.Term=1
-- &amp;ReservedInstancesId=e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- &amp;AUTHPARAMS a42481af-335a-4e9e-b291-bd18dexample
-- 5ec28771-05ff-4b9b-aa31-9e57dEXAMPLE e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- 2012-07-17T17:11:09.449Z 2012-07-17T17:11:09.468Z active ACTIVE Available 1
-- Sold 0 Cancelled 0 Pending 0 11 2.5 USD true 10 2.5 USD false 9 2.5 USD
-- false 8 2.0 USD false 7 2.0 USD false 6 2.0 USD false 5 1.5 USD false 4 1.5
-- USD false 3 0.7 USD false 2 0.7 USD false 1 0.1 USD false myIdempToken1.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateReservedInstancesListing'

createReservedInstancesListing :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'cril1ReservedInstancesId'
    -> Integer -- ^ 'cril1InstanceCount'
    -> [PriceScheduleSpecification] -- ^ 'cril1PriceSchedules'
    -> Text -- ^ 'cril1ClientToken'
    -> State CreateReservedInstancesListing a
    -> m CreateReservedInstancesListingResponse
createReservedInstancesListing p1 p2 p3 p4 s =
    send $ (mkCreateReservedInstancesListing p1 p2 p3 p4) &~ s

createReservedInstancesListingCatch :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadReader Env m
                                       )
    => Text -- ^ 'cril1ReservedInstancesId'
    -> Integer -- ^ 'cril1InstanceCount'
    -> [PriceScheduleSpecification] -- ^ 'cril1PriceSchedules'
    -> Text -- ^ 'cril1ClientToken'
    -> State CreateReservedInstancesListing a
    -> m (Either ServiceErr CreateReservedInstancesListingResponse)
createReservedInstancesListingCatch p1 p2 p3 p4 s =
    sendCatch $ (mkCreateReservedInstancesListing p1 p2 p3 p4) &~ s

-- $CreateRoute
-- Creates a route in a route table within a VPC. You must specify one of the
-- following targets: Internet gateway, NAT instance, VPC peering connection,
-- or network interface. When determining how to route traffic, we use the
-- route with the most specific match. For example, let's say the traffic is
-- destined for 192.0.2.3, and the route table includes the following two
-- routes: 192.0.2.0/24 (goes to some target A) 192.0.2.0/28 (goes to some
-- target B) Both routes apply to the traffic destined for 192.0.2.3. However,
-- the second route in the list covers a smaller number of IP addresses and is
-- therefore more specific, so we use that route to determine where to target
-- the traffic. For more information about route tables, see Route Tables in
-- the Amazon Virtual Private Cloud User Guide. Example 1 This example creates
-- a route in the route table with the ID rtb-e4ad488d. The route matches all
-- traffic (0.0.0.0/0) and routes it to the Internet gateway with the ID
-- igw-eaad4883. https://ec2.amazonaws.com/?Action=CreateRoute
-- &amp;RouteTableId=rtb-e4ad488d &amp;DestinationCidrBlock=0.0.0.0/0
-- &amp;GatewayId=igw-eaad4883 &amp;AUTHPARAMS Example 2 This example creates
-- a route in the route table with the ID rtb-g8ff4ea2. The route sends all
-- traffic (0.0.0.0/0) to the NAT instance with the ID i-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=CreateRoute
-- &amp;RouteTableId=rtb-g8ff4ea2 &amp;DestinationCidrBlock=0.0.0.0/0
-- &amp;InstanceId=i-1a2b3c4d &amp;AUTHPARAMS Example 3 This example command
-- creates a route in route table rtb-g8ff4ea2. The route matches traffic for
-- the CIDR block 10.0.0.0/16 and routes it to VPC peering connection,
-- pcx-111aaa22. This route enables traffic to be directed to the other peered
-- VPC in the VPC peering connection.
-- https://ec2.amazonaws.com/?Action=CreateRoute
-- &amp;RouteTableId=rtb-g8ff4ea2 &amp;DestinationCidrBlock=10.0.0.0/16
-- &amp;vpcPeeringConnectionId=pcx-111aaa22 &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateRoute'

createRoute :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'crRouteTableId'
    -> Text -- ^ 'crDestinationCidrBlock'
    -> State CreateRoute a
    -> m CreateRouteResponse
createRoute p1 p2 s =
    send $ (mkCreateRoute p1 p2) &~ s

createRouteCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'crRouteTableId'
    -> Text -- ^ 'crDestinationCidrBlock'
    -> State CreateRoute a
    -> m (Either ServiceErr CreateRouteResponse)
createRouteCatch p1 p2 s =
    sendCatch $ (mkCreateRoute p1 p2) &~ s

-- $CreateRouteTable
-- Creates a route table for the specified VPC. After you create a route
-- table, you can add routes and associate the table with a subnet. For more
-- information about route tables, see Route Tables in the Amazon Virtual
-- Private Cloud User Guide. Example This example creates a route table for
-- the VPC with the ID vpc-11ad4878. By default, every route table includes a
-- local route that enables traffic to flow within the VPC. The following
-- response shows that route.
-- https://ec2.amazonaws.com/?Action=CreateRouteTable &amp;VpcId=vpc-11ad4878
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE rtb-f9ad4890
-- vpc-11ad4878 10.0.0.0/22 local active.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateRouteTable'

createRouteTable :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'crtVpcId'
    -> State CreateRouteTable a
    -> m CreateRouteTableResponse
createRouteTable p1 s =
    send $ (mkCreateRouteTable p1) &~ s

createRouteTableCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'crtVpcId'
    -> State CreateRouteTable a
    -> m (Either ServiceErr CreateRouteTableResponse)
createRouteTableCatch p1 s =
    sendCatch $ (mkCreateRouteTable p1) &~ s

-- $CreateSecurityGroup
-- Creates a security group. A security group is for use with instances either
-- in the EC2-Classic platform or in a specific VPC. For more information, see
-- Amazon EC2 Security Groups in the Amazon Elastic Compute Cloud User Guide
-- and Security Groups for Your VPC in the Amazon Virtual Private Cloud User
-- Guide. EC2-Classic: You can have up to 500 security groups. EC2-VPC: You
-- can create up to 100 security groups per VPC. When you create a security
-- group, you specify a friendly name of your choice. You can have a security
-- group for use in EC2-Classic with the same name as a security group for use
-- in a VPC. However, you can't have two security groups for use in
-- EC2-Classic with the same name or two security groups for use in a VPC with
-- the same name. You have a default security group for use in EC2-Classic and
-- a default security group for use in your VPC. If you don't specify a
-- security group when you launch an instance, the instance is launched into
-- the appropriate default security group. A default security group includes a
-- default rule that grants instances unrestricted network access to each
-- other. You can add or remove rules from your security groups using
-- AuthorizeSecurityGroupIngress, AuthorizeSecurityGroupEgress,
-- RevokeSecurityGroupIngress, and RevokeSecurityGroupEgress. Example for
-- EC2-Classic This example creates a security group named websrv for
-- EC2-Classic. https://ec2.amazonaws.com/?Action=CreateSecurityGroup
-- &amp;GroupName=websrv &amp;GroupDescription=Web Servers &amp;AUTHPARAMS
-- &lt;CreateSecurityGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;/CreateSecurityGroupResponse&gt; Example for EC2-VPC This example
-- creates a security group named WebServerSG for the specified VPC.
-- https://ec2.amazonaws.com/?Action=CreateSecurityGroup
-- &amp;GroupName=WebServerSG &amp;GroupDescription=Web Servers
-- &amp;VpcId=vpc-3325caf2 &amp;AUTHPARAMS &lt;CreateSecurityGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;groupId&gt;sg-0a42d66a&lt;/groupId&gt;
-- &lt;/CreateSecurityGroupResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateSecurityGroup'

createSecurityGroup :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'csgGroupName'
    -> Text -- ^ 'csgDescription'
    -> State CreateSecurityGroup a
    -> m CreateSecurityGroupResponse
createSecurityGroup p1 p2 s =
    send $ (mkCreateSecurityGroup p1 p2) &~ s

createSecurityGroupCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'csgGroupName'
    -> Text -- ^ 'csgDescription'
    -> State CreateSecurityGroup a
    -> m (Either ServiceErr CreateSecurityGroupResponse)
createSecurityGroupCatch p1 p2 s =
    sendCatch $ (mkCreateSecurityGroup p1 p2) &~ s

-- $CreateSnapshot
-- Creates a snapshot of an Amazon EBS volume and stores it in Amazon S3. You
-- can use snapshots for backups, to make copies of Amazon EBS volumes, and to
-- save data before shutting down an instance. When a snapshot is created, any
-- AWS Marketplace product codes that are associated with the source volume
-- are propagated to the snapshot. You can take a snapshot of an attached
-- volume that is in use. However, snapshots only capture data that has been
-- written to your Amazon EBS volume at the time the snapshot command is
-- issued; this may exclude any data that has been cached by any applications
-- or the operating system. If you can pause any file systems on the volume
-- long enough to take a snapshot, your snapshot should be complete. However,
-- if you cannot pause all file writes to the volume, you should unmount the
-- volume from within the instance, issue the snapshot command, and then
-- remount the volume to ensure a consistent and complete snapshot. You may
-- remount and use your volume while the snapshot status is pending. To create
-- a snapshot for Amazon EBS volumes that serve as root devices, you should
-- stop the instance before taking the snapshot. Snapshots that are taken from
-- encrypted volumes are automatically encrypted. Volumes that are created
-- from encrypted snapshots are also automatically encrypted. Your encrypted
-- volumes and any associated snapshots always remain protected. For more
-- information, see Amazon Elastic Block Store and Amazon EBS Encryption in
-- the Amazon Elastic Compute Cloud User Guide. Example This example creates a
-- snapshot of the volume with the ID vol-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=CreateSnapshot &amp;VolumeId=vol-1a2b3c4d
-- &amp;Description=Daily+Backup &amp;AUTHPARAMS &lt;CreateSnapshotResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;snapshotId&gt;snap-1a2b3c4d&lt;/snapshotId&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;status&gt;pending&lt;/status&gt;
-- &lt;startTime&gt;YYYY-MM-DDTHH:MM:SS.000Z&lt;/startTime&gt;
-- &lt;progress&gt;60%&lt;/progress&gt;
-- &lt;ownerId&gt;111122223333&lt;/ownerId&gt;
-- &lt;volumeSize&gt;30&lt;/volumeSize&gt; &lt;description&gt;Daily
-- Backup&lt;/description&gt; &lt;/CreateSnapshotResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateSnapshot'

createSnapshot :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'cs1VolumeId'
    -> State CreateSnapshot a
    -> m CreateSnapshotResponse
createSnapshot p1 s =
    send $ (mkCreateSnapshot p1) &~ s

createSnapshotCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'cs1VolumeId'
    -> State CreateSnapshot a
    -> m (Either ServiceErr CreateSnapshotResponse)
createSnapshotCatch p1 s =
    sendCatch $ (mkCreateSnapshot p1) &~ s

-- $CreateSpotDatafeedSubscription
-- Creates a datafeed for Spot Instances, enabling you to view Spot Instance
-- usage logs. You can create one data feed per AWS account. For more
-- information, see Spot Instances in the Amazon Elastic Compute Cloud User
-- Guide. Example This example creates a Spot Instance datafeed for the
-- account. https://ec2.amazonaws.com/?Action=CreateSpotDatafeedSubscription
-- &amp;Bucket=my-s3-bucket &amp;AUTHPARAMS
-- &lt;CreateSpotDatafeedSubscriptionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;spotDatafeedSubscription&gt;
-- &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;bucket&gt;my-s3-bucket&lt;/bucket&gt;
-- &lt;prefix&gt;spotdata_&lt;/prefix&gt; &lt;state&gt;Active&lt;/state&gt;
-- &lt;/spotDatafeedSubscription&gt;
-- &lt;/CreateSpotDatafeedSubscriptionResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateSpotDatafeedSubscription'

createSpotDatafeedSubscription :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'csdsBucket'
    -> State CreateSpotDatafeedSubscription a
    -> m CreateSpotDatafeedSubscriptionResponse
createSpotDatafeedSubscription p1 s =
    send $ (mkCreateSpotDatafeedSubscription p1) &~ s

createSpotDatafeedSubscriptionCatch :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadReader Env m
                                       )
    => Text -- ^ 'csdsBucket'
    -> State CreateSpotDatafeedSubscription a
    -> m (Either ServiceErr CreateSpotDatafeedSubscriptionResponse)
createSpotDatafeedSubscriptionCatch p1 s =
    sendCatch $ (mkCreateSpotDatafeedSubscription p1) &~ s

-- $CreateSubnet
-- Creates a subnet in an existing VPC. When you create each subnet, you
-- provide the VPC ID and the CIDR block you want for the subnet. After you
-- create a subnet, you can't change its CIDR block. The subnet's CIDR block
-- can be the same as the VPC's CIDR block (assuming you want only a single
-- subnet in the VPC), or a subset of the VPC's CIDR block. If you create more
-- than one subnet in a VPC, the subnets' CIDR blocks must not overlap. The
-- smallest subnet (and VPC) you can create uses a /28 netmask (16 IP
-- addresses), and the largest uses a /16 netmask (65,536 IP addresses). AWS
-- reserves both the first four and the last IP address in each subnet's CIDR
-- block. They're not available for use. If you add more than one subnet to a
-- VPC, they're set up in a star topology with a logical router in the middle.
-- For more information about subnets, see Your VPC and Subnets in the Amazon
-- Virtual Private Cloud User Guide. Example This example creates a subnet
-- with CIDR block 10.0.1.0/24 in the VPC with the ID vpc-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=CreateSubnet &amp;VpcId=vpc-1a2b3c4d
-- &amp;CidrBlock=10.0.1.0/24 &amp;AUTHPARAMS &lt;CreateSubnetResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;subnet&gt; &lt;subnetId&gt;subnet-9d4a7b6c&lt;/subnetId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.1.0/24&lt;/cidrBlock&gt;
-- &lt;availableIpAddressCount&gt;251&lt;/availableIpAddressCount&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt; &lt;tagSet/&gt;
-- &lt;/subnet&gt; &lt;/CreateSubnetResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateSubnet'

createSubnet :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'cs2VpcId'
    -> Text -- ^ 'cs2CidrBlock'
    -> State CreateSubnet a
    -> m CreateSubnetResponse
createSubnet p1 p2 s =
    send $ (mkCreateSubnet p1 p2) &~ s

createSubnetCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'cs2VpcId'
    -> Text -- ^ 'cs2CidrBlock'
    -> State CreateSubnet a
    -> m (Either ServiceErr CreateSubnetResponse)
createSubnetCatch p1 p2 s =
    sendCatch $ (mkCreateSubnet p1 p2) &~ s

-- $CreateTags
-- Adds or overwrites one or more tags for the specified EC2 resource or
-- resources. Each resource can have a maximum of 10 tags. Each tag consists
-- of a key and optional value. Tag keys must be unique per resource. For more
-- information about tags, see Tagging Your Resources in the Amazon Elastic
-- Compute Cloud User Guide. Example This example request adds (or overwrites)
-- two tags for an AMI and an instance. One of the tags is just a key
-- (webserver), with no value (we set the value to an empty string). The other
-- tag consists of a key (stack) and value (Production).
-- https://ec2.amazonaws.com/?Action=CreateTags &amp;ResourceId.1=ami-1a2b3c4d
-- &amp;ResourceId.2=i-7f4d3a2b &amp;Tag.1.Key=webserver &amp;Tag.1.Value=
-- &amp;Tag.2.Key=stack &amp;Tag.2.Value=Production &amp;AUTHPARAMS
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/">
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE true.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateTags'

createTags :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => [Text] -- ^ 'ctResources'
    -> [Tag] -- ^ 'ctTags'
    -> State CreateTags a
    -> m CreateTagsResponse
createTags p1 p2 s =
    send $ (mkCreateTags p1 p2) &~ s

createTagsCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => [Text] -- ^ 'ctResources'
    -> [Tag] -- ^ 'ctTags'
    -> State CreateTags a
    -> m (Either ServiceErr CreateTagsResponse)
createTagsCatch p1 p2 s =
    sendCatch $ (mkCreateTags p1 p2) &~ s

-- $CreateVolume
-- Creates an Amazon EBS volume that can be attached to an instance in the
-- same Availability Zone. The volume is created in the specified region. You
-- can create a new empty volume or restore a volume from an Amazon EBS
-- snapshot. Any AWS Marketplace product codes from the snapshot are
-- propagated to the volume. You can create encrypted volumes with the
-- Encrypted parameter. Encrypted volumes may only be attached to instances
-- that support Amazon EBS encryption. Volumes that are created from encrypted
-- snapshots are also automatically encrypted. For more information, see
-- Amazon EBS Encryption in the Amazon Elastic Compute Cloud User Guide. For
-- more information, see Creating or Restoring an Amazon EBS Volume in the
-- Amazon Elastic Compute Cloud User Guide. Example This example request
-- creates an 80 GiB encrypted volume in the Availability Zone us-east-1a.
-- https://ec2.amazonaws.com/?Action=CreateVolume &amp;Size=80
-- &amp;AvailabilityZone=us-east-1a &amp;Encrypted=1 &amp;AUTHPARAMS
-- &lt;CreateVolumeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt; &lt;size&gt;80&lt;/size&gt;
-- &lt;snapshotId/&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;status&gt;creating&lt;/status&gt;
-- &lt;createTime&gt;YYYY-MM-DDTHH:MM:SS.000Z&lt;/createTime&gt;
-- &lt;volumeType&gt;standard&lt;/volumeType&gt;
-- &lt;encrypted&gt;true&lt;/encrypted&gt; &lt;/CreateVolumeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateVolume'

createVolume :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'cvAvailabilityZone'
    -> State CreateVolume a
    -> m CreateVolumeResponse
createVolume p3 s =
    send $ (mkCreateVolume p3) &~ s

createVolumeCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'cvAvailabilityZone'
    -> State CreateVolume a
    -> m (Either ServiceErr CreateVolumeResponse)
createVolumeCatch p3 s =
    sendCatch $ (mkCreateVolume p3) &~ s

-- $CreateVpc
-- Creates a VPC with the specified CIDR block. The smallest VPC you can
-- create uses a /28 netmask (16 IP addresses), and the largest uses a /16
-- netmask (65,536 IP addresses). To help you decide how big to make your VPC,
-- see Your VPC and Subnets in the Amazon Virtual Private Cloud User Guide. By
-- default, each instance you launch in the VPC has the default DHCP options,
-- which includes only a default DNS server that we provide
-- (AmazonProvidedDNS). For more information about DHCP options, see DHCP
-- Options Sets in the Amazon Virtual Private Cloud User Guide. Example 1 This
-- example creates a VPC with the CIDR block 10.0.0.0/16.
-- https://ec2.amazonaws.com/?Action=CreateVpc &amp;CidrBlock=10.0.0.0/16
-- &amp;AUTHPARAMS &lt;CreateVpcResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpc&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;state&gt;pending&lt;/state&gt;
-- &lt;cidrBlock&gt;10.0.0.0/16&lt;/cidrBlock&gt;
-- &lt;dhcpOptionsId&gt;dopt-1a2b3c4d2&lt;/dhcpOptionsId&gt;
-- &lt;instanceTenancy&gt;default&lt;/instanceTenancy&gt; &lt;tagSet/&gt;
-- &lt;/vpc&gt; &lt;/CreateVpcResponse&gt; Example 2 This example creates a
-- VPC with the dedicated tenancy option.
-- https://ec2.amazonaws.com/?Action=CreateVpc &amp;CidrBlock=10.32.0.0/16
-- &amp;InstanceTenancy=dedicated &amp;AUTHPARAMS &lt;CreateVpcResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;a9e49797-a74f-4f68-b302-a134a51fd054&lt;/requestId&gt;
-- &lt;vpc&gt; &lt;vpcId&gt;vpc-11a63c78&lt;/vpcId&gt;
-- &lt;state&gt;pending&lt;/state&gt;
-- &lt;cidrBlock&gt;10.32.0.0/16&lt;/cidrBlock&gt;
-- &lt;dhcpOptionsId&gt;dopt-1a2b3c4d2&lt;/dhcpOptionsId&gt;
-- &lt;instanceTenancy&gt;dedicated&lt;/instanceTenancy&gt; &lt;/vpc&gt;
-- &lt;/CreateVpcResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateVpc'

createVpc :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'cv1CidrBlock'
    -> State CreateVpc a
    -> m CreateVpcResponse
createVpc p1 s =
    send $ (mkCreateVpc p1) &~ s

createVpcCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'cv1CidrBlock'
    -> State CreateVpc a
    -> m (Either ServiceErr CreateVpcResponse)
createVpcCatch p1 s =
    sendCatch $ (mkCreateVpc p1) &~ s

-- $CreateVpcPeeringConnection
-- Requests a VPC peering connection between two VPCs: a requester VPC that
-- you own and a peer VPC with which to create the connection. The peer VPC
-- can belong to another AWS account. The requester VPC and peer VPC cannot
-- have overlapping CIDR blocks. The owner of the peer VPC must accept the
-- peering request to activate the peering connection. The VPC peering
-- connection request expires after 7 days, after which it cannot be accepted
-- or rejected. A CreateVpcPeeringConnection request between VPCs with
-- overlapping CIDR blocks results in the VPC peering connection having a
-- status of failed. Example 1 This example requests a peering connection
-- between your VPC (vpc-1a2b3c4d), and a VPC (vpc-a1b2c3d4) that belongs to
-- AWS account 123456789012.
-- https://ec2.amazonaws.com/?Action=CreateVpcPeeringConnection
-- &amp;VpcId=vpc-1a2b3c4d &amp;PeerVpcId=vpc-a1b2c3d4
-- &amp;PeerOwnerId=123456789012 &amp;AUTHPARAMS
-- &lt;CreateVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcPeeringConnection&gt;
-- &lt;vpcPeeringConnectionId&gt;pcx-73a5401a&lt;/vpcPeeringConnectionId&gt;
-- &lt;requesterVpcInfo&gt; &lt;ownerId&gt;777788889999&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.0.0/28&lt;/cidrBlock&gt; &lt;/requesterVpcInfo&gt;
-- &lt;accepterVpcInfo&gt; &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-a1b2c3d4&lt;/vpcId&gt; &lt;/accepterVpcInfo&gt;
-- &lt;status&gt; &lt;code&gt;initiating-request&lt;/code&gt;
-- &lt;message&gt;Initiating Request to 123456789012&lt;/message&gt;
-- &lt;/status&gt;
-- &lt;expirationTime&gt;2014-02-18T14:37:25.000Z&lt;/expirationTime&gt;
-- &lt;tagSet/&gt; &lt;/vpcPeeringConnection&gt;
-- &lt;/CreateVpcPeeringConnectionResponse&gt; Example 2 This example requests
-- a peering connection between your VPCs vpc-1a2b3c4d and vpc-11122233.
-- https://ec2.amazonaws.com/?Action=CreateVpcPeeringConnection
-- &amp;VpcId=vpc-1a2b3c4d &amp;PeerVpcId=vpc-11122233 &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateVpcPeeringConnection'

createVpcPeeringConnection :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => State CreateVpcPeeringConnection a
    -> m CreateVpcPeeringConnectionResponse
createVpcPeeringConnection s =
    send (mkCreateVpcPeeringConnection &~ s)

createVpcPeeringConnectionCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => State CreateVpcPeeringConnection a
    -> m (Either ServiceErr CreateVpcPeeringConnectionResponse)
createVpcPeeringConnectionCatch s =
    sendCatch (mkCreateVpcPeeringConnection &~ s)

-- $CreateVpnConnection
-- Creates a VPN connection between an existing virtual private gateway and a
-- VPN customer gateway. The only supported connection type is ipsec.1. The
-- response includes information that you need to give to your network
-- administrator to configure your customer gateway. We strongly recommend
-- that you use HTTPS when calling this operation because the response
-- contains sensitive cryptographic information for configuring your customer
-- gateway. If you decide to shut down your VPN connection for any reason and
-- later create a new VPN connection, you must reconfigure your customer
-- gateway with the new information returned from this call. For more
-- information about VPN connections, see Adding a Hardware Virtual Private
-- Gateway to Your VPC in the Amazon Virtual Private Cloud User Guide. Example
-- 1 This example creates a VPN connection between the virtual private gateway
-- with the ID vgw-8db04f81 and the customer gateway with the ID cgw-b4dc3961.
-- The response includes configuration information for the customer gateway.
-- Because it's a long set of information, we haven't included the complete
-- response here. To see an example of the configuation information, see the
-- Amazon Virtual Private Cloud Network Administrator Guide.
-- https://ec2.amazonaws.com/?Action=CreateVpnConnection &amp;Type=ipsec.1
-- &amp;CustomerGatewayId=cgw-b4dc3961 &amp;VpnGatewayId=vgw-8db04f81
-- &amp;AUTHPARAMS &lt;CreateVpnConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnConnection&gt;
-- &lt;vpnConnectionId&gt;vpn-44a8938f&lt;/vpnConnectionId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;customerGatewayConfiguration&gt;
-- ...Customer gateway configuration data in escaped XML format...
-- &lt;/customerGatewayConfiguration&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt; &lt;tagSet/&gt;
-- &lt;/vpnConnection&gt; &lt;/CreateVpnConnectionResponse&gt; Example 2 This
-- example creates a VPN connection with the static routes option between the
-- virtual private gateway with the ID vgw-8db04f81, and the customer gateway
-- with the ID cgw-b4dc3961, for a device that does not support the Border
-- Gateway Protocol (BGP). The response includes configuration information for
-- the VPN connection's customer gateway. Because it's a long set of
-- information, we haven't included the complete response here.
-- https://ec2.amazonaws.com/?Action=CreateVpnConnection &amp;Type=ipsec.1
-- &amp;CustomerGatewayId=cgw-b4dc3961 &amp;VpnGatewayId=vgw-8db04f81
-- &amp;Options.StaticRoutesOnly=true &amp;AUTHPARAMS
-- &lt;CreateVpnConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;5cc7891f-1f3b-4fc4-a626-bdea8f63ff5a&lt;/requestId&gt;
-- &lt;vpnConnection&gt;
-- &lt;vpnConnectionId&gt;vpn-83ad48ea&lt;/vpnConnectionId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;customerGatewayConfiguration&gt;
-- ...Customer gateway configuration data in escaped XML format...
-- &lt;/customerGatewayConfiguration&gt;
-- &lt;customerGatewayId&gt;cgw-63ae4b0a&lt;/customerGatewayId&gt;
-- &lt;vpnGatewayId&gt;vgw-4ea04527&lt;/vpnGatewayId&gt; &lt;options&gt;
-- &lt;staticRoutesOnly&gt;true&lt;/staticRoutesOnly&gt; &lt;/options&gt;
-- &lt;routes/&gt; &lt;/vpnConnection&gt;
-- &lt;/CreateVpnConnectionResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateVpnConnection'

createVpnConnection :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'cvcType'
    -> Text -- ^ 'cvcCustomerGatewayId'
    -> Text -- ^ 'cvcVpnGatewayId'
    -> State CreateVpnConnection a
    -> m CreateVpnConnectionResponse
createVpnConnection p1 p2 p3 s =
    send $ (mkCreateVpnConnection p1 p2 p3) &~ s

createVpnConnectionCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'cvcType'
    -> Text -- ^ 'cvcCustomerGatewayId'
    -> Text -- ^ 'cvcVpnGatewayId'
    -> State CreateVpnConnection a
    -> m (Either ServiceErr CreateVpnConnectionResponse)
createVpnConnectionCatch p1 p2 p3 s =
    sendCatch $ (mkCreateVpnConnection p1 p2 p3) &~ s

-- $CreateVpnConnectionRoute
-- Creates a static route associated with a VPN connection between an existing
-- virtual private gateway and a VPN customer gateway. The static route allows
-- traffic to be routed from the virtual private gateway to the VPN customer
-- gateway. For more information about VPN connections, see Adding a Hardware
-- Virtual Private Gateway to Your VPC in the Amazon Virtual Private Cloud
-- User Guide. Example This example creates a static route to the VPN
-- connection for the VPN connection with the ID vpn-83ad48ea to the
-- destination CIDR block 11.12.0.0/16. Note that when using the Query API the
-- "/" is denoted as "%2F".
-- https://ec2.amazonaws.com/?Action=CreateVpnConnectionRoute
-- &amp;DestinationCidrBlock=11.12.0.0%2F16 &amp;VpnConnectionId=vpn-83ad48ea
-- &amp;AUTHPARAMS &lt;CreateVpnConnectionRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/CreateVpnConnectionRouteResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateVpnConnectionRoute'

createVpnConnectionRoute :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'cvcr1VpnConnectionId'
    -> Text -- ^ 'cvcr1DestinationCidrBlock'
    -> State CreateVpnConnectionRoute a
    -> m CreateVpnConnectionRouteResponse
createVpnConnectionRoute p1 p2 s =
    send $ (mkCreateVpnConnectionRoute p1 p2) &~ s

createVpnConnectionRouteCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'cvcr1VpnConnectionId'
    -> Text -- ^ 'cvcr1DestinationCidrBlock'
    -> State CreateVpnConnectionRoute a
    -> m (Either ServiceErr CreateVpnConnectionRouteResponse)
createVpnConnectionRouteCatch p1 p2 s =
    sendCatch $ (mkCreateVpnConnectionRoute p1 p2) &~ s

-- $CreateVpnGateway
-- Creates a virtual private gateway. A virtual private gateway is the
-- endpoint on the VPC side of your VPN connection. You can create a virtual
-- private gateway before creating the VPC itself. For more information about
-- virtual private gateways, see Adding a Hardware Virtual Private Gateway to
-- Your VPC in the Amazon Virtual Private Cloud User Guide. Example This
-- example creates a virtual private gateway.
-- https://ec2.amazonaws.com/?Action=CreateVpnGateway &amp;Type=ipsec.1
-- &amp;AUTHPARAMS &lt;CreateVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnGateway&gt; &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;attachments/&gt; &lt;tagSet/&gt; &lt;/vpnGateway&gt;
-- &lt;/CreateVpnGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.CreateVpnGateway'

createVpnGateway :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => GatewayType -- ^ 'cvgType'
    -> State CreateVpnGateway a
    -> m CreateVpnGatewayResponse
createVpnGateway p1 s =
    send $ (mkCreateVpnGateway p1) &~ s

createVpnGatewayCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => GatewayType -- ^ 'cvgType'
    -> State CreateVpnGateway a
    -> m (Either ServiceErr CreateVpnGatewayResponse)
createVpnGatewayCatch p1 s =
    sendCatch $ (mkCreateVpnGateway p1) &~ s

-- $DeleteCustomerGateway
-- Deletes the specified customer gateway. You must delete the VPN connection
-- before you can delete the customer gateway. Example This example deletes
-- the specified customer gateway.
-- https://ec2.amazonaws.com/?Action=DeleteCustomerGateway
-- &amp;CustomerGatewayId=cgw-b4dc3961 &amp;AUTHPARAMS
-- &lt;DeleteCustomerGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteCustomerGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteCustomerGateway'

deleteCustomerGateway :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dcgCustomerGatewayId'
    -> State DeleteCustomerGateway a
    -> m DeleteCustomerGatewayResponse
deleteCustomerGateway p1 s =
    send $ (mkDeleteCustomerGateway p1) &~ s

deleteCustomerGatewayCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dcgCustomerGatewayId'
    -> State DeleteCustomerGateway a
    -> m (Either ServiceErr DeleteCustomerGatewayResponse)
deleteCustomerGatewayCatch p1 s =
    sendCatch $ (mkDeleteCustomerGateway p1) &~ s

-- $DeleteDhcpOptions
-- Deletes the specified set of DHCP options. You must disassociate the set of
-- DHCP options before you can delete it. You can disassociate the set of DHCP
-- options by associating either a new set of options or the default set of
-- options with the VPC. Example This example deletes the specified set of
-- DHCP options. https://ec2.amazonaws.com/?Action=DeleteDhcpOptions
-- &amp;DhcpOptionsId=dopt-7a8b9c2d &amp;AUTHPARAMS
-- &lt;DeleteDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteDhcpOptionsResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteDhcpOptions'

deleteDhcpOptions :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'ddoDhcpOptionsId'
    -> State DeleteDhcpOptions a
    -> m DeleteDhcpOptionsResponse
deleteDhcpOptions p1 s =
    send $ (mkDeleteDhcpOptions p1) &~ s

deleteDhcpOptionsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'ddoDhcpOptionsId'
    -> State DeleteDhcpOptions a
    -> m (Either ServiceErr DeleteDhcpOptionsResponse)
deleteDhcpOptionsCatch p1 s =
    sendCatch $ (mkDeleteDhcpOptions p1) &~ s

-- $DeleteInternetGateway
-- Deletes the specified Internet gateway. You must detach the Internet
-- gateway from the VPC before you can delete it. Example This example deletes
-- the specified Internet gateway.
-- https://ec2.amazonaws.com/?Action=DeleteInternetGateway
-- &amp;InternetGatewayId=igw-eaad4883 &amp;AUTHPARAMS
-- &lt;DeleteInternetGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteInternetGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteInternetGateway'

deleteInternetGateway :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'digInternetGatewayId'
    -> State DeleteInternetGateway a
    -> m DeleteInternetGatewayResponse
deleteInternetGateway p1 s =
    send $ (mkDeleteInternetGateway p1) &~ s

deleteInternetGatewayCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'digInternetGatewayId'
    -> State DeleteInternetGateway a
    -> m (Either ServiceErr DeleteInternetGatewayResponse)
deleteInternetGatewayCatch p1 s =
    sendCatch $ (mkDeleteInternetGateway p1) &~ s

-- $DeleteKeyPair
-- Deletes the specified key pair, by removing the public key from Amazon EC2.
-- Example This example request deletes the key pair named my-key-pair.
-- https://ec2.amazonaws.com/?Action=DeleteKeyPair &amp;KeyName=my-key-pair
-- &amp;AUTHPARAMS &lt;DeleteKeyPairResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteKeyPairResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteKeyPair'

deleteKeyPair :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'dkpKeyName'
    -> State DeleteKeyPair a
    -> m DeleteKeyPairResponse
deleteKeyPair p1 s =
    send $ (mkDeleteKeyPair p1) &~ s

deleteKeyPairCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'dkpKeyName'
    -> State DeleteKeyPair a
    -> m (Either ServiceErr DeleteKeyPairResponse)
deleteKeyPairCatch p1 s =
    sendCatch $ (mkDeleteKeyPair p1) &~ s

-- $DeleteNetworkAcl
-- Deletes the specified network ACL. You can't delete the ACL if it's
-- associated with any subnets. You can't delete the default network ACL.
-- Example This example deletes the specified network ACL.
-- https://ec2.amazonaws.com/?Action=DeleteNetworkAcl
-- &amp;NetworkAclId=acl-2cb85d45 &amp;AUTHPARAMS &lt;DeleteNetworkAclResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteNetworkAclResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteNetworkAcl'

deleteNetworkAcl :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'dnaNetworkAclId'
    -> State DeleteNetworkAcl a
    -> m DeleteNetworkAclResponse
deleteNetworkAcl p1 s =
    send $ (mkDeleteNetworkAcl p1) &~ s

deleteNetworkAclCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dnaNetworkAclId'
    -> State DeleteNetworkAcl a
    -> m (Either ServiceErr DeleteNetworkAclResponse)
deleteNetworkAclCatch p1 s =
    sendCatch $ (mkDeleteNetworkAcl p1) &~ s

-- $DeleteNetworkAclEntry
-- Deletes the specified ingress or egress entry (rule) from the specified
-- network ACL. Example This example deletes ingress rule number 100 from the
-- specified network ACL.
-- https://ec2.amazonaws.com/?Action=DeleteNetworkAclEntry
-- &amp;NetworkAclId=acl-2cb85d45 &amp;RuleNumber=100 &amp;AUTHPARAMS
-- &lt;DeleteNetworkAclEntryResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteNetworkAclEntryResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteNetworkAclEntry'

deleteNetworkAclEntry :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dnaeNetworkAclId'
    -> Integer -- ^ 'dnaeRuleNumber'
    -> Bool -- ^ 'dnaeEgress'
    -> State DeleteNetworkAclEntry a
    -> m DeleteNetworkAclEntryResponse
deleteNetworkAclEntry p1 p2 p3 s =
    send $ (mkDeleteNetworkAclEntry p1 p2 p3) &~ s

deleteNetworkAclEntryCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dnaeNetworkAclId'
    -> Integer -- ^ 'dnaeRuleNumber'
    -> Bool -- ^ 'dnaeEgress'
    -> State DeleteNetworkAclEntry a
    -> m (Either ServiceErr DeleteNetworkAclEntryResponse)
deleteNetworkAclEntryCatch p1 p2 p3 s =
    sendCatch $ (mkDeleteNetworkAclEntry p1 p2 p3) &~ s

-- $DeleteNetworkInterface
-- Deletes the specified network interface. You must detach the network
-- interface before you can delete it. Example This example deletes the
-- specified network interface.
-- https://ec2.amazonaws.com/?Action=DeleteNetworkInterface
-- &amp;NetworkInterfaceId=eni-ffda3197 &amp;AUTHPARAMS
-- &lt;DeleteNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;e1c6d73b-edaa-4e62-9909-6611404e1739&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteNetworkInterfaceResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteNetworkInterface'

deleteNetworkInterface :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'dniNetworkInterfaceId'
    -> State DeleteNetworkInterface a
    -> m DeleteNetworkInterfaceResponse
deleteNetworkInterface p1 s =
    send $ (mkDeleteNetworkInterface p1) &~ s

deleteNetworkInterfaceCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'dniNetworkInterfaceId'
    -> State DeleteNetworkInterface a
    -> m (Either ServiceErr DeleteNetworkInterfaceResponse)
deleteNetworkInterfaceCatch p1 s =
    sendCatch $ (mkDeleteNetworkInterface p1) &~ s

-- $DeletePlacementGroup
-- Deletes the specified placement group. You must terminate all instances in
-- the placement group before you can delete the placement group. For more
-- information about placement groups and cluster instances, see Cluster
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example deletes the placement group named XYZ-cluster.
-- https://ec2.amazonaws.com/?Action=DeletePlacementGroup
-- &amp;GroupName=XYZ-cluster &amp;AUTHPARAMS &lt;DeletePlacementGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeletePlacementGroupResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeletePlacementGroup'

deletePlacementGroup :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'dpgGroupName'
    -> State DeletePlacementGroup a
    -> m DeletePlacementGroupResponse
deletePlacementGroup p1 s =
    send $ (mkDeletePlacementGroup p1) &~ s

deletePlacementGroupCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dpgGroupName'
    -> State DeletePlacementGroup a
    -> m (Either ServiceErr DeletePlacementGroupResponse)
deletePlacementGroupCatch p1 s =
    sendCatch $ (mkDeletePlacementGroup p1) &~ s

-- $DeleteRoute
-- Deletes the specified route from the specified route table. Example This
-- example deletes the route with destination CIDR 172.16.1.0/24 from the
-- specified route table. https://ec2.amazonaws.com/?Action=DeleteRoute
-- &amp;RouteTableId=rtb-e4ad488d &amp;DestinationCidrBlock=172.16.1.0/24
-- &amp;AUTHPARMS &lt;DeleteRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteRouteResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteRoute'

deleteRoute :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'drRouteTableId'
    -> Text -- ^ 'drDestinationCidrBlock'
    -> State DeleteRoute a
    -> m DeleteRouteResponse
deleteRoute p1 p2 s =
    send $ (mkDeleteRoute p1 p2) &~ s

deleteRouteCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'drRouteTableId'
    -> Text -- ^ 'drDestinationCidrBlock'
    -> State DeleteRoute a
    -> m (Either ServiceErr DeleteRouteResponse)
deleteRouteCatch p1 p2 s =
    sendCatch $ (mkDeleteRoute p1 p2) &~ s

-- $DeleteRouteTable
-- Deletes the specified route table. You must disassociate the route table
-- from any subnets before you can delete it. You can't delete the main route
-- table. Example This example deletes the specified route table.
-- https://ec2.amazonaws.com/?Action=DeleteRouteTable
-- &amp;RouteTableId=rtb-e4ad488d &amp;AUTHPARAMS &lt;DeleteRouteTableResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteRouteTableResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteRouteTable'

deleteRouteTable :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'drtRouteTableId'
    -> State DeleteRouteTable a
    -> m DeleteRouteTableResponse
deleteRouteTable p1 s =
    send $ (mkDeleteRouteTable p1) &~ s

deleteRouteTableCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'drtRouteTableId'
    -> State DeleteRouteTable a
    -> m (Either ServiceErr DeleteRouteTableResponse)
deleteRouteTableCatch p1 s =
    sendCatch $ (mkDeleteRouteTable p1) &~ s

-- $DeleteSecurityGroup
-- Deletes a security group. If you attempt to delete a security group that is
-- associated with an instance, or is referenced by another security group,
-- the operation fails with InvalidGroup.InUse in EC2-Classic or
-- DependencyViolation in EC2-VPC. Example for EC2-Classic This example
-- deletes the specified security group for EC2-Classic.
-- https://ec2.amazonaws.com/?Action=DeleteSecurityGroup &amp;GroupName=websrv
-- &amp;AUTHPARAMS &lt;DeleteSecurityGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSecurityGroupResponse&gt;
-- Example for EC2-VPC his example deletes the specified security group for
-- EC2-VPC. https://ec2.amazonaws.com/?Action=DeleteSecurityGroup
-- &amp;GroupId=sg-1a2b3c4d &amp;AUTHPARAMS &lt;DeleteSecurityGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSecurityGroupResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteSecurityGroup'

deleteSecurityGroup :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => State DeleteSecurityGroup a
    -> m DeleteSecurityGroupResponse
deleteSecurityGroup s =
    send (mkDeleteSecurityGroup &~ s)

deleteSecurityGroupCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => State DeleteSecurityGroup a
    -> m (Either ServiceErr DeleteSecurityGroupResponse)
deleteSecurityGroupCatch s =
    sendCatch (mkDeleteSecurityGroup &~ s)

-- $DeleteSnapshot
-- Deletes the specified snapshot. When you make periodic snapshots of a
-- volume, the snapshots are incremental, and only the blocks on the device
-- that have changed since your last snapshot are saved in the new snapshot.
-- When you delete a snapshot, only the data not needed for any other snapshot
-- is removed. So regardless of which prior snapshots have been deleted, all
-- active snapshots will have access to all the information needed to restore
-- the volume. You cannot delete a snapshot of the root device of an Amazon
-- EBS volume used by a registered AMI. You must first de-register the AMI
-- before you can delete the snapshot. For more information, see Deleting an
-- Amazon EBS Snapshot in the Amazon Elastic Compute Cloud User Guide. Example
-- This example request deletes the snapshot with the ID snap-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DeleteSnapshot
-- &amp;SnapshotId.1=snap-1a2b3c4d &amp;AUTHPARAMS &lt;DeleteSnapshotResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSnapshotResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteSnapshot'

deleteSnapshot :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'dsSnapshotId'
    -> State DeleteSnapshot a
    -> m DeleteSnapshotResponse
deleteSnapshot p1 s =
    send $ (mkDeleteSnapshot p1) &~ s

deleteSnapshotCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dsSnapshotId'
    -> State DeleteSnapshot a
    -> m (Either ServiceErr DeleteSnapshotResponse)
deleteSnapshotCatch p1 s =
    sendCatch $ (mkDeleteSnapshot p1) &~ s

-- $DeleteSpotDatafeedSubscription
-- Deletes the datafeed for Spot Instances. For more information, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example request deletes the datafeed for the AWS account.
-- https://ec2.amazonaws.com/?Action=DeleteSpotDatafeedSubscription
-- &amp;AUTHPARAMS &lt;DeleteSpotDatafeedSubscriptionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteSpotDatafeedSubscriptionResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteSpotDatafeedSubscription'

deleteSpotDatafeedSubscription :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
    => State DeleteSpotDatafeedSubscription a
    -> m DeleteSpotDatafeedSubscriptionResponse
deleteSpotDatafeedSubscription s =
    send (mkDeleteSpotDatafeedSubscription &~ s)

deleteSpotDatafeedSubscriptionCatch :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadReader Env m
                                       )
    => State DeleteSpotDatafeedSubscription a
    -> m (Either ServiceErr DeleteSpotDatafeedSubscriptionResponse)
deleteSpotDatafeedSubscriptionCatch s =
    sendCatch (mkDeleteSpotDatafeedSubscription &~ s)

-- $DeleteSubnet
-- Deletes the specified subnet. You must terminate all running instances in
-- the subnet before you can delete the subnet. Example This example deletes
-- the specified subnet. https://ec2.amazonaws.com/?Action=DeleteSubnet
-- &amp;SubnetId=subnet-9d4a7b6c &amp;AUTHPARAMS &lt;DeleteSubnetResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSubnetResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteSubnet'

deleteSubnet :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'ds1SubnetId'
    -> State DeleteSubnet a
    -> m DeleteSubnetResponse
deleteSubnet p1 s =
    send $ (mkDeleteSubnet p1) &~ s

deleteSubnetCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'ds1SubnetId'
    -> State DeleteSubnet a
    -> m (Either ServiceErr DeleteSubnetResponse)
deleteSubnetCatch p1 s =
    sendCatch $ (mkDeleteSubnet p1) &~ s

-- $DeleteTags
-- Deletes the specified set of tags from the specified set of resources. This
-- call is designed to follow a DescribeTags request. For more information
-- about tags, see Tagging Your Resources in the Amazon Elastic Compute Cloud
-- User Guide. Example This example deletes the tags for the AMI with the ID
-- ami-1a2b3c4d. First, get a list of the tags by using the DescribeTags
-- request, then delete them. https://ec2.amazonaws.com/?Action=DeleteTags
-- &amp;ResourceId.1=ami-1a2b3c4d &amp;Tag.1.Key=webserver
-- &amp;Tag.2.Key=stack &amp;AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE
-- true Example This example deletes the stack and webserver tags for two
-- particular instances. https://ec2.amazonaws.com/?Action=DeleteTags
-- &amp;ResourceId.1=i-5f4e3d2a &amp;ResourceId.2=i-5f4e3d2a
-- &amp;Tag.1.Key=stack &amp;Tag.2.Key=webserver &amp;AUTHPARAMS Example You
-- can specify a tag key without a corresponding tag value to delete the tag
-- regardless of its value. This example request deletes all tags that have a
-- key of Purpose, regardless of the tag value.
-- https://ec2.amazonaws.com/?Action=DeleteTags &amp;ResourceId.1=i-5f4e3d2a
-- &amp;Tag.1.Key=Purpose &amp;AUTHPARAMS Example When you create a tag, you
-- can set the tag value to the empty string. Correspondingly, you can delete
-- only tags that have a specific key and whose value is the empty string.
-- This example request deletes all tags for the specified instance where the
-- key is Purpose and the tag value is the empty string.
-- https://ec2.amazonaws.com/?Action=DeleteTags &amp;ResourceId.1=i-5f4e3d2a
-- &amp;Tag.1.Key=Purpose &amp;Tag.2.Value= &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteTags'

deleteTags :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => [Text] -- ^ 'dtResources'
    -> State DeleteTags a
    -> m DeleteTagsResponse
deleteTags p1 s =
    send $ (mkDeleteTags p1) &~ s

deleteTagsCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => [Text] -- ^ 'dtResources'
    -> State DeleteTags a
    -> m (Either ServiceErr DeleteTagsResponse)
deleteTagsCatch p1 s =
    sendCatch $ (mkDeleteTags p1) &~ s

-- $DeleteVolume
-- Deletes the specified Amazon EBS volume. The volume must be in the
-- available state (not attached to an instance). The volume may remain in the
-- deleting state for several minutes. For more information, see Deleting an
-- Amazon EBS Volume in the Amazon Elastic Compute Cloud User Guide. Example
-- This example request deletes the volume with the ID vol-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DeleteVolume &amp;VolumeId=vol-1a2b3c4d
-- &amp;AUTHPARAMS &lt;DeleteVolumeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVolumeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteVolume'

deleteVolume :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'dvVolumeId'
    -> State DeleteVolume a
    -> m DeleteVolumeResponse
deleteVolume p1 s =
    send $ (mkDeleteVolume p1) &~ s

deleteVolumeCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'dvVolumeId'
    -> State DeleteVolume a
    -> m (Either ServiceErr DeleteVolumeResponse)
deleteVolumeCatch p1 s =
    sendCatch $ (mkDeleteVolume p1) &~ s

-- $DeleteVpc
-- Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete
-- all route tables associated with the VPC (except the default one), and so
-- on. Example This example deletes the specified VPC.
-- https://ec2.amazonaws.com/?Action=DeleteVpc &amp;VpcId=vpc-1a2b3c4d
-- &amp;AUTHPARAMS &lt;DeleteVpcResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVpcResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteVpc'

deleteVpc :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'dv1VpcId'
    -> State DeleteVpc a
    -> m DeleteVpcResponse
deleteVpc p1 s =
    send $ (mkDeleteVpc p1) &~ s

deleteVpcCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'dv1VpcId'
    -> State DeleteVpc a
    -> m (Either ServiceErr DeleteVpcResponse)
deleteVpcCatch p1 s =
    sendCatch $ (mkDeleteVpc p1) &~ s

-- $DeleteVpcPeeringConnection
-- Deletes a VPC peering connection. Either the owner of the requester VPC or
-- the owner of the peer VPC can delete the VPC peering connection if it's in
-- the active state. The owner of the requester VPC can delete a VPC peering
-- connection in the pending-acceptance state. Example This example deletes
-- the specified VPC peering connection.
-- https://ec2.amazonaws.com/?Action=DeleteVpcPeeringConnection
-- &amp;vpcPeeringConnectionId=pcx-1a2b3c4d &amp;AUTHPARAMS
-- &lt;DeleteVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteVpcPeeringConnectionResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteVpcPeeringConnection'

deleteVpcPeeringConnection :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dvpcVpcPeeringConnectionId'
    -> State DeleteVpcPeeringConnection a
    -> m DeleteVpcPeeringConnectionResponse
deleteVpcPeeringConnection p1 s =
    send $ (mkDeleteVpcPeeringConnection p1) &~ s

deleteVpcPeeringConnectionCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'dvpcVpcPeeringConnectionId'
    -> State DeleteVpcPeeringConnection a
    -> m (Either ServiceErr DeleteVpcPeeringConnectionResponse)
deleteVpcPeeringConnectionCatch p1 s =
    sendCatch $ (mkDeleteVpcPeeringConnection p1) &~ s

-- $DeleteVpnConnection
-- Deletes the specified VPN connection. If you're deleting the VPC and its
-- associated components, we recommend that you detach the virtual private
-- gateway from the VPC and delete the VPC before deleting the VPN connection.
-- Example This example deletes the specified VPN connection.
-- https://ec2.amazonaws.com/?Action=DeleteVpnConnection
-- &amp;vpnConnectionId=vpn-44a8938f &amp;AUTHPARAMS
-- &lt;DeleteVpnConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVpnConnectionResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteVpnConnection'

deleteVpnConnection :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dvcVpnConnectionId'
    -> State DeleteVpnConnection a
    -> m DeleteVpnConnectionResponse
deleteVpnConnection p1 s =
    send $ (mkDeleteVpnConnection p1) &~ s

deleteVpnConnectionCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'dvcVpnConnectionId'
    -> State DeleteVpnConnection a
    -> m (Either ServiceErr DeleteVpnConnectionResponse)
deleteVpnConnectionCatch p1 s =
    sendCatch $ (mkDeleteVpnConnection p1) &~ s

-- $DeleteVpnConnectionRoute
-- Deletes the specified static route associated with a VPN connection between
-- an existing virtual private gateway and a VPN customer gateway. The static
-- route allows traffic to be routed from the virtual private gateway to the
-- VPN customer gateway. Example This example deletes a static route to the
-- destination CIDR block 11.12.0.0/16 associated with the VPN connection with
-- the ID vpn-83ad48ea. Note that when using the Query API, the "/" is denoted
-- as "%2F". https://ec2.amazonaws.com/?Action=DeleteVpnConnectionRoute
-- &amp;DestinationCidrBlock=11.12.0.0%2F16 &amp;VpnConnectionId=vpn-83ad48ea
-- &amp;AUTHPARAMS &lt;DeleteVpnConnectionRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteVpnConnectionRouteResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteVpnConnectionRoute'

deleteVpnConnectionRoute :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'dvcrVpnConnectionId'
    -> Text -- ^ 'dvcrDestinationCidrBlock'
    -> State DeleteVpnConnectionRoute a
    -> m DeleteVpnConnectionRouteResponse
deleteVpnConnectionRoute p1 p2 s =
    send $ (mkDeleteVpnConnectionRoute p1 p2) &~ s

deleteVpnConnectionRouteCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'dvcrVpnConnectionId'
    -> Text -- ^ 'dvcrDestinationCidrBlock'
    -> State DeleteVpnConnectionRoute a
    -> m (Either ServiceErr DeleteVpnConnectionRouteResponse)
deleteVpnConnectionRouteCatch p1 p2 s =
    sendCatch $ (mkDeleteVpnConnectionRoute p1 p2) &~ s

-- $DeleteVpnGateway
-- Deletes the specified virtual private gateway. We recommend that before you
-- delete a virtual private gateway, you detach it from the VPC and delete the
-- VPN connection. Note that you don't need to delete the virtual private
-- gateway if you plan to delete and recreate the VPN connection between your
-- VPC and your network. Example This example deletes the specified virtual
-- private gateway. https://ec2.amazonaws.com/?Action=DeleteVpnGateway
-- &amp;vpnGatewayId=vgw-8db04f81 &amp;AUTHPARAMS &lt;DeleteVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVpnGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeleteVpnGateway'

deleteVpnGateway :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'dvgVpnGatewayId'
    -> State DeleteVpnGateway a
    -> m DeleteVpnGatewayResponse
deleteVpnGateway p1 s =
    send $ (mkDeleteVpnGateway p1) &~ s

deleteVpnGatewayCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dvgVpnGatewayId'
    -> State DeleteVpnGateway a
    -> m (Either ServiceErr DeleteVpnGatewayResponse)
deleteVpnGatewayCatch p1 s =
    sendCatch $ (mkDeleteVpnGateway p1) &~ s

-- $DeregisterImage
-- Deregisters the specified AMI. After you deregister an AMI, it can't be
-- used to launch new instances. This command does not delete the AMI. Example
-- This example request deregisters the specified AMI.
-- https://ec2.amazonaws.com/?Action=DeregisterImage &amp;ImageId=ami-4fa54026
-- &amp;AUTHPARAMS &lt;DeregisterImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeregisterImageResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DeregisterImage'

deregisterImage :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'diImageId'
    -> State DeregisterImage a
    -> m DeregisterImageResponse
deregisterImage p1 s =
    send $ (mkDeregisterImage p1) &~ s

deregisterImageCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'diImageId'
    -> State DeregisterImage a
    -> m (Either ServiceErr DeregisterImageResponse)
deregisterImageCatch p1 s =
    sendCatch $ (mkDeregisterImage p1) &~ s

-- $DescribeAccountAttributes
-- Describes the specified attribute of your AWS account. Example This example
-- describes the platforms that are supported by your AWS account. The first
-- response is for an account that supports only EC2-VPC. The second response
-- if for an account that supports both EC2-Classic and EC2-VPC.
-- https://ec2.amazonaws.com/?Action=DescribeAccountAttributes
-- &amp;AttributeName.1=supported-platforms &amp;AUTHPARAMS
-- &lt;DescribeAccountAttributesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;accountAttributeSet&gt; &lt;item&gt;
-- &lt;attributeName&gt;supported-platforms&lt;/attributeName&gt;
-- &lt;attributeValueSet&gt; &lt;item&gt;
-- &lt;attributeValue&gt;VPC&lt;/attributeValue&gt; &lt;/item&gt;
-- &lt;/attributeValueSet&gt; &lt;/item&gt; &lt;/accountAttributeSet&gt;
-- &lt;/DescribeAccountAttributesResponse&gt;
-- &lt;DescribeAccountAttributesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;accountAttributeSet&gt; &lt;item&gt;
-- &lt;attributeName&gt;supported-platforms&lt;/attributeName&gt;
-- &lt;attributeValueSet&gt; &lt;item&gt;
-- &lt;attributeValue&gt;EC2&lt;/attributeValue&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;attributeValue&gt;VPC&lt;/attributeValue&gt; &lt;/item&gt;
-- &lt;/attributeValueSet&gt; &lt;/item&gt; &lt;/accountAttributeSet&gt;
-- &lt;/DescribeAccountAttributesResponse&gt; Example 2 This example describes
-- the ID of your default VPC. The first response is for an account that
-- supports only EC2-VPC. The second response if for an account that supports
-- both EC2-Classic and EC2-VPC.
-- https://ec2.amazonaws.com/?Action=DescribeAccountAttributes
-- &amp;AttributeName.1=default-vpc &amp;AUTHPARAMS
-- &lt;DescribeAccountAttributesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;accountAttributeSet&gt; &lt;item&gt;
-- &lt;attributeName&gt;default-vpc&lt;/attributeName&gt;
-- &lt;attributeValueSet&gt; &lt;item&gt;
-- &lt;attributeValue&gt;vpc-xxxxxxxx&lt;/attributeValue&gt; &lt;/item&gt;
-- &lt;/attributeValueSet&gt; &lt;/item&gt; &lt;/accountAttributeSet&gt;
-- &lt;/DescribeAccountAttributesResponse&gt;
-- &lt;DescribeAccountAttributesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;accountAttributeSet&gt; &lt;item&gt;
-- &lt;attributeName&gt;default-vpc&lt;/attributeName&gt;
-- &lt;attributeValueSet&gt; &lt;item&gt;
-- &lt;attributeValue&gt;none&lt;/attributeValue&gt; &lt;/item&gt;
-- &lt;/attributeValueSet&gt; &lt;/item&gt; &lt;/accountAttributeSet&gt;
-- &lt;/DescribeAccountAttributesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeAccountAttributes'

describeAccountAttributes :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => State DescribeAccountAttributes a
    -> m DescribeAccountAttributesResponse
describeAccountAttributes s =
    send (mkDescribeAccountAttributes &~ s)

describeAccountAttributesCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => State DescribeAccountAttributes a
    -> m (Either ServiceErr DescribeAccountAttributesResponse)
describeAccountAttributesCatch s =
    sendCatch (mkDescribeAccountAttributes &~ s)

-- $DescribeAddresses
-- Describes one or more of your Elastic IP addresses. An Elastic IP address
-- is for use in either the EC2-Classic platform or in a VPC. For more
-- information, see Elastic IP Addresses in the Amazon Elastic Compute Cloud
-- User Guide. Example for EC2-Classic This example request describes two
-- specific Elastic IP addresses allocated to your account. Both addresses
-- were created for instances in EC2-Classic, so you must specify them using
-- their IP addresses. The address 192.0.2.1 is assigned to instance
-- i-f15ebb98, and 198.51.100.2 isn't assigned to an instance.
-- https://ec2.amazonaws.com/?Action=DescribeAddresses
-- &amp;PublicIp.1=192.0.2.1 &amp;PublicIp.2=198.51.100.2 &amp;AUTHPARAMS
-- &lt;DescribeAddressesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;addressesSet&gt; &lt;item&gt;
-- &lt;publicIp&gt;192.0.2.1&lt;/publicIp&gt;
-- &lt;domain&gt;standard&lt;/domain&gt;
-- &lt;instanceId&gt;i-f15ebb98&lt;/instanceId&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;publicIp&gt;198.51.100.2&lt;/publicIp&gt;
-- &lt;domain&gt;standard&lt;/domain&gt; &lt;instanceId/&gt; &lt;/item&gt;
-- &lt;/addressesSet&gt; &lt;/DescribeAddressesResponse&gt; Example 1 for
-- EC2-VPC This example request describes a specific Elastic IP address
-- allocated to your account. This address was created for instances in
-- EC2-VPC, so you must use the allocation ID to specify the address.
-- https://ec2.amazonaws.com/?Action=DescribeAddresses &amp;AllocationId.1=
-- eipalloc-08229861 &amp;AUTHPARAMS &lt;DescribeAddressesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;f7de5e98-491a-4c19-a92d-908d6EXAMPLE&lt;/requestId&gt;
-- &lt;addressesSet&gt; &lt;item&gt;
-- &lt;publicIp&gt;203.0.113.41&lt;/publicIp&gt;
-- &lt;allocationId&gt;eipalloc-08229861&lt;/allocationId&gt;
-- &lt;domain&gt;vpc&lt;/domain&gt;
-- &lt;instanceId&gt;i-64600030&lt;/instanceId&gt;
-- &lt;associationId&gt;eipassoc-f0229899&lt;/associationId&gt;
-- &lt;networkInterfaceId&gt;eni-ef229886&lt;/networkInterfaceId&gt;
-- &lt;networkInterfaceOwnerId&gt;053230519467&lt;/networkInterfaceOwnerId&gt;
-- &lt;privateIpAddress&gt;10.0.0.228&lt;/privateIpAddress&gt; &lt;/item&gt;
-- &lt;/addressesSet&gt; &lt;/DescribeAddressesResponse&gt; Example 2 for
-- EC2-VPC This example describes your Elastic IP addresses for EC2-VPC only.
-- https://ec2.amazonaws.com/?Action=DescribeAddresses
-- &amp;Filter.1.Name=domain &amp;Filter.1.Value.1=vpc &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeAddresses'

describeAddresses :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => State DescribeAddresses a
    -> m DescribeAddressesResponse
describeAddresses s =
    send (mkDescribeAddresses &~ s)

describeAddressesCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => State DescribeAddresses a
    -> m (Either ServiceErr DescribeAddressesResponse)
describeAddressesCatch s =
    sendCatch (mkDescribeAddresses &~ s)

-- $DescribeAvailabilityZones
-- Describes one or more of the Availability Zones that are available to you.
-- The results include zones only for the region you're currently using. If
-- there is an event impacting an Availability Zone, you can use this request
-- to view the state and any provided message for that Availability Zone. For
-- more information, see Regions and Availability Zones in the Amazon Elastic
-- Compute Cloud User Guide. Example This example request describes the
-- Availability Zones that are available to you. The response includes
-- Availability Zones only for the current region.
-- https://ec2.amazonaws.com/?Action=DescribeAvailabilityZones &amp;AUTHPARAMS
-- &lt;DescribeAvailabilityZonesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;availabilityZoneInfo&gt; &lt;item&gt;
-- &lt;zoneName&gt;us-east-1a&lt;/zoneName&gt;
-- &lt;zoneState&gt;available&lt;/zoneState&gt;
-- &lt;regionName&gt;us-east-1&lt;/regionName&gt; &lt;messageSet/&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;zoneName&gt;us-east-1b&lt;/zoneName&gt;
-- &lt;zoneState&gt;available&lt;/zoneState&gt;
-- &lt;regionName&gt;us-east-1&lt;/regionName&gt; &lt;messageSet/&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;zoneName&gt;us-east-1c&lt;/zoneName&gt;
-- &lt;zoneState&gt;available&lt;/zoneState&gt;
-- &lt;regionName&gt;us-east-1&lt;/regionName&gt; &lt;messageSet/&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;zoneName&gt;us-east-1d&lt;/zoneName&gt;
-- &lt;zoneState&gt;available&lt;/zoneState&gt;
-- &lt;regionName&gt;us-east-1&lt;/regionName&gt; &lt;messageSet/&gt;
-- &lt;/item&gt; &lt;/availabilityZoneInfo&gt;
-- &lt;/DescribeAvailabilityZonesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeAvailabilityZones'

describeAvailabilityZones :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => State DescribeAvailabilityZones a
    -> m DescribeAvailabilityZonesResponse
describeAvailabilityZones s =
    send (mkDescribeAvailabilityZones &~ s)

describeAvailabilityZonesCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => State DescribeAvailabilityZones a
    -> m (Either ServiceErr DescribeAvailabilityZonesResponse)
describeAvailabilityZonesCatch s =
    sendCatch (mkDescribeAvailabilityZones &~ s)

-- $DescribeBundleTasks
-- Describes one or more of your bundling tasks. Completed bundle tasks are
-- listed for only a limited time. If your bundle task is no longer in the
-- list, you can still register an AMI from it. Just use RegisterImage with
-- the Amazon S3 bucket name and image manifest name you provided to the
-- bundle task. Example 1 This example describes the status of the specified
-- bundle task. https://ec2.amazonaws.com/?Action=DescribeBundleTasks
-- &amp;bundleId.1=bun-c1a540a8 &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE i-12345678 bun-c1a540a8 cancelling
-- 2008-10-07T11:41:50.000Z 2008-10-07T11:51:50.000Z myawsbucket winami 20%
-- Example 2 This example filters the response to include only bundle tasks
-- whose state is either complete or failed, and in addition are targeted for
-- the Amazon S3 bucket named myawsbucket.
-- https://ec2.amazonaws.com/?Action=DescribeBundleTasks
-- &amp;Filter.1.Name=s3-bucket &amp;Filter.1.Value.1=myawsbucket
-- &amp;Filter.2.Name=state &amp;Filter.2.Name.1=complete
-- &amp;Filter.2.Name.2=failed &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeBundleTasks'

describeBundleTasks :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => State DescribeBundleTasks a
    -> m DescribeBundleTasksResponse
describeBundleTasks s =
    send (mkDescribeBundleTasks &~ s)

describeBundleTasksCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => State DescribeBundleTasks a
    -> m (Either ServiceErr DescribeBundleTasksResponse)
describeBundleTasksCatch s =
    sendCatch (mkDescribeBundleTasks &~ s)

-- $DescribeConversionTasks
-- Describes one or more of your conversion tasks. For more information, see
-- Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2
-- in the Amazon Elastic Compute Cloud User Guide. Example This example
-- describes all your conversion tasks.
-- https://ec2.amazonaws.com/?Action=DescribeConversionTasks &amp;AUTHPARAMS
-- import-i-fh95npoc 2010-12-22T12:01Z 1000 us-east-1a VDMK 128696320
-- https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- 8 vol-34d8a2ff active.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeConversionTasks'

describeConversionTasks :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => State DescribeConversionTasks a
    -> m DescribeConversionTasksResponse
describeConversionTasks s =
    send (mkDescribeConversionTasks &~ s)

describeConversionTasksCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => State DescribeConversionTasks a
    -> m (Either ServiceErr DescribeConversionTasksResponse)
describeConversionTasksCatch s =
    sendCatch (mkDescribeConversionTasks &~ s)

-- $DescribeCustomerGateways
-- Describes one or more of your VPN customer gateways. For more information
-- about VPN customer gateways, see Adding a Hardware Virtual Private Gateway
-- to Your VPC in the Amazon Virtual Private Cloud User Guide. Example 1 This
-- example request describes the specified customer gateway.
-- https://ec2.amazonaws.com/?Action=DescribeCustomerGateways
-- &amp;CustomerGatewayId.1=cgw-b4dc3961 &amp;AUTHPARAMS
-- &lt;DescribeCustomerGatewaysResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;customerGatewaySet&gt; &lt;item&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;state&gt;available&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;ipAddress&gt;12.1.2.3&lt;/ipAddress&gt;
-- &lt;bgpAsn&gt;65534&lt;/bgpasn&gt; &lt;tagSet/&gt; &lt;/item&gt;
-- &lt;/customerGatewaySet&gt; &lt;/DescribeCustomerGatewaysResponse&gt;
-- Example 2 This example request uses filters to describe any customer
-- gateway you own whose IP address is 12.1.2.3, and whose state is either
-- pending or available.
-- https://ec2.amazonaws.com/?Action=DescribeCustomerGateways
-- &amp;Filter.1.Name=ip-address &amp;Filter.1.Value.1=12.1.2.3
-- &amp;Filter.2.Name=state &amp;Filter.2.Value.1=pending
-- &amp;Filter.2.Value.2=available &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeCustomerGateways'

describeCustomerGateways :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => State DescribeCustomerGateways a
    -> m DescribeCustomerGatewaysResponse
describeCustomerGateways s =
    send (mkDescribeCustomerGateways &~ s)

describeCustomerGatewaysCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => State DescribeCustomerGateways a
    -> m (Either ServiceErr DescribeCustomerGatewaysResponse)
describeCustomerGatewaysCatch s =
    sendCatch (mkDescribeCustomerGateways &~ s)

-- $DescribeDhcpOptions
-- Describes one or more of your DHCP options sets. For more information about
-- DHCP options sets, see DHCP Options Sets in the Amazon Virtual Private
-- Cloud User Guide. Example 1 This example describes the specified DHCP
-- options set. https://ec2.amazonaws.com/?Action=DescribeDhcpOptions
-- &amp;DhcpOptionsId.1=dopt-7a8b9c2d &amp;AUTHPARAMS
-- &lt;DescribeDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;dhcpOptionsSet&gt; &lt;item&gt;
-- &lt;dhcpOptionsId&gt;dopt-7a8b9c2d&lt;/dhcpOptionsId&gt;
-- &lt;dhcpConfigurationSet&gt; &lt;item&gt;
-- &lt;key&gt;domain-name&lt;/key&gt; &lt;valueSet&gt; &lt;item&gt;
-- &lt;value&gt;example.com&lt;/value&gt; &lt;/item&gt; &lt;/valueSet&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;key&gt;domain-name-servers&lt;/key&gt;
-- &lt;valueSet&gt; &lt;item&gt; &lt;value&gt;10.2.5.1&lt;/value&gt;
-- &lt;/item&gt; &lt;/valueSet&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;key&gt;domain-name-servers&lt;/key&gt; &lt;valueSet&gt; &lt;item&gt;
-- &lt;value&gt;10.2.5.2&lt;/value&gt; &lt;/item&gt; &lt;/valueSet&gt;
-- &lt;/item&gt; &lt;/dhcpConfigurationSet&gt; &lt;tagSet/&gt; &lt;/item&gt;
-- &lt;/dhcpOptionsSet&gt; &lt;/DescribeDhcpOptionsResponse&gt; Example 2 This
-- example uses filters to describe any DHCP options set that includes a
-- domain-name option whose value includes the string example.
-- https://ec2.amazonaws.com/?Action=DescribeDhcpOptions
-- &amp;Filter.1.Name=key &amp;Filter.1.Value.1=domain-name
-- &amp;Filter.2.Name=value &amp;Filter.2.Value.1=*example* &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeDhcpOptions'

describeDhcpOptions :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => State DescribeDhcpOptions a
    -> m DescribeDhcpOptionsResponse
describeDhcpOptions s =
    send (mkDescribeDhcpOptions &~ s)

describeDhcpOptionsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => State DescribeDhcpOptions a
    -> m (Either ServiceErr DescribeDhcpOptionsResponse)
describeDhcpOptionsCatch s =
    sendCatch (mkDescribeDhcpOptions &~ s)

-- $DescribeExportTasks
-- Describes one or more of your export tasks. Example This example describes
-- a single export task. https://ec2.amazonaws.com/?Action=DescribeExportTasks
-- &amp;exportTaskId.1=export-i-1234wxyz &amp;AUTHPARAMS
-- &lt;DescribeExportTasksResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;exportTaskSet&gt; &lt;item&gt;
-- &lt;exportTaskId&gt;export-i-1234wxyz&lt;/exportTaskId&gt;
-- &lt;description&gt;Example for docs&lt;/description&gt;
-- &lt;state&gt;active&lt;/state&gt;
-- &lt;statusMessage&gt;Running&lt;/statusMessage&gt; &lt;instanceExport&gt;
-- &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;targetEnvironment&gt;VMWare&lt;/targetEnvironment&gt;
-- &lt;/instanceExport&gt; &lt;exportToS3&gt;
-- &lt;diskImageFormat&gt;VMDK&lt;/diskImageFormat&gt;
-- &lt;containerFormat&gt;OVA&lt;/containerFormat&gt;
-- &lt;s3Bucket&gt;my-bucket-for-exported-vm&lt;/s3Bucket&gt;
-- &lt;s3Key&gt;my-exports/ export-i-1234wxyz .ova&lt;/s3Key&gt;
-- &lt;/exportToS3&gt; &lt;/item&gt; &lt;/exportTaskSet&gt; &lt;/
-- DescribeExportTasksResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeExportTasks'

describeExportTasks :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => State DescribeExportTasks a
    -> m DescribeExportTasksResponse
describeExportTasks s =
    send (mkDescribeExportTasks &~ s)

describeExportTasksCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => State DescribeExportTasks a
    -> m (Either ServiceErr DescribeExportTasksResponse)
describeExportTasksCatch s =
    sendCatch (mkDescribeExportTasks &~ s)

-- $DescribeImageAttribute
-- Describes the specified attribute of the specified AMI. You can specify
-- only one attribute at a time. Example 1 This example lists the launch
-- permissions for the specified AMI.
-- https://ec2.amazonaws.com/?Action=DescribeImageAttribute
-- &amp;ImageId=ami-61a54008 &amp;Attribute=launchPermission &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-61a54008 all 495219933132 Example
-- 2 This example lists the product codes for the specified AMI.
-- https://ec2.amazonaws.com/?Action=DescribeImageAttribute
-- &amp;ImageId=ami-2bb65342 &amp;Attribute=productCodes &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-2bb65342 a1b2c3d4e5f6g7h8i9j10k11
-- marketplace.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeImageAttribute'

describeImageAttribute :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'diaImageId'
    -> ImageAttributeName -- ^ 'diaAttribute'
    -> State DescribeImageAttribute a
    -> m DescribeImageAttributeResponse
describeImageAttribute p1 p2 s =
    send $ (mkDescribeImageAttribute p1 p2) &~ s

describeImageAttributeCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'diaImageId'
    -> ImageAttributeName -- ^ 'diaAttribute'
    -> State DescribeImageAttribute a
    -> m (Either ServiceErr DescribeImageAttributeResponse)
describeImageAttributeCatch p1 p2 s =
    sendCatch $ (mkDescribeImageAttribute p1 p2) &~ s

-- $DescribeImages
-- Describes one or more of the images (AMIs, AKIs, and ARIs) available to
-- you. Images available to you include public images, private images that you
-- own, and private images owned by other AWS accounts but for which you have
-- explicit launch permissions. Deregistered images are included in the
-- returned results for an unspecified interval after deregistration. Example
-- 1 This example describes the specified AMI.
-- https://ec2.amazonaws.com/?Action=DescribeImages
-- &amp;ImageId.1=ami-be3adfd7 &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-1a2b3c4d amazon/getting-started
-- available 123456789012 true i386 machine aki-1a2b3c4d ari-1a2b3c4d amazon
-- getting-started Image Description ebs /dev/sda /dev/sda1 snap-1a2b3c4d 15
-- false standard paravirtual xen Example 2 This example filters the response
-- to include only public Windows images with an x86_64 architecture.
-- https://ec2.amazonaws.com/?Action=DescribeImages
-- &amp;Filter.1.Name=is-public &amp;Filter.1.Value.1=true
-- &amp;Filter.2.Name=architecture &amp;Filter.2.Value.1=x86_64
-- &amp;Filter.3.Name=platform &amp;Filter.3.Value.1=windows &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-1a2b3c4d
-- ec2-public-windows-images/Server2003r2-x86_64-Win-v1.07.manifest.xml
-- available 123456789012 true x86_64 machine windows amazon instance-store
-- hvm xen ... Example 3 This example returns the results to display images
-- where the owner is aws-marketplace.
-- https://ec2.amazonaws.com/?Action=DescribeImages
-- &amp;Owner.0=aws-marketplace &amp;AUTHPARAMS
-- 4a4a27a2-2e7c-475d-b35b-ca822EXAMPLE ami-1a2b3c4d
-- aws-marketplace/example-marketplace-amzn-ami.1 available 123456789012 true
-- a1b2c3d4e5f6g7h8i9j10k11 marketplace i386 machine aki-1a2b3c4d
-- aws-marketplace example-marketplace-amzn-ami.1 Amazon Linux AMI i386 EBS
-- ebs /dev/sda1 /dev/sda1 snap-1a2b3c4d 8 true paravirtual xen ...
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeImages'

describeImages :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => State DescribeImages a
    -> m DescribeImagesResponse
describeImages s =
    send (mkDescribeImages &~ s)

describeImagesCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => State DescribeImages a
    -> m (Either ServiceErr DescribeImagesResponse)
describeImagesCatch s =
    sendCatch (mkDescribeImages &~ s)

-- $DescribeInstanceAttribute
-- Describes the specified attribute of the specified instance. You can
-- specify only one attribute at a time. Example 1 This example lists the
-- instance type of the specified instance.
-- https://ec2.amazonaws.com/?Action=DescribeInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;Attribute=instanceType &amp;AUTHPARAMS
-- &lt;DescribeInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt; &lt;instanceType&gt;
-- &lt;value&gt;t1.micro&lt;/value&gt; &lt;/instanceType&gt;
-- &lt;/DescribeInstanceAttributeResponse&gt; Example 2 This example lists the
-- current value of the InstanceInitiatedShutdownBehavior attribute for the
-- specified instance.
-- https://ec2.amazonaws.com/?Action=DescribeInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;Attribute=instanceInitiatedShutdownBehavior
-- &amp;AUTHPARAMS &lt;DescribeInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt;
-- &lt;instanceInitiatedShutdownBehavior&gt; &lt;value&gt;stop&lt;/value&gt;
-- &lt;/instanceInitiatedShutdownBehavior&gt;
-- &lt;/DescribeInstanceAttributeResponse&gt; Example 3 This example lists the
-- current value of the DisableApiTermination attribute for the specified
-- instance. https://ec2.amazonaws.com/?Action=DescribeInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;Attribute=disableApiTermination
-- &amp;AUTHPARAMS &lt;DescribeInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt;
-- &lt;disableApiTermination&gt; &lt;value&gt;false&lt;/value&gt;
-- &lt;/disableApiTermination&gt; &lt;/DescribeInstanceAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeInstanceAttribute'

describeInstanceAttribute :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dia1InstanceId'
    -> InstanceAttributeName -- ^ 'dia1Attribute'
    -> State DescribeInstanceAttribute a
    -> m DescribeInstanceAttributeResponse
describeInstanceAttribute p1 p2 s =
    send $ (mkDescribeInstanceAttribute p1 p2) &~ s

describeInstanceAttributeCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'dia1InstanceId'
    -> InstanceAttributeName -- ^ 'dia1Attribute'
    -> State DescribeInstanceAttribute a
    -> m (Either ServiceErr DescribeInstanceAttributeResponse)
describeInstanceAttributeCatch p1 p2 s =
    sendCatch $ (mkDescribeInstanceAttribute p1 p2) &~ s

-- $DescribeInstanceStatus
-- Describes the status of one or more instances, including any scheduled
-- events. Instance status has two main components: System Status reports
-- impaired functionality that stems from issues related to the systems that
-- support an instance, such as such as hardware failures and network
-- connectivity problems. This call reports such problems as impaired
-- reachability. Instance Status reports impaired functionality that arises
-- from problems internal to the instance. This call reports such problems as
-- impaired reachability. Instance status provides information about four
-- types of scheduled events for an instance that may require your attention:
-- Scheduled Reboot: When Amazon EC2 determines that an instance must be
-- rebooted, the instances status returns one of two event codes:
-- system-reboot or instance-reboot. System reboot commonly occurs if certain
-- maintenance or upgrade operations require a reboot of the underlying host
-- that supports an instance. Instance reboot commonly occurs if the instance
-- must be rebooted, rather than the underlying host. Rebooting events include
-- a scheduled start and end time. System Maintenance: When Amazon EC2
-- determines that an instance requires maintenance that requires power or
-- network impact, the instance status is the event code system-maintenance.
-- System maintenance is either power maintenance or network maintenance. For
-- power maintenance, your instance will be unavailable for a brief period of
-- time and then rebooted. For network maintenance, your instance will
-- experience a brief loss of network connectivity. System maintenance events
-- include a scheduled start and end time. You will also be notified by email
-- if one of your instances is set for system maintenance. The email message
-- indicates when your instance is scheduled for maintenance. Scheduled
-- Retirement: When Amazon EC2 determines that an instance must be shut down,
-- the instance status is the event code instance-retirement. Retirement
-- commonly occurs when the underlying host is degraded and must be replaced.
-- Retirement events include a scheduled start and end time. You will also be
-- notified by email if one of your instances is set to retiring. The email
-- message indicates when your instance will be permanently retired. Scheduled
-- Stop: When Amazon EC2 determines that an instance must be shut down, the
-- instances status returns an event code called instance-stop. Stop events
-- include a scheduled start and end time. You will also be notified by email
-- if one of your instances is set to stop. The email message indicates when
-- your instance will be stopped. When your instance is retired, it will
-- either be terminated (if its root device type is the instance-store) or
-- stopped (if its root device type is an EBS volume). Instances stopped due
-- to retirement will not be restarted, but you can do so manually. You can
-- also avoid retirement of EBS-backed instances by manually restarting your
-- instance when its event code is instance-retirement. This ensures that your
-- instance is started on a different underlying host. For more information
-- about failed status checks, see Troubleshooting Instances with Failed
-- Status Checks in the Amazon Elastic Compute Cloud User Guide. For more
-- information about working with scheduled events, see Working with an
-- Instance That Has a Scheduled Event in the Amazon Elastic Compute Cloud
-- User Guide. Example 1 This example returns instance status descriptions for
-- all instances. https://ec2.amazonaws.com/? Action=DescribeInstanceStatus
-- &amp;AUTHPARAMS Example 2 This example returns instance status descriptions
-- for the specified instances. https://ec2.amazonaws.com/?
-- Action=DescribeInstanceStatus &amp;InstanceId.0=i-1a2b3c4d
-- &amp;InstanceId.1=i-2a2b3c4d &amp;AUTHPARAMS Example 3 This example returns
-- instance status descriptions for all instances specified by supported
-- DescribeInstanceStatus filters. https://ec2.amazonaws.com/?
-- Action=DescribeInstanceStatus &amp;Filter.0.Name=system-status.reachability
-- &amp;Filter.0.Value.failed &amp;AUTHPARAMS
-- 3be1508e-c444-4fef-89cc-0b1223c4f02fEXAMPLE i-1a2b3c4d us-east-1d 16
-- running impaired reachability failed YYYY-MM-DDTHH:MM:SS.000Z impaired
-- reachability failed YYYY-MM-DDTHH:MM:SS.000Z instance-retirement The
-- instance is running on degraded hardware YYYY-MM-DDTHH:MM:SS+0000
-- YYYY-MM-DDTHH:MM:SS+0000 i-2a2b3c4d us-east-1d 16 running ok reachability
-- passed ok reachability passed instance-reboot The instance is scheduled for
-- a reboot YYYY-MM-DDTHH:MM:SS+0000 YYYY-MM-DDTHH:MM:SS+0000 i-3a2b3c4d
-- us-east-1d 16 running ok reachability passed ok reachability passed
-- i-4a2b3c4d us-east-1d 16 running ok reachability passed insufficient-data
-- reachability insufficient-data.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeInstanceStatus'

describeInstanceStatus :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env (ResumableSource m)
                          )
    => State DescribeInstanceStatus a
    -> ResumableSource m DescribeInstanceStatusResponse
describeInstanceStatus s =
    paginate (mkDescribeInstanceStatus &~ s)

describeInstanceStatusCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env (ResumableSource m)
                               )
    => State DescribeInstanceStatus a
    -> ResumableSource m (Either ServiceErr DescribeInstanceStatusResponse)
describeInstanceStatusCatch s =
    paginateCatch (mkDescribeInstanceStatus &~ s)

-- $DescribeInstances
-- Describes one or more of your instances. If you specify one or more
-- instance IDs, Amazon EC2 returns information for those instances. If you do
-- not specify instance IDs, Amazon EC2 returns information for all relevant
-- instances. If you specify an instance ID that is not valid, an error is
-- returned. If you specify an instance that you do not own, it is not
-- included in the returned results. Recently terminated instances might
-- appear in the returned results. This interval is usually less than one
-- hour. Example 1 This example describes all instances owned by your AWS
-- account. The example response shows information for one instance.
-- https://ec2.amazonaws.com/?Action=DescribeInstances &amp;AUTHPARAMS
-- &lt;DescribeInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;fdcdcab1-ae5c-489e-9c33-4637c5dda355&lt;/requestId&gt;
-- &lt;reservationSet&gt; &lt;item&gt;
-- &lt;reservationId&gt;r-1a2b3c4d&lt;/reservationId&gt;
-- &lt;ownerId&gt;123456789012&lt;/ownerId&gt; &lt;groupSet&gt; &lt;item&gt;
-- &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;groupName&gt;my-security-group&lt;/groupName&gt; &lt;/item&gt;
-- &lt;/groupSet&gt; &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-1a2b3c4d&lt;/instanceId&gt;
-- &lt;imageId&gt;ami-1a2b3c4d&lt;/imageId&gt; &lt;instanceState&gt;
-- &lt;code&gt;16&lt;/code&gt; &lt;name&gt;running&lt;/name&gt;
-- &lt;/instanceState&gt; &lt;privateDnsName/&gt; &lt;dnsName/&gt;
-- &lt;reason/&gt; &lt;keyName&gt;my-key-pair&lt;/keyName&gt;
-- &lt;amiLaunchIndex&gt;0&lt;/amiLaunchIndex&gt; &lt;productCodes/&gt;
-- &lt;instanceType&gt;c1.medium&lt;/instanceType&gt;
-- &lt;launchTime&gt;YYYY-MM-DDTHH:MM:SS+0000&lt;/launchTime&gt;
-- &lt;placement&gt;
-- &lt;availabilityZone&gt;us-west-2a&lt;/availabilityZone&gt;
-- &lt;groupName/&gt; &lt;tenancy&gt;default&lt;/tenancy&gt;
-- &lt;/placement&gt; &lt;platform&gt;windows&lt;/platform&gt;
-- &lt;monitoring&gt; &lt;state&gt;disabled&lt;/state&gt; &lt;/monitoring&gt;
-- &lt;subnetId&gt;subnet-1a2b3c4d&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;privateIpAddress&gt;10.0.0.12&lt;/privateIpAddress&gt;
-- &lt;ipAddress&gt;46.51.219.63&lt;/ipAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;groupName&gt;my-security-group&lt;/groupName&gt; &lt;/item&gt;
-- &lt;/groupSet&gt; &lt;architecture&gt;x86_64&lt;/architecture&gt;
-- &lt;rootDeviceType&gt;ebs&lt;/rootDeviceType&gt;
-- &lt;rootDeviceName&gt;/dev/sda1&lt;/rootDeviceName&gt;
-- &lt;blockDeviceMapping&gt; &lt;item&gt;
-- &lt;deviceName&gt;/dev/sda1&lt;/deviceName&gt; &lt;ebs&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;status&gt;attached&lt;/status&gt;
-- &lt;attachTime&gt;YYYY-MM-DDTHH:MM:SS.SSSZ&lt;/attachTime&gt;
-- &lt;deleteOnTermination&gt;true&lt;/deleteOnTermination&gt; &lt;/ebs&gt;
-- &lt;/item&gt; &lt;/blockDeviceMapping&gt;
-- &lt;virtualizationType&gt;hvm&lt;/virtualizationType&gt;
-- &lt;clientToken&gt;ABCDE1234567890123&lt;/clientToken&gt; &lt;tagSet&gt;
-- &lt;item&gt; &lt;key&gt;Name&lt;/key&gt; &lt;value&gt;Windows
-- Instance&lt;/value&gt; &lt;/item&gt; &lt;/tagSet&gt;
-- &lt;hypervisor&gt;xen&lt;/hypervisor&gt; &lt;networkInterfaceSet&gt;
-- &lt;item&gt;
-- &lt;networkInterfaceId&gt;eni-1a2b3c4d&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-1a2b3c4d&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt; &lt;description&gt;Primary network
-- interface&lt;/description&gt; &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;status&gt;in-use&lt;/status&gt;
-- &lt;macAddress&gt;1b:2b:3c:4d:5e:6f&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.0.12&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;groupName&gt;my-security-group&lt;/groupName&gt; &lt;/item&gt;
-- &lt;/groupSet&gt; &lt;attachment&gt;
-- &lt;attachmentId&gt;eni-attach-1a2b3c4d&lt;/attachmentId&gt;
-- &lt;deviceIndex&gt;0&lt;/deviceIndex&gt;
-- &lt;status&gt;attached&lt;/status&gt;
-- &lt;attachTime&gt;YYYY-MM-DDTHH:MM:SS+0000&lt;/attachTime&gt;
-- &lt;deleteOnTermination&gt;true&lt;/deleteOnTermination&gt;
-- &lt;/attachment&gt; &lt;association&gt;
-- &lt;publicIp&gt;198.51.100.63&lt;/publicIp&gt;
-- &lt;ipOwnerId&gt;123456789012&lt;/ipOwnerId&gt; &lt;/association&gt;
-- &lt;privateIpAddressesSet&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.0.12&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;association&gt;
-- &lt;publicIp&gt;198.51.100.63&lt;/publicIp&gt;
-- &lt;ipOwnerId&gt;123456789012&lt;/ipOwnerId&gt; &lt;/association&gt;
-- &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.0.14&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;association&gt;
-- &lt;publicIp&gt;198.51.100.177&lt;/publicIp&gt;
-- &lt;ipOwnerId&gt;123456789012&lt;/ipOwnerId&gt; &lt;/association&gt;
-- &lt;/item&gt; &lt;/privateIpAddressesSet&gt; &lt;/item&gt;
-- &lt;/networkInterfaceSet&gt; &lt;/item&gt; &lt;/instancesSet&gt;
-- &lt;/item&gt; &lt;/reservationSet&gt; &lt;/DescribeInstancesResponse&gt;
-- Example 2 This example describes only the instances that have the m1.small
-- or m1.large instance type and an attached Amazon EBS volume that will be
-- deleted on termination. https://ec2.amazonaws.com/?Action=DescribeInstances
-- &amp;Filter.1.Name=instance-type &amp;Filter.1.Value.1=m1.small
-- &amp;Filter.1.Value.2=m1.large
-- &amp;Filter.2.Name=block-device-mapping.status
-- &amp;Filter.2.Value.1=attached
-- &amp;Filter.3.Name=block-device-mapping.delete-on-termination
-- &amp;Filter.3.Value.1=true &amp;AUTHPARAMS Example 3 This example describes
-- all instances that are running in a VPC.
-- https://ec2.amazonaws.com/?Action=DescribeInstances
-- &amp;Filter.1.Name=vpc-id &amp;Filter.1.Value.1=* &amp;AUTHPARAMS Example 4
-- This example describes any instances that have a tag with the key Owner,
-- regardless of the value of the tag.
-- https://ec2.amazonaws.com/?Action=DescribeInstances
-- &amp;Filter.1.Name=tag-key &amp;Filter.1.Value.1=Owner &amp;AUTHPARAMS
-- Example This example lists only the instances that have a tag with the key
-- Owner and the value DbAdmin.
-- https://ec2.amazonaws.com/?Action=DescribeInstances
-- &amp;Filter.1.Name=tag:Owner &amp;Filter.1.Value.1=DbAdmin &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeInstances'

describeInstances :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env (ResumableSource m)
                     )
    => State DescribeInstances a
    -> ResumableSource m DescribeInstancesResponse
describeInstances s =
    paginate (mkDescribeInstances &~ s)

describeInstancesCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env (ResumableSource m)
                          )
    => State DescribeInstances a
    -> ResumableSource m (Either ServiceErr DescribeInstancesResponse)
describeInstancesCatch s =
    paginateCatch (mkDescribeInstances &~ s)

-- $DescribeInternetGateways
-- Describes one or more of your Internet gateways. Example This example
-- describes all your Internet gateways.
-- https://ec2.amazonaws.com/?Action=DescribeInternetGateways &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE igw-eaad4883EXAMPLE vpc-11ad4878
-- available.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeInternetGateways'

describeInternetGateways :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => State DescribeInternetGateways a
    -> m DescribeInternetGatewaysResponse
describeInternetGateways s =
    send (mkDescribeInternetGateways &~ s)

describeInternetGatewaysCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => State DescribeInternetGateways a
    -> m (Either ServiceErr DescribeInternetGatewaysResponse)
describeInternetGatewaysCatch s =
    sendCatch (mkDescribeInternetGateways &~ s)

-- $DescribeKeyPairs
-- Describes one or more of your key pairs. For more information about key
-- pairs, see Key Pairs in the Amazon Elastic Compute Cloud User Guide.
-- Example This example describes the keypair with name my-key-pair.
-- https://ec2.amazonaws.com/?Action=DescribeKeyPairs
-- &amp;KeyName.1=my-key-pair &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE my-key-pair
-- 1f:51:ae:28:bf:89:e9:d8:1f:25:5d:37:2d:7d:b8:ca:9f:f5:f1:6f Example This
-- example filters the response to include only key pairs whose names include
-- the string Dave. https://ec2.amazonaws.com/?Action=DescribeKeyPairs
-- &amp;Filter.1.Name=key-name &amp;Filter.1.Value.1=*Dave* &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeKeyPairs'

describeKeyPairs :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => State DescribeKeyPairs a
    -> m DescribeKeyPairsResponse
describeKeyPairs s =
    send (mkDescribeKeyPairs &~ s)

describeKeyPairsCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => State DescribeKeyPairs a
    -> m (Either ServiceErr DescribeKeyPairsResponse)
describeKeyPairsCatch s =
    sendCatch (mkDescribeKeyPairs &~ s)

-- $DescribeNetworkAcls
-- Describes one or more of your network ACLs. For more information about
-- network ACLs, see Network ACLs in the Amazon Virtual Private Cloud User
-- Guide. Example This example describes all your network ACLs.
-- https://ec2.amazonaws.com/?Action=DescribeNetworkAcls &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE acl-5566953c vpc-5266953b true 100 all
-- allow true 0.0.0.0/0 32767 all deny true 0.0.0.0/0 100 all allow false
-- 0.0.0.0/0 32767 all deny false 0.0.0.0/0 acl-5d659634 vpc-5266953b false
-- 110 6 allow true 0.0.0.0/0 49152 65535 32767 all deny true 0.0.0.0/0 110 6
-- allow false 0.0.0.0/0 80 80 120 6 allow false 0.0.0.0/0 443 443 32767 all
-- deny false 0.0.0.0/0 aclassoc-5c659635 acl-5d659634 subnet-ff669596
-- aclassoc-c26596ab acl-5d659634 subnet-f0669599.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeNetworkAcls'

describeNetworkAcls :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => State DescribeNetworkAcls a
    -> m DescribeNetworkAclsResponse
describeNetworkAcls s =
    send (mkDescribeNetworkAcls &~ s)

describeNetworkAclsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => State DescribeNetworkAcls a
    -> m (Either ServiceErr DescribeNetworkAclsResponse)
describeNetworkAclsCatch s =
    sendCatch (mkDescribeNetworkAcls &~ s)

-- $DescribeNetworkInterfaceAttribute
-- Describes a network interface attribute. You can specify only one attribute
-- at a time. Example This example describes the sourceDestCheck attribute of
-- the specified network interface.
-- https://ec2.amazonaws.com/?Action=DescribeNetworkInterfaceAttribute
-- &amp;NetworkInterfaceId=eni-686ea200 &amp;Attribute=sourceDestCheck
-- &amp;AUTHPARAMS &lt;DescribeNetworkInterfaceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a20c6b2-d71c-45fb-bba7-37306850544b&lt;/requestId&gt;
-- &lt;networkInterfaceId&gt;eni-686ea200&lt;/networkInterfaceId&gt;
-- &lt;sourceDestCheck&gt; &lt;value&gt;true&lt;/value&gt;
-- &lt;/sourceDestCheck&gt;
-- &lt;/DescribeNetworkInterfaceAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeNetworkInterfaceAttribute'

describeNetworkInterfaceAttribute :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'dniaNetworkInterfaceId'
    -> State DescribeNetworkInterfaceAttribute a
    -> m DescribeNetworkInterfaceAttributeResponse
describeNetworkInterfaceAttribute p1 s =
    send $ (mkDescribeNetworkInterfaceAttribute p1) &~ s

describeNetworkInterfaceAttributeCatch :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadReader Env m
                                          )
    => Text -- ^ 'dniaNetworkInterfaceId'
    -> State DescribeNetworkInterfaceAttribute a
    -> m (Either ServiceErr DescribeNetworkInterfaceAttributeResponse)
describeNetworkInterfaceAttributeCatch p1 s =
    sendCatch $ (mkDescribeNetworkInterfaceAttribute p1) &~ s

-- $DescribeNetworkInterfaces
-- Describes one or more of your network interfaces. Example This example
-- describes all your network interfaces.
-- https://ec2.amazonaws.com/?Action=DescribeNetworkInterfaces &amp;AUTHPARAMS
-- &lt;DescribeNetworkInterfacesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;fc45294c-006b-457b-bab9-012f5b3b0e40&lt;/requestId&gt;
-- &lt;networkInterfaceSet&gt; &lt;item&gt;
-- &lt;networkInterfaceId&gt;eni-0f62d866&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-c53c87ac&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-cc3c87a5&lt;/vpcId&gt;
-- &lt;availabilityZone&gt;api-southeast-1b&lt;/availabilityZone&gt;
-- &lt;description/&gt; &lt;ownerId&gt;053230519467&lt;/ownerId&gt;
-- &lt;requesterManaged&gt;false&lt;/requesterManaged&gt;
-- &lt;status&gt;in-use&lt;/status&gt;
-- &lt;macAddress&gt;02:81:60:cb:27:37&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.0.146&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-3f4b5653&lt;/groupId&gt;
-- &lt;groupName&gt;default&lt;/groupName&gt; &lt;/item&gt; &lt;/groupSet&gt;
-- &lt;attachment&gt;
-- &lt;attachmentId&gt;eni-attach-6537fc0c&lt;/attachmentId&gt;
-- &lt;instanceId&gt;i-22197876&lt;/instanceId&gt;
-- &lt;instanceOwnerId&gt;053230519467&lt;/instanceOwnerId&gt;
-- &lt;deviceIndex&gt;0&lt;/deviceIndex&gt;
-- &lt;status&gt;attached&lt;/status&gt;
-- &lt;attachTime&gt;2012-07-01T21:45:27.000Z&lt;/attachTime&gt;
-- &lt;deleteOnTermination&gt;true&lt;/deleteOnTermination&gt;
-- &lt;/attachment&gt; &lt;tagSet/&gt; &lt;privateIpAddressesSet&gt;
-- &lt;item&gt; &lt;privateIpAddress&gt;10.0.0.146&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.0.148&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.0.150&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt;
-- &lt;/privateIpAddressesSet&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;networkInterfaceId&gt;eni-a66ed5cf&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-cd8a35a4&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-f28a359b&lt;/vpcId&gt;
-- &lt;availabilityZone&gt;ap-southeast-1b&lt;/availabilityZone&gt;
-- &lt;description&gt;Primary network interface&lt;/description&gt;
-- &lt;ownerId&gt;053230519467&lt;/ownerId&gt;
-- &lt;requesterManaged&gt;false&lt;/requesterManaged&gt;
-- &lt;status&gt;in-use&lt;/status&gt;
-- &lt;macAddress&gt;02:78:d7:00:8a:1e&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.1.233&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-a2a0b2ce&lt;/groupId&gt;
-- &lt;groupName&gt;quick-start-1&lt;/groupName&gt; &lt;/item&gt;
-- &lt;/groupSet&gt; &lt;attachment&gt;
-- &lt;attachmentId&gt;eni-attach-a99c57c0&lt;/attachmentId&gt;
-- &lt;instanceId&gt;i-886401dc&lt;/instanceId&gt;
-- &lt;instanceOwnerId&gt;053230519467&lt;/instanceOwnerId&gt;
-- &lt;deviceIndex&gt;0&lt;/deviceIndex&gt;
-- &lt;status&gt;attached&lt;/status&gt;
-- &lt;attachTime&gt;2012-06-27T20:08:44.000Z&lt;/attachTime&gt;
-- &lt;deleteOnTermination&gt;true&lt;/deleteOnTermination&gt;
-- &lt;/attachment&gt; &lt;tagSet/&gt; &lt;privateIpAddressesSet&gt;
-- &lt;item&gt; &lt;privateIpAddress&gt;10.0.1.233&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.1.20&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt;
-- &lt;/privateIpAddressesSet&gt; &lt;/item&gt; &lt;/networkInterfaceSet&gt;
-- &lt;/DescribeNetworkInterfacesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeNetworkInterfaces'

describeNetworkInterfaces :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => State DescribeNetworkInterfaces a
    -> m DescribeNetworkInterfacesResponse
describeNetworkInterfaces s =
    send (mkDescribeNetworkInterfaces &~ s)

describeNetworkInterfacesCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => State DescribeNetworkInterfaces a
    -> m (Either ServiceErr DescribeNetworkInterfacesResponse)
describeNetworkInterfacesCatch s =
    sendCatch (mkDescribeNetworkInterfaces &~ s)

-- $DescribePlacementGroups
-- Describes one or more of your placement groups. For more information about
-- placement groups and cluster instances, see Cluster Instances in the Amazon
-- Elastic Compute Cloud User Guide. Example This example describes the
-- placement group named XYZ-cluster.
-- https://ec2.amazonaws.com/?Action=DescribePlacementGroups
-- &amp;GroupName.1=XYZ-cluster &amp;AUTHPARAMS
-- d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE XYZ-cluster cluster available Example
-- This example filters the response to include only placement groups that
-- include the string Project in the name.
-- https://ec2.amazonaws.com/?Action=DescribePlacementGroups
-- &amp;Filter.1.Name=group-name &amp;Filter.1.Value=*Project* &amp;AUTHPARAMS
-- d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE Project-cluster cluster available.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribePlacementGroups'

describePlacementGroups :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => State DescribePlacementGroups a
    -> m DescribePlacementGroupsResponse
describePlacementGroups s =
    send (mkDescribePlacementGroups &~ s)

describePlacementGroupsCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => State DescribePlacementGroups a
    -> m (Either ServiceErr DescribePlacementGroupsResponse)
describePlacementGroupsCatch s =
    sendCatch (mkDescribePlacementGroups &~ s)

-- $DescribeRegions
-- Describes one or more regions that are currently available to you. For a
-- list of the regions supported by Amazon EC2, see Regions and Endpoints.
-- Example 1 This example displays information about all regions.
-- https://ec2.amazonaws.com/?Action=DescribeRegions &amp;AUTHPARAMS Example 2
-- This example displays information about the specified regions only.
-- https://ec2.amazonaws.com/?Action=DescribeRegions
-- &amp;RegionName.1=us-east-1 &amp;RegionName.2=eu-west-1 &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE us-east-1 ec2.us-east-1.amazonaws.com
-- eu-west-1 ec2.eu-west-1amazonaws.com.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeRegions'

describeRegions :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => State DescribeRegions a
    -> m DescribeRegionsResponse
describeRegions s =
    send (mkDescribeRegions &~ s)

describeRegionsCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => State DescribeRegions a
    -> m (Either ServiceErr DescribeRegionsResponse)
describeRegionsCatch s =
    sendCatch (mkDescribeRegions &~ s)

-- $DescribeReservedInstances
-- Describes one or more of the Reserved Instances that you purchased. For
-- more information about Reserved Instances, see Reserved Instances in the
-- Amazon Elastic Compute Cloud User Guide. Example This example describes
-- Reserved Instances owned by your account.
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstances &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ...
-- e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE m1.xlarge us-east-1b 31536000 61.0
-- 0.034 3 Linux/UNIX active default USD Light Utilization ... Example This
-- example filters the response to include only one-year, m1.small Linux/UNIX
-- Reserved Instances. If you want Linux/UNIX Reserved Instances specifically
-- for use with a VPC, set the product description to Linux/UNIX (Amazon VPC).
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstances
-- &amp;Filter.1.Name=duration &amp;Filter.1.Value.1=31536000
-- &amp;Filter.2.Name=instance-type &amp;Filter.2.Value.1=m1.small
-- &amp;Filter.3.Name=product-description &amp;Filter.3.Value.1=Linux%2FUNIX
-- &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeReservedInstances'

describeReservedInstances :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => State DescribeReservedInstances a
    -> m DescribeReservedInstancesResponse
describeReservedInstances s =
    send (mkDescribeReservedInstances &~ s)

describeReservedInstancesCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => State DescribeReservedInstances a
    -> m (Either ServiceErr DescribeReservedInstancesResponse)
describeReservedInstancesCatch s =
    sendCatch (mkDescribeReservedInstances &~ s)

-- $DescribeReservedInstancesListings
-- Describes your account's Reserved Instance listings in the Reserved
-- Instance Marketplace. For more information, see Reserved Instance
-- Marketplace in the Amazon Elastic Compute Cloud User Guide. Example This
-- example shows all the listings associated with your account.
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesListings
-- &amp;AUTHPARAMS cec5c904-8f3a-4de5-8f5a-ff7f9EXAMPLE
-- 253dfbf9-c335-4808-b956-d942cEXAMPLE e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- 2012-07-06T19:35:29.000Z 2012-07-06T19:35:30.000Z active ACTIVE Available
-- 20 Sold 0 Cancelled 0 Pending 0 8 480.0 USD false 7 420.0 USD false 6 360.0
-- USD active 5 300.0 USD false 4 240.0 USD false 3 180.0 USD false 2 120.0
-- USD false 1 60.0 USD false myclienttoken1.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesListings'

describeReservedInstancesListings :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     )
    => State DescribeReservedInstancesListings a
    -> m DescribeReservedInstancesListingsResponse
describeReservedInstancesListings s =
    send (mkDescribeReservedInstancesListings &~ s)

describeReservedInstancesListingsCatch :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadReader Env m
                                          )
    => State DescribeReservedInstancesListings a
    -> m (Either ServiceErr DescribeReservedInstancesListingsResponse)
describeReservedInstancesListingsCatch s =
    sendCatch (mkDescribeReservedInstancesListings &~ s)

-- $DescribeReservedInstancesModifications
-- Describes the modifications made to your Reserved Instances. If no
-- parameter is specified, information about all your Reserved Instances
-- modification requests is returned. If a modification ID is specified, only
-- information about the specific modification is returned. Example 1
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesModifications
-- &amp;AUTHPARAMS Example 2 This example filters the response to include only
-- Reserved Instances modification requests with status processing.
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesModifications
-- &amp;Filter.1.Name=status &amp;Filter.1.Value.1=processing &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesModifications'

describeReservedInstancesModifications :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadError AWS.Error m
                                          , MonadReader Env (ResumableSource m)
                                          )
    => State DescribeReservedInstancesModifications a
    -> ResumableSource m DescribeReservedInstancesModificationsResponse
describeReservedInstancesModifications s =
    paginate (mkDescribeReservedInstancesModifications &~ s)

describeReservedInstancesModificationsCatch :: ( MonadCatch m
                                               , MonadResource m
                                               , MonadReader Env (ResumableSource m)
                                               )
    => State DescribeReservedInstancesModifications a
    -> ResumableSource m (Either ServiceErr DescribeReservedInstancesModificationsResponse)
describeReservedInstancesModificationsCatch s =
    paginateCatch (mkDescribeReservedInstancesModifications &~ s)

-- $DescribeReservedInstancesOfferings
-- Describes Reserved Instance offerings that are available for purchase. With
-- Reserved Instances, you purchase the right to launch instances for a period
-- of time. During that time period, you do not receive insufficient capacity
-- errors, and you pay a lower usage rate than the rate charged for On-Demand
-- instances for the actual time used. For more information, see Reserved
-- Instance Marketplace in the Amazon Elastic Compute Cloud User Guide.
-- Example Describing Reserved Instance Marketplace Offerings Only This
-- example requests a list of Linux/Unix, Light Utilization Reserved Instances
-- that are available through the Reserved Instance Marketplace only. When
-- using the Query API, all strings must be URL-encoded.
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;Filter.0.Name=marketplace &amp;Filter.0.Value.1=true
-- &amp;IncludeMarketplace=true &amp;OfferingType=Light+Utilization
-- &amp;ProductDescription=Linux%2FUNIX &amp;Version=2013-10-01
-- &amp;AUTHPARAMS 2bc7dafa-dafd-4257-bdf9-c0814EXAMPLE
-- a6ce8269-7b8c-42cd-a7f5-0841cEXAMPLE m1.large us-east-1a 90720000 96.03
-- 0.027 Linux/UNIX default USD Light Utilization true 96.03 1
-- 2bc7dafa-dafd-4257-bdf9-c0814EXAMPLE m1.xlarge us-east-1b 28512000 61.0
-- 0.034 Linux/UNIX default USD Light Utilization Hourly 0.29 true 61.0 2
-- Example Describing AWS Offerings Only This example lists AWS offerings
-- only. https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;IncludeMarketplace=false &amp;AUTHPARAMS Example Using Tokens to
-- Manage Results You can use pagination support to query the results
-- sequentially and in parts. Specify the maximum number of results that are
-- returned in the response. Then, each paginated response contains a token
-- that can be provided as input to a subsequent
-- DescribeReservedInstancesOfferings call to fetch the next page. (Make sure
-- that you use URL encoding for the token value.)
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;MaxResults=5 &amp;AUTHPARAMS d072f652-cc57-458c-89e0-e6c02EXAMPLE ...
-- 649fd0c8-7846-46b8-8f84-a6400EXAMPLE m1.large us-east-1a 94608000 1200.0
-- 0.0 Linux/UNIX (Amazon VPC) default USD Heavy Utilization Hourly 0.052
-- false e5a2ff3b-a4f3-477c-8928-dbd00EXAMPLE m1.large us-east-1a 94608000
-- 1000.0 0.076 Linux/UNIX (Amazon VPC) default USD Medium Utilization false
-- ... h/C8YKPQBHEjW8xKz1827/Zzyb0VqsqkjRo3TqhFYeE=
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;MaxResults=5
-- &amp;NextToken=h%2FC8YKPQBHEjW8xKz1827%2FZzyb0VqsqkjRo3TqhFYeE%3D
-- &amp;AUTHPARAMS Example Using Filters This example filters the response to
-- include only one-year, m1.small or m1.large Linux/UNIX Reserved Instances.
-- If you want Linux/UNIX Reserved Instances specifically for use with a VPC,
-- set the product description to Linux/UNIX (Amazon VPC).
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;Filter.1.Name=duration &amp;Filter.1.Value.1=31536000
-- &amp;Filter.2.Name=instance-type &amp;Filter.2.Value.1=m1.small
-- &amp;Filter.2.Value.2=m1.large &amp;Filter.3.Name=product-description
-- &amp;Filter.3.Value.1=Linux%2FUNIX &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesOfferings'

describeReservedInstancesOfferings :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadError AWS.Error m
                                      , MonadReader Env (ResumableSource m)
                                      )
    => State DescribeReservedInstancesOfferings a
    -> ResumableSource m DescribeReservedInstancesOfferingsResponse
describeReservedInstancesOfferings s =
    paginate (mkDescribeReservedInstancesOfferings &~ s)

describeReservedInstancesOfferingsCatch :: ( MonadCatch m
                                           , MonadResource m
                                           , MonadReader Env (ResumableSource m)
                                           )
    => State DescribeReservedInstancesOfferings a
    -> ResumableSource m (Either ServiceErr DescribeReservedInstancesOfferingsResponse)
describeReservedInstancesOfferingsCatch s =
    paginateCatch (mkDescribeReservedInstancesOfferings &~ s)

-- $DescribeRouteTables
-- Describes one or more of your route tables. For more information about
-- route tables, see Route Tables in the Amazon Virtual Private Cloud User
-- Guide. Example This example describes all your route tables. The first
-- route table in the returned list is the VPC's main route table. Its
-- association ID represents the association between the table and the VPC.
-- https://ec2.amazonaws.com/?Action=DescribeRouteTables &amp;AUTHPARAMS
-- 6f570b0b-9c18-4b07-bdec-73740dcf861a rtb-13ad487a vpc-11ad4878 10.0.0.0/22
-- local active CreateRouteTable rtbassoc-12ad487b rtb-13ad487a true
-- rtb-f9ad4890 vpc-11ad4878 10.0.0.0/22 local active CreateRouteTable
-- 0.0.0.0/0 igw-eaad4883 active rtbassoc-faad4893 rtb-f9ad4890
-- subnet-15ad487c.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeRouteTables'

describeRouteTables :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => State DescribeRouteTables a
    -> m DescribeRouteTablesResponse
describeRouteTables s =
    send (mkDescribeRouteTables &~ s)

describeRouteTablesCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => State DescribeRouteTables a
    -> m (Either ServiceErr DescribeRouteTablesResponse)
describeRouteTablesCatch s =
    sendCatch (mkDescribeRouteTables &~ s)

-- $DescribeSecurityGroups
-- Describes one or more of your security groups. A security group is for use
-- with instances either in the EC2-Classic platform or in a specific VPC. For
-- more information, see Amazon EC2 Security Groups in the Amazon Elastic
-- Compute Cloud User Guide and Security Groups for Your VPC in the Amazon
-- Virtual Private Cloud User Guide. Example 1 This example returns
-- information about two security groups that are configured for the account.
-- https://ec2.amazonaws.com/?Action=DescribeSecurityGroups
-- &amp;GroupName.1=WebServers &amp;GroupName.2=RangedPortsBySource
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE 123456789012
-- sg-1a2b3c4d WebServers Web Servers tcp 80 80 0.0.0.0/0 123456789012
-- sg-2a2b3c4d RangedPortsBySource Group A tcp 6000 7000 123456789012
-- sg-3a2b3c4d Group B Example 2 This example describes all security groups
-- that grant access over TCP specifically on port 22 from instances
-- associated with app_server_group or database_group.
-- https://ec2.amazonaws.com/?Action=DescribeSecurityGroups
-- &amp;Filter.1.Name=ip-permission.protocol &amp;Filter.1.Value.1=tcp
-- &amp;Filter.2.Name=ip-permission.from-port &amp;Filter.2.Value.1=22
-- &amp;Filter.3.Name=ip-permission.to-port &amp;Filter.3.Value.1=22
-- &amp;Filter.4.Name=ip-permission.group-name
-- &amp;Filter.4.Value.1=app_server_group &amp;Filter.4.Value.2=database_group
-- &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeSecurityGroups'

describeSecurityGroups :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => State DescribeSecurityGroups a
    -> m DescribeSecurityGroupsResponse
describeSecurityGroups s =
    send (mkDescribeSecurityGroups &~ s)

describeSecurityGroupsCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => State DescribeSecurityGroups a
    -> m (Either ServiceErr DescribeSecurityGroupsResponse)
describeSecurityGroupsCatch s =
    sendCatch (mkDescribeSecurityGroups &~ s)

-- $DescribeSnapshotAttribute
-- Describes the specified attribute of the specified snapshot. You can
-- specify only one attribute at a time. For more information about Amazon EBS
-- snapshots, see Amazon EBS Snapshots in the Amazon Elastic Compute Cloud
-- User Guide. Example This example describes permissions for a snapshot with
-- the ID of snap-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DescribeSnapshotAttribute
-- &amp;SnapshotId=snap-1a2b3c4d &amp;Attribute=createVolumePermission
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE snap-1a2b3c4d all.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeSnapshotAttribute'

describeSnapshotAttribute :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dsaSnapshotId'
    -> SnapshotAttributeName -- ^ 'dsaAttribute'
    -> State DescribeSnapshotAttribute a
    -> m DescribeSnapshotAttributeResponse
describeSnapshotAttribute p1 p2 s =
    send $ (mkDescribeSnapshotAttribute p1 p2) &~ s

describeSnapshotAttributeCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'dsaSnapshotId'
    -> SnapshotAttributeName -- ^ 'dsaAttribute'
    -> State DescribeSnapshotAttribute a
    -> m (Either ServiceErr DescribeSnapshotAttributeResponse)
describeSnapshotAttributeCatch p1 p2 s =
    sendCatch $ (mkDescribeSnapshotAttribute p1 p2) &~ s

-- $DescribeSnapshots
-- Describes one or more of the Amazon EBS snapshots available to you.
-- Available snapshots include public snapshots available for any AWS account
-- to launch, private snapshots that you own, and private snapshots owned by
-- another AWS account but for which you've been given explicit create volume
-- permissions. The create volume permissions fall into the following
-- categories: public: The owner of the snapshot granted create volume
-- permissions for the snapshot to the all group. All AWS accounts have create
-- volume permissions for these snapshots. explicit: The owner of the snapshot
-- granted create volume permissions to a specific AWS account. implicit: An
-- AWS account has implicit create volume permissions for all snapshots it
-- owns. The list of snapshots returned can be modified by specifying snapshot
-- IDs, snapshot owners, or AWS accounts with create volume permissions. If no
-- options are specified, Amazon EC2 returns all snapshots for which you have
-- create volume permissions. If you specify one or more snapshot IDs, only
-- snapshots that have the specified IDs are returned. If you specify an
-- invalid snapshot ID, an error is returned. If you specify a snapshot ID for
-- which you do not have access, it is not included in the returned results.
-- If you specify one or more snapshot owners, only snapshots from the
-- specified owners and for which you have access are returned. The results
-- can include the AWS account IDs of the specified owners, amazon for
-- snapshots owned by Amazon, or self for snapshots that you own. If you
-- specify a list of restorable users, only snapshots with create snapshot
-- permissions for those users are returned. You can specify AWS account IDs
-- (if you own the snapshots), self for snapshots for which you own or have
-- explicit permissions, or all for public snapshots. For more information
-- about Amazon EBS snapshots, see Amazon EBS Snapshots in the Amazon Elastic
-- Compute Cloud User Guide. Example This example describes a snapshot with an
-- ID of snap-1a2b3c4d. https://ec2.amazonaws.com/?Action=DescribeSnapshots
-- &amp;SnapshotId=snap-1a2b3c4d &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE snap-1a2b3c4d vol-1a2b3c4d pending
-- YYYY-MM-DDTHH:MM:SS.SSSZ 80% 111122223333 15 Daily Backup true Example This
-- example filters the response to include only snapshots with the pending
-- status, and that are also tagged with a value that includes the string db_.
-- https://ec2.amazonaws.com/?Action=DescribeSnapshots
-- &amp;Filter.1.Name=status &amp;Filter.1.Value.1=pending
-- &amp;Filter.2.Name=tag-value &amp;Filter.2.Value.1=*db_* &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE snap-1a2b3c4d vol-1a2b3c4d pending
-- YYYY-MM-DDTHH:MM:SS.SSSZ 30% 111122223333 15 Daily Backup Purpose
-- demo_db_14_backup true.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeSnapshots'

describeSnapshots :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => State DescribeSnapshots a
    -> m DescribeSnapshotsResponse
describeSnapshots s =
    send (mkDescribeSnapshots &~ s)

describeSnapshotsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => State DescribeSnapshots a
    -> m (Either ServiceErr DescribeSnapshotsResponse)
describeSnapshotsCatch s =
    sendCatch (mkDescribeSnapshots &~ s)

-- $DescribeSpotDatafeedSubscription
-- Describes the datafeed for Spot Instances. For more information, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example describes the datafeed for the account.
-- https://ec2.amazonaws.com/?Action=DescribeSpotDatafeedSubscription
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE 123456789012
-- my-s3-bucket spotdata_ Active.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeSpotDatafeedSubscription'

describeSpotDatafeedSubscription :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadError AWS.Error m
                                    , MonadReader Env m
                                    )
    => State DescribeSpotDatafeedSubscription a
    -> m DescribeSpotDatafeedSubscriptionResponse
describeSpotDatafeedSubscription s =
    send (mkDescribeSpotDatafeedSubscription &~ s)

describeSpotDatafeedSubscriptionCatch :: ( MonadCatch m
                                         , MonadResource m
                                         , MonadReader Env m
                                         )
    => State DescribeSpotDatafeedSubscription a
    -> m (Either ServiceErr DescribeSpotDatafeedSubscriptionResponse)
describeSpotDatafeedSubscriptionCatch s =
    sendCatch (mkDescribeSpotDatafeedSubscription &~ s)

-- $DescribeSpotInstanceRequests
-- Describes the Spot Instance requests that belong to your account. Spot
-- Instances are instances that Amazon EC2 starts on your behalf when the
-- maximum price that you specify exceeds the current Spot Price. Amazon EC2
-- periodically sets the Spot Price based on available Spot Instance capacity
-- and current Spot Instance requests. For more information about Spot
-- Instances, see Spot Instances in the Amazon Elastic Compute Cloud User
-- Guide. You can use DescribeSpotInstanceRequests to find a running Spot
-- Instance by examining the response. If the status of the Spot Instance is
-- fulfilled, the instance ID appears in the response and contains the
-- identifier of the instance. Alternatively, you can use DescribeInstances
-- with a filter to look for instances where the instance lifecycle is spot.
-- Example for DescribeSpotInstanceRequests This example returns information
-- about current Spot Instance requests.
-- https://ec2.amazonaws.com/?Action=DescribeSpotInstanceRequests
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE sir-1a2b3c4d 0.09
-- one-time active fulfilled YYYY-MM-DDTHH:MM:SS.000Z Your Spot request is
-- fulfilled. ami-1a2b3c4d my-key-pair sg-1a2b3c4d websrv m1.small false false
-- i-1a2b3c4d YYYY-MM-DDTHH:MM:SS.000Z Linux/UNIX us-east-1a Example for
-- DescribeInstances Alternatively, you can use DescribeInstances as follows.
-- https://ec2.amazonaws.com/?Action=DescribeInstances
-- &amp;Filter.1.Name=instance-lifecycle &amp;Filter.1.Value.1=spot
-- &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeSpotInstanceRequests'

describeSpotInstanceRequests :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => State DescribeSpotInstanceRequests a
    -> m DescribeSpotInstanceRequestsResponse
describeSpotInstanceRequests s =
    send (mkDescribeSpotInstanceRequests &~ s)

describeSpotInstanceRequestsCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => State DescribeSpotInstanceRequests a
    -> m (Either ServiceErr DescribeSpotInstanceRequestsResponse)
describeSpotInstanceRequestsCatch s =
    sendCatch (mkDescribeSpotInstanceRequests &~ s)

-- $DescribeSpotPriceHistory
-- Describes the Spot Price history. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current Spot Instance requests. For
-- more information about Spot Instances, see Spot Instances in the Amazon
-- Elastic Compute Cloud User Guide. When you specify an Availability Zone,
-- this operation describes the price history for the specified Availability
-- Zone with the most recent set of prices listed first. If you don't specify
-- an Availability Zone, you get the prices across all Availability Zones,
-- starting with the most recent set. However, if you're using an API version
-- earlier than 2011-05-15, you get the lowest price across the region for the
-- specified time period. The prices returned are listed in chronological
-- order, from the oldest to the most recent. Example This example gets Spot
-- Price history for a particular day in December 2009 for the specified
-- Availability Zone.
-- https://ec2.amazonaws.com/?Action=DescribeSpotPriceHistory
-- &amp;StartTime=2009-12-04T00:00:00.000Z
-- &amp;EndTime=2009-12-04T23:59:59.000Z &amp;AvailabilityZone=us-east-1a
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE m1.small Linux/UNIX
-- 0.287 2009-12-04T20:56:05.000Z us-east-1a m1.small Windows 0.033
-- 2009-12-04T22:33:47.000Z us-east-1a Example with Filters This example uses
-- filters to get the same results as the previous example.
-- https://ec2.amazonaws.com/?Action=DescribeSpotPriceHistory
-- &amp;Filter.1.Name=timestamp &amp;Filter.1.Value.1=2009-12-04*
-- &amp;Filter.2.Name=availability-zone &amp;Filter.2.Value.1=us-east-1a
-- &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeSpotPriceHistory'

describeSpotPriceHistory :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env (ResumableSource m)
                            )
    => State DescribeSpotPriceHistory a
    -> ResumableSource m DescribeSpotPriceHistoryResponse
describeSpotPriceHistory s =
    paginate (mkDescribeSpotPriceHistory &~ s)

describeSpotPriceHistoryCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env (ResumableSource m)
                                 )
    => State DescribeSpotPriceHistory a
    -> ResumableSource m (Either ServiceErr DescribeSpotPriceHistoryResponse)
describeSpotPriceHistoryCatch s =
    paginateCatch (mkDescribeSpotPriceHistory &~ s)

-- $DescribeSubnets
-- Describes one or more of your subnets. For more information about subnets,
-- see Your VPC and Subnets in the Amazon Virtual Private Cloud User Guide.
-- Example 1 This example describes the subnets with the IDs subnet-9d4a7b6c
-- and subnet-6e7f829e. https://ec2.amazonaws.com/?Action=DescribeSubnets
-- &amp;SubnetId.1=subnet-9d4a7b6c &amp;SubnetId.2=subnet-6e7f829e
-- &amp;AUTHPARAMS &lt;DescribeSubnetsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;subnetSet&gt; &lt;item&gt;
-- &lt;subnetId&gt;subnet-9d4a7b6c&lt;/subnetId&gt;
-- &lt;state&gt;available&lt;/state&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.1.0/24&lt;/cidrBlock&gt;
-- &lt;availableIpAddressCount&gt;251&lt;/availableIpAddressCount&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;defaultForAz&gt;false&lt;/defaultForAz&gt;
-- &lt;mapPublicIpOnLaunch&gt;false&lt;/mapPublicIpOnLaunch&gt;
-- &lt;tagSet/&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;subnetId&gt;subnet-6e7f829e&lt;/subnetId&gt;
-- &lt;state&gt;available&lt;/state&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&gt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.0.0/24&lt;/cidrBlock&gt;
-- &lt;availableIpAddressCount&gt;251&lt;/availableIpAddressCount&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;defaultForAz&gt;false&lt;/defaultForAz&gt;
-- &lt;mapPublicIpOnLaunch&gt;false&lt;/mapPublicIpOnLaunch&gt;
-- &lt;tagSet/&gt; &lt;/item&gt; &lt;subnetSet/&gt;
-- &lt;/DescribeSubnetsResponse&gt; Example 2 This example uses filters to
-- describe any subnet you own that is in the VPC with the ID vpc-1a2b3c4d or
-- vpc-6e7f8a92, and whose state is available.
-- https://ec2.amazonaws.com/?Action=DescribeSubnets &amp;Filter.1.Name=vpc-id
-- &amp;Filter.1.Value.1=vpc-1a2b3c4d &amp;Filter.1.Value.2=vpc-6e7f8a92
-- &amp;Filter.2.Name=state &amp;Filter.2.Value.1=available &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeSubnets'

describeSubnets :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => State DescribeSubnets a
    -> m DescribeSubnetsResponse
describeSubnets s =
    send (mkDescribeSubnets &~ s)

describeSubnetsCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => State DescribeSubnets a
    -> m (Either ServiceErr DescribeSubnetsResponse)
describeSubnetsCatch s =
    sendCatch (mkDescribeSubnets &~ s)

-- $DescribeTags
-- Describes one or more of the tags for your EC2 resources. For more
-- information about tags, see Tagging Your Resources in the Amazon Elastic
-- Compute Cloud User Guide. Example This example describes all the tags in
-- your account. https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE ami-1a2b3c4d image
-- webserver ami-1a2b3c4d image stack Production i-5f4e3d2a instance webserver
-- i-5f4e3d2a instance stack Production i-12345678 instance database_server
-- i-12345678 instance stack Test Example This example describes only the tags
-- for the AMI with ID ami-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;Filter.1.Name=resource-id &amp;Filter.1.Value.1=ami-1a2b3c4d
-- &amp;AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE ami-1a2b3c4d image
-- webserver ami-1a2b3c4d image stack Production Example This example
-- describes the tags for all your instances.
-- https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;Filter.1.Name=resource-type &amp;Filter.1.Value.1=instance
-- &amp;AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE i-5f4e3d2a instance
-- webserver i-5f4e3d2a instance stack Production i-12345678 instance
-- database_server i-12345678 instance stack Test Example This example
-- describes the tags for all your instances tagged with the key webserver.
-- Note that you can use wildcards with filters, so you could specify the
-- value as ?ebserver to find tags with the key webserver or Webserver.
-- https://ec2.amazonaws.com/?Action=DescribeTags &amp;Filter.1.Name=key
-- &amp;Filter.1.Value.1=webserver &amp;AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE i-5f4e3d2a instance webserver Example
-- This example describes the tags for all your instances tagged with either
-- stack=Test or stack=Production.
-- https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;Filter.1.Name=resource-type &amp;Filter.1.Value.1=instance
-- &amp;Filter.2.Name=key &amp;Filter.2.Value.1=stack &amp;Filter.3.Name=value
-- &amp;Filter.3.Value.1=Test &amp;Filter.3.Value.2=Production &amp;AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE i-5f4e3d2a instance stack Production
-- i-12345678 instance stack Test Example This example describes the tags for
-- all your instances tagged with Purpose=[empty string].
-- https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;Filter.1.Name=resource-type &amp;Filter.1.Value.1=instance
-- &amp;Filter.2.Name=key &amp;Filter.2.Value.1=Purpose
-- &amp;Filter.3.Name=value &amp;Filter.3.Value.1= &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeTags'

describeTags :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env (ResumableSource m)
                )
    => State DescribeTags a
    -> ResumableSource m DescribeTagsResponse
describeTags s =
    paginate (mkDescribeTags &~ s)

describeTagsCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env (ResumableSource m)
                     )
    => State DescribeTags a
    -> ResumableSource m (Either ServiceErr DescribeTagsResponse)
describeTagsCatch s =
    paginateCatch (mkDescribeTags &~ s)

-- $DescribeVolumeAttribute
-- Describes the specified attribute of the specified volume. You can specify
-- only one attribute at a time. For more information about Amazon EBS
-- volumes, see Amazon EBS Volumes in the Amazon Elastic Compute Cloud User
-- Guide. Example This example describes the autoEnableIO attribute of the
-- volume vol-12345678.
-- https://ec2.amazonaws.com/?Action=DescribeVolumeAttribute
-- &amp;Attribute=autoEnableIO &amp;VolumeId=vol-12345678 &amp;AUTHPARAMS
-- &lt;DescribeVolumeAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;5jkdf074-37ed-4004-8671-a78ee82bf1cbEXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-12345678&lt;/volumeId&gt; &lt;autoEnableIO&gt;
-- &lt;value&gt;false&lt;/value&gt; &lt;/autoEnableIO&gt;
-- &lt;/DescribeVolumeAttributeResponse&gt; Example This example describes the
-- productCodes attribute of the volume vol-12345678.
-- https://ec2.amazonaws.com/?Action=DescribeVolumeAttribute
-- &amp;Attribute=productCodes &amp;VolumeId=vol-12345678 &amp;AUTHPARAMS
-- &lt;DescribeVolumeAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;5jkdf074-37ed-4004-8671-a78ee82bf1cbEXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-12345678&lt;/volumeId&gt; &lt;productCodes&gt;
-- &lt;item&gt;
-- &lt;productCode&gt;a1b2c3d4e5f6g7h8i9j10k11&lt;/productCode&gt;
-- &lt;type&gt;marketplace&lt;/type&gt; &lt;/item&gt; &lt;/productCodes&gt;
-- &lt;/DescribeVolumeAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeVolumeAttribute'

describeVolumeAttribute :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'dvaVolumeId'
    -> State DescribeVolumeAttribute a
    -> m DescribeVolumeAttributeResponse
describeVolumeAttribute p1 s =
    send $ (mkDescribeVolumeAttribute p1) &~ s

describeVolumeAttributeCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'dvaVolumeId'
    -> State DescribeVolumeAttribute a
    -> m (Either ServiceErr DescribeVolumeAttributeResponse)
describeVolumeAttributeCatch p1 s =
    sendCatch $ (mkDescribeVolumeAttribute p1) &~ s

-- $DescribeVolumeStatus
-- Describes the status of the specified volumes. Volume status provides the
-- result of the checks performed on your volumes to determine events that can
-- impair the performance of your volumes. The performance of a volume can be
-- affected if an issue occurs on the volume's underlying host. If the
-- volume's underlying host experiences a power outage or system issue, after
-- the system is restored, there could be data inconsistencies on the volume.
-- Volume events notify you if this occurs. Volume actions notify you if any
-- action needs to be taken in response to the event. The DescribeVolumeStatus
-- operation provides the following information about the specified volumes:
-- Status: Reflects the current status of the volume. The possible values are
-- ok, impaired , warning, or insufficient-data. If all checks pass, the
-- overall status of the volume is ok. If the check fails, the overall status
-- is impaired. If the status is insufficient-data, then the checks may still
-- be taking place on your volume at the time. We recommend that you retry the
-- request. For more information on volume status, see Monitoring the Status
-- of Your Volumes. Events: Reflect the cause of a volume status and may
-- require you to take action. For example, if your volume returns an impaired
-- status, then the volume event might be potential-data-inconsistency. This
-- means that your volume has been affected by an issue with the underlying
-- host, has all I/O operations disabled, and may have inconsistent data.
-- Actions: Reflect the actions you may have to take in response to an event.
-- For example, if the status of the volume is impaired and the volume event
-- shows potential-data-inconsistency, then the action shows enable-volume-io.
-- This means that you may want to enable the I/O operations for the volume by
-- calling the EnableVolumeIO action and then check the volume for data
-- consistency. Volume status is based on the volume status checks, and does
-- not reflect the volume state. Therefore, volume status does not indicate
-- volumes in the error state (for example, when a volume is incapable of
-- accepting I/O.) Example This example describes the status of all the
-- volumes associated with your account.
-- https://ec2.amazonaws.com/?Action=DescribeVolumeStatus &amp;AUTHPARAMS
-- 5jkdf074-37ed-4004-8671-a78ee82bf1cbEXAMPLE vol-11111111 us-east-1d ok
-- io-enabled passed vol-22222222 us-east-1d impaired io-enabled failed
-- evol-61a54008 potential-data-inconsistency THIS IS AN EXAMPLE
-- 2011-12-01T14:00:00.000Z 2011-12-01T15:00:00.000Z enable-volume-io
-- evol-61a54008 potential-data-inconsistency THIS IS AN EXAMPLE Example This
-- example describes all the volumes in the us-east-1d Availability Zone with
-- failed io-enabled status.
-- https://ec2.amazonaws.com/?Action=DescribeVolumeStatus
-- &amp;Filter.1.Name=availability-zone &amp;Filter.1.Value.1=us-east-1d
-- &amp;Filter.2.Name=volume-status.details-name
-- &amp;Filter.2.Value.1=io-enabled
-- &amp;Filter.3.Name=volume-status.details-status
-- &amp;Filter.3.Value.1=failed &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeVolumeStatus'

describeVolumeStatus :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env (ResumableSource m)
                        )
    => State DescribeVolumeStatus a
    -> ResumableSource m DescribeVolumeStatusResponse
describeVolumeStatus s =
    paginate (mkDescribeVolumeStatus &~ s)

describeVolumeStatusCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env (ResumableSource m)
                             )
    => State DescribeVolumeStatus a
    -> ResumableSource m (Either ServiceErr DescribeVolumeStatusResponse)
describeVolumeStatusCatch s =
    paginateCatch (mkDescribeVolumeStatus &~ s)

-- $DescribeVolumes
-- Describes the specified Amazon EBS volumes. For more information about
-- Amazon EBS volumes, see Amazon EBS Volumes in the Amazon Elastic Compute
-- Cloud User Guide. Example This example describes all volumes associated
-- with your account. https://ec2.amazonaws.com/?Action=DescribeVolumes
-- &amp;AUTHPARAMS &lt;DescribeVolumesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;volumeSet&gt; &lt;item&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt; &lt;size&gt;80&lt;/size&gt;
-- &lt;snapshotId/&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;status&gt;in-use&lt;/status&gt;
-- &lt;createTime&gt;YYYY-MM-DDTHH:MM:SS.SSSZ&lt;/createTime&gt;
-- &lt;attachmentSet&gt; &lt;item&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;instanceId&gt;i-1a2b3c4d&lt;/instanceId&gt;
-- &lt;device&gt;/dev/sdh&lt;/device&gt; &lt;status&gt;attached&lt;/status&gt;
-- &lt;attachTime&gt;YYYY-MM-DDTHH:MM:SS.SSSZ&lt;/attachTime&gt;
-- &lt;deleteOnTermination&gt;false&lt;/deleteOnTermination&gt; &lt;/item&gt;
-- &lt;/attachmentSet&gt; &lt;volumeType&gt;standard&lt;/volumeType&gt;
-- &lt;encrypted&gt;true&lt;/encrypted&gt; &lt;/item&gt; &lt;/volumeSet&gt;
-- &lt;/DescribeVolumesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeVolumes'

describeVolumes :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => State DescribeVolumes a
    -> m DescribeVolumesResponse
describeVolumes s =
    send (mkDescribeVolumes &~ s)

describeVolumesCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => State DescribeVolumes a
    -> m (Either ServiceErr DescribeVolumesResponse)
describeVolumesCatch s =
    sendCatch (mkDescribeVolumes &~ s)

-- $DescribeVpcAttribute
-- Describes the specified attribute of the specified VPC. You can specify
-- only one attribute at a time. Example 1 This example describes the
-- enableDnsSupport attribute of the specified VPC. The sample response
-- indicates that DNS resolution is supported.
-- https://ec2.amazonaws.com/?Action=DescribeVpcAttribute
-- &amp;VpcId=vpc-1a2b3c4d &amp;Attribute=enableDnsSupport &amp;AUTHPARAMS
-- &lt;DescribeVpcAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt; &lt;enableDnsSupport&gt;
-- &lt;value&gt;true&lt;/value&gt; &lt;/enableDnsSupport&gt;
-- &lt;/DescribeVpcAttributeResponse&gt; Example 2 This request describes the
-- enableDnsHostnames attribute of the specified VPC. The sample response
-- indicates that DNS hostnames are supported.
-- https://ec2.amazonaws.com/?Action=DescribeVpcAttribute
-- &amp;VpcId=vpc-1a2b3c4d &amp;Attribute=enableDnsHostnames &amp;AUTHPARAMS
-- &lt;DescribeVpcAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt; &lt;enableDnsHostnames&gt;
-- &lt;value&gt;true&lt;/value&gt; &lt;/enableDnsHostnames&gt;
-- &lt;/DescribeVpcAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeVpcAttribute'

describeVpcAttribute :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'dva1VpcId'
    -> State DescribeVpcAttribute a
    -> m DescribeVpcAttributeResponse
describeVpcAttribute p1 s =
    send $ (mkDescribeVpcAttribute p1) &~ s

describeVpcAttributeCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dva1VpcId'
    -> State DescribeVpcAttribute a
    -> m (Either ServiceErr DescribeVpcAttributeResponse)
describeVpcAttributeCatch p1 s =
    sendCatch $ (mkDescribeVpcAttribute p1) &~ s

-- $DescribeVpcPeeringConnections
-- Describes one or more of your VPC peering connections. Example 1 This
-- example describes all of your VPC peering connections.
-- https://ec2.amazonaws.com/?Action=DescribeVpcPeeringConnections
-- &amp;AUTHPARAMS &lt;DescribeVpcPeeringConnectionsResponse
-- xmlns=http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcPeeringConnectionSet&gt; &lt;item&gt;
-- &lt;vpcPeeringConnectionId&gt;pcx-111aaa22&lt;/vpcPeeringConnectionId&gt;
-- &lt;requesterVpcInfo&gt; &lt;ownerId&gt;777788889999&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;172.31.0.0/16&lt;/cidrBlock&gt; &lt;/requesterVpcInfo&gt;
-- &lt;accepterVpcInfo&gt; &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-aa22cc33&lt;/vpcId&gt; &lt;/accepterVpcInfo&gt;"
-- &lt;status&gt; &lt;code&gt;pending-acceptance&lt;/code&gt;
-- &lt;message&gt;Pending Acceptance by 123456789012&lt;/message&gt;
-- &lt;/status&gt;
-- &lt;expirationTime&gt;2014-02-17T16:00:50.000Z&lt;/expirationTime&gt;
-- &lt;tagSet/&gt; &lt;/item&gt; &lt;/vpcPeeringConnectionSet&gt;
-- &lt;/DescribeVpcPeeringConnectionsResponse&gt; Example 2 This example
-- describes all of your VPC peering connections that are in the
-- pending-acceptance state.
-- https://ec2.amazonaws.com/?Action=DescribeVpcPeeringConnections
-- &amp;Filter.1.Name=status-code &amp;Filter.1.Value=pending-acceptance
-- &amp;AUTHPARAMS Example 3 This example describes all of your VPC peering
-- connections that have the tag Name=Finance or Name=Accounts.
-- https://ec2.amazonaws.com/?Action=DescribeVpcPeeringConnections
-- &amp;Filter.1.Name=tag:Name &amp;Filter.1.Value.1=Finance
-- &amp;Filter.1.Value.2=Accounts &amp;AUTHPARAMS Example 4 This example
-- describes all of the VPC peering connections for your specified VPC,
-- vpc-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DescribeVpcPeeringConnections
-- &amp;Filter.1.Name=requester-vpc-info.vpc-id
-- &amp;Filter.1.Value=vpc-1a2b3c4d &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeVpcPeeringConnections'

describeVpcPeeringConnections :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 )
    => State DescribeVpcPeeringConnections a
    -> m DescribeVpcPeeringConnectionsResponse
describeVpcPeeringConnections s =
    send (mkDescribeVpcPeeringConnections &~ s)

describeVpcPeeringConnectionsCatch :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadReader Env m
                                      )
    => State DescribeVpcPeeringConnections a
    -> m (Either ServiceErr DescribeVpcPeeringConnectionsResponse)
describeVpcPeeringConnectionsCatch s =
    sendCatch (mkDescribeVpcPeeringConnections &~ s)

-- $DescribeVpcs
-- Describes one or more of your VPCs. Example 1 This example describes the
-- specified VPC. https://ec2.amazonaws.com/?Action=DescribeVpcs
-- &amp;VpcId.1=vpc-1a2b3c4d &amp;AUTHPARAMS &lt;DescribeVpcsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcSet&gt; &lt;item&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;state&gt;available&lt;/state&gt;
-- &lt;cidrBlock&gt;10.0.0.0/23&lt;/cidrBlock&gt;
-- &lt;dhcpOptionsId&gt;dopt-7a8b9c2d&lt;/dhcpOptionsId&gt;
-- &lt;instanceTenancy&gt;default&lt;/instanceTenancy&gt;
-- &lt;isDefault&gt;false&lt;/isDefault&gt; &lt;tagSet/&gt; &lt;/item&gt;
-- &lt;/vpcSet&gt; &lt;/DescribeVpcsResponse&gt; Example 2 This example uses
-- filters to describe any VPC you own that uses the set of DHCP options with
-- the ID dopt-7a8b9c2d or dopt-2b2a3d3c and whose state is available.
-- https://ec2.amazonaws.com/?Action=DescribeVpcs
-- &amp;Filter.1.Name=dhcp-options-id &amp;Filter.1.Value.1=dopt-7a8b9c2d
-- &amp;Filter.1.Value.2=dopt-2b2a3d3c &amp;Filter.2.Name=state
-- &amp;Filter.2.Value.1=available &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeVpcs'

describeVpcs :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => State DescribeVpcs a
    -> m DescribeVpcsResponse
describeVpcs s =
    send (mkDescribeVpcs &~ s)

describeVpcsCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => State DescribeVpcs a
    -> m (Either ServiceErr DescribeVpcsResponse)
describeVpcsCatch s =
    sendCatch (mkDescribeVpcs &~ s)

-- $DescribeVpnConnections
-- Describes one or more of your VPN connections. For more information about
-- VPN connections, see Adding a Hardware Virtual Private Gateway to Your VPC
-- in the Amazon Virtual Private Cloud User Guide. Example 1 This example
-- describes the specified VPN connection. The response includes the customer
-- gateway configuration information. Because it's a long set of information,
-- we haven't displayed it here. To see an example of the configuration
-- information, see the Amazon Virtual Private Cloud Network Administrator
-- Guide. https://ec2.amazonaws.com/?Action=DescribeVpnConnections
-- &amp;VpnConnectionId.1=vpn-44a8938f &amp;AUTHPARAMS
-- &lt;DescribeVpnConnectionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnConnectionSet&gt; &lt;item&gt;
-- &lt;vpnConnectionId&gt;vpn-44a8938f&lt;/vpnConnectionId&gt;
-- &lt;state&gt;available&lt;/state&gt; &lt;customerGatewayConfiguration&gt;
-- ...Customer gateway configuration data in escaped XML format...
-- &lt;/customerGatewayConfiguration&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt; &lt;tagSet/&gt;
-- &lt;/item&gt; &lt;/vpnConnectionSet&gt;
-- &lt;/DescribeVpnConnectionsResponse&gt; Example 2 This example describes
-- any VPN connection you own that is associated with the customer gateway
-- with ID cgw-b4dc3961, and whose state is either pending or available.
-- https://ec2.amazonaws.com/?Action=DescribeVpnConnections
-- &amp;Filter.1.Name=customer-gateway-id &amp;Filter.1.Value.1=cgw-b4dc3961
-- &amp;Filter.2.Name=state &amp;Filter.2.Value.1=pending
-- &amp;Filter.2.Value.2=available &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeVpnConnections'

describeVpnConnections :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => State DescribeVpnConnections a
    -> m DescribeVpnConnectionsResponse
describeVpnConnections s =
    send (mkDescribeVpnConnections &~ s)

describeVpnConnectionsCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => State DescribeVpnConnections a
    -> m (Either ServiceErr DescribeVpnConnectionsResponse)
describeVpnConnectionsCatch s =
    sendCatch (mkDescribeVpnConnections &~ s)

-- $DescribeVpnGateways
-- Describes one or more of your virtual private gateways. For more
-- information about virtual private gateways, see Adding an IPsec Hardware
-- VPN to Your VPC in the Amazon Virtual Private Cloud User Guide. Example 1
-- This example describes the specified virtual private gateway.
-- https://ec2.amazonaws.com/?Action=DescribeVpnGateways
-- &amp;VpnGatewayId.1=vgw-8db04f81 &amp;AUTHPARAMS
-- &lt;DescribeVpnGatewaysResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnGatewaySet&gt; &lt;item&gt;
-- &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt;
-- &lt;state&gt;available&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;attachments&gt; &lt;item&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;vpcId&gt;
-- &lt;state&gt;attached&lt;/state&gt; &lt;/item&gt; &lt;/attachments&gt;
-- &lt;tagSet/&gt; &lt;/item&gt; &lt;/vpnGatewaySet&gt;
-- &lt;/DescribeVpnGatewaysResponse&gt; Example 2 This example uses filters to
-- describe any virtual private gateway you own that is in the us-east-1a
-- Availability Zone, and whose state is either pending or available.
-- https://ec2.amazonaws.com/?Action=DescribeVpnGateways
-- &amp;Filter.1.Name=availability-zone &amp;Filter.1.Value.1=us-east-1a
-- &amp;Filter.2.Name=state &amp;Filter.2.Value.1=pending
-- &amp;Filter.2.Value.2=available &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DescribeVpnGateways'

describeVpnGateways :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => State DescribeVpnGateways a
    -> m DescribeVpnGatewaysResponse
describeVpnGateways s =
    send (mkDescribeVpnGateways &~ s)

describeVpnGatewaysCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => State DescribeVpnGateways a
    -> m (Either ServiceErr DescribeVpnGatewaysResponse)
describeVpnGatewaysCatch s =
    sendCatch (mkDescribeVpnGateways &~ s)

-- $DetachInternetGateway
-- Detaches an Internet gateway from a VPC, disabling connectivity between the
-- Internet and the VPC. The VPC must not contain any running instances with
-- Elastic IP addresses. Example The example detaches the specified Internet
-- gateway from the specified VPC.
-- https://ec2.amazonaws.com/?Action=DetachInternetGateway
-- &amp;InternetGatewayId=igw-eaad4883 &amp;VpcId=vpc-11ad4878 &amp;AUTHPARAMS
-- &lt;DetachInternetGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DetachInternetGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DetachInternetGateway'

detachInternetGateway :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dig2InternetGatewayId'
    -> Text -- ^ 'dig2VpcId'
    -> State DetachInternetGateway a
    -> m DetachInternetGatewayResponse
detachInternetGateway p1 p2 s =
    send $ (mkDetachInternetGateway p1 p2) &~ s

detachInternetGatewayCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dig2InternetGatewayId'
    -> Text -- ^ 'dig2VpcId'
    -> State DetachInternetGateway a
    -> m (Either ServiceErr DetachInternetGatewayResponse)
detachInternetGatewayCatch p1 p2 s =
    sendCatch $ (mkDetachInternetGateway p1 p2) &~ s

-- $DetachNetworkInterface
-- Detaches a network interface from an instance. Example This example
-- detaches the specified elastic network interface (ENI).
-- https://ec2.amazonaws.com/?Action=DetachNetworkInterface
-- &amp;AttachmentId=eni-attach-d94b09b0 &amp;AUTHPARAMS
-- &lt;DetachNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;ce540707-0635-46bc-97da-33a8a362a0e8&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DetachNetworkInterfaceResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DetachNetworkInterface'

detachNetworkInterface :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'dni2AttachmentId'
    -> State DetachNetworkInterface a
    -> m DetachNetworkInterfaceResponse
detachNetworkInterface p1 s =
    send $ (mkDetachNetworkInterface p1) &~ s

detachNetworkInterfaceCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'dni2AttachmentId'
    -> State DetachNetworkInterface a
    -> m (Either ServiceErr DetachNetworkInterfaceResponse)
detachNetworkInterfaceCatch p1 s =
    sendCatch $ (mkDetachNetworkInterface p1) &~ s

-- $DetachVolume
-- Detaches an Amazon EBS volume from an instance. Make sure to unmount any
-- file systems on the device within your operating system before detaching
-- the volume. Failure to do so results in the volume being stuck in a busy
-- state while detaching. If an Amazon EBS volume is the root device of an
-- instance, it can't be detached while the instance is running. To detach the
-- root volume, stop the instance first. If the root volume is detached from
-- an instance with an AWS Marketplace product code, then the AWS Marketplace
-- product codes from that volume are no longer associated with the instance.
-- For more information, see Detaching an Amazon EBS Volume in the Amazon
-- Elastic Compute Cloud User Guide. Example This example detaches volume
-- vol-1a2b3c4d. https://ec2.amazonaws.com/?Action=DetachVolume
-- &amp;VolumeId=vol-1a2b3c4d &amp;AUTHPARAMS &lt;DetachVolumeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;instanceId&gt;i-1a2b3c4d&lt;/instanceId&gt;
-- &lt;device&gt;/dev/sdh&lt;/device&gt;
-- &lt;status&gt;detaching&lt;/status&gt;
-- &lt;attachTime&gt;YYYY-MM-DDTHH:MM:SS.000Z&lt;/attachTime&gt;
-- &lt;/DetachVolumeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DetachVolume'

detachVolume :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'dv4VolumeId'
    -> State DetachVolume a
    -> m DetachVolumeResponse
detachVolume p1 s =
    send $ (mkDetachVolume p1) &~ s

detachVolumeCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'dv4VolumeId'
    -> State DetachVolume a
    -> m (Either ServiceErr DetachVolumeResponse)
detachVolumeCatch p1 s =
    sendCatch $ (mkDetachVolume p1) &~ s

-- $DetachVpnGateway
-- Detaches a virtual private gateway from a VPC. You do this if you're
-- planning to turn off the VPC and not use it anymore. You can confirm a
-- virtual private gateway has been completely detached from a VPC by
-- describing the virtual private gateway (any attachments to the virtual
-- private gateway are also described). You must wait for the attachment's
-- state to switch to detached before you can delete the VPC or attach a
-- different VPC to the virtual private gateway. Example This example detaches
-- the specified virtual private gateway from the specified VPC.
-- https://ec2.amazonaws.com/?Action=DetachVpnGateway
-- &amp;VpnGatewayId=vgw-8db04f81 &amp;VpcId=vpc-1a2b3c4d &amp;AUTHPARAMS
-- &lt;DetachVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DetachVpnGatewayResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DetachVpnGateway'

detachVpnGateway :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'dvg2VpnGatewayId'
    -> Text -- ^ 'dvg2VpcId'
    -> State DetachVpnGateway a
    -> m DetachVpnGatewayResponse
detachVpnGateway p1 p2 s =
    send $ (mkDetachVpnGateway p1 p2) &~ s

detachVpnGatewayCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dvg2VpnGatewayId'
    -> Text -- ^ 'dvg2VpcId'
    -> State DetachVpnGateway a
    -> m (Either ServiceErr DetachVpnGatewayResponse)
detachVpnGatewayCatch p1 p2 s =
    sendCatch $ (mkDetachVpnGateway p1 p2) &~ s

-- $DisableVgwRoutePropagation
-- Disables a virtual private gateway (VGW) from propagating routes to the
-- routing tables of a VPC. Example This example disables the virtual private
-- gateway vgw-d8e09e8a from automatically propagating routes to the routing
-- table with ID rtb-c98a35a0.
-- https://ec2.amazonaws.com/?Action=DisableVgwRoutePropagationResponse
-- &amp;RouteTableID=rtb-c98a35a0 &amp;GatewayId= vgw-d8e09e8a &amp;AUTHPARAMS
-- &lt;DisableVgwRoutePropagationResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DisableVgwRoutePropagationResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DisableVgwRoutePropagation'

disableVgwRoutePropagation :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dvrpRouteTableId'
    -> Text -- ^ 'dvrpGatewayId'
    -> State DisableVgwRoutePropagation a
    -> m DisableVgwRoutePropagationResponse
disableVgwRoutePropagation p1 p2 s =
    send $ (mkDisableVgwRoutePropagation p1 p2) &~ s

disableVgwRoutePropagationCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'dvrpRouteTableId'
    -> Text -- ^ 'dvrpGatewayId'
    -> State DisableVgwRoutePropagation a
    -> m (Either ServiceErr DisableVgwRoutePropagationResponse)
disableVgwRoutePropagationCatch p1 p2 s =
    sendCatch $ (mkDisableVgwRoutePropagation p1 p2) &~ s

-- $DisassociateAddress
-- Disassociates an Elastic IP address from the instance or network interface
-- it's associated with. This is an idempotent operation. If you perform the
-- operation more than once, Amazon EC2 doesn't return an error. Example for
-- EC2-Classic This example disassociates the specified Elastic IP address
-- from the instance in EC2-Classic to which it is associated.
-- https://ec2.amazonaws.com/?Action=DisassociateAddress
-- &amp;PublicIp=192.0.2.1 &amp;AUTHPARAMS Example for EC2-VPC This example
-- disassociates the specified Elastic IP address from the instance in a VPC
-- to which it is associated.
-- https://ec2.amazonaws.com/?Action=DisassociateAddress
-- &amp;AssociationId=eipassoc-aa7486c3 &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DisassociateAddress'

disassociateAddress :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => State DisassociateAddress a
    -> m DisassociateAddressResponse
disassociateAddress s =
    send (mkDisassociateAddress &~ s)

disassociateAddressCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => State DisassociateAddress a
    -> m (Either ServiceErr DisassociateAddressResponse)
disassociateAddressCatch s =
    sendCatch (mkDisassociateAddress &~ s)

-- $DisassociateRouteTable
-- Disassociates a subnet from a route table. After you perform this action,
-- the subnet no longer uses the routes in the route table. Instead, it uses
-- the routes in the VPC's main route table. For more information about route
-- tables, see Route Tables in the Amazon Virtual Private Cloud User Guide.
-- Example This example disassociates the specified route table from the
-- subnet it's associated to.
-- https://ec2.amazonaws.com/?Action=DisassociateRouteTable
-- &amp;AssociationId=rtbassoc-fdad4894 &amp;AUTHPARAMS
-- &lt;DisassociateRouteTableResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DisassociateRouteTableResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.DisassociateRouteTable'

disassociateRouteTable :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'drt2AssociationId'
    -> State DisassociateRouteTable a
    -> m DisassociateRouteTableResponse
disassociateRouteTable p1 s =
    send $ (mkDisassociateRouteTable p1) &~ s

disassociateRouteTableCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'drt2AssociationId'
    -> State DisassociateRouteTable a
    -> m (Either ServiceErr DisassociateRouteTableResponse)
disassociateRouteTableCatch p1 s =
    sendCatch $ (mkDisassociateRouteTable p1) &~ s

-- $EnableVgwRoutePropagation
-- Enables a virtual private gateway (VGW) to propagate routes to the routing
-- tables of a VPC. Example This example enables the specified virtual private
-- gateway to propagate routes automatically to the routing table with the ID
-- rtb-c98a35a0. https://ec2.amazonaws.com/?Action=EnableVgwRoutePropagation
-- &amp;RouteTableID=rtb-c98a35a0 &amp;GatewayId= vgw-d8e09e8a &amp;AUTHPARAMS
-- &lt;EnableVgwRoutePropagation
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/EnableVgwRoutePropagation&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.EnableVgwRoutePropagation'

enableVgwRoutePropagation :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'evrpRouteTableId'
    -> Text -- ^ 'evrpGatewayId'
    -> State EnableVgwRoutePropagation a
    -> m EnableVgwRoutePropagationResponse
enableVgwRoutePropagation p1 p2 s =
    send $ (mkEnableVgwRoutePropagation p1 p2) &~ s

enableVgwRoutePropagationCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'evrpRouteTableId'
    -> Text -- ^ 'evrpGatewayId'
    -> State EnableVgwRoutePropagation a
    -> m (Either ServiceErr EnableVgwRoutePropagationResponse)
enableVgwRoutePropagationCatch p1 p2 s =
    sendCatch $ (mkEnableVgwRoutePropagation p1 p2) &~ s

-- $EnableVolumeIO
-- Enables I/O operations for a volume that had I/O operations disabled
-- because the data on the volume was potentially inconsistent. Example This
-- example enables the I/O operations of the volume vol-8888888.
-- https://ec2.amazonaws.com/?Action=EnableVolumeIO &amp;VolumeId= vol-8888888
-- &amp;AUTHPARAMS &lt;EnableVolumeIOResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/EnableVolumeIOResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.EnableVolumeIO'

enableVolumeIO :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'evioVolumeId'
    -> State EnableVolumeIO a
    -> m EnableVolumeIOResponse
enableVolumeIO p1 s =
    send $ (mkEnableVolumeIO p1) &~ s

enableVolumeIOCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'evioVolumeId'
    -> State EnableVolumeIO a
    -> m (Either ServiceErr EnableVolumeIOResponse)
enableVolumeIOCatch p1 s =
    sendCatch $ (mkEnableVolumeIO p1) &~ s

-- $GetConsoleOutput
-- Gets the console output for the specified instance. Instances do not have a
-- physical monitor through which you can view their console output. They also
-- lack physical controls that allow you to power up, reboot, or shut them
-- down. To allow these actions, we provide them through the Amazon EC2 API
-- and command line interface. Instance console output is buffered and posted
-- shortly after instance boot, reboot, and termination. Amazon EC2 preserves
-- the most recent 64 KB output which is available for at least one hour after
-- the most recent post. For Linux/Unix instances, the instance console output
-- displays the exact console output that would normally be displayed on a
-- physical monitor attached to a machine. This output is buffered because the
-- instance produces it and then posts it to a store where the instance's
-- owner can retrieve it. For Windows instances, the instance console output
-- displays the last three system event log errors. Example This example
-- retrieves the console output for the specified instance.
-- https://ec2.amazonaws.com/?Action=GetConsoleOutput
-- &amp;InstanceId=i-10a64379 &amp;AUTHPARAMS &lt;GetConsoleOutputResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-28a64341&lt;/instanceId&gt;
-- &lt;timestamp&gt;2010-10-14T01:12:41.000Z&lt;/timestamp&gt;
-- &lt;output&gt;TGludXggdmVyc2lvbiAyLjYuMTYteGVuVSAoYnVpbGRlckBwYXRjaGJhdC5hbWF6b25zYSkgKGdj
-- 
-- YyB2ZXJzaW9uIDQuMC4xIDIwMDUwNzI3IChSZWQgSGF0IDQuMC4xLTUpKSAjMSBTTVAgVGh1IE9j
-- 
-- dCAyNiAwODo0MToyNiBTQVNUIDIwMDYKQklPUy1wcm92aWRlZCBwaHlzaWNhbCBSQU0gbWFwOgpY
-- 
-- ZW46IDAwMDAwMDAwMDAwMDAwMDAgLSAwMDAwMDAwMDZhNDAwMDAwICh1c2FibGUpCjk4ME1CIEhJ
-- 
-- R0hNRU0gYXZhaWxhYmxlLgo3MjdNQiBMT1dNRU0gYXZhaWxhYmxlLgpOWCAoRXhlY3V0ZSBEaXNh
-- 
-- YmxlKSBwcm90ZWN0aW9uOiBhY3RpdmUKSVJRIGxvY2t1cCBkZXRlY3Rpb24gZGlzYWJsZWQKQnVp
-- 
-- bHQgMSB6b25lbGlzdHMKS2VybmVsIGNvbW1hbmQgbGluZTogcm9vdD0vZGV2L3NkYTEgcm8gNApF
-- bmFibGluZyBmYXN0IEZQVSBzYXZlIGFuZCByZXN0b3JlLi4uIGRvbmUuCg==&lt;/output&gt;
-- &lt;/GetConsoleOutputResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.GetConsoleOutput'

getConsoleOutput :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'gcoInstanceId'
    -> State GetConsoleOutput a
    -> m GetConsoleOutputResponse
getConsoleOutput p1 s =
    send $ (mkGetConsoleOutput p1) &~ s

getConsoleOutputCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'gcoInstanceId'
    -> State GetConsoleOutput a
    -> m (Either ServiceErr GetConsoleOutputResponse)
getConsoleOutputCatch p1 s =
    sendCatch $ (mkGetConsoleOutput p1) &~ s

-- $GetPasswordData
-- Retrieves the encrypted administrator password for an instance running
-- Windows. The Windows password is only generated the first time an AMI is
-- launched. It is not generated for rebundled AMIs or after the password is
-- changed on an instance. The password is encrypted using the key pair that
-- you specified when you launched the instance. You must provide the
-- corresponding key pair file. Password generation and encryption takes a few
-- moments. We recommend that you wait up to 15 minutes after launching an
-- instance before trying to retrieve the generated password. Example This
-- example returns the encrypted version of the administrator password for the
-- specified instance. https://ec2.amazonaws.com/?Action=GetPasswordData
-- &amp;InstanceId=i-10a64379 &amp;AUTHPARAMS &lt;GetPasswordDataResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-2574e22a&lt;/instanceId&gt; &lt;timestamp&gt;2009-10-24
-- 15:00:00&lt;/timestamp&gt;
-- &lt;passwordData&gt;TGludXggdmVyc2lvbiAyLjYuMTYteGVuVSAoYnVpbGRlckBwYXRjaGJhdC5hbWF6b25zYSkgKGdj&lt;/passwordData&gt;
-- &lt;/GetPasswordDataResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.GetPasswordData'

getPasswordData :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'gpdInstanceId'
    -> State GetPasswordData a
    -> m GetPasswordDataResponse
getPasswordData p1 s =
    send $ (mkGetPasswordData p1) &~ s

getPasswordDataCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'gpdInstanceId'
    -> State GetPasswordData a
    -> m (Either ServiceErr GetPasswordDataResponse)
getPasswordDataCatch p1 s =
    sendCatch $ (mkGetPasswordData p1) &~ s

-- $ImportInstance
-- Creates an import instance task using metadata from the specified disk
-- image. After importing the image, you then upload it using the
-- ec2-import-volume command in the EC2 command line tools. For more
-- information, see Using the Command Line Tools to Import Your Virtual
-- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
-- Example This example creates an import instance task that migrates a
-- Windows Server 2008 SP2 (32-bit) VM into the AWS us-east-1 region.
-- https://ec2.amazonaws.com/?Action=ImportInstance
-- &amp;LaunchSpecification.Architecture=x86_64
-- &amp;LaunchSpecification.InstanceType=m1.xlarge
-- &amp;DiskImage.1.Image.Format=VMDK &amp;DiskImage.1.Image.Bytes=1179593728
-- &amp;DiskImage.1.Image.ImportManifestUrl=https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- &amp;DiskImage.1.Volume.Size=12 &amp;Platform=Windows &amp;AUTHPARAMS
-- import-i-ffvko9js 2010-12-22T12:01Z 0 us-east-1a VMDK 1179593728
-- https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- 12 vol-1a2b3c4d active i-12655a7f.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ImportInstance'

importInstance :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => PlatformValues -- ^ 'iiPlatform'
    -> State ImportInstance a
    -> m ImportInstanceResponse
importInstance p4 s =
    send $ (mkImportInstance p4) &~ s

importInstanceCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => PlatformValues -- ^ 'iiPlatform'
    -> State ImportInstance a
    -> m (Either ServiceErr ImportInstanceResponse)
importInstanceCatch p4 s =
    sendCatch $ (mkImportInstance p4) &~ s

-- $ImportKeyPair
-- Imports the public key from an RSA key pair that you created with a
-- third-party tool. Compare this with CreateKeyPair, in which AWS creates the
-- key pair and gives the keys to you (AWS keeps a copy of the public key).
-- With ImportKeyPair, you create the key pair and give AWS just the public
-- key. The private key is never transferred between you and AWS. For more
-- information about key pairs, see Key Pairs in the Amazon Elastic Compute
-- Cloud User Guide. Example This example imports the public key named
-- my-key-pair. https://ec2.amazonaws.com/?Action=ImportKeyPair
-- &amp;KeyName=my-key-pair
-- &amp;PublicKeyMaterial=MIICiTCCAfICCQD6m7oRw0uXOjANBgkqhkiG9w0BAQUFADCBiDELMAkGA1UEBhMC
-- VVMxCzAJBgNVBAgTAldBMRAwDgYDVQQHEwdTZWF0dGxlMQ8wDQYDVQQKEwZBbWF6
-- b24xFDASBgNVBAsTC0lBTSBDb25zb2xlMRIwEAYDVQQDEwlUZXN0Q2lsYWMxHzAd
-- BgkqhkiG9w0BCQEWEG5vb25lQGFtYXpvbi5jb20wHhcNMTEwNDI1MjA0NTIxWhcN
-- MTIwNDI0MjA0NTIxWjCBiDELMAkGA1UEBhMCVVMxCzAJBgNVBAgTAldBMRAwDgYD
-- VQQHEwdTZWF0dGxlMQ8wDQYDVQQKEwZBbWF6b24xFDASBgNVBAsTC0lBTSBDb25z
-- b2xlMRIwEAYDVQQDEwlUZXN0Q2lsYWMxHzAdBgkqhkiG9w0BCQEWEG5vb25lQGFt
-- YXpvbi5jb20wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAMaK0dn+a4GmWIWJ
-- 21uUSfwfEvySWtC2XADZ4nB+BLYgVIk60CpiwsZ3G93vUEIO3IyNoH/f0wYK8m9T
-- rDHudUZg3qX4waLG5M43q7Wgc/MbQITxOUSQv7c7ugFFDzQGBzZswY6786m86gpE
-- Ibb3OhjZnzcvQAaRHhdlQWIMm2nrAgMBAAEwDQYJKoZIhvcNAQEFBQADgYEAtCu4
-- nUhVVxYUntneD9+h8Mg9q6q+auNKyExzyLwaxlAoo7TJHidbtS4J5iNmZgXL0Fkb
-- FFBjvSfpJIlJ00zbhNYS5f6GuoEDmFJl0ZxBHjJnyp378OD8uTs7fLvjx79LjSTb
-- NYiytVbZPQUQ5Yaxu2jXnimvw3rrszlaEXAMPLE &amp;AUTHPARAMS
-- &lt;ImportKeyPairResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;keyName&gt;my-key-pair&lt;/keyName&gt;
-- &lt;keyFingerprint&gt;1f:51:ae:28:bf:89:e9:d8:1f:25:5d:37:2d:7d:b8:ca:9f:f5:f1:6f&lt;/keyFingerprint&gt;
-- &lt;/ImportKeyPairResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ImportKeyPair'

importKeyPair :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'ikpKeyName'
    -> ByteString -- ^ 'ikpPublicKeyMaterial'
    -> State ImportKeyPair a
    -> m ImportKeyPairResponse
importKeyPair p1 p2 s =
    send $ (mkImportKeyPair p1 p2) &~ s

importKeyPairCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ikpKeyName'
    -> ByteString -- ^ 'ikpPublicKeyMaterial'
    -> State ImportKeyPair a
    -> m (Either ServiceErr ImportKeyPairResponse)
importKeyPairCatch p1 p2 s =
    sendCatch $ (mkImportKeyPair p1 p2) &~ s

-- $ImportVolume
-- Creates an import volume task using metadata from the specified disk image.
-- After importing the image, you then upload it using the ec2-import-volume
-- command in the Amazon EC2 command-line interface (CLI) tools. For more
-- information, see Using the Command Line Tools to Import Your Virtual
-- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
-- Example This example creates an import volume task that migrates a Windows
-- Server 2008 SP2 (32-bit) volume into the AWS us-east-1 region.
-- https://ec2.amazonaws.com/?Action=ImportVolume
-- &amp;AvailabilityZone=us-east-1c &amp;Image.Format=VMDK
-- &amp;Image.Bytes=128696320
-- &amp;Image.ImportManifestUrl=https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- &amp;VolumeSize=8 &amp;AUTHPARAMS&gt; import-i-fh95npoc 2010-12-22T12:01Z 0
-- us-east-1c VDMK 128696320
-- https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- ccb1b0536a4a70e86016b85229b5c6b10b14a4eb 8 vol-34d8a2ff active.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ImportVolume'

importVolume :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'ivAvailabilityZone'
    -> DiskImageDetail -- ^ 'ivImage'
    -> VolumeDetail -- ^ 'ivVolume'
    -> State ImportVolume a
    -> m ImportVolumeResponse
importVolume p1 p2 p4 s =
    send $ (mkImportVolume p1 p2 p4) &~ s

importVolumeCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'ivAvailabilityZone'
    -> DiskImageDetail -- ^ 'ivImage'
    -> VolumeDetail -- ^ 'ivVolume'
    -> State ImportVolume a
    -> m (Either ServiceErr ImportVolumeResponse)
importVolumeCatch p1 p2 p4 s =
    sendCatch $ (mkImportVolume p1 p2 p4) &~ s

-- $ModifyImageAttribute
-- Modifies the specified attribute of the specified AMI. You can specify only
-- one attribute at a time. AWS Marketplace product codes cannot be modified.
-- Images with an AWS Marketplace product code cannot be made public. Example.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ModifyImageAttribute'

modifyImageAttribute :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'miaImageId'
    -> State ModifyImageAttribute a
    -> m ModifyImageAttributeResponse
modifyImageAttribute p1 s =
    send $ (mkModifyImageAttribute p1) &~ s

modifyImageAttributeCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'miaImageId'
    -> State ModifyImageAttribute a
    -> m (Either ServiceErr ModifyImageAttributeResponse)
modifyImageAttributeCatch p1 s =
    sendCatch $ (mkModifyImageAttribute p1) &~ s

-- $ModifyInstanceAttribute
-- Modifies the specified attribute of the specified instance. You can specify
-- only one attribute at a time. To modify some attributes, the instance must
-- be stopped. For more information, see Modifying Attributes of a Stopped
-- Instance in the Amazon Elastic Compute Cloud User Guide. Example 1 This
-- example changes the instance type of the specified instance. The instance
-- must be in the stopped state.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;InstanceType.Value=m1.small &amp;AUTHPARAMS
-- &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;
-- Example 2 This example changes the InstanceInitiatedShutdownBehavior
-- attribute of the specified instance.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379
-- &amp;InstanceInitiatedShutdownBehavior.Value=terminate &amp;AUTHPARAMS
-- &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;
-- Example 3 This example changes the DisableApiTermination attribute of the
-- specified instance.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;DisableApiTermination.Value=true
-- &amp;AUTHPARAMS &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ModifyInstanceAttribute'

modifyInstanceAttribute :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'mia1InstanceId'
    -> State ModifyInstanceAttribute a
    -> m ModifyInstanceAttributeResponse
modifyInstanceAttribute p1 s =
    send $ (mkModifyInstanceAttribute p1) &~ s

modifyInstanceAttributeCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'mia1InstanceId'
    -> State ModifyInstanceAttribute a
    -> m (Either ServiceErr ModifyInstanceAttributeResponse)
modifyInstanceAttributeCatch p1 s =
    sendCatch $ (mkModifyInstanceAttribute p1) &~ s

-- $ModifyNetworkInterfaceAttribute
-- Modifies the specified network interface attribute. You can specify only
-- one attribute at a time. Example This example sets source/destination
-- checking to false for the specified network interface.
-- https://ec2.amazonaws.com/?Action=ModifyNetworkInterfaceAttribute
-- &amp;NetworkInterfaceId=eni-ffda3197 &amp;SourceDestCheck.Value=false
-- &amp;AUTHPARAMS &lt;ModifyNetworkInterfaceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;657a4623-5620-4232-b03b-427e852d71cf&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/ModifyNetworkInterfaceAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ModifyNetworkInterfaceAttribute'

modifyNetworkInterfaceAttribute :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError AWS.Error m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'mniaNetworkInterfaceId'
    -> State ModifyNetworkInterfaceAttribute a
    -> m ModifyNetworkInterfaceAttributeResponse
modifyNetworkInterfaceAttribute p1 s =
    send $ (mkModifyNetworkInterfaceAttribute p1) &~ s

modifyNetworkInterfaceAttributeCatch :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadReader Env m
                                        )
    => Text -- ^ 'mniaNetworkInterfaceId'
    -> State ModifyNetworkInterfaceAttribute a
    -> m (Either ServiceErr ModifyNetworkInterfaceAttributeResponse)
modifyNetworkInterfaceAttributeCatch p1 s =
    sendCatch $ (mkModifyNetworkInterfaceAttribute p1) &~ s

-- $ModifyReservedInstances
-- Modifies the Availability Zone, instance count, instance type, or network
-- platform (EC2-Classic or EC2-VPC) of your Reserved Instances. The Reserved
-- Instances to be modified must be identical, except for Availability Zone,
-- network platform, and instance type. Example
-- https://ec2.amazonaws.com/?Action=ModifyReservedInstances
-- &amp;ClientToken=myClientToken
-- &amp;ReservedInstancesConfigurationSetItemType.0.AvailabilityZone=us-east-1a
-- &amp;ReservedInstancesConfigurationSetItemType.0.InstanceCount=1
-- &amp;ReservedInstancesConfigurationSetItemType.0.Platform=EC2-VPC
-- &amp;ReservedInstancesConfigurationSetItemType.0.InstanceType=m1.small
-- &amp;ReservedInstancesId.0=d16f7a91-4d0f-4f19-9d7f-a74d26b1ccfa
-- &amp;AUTHPARAMS bef729b6-0731-4489-8881-2258746ae163
-- rimod-3aae219d-3d63-47a9-a7e9-e764example.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ModifyReservedInstances'

modifyReservedInstances :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => [Text] -- ^ 'mriReservedInstancesIds'
    -> [ReservedInstancesConfiguration] -- ^ 'mriTargetConfigurations'
    -> State ModifyReservedInstances a
    -> m ModifyReservedInstancesResponse
modifyReservedInstances p2 p3 s =
    send $ (mkModifyReservedInstances p2 p3) &~ s

modifyReservedInstancesCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => [Text] -- ^ 'mriReservedInstancesIds'
    -> [ReservedInstancesConfiguration] -- ^ 'mriTargetConfigurations'
    -> State ModifyReservedInstances a
    -> m (Either ServiceErr ModifyReservedInstancesResponse)
modifyReservedInstancesCatch p2 p3 s =
    sendCatch $ (mkModifyReservedInstances p2 p3) &~ s

-- $ModifySnapshotAttribute
-- Adds or removes permission settings for the specified snapshot. You may add
-- or remove specified AWS account IDs from a snapshot's list of create volume
-- permissions, but you cannot do both in a single API call. If you need to
-- both add and remove account IDs for a snapshot, you must use multiple API
-- calls. For more information on modifying snapshot permissions, see Sharing
-- Snapshots in the Amazon Elastic Compute Cloud User Guide. Snapshots with
-- AWS Marketplace product codes cannot be made public. Example This example
-- makes the snap-1a2b3c4d snapshot public, and gives the account with ID
-- 111122223333 permission to create volumes from the snapshot.
-- https://ec2.amazonaws.com/?Action=ModifySnapshotAttribute
-- &amp;snapshotId=snap-1a2b3c4d
-- &amp;CreateVolumePermission.Add.1.UserId=111122223333
-- &amp;CreateVolumePermission.Add.1.Group=all &amp;AUTHPARAMS
-- &lt;ModifySnapshotAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifySnapshotAttributeResponse&gt;
-- Example This example makes the snap-1a2b3c4d snapshot public, and removes
-- the account with ID 111122223333 from the list of users with permission to
-- create volumes from the snapshot.
-- https://ec2.amazonaws.com/?Action=ModifySnapshotAttribute
-- &amp;snapshotId=snap-1a2b3c4d
-- &amp;CreateVolumePermission.Remove.1.UserId=111122223333
-- &amp;CreateVolumePermission.Add.1.Group=all &amp;AUTHPARAMS
-- &lt;ModifySnapshotAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifySnapshotAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ModifySnapshotAttribute'

modifySnapshotAttribute :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'msaSnapshotId'
    -> State ModifySnapshotAttribute a
    -> m ModifySnapshotAttributeResponse
modifySnapshotAttribute p1 s =
    send $ (mkModifySnapshotAttribute p1) &~ s

modifySnapshotAttributeCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'msaSnapshotId'
    -> State ModifySnapshotAttribute a
    -> m (Either ServiceErr ModifySnapshotAttributeResponse)
modifySnapshotAttributeCatch p1 s =
    sendCatch $ (mkModifySnapshotAttribute p1) &~ s

-- $ModifySubnetAttribute
-- Modifies a subnet attribute. Example This example modifies the attribute
-- for subnet-1a2b3c4d to specify that all instances launched into this subnet
-- are assigned a public IP address.
-- https://ec2.amazonaws.com/?Action=ModifySubnetAttribute
-- &amp;SubnetId=subnet-1a2b3c4d &amp;MapPublicIpOnLaunch.Value=true
-- &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ModifySubnetAttribute'

modifySubnetAttribute :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'msa1SubnetId'
    -> State ModifySubnetAttribute a
    -> m ModifySubnetAttributeResponse
modifySubnetAttribute p1 s =
    send $ (mkModifySubnetAttribute p1) &~ s

modifySubnetAttributeCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'msa1SubnetId'
    -> State ModifySubnetAttribute a
    -> m (Either ServiceErr ModifySubnetAttributeResponse)
modifySubnetAttributeCatch p1 s =
    sendCatch $ (mkModifySubnetAttribute p1) &~ s

-- $ModifyVolumeAttribute
-- Modifies a volume attribute. By default, all I/O operations for the volume
-- are suspended when the data on the volume is determined to be potentially
-- inconsistent, to prevent undetectable, latent data corruption. The I/O
-- access to the volume can be resumed by first enabling I/O access and then
-- checking the data consistency on your volume. You can change the default
-- behavior to resume I/O operations. We recommend that you change this only
-- for boot volumes or for volumes that are stateless or disposable. Example
-- This example modifies the attribute of the volume vol-12345678.
-- https://ec2.amazonaws.com/?Action=ModifyVolumeAttribute
-- &amp;VolumeId=vol-12345678 &amp;AutoEnableIO.Value=true &amp;AUTHPARAMS
-- &lt;ModifyVolumeAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;5jkdf074-37ed-4004-8671-a78ee82bf1cbEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyVolumeAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ModifyVolumeAttribute'

modifyVolumeAttribute :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'mvaVolumeId'
    -> State ModifyVolumeAttribute a
    -> m ModifyVolumeAttributeResponse
modifyVolumeAttribute p1 s =
    send $ (mkModifyVolumeAttribute p1) &~ s

modifyVolumeAttributeCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'mvaVolumeId'
    -> State ModifyVolumeAttribute a
    -> m (Either ServiceErr ModifyVolumeAttributeResponse)
modifyVolumeAttributeCatch p1 s =
    sendCatch $ (mkModifyVolumeAttribute p1) &~ s

-- $ModifyVpcAttribute
-- Modifies the specified attribute of the specified VPC. Example This example
-- disables support for DNS hostnames in the specified VPC.
-- https://ec2.amazonaws.com/?Action=ModifyVpcAttribute
-- &amp;VpcId=vpc-1a2b3c4d &amp;EnableDnsHostnames.Value=false
-- &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ModifyVpcAttribute'

modifyVpcAttribute :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'mva1VpcId'
    -> State ModifyVpcAttribute a
    -> m ModifyVpcAttributeResponse
modifyVpcAttribute p1 s =
    send $ (mkModifyVpcAttribute p1) &~ s

modifyVpcAttributeCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'mva1VpcId'
    -> State ModifyVpcAttribute a
    -> m (Either ServiceErr ModifyVpcAttributeResponse)
modifyVpcAttributeCatch p1 s =
    sendCatch $ (mkModifyVpcAttribute p1) &~ s

-- $MonitorInstances
-- Enables monitoring for a running instance. For more information about
-- monitoring instances, see Monitoring Your Instances and Volumes in the
-- Amazon Elastic Compute Cloud User Guide. Example This example enables
-- monitoring for two instances.
-- https://ec2.amazonaws.com/?Action=MonitorInstances
-- &amp;InstanceId.1=i-43a4412a &amp;InstanceId.2=i-23a3397d &amp;AUTHPARAMS
-- &lt;MonitorInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-43a4412a&lt;/instanceId&gt; &lt;monitoring&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;/monitoring&gt; &lt;/item&gt;
-- &lt;item&gt; &lt;instanceId&gt;i-23a3397d&lt;/instanceId&gt;
-- &lt;monitoring&gt; &lt;state&gt;pending&lt;/state&gt; &lt;/monitoring&gt;
-- &lt;/item&gt; &lt;/instancesSet&gt; &lt;/MonitorInstancesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.MonitorInstances'

monitorInstances :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => [Text] -- ^ 'miInstanceIds'
    -> State MonitorInstances a
    -> m MonitorInstancesResponse
monitorInstances p1 s =
    send $ (mkMonitorInstances p1) &~ s

monitorInstancesCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => [Text] -- ^ 'miInstanceIds'
    -> State MonitorInstances a
    -> m (Either ServiceErr MonitorInstancesResponse)
monitorInstancesCatch p1 s =
    sendCatch $ (mkMonitorInstances p1) &~ s

-- $PurchaseReservedInstancesOffering
-- Purchases a Reserved Instance for use with your account. With Amazon EC2
-- Reserved Instances, you obtain a capacity reservation for a certain
-- instance configuration over a specified period of time. You pay a lower
-- usage rate than with On-Demand instances for the time that you actually use
-- the capacity reservation. For more information, see Reserved Instance
-- Marketplace in the Amazon Elastic Compute Cloud User Guide. Example 1 This
-- example uses a limit price to limit the total purchase order of Reserved
-- Instances from Reserved Instance Marketplace.
-- https://ec2.amazonaws.com/?Action=PurchaseReservedInstancesOffering
-- &amp;ReservedInstancesOfferingId=4b2293b4-5813-4cc8-9ce3-1957fEXAMPLE
-- &amp;LimitPrice.Amount=200 &amp;InstanceCount=2 &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- Example 2 This example illustrates a purchase of a Reserved Instances
-- offering.
-- https://ec2.amazonaws.com/?Action=PurchaseReservedInstancesOffering
-- &amp;ReservedInstancesOfferingId=4b2293b4-5813-4cc8-9ce3-1957fEXAMPLE
-- &amp;InstanceCount=2 &amp;AUTHPARAMS
-- &lt;PurchaseReservedInstancesOfferingResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;reservedInstancesId&gt;e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE&lt;/reservedInstancesId&gt;
-- &lt;/PurchaseReservedInstancesOfferingResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.PurchaseReservedInstancesOffering'

purchaseReservedInstancesOffering :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'prioReservedInstancesOfferingId'
    -> Integer -- ^ 'prioInstanceCount'
    -> State PurchaseReservedInstancesOffering a
    -> m PurchaseReservedInstancesOfferingResponse
purchaseReservedInstancesOffering p1 p2 s =
    send $ (mkPurchaseReservedInstancesOffering p1 p2) &~ s

purchaseReservedInstancesOfferingCatch :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadReader Env m
                                          )
    => Text -- ^ 'prioReservedInstancesOfferingId'
    -> Integer -- ^ 'prioInstanceCount'
    -> State PurchaseReservedInstancesOffering a
    -> m (Either ServiceErr PurchaseReservedInstancesOfferingResponse)
purchaseReservedInstancesOfferingCatch p1 p2 s =
    sendCatch $ (mkPurchaseReservedInstancesOffering p1 p2) &~ s

-- $RebootInstances
-- Requests a reboot of one or more instances. This operation is asynchronous;
-- it only queues a request to reboot the specified instances. The operation
-- succeeds if the instances are valid and belong to you. Requests to reboot
-- terminated instances are ignored. If a Linux/Unix instance does not cleanly
-- shut down within four minutes, Amazon EC2 performs a hard reboot. For more
-- information about troubleshooting, see Getting Console Output and Rebooting
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example reboots two instances.
-- https://ec2.amazonaws.com/?Action=RebootInstances
-- &amp;InstanceId.1=i-1a2b3c4d &amp;InstanceId.2=i-4d3acf62 &amp;AUTHPARAMS
-- &lt;RebootInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/RebootInstancesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.RebootInstances'

rebootInstances :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => [Text] -- ^ 'ri1InstanceIds'
    -> State RebootInstances a
    -> m RebootInstancesResponse
rebootInstances p1 s =
    send $ (mkRebootInstances p1) &~ s

rebootInstancesCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => [Text] -- ^ 'ri1InstanceIds'
    -> State RebootInstances a
    -> m (Either ServiceErr RebootInstancesResponse)
rebootInstancesCatch p1 s =
    sendCatch $ (mkRebootInstances p1) &~ s

-- $RegisterImage
-- Registers an AMI. When you're creating an AMI, this is the final step you
-- must complete before you can launch an instance from the AMI. For more
-- information about creating AMIs, see Creating Your Own AMIs in the Amazon
-- Elastic Compute Cloud User Guide. For Amazon EBS-backed instances,
-- CreateImage creates and registers the AMI in a single request, so you don't
-- have to register the AMI yourself. You can also use RegisterImage to create
-- an Amazon EBS-backed AMI from a snapshot of a root device volume. For more
-- information, see Launching an Instance from a Snapshot in the Amazon
-- Elastic Compute Cloud User Guide. If needed, you can deregister an AMI at
-- any time. Any modifications you make to an AMI backed by an instance store
-- volume invalidates its registration. If you make changes to an image,
-- deregister the previous image and register the new image. You can't
-- register an image where a secondary (non-root) snapshot has AWS Marketplace
-- product codes. Example 1 This example registers the AMI specified in the
-- my-new-image.manifest.xml manifest file, located in the bucket called
-- myawsbucket. https://ec2.amazonaws.com/?Action=RegisterImage
-- &amp;ImageLocation=myawsbucket/my-new-image.manifest.xml &amp;AUTHPARAMS
-- &lt;RegisterImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-1a2b3c4d&lt;/imageId&gt; &lt;/RegisterImageResponse&gt;
-- Example 2 This example registers an Amazon EBS snapshot to create an AMI
-- backed by Amazon EBS. https://ec2.amazonaws.com/?Action=RegisterImage
-- &amp;RootDeviceName=/dev/sda1
-- &amp;BlockDeviceMapping.1.DeviceName=/dev/sda1
-- &amp;BlockDeviceMapping.1.Ebs.SnapshotId=snap-1a2b3c4d &amp;Name=MyImage
-- &amp;AUTHPARAMS &lt;RegisterImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-1a2b3c4d&lt;/imageId&gt; &lt;/RegisterImageResponse&gt;
-- Example 3 This example registers the AMI with an Amazon EBS snapshot as the
-- root device, a separate snapshot as a secondary device, and an empty 100
-- GiB Amazon EBS volume as a storage device.
-- https://ec2.amazonaws.com/?Action=RegisterImage
-- &amp;RootDeviceName=/dev/sda1
-- &amp;BlockDeviceMapping.1.DeviceName=/dev/sda1
-- &amp;BlockDeviceMapping.1.Ebs.SnapshotId=snap-1a2b3c4d
-- &amp;BlockDeviceMapping.2.DeviceName=/dev/sdb
-- &amp;BlockDeviceMapping.2.Ebs.SnapshotId=snap-2a2b3c4d
-- &amp;BlockDeviceMapping.3.DeviceName=/dev/sdc
-- &amp;BlockDeviceMapping.3.Ebs.VolumeSize=100 &amp;Name=MyImage
-- &amp;AUTHPARAMS &lt;RegisterImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-1a2b3c4d&lt;/imageId&gt; &lt;/RegisterImageResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.RegisterImage'

registerImage :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'ri2Name'
    -> State RegisterImage a
    -> m RegisterImageResponse
registerImage p2 s =
    send $ (mkRegisterImage p2) &~ s

registerImageCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ri2Name'
    -> State RegisterImage a
    -> m (Either ServiceErr RegisterImageResponse)
registerImageCatch p2 s =
    sendCatch $ (mkRegisterImage p2) &~ s

-- $RejectVpcPeeringConnection
-- Rejects a VPC peering connection request. The VPC peering connection must
-- be in the pending-acceptance state. Use the DescribeVpcPeeringConnections
-- request to view your outstanding VPC peering connection requests. Example
-- This example rejects the specified VPC peering connection request.
-- https://ec2.amazonaws.com/?Action=RejectVpcPeeringConnection
-- &amp;vpcPeeringConnectionId=pcx-1a2b3c4d &amp;AUTHPARAMS
-- &lt;RejectVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/RejectVpcPeeringConnectionResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.RejectVpcPeeringConnection'

rejectVpcPeeringConnection :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'rvpcVpcPeeringConnectionId'
    -> State RejectVpcPeeringConnection a
    -> m RejectVpcPeeringConnectionResponse
rejectVpcPeeringConnection p1 s =
    send $ (mkRejectVpcPeeringConnection p1) &~ s

rejectVpcPeeringConnectionCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'rvpcVpcPeeringConnectionId'
    -> State RejectVpcPeeringConnection a
    -> m (Either ServiceErr RejectVpcPeeringConnectionResponse)
rejectVpcPeeringConnectionCatch p1 s =
    sendCatch $ (mkRejectVpcPeeringConnection p1) &~ s

-- $ReleaseAddress
-- Releases the specified Elastic IP address. After releasing an Elastic IP
-- address, it is released to the IP address pool and might be unavailable to
-- you. Be sure to update your DNS records and any servers or devices that
-- communicate with the address. If you attempt to release an Elastic IP
-- address that you already released, you'll get an AuthFailure error if the
-- address is already allocated to another AWS account. [EC2-Classic, default
-- VPC] Releasing an Elastic IP address automatically disassociates it from
-- any instance that it's associated with. To disassociate an Elastic IP
-- address without releasing it, use DisassociateAddress. [Nondefault VPC] You
-- must use the DisassociateAddress to disassociate the Elastic IP address
-- before you try to release it. Otherwise, Amazon EC2 returns an error
-- (InvalidIPAddress.InUse). Example for EC2-Classic This example releases the
-- specified Elastic IP address for EC2-Classic.
-- https://ec2.amazonaws.com/?Action=ReleaseAddress &amp;PublicIp=192.0.2.1
-- &amp;AUTHPARAMS Example for EC2-VPC This example releases the specified
-- Elastic IP address for EC2-VPC.
-- https://ec2.amazonaws.com/?Action=ReleaseAddress
-- &amp;AllocationId=eipalloc-5723d13e &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ReleaseAddress'

releaseAddress :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => State ReleaseAddress a
    -> m ReleaseAddressResponse
releaseAddress s =
    send (mkReleaseAddress &~ s)

releaseAddressCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => State ReleaseAddress a
    -> m (Either ServiceErr ReleaseAddressResponse)
releaseAddressCatch s =
    sendCatch (mkReleaseAddress &~ s)

-- $ReplaceNetworkAclAssociation
-- Changes which network ACL a subnet is associated with. By default when you
-- create a subnet, it's automatically associated with the default network
-- ACL. For more information about network ACLs, see Network ACLs in the
-- Amazon Virtual Private Cloud User Guide. Example This example starts with a
-- network ACL associated with a subnet, and a corresponding association ID
-- aclassoc-e5b95c8c. You want to associate a different network ACL
-- (acl-5fb85d36) with the subnet. The result is a new association ID
-- representing the new association.
-- https://ec2.amazonaws.com/?Action=ReplaceNetworkAclAssociation
-- &amp;AssociationId=aclassoc-e5b95c8c &amp;NetworkAclId=acl-5fb85d36
-- &amp;AUTHPARAMS &lt;ReplaceNetworkAclAssociationResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;newAssociationId&gt;aclassoc-17b85d7e&lt;/newAssociationId&gt;
-- &lt;/ReplaceNetworkAclAssociationResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclAssociation'

replaceNetworkAclAssociation :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => Text -- ^ 'rnaaAssociationId'
    -> Text -- ^ 'rnaaNetworkAclId'
    -> State ReplaceNetworkAclAssociation a
    -> m ReplaceNetworkAclAssociationResponse
replaceNetworkAclAssociation p1 p2 s =
    send $ (mkReplaceNetworkAclAssociation p1 p2) &~ s

replaceNetworkAclAssociationCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'rnaaAssociationId'
    -> Text -- ^ 'rnaaNetworkAclId'
    -> State ReplaceNetworkAclAssociation a
    -> m (Either ServiceErr ReplaceNetworkAclAssociationResponse)
replaceNetworkAclAssociationCatch p1 p2 s =
    sendCatch $ (mkReplaceNetworkAclAssociation p1 p2) &~ s

-- $ReplaceNetworkAclEntry
-- Replaces an entry (rule) in a network ACL. For more information about
-- network ACLs, see Network ACLs in the Amazon Virtual Private Cloud User
-- Guide. Example This example replaces the egress entry numbered 110 in the
-- network ACL with ID acl-2cb85d45. The new rule denies egress traffic
-- destined for anywhere (0.0.0.0/0) on TCP port 139.
-- https://ec2.amazonaws.com/?Action=ReplaceNetworkAclEntry
-- &amp;NetworkAclId=acl-2cb85d45 &amp;RuleNumber=110 &amp;Protocol=tcp
-- &amp;RuleAction=deny &amp;Egress=true &amp;CidrBlock=0.0.0.0/0
-- &amp;PortRange.From=139 &amp;PortRange.To=139 &amp;AUTHPARAMS
-- &lt;ReplaceNetworkAclEntryResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ReplaceNetworkAclEntryResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclEntry'

replaceNetworkAclEntry :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'rnaeNetworkAclId'
    -> Integer -- ^ 'rnaeRuleNumber'
    -> Text -- ^ 'rnaeProtocol'
    -> RuleAction -- ^ 'rnaeRuleAction'
    -> Bool -- ^ 'rnaeEgress'
    -> Text -- ^ 'rnaeCidrBlock'
    -> State ReplaceNetworkAclEntry a
    -> m ReplaceNetworkAclEntryResponse
replaceNetworkAclEntry p1 p2 p3 p4 p5 p6 s =
    send $ (mkReplaceNetworkAclEntry p1 p2 p3 p4 p5 p6) &~ s

replaceNetworkAclEntryCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'rnaeNetworkAclId'
    -> Integer -- ^ 'rnaeRuleNumber'
    -> Text -- ^ 'rnaeProtocol'
    -> RuleAction -- ^ 'rnaeRuleAction'
    -> Bool -- ^ 'rnaeEgress'
    -> Text -- ^ 'rnaeCidrBlock'
    -> State ReplaceNetworkAclEntry a
    -> m (Either ServiceErr ReplaceNetworkAclEntryResponse)
replaceNetworkAclEntryCatch p1 p2 p3 p4 p5 p6 s =
    sendCatch $ (mkReplaceNetworkAclEntry p1 p2 p3 p4 p5 p6) &~ s

-- $ReplaceRoute
-- Replaces an existing route within a route table in a VPC. You must provide
-- only one of the following: Internet gateway, NAT instance, VPC peering
-- connection, or network interface. For more information about route tables,
-- see Route Tables in the Amazon Virtual Private Cloud User Guide. Example
-- This example replaces a route in the specified route table. The new route
-- matches the CIDR 10.0.0.0/8 and sends the traffic to the virtual private
-- gateway with the ID vgw-1d00376e.
-- https://ec2.amazonaws.com/?Action=ReplaceRoute
-- &amp;RouteTableId=rtb-e4ad488d &amp;DestinationCidrBlock=10.0.0.0/8
-- &amp;GatewayId=vgw-1d00376e &amp;AUTHPARAMS &lt;ReplaceRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ReplaceRouteResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ReplaceRoute'

replaceRoute :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'rr1RouteTableId'
    -> Text -- ^ 'rr1DestinationCidrBlock'
    -> State ReplaceRoute a
    -> m ReplaceRouteResponse
replaceRoute p1 p2 s =
    send $ (mkReplaceRoute p1 p2) &~ s

replaceRouteCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'rr1RouteTableId'
    -> Text -- ^ 'rr1DestinationCidrBlock'
    -> State ReplaceRoute a
    -> m (Either ServiceErr ReplaceRouteResponse)
replaceRouteCatch p1 p2 s =
    sendCatch $ (mkReplaceRoute p1 p2) &~ s

-- $ReplaceRouteTableAssociation
-- Changes the route table associated with a given subnet in a VPC. After the
-- operation completes, the subnet uses the routes in the new route table it's
-- associated with. For more information about route tables, see Route Tables
-- in the Amazon Virtual Private Cloud User Guide. You can also use
-- ReplaceRouteTableAssociation to change which table is the main route table
-- in the VPC. You just specify the main route table's association ID and the
-- route table to be the new main route table. Example This example starts
-- with a route table associated with a subnet, and a corresponding
-- association ID rtbassoc-f8ad4891. You want to associate a different route
-- table (table rtb-f9ad4890) to the subnet. The result is a new association
-- ID representing the new association.
-- https://ec2.amazonaws.com/?Action=ReplaceRouteTableAssociation
-- &amp;AssociationId=rtbassoc-f8ad4891 &amp;RouteTableId=rtb-f9ad4890
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE rtbassoc-faad4893.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ReplaceRouteTableAssociation'

replaceRouteTableAssociation :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => Text -- ^ 'rrtaAssociationId'
    -> Text -- ^ 'rrtaRouteTableId'
    -> State ReplaceRouteTableAssociation a
    -> m ReplaceRouteTableAssociationResponse
replaceRouteTableAssociation p1 p2 s =
    send $ (mkReplaceRouteTableAssociation p1 p2) &~ s

replaceRouteTableAssociationCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'rrtaAssociationId'
    -> Text -- ^ 'rrtaRouteTableId'
    -> State ReplaceRouteTableAssociation a
    -> m (Either ServiceErr ReplaceRouteTableAssociationResponse)
replaceRouteTableAssociationCatch p1 p2 s =
    sendCatch $ (mkReplaceRouteTableAssociation p1 p2) &~ s

-- $ReportInstanceStatus
-- Submits feedback about the status of an instance. The instance must be in
-- the running state. If your experience with the instance differs from the
-- instance status returned by DescribeInstanceStatus, use
-- ReportInstanceStatus to report your experience with the instance. Amazon
-- EC2 collects this information to improve the accuracy of status checks.
-- Example 1 This example reports instance health state for two instances.
-- https://ec2.amazonaws.com/?Action=ReportInstanceStatus &amp;Status=impaired
-- &amp;InstanceId.0=i-9440effb &amp;InstanceId.1=i-0cf27c63 &amp;AUTHPARAMS
-- Example 2 This example reports instance health state for two instances with
-- reason codes. https://ec2.amazonaws.com/?Action=ReportInstanceStatus
-- &amp;Description=Description+of+my+issue. &amp;Status=impaired
-- &amp;InstanceId.0=i-9440effb &amp;InstanceId.1=i-0cf27c63
-- &amp;ReasonCode.0=instance-performance-network
-- &amp;ReasonCode.1=instance-performance-disk &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ReportInstanceStatus'

reportInstanceStatus :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => [Text] -- ^ 'risInstances'
    -> ReportStatusType -- ^ 'risStatus'
    -> [ReportInstanceReasonCodes] -- ^ 'risReasonCodes'
    -> State ReportInstanceStatus a
    -> m ReportInstanceStatusResponse
reportInstanceStatus p1 p2 p5 s =
    send $ (mkReportInstanceStatus p1 p2 p5) &~ s

reportInstanceStatusCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => [Text] -- ^ 'risInstances'
    -> ReportStatusType -- ^ 'risStatus'
    -> [ReportInstanceReasonCodes] -- ^ 'risReasonCodes'
    -> State ReportInstanceStatus a
    -> m (Either ServiceErr ReportInstanceStatusResponse)
reportInstanceStatusCatch p1 p2 p5 s =
    sendCatch $ (mkReportInstanceStatus p1 p2 p5) &~ s

-- $RequestSpotInstances
-- Creates a Spot Instance request. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current Spot Instance requests. For
-- more information about Spot Instances, see Spot Instances in the Amazon
-- Elastic Compute Cloud User Guide. Example This example creates a Spot
-- Instance request for two m1.small instances and associates an IAM instance
-- profile called s3access with them.
-- https://ec2.amazonaws.com/?Action=RequestSpotInstances &amp;SpotPrice=0.50
-- &amp;InstanceCount=2 &amp;Type=one-time
-- &amp;AvailabilityZoneGroup=MyAzGroup
-- &amp;LaunchSpecification.ImageId=ami-1a2b3c4d
-- &amp;LaunchSpecification.KeyName=my-key-pair
-- &amp;LaunchSpecification.SecurityGroup.1=websrv
-- &amp;LaunchSpecification.InstanceType=m1.small
-- &amp;LaunchSpecification.IamInstanceProfile.Name=s3access &amp;AUTHPARAMS
-- &lt;RequestSpotInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- sir-1a2b3c4d 0.5 one-time open pending-evaluation YYYY-MM-DDTHH:MM:SS.000Z
-- Your Spot request has been submitted for review, and is pending evaluation.
-- MyAzGroup ami-1a2b3c4d my-key-pair sg-1a2b3c4d websrv m1.small false false
-- YYYY-MM-DDTHH:MM:SS.000Z Linux/UNIX.
--
-- See: 'Network.AWS.EC2.V2014_06_15.RequestSpotInstances'

requestSpotInstances :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'rsiSpotPrice'
    -> State RequestSpotInstances a
    -> m RequestSpotInstancesResponse
requestSpotInstances p1 s =
    send $ (mkRequestSpotInstances p1) &~ s

requestSpotInstancesCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'rsiSpotPrice'
    -> State RequestSpotInstances a
    -> m (Either ServiceErr RequestSpotInstancesResponse)
requestSpotInstancesCatch p1 s =
    sendCatch $ (mkRequestSpotInstances p1) &~ s

-- $ResetImageAttribute
-- Resets an attribute of an AMI to its default value. Example This example
-- resets the launchPermission attribute for the specified AMI.
-- https://ec2.amazonaws.com/?Action=ResetImageAttribute
-- &amp;ImageId=ami-61a54008 &amp;Attribute=launchPermission &amp;AUTHPARAMS
-- &lt;ResetImageAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ResetImageAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ResetImageAttribute'

resetImageAttribute :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'riaImageId'
    -> ResetImageAttributeName -- ^ 'riaAttribute'
    -> State ResetImageAttribute a
    -> m ResetImageAttributeResponse
resetImageAttribute p1 p2 s =
    send $ (mkResetImageAttribute p1 p2) &~ s

resetImageAttributeCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'riaImageId'
    -> ResetImageAttributeName -- ^ 'riaAttribute'
    -> State ResetImageAttribute a
    -> m (Either ServiceErr ResetImageAttributeResponse)
resetImageAttributeCatch p1 p2 s =
    sendCatch $ (mkResetImageAttribute p1 p2) &~ s

-- $ResetInstanceAttribute
-- Resets an attribute of an instance to its default value. To reset the
-- kernel or RAM disk, the instance must be in a stopped state. To reset the
-- SourceDestCheck, the instance can be either running or stopped. The
-- SourceDestCheck attribute controls whether source/destination checking is
-- enabled. The default value is true, which means checking is enabled. This
-- value must be false for a NAT instance to perform NAT. For more
-- information, see NAT Instances in the Amazon Virtual Private Cloud User
-- Guide. Example This example resets the sourceDestCheck attribute.
-- https://ec2.amazonaws.com/?Action=ResetInstanceAttribute
-- &amp;InstanceId=i-1a2b3c4d &amp;Attribute=sourceDestCheck &amp;AUTHPARAMS
-- &lt;ResetInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ResetInstanceAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ResetInstanceAttribute'

resetInstanceAttribute :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'ria1InstanceId'
    -> InstanceAttributeName -- ^ 'ria1Attribute'
    -> State ResetInstanceAttribute a
    -> m ResetInstanceAttributeResponse
resetInstanceAttribute p1 p2 s =
    send $ (mkResetInstanceAttribute p1 p2) &~ s

resetInstanceAttributeCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'ria1InstanceId'
    -> InstanceAttributeName -- ^ 'ria1Attribute'
    -> State ResetInstanceAttribute a
    -> m (Either ServiceErr ResetInstanceAttributeResponse)
resetInstanceAttributeCatch p1 p2 s =
    sendCatch $ (mkResetInstanceAttribute p1 p2) &~ s

-- $ResetNetworkInterfaceAttribute
-- Resets a network interface attribute. You can specify only one attribute at
-- a time. Example This example resets the sourceDestCheck attribute for the
-- specified network interface.
-- https://ec2.amazonaws.com/?Action=ResetNetworkInterfaceAttribute
-- &amp;NetworkInterfaceId=eni-ffda3197 &amp;Attribute=sourceDestCheck
-- &amp;AUTHPARAMS &lt;ResetNetworkInterfaceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;5187642e-3f16-44a3-b05f-24c3848b5162&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/ResetNetworkInterfaceAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ResetNetworkInterfaceAttribute'

resetNetworkInterfaceAttribute :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'rniaNetworkInterfaceId'
    -> State ResetNetworkInterfaceAttribute a
    -> m ResetNetworkInterfaceAttributeResponse
resetNetworkInterfaceAttribute p1 s =
    send $ (mkResetNetworkInterfaceAttribute p1) &~ s

resetNetworkInterfaceAttributeCatch :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadReader Env m
                                       )
    => Text -- ^ 'rniaNetworkInterfaceId'
    -> State ResetNetworkInterfaceAttribute a
    -> m (Either ServiceErr ResetNetworkInterfaceAttributeResponse)
resetNetworkInterfaceAttributeCatch p1 s =
    sendCatch $ (mkResetNetworkInterfaceAttribute p1) &~ s

-- $ResetSnapshotAttribute
-- Resets permission settings for the specified snapshot. For more information
-- on modifying snapshot permissions, see Sharing Snapshots in the Amazon
-- Elastic Compute Cloud User Guide. Example This example resets the
-- permissions for snap-1a2b3c4d, making it a private snapshot that can only
-- be used by the account that created it.
-- https://ec2.amazonaws.com/?Action=ResetSnapshotAttribute
-- &amp;SnapshotId=snap-1a2b3c4d &amp;Attribute=createVolumePermission
-- &amp;AUTHPARAMS &lt;ResetSnapshotAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ResetSnapshotAttributeResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.ResetSnapshotAttribute'

resetSnapshotAttribute :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'rsaSnapshotId'
    -> SnapshotAttributeName -- ^ 'rsaAttribute'
    -> State ResetSnapshotAttribute a
    -> m ResetSnapshotAttributeResponse
resetSnapshotAttribute p1 p2 s =
    send $ (mkResetSnapshotAttribute p1 p2) &~ s

resetSnapshotAttributeCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'rsaSnapshotId'
    -> SnapshotAttributeName -- ^ 'rsaAttribute'
    -> State ResetSnapshotAttribute a
    -> m (Either ServiceErr ResetSnapshotAttributeResponse)
resetSnapshotAttributeCatch p1 p2 s =
    sendCatch $ (mkResetSnapshotAttribute p1 p2) &~ s

-- $RevokeSecurityGroupEgress
-- Removes one or more egress rules from a security group for EC2-VPC. The
-- values that you specify in the revoke request (for example, ports) must
-- match the existing rule's values for the rule to be revoked. Each rule
-- consists of the protocol and the CIDR range or source security group. For
-- the TCP and UDP protocols, you must also specify the destination port or
-- range of ports. For the ICMP protocol, you must also specify the ICMP type
-- and code. Rule changes are propagated to instances within the security
-- group as quickly as possible. However, a small delay might occur. Example 1
-- This example revokes the access that the specified security group has to
-- the 205.192.0.0/16 and 205.159.0.0/16 address ranges on TCP port 80.
-- https://ec2.amazonaws.com/?Action=RevokeSecurityGroupEgress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=205.192.0.0/16
-- &amp;IpPermissions.1.IpRanges.2.CidrIp=205.159.0.0/16 &amp;AUTHPARAMS
-- Example 2 This example revokes the access that the specified security group
-- has to the security group with the ID sg-9a8d7f5c on TCP port 1433.
-- https://ec2.amazonaws.com/?Action=RevokeSecurityGroupEgress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=1433 &amp;IpPermissions.1.ToPort=1433
-- &amp;IpPermissions.1.Groups.1.GroupId=sg-9a8d7f5c &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.RevokeSecurityGroupEgress'

revokeSecurityGroupEgress :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'rsgeGroupId'
    -> State RevokeSecurityGroupEgress a
    -> m RevokeSecurityGroupEgressResponse
revokeSecurityGroupEgress p1 s =
    send $ (mkRevokeSecurityGroupEgress p1) &~ s

revokeSecurityGroupEgressCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'rsgeGroupId'
    -> State RevokeSecurityGroupEgress a
    -> m (Either ServiceErr RevokeSecurityGroupEgressResponse)
revokeSecurityGroupEgressCatch p1 s =
    sendCatch $ (mkRevokeSecurityGroupEgress p1) &~ s

-- $RevokeSecurityGroupIngress
-- Removes one or more ingress rules from a security group. The values that
-- you specify in the revoke request (for example, ports) must match the
-- existing rule's values for the rule to be removed. Each rule consists of
-- the protocol and the CIDR range or source security group. For the TCP and
-- UDP protocols, you must also specify the destination port or range of
-- ports. For the ICMP protocol, you must also specify the ICMP type and code.
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur. Example This
-- example revokes TCP port 80 access from the 205.192.0.0/16 address range
-- for the security group named websrv. If the security group is for a VPC,
-- specify the ID of the security group instead of the name.
-- https://ec2.amazonaws.com/?Action=RevokeSecurityGroupIngress
-- &amp;GroupName=websrv &amp;IpProtocol=tcp &amp;FromPort=80 &amp;ToPort=80
-- &amp;CidrIp=205.192.0.0/16 &amp;AUTHPARAMS
-- &lt;RevokeSecurityGroupIngressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/RevokeSecurityGroupIngressResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.RevokeSecurityGroupIngress'

revokeSecurityGroupIngress :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => State RevokeSecurityGroupIngress a
    -> m RevokeSecurityGroupIngressResponse
revokeSecurityGroupIngress s =
    send (mkRevokeSecurityGroupIngress &~ s)

revokeSecurityGroupIngressCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => State RevokeSecurityGroupIngress a
    -> m (Either ServiceErr RevokeSecurityGroupIngressResponse)
revokeSecurityGroupIngressCatch s =
    sendCatch (mkRevokeSecurityGroupIngress &~ s)

-- $RunInstances
-- Launches the specified number of instances using an AMI for which you have
-- permissions. When you launch an instance, it enters the pending state.
-- After the instance is ready for you, it enters the running state. To check
-- the state of your instance, call DescribeInstances. If you don't specify a
-- security group when launching an instance, Amazon EC2 uses the default
-- security group. For more information, see Security Groups in the Amazon
-- Elastic Compute Cloud User Guide. Linux instances have access to the public
-- key of the key pair at boot. You can use this key to provide secure access
-- to the instance. Amazon EC2 public images use this feature to provide
-- secure access without passwords. For more information, see Key Pairs in the
-- Amazon Elastic Compute Cloud User Guide. You can provide optional user data
-- when launching an instance. For more information, see Instance Metadata in
-- the Amazon Elastic Compute Cloud User Guide. If any of the AMIs have a
-- product code attached for which the user has not subscribed, RunInstances
-- fails. T2 instance types can only be launched into a VPC. If you do not
-- have a default VPC, or if you do not specify a subnet ID in the request,
-- RunInstances fails. For more information about troubleshooting, see What To
-- Do If An Instance Immediately Terminates, and Troubleshooting Connecting to
-- Your Instance in the Amazon Elastic Compute Cloud User Guide. This example
-- launches three instances using the AMI with the ID ami-60a54009.
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-60a54009
-- &amp;MaxCount=3 &amp;MinCount=1 &amp;KeyName=my-key-pair
-- &amp;Placement.AvailabilityZone=us-east-1d &amp;AUTHPARAMS This example
-- launches an m1.small instance into a subnet. Because no network interface
-- is specified, the default network interface is used.
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-31814f58
-- &amp;InstanceType=m1.small &amp;MaxCount=1 &amp;MinCount=1
-- &amp;KeyName=my-key-pair &amp;SubnetId=subnet-b2a249da &amp;AUTHPARAMS This
-- example launches an m1.large instance into a subnet. The network interface
-- specifies a primary private IP address of 10.0.2.106 and two secondary
-- private IP addresses (10.0.2.107 and 10.0.2.108).
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-beb0caec
-- &amp;InstanceType=m1.large &amp;MaxCount=1 &amp;MinCount=1
-- &amp;KeyName=my-key-pair &amp;NetworkInterface.0.DeviceIndex=0
-- &amp;NetworkInterface.0.PrivateIpAddresses.0.Primary=true
-- &amp;NetworkInterface.0.PrivateIpAddresses.0.PrivateIpAddress=10.0.2.106
-- &amp;NetworkInterface.0.PrivateIpAddresses.1.Primary=false
-- &amp;NetworkInterface.0.PrivateIpAddresses.1.PrivateIpAddress=10.0.2.107
-- &amp;NetworkInterface.0.PrivateIpAddresses.2.Primary=false
-- &amp;NetworkInterface.0.PrivateIpAddresses.2.PrivateIpAddress=10.0.2.108
-- &amp;NetworkInterface.0.SubnetId=subnet-a61dafcf &amp;AUTHPARAMS This
-- example launches a Dedicated Instance into the specified subnet.
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-2a1fec43
-- &amp;MaxCount=1 &amp;MinCount=1 &amp;KeyName=my-key-pair
-- &amp;SubnetId=subnet-dea63cb7 &amp;Placement.Tenancy=dedicated
-- &amp;AUTHPARAMS This request launches an instance into a nondefault subnet,
-- and requests a public IP address for a new network interface with the
-- device index of 0. https://ec2.amazonaws.com/?Action=RunInstances
-- &amp;ImageId=ami-1a2b3c4d &amp;MaxCount=1 &amp;MinCount=1
-- &amp;NetworkInterface.0.DeviceIndex=0
-- &amp;NetworkInterface.0.AssociatePublicIpAddress=true
-- &amp;NetworkInterface.0.SubnetId=subnet-1a2b3c4d &amp;AUTHPARAMS This
-- request launches an m1.large instance with a block device mapping. There
-- are two instance store volumes mapped to /dev/sdc and /dev/sdd, and a 100
-- GB Amazon EBS volume mapped to /dev/sdf.
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-1a2b3c4d
-- &amp;InstanceType=m1.large
-- &amp;BlockDeviceMapping.1.DeviceName=%2Fdev%2Fsdc
-- &amp;BlockDeviceMapping.1.VirtualName=ephemeral0
-- &amp;BlockDeviceMapping.2.DeviceName=%2Fdev%2Fsdd
-- &amp;BlockDeviceMapping.2.VirtualName=ephemeral1
-- &amp;BlockDeviceMapping.3.DeviceName=%2Fdev%2Fsdf
-- &amp;BlockDeviceMapping.3.Ebs.DeleteOnTermination=false
-- &amp;BlockDeviceMapping.3.Ebs.VolumeSize=100 &amp;EbsOptimized=false
-- &amp;MinCount=1 &amp;MaxCount=1 &amp;DisableApiTermination=false
-- &amp;Monitoring.Enabled=false &amp;AUTHPARAMS.
--
-- See: 'Network.AWS.EC2.V2014_06_15.RunInstances'

runInstances :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'ri3ImageId'
    -> Integer -- ^ 'ri3MinCount'
    -> Integer -- ^ 'ri3MaxCount'
    -> State RunInstances a
    -> m RunInstancesResponse
runInstances p1 p2 p3 s =
    send $ (mkRunInstances p1 p2 p3) &~ s

runInstancesCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'ri3ImageId'
    -> Integer -- ^ 'ri3MinCount'
    -> Integer -- ^ 'ri3MaxCount'
    -> State RunInstances a
    -> m (Either ServiceErr RunInstancesResponse)
runInstancesCatch p1 p2 p3 s =
    sendCatch $ (mkRunInstances p1 p2 p3) &~ s

-- $StartInstances
-- Starts an Amazon EBS-backed AMI that you've previously stopped. Instances
-- that use Amazon EBS volumes as their root devices can be quickly stopped
-- and started. When an instance is stopped, the compute resources are
-- released and you are not billed for hourly instance usage. However, your
-- root partition Amazon EBS volume remains, continues to persist your data,
-- and you are charged for Amazon EBS volume usage. You can restart your
-- instance at any time. Each time you transition an instance from stopped to
-- started, Amazon EC2 charges a full instance hour, even if transitions
-- happen multiple times within a single hour. Before stopping an instance,
-- make sure it is in a state from which it can be restarted. Stopping an
-- instance does not preserve data stored in RAM. Performing this operation on
-- an instance that uses an instance store as its root device returns an
-- error. For more information, see Stopping Instances in the Amazon Elastic
-- Compute Cloud User Guide. Example This example starts the specified
-- instance. https://ec2.amazonaws.com/?Action=StartInstances
-- &amp;InstanceId.1=i-10a64379 &amp;AUTHPARAMS &lt;StartInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt; &lt;currentState&gt;
-- &lt;code&gt;0&lt;/code&gt; &lt;name&gt;pending&lt;/name&gt;
-- &lt;/currentState&gt; &lt;previousState&gt; &lt;code&gt;80&lt;/code&gt;
-- &lt;name&gt;stopped&lt;/name&gt; &lt;/previousState&gt; &lt;/item&gt;
-- &lt;/instancesSet&gt; &lt;/StartInstancesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.StartInstances'

startInstances :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => [Text] -- ^ 'siInstanceIds'
    -> State StartInstances a
    -> m StartInstancesResponse
startInstances p1 s =
    send $ (mkStartInstances p1) &~ s

startInstancesCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => [Text] -- ^ 'siInstanceIds'
    -> State StartInstances a
    -> m (Either ServiceErr StartInstancesResponse)
startInstancesCatch p1 s =
    sendCatch $ (mkStartInstances p1) &~ s

-- $StopInstances
-- Stops an Amazon EBS-backed instance. Each time you transition an instance
-- from stopped to started, Amazon EC2 charges a full instance hour, even if
-- transitions happen multiple times within a single hour. You can't start or
-- stop Spot Instances. Instances that use Amazon EBS volumes as their root
-- devices can be quickly stopped and started. When an instance is stopped,
-- the compute resources are released and you are not billed for hourly
-- instance usage. However, your root partition Amazon EBS volume remains,
-- continues to persist your data, and you are charged for Amazon EBS volume
-- usage. You can restart your instance at any time. Before stopping an
-- instance, make sure it is in a state from which it can be restarted.
-- Stopping an instance does not preserve data stored in RAM. Performing this
-- operation on an instance that uses an instance store as its root device
-- returns an error. You can stop, start, and terminate EBS-backed instances.
-- You can only terminate instance store-backed instances. What happens to an
-- instance differs if you stop it or terminate it. For example, when you stop
-- an instance, the root device and any other devices attached to the instance
-- persist. When you terminate an instance, the root device and any other
-- devices attached during the instance launch are automatically deleted. For
-- more information about the differences between stopping and terminating
-- instances, see Instance Lifecycle in the Amazon Elastic Compute Cloud User
-- Guide. For more information about troubleshooting, see Troubleshooting
-- Stopping Your Instance in the Amazon Elastic Compute Cloud User Guide.
-- Example This example stops the specified instance.
-- https://ec2.amazonaws.com/?Action=StopInstances
-- &amp;InstanceId.1=i-10a64379 &amp;AUTHPARAMS &lt;StopInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt; &lt;currentState&gt;
-- &lt;code&gt;64&lt;/code&gt; &lt;name&gt;stopping&lt;/name&gt;
-- &lt;/currentState&gt; &lt;previousState&gt; &lt;code&gt;16&lt;/code&gt;
-- &lt;name&gt;running&lt;/name&gt; &lt;/previousState&gt;
-- &lt;/instancesSet&gt; &lt;/StopInstancesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.StopInstances'

stopInstances :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => [Text] -- ^ 'si1InstanceIds'
    -> State StopInstances a
    -> m StopInstancesResponse
stopInstances p1 s =
    send $ (mkStopInstances p1) &~ s

stopInstancesCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => [Text] -- ^ 'si1InstanceIds'
    -> State StopInstances a
    -> m (Either ServiceErr StopInstancesResponse)
stopInstancesCatch p1 s =
    sendCatch $ (mkStopInstances p1) &~ s

-- $TerminateInstances
-- Shuts down one or more instances. This operation is idempotent; if you
-- terminate an instance more than once, each call succeeds. Terminated
-- instances remain visible after termination (for approximately one hour). By
-- default, Amazon EC2 deletes all Amazon EBS volumes that were attached when
-- the instance launched. Volumes attached after instance launch continue
-- running. You can stop, start, and terminate EBS-backed instances. You can
-- only terminate instance store-backed instances. What happens to an instance
-- differs if you stop it or terminate it. For example, when you stop an
-- instance, the root device and any other devices attached to the instance
-- persist. When you terminate an instance, the root device and any other
-- devices attached during the instance launch are automatically deleted. For
-- more information about the differences between stopping and terminating
-- instances, see Instance Lifecycle in the Amazon Elastic Compute Cloud User
-- Guide. For more information about troubleshooting, see Troubleshooting
-- Terminating Your Instance in the Amazon Elastic Compute Cloud User Guide.
-- Example This example terminates the specified instance.
-- https://ec2.amazonaws.com/?Action=TerminateInstances
-- &amp;InstanceId.1=i-3ea74257 &amp;AUTHPARAMS &lt;TerminateInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-3ea74257&lt;/instanceId&gt; &lt;currentState&gt;
-- &lt;code&gt;32&lt;/code&gt; &lt;name&gt;shutting-down&lt;/name&gt;
-- &lt;/currentState&gt; &lt;previousState&gt; &lt;code&gt;16&lt;/code&gt;
-- &lt;name&gt;running&lt;/name&gt; &lt;/previousState&gt; &lt;/item&gt;
-- &lt;/instancesSet&gt; &lt;/TerminateInstancesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.TerminateInstances'

terminateInstances :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => [Text] -- ^ 'tiInstanceIds'
    -> State TerminateInstances a
    -> m TerminateInstancesResponse
terminateInstances p1 s =
    send $ (mkTerminateInstances p1) &~ s

terminateInstancesCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => [Text] -- ^ 'tiInstanceIds'
    -> State TerminateInstances a
    -> m (Either ServiceErr TerminateInstancesResponse)
terminateInstancesCatch p1 s =
    sendCatch $ (mkTerminateInstances p1) &~ s

-- $UnassignPrivateIpAddresses
-- Unassigns one or more secondary private IP addresses from a network
-- interface. Example The following example unassigns two secondary private IP
-- addresses from the specified network interface.
-- https://ec2.amazonaws.com/?Action=UnassignPrivateIpAddresses
-- &amp;NetworkInterfaceId=eni-197d9972 &amp;PrivateIpAddress.0=10.0.2.60
-- &amp;PrivateIpAddress.1=10.0.2.65 &amp;AUTHPARAMS
-- &lt;UnassignPrivateIpAddresses
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/UnassignPrivateIpAddresses&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.UnassignPrivateIpAddresses'

unassignPrivateIpAddresses :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'upiaNetworkInterfaceId'
    -> [Text] -- ^ 'upiaPrivateIpAddresses'
    -> State UnassignPrivateIpAddresses a
    -> m UnassignPrivateIpAddressesResponse
unassignPrivateIpAddresses p1 p2 s =
    send $ (mkUnassignPrivateIpAddresses p1 p2) &~ s

unassignPrivateIpAddressesCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'upiaNetworkInterfaceId'
    -> [Text] -- ^ 'upiaPrivateIpAddresses'
    -> State UnassignPrivateIpAddresses a
    -> m (Either ServiceErr UnassignPrivateIpAddressesResponse)
unassignPrivateIpAddressesCatch p1 p2 s =
    sendCatch $ (mkUnassignPrivateIpAddresses p1 p2) &~ s

-- $UnmonitorInstances
-- Disables monitoring for a running instance. For more information about
-- monitoring instances, see Monitoring Your Instances and Volumes in the
-- Amazon Elastic Compute Cloud User Guide. Example This example disables
-- monitoring for the specified instances.
-- https://ec2.amazonaws.com/?Action=UnmonitorInstances
-- &amp;InstanceId.1=i-43a4412a &amp;InstanceId.2=i-23a3397d &amp;AUTHPARAMS
-- &lt;UnmonitorInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-43a4412a&lt;/instanceId&gt; &lt;monitoring&gt;
-- &lt;state&gt;disabled&lt;/state&gt; &lt;/monitoring&gt; &lt;/item&gt;
-- &lt;item&gt; &lt;instanceId&gt;i-23a3397d&lt;/instanceId&gt;
-- &lt;monitoring&gt; &lt;state&gt;disabled&lt;/state&gt; &lt;/monitoring&gt;
-- &lt;/item&gt; &lt;/instancesSet&gt; &lt;/UnmonitorInstancesResponse&gt;.
--
-- See: 'Network.AWS.EC2.V2014_06_15.UnmonitorInstances'

unmonitorInstances :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => [Text] -- ^ 'uiInstanceIds'
    -> State UnmonitorInstances a
    -> m UnmonitorInstancesResponse
unmonitorInstances p1 s =
    send $ (mkUnmonitorInstances p1) &~ s

unmonitorInstancesCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => [Text] -- ^ 'uiInstanceIds'
    -> State UnmonitorInstances a
    -> m (Either ServiceErr UnmonitorInstancesResponse)
unmonitorInstancesCatch p1 s =
    sendCatch $ (mkUnmonitorInstances p1) &~ s
