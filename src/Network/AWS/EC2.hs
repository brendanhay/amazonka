{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic Compute Cloud provides resizable computing
-- capacity in the Amazon Web Services cloud.
module Network.AWS.EC2
   (
   -- * EC2 API Version
     ec2Version

   -- * Actions
   -- ** AllocateAddress
   , AllocateAddress                            (..)
   , AllocateAddressResponse                    (..)

   -- ** AssignPrivateIpAddresses
   , AssignPrivateIpAddresses                   (..)
   , AssignPrivateIpAddressesResponse           (..)

   -- ** AssociateAddress
   , AssociateAddress                           (..)
   , AssociateAddressResponse                   (..)

   -- ** AssociateDhcpOptions
   , AssociateDhcpOptions                       (..)
   , AssociateDhcpOptionsResponse               (..)

   -- ** AssociateRouteTable
   , AssociateRouteTable                        (..)
   , AssociateRouteTableResponse                (..)

   -- ** AttachInternetGateway
   , AttachInternetGateway                      (..)
   , AttachInternetGatewayResponse              (..)

   -- ** AttachNetworkInterface
   , AttachNetworkInterface                     (..)
   , AttachNetworkInterfaceResponse             (..)

   -- ** AttachVolume
   , AttachVolume                               (..)
   , AttachVolumeResponse                       (..)

   -- ** AttachVpnGateway
   , AttachVpnGateway                           (..)
   , AttachVpnGatewayResponse                   (..)

   -- ** AuthorizeSecurityGroupEgress
   , AuthorizeSecurityGroupEgress               (..)
   , AuthorizeSecurityGroupEgressResponse       (..)

   -- ** AuthorizeSecurityGroupIngress
   , AuthorizeSecurityGroupIngress              (..)
   , AuthorizeSecurityGroupIngressResponse      (..)

   -- ** BundleInstance
   , BundleInstance                             (..)
   , BundleInstanceResponse                     (..)

   -- ** CancelBundleTask
   , CancelBundleTask                           (..)
   , CancelBundleTaskResponse                   (..)

   -- ** CancelConversionTask
   , CancelConversionTask                       (..)
   , CancelConversionTaskResponse               (..)

   -- ** CancelExportTask
   , CancelExportTask                           (..)
   , CancelExportTaskResponse                   (..)

   -- ** CancelReservedInstancesListing
   , CancelReservedInstancesListing             (..)
   , CancelReservedInstancesListingResponse     (..)

   -- ** CancelSpotInstanceRequests
   , CancelSpotInstanceRequests                 (..)
   , CancelSpotInstanceRequestsResponse         (..)

   -- ** ConfirmProductInstance
   , ConfirmProductInstance                     (..)
   , ConfirmProductInstanceResponse             (..)

   -- ** CopyImage
   , CopyImage                                  (..)
   , CopyImageResponse                          (..)

   -- ** CopySnapshot
   , CopySnapshot                               (..)
   , CopySnapshotResponse                       (..)

   -- ** CreateCustomerGateway
   , CreateCustomerGateway                      (..)
   , CreateCustomerGatewayResponse              (..)

   -- ** CreateDhcpOptions
   , CreateDhcpOptions                          (..)
   , CreateDhcpOptionsResponse                  (..)

   -- ** CreateImage
   , CreateImage                                (..)
   , CreateImageResponse                        (..)

   -- ** CreateInstanceExportTask
   , CreateInstanceExportTask                   (..)
   , CreateInstanceExportTaskResponse           (..)

   -- ** CreateInternetGateway
   , CreateInternetGateway                      (..)
   , CreateInternetGatewayResponse              (..)

   -- ** CreateKeyPair
   , CreateKeyPair                              (..)
   , CreateKeyPairResponse                      (..)

   -- ** CreateNetworkAcl
   , CreateNetworkAcl                           (..)
   , CreateNetworkAclResponse                   (..)

   -- ** CreateNetworkAclEntry
   , CreateNetworkAclEntry                      (..)
   , CreateNetworkAclEntryResponse              (..)

   -- ** CreateNetworkInterface
   , CreateNetworkInterface                     (..)
   , CreateNetworkInterfaceResponse             (..)

   -- ** CreatePlacementGroup
   , CreatePlacementGroup                       (..)
   , CreatePlacementGroupResponse               (..)

   -- ** CreateReservedInstancesListing
   , CreateReservedInstancesListing             (..)
   , CreateReservedInstancesListingResponse     (..)

   -- ** CreateRoute
   , CreateRoute                                (..)
   , CreateRouteResponse                        (..)

   -- ** CreateRouteTable
   , CreateRouteTable                           (..)
   , CreateRouteTableResponse                   (..)

   -- ** CreateSecurityGroup
   , CreateSecurityGroup                        (..)
   , CreateSecurityGroupResponse                (..)

   -- ** CreateSnapshot
   , CreateSnapshot                             (..)
   , CreateSnapshotResponse                     (..)

   -- ** CreateSpotDatafeedSubscription
   , CreateSpotDatafeedSubscription             (..)
   , CreateSpotDatafeedSubscriptionResponse     (..)

   -- ** CreateSubnet
   , CreateSubnet                               (..)
   , CreateSubnetResponse                       (..)

   -- ** CreateTags
   , CreateTags                                 (..)
   , CreateTagsResponse                         (..)

   -- ** CreateVolume
   , CreateVolume                               (..)
   , CreateVolumeResponse                       (..)

   -- ** CreateVpc
   , CreateVpc                                  (..)
   , CreateVpcResponse                          (..)

   -- ** CreateVpnConnection
   , CreateVpnConnection                        (..)
   , CreateVpnConnectionResponse                (..)

   -- ** CreateVpnConnectionRoute
   , CreateVpnConnectionRoute                   (..)
   , CreateVpnConnectionRouteResponse           (..)

   -- ** CreateVpnGateway
   , CreateVpnGateway                           (..)
   , CreateVpnGatewayResponse                   (..)

   -- ** DeleteCustomerGateway
   , DeleteCustomerGateway                      (..)
   , DeleteCustomerGatewayResponse              (..)

   -- ** DeleteDhcpOptions
   , DeleteDhcpOptions                          (..)
   , DeleteDhcpOptionsResponse                  (..)

   -- ** DeleteInternetGateway
   , DeleteInternetGateway                      (..)
   , DeleteInternetGatewayResponse              (..)

   -- ** DeleteKeyPair
   , DeleteKeyPair                              (..)
   , DeleteKeyPairResponse                      (..)

   -- ** DeleteNetworkAcl
   , DeleteNetworkAcl                           (..)
   , DeleteNetworkAclResponse                   (..)

   -- ** DeleteNetworkAclEntry
   , DeleteNetworkAclEntry                      (..)
   , DeleteNetworkAclEntryResponse              (..)

   -- ** DeleteNetworkInterface
   , DeleteNetworkInterface                     (..)
   , DeleteNetworkInterfaceResponse             (..)

   -- ** DeletePlacementGroup
   , DeletePlacementGroup                       (..)
   , DeletePlacementGroupResponse               (..)

   -- ** DeleteRoute
   , DeleteRoute                                (..)
   , DeleteRouteResponse                        (..)

   -- ** DeleteRouteTable
   , DeleteRouteTable                           (..)
   , DeleteRouteTableResponse                   (..)

   -- ** DeleteSecurityGroup
   , DeleteSecurityGroup                        (..)
   , DeleteSecurityGroupResponse                (..)

   -- ** DeleteSnapshot
   , DeleteSnapshot                             (..)
   , DeleteSnapshotResponse                     (..)

   -- ** DeleteSpotDatafeedSubscription
   , DeleteSpotDatafeedSubscription             (..)
   , DeleteSpotDatafeedSubscriptionResponse     (..)

   -- ** DeleteSubnet
   , DeleteSubnet                               (..)
   , DeleteSubnetResponse                       (..)

   -- ** DeleteTags
   , DeleteTags                                 (..)
   , DeleteTagsResponse                         (..)

   -- ** DeleteVolume
   , DeleteVolume                               (..)
   , DeleteVolumeResponse                       (..)

   -- ** DeleteVpc
   , DeleteVpc                                  (..)
   , DeleteVpcResponse                          (..)

   -- ** DeleteVpnConnection
   , DeleteVpnConnection                        (..)
   , DeleteVpnConnectionResponse                (..)

   -- ** DeleteVpnConnectionRoute
   , DeleteVpnConnectionRoute                   (..)
   , DeleteVpnConnectionRouteResponse           (..)

   -- ** DeleteVpnGateway
   , DeleteVpnGateway                           (..)
   , DeleteVpnGatewayResponse                   (..)

   -- ** DeregisterImage
   , DeregisterImage                            (..)
   , DeregisterImageResponse                    (..)

   -- ** DescribeAccountAttributes
   , DescribeAccountAttributes                  (..)
   , DescribeAccountAttributesResponse          (..)

   -- ** DescribeAddresses
   , DescribeAddresses                          (..)
   , DescribeAddressesResponse                  (..)

   -- ** DescribeAvailabilityZones
   , DescribeAvailabilityZones                  (..)
   , DescribeAvailabilityZonesResponse          (..)

   -- ** DescribeBundleTasks
   , DescribeBundleTasks                        (..)
   , DescribeBundleTasksResponse                (..)

   -- ** DescribeConversionTasks
   , DescribeConversionTasks                    (..)
   , DescribeConversionTasksResponse            (..)

   -- ** DescribeCustomerGateways
   , DescribeCustomerGateways                   (..)
   , DescribeCustomerGatewaysResponse           (..)

   -- ** DescribeDhcpOptions
   , DescribeDhcpOptions                        (..)
   , DescribeDhcpOptionsResponse                (..)

   -- ** DescribeExportTasks
   , DescribeExportTasks                        (..)
   , DescribeExportTasksResponse                (..)

   -- ** DescribeImageAttribute
   , DescribeImageAttribute                     (..)
   , DescribeImageAttributeResponse             (..)

   -- ** DescribeImages
   , DescribeImages                             (..)
   , DescribeImagesResponse                     (..)

   -- ** DescribeInstanceAttribute
   , DescribeInstanceAttribute                  (..)
   , DescribeInstanceAttributeResponse          (..)

   -- ** DescribeInstances
   , DescribeInstances                          (..)
   , DescribeInstancesResponse                  (..)

   -- ** DescribeInstanceStatus
   , DescribeInstanceStatus                     (..)
   , DescribeInstanceStatusResponse             (..)

   -- ** DescribeInternetGateways
   , DescribeInternetGateways                   (..)
   , DescribeInternetGatewaysResponse           (..)

   -- ** DescribeKeyPairs
   , DescribeKeyPairs                           (..)
   , DescribeKeyPairsResponse                   (..)

   -- ** DescribeNetworkAcls
   , DescribeNetworkAcls                        (..)
   , DescribeNetworkAclsResponse                (..)

   -- ** DescribeNetworkInterfaceAttribute
   , DescribeNetworkInterfaceAttribute          (..)
   , DescribeNetworkInterfaceAttributeResponse  (..)

   -- ** DescribeNetworkInterfaces
   , DescribeNetworkInterfaces                  (..)
   , DescribeNetworkInterfacesResponse          (..)

   -- ** DescribePlacementGroups
   , DescribePlacementGroups                    (..)
   , DescribePlacementGroupsResponse            (..)

   -- ** DescribeRegions
   , DescribeRegions                            (..)
   , DescribeRegionsResponse                    (..)

   -- ** DescribeReservedInstances
   , DescribeReservedInstances                  (..)
   , DescribeReservedInstancesResponse          (..)

   -- ** DescribeReservedInstancesListings
   , DescribeReservedInstancesListings          (..)
   , DescribeReservedInstancesListingsResponse  (..)

   -- ** DescribeReservedInstancesOfferings
   , DescribeReservedInstancesOfferings         (..)
   , DescribeReservedInstancesOfferingsResponse (..)

   -- ** DescribeRouteTables
   , DescribeRouteTables                        (..)
   , DescribeRouteTablesResponse                (..)

   -- ** DescribeSecurityGroups
   , DescribeSecurityGroups                     (..)
   , DescribeSecurityGroupsResponse             (..)

   -- ** DescribeSnapshotAttribute
   , DescribeSnapshotAttribute                  (..)
   , DescribeSnapshotAttributeResponse          (..)

   -- ** DescribeSnapshots
   , DescribeSnapshots                          (..)
   , DescribeSnapshotsResponse                  (..)

   -- ** DescribeSpotDatafeedSubscription
   , DescribeSpotDatafeedSubscription           (..)
   , DescribeSpotDatafeedSubscriptionResponse   (..)

   -- ** DescribeSpotInstanceRequests
   , DescribeSpotInstanceRequests               (..)
   , DescribeSpotInstanceRequestsResponse       (..)

   -- ** DescribeSpotPriceHistory
   , DescribeSpotPriceHistory                   (..)
   , DescribeSpotPriceHistoryResponse           (..)

   -- ** DescribeSubnets
   , DescribeSubnets                            (..)
   , DescribeSubnetsResponse                    (..)

   -- ** DescribeTags
   , DescribeTags                               (..)
   , DescribeTagsResponse                       (..)

   -- ** DescribeVolumeAttribute
   , DescribeVolumeAttribute                    (..)
   , DescribeVolumeAttributeResponse            (..)

   -- ** DescribeVolumes
   , DescribeVolumes                            (..)
   , DescribeVolumesResponse                    (..)

   -- ** DescribeVolumeStatus
   , DescribeVolumeStatus                       (..)
   , DescribeVolumeStatusResponse               (..)

   -- ** DescribeVpcAttribute
   , DescribeVpcAttribute                       (..)
   , DescribeVpcAttributeResponse               (..)

   -- ** DescribeVpcs
   , DescribeVpcs                               (..)
   , DescribeVpcsResponse                       (..)

   -- ** DescribeVpnConnections
   , DescribeVpnConnections                     (..)
   , DescribeVpnConnectionsResponse             (..)

   -- ** DescribeVpnGateways
   , DescribeVpnGateways                        (..)
   , DescribeVpnGatewaysResponse                (..)

   -- ** DetachInternetGateway
   , DetachInternetGateway                      (..)
   , DetachInternetGatewayResponse              (..)

   -- ** DetachNetworkInterface
   , DetachNetworkInterface                     (..)
   , DetachNetworkInterfaceResponse             (..)

   -- ** DetachVolume
   , DetachVolume                               (..)
   , DetachVolumeResponse                       (..)

   -- ** DetachVpnGateway
   , DetachVpnGateway                           (..)
   , DetachVpnGatewayResponse                   (..)

   -- ** DisableVgwRoutePropagation
   , DisableVgwRoutePropagation                 (..)
   , DisableVgwRoutePropagationResponse         (..)

   -- ** DisassociateAddress
   , DisassociateAddress                        (..)
   , DisassociateAddressResponse                (..)

   -- ** DisassociateRouteTable
   , DisassociateRouteTable                     (..)
   , DisassociateRouteTableResponse             (..)

   -- ** EnableVgwRoutePropagation
   , EnableVgwRoutePropagation                  (..)
   , EnableVgwRoutePropagationResponse          (..)

   -- ** EnableVolumeIO
   , EnableVolumeIO                             (..)
   , EnableVolumeIOResponse                     (..)

   -- ** GetConsoleOutput
   , GetConsoleOutput                           (..)
   , GetConsoleOutputResponse                   (..)

   -- ** GetPasswordData
   , GetPasswordData                            (..)
   , GetPasswordDataResponse                    (..)

   -- ** ImportInstance
   , ImportInstance                             (..)
   , ImportInstanceResponse                     (..)

   -- ** ImportKeyPair
   , ImportKeyPair                              (..)
   , ImportKeyPairResponse                      (..)

   -- ** ImportVolume
   , ImportVolume                               (..)
   , ImportVolumeResponse                       (..)

   -- ** ModifyImageAttribute
   , ModifyImageAttribute                       (..)
   , ModifyImageAttributeResponse               (..)

   -- ** ModifyInstanceAttribute
   , ModifyInstanceAttribute                    (..)
   , ModifyInstanceAttributeResponse            (..)

   -- ** ModifyNetworkInterfaceAttribute
   , ModifyNetworkInterfaceAttribute            (..)
   , ModifyNetworkInterfaceAttributeResponse    (..)

   -- ** ModifySnapshotAttribute
   , ModifySnapshotAttribute                    (..)
   , ModifySnapshotAttributeResponse            (..)

   -- ** ModifyVolumeAttribute
   , ModifyVolumeAttribute                      (..)
   , ModifyVolumeAttributeResponse              (..)

   -- ** ModifyVpcAttribute
   , ModifyVpcAttribute                         (..)
   , ModifyVpcAttributeResponse                 (..)

   -- ** MonitorInstances
   , MonitorInstances                           (..)
   , MonitorInstancesResponse                   (..)

   -- ** PurchaseReservedInstancesOffering
   , PurchaseReservedInstancesOffering          (..)
   , PurchaseReservedInstancesOfferingResponse  (..)

   -- ** RebootInstances
   , RebootInstances                            (..)
   , RebootInstancesResponse                    (..)

   -- ** RegisterImage
   , RegisterImage                              (..)
   , RegisterImageResponse                      (..)

   -- ** ReleaseAddress
   , ReleaseAddress                             (..)
   , ReleaseAddressResponse                     (..)

   -- ** ReplaceNetworkAclAssociation
   , ReplaceNetworkAclAssociation               (..)
   , ReplaceNetworkAclAssociationResponse       (..)

   -- ** ReplaceNetworkAclEntry
   , ReplaceNetworkAclEntry                     (..)
   , ReplaceNetworkAclEntryResponse             (..)

   -- ** ReplaceRoute
   , ReplaceRoute                               (..)
   , ReplaceRouteResponse                       (..)

   -- ** ReplaceRouteTableAssociation
   , ReplaceRouteTableAssociation               (..)
   , ReplaceRouteTableAssociationResponse       (..)

   -- ** ReportInstanceStatus
   , ReportInstanceStatus                       (..)
   , ReportInstanceStatusResponse               (..)

   -- ** RequestSpotInstances
   , RequestSpotInstances                       (..)
   , RequestSpotInstancesResponse               (..)

   -- ** ResetImageAttribute
   , ResetImageAttribute                        (..)
   , ResetImageAttributeResponse                (..)

   -- ** ResetInstanceAttribute
   , ResetInstanceAttribute                     (..)
   , ResetInstanceAttributeResponse             (..)

   -- ** ResetNetworkInterfaceAttribute
   , ResetNetworkInterfaceAttribute             (..)
   , ResetNetworkInterfaceAttributeResponse     (..)

   -- ** ResetSnapshotAttribute
   , ResetSnapshotAttribute                     (..)
   , ResetSnapshotAttributeResponse             (..)

   -- ** RevokeSecurityGroupEgress
   , RevokeSecurityGroupEgress                  (..)
   , RevokeSecurityGroupEgressResponse          (..)

   -- ** RevokeSecurityGroupIngress
   , RevokeSecurityGroupIngress                 (..)
   , RevokeSecurityGroupIngressResponse         (..)

   -- ** RunInstances
   , RunInstances                               (..)
   , RunInstancesResponse                       (..)

   -- ** StartInstances
   , StartInstances                             (..)
   , StartInstancesResponse                     (..)

   -- ** StopInstances
   , StopInstances                              (..)
   , StopInstancesResponse                      (..)

   -- ** TerminateInstances
   , TerminateInstances                         (..)
   , TerminateInstancesResponse                 (..)

   -- ** UnassignPrivateIpAddresses
   , UnassignPrivateIpAddresses                 (..)
   , UnassignPrivateIpAddressesResponse         (..)

   -- ** UnmonitorInstances
   , UnmonitorInstances                         (..)
   , UnmonitorInstancesResponse                 (..)

   -- * Data Types
   , module Network.AWS.EC2.Types
   ) where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Time
import Network.AWS.Internal
import Network.AWS.EC2.Types
import Network.Http.Client (Method(..))

data EC2

instance AWSService EC2 where
    service _ = awsService "ec2" ec2Version SigningVersion4

req :: IsQuery a => Method -> ByteString -> a -> RawRequest EC2 b
req meth act qry = (emptyRequest meth FormEncoded "/" Nothing)
    { rqAction = Just act
    , rqQuery  = toQuery qry
    }

--
-- Actions
--

-- | Acquires an Elastic IP address.An Elastic IP address is for use either in
-- the EC2-Classic platform or in a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AllocateAddress.html>

data AllocateAddress = AllocateAddress
    { aaDomain :: !ByteString
      -- ^ Set to vpc to allocate the address for use with instances in a
      -- VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery AllocateAddress

instance IsXML AllocateAddress where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AllocateAddress AllocateAddressResponse where
    request = req GET "AllocateAddress"

data AllocateAddressResponse = AllocateAddressResponse
    { aaRequestId    :: !ByteString
      -- ^ The ID of the request.
    , aaPublicIp     :: !ByteString
      -- ^ The Elastic IP address.
    , abDomain       :: !ByteString
      -- ^ Indicates whether this Elastic IP address is for use with
      -- instances in EC2-Classic (standard) or instances in a VPC (vpc).
    , abAllocationId :: !ByteString
      -- ^ [EC2-VPC] The ID that AWS assigns to represent the allocation of
      -- the Elastic IP address for use with a VPC.
    } deriving (Eq, Show, Generic)

instance IsXML AllocateAddressResponse where
    xmlPickler = withNS ec2NS

-- | Assigns one or more secondary private IP addresses to the specified network
-- interface. You can specify one or more specific secondary IP addresses, or
-- you can specify the number of secondary IP addresses to be automatically
-- assigned within the subnet's CIDR block range. The number of secondary IP
-- addresses that you can assign to an instance varies by instance type. For
-- information about instance types, see Available Instance Types in the
-- Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssignPrivateIpAddresses.html>

data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { apiaNetworkInterfaceId             :: !ByteString
      -- ^ The ID of the network interface.
    , apiaPrivateIpAddress               :: Members AssignPrivateIpAddressesSetItemRequestType
      -- ^ The IP address to be assigned as a secondary private IP address
      -- to the network interface. This option can be used multiple times
      -- to assign multiple secondary private IP addresses to the network
      -- interface.
    , apiaSecondaryPrivateIpAddressCount :: !Integer
      -- ^ The number of secondary IP addresses to assign to the network
      -- interface.
    , apiaAllowReassignment              :: Maybe Bool
      -- ^ Specifies whether to allow an IP address that is already assigned
      -- to another network interface or instance to be reassigned to the
      -- specified network interface.
    } deriving (Eq, Show, Generic)

instance IsQuery AssignPrivateIpAddresses

instance IsXML AssignPrivateIpAddresses where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AssignPrivateIpAddresses AssignPrivateIpAddressesResponse where
    request = req GET "AssignPrivateIpAddresses"

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    { apiaRequestId :: !ByteString
      -- ^ The ID of the request.
    , apiaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AssignPrivateIpAddressesResponse where
    xmlPickler = withNS ec2NS

-- | Associates an Elastic IP address with an instance or a network interface.
-- For more information about Elastic IP addresses, see Elastic IP Addresses
-- in the Amazon Elastic Compute Cloud User Guide.[EC2-Classic, default VPC]
-- If the Elastic IP address is already associated with a different instance,
-- it is disassociated from that instance and associated with the specified
-- instance.[EC2-VPC] If you don't specify a private IP address, the Elastic
-- IP address is associated with the primary IP address. If the Elastic IP
-- address is already associated with a different instance or a network
-- interface, you get an error unless you specify the AllowReassociation
-- parameter. This is an idempotent operation. If you enter it more than once,
-- Amazon EC2 does not return an error.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateAddress.html>

data AssociateAddress = AssociateAddress
    { abPublicIp           :: !ByteString
      -- ^ The Elastic IP address.
    , abInstanceId         :: !ByteString
      -- ^ The ID of the instance. The operation fails if you specify an
      -- instance ID unless exactly one network interface is attached.
    , acAllocationId       :: !ByteString
      -- ^ [EC2-VPC] The allocation ID.
    , acNetworkInterfaceId :: !ByteString
      -- ^ [EC2-VPC] The ID of the network interface.
    , acPrivateIpAddress   :: Maybe ByteString
      -- ^ [EC2-VPC] The primary or secondary private IP address to
      -- associate with the Elastic IP address. If no private IP address
      -- is specified, the Elastic IP address is associated with the
      -- primary private IP address.
    , acAllowReassociation :: Maybe Bool
      -- ^ [EC2-VPC] Allows an Elastic IP address that is already associated
      -- with an instance or network interface to be re-associated with
      -- the specified instance or network interface. Otherwise, the
      -- operation fails.
    } deriving (Eq, Show, Generic)

instance IsQuery AssociateAddress

instance IsXML AssociateAddress where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AssociateAddress AssociateAddressResponse where
    request = req GET "AssociateAddress"

data AssociateAddressResponse = AssociateAddressResponse
    { acRequestId     :: !ByteString
      -- ^ The ID of the request.
    , acReturn        :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    , acAssociationId :: !ByteString
      -- ^ [EC2-VPC] The ID that represents the association of the Elastic
      -- IP address with an instance.
    } deriving (Eq, Show, Generic)

instance IsXML AssociateAddressResponse where
    xmlPickler = withNS ec2NS

-- | Associates a set of DHCP options (that you've previously created) with the
-- specified VPC, or associates no DHCP options with the VPC. After you
-- associate the options with the VPC, any existing instances and all new
-- instances that you launch in that VPC use the options. You don't need to
-- restart or relaunch the instances. They automatically pick up the changes
-- within a few hours, depending on how frequently the instance renews its
-- DHCP lease. You can explicitly renew the lease using the operating system
-- on the instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateDhcpOptions.html>

data AssociateDhcpOptions = AssociateDhcpOptions
    { adoDhcpOptionsId :: !ByteString
      -- ^ The ID of the DHCP options set, or default to associate no DHCP
      -- options with the VPC.
    , adoVpcId         :: !ByteString
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery AssociateDhcpOptions

instance IsXML AssociateDhcpOptions where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AssociateDhcpOptions AssociateDhcpOptionsResponse where
    request = req GET "AssociateDhcpOptions"

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    { adoRequestId :: !ByteString
      -- ^ The ID of the request.
    , adoReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AssociateDhcpOptionsResponse where
    xmlPickler = withNS ec2NS

-- | Associates a subnet with a route table. The subnet and route table must be
-- in the same VPC. This association causes traffic originating from the
-- subnet to be routed according to the routes in the route table. The action
-- returns an association ID, which you need in order to disassociate the
-- route table from the subnet later. A route table can be associated with
-- multiple subnets.For more information about route tables, see Route Tables
-- in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateRouteTable.html>

data AssociateRouteTable = AssociateRouteTable
    { artRouteTableId :: !ByteString
      -- ^ The ID of the route table.
    , artSubnetId     :: !ByteString
      -- ^ The ID of the subnet.
    } deriving (Eq, Show, Generic)

instance IsQuery AssociateRouteTable

instance IsXML AssociateRouteTable where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AssociateRouteTable AssociateRouteTableResponse where
    request = req GET "AssociateRouteTable"

data AssociateRouteTableResponse = AssociateRouteTableResponse
    { artRequestId     :: !ByteString
      -- ^ The ID of the request.
    , artAssociationId :: !ByteString
      -- ^ The route table association ID (needed to disassociate the route
      -- table).
    } deriving (Eq, Show, Generic)

instance IsXML AssociateRouteTableResponse where
    xmlPickler = withNS ec2NS

-- | Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, see the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachInternetGateway.html>

data AttachInternetGateway = AttachInternetGateway
    { aigInternetGatewayId :: !ByteString
      -- ^ The ID of the Internet gateway.
    , aigVpcId             :: !ByteString
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery AttachInternetGateway

instance IsXML AttachInternetGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AttachInternetGateway AttachInternetGatewayResponse where
    request = req GET "AttachInternetGateway"

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    { aigRequestId :: !ByteString
      -- ^ The ID of the request.
    , aigReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AttachInternetGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Attaches a network interface to an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachNetworkInterface.html>

data AttachNetworkInterface = AttachNetworkInterface
    { aniNetworkInterfaceId :: !ByteString
      -- ^ The ID of the network interface.
    , aniInstanceId         :: !ByteString
      -- ^ The ID of the instance.
    , aniDeviceIndex        :: !Integer
      -- ^ The index of the device for the network interface attachment.
    } deriving (Eq, Show, Generic)

instance IsQuery AttachNetworkInterface

instance IsXML AttachNetworkInterface where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AttachNetworkInterface AttachNetworkInterfaceResponse where
    request = req GET "AttachNetworkInterface"

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { aniRequestId    :: !ByteString
      -- ^ The ID of the attachment request.
    , aniAttachmentId :: !ByteString
      -- ^ The ID of the network interface attachment.
    } deriving (Eq, Show, Generic)

instance IsXML AttachNetworkInterfaceResponse where
    xmlPickler = withNS ec2NS

-- | Attaches an Amazon EBS volume to a running or stopped instance and exposes
-- it to the instance with the specified device name.For a list of supported
-- device names, see Attaching the Volume to an Instance. Any device names
-- that aren't reserved for instance store volumes can be used for Amazon EBS
-- volumes.For an overview of the AWS Marketplace,
-- see https://aws.amazon.com/marketplace/help/200900000. For details on how
-- to use the AWS Marketplace, see AWS Marketplace.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVolume.html>

data AttachVolume = AttachVolume
    { avVolumeId   :: !ByteString
      -- ^ The ID of the Amazon EBS volume. The volume and instance must be
      -- within the same Availability Zone.
    , avInstanceId :: !ByteString
      -- ^ The ID of the instance.
    , avDevice     :: !ByteString
      -- ^ The device name to expose to the instance (for example, /dev/sdh
      -- or xvdh).
    } deriving (Eq, Show, Generic)

instance IsQuery AttachVolume

instance IsXML AttachVolume where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AttachVolume AttachVolumeResponse where
    request = req GET "AttachVolume"

data AttachVolumeResponse = AttachVolumeResponse
    { avRequestId  :: !ByteString
      -- ^ The ID of the request.
    , awVolumeId   :: !ByteString
      -- ^ The ID of the volume.
    , awInstanceId :: !ByteString
      -- ^ The ID of the instance.
    , awDevice     :: !ByteString
      -- ^ The device name.
    , awStatus     :: !ByteString
      -- ^ The attachment state of the volume.
    , awAttachTime :: !UTCTime
      -- ^ The time stamp when the attachment initiated.
    } deriving (Eq, Show, Generic)

instance IsXML AttachVolumeResponse where
    xmlPickler = withNS ec2NS

-- | Attaches a virtual private gateway to a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVpnGateway.html>

data AttachVpnGateway = AttachVpnGateway
    { avgVpnGatewayId :: !ByteString
      -- ^ The ID of the virtual private gateway.
    , avgVpcId        :: !ByteString
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery AttachVpnGateway

instance IsXML AttachVpnGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AttachVpnGateway AttachVpnGatewayResponse where
    request = req GET "AttachVpnGateway"

data AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { avgRequestId  :: !ByteString
      -- ^ The ID of the request.
    , avgAttachment :: !AttachmentType
      -- ^ Information about the attachment.
    } deriving (Eq, Show, Generic)

instance IsXML AttachVpnGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Adds one or more egress rules to a security group for use with a VPC.
-- Specifically, this action permits instances to send traffic to one or more
-- destination CIDR IP address ranges, or to one or more destination security
-- groups for the same VPC.A security group is for use with instances either
-- in the EC2-Classic platform or in a specific VPC. This action doesn't apply
-- to security groups for use in EC2-Classic.Each rule consists of the protocol (for example, TCP), plus either a
-- CIDR range or a source group. For the TCP and UDP protocols, you must also
-- specify the destination port or port range. For the ICMP protocol, you must
-- also specify the ICMP type and code. You can use -1 for the type or code to
-- mean all types or all codes. Rule changes are propagated to affected
-- instances as quickly as possible. However, a small delay might occur.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupEgress.html>

data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { asgeGroupId       :: !ByteString
      -- ^ The ID of the security group to modify.
    , asgeIpPermissions :: Members IpPermissionType
      -- ^ The IP protocol name or number (see Protocol Numbers).
    } deriving (Eq, Show, Generic)

instance IsQuery AuthorizeSecurityGroupEgress

instance IsXML AuthorizeSecurityGroupEgress where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AuthorizeSecurityGroupEgress AuthorizeSecurityGroupEgressResponse where
    request = req GET "AuthorizeSecurityGroupEgress"

data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
    { asgeRequestId :: !ByteString
      -- ^ The ID of the request.
    , asgeReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AuthorizeSecurityGroupEgressResponse where
    xmlPickler = withNS ec2NS

-- | Adds one or more ingress rules to a security group.Rule changes are
-- propagated to instances within the security group as quickly as possible.
-- However, a small delay might occur. A security group is for use with
-- instances either in the EC2-Classic platform or in a specific VPC. For more
-- information, see Amazon EC2 Security Groups in the Amazon Elastic Compute
-- Cloud User Guide and Security Groups for Your VPC in the Amazon Virtual
-- Private Cloud User Guide.[EC2-Classic] This action gives one or more CIDR
-- IP address ranges permission to access a security group in your account, or
-- gives one or more security groups (called the source groups) permission to
-- access a security group for your account. A source group can be for your
-- own AWS account, or another. [EC2-VPC] This action gives one or more CIDR
-- IP address ranges permission to access a security group in your VPC, or
-- gives one or more other security groups (called the source groups)
-- permission to access a security group for your VPC. The security groups
-- must all be for the same VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupIngress.html>

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { asgiUserId        :: Maybe ByteString
      -- ^ Deprecated
    , asgiGroupId       :: !ByteString
      -- ^ The ID of the security group to modify. The security group must
      -- belong to your account.
    , asgiGroupName     :: !ByteString
      -- ^ The name of the security group to modify.
    , asgiIpPermissions :: Members IpPermissionType
      -- ^ The IP protocol name or number (see Protocol Numbers). For
      -- EC2-Classic, security groups can have rules only for TCP, UDP,
      -- and ICMP. For EC2-VPC, security groups can have rules assigned to
      -- any protocol number.
    } deriving (Eq, Show, Generic)

instance IsQuery AuthorizeSecurityGroupIngress

instance IsXML AuthorizeSecurityGroupIngress where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 AuthorizeSecurityGroupIngress AuthorizeSecurityGroupIngressResponse where
    request = req GET "AuthorizeSecurityGroupIngress"

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    { asgiRequestId :: !ByteString
      -- ^ The ID of the request.
    , asgiReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AuthorizeSecurityGroupIngressResponse where
    xmlPickler = withNS ec2NS

-- | Bundles an Amazon instance store-backed Windows instance.During bundling,
-- only the root device volume (C:\) is bundled. Data on other instance store
-- volumes is not preserved.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-BundleInstance.html>

data BundleInstance = BundleInstance
    { biInstanceId :: !ByteString
      -- ^ The ID of the instance to bundle.
    , biStorage    :: !StorageType
      -- ^ The bucket in which to store the AMI. You can specify a bucket
      -- that you already own or a new bucket that Amazon EC2 creates on
      -- your behalf. If you specify a bucket that belongs to someone
      -- else, Amazon EC2 returns an error.
    } deriving (Eq, Show, Generic)

instance IsQuery BundleInstance

instance IsXML BundleInstance where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 BundleInstance BundleInstanceResponse where
    request = req GET "BundleInstance"

data BundleInstanceResponse = BundleInstanceResponse
    { biRequestId          :: !ByteString
      -- ^ The ID of the request.
    , biBundleInstanceTask :: !BundleInstanceTaskType
      -- ^ The bundle task.
    } deriving (Eq, Show, Generic)

instance IsXML BundleInstanceResponse where
    xmlPickler = withNS ec2NS

-- | Cancels a bundling operation for an instance store-backed Windows instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelBundleTask.html>

data CancelBundleTask = CancelBundleTask
    { cbtBundleId :: !ByteString
      -- ^ The ID of the bundle task.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelBundleTask

instance IsXML CancelBundleTask where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CancelBundleTask CancelBundleTaskResponse where
    request = req GET "CancelBundleTask"

data CancelBundleTaskResponse = CancelBundleTaskResponse
    { cbtRequestId          :: !ByteString
      -- ^ The ID of the request.
    , cbtBundleInstanceTask :: !BundleInstanceTaskType
      -- ^ The bundle task.
    } deriving (Eq, Show, Generic)

instance IsXML CancelBundleTaskResponse where
    xmlPickler = withNS ec2NS

-- | Cancels an active conversion task. The task can be the import of an
-- instance or volume. The action removes all artifacts of the conversion,
-- including a partially uploaded volume or instance. If the conversion is
-- complete or is in the process of transferring the final disk image, the
-- command fails and returns an exception.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelConversionTask.html>

data CancelConversionTask = CancelConversionTask
    { cctConversionTaskId :: !ByteString
      -- ^ The ID of the conversion task.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelConversionTask

instance IsXML CancelConversionTask where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CancelConversionTask CancelConversionTaskResponse where
    request = req GET "CancelConversionTask"

data CancelConversionTaskResponse = CancelConversionTaskResponse
    { cctRequestId :: !ByteString
      -- ^ The ID of the request.
    , cctReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML CancelConversionTaskResponse where
    xmlPickler = withNS ec2NS

-- | Cancels an active export task. The request removes all artifacts of the
-- export, including any partially created Amazon S3 objects. If the export
-- task is complete or is in the process of transferring the final disk image,
-- the command fails and returns an error.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelExportTask.html>

data CancelExportTask = CancelExportTask
    { cetExportTaskId :: !ByteString
      -- ^ The ID of the export task. This is the ID returned by
      -- CreateInstanceExportTask.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelExportTask

instance IsXML CancelExportTask where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CancelExportTask CancelExportTaskResponse where
    request = req GET "CancelExportTask"

data CancelExportTaskResponse = CancelExportTaskResponse
    { cetRequestId :: !ByteString
      -- ^ The ID of the request.
    , cetReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML CancelExportTaskResponse where
    xmlPickler = withNS ec2NS

-- | Cancels the specified Reserved Instance listing in the Reserved Instance
-- Marketplace.For more information about Reserved Instance Marketplace, see
-- Reserved Instance Marketplace in the Amazon Elastic Compute Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelReservedInstancesListing.html>

data CancelReservedInstancesListing = CancelReservedInstancesListing
    { crilReservedInstancesListingId :: !ByteString
      -- ^ The ID of the Reserved Instance listing to be canceled.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelReservedInstancesListing

instance IsXML CancelReservedInstancesListing where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CancelReservedInstancesListing CancelReservedInstancesListingResponse where
    request = req GET "CancelReservedInstancesListing"

data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { crilRequestId                    :: !ByteString
      -- ^ The ID of the request.
    , crilReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSetItemType
      -- ^ The Reserved Instance listing for cancellation. The listing
      -- information is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML CancelReservedInstancesListingResponse where
    xmlPickler = withNS ec2NS

-- | Cancels one or more Spot Instance requests. Spot Instances are instances
-- that Amazon EC2 starts on your behalf when the maximum price that you
-- specify exceeds the current Spot Price. Amazon EC2 periodically sets the
-- Spot Price based on available Spot Instance capacity and current Spot
-- Instance requests. For more information about Spot Instances, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotInstanceRequests.html>

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { csirSpotInstanceRequestId :: Members ByteString
      -- ^ One or more Spot Instance request IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelSpotInstanceRequests

instance IsXML CancelSpotInstanceRequests where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CancelSpotInstanceRequests CancelSpotInstanceRequestsResponse where
    request = req GET "CancelSpotInstanceRequests"

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { csirRequestId              :: !ByteString
      -- ^ The ID of the request.
    , csirSpotInstanceRequestSet :: !CancelSpotInstanceRequestsResponseSetItemType
      -- ^ A list of Spot Instance requests. Each request is wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML CancelSpotInstanceRequestsResponse where
    xmlPickler = withNS ec2NS

-- | Determines whether a product code is associated with an instance. This
-- action can only be used by the owner of the product code. It is useful when
-- a product code owner needs to verify whether another user's instance is
-- eligible for support.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ConfirmProductInstance.html>

data ConfirmProductInstance = ConfirmProductInstance
    { cpiProductCode :: !ByteString
      -- ^ The product code. This must be an Amazon DevPay product code that
      -- you own.
    , cpiInstanceId  :: !ByteString
      -- ^ The instance.
    } deriving (Eq, Show, Generic)

instance IsQuery ConfirmProductInstance

instance IsXML ConfirmProductInstance where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ConfirmProductInstance ConfirmProductInstanceResponse where
    request = req GET "ConfirmProductInstance"

data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { cpiRequestId :: !ByteString
      -- ^ The ID of the request.
    , cpiReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    , cpiOwnerId   :: !ByteString
      -- ^ The instance owner's account ID. Only present if the product code
      -- is attached to the instance.
    } deriving (Eq, Show, Generic)

instance IsXML ConfirmProductInstanceResponse where
    xmlPickler = withNS ec2NS

-- | Initiates the copy of an AMI from the specified source region to the region
-- in which the request was made.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopyImage.html>

data CopyImage = CopyImage
    { ciSourceRegion  :: !ByteString
      -- ^ The name of the region that contains the AMI to be copied
      -- (source).
    , ciSourceImageId :: !ByteString
      -- ^ The ID of the AMI to copy.
    , ciName          :: Maybe ByteString
      -- ^ The name of the new AMI in the destination region.
    , ciDescription   :: Maybe ByteString
      -- ^ A description for the new AMI in the destination region.
    , ciClientToken   :: Maybe ByteString
      -- ^ Unique, case-sensitive identifier you provide to ensure
      -- idempotency of the request.
    } deriving (Eq, Show, Generic)

instance IsQuery CopyImage

instance IsXML CopyImage where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CopyImage CopyImageResponse where
    request = req GET "CopyImage"

data CopyImageResponse = CopyImageResponse
    { ciRequestId :: !ByteString
      -- ^ The ID of the request.
    , ciImageId   :: !ByteString
      -- ^ The ID of the new AMI.
    } deriving (Eq, Show, Generic)

instance IsXML CopyImageResponse where
    xmlPickler = withNS ec2NS

-- | Copies a point-in-time snapshot of an Amazon Elastic Block Store (Amazon
-- EBS) volume and stores it in Amazon Simple Storage Service (Amazon S3). You
-- can copy the snapshot within the same region or from one region to another.
-- You can use the snapshot to create Amazon EBS volumes or Amazon Machine
-- Images (AMIs).For more information about Amazon EBS, see Amazon Elastic
-- Block Store (Amazon EBS).
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopySnapshot.html>

data CopySnapshot = CopySnapshot
    { csSourceRegion     :: !ByteString
      -- ^ The ID of the region that contains the snapshot to be copied.
    , csSourceSnapshotId :: !ByteString
      -- ^ The ID of the Amazon EBS snapshot to copy.
    , csDescription      :: Maybe ByteString
      -- ^ A description for the new Amazon EBS snapshot.
    } deriving (Eq, Show, Generic)

instance IsQuery CopySnapshot

instance IsXML CopySnapshot where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CopySnapshot CopySnapshotResponse where
    request = req GET "CopySnapshot"

data CopySnapshotResponse = CopySnapshotResponse
    { csRequestId  :: !ByteString
      -- ^ The ID of the request.
    , csSnapshotId :: !ByteString
      -- ^ The ID of the new snapshot.
    } deriving (Eq, Show, Generic)

instance IsXML CopySnapshotResponse where
    xmlPickler = withNS ec2NS

-- | Provides information to AWS about your VPN customer gateway device. The
-- customer gateway is the appliance at your end of the VPN connection. (The
-- device on the AWS side of the VPN connection is the virtual private
-- gateway.) You must provide the Internet-routable IP address of the customer
-- gateway's external interface. The IP address must be static and can't be
-- behind a device performing network address translation (NAT).You must
-- provide the Internet-routable IP address of the customer gateway's external
-- interface. The IP address must be static and can't be behind a device
-- performing network address translation (NAT).For devices that use Border
-- Gateway Protocol (BGP), you can also provide the device's BGP Autonomous
-- System Number (ASN). You can use an existing ASN assigned to your network.
-- If you don't have an ASN already, you can use a private ASN (in the 64512 -
-- 65534 range).For more information about ASNs, see the Wikipedia article.For
-- more information about VPN customer gateways, see Adding a Hardware Virtual
-- Private Gateway to Your VPC in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateCustomerGateway.html>

data CreateCustomerGateway = CreateCustomerGateway
    { ccgType      :: !ByteString
      -- ^ The type of VPN connection this customer gateway supports.
    , ccgIpAddress :: !ByteString
      -- ^ The Internet-routable IP address for the customer gateway's
      -- outside interface. The address must be static.
    , ccgBgpAsn    :: Maybe Integer
      -- ^ For devices that support BGP, the customer gateway's Border
      -- Gateway Protocol (BGP) Autonomous System Number (ASN).
    } deriving (Eq, Show, Generic)

instance IsQuery CreateCustomerGateway

instance IsXML CreateCustomerGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateCustomerGateway CreateCustomerGatewayResponse where
    request = req GET "CreateCustomerGateway"

data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
    { ccgRequestId       :: !ByteString
      -- ^ The ID of the request.
    , ccgCustomerGateway :: !CustomerGatewayType
      -- ^ Information about the customer gateway.
    } deriving (Eq, Show, Generic)

instance IsXML CreateCustomerGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Creates a set of DHCP options for your VPC. After creating the set, you
-- must associate it with the VPC, causing all existing and new instances that
-- you launch in the VPC to use this set of DHCP options. The following are
-- the individual DHCP options you can specify. For more information about the
-- options, see RFC 2132. For more information about DHCP options, see DHCP
-- Options Sets in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateDhcpOptions.html>

data CreateDhcpOptions = CreateDhcpOptions
    { cdoDhcpConfiguration :: Members DhcpConfigurationType
      -- ^ The name of a DHCP option.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateDhcpOptions

instance IsXML CreateDhcpOptions where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateDhcpOptions CreateDhcpOptionsResponse where
    request = req GET "CreateDhcpOptions"

data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse
    { cdoRequestId   :: !ByteString
      -- ^ The ID of the request.
    , cdoDhcpOptions :: !DhcpOptionsType
      -- ^ A set of DHCP options.
    } deriving (Eq, Show, Generic)

instance IsXML CreateDhcpOptionsResponse where
    xmlPickler = withNS ec2NS

-- | Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that is
-- either running or stopped.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateImage.html>

data CreateImage = CreateImage
    { ciInstanceId         :: !ByteString
      -- ^ The ID of the instance.
    , cjName               :: !ByteString
      -- ^ A name for the new image.
    , cjDescription        :: Maybe ByteString
      -- ^ A description for the new image.
    , cjNoReboot           :: Maybe Bool
      -- ^ By default this parameter is set to false, which means Amazon EC2
      -- attempts to cleanly shut down the instance before image creation
      -- and then reboots the instance. When the parameter is set to true,
      -- Amazon EC2 doesn't shut down the instance before creating the
      -- image. When this option is used, file system integrity on the
      -- created image can't be guaranteed.
    , cjBlockDeviceMapping :: Members BlockDeviceMappingType
      -- ^ The device name exposed to the instance (for example, /dev/sdh or
      -- xvdh).
    } deriving (Eq, Show, Generic)

instance IsQuery CreateImage

instance IsXML CreateImage where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateImage CreateImageResponse where
    request = req GET "CreateImage"

data CreateImageResponse = CreateImageResponse
    { cjRequestId :: !ByteString
      -- ^ The ID of the request.
    , cjImageId   :: !ByteString
      -- ^ The ID of the new AMI.
    } deriving (Eq, Show, Generic)

instance IsXML CreateImageResponse where
    xmlPickler = withNS ec2NS

-- | Exports a running or stopped instance to an Amazon S3 bucket.For
-- information about the supported operating systems, image formats, and known
-- limitations for the types of instances you can export, see Exporting EC2
-- Instances in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInstanceExportTask.html>

data CreateInstanceExportTask = CreateInstanceExportTask
    { cietDescription       :: Maybe ByteString
      -- ^ A description for the conversion task or the resource being
      -- exported. The maximum length is 255 bytes.
    , cietInstanceId        :: !ByteString
      -- ^ The ID of the instance.
    , cietTargetEnvironment :: !ByteString
      -- ^ The target virtualization environment.
    , cietExportToS3        :: Members ExportToS3Type
      -- ^ The format for the exported image.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateInstanceExportTask

instance IsXML CreateInstanceExportTask where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateInstanceExportTask CreateInstanceExportTaskResponse where
    request = req GET "CreateInstanceExportTask"

data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { cietRequestId  :: !ByteString
      -- ^ The ID of the request.
    , cietExportTask :: !ExportTaskResponseType
      -- ^ The details of the created ExportVM task.
    } deriving (Eq, Show, Generic)

instance IsXML CreateInstanceExportTaskResponse where
    xmlPickler = withNS ec2NS

-- | Creates an Internet gateway for use with a VPC. After creating the Internet
-- gateway, you attach it to a VPC using AttachInternetGateway.For more
-- information about your VPC and Internet gateway, see the Amazon Virtual
-- Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInternetGateway.html>

data CreateInternetGateway = CreateInternetGateway
    deriving (Eq, Read, Show, Generic)

instance IsQuery CreateInternetGateway

instance IsXML CreateInternetGateway where
    xmlPickler = xpEmpty $ Just ec2NS

instance AWSRequest EC2 CreateInternetGateway CreateInternetGatewayResponse where
    request = req GET "CreateInternetGateway"

data CreateInternetGatewayResponse = CreateInternetGatewayResponse
    { cigRequestId       :: !ByteString
      -- ^ The ID of the request.
    , cigInternetGateway :: !InternetGatewayType
      -- ^ Information about the Internet gateway
    } deriving (Eq, Show, Generic)

instance IsXML CreateInternetGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Creates a 2048-bit RSA key pair with the specified name. Amazon EC2 stores
-- the public key and displays the private key for you to save to a file. The
-- private key is returned as an unencrypted PEM encoded PKCS#8 private key.
-- If a key with the specified name already exists, Amazon EC2 returns an
-- error. You can have up to five thousand key pairs per region.For more
-- information about key pairs, see Key Pairs in the Amazon Elastic Compute
-- Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateKeyPair.html>

data CreateKeyPair = CreateKeyPair
    { ckpKeyName :: !ByteString
      -- ^ A unique name for the key pair.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateKeyPair

instance IsXML CreateKeyPair where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateKeyPair CreateKeyPairResponse where
    request = req GET "CreateKeyPair"

data CreateKeyPairResponse = CreateKeyPairResponse
    { ckpRequestId      :: !ByteString
      -- ^ The ID of the request.
    , ckqKeyName        :: !ByteString
      -- ^ The name of the key pair name.
    , ckqKeyFingerprint :: !ByteString
      -- ^ A SHA-1 digest of the DER encoded private key.
    , ckqKeyMaterial    :: !ByteString
      -- ^ An unencrypted PEM encoded RSA private key.
    } deriving (Eq, Show, Generic)

instance IsXML CreateKeyPairResponse where
    xmlPickler = withNS ec2NS

-- | Creates a network ACL in a VPC. Network ACLs provide an optional layer of
-- security (on top of security groups) for the instances in your VPC.For more
-- information about network ACLs, see Network ACLs in the Amazon Virtual
-- Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkAcl.html>

data CreateNetworkAcl = CreateNetworkAcl
    { cnaVpcId :: !ByteString
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateNetworkAcl

instance IsXML CreateNetworkAcl where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateNetworkAcl CreateNetworkAclResponse where
    request = req GET "CreateNetworkAcl"

data CreateNetworkAclResponse = CreateNetworkAclResponse
    { cnaRequestId  :: !ByteString
      -- ^ The ID of the request.
    , cnaNetworkAcl :: !NetworkAclType
      -- ^ Information about the new network ACL.
    } deriving (Eq, Show, Generic)

instance IsXML CreateNetworkAclResponse where
    xmlPickler = withNS ec2NS

-- | Creates an entry (a rule) in a network ACL with the specified rule number.
-- Each network ACL has a set of numbered ingress rules and a separate set of
-- numbered egress rules. When determining whether a packet should be allowed
-- in or out of a subnet associated with the ACL, we process the entries in
-- the ACL according to the rule numbers, in ascending order. Each network ACL
-- has a set of ingress rules and a separate set of egress rules.After you add
-- an entry, you can't modify it; you must either replace it, or create a new
-- entry and delete the old one.For more information about network ACLs, see
-- Network ACLs in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkAclEntry.html>

data CreateNetworkAclEntry = CreateNetworkAclEntry
    { cnaeNetworkAclId :: !ByteString
      -- ^ The ID of the ACL.
    , cnaeRuleNumber   :: !Integer
      -- ^ The rule number to assign to the entry (for example, 100). ACL
      -- entries are processed in ascending order by rule number.
    , cnaeProtocol     :: !Integer
      -- ^ The IP protocol the rule applies to. You can use -1 to mean all
      -- protocols.
    , cnaeRuleAction   :: !ByteString
      -- ^ Indicates whether to allow or deny traffic that matches the rule.
    , cnaeEgress       :: Maybe Bool
      -- ^ Indicates whether this rule applies to egress traffic from the
      -- subnet (true) or ingress traffic to the subnet (false).
    , cnaeCidrBlock    :: !ByteString
      -- ^ The CIDR range to allow or deny, in CIDR notation (for example,
      -- 172.16.0.0/24).
    , cnaeIcmp         :: Members IcmpType
      -- ^ For the ICMP protocol, the ICMP code. You can use -1 to specify
      -- all ICMP codes for the given ICMP type.
    , cnaePortRange    :: Members PortRangeType
      -- ^ The first port in the range.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateNetworkAclEntry

instance IsXML CreateNetworkAclEntry where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateNetworkAclEntry CreateNetworkAclEntryResponse where
    request = req GET "CreateNetworkAclEntry"

data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
    { cnaeRequestId :: !ByteString
      -- ^ The ID of the request.
    , cnaeReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML CreateNetworkAclEntryResponse where
    xmlPickler = withNS ec2NS

-- | Creates a network interface in the specified subnet.For more information
-- about network interfaces, see Elastic Network Interfaces in the Amazon
-- Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkInterface.html>

data CreateNetworkInterface = CreateNetworkInterface
    { cniSubnetId                       :: !ByteString
      -- ^ The ID of the subnet to associate with the network interface.
    , cniPrivateIpAddress               :: Maybe ByteString
      -- ^ The primary private IP address of the network interface. If you
      -- don't specify an IP address, Amazon EC2 will select one for you
      -- from the subnet range.
    , cniPrivateIpAddresses             :: Members PrivateIpAddressesType
      -- ^ The private IP address of the specified network interface. You
      -- can use this parameter multiple times to specify explicit private
      -- IP addresses for a network interface, but only one private IP
      -- address can be designated as primary.
    , cniSecondaryPrivateIpAddressCount :: Maybe Integer
      -- ^ The number of secondary private IP addresses to assign to a
      -- network interface. When you specify a number of secondary IP
      -- addresses, Amazon EC2 selects these IP addresses within the
      -- subnet range.
    , cniDescription                    :: Maybe ByteString
      -- ^ A description for the network interface.
    , cniSecurityGroupId                :: Members SecurityGroupIdSetItemType
      -- ^ The list of security group IDs for the network interface.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateNetworkInterface

instance IsXML CreateNetworkInterface where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateNetworkInterface CreateNetworkInterfaceResponse where
    request = req GET "CreateNetworkInterface"

data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { cniRequestId        :: !ByteString
      -- ^ The ID of the request.
    , cniNetworkInterface :: !NetworkInterfaceType
      -- ^ The network interface that was created.
    } deriving (Eq, Show, Generic)

instance IsXML CreateNetworkInterfaceResponse where
    xmlPickler = withNS ec2NS

-- | Creates a placement group that you launch cluster instances into. You must
-- give the group a name unique within the scope of your account.For more
-- information about placement groups and cluster instances, see Cluster
-- Instances in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreatePlacementGroup.html>

data CreatePlacementGroup = CreatePlacementGroup
    { cpgGroupName :: !ByteString
      -- ^ A name for the placement group.
    , cpgStrategy  :: !ByteString
      -- ^ The placement strategy.
    } deriving (Eq, Show, Generic)

instance IsQuery CreatePlacementGroup

instance IsXML CreatePlacementGroup where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreatePlacementGroup CreatePlacementGroupResponse where
    request = req GET "CreatePlacementGroup"

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    { cpgRequestId :: !ByteString
      -- ^ The ID of the request.
    , cpgReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML CreatePlacementGroupResponse where
    xmlPickler = withNS ec2NS

-- | Creates a listing for Amazon EC2 Reserved Instances that will be sold in
-- the Reserved Instance Marketplace. You can submit one Reserved Instance
-- listing at a time.The Reserved Instance Marketplace matches sellers who
-- want to resell Reserved Instance capacity that they no longer need with
-- buyers who want to purchase additional capacity. Reserved Instances bought
-- and sold through the Reserved Instance Marketplace work like any other
-- Reserved Instances.If you want to sell your Reserved Instances, you must
-- first register as a Seller in the Reserved Instance Marketplace. After
-- completing the registration process, you can create a Reserved Instance
-- Marketplace listing of some or all of your Reserved Instances, and specify
-- the upfront price you want to receive for them. Your Reserved Instance
-- listings then become available for purchase.For more information about
-- Reserved Instance Marketplace, see Reserved Instance Marketplace in the
-- Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateReservedInstancesListing.html>

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { crilReservedInstancesId :: !ByteString
      -- ^ The ID of the active Reserved Instance.
    , crilInstanceCount       :: !Integer
      -- ^ The number of instances that are a part of a Reserved Instance
      -- account that will be listed in the Reserved Instance Marketplace.
      -- This number should be less than or equal to the instance count
      -- associated with the Reserved Instance ID specified in this call.
    , crilPriceSchedules      :: !PriceScheduleRequestSetItemType
      -- ^ A list specifying the price of the Reserved Instance for each
      -- month remaining in the Reserved Instance term.
    , crilClientToken         :: !ByteString
      -- ^ Unique, case-sensitive identifier you provide to ensure
      -- idempotency of your listings. This helps avoid duplicate
      -- listings.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateReservedInstancesListing

instance IsXML CreateReservedInstancesListing where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateReservedInstancesListing CreateReservedInstancesListingResponse where
    request = req GET "CreateReservedInstancesListing"

data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { crimRequestId                   :: !ByteString
      -- ^ The ID of the request.
    , crimReservedInstancesListingSet :: !DescribeReservedInstancesListingsResponseSetItemType
      -- ^ The Reserved Instances listing that was created. The listing
      -- information is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML CreateReservedInstancesListingResponse where
    xmlPickler = withNS ec2NS

-- | Creates a route in a route table within a VPC. The route's target can be
-- either a gateway attached to the VPC or a NAT instance in the VPC.When
-- determining how to route traffic, we use the route with the most specific
-- match. For example, let's say the traffic is destined for 192.0.2.3, and
-- the route table includes the following two routes: Both routes apply to the
-- traffic destined for 192.0.2.3. However, the second route in the list
-- covers a smaller number of IP addresses and is therefore more specific, so
-- we use that route to determine where to target the traffic.For more
-- information about route tables, see Route Tables in the Amazon Virtual
-- Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRoute.html>

data CreateRoute = CreateRoute
    { crRouteTableId         :: !ByteString
      -- ^ The ID of the route table for the route.
    , crDestinationCidrBlock :: !ByteString
      -- ^ The CIDR address block used for the destination match. Routing
      -- decisions are based on the most specific match.
    , crGatewayId            :: !ByteString
      -- ^ The ID of an Internet gateway attached to your VPC.
    , crInstanceId           :: !ByteString
      -- ^ The ID of a NAT instance in your VPC. The operation fails if you
      -- specify an instance ID unless exactly one network interface is
      -- attached.
    , crNetworkInterfaceId   :: !ByteString
      -- ^ The ID of a network interface.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateRoute

instance IsXML CreateRoute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateRoute CreateRouteResponse where
    request = req GET "CreateRoute"

data CreateRouteResponse = CreateRouteResponse
    { crRequestId :: !ByteString
      -- ^ The ID of the request.
    , crReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML CreateRouteResponse where
    xmlPickler = withNS ec2NS

-- | Creates a route table for the specified VPC. After you create a route
-- table, you can add routes and associate the table with a subnet.For more
-- information about route tables, see Route Tables in the Amazon Virtual
-- Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRouteTable.html>

data CreateRouteTable = CreateRouteTable
    { crtVpcId :: !ByteString
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateRouteTable

instance IsXML CreateRouteTable where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateRouteTable CreateRouteTableResponse where
    request = req GET "CreateRouteTable"

data CreateRouteTableResponse = CreateRouteTableResponse
    { crtRequestId  :: !ByteString
      -- ^ The ID of the request.
    , crtRouteTable :: !RouteTableType
      -- ^ Information about the newly created route table.
    } deriving (Eq, Show, Generic)

instance IsXML CreateRouteTableResponse where
    xmlPickler = withNS ec2NS

-- | Creates a security group.A security group is for use with instances either
-- in the EC2-Classic platform or in a specific VPC.When you create a security group, you specify a friendly name of your
-- choice. You can have a security group for use in EC2-Classic with the same
-- name as a security group for use in a VPC. However, you can't have two
-- security groups for use in EC2-Classic with the same name or two security
-- groups for use in a VPC with the same name. You have a default security
-- group for use in EC2-Classic and a default security group for use in your
-- VPC. If you don't specify a security group when you launch an instance, the
-- instance is launched into the appropriate default security group. A default
-- security group includes a default rule that grants instances unrestricted
-- network access to each other.You can add or remove rules from your security
-- groups using AuthorizeSecurityGroupIngress, AuthorizeSecurityGroupEgress,
-- RevokeSecurityGroupIngress, and RevokeSecurityGroupEgress.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSecurityGroup.html>

data CreateSecurityGroup = CreateSecurityGroup
    { csgGroupName        :: !ByteString
      -- ^ The name of the security group.
    , csgGroupDescription :: !ByteString
      -- ^ A description for the security group. This is informational only.
    , csgVpcId            :: !ByteString
      -- ^ [EC2-VPC] The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateSecurityGroup

instance IsXML CreateSecurityGroup where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateSecurityGroup CreateSecurityGroupResponse where
    request = req GET "CreateSecurityGroup"

data CreateSecurityGroupResponse = CreateSecurityGroupResponse
    { csgRequestId :: !ByteString
      -- ^ The ID of the request.
    , csgReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    , csgGroupId   :: !ByteString
      -- ^ The ID of the new security group.
    } deriving (Eq, Show, Generic)

instance IsXML CreateSecurityGroupResponse where
    xmlPickler = withNS ec2NS

-- | Creates a snapshot of an Amazon EBS volume and stores it in Amazon S3. You
-- can use snapshots for backups, to make copies of instance store volumes,
-- and to save data before shutting down an instance.When a snapshot is
-- created, any AWS Marketplace product codes from the volume are propagated
-- to the snapshot.You can take a snapshot of an attached volume that is in
-- use. However, snapshots only capture data that has been written to your
-- Amazon EBS volume at the time the snapshot command is issued. This may
-- exclude any data that has been cached by any applications or the operating
-- system. If you can pause any file writes to the volume long enough to take
-- a snapshot, your snapshot should be complete. However, if you can't pause
-- all file writes to the volume, you should unmount the volume from within
-- the instance, issue the snapshot command, and then remount the volume to
-- ensure a consistent and complete snapshot. You may remount and use your
-- volume while the snapshot status is pending.To create a snapshot for Amazon
-- EBS volumes that serve as root devices, you should stop the instance before
-- taking the snapshot.To unmount the volume in Linux/UNIX, use the following
-- command:Where device_name is the device name (for example, /dev/sdh).To
-- unmount the volume in Windows, open Disk Management, right-click the volume
-- to unmount, and select Change Drive Letter and Path. Select the mount point
-- to remove, and then click Remove.For more information about Amazon EBS, see
-- Amazon Elastic Block Store in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSnapshot.html>

data CreateSnapshot = CreateSnapshot
    { csVolumeId    :: !ByteString
      -- ^ The ID of the Amazon EBS volume.
    , ctDescription :: Maybe ByteString
      -- ^ A description for the snapshot.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateSnapshot

instance IsXML CreateSnapshot where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateSnapshot CreateSnapshotResponse where
    request = req GET "CreateSnapshot"

data CreateSnapshotResponse = CreateSnapshotResponse
    { ctRequestId   :: !ByteString
      -- ^ The ID of the request.
    , ctSnapshotId  :: !ByteString
      -- ^ The ID of the snapshot.
    , ctVolumeId    :: !ByteString
      -- ^ The ID of the volume.
    , ctStatus      :: !ByteString
      -- ^ The snapshot state.
    , ctStartTime   :: !UTCTime
      -- ^ The time stamp when the snapshot was initiated.
    , ctProgress    :: !ByteString
      -- ^ The progress of the snapshot, as a percentage.
    , ctOwnerId     :: !ByteString
      -- ^ The AWS account ID of the Amazon EBS snapshot owner.
    , ctVolumeSize  :: !ByteString
      -- ^ The size of the volume, in GiB.
    , cuDescription :: !ByteString
      -- ^ The description for the snapshot.
    } deriving (Eq, Show, Generic)

instance IsXML CreateSnapshotResponse where
    xmlPickler = withNS ec2NS

-- | Creates the datafeed for Spot Instances, enabling you to view Spot Instance
-- usage logs. You can create one data feed per account. For more information
-- about Spot Instances, see Spot Instances in the Amazon Elastic Compute
-- Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSpotDatafeedSubscription.html>

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { csdsBucket :: !ByteString
      -- ^ The Amazon S3 bucket in which to store the Spot Instance
      -- datafeed.
    , csdsPrefix :: Maybe ByteString
      -- ^ A prefix for the datafeed file names.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateSpotDatafeedSubscription

instance IsXML CreateSpotDatafeedSubscription where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateSpotDatafeedSubscription CreateSpotDatafeedSubscriptionResponse where
    request = req GET "CreateSpotDatafeedSubscription"

data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { csdsRequestId                :: !ByteString
      -- ^ The ID of the request.
    , csdsSpotDatafeedSubscription :: !SpotDatafeedSubscriptionType
      -- ^ Type: SpotDatafeedSubscriptionType
    } deriving (Eq, Show, Generic)

instance IsXML CreateSpotDatafeedSubscriptionResponse where
    xmlPickler = withNS ec2NS

-- | Creates a subnet in an existing VPC.When you create each subnet, you
-- provide the VPC ID and the CIDR block you want for the subnet. After you
-- create a subnet, you can't change its CIDR block. The subnet's CIDR block
-- can be the same as the VPC's CIDR block (assuming you want only a single
-- subnet in the VPC), or a subset of the VPC's CIDR block. If you create more
-- than one subnet in a VPC, the subnets' CIDR blocks must not overlap. The
-- smallest subnet (and VPC) you can create uses a /28 netmask (16 IP
-- addresses), and the largest uses a /16 netmask (65,536 IP addresses).If you
-- add more than one subnet to a VPC, they're set up in a star topology with a
-- logical router in the middle. By default, you can create up to 20 subnets
-- in a VPC. If you need more than 20 subnets, you can request more by going
-- to Request to Increase Amazon VPC Limits.If you launch an instance in a VPC
-- using an Amazon EBS-backed AMI, the IP address doesn't change if you stop
-- and restart the instance (unlike a similar instance launched outside a VPC,
-- which gets a new IP address when restarted). It's therefore possible to
-- have a subnet with no running instances (they're all stopped), but no
-- remaining IP addresses available. For more information about Amazon
-- EBS-backed AMIs, see AMI Basics in the Amazon Elastic Compute Cloud User
-- Guide. For more information about subnets, see Your VPC and Subnets in the
-- Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSubnet.html>

data CreateSubnet = CreateSubnet
    { csVpcId            :: !ByteString
      -- ^ The ID of the VPC.
    , csCidrBlock        :: !ByteString
      -- ^ The CIDR block for the subnet. For example, 10.0.0.0/24.
    , csAvailabilityZone :: Maybe ByteString
      -- ^ The Availability Zone for the subnet.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateSubnet

instance IsXML CreateSubnet where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateSubnet CreateSubnetResponse where
    request = req GET "CreateSubnet"

data CreateSubnetResponse = CreateSubnetResponse
    { cuRequestId :: !ByteString
      -- ^ The ID of the request.
    , cuSubnet    :: !SubnetType
      -- ^ Information about the subnet.
    } deriving (Eq, Show, Generic)

instance IsXML CreateSubnetResponse where
    xmlPickler = withNS ec2NS

-- | Adds or overwrites one or more tags for the specified EC2 resource or
-- resources. Each resource can have a maximum of 10 tags. Each tag consists
-- of a key and optional value. Tag keys must be unique per resource.For more
-- information about tags, see Tagging Your Resources in the Amazon Elastic
-- Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateTags.html>

data CreateTags = CreateTags
    { ctResourceId :: Members ByteString
      -- ^ The IDs of one or more resources to tag. For example,
      -- ami-1a2b3c4d.
    , ctTag        :: Members TagType
      -- ^ The key for a tag.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateTags

instance IsXML CreateTags where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateTags CreateTagsResponse where
    request = req GET "CreateTags"

data CreateTagsResponse = CreateTagsResponse
    { cvRequestId :: !ByteString
      -- ^ The ID of the request.
    , cvReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML CreateTagsResponse where
    xmlPickler = withNS ec2NS

-- | Creates an Amazon EBS volume that can be attached to any instance in the
-- same Availability Zone.Any AWS Marketplace product codes from the snapshot
-- are propagated to the volume.For more information about Amazon EBS, see
-- Amazon Elastic Block Store in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVolume.html>

data CreateVolume = CreateVolume
    { cvSize             :: Maybe ByteString
      -- ^ The size of the volume, in GiBs.
    , cvSnapshotId       :: !ByteString
      -- ^ The snapshot from which to create the volume.
    , cvAvailabilityZone :: !ByteString
      -- ^ The Availability Zone in which to create the volume. Use
      -- DescribeAvailabilityZones to list the Availability Zones that are
      -- currently available to you.
    , cvVolumeType       :: Maybe ByteString
      -- ^ The volume type.
    , cvIops             :: !Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateVolume

instance IsXML CreateVolume where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateVolume CreateVolumeResponse where
    request = req GET "CreateVolume"

data CreateVolumeResponse = CreateVolumeResponse
    { cwRequestId        :: !ByteString
      -- ^ The ID of the request.
    , cwVolumeId         :: !ByteString
      -- ^ The ID of the volume.
    , cwSize             :: !ByteString
      -- ^ The size of the volume, in GiBs.
    , cwSnapshotId       :: !ByteString
      -- ^ The snapshot from which the volume was created, if applicable.
    , cwAvailabilityZone :: !ByteString
      -- ^ The Availability Zone for the volume.
    , cwStatus           :: !ByteString
      -- ^ The volume state.
    , cwCreateTime       :: !UTCTime
      -- ^ The time stamp when volume creation was initiated.
    , cwVolumeType       :: !ByteString
      -- ^ The volume type.
    , cwIops             :: !Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports.
    } deriving (Eq, Show, Generic)

instance IsXML CreateVolumeResponse where
    xmlPickler = withNS ec2NS

-- | Creates a VPC with the specified CIDR block.The smallest VPC you can create
-- uses a /28 netmask (16 IP addresses), and the largest uses a /16 netmask
-- (65,536 IP addresses). To help you decide how big to make your VPC, see
-- Your VPC and Subnets in the Amazon Virtual Private Cloud User Guide.By
-- default, each instance you launch in the VPC has the default DHCP options,
-- which includes only a default DNS server that we provide
-- (AmazonProvidedDNS).For more information about DHCP options, see DHCP
-- Options Sets in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpc.html>

data CreateVpc = CreateVpc
    { cvCidrBlock       :: !ByteString
      -- ^ The CIDR block for the VPC (for example, 10.0.0.0/16).
    , cvInstanceTenancy :: Maybe ByteString
      -- ^ The supported tenancy options for instances launched into the
      -- VPC. A value of default means that instances can be launched with
      -- any tenancy; a value of dedicated means all instances launched
      -- into the VPC are launched as dedicated tenancy instances
      -- regardless of the tenancy assigned to the instance at launch.
      -- Dedicated tenancy instances runs on single-tenant hardware.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateVpc

instance IsXML CreateVpc where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateVpc CreateVpcResponse where
    request = req GET "CreateVpc"

data CreateVpcResponse = CreateVpcResponse
    { cxRequestId :: !ByteString
      -- ^ The ID of the request.
    , cxVpc       :: !VpcType
      -- ^ Information about the VPC.
    } deriving (Eq, Show, Generic)

instance IsXML CreateVpcResponse where
    xmlPickler = withNS ec2NS

-- | Creates a VPN connection between an existing virtual private gateway and a
-- VPN customer gateway. The only supported connection type is ipsec.1. The
-- response includes information that you need to give to your network
-- administrator to configure your customer gateway. We recommend that you use
-- the command line version of this operation (ec2-create-vpn-connection),
-- which lets you get the configuration information formatted in a friendlier
-- way. For information about the command, see ec2-create-vpn-connection in
-- the Amazon Elastic Compute Cloud Command Line Reference.If you decide to
-- shut down your VPN connection for any reason and later create a new VPN
-- connection, you must reconfigure your customer gateway with the new
-- information returned from this call.For more information about VPN
-- connections, see Adding a Hardware Virtual Private Gateway to Your VPC in
-- the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnConnection.html>

data CreateVpnConnection = CreateVpnConnection
    { cvcType              :: !ByteString
      -- ^ The type of VPN connection.
    , cvcCustomerGatewayId :: !ByteString
      -- ^ The ID of the customer gateway.
    , cvcVpnGatewayId      :: !ByteString
      -- ^ The ID of the virtual private gateway.
    , cvcOptions           :: Members OptionsType
      -- ^ Indicates whether the VPN connection requires static routes. If
      -- you are creating a VPN connection for a device that does not
      -- support BGP, you must specify true.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateVpnConnection

instance IsXML CreateVpnConnection where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateVpnConnection CreateVpnConnectionResponse where
    request = req GET "CreateVpnConnection"

data CreateVpnConnectionResponse = CreateVpnConnectionResponse
    { cvcRequestId     :: !ByteString
      -- ^ The ID of the request.
    , cvcVpnConnection :: !VpnConnectionType
      -- ^ Information about the VPN connection.
    } deriving (Eq, Show, Generic)

instance IsXML CreateVpnConnectionResponse where
    xmlPickler = withNS ec2NS

-- | Creates a static route associated with a VPN connection between an existing
-- virtual private gateway and a VPN customer gateway. The static route allows
-- traffic to be routed from the virtual private gateway to the VPN customer
-- gateway.For more information about VPN connections, see Adding a Hardware
-- Virtual Private Gateway to Your VPC in the Amazon Virtual Private Cloud
-- User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnConnectionRoute.html>

data CreateVpnConnectionRoute = CreateVpnConnectionRoute
    { cvcrDestinationCidrBlock :: !ByteString
      -- ^ The CIDR block associated with the local subnet of the customer
      -- network.
    , cvcrVpnConnectionId      :: !ByteString
      -- ^ The ID of the VPN connection.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateVpnConnectionRoute

instance IsXML CreateVpnConnectionRoute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateVpnConnectionRoute CreateVpnConnectionRouteResponse where
    request = req GET "CreateVpnConnectionRoute"

data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
    { cvcrRequestId :: !ByteString
      -- ^ The ID of the request.
    , cvcrReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML CreateVpnConnectionRouteResponse where
    xmlPickler = withNS ec2NS

-- | Creates a virtual private gateway. A virtual private gateway is the
-- VPC-side endpoint for your VPN connection. You can create a virtual private
-- gateway before creating the VPC itself. For more information about virtual
-- private gateways, see Adding a Hardware Virtual Private Gateway to Your VPC
-- in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html>

data CreateVpnGateway = CreateVpnGateway
    { cvgType :: !ByteString
      -- ^ The type of VPN connection this virtual private gateway supports.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateVpnGateway

instance IsXML CreateVpnGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 CreateVpnGateway CreateVpnGatewayResponse where
    request = req GET "CreateVpnGateway"

data CreateVpnGatewayResponse = CreateVpnGatewayResponse
    { cvgRequestId  :: !ByteString
      -- ^ The ID of the request.
    , cvgVpnGateway :: !VpnGatewayType
      -- ^ Information about the virtual private gateway.
    } deriving (Eq, Show, Generic)

instance IsXML CreateVpnGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified VPN customer gateway. You must delete the VPN
-- connection before you can delete the customer gateway.For more information
-- about VPN customer gateways, see Adding a Hardware Virtual Private Gateway
-- to Your VPC in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteCustomerGateway.html>

data DeleteCustomerGateway = DeleteCustomerGateway
    { dcgCustomerGatewayId :: !ByteString
      -- ^ The ID of the customer gateway.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteCustomerGateway

instance IsXML DeleteCustomerGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteCustomerGateway DeleteCustomerGatewayResponse where
    request = req GET "DeleteCustomerGateway"

data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
    { dcgRequestId :: !ByteString
      -- ^ The ID of the request.
    , dcgReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteCustomerGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified set of DHCP options. You must disassociate the set of
-- DHCP options before you can delete it. You can disassociate the set of DHCP
-- options by associating either a new set of options or the default set of
-- options with the VPC. For more information about DHCP options sets, see
-- DHCP Options Sets in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteDhcpOptions.html>

data DeleteDhcpOptions = DeleteDhcpOptions
    { ddoDhcpOptionsId :: !ByteString
      -- ^ The ID of the DHCP options set.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteDhcpOptions

instance IsXML DeleteDhcpOptions where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteDhcpOptions DeleteDhcpOptionsResponse where
    request = req GET "DeleteDhcpOptions"

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    { ddoRequestId :: !ByteString
      -- ^ The ID of the request.
    , ddoReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteDhcpOptionsResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified Internet gateway. You must detach the Internet
-- gateway from the VPC before you can delete it. For more information about
-- your VPC and Internet gateway, see the Amazon Virtual Private Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteInternetGateway.html>

data DeleteInternetGateway = DeleteInternetGateway
    { digInternetGatewayId :: !ByteString
      -- ^ The ID of the Internet gateway.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteInternetGateway

instance IsXML DeleteInternetGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteInternetGateway DeleteInternetGatewayResponse where
    request = req GET "DeleteInternetGateway"

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    { digRequestId :: !ByteString
      -- ^ The ID of the request.
    , digReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteInternetGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified key pair, by removing the public key from Amazon EC2.
-- You must own the key pair.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteKeyPair.html>

data DeleteKeyPair = DeleteKeyPair
    { dkpKeyName :: !ByteString
      -- ^ The name of the key pair.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteKeyPair

instance IsXML DeleteKeyPair where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteKeyPair DeleteKeyPairResponse where
    request = req GET "DeleteKeyPair"

data DeleteKeyPairResponse = DeleteKeyPairResponse
    { dkpRequestId :: !ByteString
      -- ^ The ID of the request.
    , dkpReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteKeyPairResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified network ACL. You can't delete the ACL if it's
-- associated with any subnets. You can't delete the default network ACL. For
-- more information about network ACLs, see Network ACLs in the Amazon Virtual
-- Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkAcl.html>

data DeleteNetworkAcl = DeleteNetworkAcl
    { dnaNetworkAclId :: !ByteString
      -- ^ The ID of the network ACL.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteNetworkAcl

instance IsXML DeleteNetworkAcl where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteNetworkAcl DeleteNetworkAclResponse where
    request = req GET "DeleteNetworkAcl"

data DeleteNetworkAclResponse = DeleteNetworkAclResponse
    { dnaRequestId :: !ByteString
      -- ^ The ID of the request.
    , dnaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteNetworkAclResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified ingress or egress entry (rule) from the specified
-- network ACL. For more information about network ACLs, see Network ACLs in
-- the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkAclEntry.html>

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { dnaeNetworkAclId :: !ByteString
      -- ^ The ID of the network ACL.
    , dnaeRuleNumber   :: !Integer
      -- ^ The rule number for the entry to delete.
    , dnaeEgress       :: Maybe Bool
      -- ^ Indicates whether the rule is an egress rule (true) or ingress
      -- rule (false).
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteNetworkAclEntry

instance IsXML DeleteNetworkAclEntry where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteNetworkAclEntry DeleteNetworkAclEntryResponse where
    request = req GET "DeleteNetworkAclEntry"

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    { dnaeRequestId :: !ByteString
      -- ^ The ID of the request.
    , dnaeReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteNetworkAclEntryResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified network interface. You must detach the network
-- interface before you can delete it.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkInterface.html>

data DeleteNetworkInterface = DeleteNetworkInterface
    { dniNetworkInterfaceId :: !ByteString
      -- ^ The ID of the network interface.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteNetworkInterface

instance IsXML DeleteNetworkInterface where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteNetworkInterface DeleteNetworkInterfaceResponse where
    request = req GET "DeleteNetworkInterface"

data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
    { dniRequestId :: !ByteString
      -- ^ The ID of the request.
    , dniReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteNetworkInterfaceResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified placement group. You must terminate all instances in
-- the placement group before you can delete the placement group. For more
-- information about placement groups and cluster instances, see Cluster
-- Instances in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeletePlacementGroup.html>

data DeletePlacementGroup = DeletePlacementGroup
    { dpgGroupName :: !ByteString
      -- ^ The name of the placement group.
    } deriving (Eq, Show, Generic)

instance IsQuery DeletePlacementGroup

instance IsXML DeletePlacementGroup where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeletePlacementGroup DeletePlacementGroupResponse where
    request = req GET "DeletePlacementGroup"

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    { dpgRequestId :: !ByteString
      -- ^ The ID of the request.
    , dpgReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeletePlacementGroupResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified route from the specified route table. For more
-- information about route tables, see Route Tables in the Amazon Virtual
-- Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteRoute.html>

data DeleteRoute = DeleteRoute
    { drRouteTableId         :: !ByteString
      -- ^ The ID of the route table.
    , drDestinationCidrBlock :: !ByteString
      -- ^ The CIDR range for the route. The value you specify must match
      -- the CIDR for the route exactly.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteRoute

instance IsXML DeleteRoute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteRoute DeleteRouteResponse where
    request = req GET "DeleteRoute"

data DeleteRouteResponse = DeleteRouteResponse
    { drRequestId :: !ByteString
      -- ^ The ID of the request.
    , drReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteRouteResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified route table. You must disassociate the route table
-- from any subnets before you can delete it. You can't delete the main route
-- table. For more information about route tables, see Route Tables in the
-- Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteRouteTable.html>

data DeleteRouteTable = DeleteRouteTable
    { drtRouteTableId :: !ByteString
      -- ^ The ID of the route table.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteRouteTable

instance IsXML DeleteRouteTable where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteRouteTable DeleteRouteTableResponse where
    request = req GET "DeleteRouteTable"

data DeleteRouteTableResponse = DeleteRouteTableResponse
    { drtRequestId :: !ByteString
      -- ^ The ID of the request.
    , drtReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteRouteTableResponse where
    xmlPickler = withNS ec2NS

-- | Deletes a security group.A security group is for use with instances either
-- in the EC2-Classic platform or in a specific VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSecurityGroup.html>

data DeleteSecurityGroup = DeleteSecurityGroup
    { dsgGroupName :: !ByteString
      -- ^ The name of the security group.
    , dsgGroupId   :: !ByteString
      -- ^ The ID of the security group.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteSecurityGroup

instance IsXML DeleteSecurityGroup where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteSecurityGroup DeleteSecurityGroupResponse where
    request = req GET "DeleteSecurityGroup"

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    { dsgRequestId :: !ByteString
      -- ^ The ID of the request.
    , dsgReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteSecurityGroupResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified snapshot.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSnapshot.html>

data DeleteSnapshot = DeleteSnapshot
    { dsSnapshotId :: !ByteString
      -- ^ The ID of the Amazon EBS snapshot.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteSnapshot

instance IsXML DeleteSnapshot where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteSnapshot DeleteSnapshotResponse where
    request = req GET "DeleteSnapshot"

data DeleteSnapshotResponse = DeleteSnapshotResponse
    { dsRequestId :: !ByteString
      -- ^ The ID of the request.
    , dsReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteSnapshotResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the datafeed for Spot Instances. For more information about Spot
-- Instances, see Spot Instances in the Amazon Elastic Compute Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSpotDatafeedSubscription.html>

data DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    deriving (Eq, Read, Show, Generic)

instance IsQuery DeleteSpotDatafeedSubscription

instance IsXML DeleteSpotDatafeedSubscription where
    xmlPickler = xpEmpty $ Just ec2NS

instance AWSRequest EC2 DeleteSpotDatafeedSubscription DeleteSpotDatafeedSubscriptionResponse where
    request = req GET "DeleteSpotDatafeedSubscription"

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    { dsdsRequestId :: !ByteString
      -- ^ The ID of the request.
    , dsdsReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteSpotDatafeedSubscriptionResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified subnet. You must terminate all running instances in
-- the subnet before you can delete the subnet.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSubnet.html>

data DeleteSubnet = DeleteSubnet
    { dsSubnetId :: !ByteString
      -- ^ The ID of the subnet.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteSubnet

instance IsXML DeleteSubnet where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteSubnet DeleteSubnetResponse where
    request = req GET "DeleteSubnet"

data DeleteSubnetResponse = DeleteSubnetResponse
    { dtRequestId :: !ByteString
      -- ^ The ID of the request.
    , dtReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteSubnetResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified set of tags from the specified set of resources. This
-- call is designed to follow a DescribeTags call.For more information about
-- tags, see Tagging Your Resources in the Amazon Elastic Compute Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteTags.html>

data DeleteTags = DeleteTags
    { dtResourceId :: Members ByteString
      -- ^ The ID of the resource. For example, ami-1a2b3c4d. You can
      -- specify more than one resource ID.
    , dtTag        :: Members TagType
      -- ^ The tag's key. You can specify more than one tag to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteTags

instance IsXML DeleteTags where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteTags DeleteTagsResponse where
    request = req GET "DeleteTags"

data DeleteTagsResponse = DeleteTagsResponse
    { duRequestId :: !ByteString
      -- ^ The ID of the request.
    , duReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteTagsResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified Amazon EBS volume. The volume must be in the
-- available state (not attached to an instance). For more information about
-- Amazon EBS, see Amazon Elastic Block Store in the Amazon Elastic Compute
-- Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVolume.html>

data DeleteVolume = DeleteVolume
    { dvVolumeId :: !ByteString
      -- ^ The ID of the volume.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteVolume

instance IsXML DeleteVolume where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteVolume DeleteVolumeResponse where
    request = req GET "DeleteVolume"

data DeleteVolumeResponse = DeleteVolumeResponse
    { dvRequestId :: !ByteString
      -- ^ The ID of the request.
    , dvReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteVolumeResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete
-- all route tables associated with the VPC (except the default one), and so
-- on.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpc.html>

data DeleteVpc = DeleteVpc
    { dvVpcId :: !ByteString
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteVpc

instance IsXML DeleteVpc where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteVpc DeleteVpcResponse where
    request = req GET "DeleteVpc"

data DeleteVpcResponse = DeleteVpcResponse
    { dwRequestId :: !ByteString
      -- ^ The ID of the request.
    , dwReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteVpcResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified VPN connection.If you're deleting the VPC and its
-- associated components, we recommend that you detach the virtual private
-- gateway from the VPC and delete the VPC before deleting the VPN
-- connection.Another reason to use this command is if you believe that the
-- tunnel credentials for your VPN connection have been compromised. In that
-- situation, you can delete the VPN connection and create a new one that has
-- new keys, without needing to delete the VPC or virtual private gateway. If
-- you create a new VPN connection, you must reconfigure the customer gateway
-- using the new configuration information returned with the new VPN
-- connection ID.For more information about VPN connections, see Adding a
-- Hardware Virtual Private Gateway to Your VPC in the Amazon Virtual Private
-- Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnConnection.html>

data DeleteVpnConnection = DeleteVpnConnection
    { dvcVpnConnectionId :: !ByteString
      -- ^ The ID of the VPN connection.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteVpnConnection

instance IsXML DeleteVpnConnection where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteVpnConnection DeleteVpnConnectionResponse where
    request = req GET "DeleteVpnConnection"

data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
    { dvcRequestId :: !ByteString
      -- ^ The ID of the request.
    , dvcReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteVpnConnectionResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified static route associated with a VPN connection between
-- an existing virtual private gateway and a VPN customer gateway. The static
-- route allows traffic to be routed from the virtual private gateway to the
-- VPN customer gateway.For more information about VPN connections, see Adding
-- a Hardware Virtual Private Gateway to Your VPC in the Amazon Virtual
-- Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnConnectionRoute.html>

data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { dvcrDestinationCidrBlock :: !ByteString
      -- ^ The CIDR block associated with the local subnet of the customer
      -- network.
    , dvcrVpnConnectionId      :: !ByteString
      -- ^ The ID of the VPN connection.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteVpnConnectionRoute

instance IsXML DeleteVpnConnectionRoute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteVpnConnectionRoute DeleteVpnConnectionRouteResponse where
    request = req GET "DeleteVpnConnectionRoute"

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    { dvcrRequestId :: !ByteString
      -- ^ The ID of the request.
    , dvcrReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteVpnConnectionRouteResponse where
    xmlPickler = withNS ec2NS

-- | Deletes the specified virtual private gateway. We recommend that before you
-- delete a virtual private gateway, you detach it from the VPC and delete the
-- VPN connection. Note that you don't need to delete the virtual private
-- gateway if you plan to delete and recreate the VPN connection between your
-- VPC and your network.For more information about virtual private gateways,
-- see Adding a Hardware Virtual Private Gateway to Your VPC in the Amazon
-- Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnGateway.html>

data DeleteVpnGateway = DeleteVpnGateway
    { dvgVpnGatewayId :: !ByteString
      -- ^ The ID of the virtual private gateway.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteVpnGateway

instance IsXML DeleteVpnGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeleteVpnGateway DeleteVpnGatewayResponse where
    request = req GET "DeleteVpnGateway"

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    { dvgRequestId :: !ByteString
      -- ^ The ID of the request.
    , dvgReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteVpnGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Deregisters the specified AMI. After you deregister an AMI, it can't be
-- used to launch new instances.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeregisterImage.html>

data DeregisterImage = DeregisterImage
    { diImageId :: !ByteString
      -- ^ The ID of the AMI.
    } deriving (Eq, Show, Generic)

instance IsQuery DeregisterImage

instance IsXML DeregisterImage where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DeregisterImage DeregisterImageResponse where
    request = req GET "DeregisterImage"

data DeregisterImageResponse = DeregisterImageResponse
    { diRequestId :: !ByteString
      -- ^ The ID of the request.
    , diReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DeregisterImageResponse where
    xmlPickler = withNS ec2NS

-- | Describes the specified attribute of your AWS account.The following are the
-- supported account attributes.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAccountAttributes.html>

data DescribeAccountAttributes = DescribeAccountAttributes
    { daaAttributeName :: Members ByteString
      -- ^ One or more account attribute names.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAccountAttributes

instance IsXML DescribeAccountAttributes where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeAccountAttributes DescribeAccountAttributesResponse where
    request = req GET "DescribeAccountAttributes"

data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { daaRequestId           :: !ByteString
      -- ^ The ID of the request.
    , daaAccountAttributeSet :: !AccountAttributeSetItemType
      -- ^ A list of the names and values of the requested attributes, each
      -- one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAccountAttributesResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your Elastic IP addresses.An Elastic IP address is
-- for use in either the EC2-Classic platform or in a VPC. For more
-- information, see Elastic IP Addresses in the Amazon Elastic Compute Cloud
-- User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAddresses.html>

-- data AddressFilter =
--     , daDomain                     :: !ByteString
--       -- ^ Indicates whether the address is for use in a VPC.
--     , daInstance-id                :: !ByteString
--       -- ^ The instance the address is associated with (if any).
--     , daPublic-ip                  :: !ByteString
--       -- ^ The Elastic IP address.
--     , daAllocation-id              :: !ByteString
--       -- ^ The allocation ID for the address (VPC only).
--     , daAssociation-id             :: !ByteString
--       -- ^ The association ID for the address (VPC only).
--     , daNetwork-interface-id       :: !ByteString
--       -- ^ The network interface (if any) that the address is associated
--       -- with (VPC only).
--     , daNetwork-interface-owner-id :: !ByteString
--       -- ^ The owner IID.
--     , daPrivate-ip-address         :: !ByteString
--       -- ^ The private IP address associated with the Elastic IP address
--       -- (VPC only).

data DescribeAddresses = DescribeAddresses
    { daPublicIp                   :: Members ByteString
      -- ^ [EC2-Classic] One or more Elastic IP addresses.
    , daAllocationId               :: Members ByteString
      -- ^ [EC2-VPC] One or more allocation IDs.
    , daFilter                     :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAddresses

instance IsXML DescribeAddresses where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeAddresses DescribeAddressesResponse where
    request = req GET "DescribeAddresses"

data DescribeAddressesResponse = DescribeAddressesResponse
    { daRequestId    :: !ByteString
      -- ^ The ID of the request.
    , daAddressesSet :: !DescribeAddressesResponseItemType
      -- ^ A list of Elastic IP addresses, each one wrapped in an item
      -- element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAddressesResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of the Availability Zones that are available to you.
-- The results include zones only for the region you're currently using.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAvailabilityZones.html>

-- data AvailabilityZoneFilter
--     , dazMessage     :: !ByteString
--       -- ^ Information about the Availability Zone.
--     , dazRegion-name :: !ByteString
--       -- ^ The region for the Availability Zone (for example, us-east-1).
--     , dazState       :: !ByteString
--       -- ^ The state of the Availability Zone
--     , dazZone-name   :: !ByteString
--       -- ^ The name of the zone.


data DescribeAvailabilityZones = DescribeAvailabilityZones
    { dazZoneName    :: Members ByteString
      -- ^ One or more Availability Zone names.
    , dazFilter      :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAvailabilityZones

instance IsXML DescribeAvailabilityZones where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeAvailabilityZones DescribeAvailabilityZonesResponse where
    request = req GET "DescribeAvailabilityZones"

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { dazRequestId            :: !ByteString
      -- ^ The ID of the request.
    , dazAvailabilityZoneInfo :: !AvailabilityZoneItemType
      -- ^ A list of Availability Zones, each one wrapped in an item
      -- element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAvailabilityZonesResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your bundling tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeBundleTasks.html>

-- data BundleFilter
--     , dbtBundle-id     :: !ByteString
--       -- ^ The ID of the bundle task.
--     , dbtError-code    :: !ByteString
--       -- ^ If the task failed, the error code returned.
--     , dbtError-message :: !ByteString
--       -- ^ If the task failed, the error message returned.
--     , dbtInstance-id   :: !ByteString
--       -- ^ The ID of the instance that was bundled.
--     , dbtProgress      :: !ByteString
--       -- ^ The level of task completion, as a percentage (for example, 20%).
--     , dbtS3-bucket     :: !ByteString
--       -- ^ The Amazon S3 bucket to store the AMI.
--     , dbtS3-prefix     :: !ByteString
--       -- ^ The beginning of the AMI name.
--     , dbtStart-time    :: !UTCTime
--       -- ^ The time the task started (for example,
--       -- 2008-09-15T17:15:20.000Z).
--     , dbtState         :: !ByteString
--       -- ^ The state of the task.
--     , dbtUpdate-time   :: !UTCTime
--       -- ^ The time of the most recent update for the task (for example,
--       -- 2008-09-15T17:15:20.000Z).


data DescribeBundleTasks = DescribeBundleTasks
    { dbtBundleId      :: Members ByteString
      -- ^ One or more bundle task IDs.
    , dbtFilter        :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeBundleTasks

instance IsXML DescribeBundleTasks where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeBundleTasks DescribeBundleTasksResponse where
    request = req GET "DescribeBundleTasks"

data DescribeBundleTasksResponse = DescribeBundleTasksResponse
    { dbtRequestId              :: !ByteString
      -- ^ The ID of the request.
    , dbtBundleInstanceTasksSet :: !BundleInstanceTaskType
      -- ^ A list of bundle tasks, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeBundleTasksResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your conversion tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeConversionTasks.html>

data DescribeConversionTasks = DescribeConversionTasks
    { dctConversionTaskId :: Members ByteString
      -- ^ One or more conversion task IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeConversionTasks

instance IsXML DescribeConversionTasks where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeConversionTasks DescribeConversionTasksResponse where
    request = req GET "DescribeConversionTasks"

data DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { dctConversionTasks :: !ConversionTaskType
      -- ^ A list of conversion tasks, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeConversionTasksResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your VPN customer gateways.For more information
-- about VPN customer gateways, see Adding a Hardware Virtual Private Gateway
-- to Your VPC in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeCustomerGateways.html>

-- data GatewayFilter
--     , dchBgp-asn             :: !ByteString
--       -- ^ The customer gateway's Border Gateway Protocol (BGP) Autonomous
--       -- System Number (ASN).
--     , dchCustomer-gateway-id :: !ByteString
--       -- ^ The ID of the customer gateway.
--     , dchIp-address          :: !ByteString
--       -- ^ The IP address of the customer gateway's Internet-routable
--       -- external interface (for example, 12.1.2.3).
--     , dchState               :: !ByteString
--       -- ^ The state of the customer gateway.
--     , dchType                :: !ByteString
--       -- ^ The type of customer gateway. Currently the only supported type
--       -- is ipsec.1.
--     , dchTag-key             :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , dchTag-value           :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , dchTag:                :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , dchKey                 :: !ByteString
--       -- ^ 

data DescribeCustomerGateways = DescribeCustomerGateways
    { dchCustomerGatewayId   :: Members ByteString
      -- ^ One or more customer gateway IDs.
    , dchFilter              :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeCustomerGateways

instance IsXML DescribeCustomerGateways where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeCustomerGateways DescribeCustomerGatewaysResponse where
    request = req GET "DescribeCustomerGateways"

data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
    { dchRequestId          :: !ByteString
      -- ^ The ID of the request.
    , dchCustomerGatewaySet :: !CustomerGatewayType
      -- ^ A list of customer gateways, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeCustomerGatewaysResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your DHCP options sets.For more information about
-- DHCP options sets, see DHCP Options Sets in the Amazon Virtual Private
-- Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeDhcpOptions.html>

-- data DhcpFilter
--     , ddpDhcp-options-id :: !ByteString
--       -- ^ The ID of a set of DHCP options.
--     , ddpKey             :: !ByteString
--       -- ^ The key for one of the options (for example, domain-name).
--     , ddpValue           :: !ByteString
--       -- ^ The value for one of the options.
--     , ddpTag-key         :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , ddpTag-value       :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , ddpTag:            :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , ddqKey             :: !ByteString
--       -- ^ 

data DescribeDhcpOptions = DescribeDhcpOptions
    { ddpDhcpOptionsId   :: Members ByteString
      -- ^ The IDs of one or more DHCP options sets.
    , ddpFilter          :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeDhcpOptions

instance IsXML DescribeDhcpOptions where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeDhcpOptions DescribeDhcpOptionsResponse where
    request = req GET "DescribeDhcpOptions"

data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
    { ddqRequestId      :: !ByteString
      -- ^ The ID of the request.
    , ddqDhcpOptionsSet :: !DhcpOptionsType
      -- ^ A list of DHCP options sets, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeDhcpOptionsResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your export tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeExportTasks.html>

data DescribeExportTasks = DescribeExportTasks
    { detExportTaskId :: Members ByteString
      -- ^ One or more export task IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeExportTasks

instance IsXML DescribeExportTasks where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeExportTasks DescribeExportTasksResponse where
    request = req GET "DescribeExportTasks"

data DescribeExportTasksResponse = DescribeExportTasksResponse
    { detRequestId     :: !ByteString
      -- ^ The ID of the request.
    , detExportTaskSet :: !ExportTaskResponseType
      -- ^ A list of export tasks, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeExportTasksResponse where
    xmlPickler = withNS ec2NS

-- | Describes an attributes of an AMI. You can specify only one attribute at a
-- time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImageAttribute.html>

data DescribeImageAttribute = DescribeImageAttribute
    { diaImageId   :: !ByteString
      -- ^ The ID of the AMI.
    , diaAttribute :: !ByteString
      -- ^ The AMI attribute.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeImageAttribute

instance IsXML DescribeImageAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeImageAttribute DescribeImageAttributeResponse where
    request = req GET "DescribeImageAttribute"

data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { diaRequestId          :: !ByteString
      -- ^ The ID of the request.
    , dibImageId            :: !ByteString
      -- ^ The ID of the AMI.
    , dibLaunchPermission   :: !LaunchPermissionItemType
      -- ^ A list of launch permissions, each one wrapped in an item
      -- element.
    , dibProductCodes       :: !ProductCodeItemType
      -- ^ A list of product codes, each one wrapped in an item element that
      -- contains a product code and a product code type.
    , dibKernel             :: !ByteString
      -- ^ The kernel ID, wrapped in a value element.
    , dibRamdisk            :: !ByteString
      -- ^ The RAM disk ID, wrapped in a value element.
    , dibDescription        :: !ByteString
      -- ^ A user-created description for the AMI, wrapped in a value
      -- element.
    , dibBlockDeviceMapping :: !BlockDeviceMappingItemType
      -- ^ One or more block device mapping entries, each one wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeImageAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of the images (AMIs, AKIs, and ARIs) available to
-- you. Images available to you include public images, private images that you
-- own, and private images owned by other AWS accounts but for which you have
-- explicit launch permissions.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImages.html>

-- data ImageFilter
--     , djArchitecture         :: !ByteString
--       -- ^ The image architecture.
--     , djBlock-device-mapping :: Members block-device-mappingType
--       -- ^ Whether the Amazon EBS volume is deleted on instance termination.
--     , djDescription          :: !ByteString
--       -- ^ The description of the image (provided during image creation).
--     , djImage-id             :: !ByteString
--       -- ^ The ID of the image.
--     , djImage-type           :: !ByteString
--       -- ^ The image type.
--     , djIs-public            :: !Bool
--       -- ^ Whether the image is public.
--     , djKernel-id            :: !ByteString
--       -- ^ The kernel ID.
--     , djManifest-location    :: !ByteString
--       -- ^ The location of the image manifest.
--     , djName                 :: !ByteString
--       -- ^ The name of the AMI (provided during image creation).
--     , djOwner-alias          :: !ByteString
--       -- ^ The AWS account alias (for example, amazon).
--     , djOwner-id             :: !ByteString
--       -- ^ The AWS account ID of the image owner.
--     , djPlatform             :: !ByteString
--       -- ^ The platform. To only list Windows-based AMIs, use windows.
--       -- Otherwise, leave blank.
--     , djProduct-code         :: !ByteString
--       -- ^ The product code.
--     , djRamdisk-id           :: !ByteString
--       -- ^ The RAM disk ID.
--     , djRoot-device-name     :: !ByteString
--       -- ^ The name of the root device volume (for example, /dev/sda1).
--     , djRoot-device-type     :: !ByteString
--       -- ^ The type of the root device volume.
--     , djState                :: !ByteString
--       -- ^ The state of the image.
--     , djState-reason-code    :: !ByteString
--       -- ^ The reason code for the state change.
--     , djState-reason-message :: !ByteString
--       -- ^ The message for the state change.
--     , djTag-key              :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , djTag-value            :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , djTag:                 :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , djKey                  :: !ByteString
--       -- ^ The virtualization type.
--     , djVirtualization-type  :: !ByteString
--       -- ^ The hypervisor type.
--     , djHypervisor           :: !ByteString
--       -- ^ 

data DescribeImages = DescribeImages
    { diExecutableBy         :: Members ByteString
      -- ^ Describes the images for which the specified user has explicit
      -- launch permissions. The user ID can be an AWS account ID, self to
      -- return images for which the sender of the request has explicit
      -- launch permissions, or all to return AMIs with public launch
      -- permissions.
    , djImageId              :: Members ByteString
      -- ^ One or more image IDs.
    , djOwner                :: Members ByteString
      -- ^ Describes images owned by the specified owners. Use the IDs
      -- amazon, aws-marketplace, and self to describe images owned by
      -- Amazon, AWS Marketplace, or you, respectively.
    , djFilter               :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeImages

instance IsXML DescribeImages where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeImages DescribeImagesResponse where
    request = req GET "DescribeImages"

data DescribeImagesResponse = DescribeImagesResponse
    { djRequestId :: !ByteString
      -- ^ The ID of the request.
    , djImagesSet :: !DescribeImagesResponseItemType
      -- ^ A list of images, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeImagesResponse where
    xmlPickler = withNS ec2NS

-- | Describes an attribute of the specified instance. You can specify only one
-- attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceAttribute.html>

data DescribeInstanceAttribute = DescribeInstanceAttribute
    { diaInstanceId :: !ByteString
      -- ^ The ID of the instance.
    , dibAttribute  :: !ByteString
      -- ^ The instance attribute.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeInstanceAttribute

instance IsXML DescribeInstanceAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeInstanceAttribute DescribeInstanceAttributeResponse where
    request = req GET "DescribeInstanceAttribute"

data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
    { dibRequestId                         :: !ByteString
      -- ^ The ID of the request.
    , dibInstanceId                        :: !ByteString
      -- ^ The ID of the instance.
    , dicBlockDeviceMapping                :: !InstanceBlockDeviceMappingResponseItemType
      -- ^ The block device mapping of the instance.
    , dicDisableApiTermination             :: !Bool
      -- ^ If the value is true, you can't terminate the instance through
      -- the Amazon EC2 console, CLI, or API; otherwise, you can.
    , dicEbsOptimized                      :: !Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O.
    , dicGroupSet                          :: !GroupItemType
      -- ^ The security groups associated with the instance.
    , dicInstanceInitiatedShutdownBehavior :: !ByteString
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , dicInstanceType                      :: !ByteString
      -- ^ The instance type.
    , dicKernel                            :: !ByteString
      -- ^ The kernel ID.
    , dicProductCodes                      :: !ProductCodesSetItemType
      -- ^ A list of product codes.
    , dicRamdisk                           :: !ByteString
      -- ^ The RAM disk ID.
    , dicRootDeviceName                    :: !ByteString
      -- ^ The name of the root device (for example, /dev/sda1).
    , dicSourceDestCheck                   :: !Bool
      -- ^ Indicates whether source/destination checking is enabled. A value
      -- of true means checking is enabled, and false means checking is
      -- disabled. This value must be false for a NAT instance to perform
      -- NAT.
    , dicUserData                          :: !ByteString
      -- ^ The Base64-encoded MIME user data.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeInstanceAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your instances.If you specify one or more instance
-- IDs, Amazon EC2 returns information for those instances. If you do not
-- specify instance IDs, Amazon EC2 returns information for all relevant
-- instances. If you specify an invalid instance ID, an error is returned. If
-- you specify an instance that you do not own, it is not included in the
-- returned results.Recently terminated instances might appear in the returned
-- results. This interval is usually less than one hour.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstances.html>

-- data InstanceFilter
--     , diArchitecture                       :: !ByteString
--       -- ^ The instance architecture.
--     , diAvailability-zone                  :: !ByteString
--       -- ^ The Availability Zone of the instance.
--     , diBlock-device-mapping               :: Members block-device-mappingType
--       -- ^ The attach time for an Amazon EBS volume mapped to the instance
--       -- (for example, 2010-09-15T17:15:20.000Z)
--     , diClient-token                       :: !ByteString
--       -- ^ The idempotency token you provided when you launched the
--       -- instance.
--     , diDns-name                           :: !ByteString
--       -- ^ The public DNS name of the instance.
--     , diGroup-id                           :: !ByteString
--       -- ^ The ID of the security group for the instance. If the instance is
--       -- in EC2-Classic or a default VPC, you can use group-name instead.
--     , diGroup-name                         :: !ByteString
--       -- ^ The name of the security group for the instance. If the instance
--       -- is in a nondefault VPC, you must use group-id instead.
--     , diImage-id                           :: !ByteString
--       -- ^ The ID of the image used to launch the instance.
--     , diInstance-id                        :: !ByteString
--       -- ^ The ID of the instance.
--     , diInstance-lifecycle                 :: !ByteString
--       -- ^ Indicates whether this is a Spot Instance.
--     , diInstance-state-code                :: !Integer
--       -- ^ The state of the instance. The high byte is an opaque internal
--       -- value and should be ignored. The low byte is set based on the
--       -- state represented.
--     , diInstance-state-name                :: !ByteString
--       -- ^ The state of the instance.
--     , diInstance-type                      :: !ByteString
--       -- ^ The type of instance (for example, m1.small).
--     , diInstance                           :: Members instanceType
--       -- ^ The ID of the security group for the instance. If the instance is
--       -- in EC2-Classic or a default VPC, you can use instance.group-name
--       -- instead.
--     , diIp-address                         :: !ByteString
--       -- ^ The public IP address of the instance.
--     , diKernel-id                          :: !ByteString
--       -- ^ The kernel ID.
--     , diKey-name                           :: !ByteString
--       -- ^ The name of the key pair used when the instance was launched.
--     , diLaunch-index                       :: !ByteString
--       -- ^ When launching multiple instances, this is the index for the
--       -- instance in the launch group (for example, 0, 1, 2, and so on).
--     , diLaunch-time                        :: !UTCTime
--       -- ^ The time the instance was launched (for example,
--       -- 2010-08-07T11:54:42.000Z).
--     , diMonitoring-state                   :: !ByteString
--       -- ^ Indicates whether monitoring is enabled for the instance.
--     , diOwner-id                           :: !ByteString
--       -- ^ The AWS account ID of the instance owner.
--     , diPlacement-group-name               :: !ByteString
--       -- ^ The name of the placement group for the instance.
--     , diPlatform                           :: !ByteString
--       -- ^ The platform. Use windows if you have Windows based instances;
--       -- otherwise, leave blank.
--     , diPrivate-dns-name                   :: !ByteString
--       -- ^ The private DNS name of the instance.
--     , diPrivate-ip-address                 :: !ByteString
--       -- ^ The private IP address of the instance.
--     , diProduct-code                       :: !ByteString
--       -- ^ The product code associated with the AMI used to launch the
--       -- instance.
--     , diRamdisk-id                         :: !ByteString
--       -- ^ The RAM disk ID.
--     , diReason                             :: !ByteString
--       -- ^ The reason for the current state of the instance (for example,
--       -- shows "User Initiated [date]" when you stop or terminate the
--       -- instance). Similar to the state-reason-code filter.
--     , diRequester-id                       :: !ByteString
--       -- ^ The ID of the entity that launched the instance on your behalf
--       -- (for example, AWS Management Console, Auto Scaling, and so on)
--     , diReservation-id                     :: !ByteString
--       -- ^ The ID of the instance's reservation. A reservation ID is created
--       -- any time you launch an instance. A reservation ID has a
--       -- one-to-one relationship with an instance launch request, but can
--       -- be associated with more than one instance if you launch multiple
--       -- instances using the same launch request. For example, if you
--       -- launch one instance, you'll get one reservation ID. If you launch
--       -- ten instances using the same launch request, you'll also get one
--       -- reservation ID.
--     , diRoot-device-name                   :: !ByteString
--       -- ^ The name of the root device for the instance (for example,
--       -- /dev/sda1).
--     , diRoot-device-type                   :: !ByteString
--       -- ^ The type of root device the instance uses.
--     , diSource-dest-check                  :: !Bool
--       -- ^ Indicates whether the instance performs source/destination
--       -- checking. A value of true means that checking is enabled, and
--       -- false means checking is disabled. The value must be false for the
--       -- instance to perform network address translation (NAT) in your
--       -- VPC.
--     , diSpot-instance-request-id           :: !ByteString
--       -- ^ The ID of the Spot Instance request.
--     , diState-reason-code                  :: !ByteString
--       -- ^ The reason code for the state change.
--     , diState-reason-message               :: !ByteString
--       -- ^ A message that describes the state change.
--     , diSubnet-id                          :: !ByteString
--       -- ^ The ID of the subnet for the instance.
--     , diTag-key                            :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , diTag-value                          :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , diTag:                               :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , diKey                                :: !ByteString
--       -- ^ The virtualization type of the instance.
--     , diVirtualization-type                :: !ByteString
--       -- ^ The ID of the VPC the instance is running in.
--     , diVpc-id                             :: !ByteString
--       -- ^ The hypervisor type of the instance.
--     , diHypervisor                         :: !ByteString
--       -- ^ The description of the network interface.
--     , diNetwork-interface                  :: Members network-interfaceType
--       -- ^ The ID of the subnet for the network interface.
--     , diNetwork-interface-private-dns-name :: !Bool
--       -- ^ Whether the network interface performs source/destination
--       -- checking. A value of true means checking is enabled, and false
--       -- means checking is disabled. The value must be false for the
--       -- network interface to perform network address translation (NAT) in
--       -- your VPC.
--     , diAssociation                        :: Members associationType
--       -- ^ The owner of the Elastic IP address associated with the network
--       -- interface.

data DescribeInstances = DescribeInstances
    { diInstanceId :: Members ByteString
      -- ^ One or more instance IDs.
    , diFilter     :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeInstances

instance IsXML DescribeInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeInstances DescribeInstancesResponse where
    request = req GET "DescribeInstances"

data DescribeInstancesResponse = DescribeInstancesResponse
    { dkRequestId      :: !ByteString
      -- ^ The ID of the request.
    , dkReservationSet :: !ReservationInfoType
      -- ^ A list of reservations, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Describes the status of one or more instances, including any scheduled
-- events.Instance status has two main components: Instance status provides
-- information about four types of scheduled events for an instance that may
-- require your attention: When your instance is retired, it will either be
-- terminated (if its root device type is the instance-store) or stopped (if
-- its root device type is an EBS volume). Instances stopped due to retirement
-- will not be restarted, but you can do so manually. You can also avoid
-- retirement of EBS-backed instances by manually restarting your instance
-- when its event code is instance-retirement. This ensures that your instance
-- is started on a different underlying host.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceStatus.html>

-- data InstanceStatusFilter
--     , disAvailability-zone   :: !ByteString
--       -- ^ The Availability Zone of the instance.
--     , disEvent               :: Members eventType
--       -- ^ The code identifying the type of event.
--     , disInstance-state-name :: !ByteString
--       -- ^ The state of the instance.
--     , disInstance-state-code :: !Integer
--       -- ^ A code representing the state of the instance. The high byte is
--       -- an opaque internal value and should be ignored. The low byte is
--       -- set based on the state represented
--     , disSystem-status       :: Members system-statusType
--       -- ^ The system status of the instance.
--     , disInstance-status     :: Members instance-statusType
--       -- ^ The status of the instance.

data DescribeInstanceStatus = DescribeInstanceStatus
    { disInstanceId          :: Maybe ByteString
      -- ^ One or more instance IDs.
    , disIncludeAllInstances :: Maybe Bool
      -- ^ When true, includes the health status for all instances. When
      -- false, includes the health status for running instances only.
    , disMaxResults          :: Maybe Integer
      -- ^ The maximum number of paginated instance items per response.
    , disNextToken           :: Maybe ByteString
      -- ^ The next paginated set of results to return.
    , disFilter              :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeInstanceStatus

instance IsXML DescribeInstanceStatus where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeInstanceStatus DescribeInstanceStatusResponse where
    request = req GET "DescribeInstanceStatus"

data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
    { disRequestId         :: !ByteString
      -- ^ The ID of the request.
    , disInstanceStatusSet :: !InstanceStatusItemType
      -- ^ A list of instances status descriptions, each one wrapped in an
      -- item element.
    , ditNextToken         :: !ByteString
      -- ^ The next paginated set of results to return.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeInstanceStatusResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your Internet gateways.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInternetGateways.html>

-- data GatewayFilter
--     , dihAttachment          :: Members attachmentType
--       -- ^ The current state of the attachment between the gateway and the
--       -- VPC. Returned only if a VPC is attached.
--     , dihInternet-gateway-id :: !ByteString
--       -- ^ The ID of the Internet gateway.
--     , dihTag-key             :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , dihTag-value           :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , dihTag:                :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , dihKey                 :: !ByteString
--       -- ^ 

data DescribeInternetGateways = DescribeInternetGateways
    { dihInternetGatewayId   :: Members ByteString
      -- ^ One or more Internet gateway IDs.
    , dihFilter              :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeInternetGateways

instance IsXML DescribeInternetGateways where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeInternetGateways DescribeInternetGatewaysResponse where
    request = req GET "DescribeInternetGateways"

data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { dihRequestId          :: !ByteString
      -- ^ The ID of the request.
    , dihInternetGatewaySet :: !InternetGatewayType
      -- ^ A list of Internet gateways, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeInternetGatewaysResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your key pairs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeKeyPairs.html>

-- data KeyPairFilter
--     , dkqFingerprint :: !ByteString
--       -- ^ The fingerprint of the key pair.
--     , dkqKey-name    :: !ByteString
--       -- ^ The name of the key pair.

data DescribeKeyPairs = DescribeKeyPairs
    { dkqKeyName :: Members ByteString
      -- ^ One or more key pair names.
    , dkqFilter  :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeKeyPairs

instance IsXML DescribeKeyPairs where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeKeyPairs DescribeKeyPairsResponse where
    request = req GET "DescribeKeyPairs"

data DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { dkqRequestId :: !ByteString
      -- ^ The ID of the request.
    , dkqKeySet    :: !DescribeKeyPairsResponseItemType
      -- ^ A list of key pairs, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeKeyPairsResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your network ACLs.For more information about
-- network ACLs, see Network ACLs in the Amazon Virtual Private Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkAcls.html>

-- data NetworkAclFilter
--     , dnbAssociation    :: Members associationType
--       -- ^ The ID of an association ID for the ACL.
--     , dnbDefault        :: !Bool
--       -- ^ Indicates whether the ACL is the default network ACL for the VPC.
--     , dnbEntry          :: Members entryType
--       -- ^ The CIDR range specified in the entry.
--     , dnbNetwork-acl-id :: !ByteString
--       -- ^ The ID of the network ACL.
--     , dnbTag-key        :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , dnbTag-value      :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , dnbTag:           :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , dnbKey            :: !ByteString
--       -- ^ The ID of the VPC for the network ACL.
--     , dnbVpc-id         :: !ByteString
--       -- ^ 

data DescribeNetworkAcls = DescribeNetworkAcls
    { dnbNetworkAclId   :: Members ByteString
      -- ^ One or more network ACL IDs.
    , dnbFilter         :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeNetworkAcls

instance IsXML DescribeNetworkAcls where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeNetworkAcls DescribeNetworkAclsResponse where
    request = req GET "DescribeNetworkAcls"

data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse
    { dnbRequestId     :: !ByteString
      -- ^ The ID of the request.
    , dnbNetworkAclSet :: !NetworkAclType
      -- ^ A list of network ACLs, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeNetworkAclsResponse where
    xmlPickler = withNS ec2NS

-- | Describes a network interface attribute. You can specify only one attribute
-- at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaceAttribute.html>

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { dniaNetworkInterfaceId :: !ByteString
      -- ^ The ID of the network interface.
    , dniaAttribute          :: !ByteString
      -- ^ The attribute of the network interface.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeNetworkInterfaceAttribute

instance IsXML DescribeNetworkInterfaceAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeNetworkInterfaceAttribute DescribeNetworkInterfaceAttributeResponse where
    request = req GET "DescribeNetworkInterfaceAttribute"

data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { dniaRequestId          :: !ByteString
      -- ^ The ID of the request.
    , dnibNetworkInterfaceId :: !ByteString
      -- ^ The ID of the network interface.
    , dnibDescription        :: !ByteString
      -- ^ The description of the network interface.
    , dnibSourceDestCheck    :: !Bool
      -- ^ Indicates whether source/destination checking is enabled.
    , dnibGroupSet           :: !GroupItemType
      -- ^ The security groups associated with the network interface.
    , dnibAttachment         :: !NetworkInterfaceAttachmentType
      -- ^ The attachment (if any) of the network interface.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeNetworkInterfaceAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your network interfaces.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaces.html>

-- data NetworkInterfaceFilter
    -- , dnjAddresses            :: Members addressesType
    --   -- ^ The private IP addresses associated with the network interface.
    -- , dnjAssociation          :: Members associationType
    --   -- ^ The association ID returned when the network interface was
    --   -- associated with an IP address.
    -- , dnjAttachment           :: Members attachmentType
    --   -- ^ The ID of the interface attachment.
    -- , dnjAvailability-zone    :: !ByteString
    --   -- ^ The Availability Zone of the network interface.
    -- , dnjDescription          :: !ByteString
    --   -- ^ The description of the network interface.
    -- , dnjGroup-id             :: !ByteString
    --   -- ^ The ID of a security group associated with the network interface.
    -- , dnjGroup-name           :: !ByteString
    --   -- ^ The name of a security group associated with the network
    --   -- interface.
    -- , dnjMac-address          :: !ByteString
    --   -- ^ The MAC address of the network interface.
    -- , dnjNetwork-interface-id :: !ByteString
    --   -- ^ The ID of the network interface.
    -- , dnjOwner-id             :: !ByteString
    --   -- ^ The AWS account ID of the network interface owner.
    -- , dnjPrivate-ip-address   :: !ByteString
    --   -- ^ The private IP address or addresses of the network interface.
    -- , dnjPrivate-dns-name     :: !ByteString
    --   -- ^ The private DNS name of the network interface.
    -- , dnjRequester-id         :: !ByteString
    --   -- ^ The ID of the entity that launched the instance on your behalf
    --   -- (for example, AWS Management Console, Auto Scaling, and so on).
    -- , dnjRequester-managed    :: !Bool
    --   -- ^ Indicates whether the network interface is being managed by an
    --   -- AWS service (for example, AWS Management Console, Auto Scaling,
    --   -- and so on).
    -- , dnjSource-dest-check    :: !Bool
    --   -- ^ Indicates whether the network interface performs
    --   -- source/destination checking. A value of true means checking is
    --   -- enabled, and false means checking is disabled. The value must be
    --   -- false for the network interface to perform Network Address
    --   -- Translation (NAT) in your VPC.
    -- , dnjStatus               :: !ByteString
    --   -- ^ The status of the network interface. If the network interface is
    --   -- not attached to an instance, the status shows available; if a
    --   -- network interface is attached to an instance the status shows
    --   -- in-use.
    -- , dnjSubnet-id            :: !ByteString
    --   -- ^ The ID of the subnet for the network interface.
    -- , dnjTag-key              :: !ByteString
    --   -- ^ The key of a tag assigned to the resource. This filter is
    --   -- independent of the tag-value filter. For example, if you use both
    --   -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
    --   -- get any resources assigned both the tag key Purpose (regardless
    --   -- of what the tag's value is), and the tag value X (regardless of
    --   -- what the tag's key is). If you want to list only resources where
    --   -- Purpose is X, see the tag:key filter.
    -- , dnjTag-value            :: !ByteString
    --   -- ^ The value of a tag assigned to the resource. This filter is
    --   -- independent of the tag-key filter.
    -- , dnjTag:                 :: !ByteString
    --   -- ^ Filters the response based on a specific tag/value combination.
    -- , dnjKey                  :: !ByteString
    --   -- ^ The ID of the VPC for the network interface.
    -- , dnjVpc-id               :: !ByteString
    --   -- ^ 

data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { dnjNetworkInterfaceId   :: Members ByteString
      -- ^ One or more network interface IDs.
    , dnjFilter               :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeNetworkInterfaces

instance IsXML DescribeNetworkInterfaces where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeNetworkInterfaces DescribeNetworkInterfacesResponse where
    request = req GET "DescribeNetworkInterfaces"

data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
    { dnjRequestId           :: !ByteString
      -- ^ The ID of the request.
    , dnjNetworkInterfaceSet :: !NetworkInterfaceType
      -- ^ Information about the network interfaces, each one wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeNetworkInterfacesResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your placement groups. For more information about
-- placement groups and cluster instances, see Cluster Instances in the Amazon
-- Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePlacementGroups.html>

-- data PlacementGroupFilter
--     , dphGroup-name :: !ByteString
--       -- ^ The name of the placement group.
--     , dphState      :: !ByteString
--       -- ^ The state of the placement group.
--     , dphStrategy   :: !ByteString
--       -- ^ The strategy of the placement group.


data DescribePlacementGroups = DescribePlacementGroups
    { dphGroupName  :: Members ByteString
      -- ^ One or more placement group names.
    , dphFilter     :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribePlacementGroups

instance IsXML DescribePlacementGroups where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribePlacementGroups DescribePlacementGroupsResponse where
    request = req GET "DescribePlacementGroups"

data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { dphRequestId         :: !ByteString
      -- ^ The ID of the request.
    , dphPlacementGroupSet :: !PlacementGroupInfoType
      -- ^ A list of placement groups, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribePlacementGroupsResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more regions that are currently available to you.For a
-- list of the regions supported by Amazon EC2, see Regions and Endpoints.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html>

-- data RegionFilter
--     , drEndpoint    :: !ByteString
--       -- ^ The endpoint of the region (for example,
--       -- ec2.us-east-1.amazonaws.com).
--     , drRegion-name :: !ByteString
--       -- ^ The name of the region.

data DescribeRegions = DescribeRegions
    { drRegionName  :: Members ByteString
      -- ^ One or more region names.
    , drFilter      :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeRegions

instance IsXML DescribeRegions where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeRegions DescribeRegionsResponse where
    request = req GET "DescribeRegions"

data DescribeRegionsResponse = DescribeRegionsResponse
    { dxRequestId  :: !ByteString
      -- ^ The ID of the request.
    , dxRegionInfo :: !RegionItemType
      -- ^ A list of regions, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeRegionsResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of the Reserved Instances that you purchased.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstances.html>

-- data ReservedInstanceFilter
--     , driAvailability-zone     :: !ByteString
--       -- ^ The Availability Zone where the Reserved Instance can be used.
--     , driDuration              :: !Integer
--       -- ^ The duration of the Reserved Instance (one year or three years),
--       -- in seconds.
--     , driFixed-price           :: !Double
--       -- ^ The purchase price of the Reserved Instance (for example, 9800.0)
--     , driInstance-type         :: !ByteString
--       -- ^ The instance type on which the Reserved Instance can be used.
--     , driProduct-description   :: !ByteString
--       -- ^ The product description of the Reserved Instance.
--     , driReserved-instances-id :: !ByteString
--       -- ^ The ID of the Reserved Instance.
--     , driStart                 :: !UTCTime
--       -- ^ The time at which the Reserved Instance purchase request was
--       -- placed (for example, 2010-08-07T11:54:42.000Z).
--     , driState                 :: !ByteString
--       -- ^ The state of the Reserved Instance.
--     , driTag-key               :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , driTag-value             :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , driTag:                  :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , driKey                   :: !Double
--       -- ^ The usage price of the Reserved Instance, per hour (for example,
--       -- 0.84)
--     , driUsage-price           :: !ByteString
--       -- ^ 

data DescribeReservedInstances = DescribeReservedInstances
    { driReservedInstancesId   :: Members ByteString
      -- ^ One or more Reserved Instance IDs.
    , driOfferingType          :: Maybe ByteString
      -- ^ The Reserved Instance offering type.
    , driFilter                :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeReservedInstances

instance IsXML DescribeReservedInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeReservedInstances DescribeReservedInstancesResponse where
    request = req GET "DescribeReservedInstances"

data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
    { driRequestId            :: !ByteString
      -- ^ The ID of the request.
    , driReservedInstancesSet :: !DescribeReservedInstancesResponseSetItemType
      -- ^ A list of Reserved Instances, each one wrapped in an item
      -- element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeReservedInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Describes your account's Reserved Instance listings in the Reserved
-- Instance Marketplace. This call returns information, such as the ID of the
-- Reserved Instance to which a listing is associated.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesListings.html>

-- data ReservedInstanceFilter
--     , drilStatus                        :: !ByteString
--       -- ^ Status of the Reserved Instance listing.
--     , drilStatus-message                :: !ByteString
--       -- ^ Reason for the status.
--     , drilReserved-instances-listing-id :: !ByteString
--       -- ^ The ID of the Reserved Instances listing.
--     , drilReserved-instances-id         :: !ByteString
--       -- ^ The ID of the Reserved Instances.

data DescribeReservedInstancesListings = DescribeReservedInstancesListings
    { drilReservedInstancesListingId :: Members DescribeReservedInstancesListingSetItemType
      -- ^ The information about the Reserved Instance listing wrapped in an
      -- item element.
    , drilReservedInstancesId        :: Members DescribeReservedInstancesSetItemType
      -- ^ The set of Reserved Instances IDs which are used to see
      -- associated listings.
    , drilFilter                     :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeReservedInstancesListings

instance IsXML DescribeReservedInstancesListings where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeReservedInstancesListings DescribeReservedInstancesListingsResponse where
    request = req GET "DescribeReservedInstancesListings"

data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
    { drilRequestId                    :: !ByteString
      -- ^ The ID of the request.
    , drilReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSetItemType
      -- ^ The Reserved Instance listing information wrapped in an item
      -- element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeReservedInstancesListingsResponse where
    xmlPickler = withNS ec2NS

-- | Describes Reserved Instance offerings that are available for purchase.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesOfferings.html>

data ReservedInstancesOfferingFilter
    -- , drioInstanceTenancy                :: Maybe ByteString
    --   -- ^ The tenancy of the Reserved Instance offering. A Reserved
    --   -- Instance with tenancy of dedicated will run on single-tenant
    --   -- hardware and can only be launched within a VPC.
    -- , drioOfferingType                   :: Maybe ByteString
    --   -- ^ The Reserved Instance offering type.
    -- , drioIncludeMarketplace             :: Maybe Bool
    --   -- ^ Include Marketplace offerings in the response.
    -- , drioMinDuration                    :: Maybe Integer
    --   -- ^ Minimum duration (in seconds) to filter when searching for
    --   -- offerings.
    -- , drioMaxDuration                    :: Maybe Integer
    --   -- ^ Maximum duration (in seconds) to filter when searching for
    --   -- offerings.
    -- , drioMaxInstanceCount               :: Maybe Integer
    --   -- ^ Maximum number of instances to filter when searching for
    --   -- offerings.
    -- , drioNextToken                      :: Maybe ByteString
    --   -- ^ Token to use when requesting the next paginated set of offerings.
    -- , drioMaxResults                     :: Maybe Integer
    --   -- ^ Maximum number of offerings to return.
    -- , drioAvailability-zone              :: !ByteString
    --   -- ^ The Availability Zone where the Reserved Instance can be used.
    -- , drioDuration                       :: !Integer
    --   -- ^ The duration of the Reserved Instance (for example, one year or
    --   -- three years), in seconds.
    -- , drioFixed-price                    :: !Double
    --   -- ^ The purchase price of the Reserved Instance (for example, 9800.0)
    -- , drioInstance-type                  :: !ByteString
    --   -- ^ The Amazon EC2 instance type on which the Reserved Instance can
    --   -- be used.
    -- , drioMarketplace                    :: !Bool
    --   -- ^ Set to true to show only Reserved Instance Marketplace offerings.
    --   -- When this filter is not used, which is the default behavior, all
    --   -- offerings from AWS and Reserved Instance Marketplace are listed.
    -- , drioProduct-description            :: !ByteString
    --   -- ^ The description of the Reserved Instance.
    -- , drioReserved-instances-offering-id :: !ByteString
    --   -- ^ The Reserved Instances offering ID.
    -- , drioUsage-price                    :: !Double
    --   -- ^ The usage price of the Reserved Instance, per hour (for example,
    --   -- 0.84)

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { drioReservedInstancesOfferingId    :: Members ByteString
      -- ^ One or more Reserved Instances offering IDs.
    , drioInstanceType                   :: Maybe ByteString
      -- ^ The Amazon EC2 instance type on which the Reserved Instance can
      -- be used. See Available Instance Types for more information.
    , drioAvailabilityZone               :: Maybe ByteString
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , drioProductDescription             :: Maybe ByteString
      -- ^ The Reserved Instance description. Instances that include (Amazon
      -- VPC) in the description are for use with Amazon VPC.
    , drioFilter                         :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeReservedInstancesOfferings

instance IsXML DescribeReservedInstancesOfferings where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeReservedInstancesOfferings DescribeReservedInstancesOfferingsResponse where
    request = req GET "DescribeReservedInstancesOfferings"

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { drioRequestId                     :: !ByteString
      -- ^ The ID of the request.
    , drioReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSetItemType
      -- ^ A list of Reserved Instances offerings. Each offering's
      -- information is wrapped in an item element.
    , dripNextToken                     :: !ByteString
      -- ^ The next paginated set of results to return.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeReservedInstancesOfferingsResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your route tables.For more information about route
-- tables, see Route Tables in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRouteTables.html>

-- data RouteTableFilter
--     , druAssociation    :: Members associationType
--       -- ^ The ID of an association ID for the route table.
--     , druRoute-table-id :: !ByteString
--       -- ^ The ID of the route table.
--     , druRoute          :: Members routeType
--       -- ^ The CIDR range specified in a route in the table.
--     , druTag-key        :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , druTag-value      :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , druTag:           :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , druKey            :: !ByteString
--       -- ^ The ID of the VPC for the route table.
--     , druVpc-id         :: !ByteString
--       -- ^ 

data DescribeRouteTables = DescribeRouteTables
    { druRouteTableId :: Members ByteString
      -- ^ One or more route table IDs.
    , druFilter       :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeRouteTables

instance IsXML DescribeRouteTables where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeRouteTables DescribeRouteTablesResponse where
    request = req GET "DescribeRouteTables"

data DescribeRouteTablesResponse = DescribeRouteTablesResponse
    { druRequestId     :: !ByteString
      -- ^ The ID of the request.
    , druRouteTableSet :: !RouteTableType
      -- ^ A list of route tables, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeRouteTablesResponse where
    xmlPickler = withNS ec2NS

-- | A security group is for use with instances either in the EC2-Classic
-- platform or in a specific VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSecurityGroups.html>

-- data SecurityGroupFilter
--     , dshDescription   :: !ByteString
--       -- ^ The description of the security group.
--     , dshGroup-id      :: !ByteString
--       -- ^ The ID of the security group.
--     , dshGroup-name    :: !ByteString
--       -- ^ The name of the security group.
--     , dshIp-permission :: Members ip-permissionType
--       -- ^ The CIDR range that has been granted the permission.
--     , dshOwner-id      :: !ByteString
--       -- ^ The AWS account ID of the owner of the security group.
--     , dshTag-key       :: !ByteString
--       -- ^ The key of a tag assigned to the security group.
--     , dshTag-value     :: !ByteString
--       -- ^ The value of a tag assigned to the security group.
--     , dshVpc-id        :: !ByteString
--       -- ^ Only return the security groups that belong to the specified
--       -- EC2-VPC ID.

data DescribeSecurityGroups = DescribeSecurityGroups
    { dshGroupName     :: Members ByteString
      -- ^ One or more security group names.
    , dshGroupId       :: Members ByteString
      -- ^ One or more security group IDs.
    , dshFilter        :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeSecurityGroups

instance IsXML DescribeSecurityGroups where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeSecurityGroups DescribeSecurityGroupsResponse where
    request = req GET "DescribeSecurityGroups"

data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { dshRequestId         :: !ByteString
      -- ^ The ID of the request.
    , dshSecurityGroupInfo :: !SecurityGroupItemType
      -- ^ A list of security groups, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeSecurityGroupsResponse where
    xmlPickler = withNS ec2NS

-- | Describes an attribute of the specified snapshot. You can specify only one
-- attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshotAttribute.html>

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { dsaSnapshotId :: !ByteString
      -- ^ The ID of the Amazon EBS snapshot.
    , dsaAttribute  :: !ByteString
      -- ^ The snapshot attribute.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeSnapshotAttribute

instance IsXML DescribeSnapshotAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeSnapshotAttribute DescribeSnapshotAttributeResponse where
    request = req GET "DescribeSnapshotAttribute"

data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { dsaRequestId              :: !ByteString
      -- ^ The ID of the request.
    , dsbSnapshotId             :: !ByteString
      -- ^ The ID of the Amazon EBS snapshot.
    , dsbCreateVolumePermission :: !CreateVolumePermissionItemType
      -- ^ A list of permissions for creating volumes from the snapshot.
      -- Each permission is wrapped in an item element.
    , dsbProductCodes           :: !ProductCodesSetItemType
      -- ^ A list of product codes. Each product code is wrapped in an item
      -- element type that contains a product code and a type.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeSnapshotAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of the Amazon EBS snapshots available to you.
-- Snapshots available to you include public snapshots available for any AWS
-- account to launch, private snapshots you own, and private snapshots owned
-- by another AWS account but for which you've been given explicit create
-- volume permissions.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html>

-- data SnapshotFilter
--     , dtOwner-alias  :: !ByteString
--       -- ^ The AWS account alias (for example, amazon) that owns the
--       -- snapshot.
--     , dtOwner-id     :: !ByteString
--       -- ^ The ID of the AWS account that owns the snapshot.
--     , dtProgress     :: !ByteString
--       -- ^ The progress of the snapshot, as a percentage (for example, 80%).
--     , dtSnapshot-id  :: !ByteString
--       -- ^ The snapshot ID.
--     , dtStart-time   :: !UTCTime
--       -- ^ The time stamp when the snapshot was initiated.
--     , dtStatus       :: !ByteString
--       -- ^ The status of the snapshot.
--     , dtTag-key      :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , dtTag-value    :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , dtTag:         :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , dtKey          :: !ByteString
--       -- ^ The ID of the volume the snapshot is for.
--     , dtVolume-id    :: !ByteString
--       -- ^ The size of the volume, in GiB (for example, 20).
--     , dtVolume-size  :: !ByteString
--       -- ^ 


data DescribeSnapshots = DescribeSnapshots
    { dtSnapshotId   :: Members ByteString
      -- ^ One or more snapshot IDs.
    , dtOwner        :: Members ByteString
      -- ^ Returns the snapshots owned by the specified owner. Multiple
      -- owners can be specified.
    , dtRestorableBy :: Members ByteString
      -- ^ One or more AWS accounts IDs that can create volumes from the
      -- snapshot.
    , dtFilter       :: Members ByteString
      -- ^ The name of a filter.
    , dtDescription  :: !ByteString
      -- ^ A description of the snapshot.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeSnapshots

instance IsXML DescribeSnapshots where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeSnapshots DescribeSnapshotsResponse where
    request = req GET "DescribeSnapshots"

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { dyRequestId   :: !ByteString
      -- ^ The ID of the request.
    , dySnapshotSet :: !DescribeSnapshotsSetItemResponseType
      -- ^ A list of snapshots. Each snapshot is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeSnapshotsResponse where
    xmlPickler = withNS ec2NS

-- | Describes the datafeed for Spot Instances. For more information about Spot
-- Instances, see Spot Instances in the Amazon Elastic Compute Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotDatafeedSubscription.html>

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    deriving (Eq, Read, Show, Generic)

instance IsQuery DescribeSpotDatafeedSubscription

instance IsXML DescribeSpotDatafeedSubscription where
    xmlPickler = xpEmpty $ Just ec2NS

instance AWSRequest EC2 DescribeSpotDatafeedSubscription DescribeSpotDatafeedSubscriptionResponse where
    request = req GET "DescribeSpotDatafeedSubscription"

data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { dsdtRequestId                :: !ByteString
      -- ^ The ID of the request.
    , dsdtSpotDatafeedSubscription :: !SpotDatafeedSubscriptionType
      -- ^ The Spot Instance datafeed subscription.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeSpotDatafeedSubscriptionResponse where
    xmlPickler = withNS ec2NS

-- | Describes the Spot Instance requests that belong to your account. Spot
-- Instances are instances that Amazon EC2 starts on your behalf when the
-- maximum price that you specify exceeds the current Spot Price. Amazon EC2
-- periodically sets the Spot Price based on available Spot Instance capacity
-- and current Spot Instance requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotInstanceRequests.html>

-- data SpotInstanceFilter
--     , dsirAvailability-zone-group    :: !ByteString
--       -- ^ The Availability Zone group. If you specify the same Availability
--       -- Zone group for all Spot Instance requests, all Spot Instances are
--       -- launched in the same Availability Zone.
--     , dsirCreate-time                :: !ByteString
--       -- ^ The time stamp when the Spot Instance request was created.
--     , dsirFault-code                 :: !ByteString
--       -- ^ The fault code related to the request.
--     , dsirFault-message              :: !ByteString
--       -- ^ The fault message related to the request.
--     , dsirInstance-id                :: !ByteString
--       -- ^ The ID of the instance that fulfilled the request.
--     , dsirLaunch-group               :: !ByteString
--       -- ^ The Spot Instance launch group. Launch groups are Spot Instances
--       -- that launch together and terminate together.
--     , dsirLaunch                     :: Members launchType
--       -- ^ Whether the Amazon EBS volume is deleted on instance termination.
--     , dsirProduct-description        :: !ByteString
--       -- ^ The product description associated with the instance.
--     , dsirSpot-instance-request-id   :: !ByteString
--       -- ^ The Spot Instance request ID.
--     , dsirSpot-price                 :: !ByteString
--       -- ^ The maximum hourly price for any Spot Instance launched to
--       -- fulfill the request.
--     , dsirState                      :: !ByteString
--       -- ^ The state of the Spot Instance request. Spot bid status
--       -- information can help you track your Amazon EC2 Spot Instance
--       -- requests. For information, see Tracking Spot Requests with Bid
--       -- Status Codes in the Amazon Elastic Compute Cloud User Guide.
--     , dsirStatus-code                :: !ByteString
--       -- ^ The short code describing the most recent evaluation of your Spot
--       -- Instance request.
--     , dsirStatus-message             :: !ByteString
--       -- ^ The message explaining the status of the Spot Instance request.
--     , dsirTag-key                    :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , dsirTag-value                  :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , dsirTag:                       :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , dsirKey                        :: !ByteString
--       -- ^ The type of Spot Instance request.
--     , dsirType                       :: !ByteString
--       -- ^ The Availability Zone in which the bid is launched.
--     , dsirLaunched-availability-zone :: !UTCTime
--       -- ^ The start date of the request.
--     , dsirValid-from                 :: !UTCTime
--       -- ^ The end date of the request.
--     , dsirValid-until                :: !ByteString
--       -- ^ 

data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests
    { dsirSpotInstanceRequestId      :: Members ByteString
      -- ^ One or more Spot Instance request IDs.
    , dsirFilter                     :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeSpotInstanceRequests

instance IsXML DescribeSpotInstanceRequests where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeSpotInstanceRequests DescribeSpotInstanceRequestsResponse where
    request = req GET "DescribeSpotInstanceRequests"

data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse
    { dsirRequestId              :: !ByteString
      -- ^ The ID of the request.
    , dsirSpotInstanceRequestSet :: !SpotInstanceRequestSetItemType
      -- ^ A list of Spot Instance requests. Each request is wrapped in an
      -- item element.
    , dsirNetworkInterfaceSet    :: !InstanceNetworkInterfaceSetItemRequestType
      -- ^ Information about the network interface.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeSpotInstanceRequestsResponse where
    xmlPickler = withNS ec2NS

-- | Describes the Spot Price history.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotPriceHistory.html>

-- data SpotPriceFilter
--     , dsphInstance-type       :: !ByteString
--       -- ^ The type of instance (for example, m1.small).
--     , dsphProduct-description :: !ByteString
--       -- ^ The product description for the Spot Price.
--     , dsphSpot-price          :: !ByteString
--       -- ^ The Spot Price. The value must match exactly (or use wildcards;
--       -- greater than or less than comparison is not supported).
    -- , dsphAvailability-zone   :: !ByteString
    --   -- ^ The Availability Zone for which prices should be returned.
    -- , dsphTimestamp           :: !UTCTime
    --   -- ^ The timestamp of the Spot Price history (for example,
    --   -- 2010-08-16T05:06:11.000Z). You can use wildcards (* and ?).
    --   -- Greater than or less than comparison is not supported.

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { dsphStartTime           :: Maybe UTCTime
      -- ^ The start date and time of the Spot Instance price history data.
    , dsphEndTime             :: Maybe UTCTime
      -- ^ The end date and time of the Spot Instance price history data.
    , dsphInstanceType        :: Members ByteString
      -- ^ The instance type to return.
    , dsphProductDescription  :: Members ByteString
      -- ^ Filters the results by basic product description.
    , dsphFilter              :: Members ByteString
      -- ^ The name of a filter.
    , dsphAvailabilityZone    :: Maybe ByteString
      -- ^ Filters the results by availability zone.
    , dsphMaxResults          :: Maybe Integer
      -- ^ The number of rows to return.
    , dsphNextToken           :: Maybe ByteString
      -- ^ The next set of rows to return.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeSpotPriceHistory

instance IsXML DescribeSpotPriceHistory where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeSpotPriceHistory DescribeSpotPriceHistoryResponse where
    request = req GET "DescribeSpotPriceHistory"

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { dsphRequestId           :: !ByteString
      -- ^ The ID of the request.
    , dsphSpotPriceHistorySet :: !SpotPriceHistorySetItemType
      -- ^ A list of historical Spot Prices. Each price is wrapped in an
      -- item element.
    , dspiNextToken           :: !ByteString
      -- ^ The string marking the next set of results returned. Displays
      -- empty if there are no more results to be returned.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeSpotPriceHistoryResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your subnets.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSubnets.html>

-- data SubnetFilter
--     , duAvailability-zone          :: !ByteString
--       -- ^ The Availability Zone for the subnet.
--     , duAvailable-ip-address-count :: !ByteString
--       -- ^ The number of IP addresses in the subnet that are available.
--     , duCidr                       :: !ByteString
--       -- ^ The CIDR block of the subnet. The CIDR block you specify must
--       -- exactly match the subnet's CIDR block for information to be
--       -- returned for the subnet.
--     , duDefaultForAz               :: !Bool
--       -- ^ Indicates whether this is the default subnet for the Availability
--       -- Zone.
--     , duState                      :: !ByteString
--       -- ^ The state of the subnet.
--     , duSubnet-id                  :: !ByteString
--       -- ^ The ID of the subnet.
--     , duTag-key                    :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , duTag-value                  :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , duTag:                       :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , duKey                        :: !ByteString
--       -- ^ The ID of the VPC for the subnet.
--     , duVpc-id                     :: !ByteString
--       -- ^ 

data DescribeSubnets = DescribeSubnets
    { dtSubnetId                   :: Members ByteString
      -- ^ One or more subnet IDs.
    , duFilter                     :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeSubnets

instance IsXML DescribeSubnets where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeSubnets DescribeSubnetsResponse where
    request = req GET "DescribeSubnets"

data DescribeSubnetsResponse = DescribeSubnetsResponse
    { dzRequestId :: !ByteString
      -- ^ The ID of the request.
    , dzSubnetSet :: !SubnetType
      -- ^ A list of subnets. Each subnet is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeSubnetsResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of the tags for your EC2 resources.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html>

data TagFilter
    --   -- ^ The tag key.
    -- , dvResource-id   :: !ByteString
    --   -- ^ The resource ID.
    -- , dvResource-type :: !ByteString
    --   -- ^ The resource type.
    -- , dvValue         :: !ByteString
    --   -- ^ The tag value.

data DescribeTags = DescribeTags
    { dvFilter        :: Members ByteString
      -- ^ The name of a filter.
    , dvKey           :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeTags

instance IsXML DescribeTags where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeTags DescribeTagsResponse where
    request = req GET "DescribeTags"

data DescribeTagsResponse = DescribeTagsResponse
    { eaRequestId :: !ByteString
      -- ^ The ID of the request.
    , eaTagSet    :: !TagSetItemType
      -- ^ A list of tags. Each tag is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeTagsResponse where
    xmlPickler = withNS ec2NS

-- | Describes the specified attribute of the specified volume. You can specify
-- only one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeAttribute.html>

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { dvaVolumeId  :: !ByteString
      -- ^ The ID of the volume.
    , dvaAttribute :: !ByteString
      -- ^ The instance attribute.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeVolumeAttribute

instance IsXML DescribeVolumeAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeVolumeAttribute DescribeVolumeAttributeResponse where
    request = req GET "DescribeVolumeAttribute"

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { dvaRequestId    :: !ByteString
      -- ^ The ID of the request.
    , dvbVolumeId     :: !ByteString
      -- ^ The ID of the volume.
    , dvbAutoEnableIO :: Maybe Bool
      -- ^ The state of autoEnableIO attribute.
    , dvbProductCodes :: !ProductCodesSetItemType
      -- ^ A list of product codes. Each product code is wrapped in an item
      -- element that contains a product code and a type.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeVolumeAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Describes the specified Amazon EBS volumes.For more information about
-- Amazon EBS, see Amazon Elastic Block Store in the Amazon Elastic Compute
-- Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html>

-- data VolumeFilter
    -- , dwAttachment        :: Members AttachmentType
    --   -- ^ The time stamp when the attachment initiated.
    -- , dwAvailability-zone :: !ByteString
    --   -- ^ The Availability Zone in which the volume was created.
    -- , dwCreate-time       :: !UTCTime
    --   -- ^ The time stamp when the volume was created.
    -- , dwSize              :: !ByteString
    --   -- ^ The size of the volume, in GiB (for example, 20).
    -- , dwSnapshot-id       :: !ByteString
    --   -- ^ The snapshot from which the volume was created.
    -- , dwStatus            :: !ByteString
    --   -- ^ The status of the volume.
    -- , dwTag-key           :: !ByteString
    --   -- ^ The key of a tag assigned to the resource. This filter is
    --   -- independent of the tag-value filter. For example, if you use both
    --   -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
    --   -- get any resources assigned both the tag key Purpose (regardless
    --   -- of what the tag's value is), and the tag value X (regardless of
    --   -- what the tag's key is). If you want to list only resources where
    --   -- Purpose is X, see the tag:key filter.
    -- , dwTag-value         :: !ByteString
    --   -- ^ The value of a tag assigned to the resource. This filter is
    --   -- independent of the tag-key filter.
    -- , dwTag:              :: !ByteString
    --   -- ^ Filters the response based on a specific tag/value combination.
    -- , dwKey               :: !ByteString
    --   -- ^ The volume ID.
    -- , dwVolume-id         :: !ByteString
    --   -- ^ The Amazon EBS volume type. If the volume is an io1 volume, the
    --   -- response includes the IOPS as well.
    -- , dwVolume-type       :: !ByteString
    --   -- ^ 

data DescribeVolumes = DescribeVolumes
    { dwVolumeId :: Members ByteString
      -- ^ One or more volume IDs.
    , dwFilter   :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeVolumes

instance IsXML DescribeVolumes where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeVolumes DescribeVolumesResponse where
    request = req GET "DescribeVolumes"

data DescribeVolumesResponse = DescribeVolumesResponse
    { ebRequestId :: !ByteString
      -- ^ The ID of the request.
    , ebVolumeSet :: !DescribeVolumesSetItemResponseType
      -- ^ A list of volumes. Each volume is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeVolumesResponse where
    xmlPickler = withNS ec2NS

-- | Describes the status of the specified volumes. Volume status provides the
-- result of the checks performed on your volumes to determine events that can
-- impair the performance of your volumes.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeStatus.html>

-- data VolumeFilter
--     , dvsAvailability-zone :: !ByteString
--       -- ^ The Availability Zone of the instance.
--     , dvsVolume-status     :: Members volume-statusType
--       -- ^ The status of the volume.
--     , dvsEvent             :: Members eventType
--       -- ^ A description of the event.
--     , dvsAction            :: Members actionType

data DescribeVolumeStatus = DescribeVolumeStatus
    { dvsVolumeId   :: Members ByteString
      -- ^ One or more volume IDs.
    , dvsFilter     :: Members ByteString
      -- ^ The name of a filter.
    , dvsMaxResults :: Maybe Integer
      -- ^ The maximum number of paginated volume items per response.
    , dvsNextToken  :: Maybe ByteString
      -- ^ A string specifying the next paginated set of results to return
      -- using the pagination token returned by a previous call to this
      -- API.
      -- ^ The action code for the event, for example, enable-volume-io
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeVolumeStatus

instance IsXML DescribeVolumeStatus where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeVolumeStatus DescribeVolumeStatusResponse where
    request = req GET "DescribeVolumeStatus"

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { dvsRequestId       :: !ByteString
      -- ^ The ID of the request.
    , dvsVolumeStatusSet :: !VolumeStatusItemType
      -- ^ A list of volumes. Each volume is wrapped in an item element.
    , dvtNextToken       :: !ByteString
      -- ^ A string specifying the next paginated set of results to return.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeVolumeStatusResponse where
    xmlPickler = withNS ec2NS

-- | Describes the specified attribute of the specified VPC. You can specify
-- only one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcAttribute.html>

data DescribeVpcAttribute = DescribeVpcAttribute
    { dvaVpcId     :: !ByteString
      -- ^ The ID of the VPC.
    , dvbAttribute :: !ByteString
      -- ^ The VPC attribute.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeVpcAttribute

instance IsXML DescribeVpcAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeVpcAttribute DescribeVpcAttributeResponse where
    request = req GET "DescribeVpcAttribute"

data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { dvbRequestId          :: !ByteString
      -- ^ The ID of the request.
    , dvbEnableDnsSupport   :: !Bool
      -- ^ Indicates whether DNS resolution is enabled for the VPC. If this
      -- attribute is true, the Amazon DNS server resolves DNS hostnames
      -- for your instances to their corresponding IP addresses;
      -- otherwise, it does not.
    , dvbEnableDnsHostnames :: !Bool
      -- ^ Indicates whether the instances launched in the VPC get DNS
      -- hostnames. If this attribute is true, instances in the VPC get
      -- DNS hostnames; otherwise, they do not.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeVpcAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your VPCs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcs.html>

-- data VpcFilter
--     , dxCidr            :: !ByteString
--       -- ^ The CIDR block of the VPC. The CIDR block you specify must
--       -- exactly match the VPC's CIDR block for information to be returned
--       -- for the VPC.
--     , dxDhcp-options-id :: !ByteString
--       -- ^ The ID of a set of DHCP options.
--     , dxIsDefault       :: !Bool
--       -- ^ Indicates whether the VPC is the default VPC.
--     , dxState           :: !ByteString
--       -- ^ The state of the VPC.
--     , dxTag-key         :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , dxTag-value       :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , dxTag:            :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , dxKey             :: !ByteString
--       -- ^ The ID of the VPC.
--     , dxVpc-id          :: !ByteString
--       -- ^ 

data DescribeVpcs = DescribeVpcs
    { dwVpcId  :: Members ByteString
      -- ^ One or more VPC IDs.
    , dxFilter :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeVpcs

instance IsXML DescribeVpcs where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeVpcs DescribeVpcsResponse where
    request = req GET "DescribeVpcs"

data DescribeVpcsResponse = DescribeVpcsResponse
    { ecRequestId :: !ByteString
      -- ^ The ID of the request.
    , ecVpcSet    :: !VpcType
      -- ^ A list of VPCs. Each VPC is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeVpcsResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your VPN connections.For more information about
-- VPN connections, see Adding a Hardware Virtual Private Gateway to Your VPC
-- in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpnConnections.html>

-- data VpcConnectionFilter
    -- , dvdCustomer-gateway-configuration :: !ByteString
    --   -- ^ The configuration information for the customer gateway.
    -- , dvdCustomer-gateway-id            :: !ByteString
    --   -- ^ The ID of a customer gateway associated with the VPN connection.
    -- , dvdState                          :: !ByteString
    --   -- ^ The state of the VPN connection.
    -- , dvdOption                         :: Members optionType
    --   -- ^ Indicates whether the connection has static routes only. Used for
    --   -- devices that do not support Border Gateway Protocol (BGP).
    -- , dvdRoute                          :: Members routeType
    --   -- ^ The destination CIDR block. This corresponds to the subnet used
    --   -- in a customer data center.
    -- , dvdBgp-asn                        :: !Integer
    --   -- ^ The BGP Autonomous System Number (ASN) associated with a BGP
    --   -- device.
    -- , dvdTag-key                        :: !ByteString
    --   -- ^ The key of a tag assigned to the resource. This filter is
    --   -- independent of the tag-value filter. For example, if you use both
    --   -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
    --   -- get any resources assigned both the tag key Purpose (regardless
    --   -- of what the tag's value is), and the tag value X (regardless of
    --   -- what the tag's key is). If you want to list only resources where
    --   -- Purpose is X, see the tag:key filter.
    -- , dvdTag-value                      :: !ByteString
    --   -- ^ The value of a tag assigned to the resource. This filter is
    --   -- independent of the tag-key filter.
    -- , dvdTag:                           :: !ByteString
    --   -- ^ Filters the response based on a specific tag/value combination.
    -- , dvdKey                            :: !ByteString
    --   -- ^ The type of VPN connection. Currently the only supported type is
    --   -- ipsec.1.
    -- , dvdType                           :: !ByteString
    --   -- ^ The ID of the VPN connection.
    -- , dvdVpn-connection-id              :: !ByteString
    --   -- ^ The ID of a virtual private gateway associated with the VPN
    --   -- connection.
    -- , dvdVpn-gateway-id                 :: !ByteString
    --   -- ^ 

data DescribeVpnConnections = DescribeVpnConnections
    { dvdVpnConnectionId :: Members ByteString
      -- ^ One or more VPN connection IDs.
    , dvdFilter          :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeVpnConnections

instance IsXML DescribeVpnConnections where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeVpnConnections DescribeVpnConnectionsResponse where
    request = req GET "DescribeVpnConnections"

data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
    { dvdRequestId        :: !ByteString
      -- ^ The ID of the request.
    , dvdVpnConnectionSet :: !VpnConnectionType
      -- ^ A list of VPN connections. Each VPN connection is wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeVpnConnectionsResponse where
    xmlPickler = withNS ec2NS

-- | Describes one or more of your virtual private gateways. For more
-- information about virtual private gateways, see Adding an IPsec Hardware
-- VPN to Your VPC in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpnGateways.html>

-- data VpnGatewayFilter
--     , dvhAttachment        :: Members AttachmentType
--       -- ^ The current state of the attachment between the gateway and the VPC.
--     , dvhAvailability-zone :: !ByteString
--       -- ^ The Availability Zone for the virtual private gateway.
--     , dvhState             :: !ByteString
--       -- ^ The state of the virtual private gateway.
--     , dvhTag-key           :: !ByteString
--       -- ^ The key of a tag assigned to the resource. This filter is
--       -- independent of the tag-value filter. For example, if you use both
--       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--       -- get any resources assigned both the tag key Purpose (regardless
--       -- of what the tag's value is), and the tag value X (regardless of
--       -- what the tag's key is). If you want to list only resources where
--       -- Purpose is X, see the tag:key filter.
--     , dvhTag-value         :: !ByteString
--       -- ^ The value of a tag assigned to the resource. This filter is
--       -- independent of the tag-key filter.
--     , dvhTag:              :: !ByteString
--       -- ^ Filters the response based on a specific tag/value combination.
--     , dvhKey               :: !ByteString
--       -- ^ The type of virtual private gateway. Currently the only supported
--       -- type is ipsec.1.
--     , dvhType              :: !ByteString
--       -- ^ The ID of the virtual private gateway.
--     , dvhVpn-gateway-id    :: !ByteString
--       -- ^ 

data DescribeVpnGateways = DescribeVpnGateways
    { dvhVpnGatewayId :: Members ByteString
      -- ^ One or more virtual private gateway IDs.
    , dvhFilter       :: Members ByteString
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeVpnGateways

instance IsXML DescribeVpnGateways where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DescribeVpnGateways DescribeVpnGatewaysResponse where
    request = req GET "DescribeVpnGateways"

data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { dvhRequestId     :: !ByteString
      -- ^ The ID of the request.
    , dvhVpnGatewaySet :: !VpnGatewayType
      -- ^ A list of virtual private gateways. Each virtual private gateway
      -- is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeVpnGatewaysResponse where
    xmlPickler = withNS ec2NS

-- | Detaches an Internet gateway from a VPC, disabling connectivity between the
-- Internet and the VPC. The VPC must not contain any running instances with
-- Elastic IP addresses.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachInternetGateway.html>

data DetachInternetGateway = DetachInternetGateway
    { diiInternetGatewayId :: !ByteString
      -- ^ The ID of the Internet gateway.
    , diiVpcId             :: !ByteString
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery DetachInternetGateway

instance IsXML DetachInternetGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DetachInternetGateway DetachInternetGatewayResponse where
    request = req GET "DetachInternetGateway"

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    { diiRequestId :: !ByteString
      -- ^ The ID of the request.
    , diiReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DetachInternetGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Detaches a network interface from an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachNetworkInterface.html>

data DetachNetworkInterface = DetachNetworkInterface
    { dniAttachmentId :: !ByteString
      -- ^ The ID of the attachment.
    , dniForce        :: Maybe Bool
      -- ^ Set to true to force a detachment.
    } deriving (Eq, Show, Generic)

instance IsQuery DetachNetworkInterface

instance IsXML DetachNetworkInterface where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DetachNetworkInterface DetachNetworkInterfaceResponse where
    request = req GET "DetachNetworkInterface"

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    { dnkRequestId :: !ByteString
      -- ^ The ID of the request.
    , dnkReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DetachNetworkInterfaceResponse where
    xmlPickler = withNS ec2NS

-- | Detaches an Amazon EBS volume from an instance. Make sure to unmount any
-- file systems on the device within your operating system before detaching
-- the volume. Failure to do so will result in the volume being stuck in
-- "busy" state while detaching. For more information about Amazon EBS, see
-- Amazon Elastic Block Store in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVolume.html>

data DetachVolume = DetachVolume
    { dxVolumeId   :: !ByteString
      -- ^ The ID of the volume.
    , dxInstanceId :: Maybe ByteString
      -- ^ The ID of the instance.
    , dxDevice     :: Maybe ByteString
      -- ^ The device name.
    , dxForce      :: Maybe Bool
      -- ^ Forces detachment if the previous detachment attempt did not
      -- occur cleanly (logging into an instance, unmounting the volume,
      -- and detaching normally). This option can lead to data loss or a
      -- corrupted file system. Use this option only as a last resort to
      -- detach a volume from a failed instance. The instance won't have
      -- an opportunity to flush file system caches or file system
      -- metadata. If you use this option, you must perform file system
      -- check and repair procedures.
    } deriving (Eq, Show, Generic)

instance IsQuery DetachVolume

instance IsXML DetachVolume where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DetachVolume DetachVolumeResponse where
    request = req GET "DetachVolume"

data DetachVolumeResponse = DetachVolumeResponse
    { edRequestId  :: !ByteString
      -- ^ The ID of the request.
    , edVolumeId   :: !ByteString
      -- ^ The ID of the volume.
    , edInstanceId :: !ByteString
      -- ^ The ID of the instance.
    , edDevice     :: !ByteString
      -- ^ The device name exposed to the instance.
    , edStatus     :: !ByteString
      -- ^ The attachment state.
    , edAttachTime :: !UTCTime
      -- ^ The time stamp when the attachment initiated.
    } deriving (Eq, Show, Generic)

instance IsXML DetachVolumeResponse where
    xmlPickler = withNS ec2NS

-- | Detaches a virtual private gateway from a VPC. You do this if you're
-- planning to turn off the VPC and not use it anymore. You can confirm a
-- virtual private gateway has been completely detached from a VPC by
-- describing the virtual private gateway (any attachments to the virtual
-- private gateway are also described).
--
-- You must wait for the attachment's state to switch to detached before you
-- can delete the VPC or attach a different VPC to the virtual private gateway.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVpnGateway.html>

data DetachVpnGateway = DetachVpnGateway
    { dviVpnGatewayId :: !ByteString
      -- ^ The ID of the virtual private gateway.
    , dviVpcId        :: !ByteString
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery DetachVpnGateway

instance IsXML DetachVpnGateway where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DetachVpnGateway DetachVpnGatewayResponse where
    request = req GET "DetachVpnGateway"

data DetachVpnGatewayResponse = DetachVpnGatewayResponse
    { dviRequestId :: !ByteString
      -- ^ The ID of the request.
    , dviReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DetachVpnGatewayResponse where
    xmlPickler = withNS ec2NS

-- | Disables a virtual private gateway (VGW) from propagating routes to the
-- routing tables of a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisableVgwRoutePropagation.html>

data DisableVgwRoutePropagation = DisableVgwRoutePropagation
    { dvrpRouteTableId :: !ByteString
      -- ^ The ID of the routing table.
    , dvrpGatewayId    :: !ByteString
      -- ^ The ID of the virtual private gateway.
    } deriving (Eq, Show, Generic)

instance IsQuery DisableVgwRoutePropagation

instance IsXML DisableVgwRoutePropagation where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DisableVgwRoutePropagation DisableVgwRoutePropagationResponse where
    request = req GET "DisableVgwRoutePropagation"

data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
    { dvrpRequestId :: !ByteString
      -- ^ The ID of the request.
    , dvrpReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DisableVgwRoutePropagationResponse where
    xmlPickler = withNS ec2NS

-- | Disassociates an Elastic IP address from the instance or network interface
-- it's associated with. An Elastic IP address is for use in either the
-- EC2-Classic platform or in a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateAddress.html>

data DisassociateAddress = DisassociateAddress
    { dbPublicIp      :: !ByteString
      -- ^ [EC2-Classic] The Elastic IP address.
    , dbAssociationId :: !ByteString
      -- ^ [EC2-VPC] The association ID.
    } deriving (Eq, Show, Generic)

instance IsQuery DisassociateAddress

instance IsXML DisassociateAddress where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DisassociateAddress DisassociateAddressResponse where
    request = req GET "DisassociateAddress"

data DisassociateAddressResponse = DisassociateAddressResponse
    { dbRequestId :: !ByteString
      -- ^ The ID of the request.
    , dbReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DisassociateAddressResponse where
    xmlPickler = withNS ec2NS

-- | Disassociates a subnet from a route table.After you perform this action,
-- the subnet no longer uses the routes in the route table. Instead, it uses
-- the routes in the VPC's main route table. For more information about route
-- tables, see Route Tables in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateRouteTable.html>

data DisassociateRouteTable = DisassociateRouteTable
    { drtAssociationId :: !ByteString
      -- ^ The association ID representing the current association between
      -- the route table and subnet.
    } deriving (Eq, Show, Generic)

instance IsQuery DisassociateRouteTable

instance IsXML DisassociateRouteTable where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 DisassociateRouteTable DisassociateRouteTableResponse where
    request = req GET "DisassociateRouteTable"

data DisassociateRouteTableResponse = DisassociateRouteTableResponse
    { drvRequestId :: !ByteString
      -- ^ The ID of the request.
    , drvReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML DisassociateRouteTableResponse where
    xmlPickler = withNS ec2NS

-- | Enables a virtual private gateway (VGW) to propagate routes to the routing
-- tables of a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVgwRoutePropagation.html>

data EnableVgwRoutePropagation = EnableVgwRoutePropagation
    { evrpRouteTableId :: !ByteString
      -- ^ The ID of the routing table.
    , evrpGatewayId    :: !ByteString
      -- ^ The ID of the virtual private gateway.
    } deriving (Eq, Show, Generic)

instance IsQuery EnableVgwRoutePropagation

instance IsXML EnableVgwRoutePropagation where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 EnableVgwRoutePropagation EnableVgwRoutePropagationResponse where
    request = req GET "EnableVgwRoutePropagation"

data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse
    { evrpRequestId :: !ByteString
      -- ^ The ID of the request.
    , evrpReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML EnableVgwRoutePropagationResponse where
    xmlPickler = withNS ec2NS

-- | Enables I/O operations for a volume that had I/O operations disabled
-- because the data on the volume was potentially inconsistent.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVolumeIO.html>

data EnableVolumeIO = EnableVolumeIO
    { evioVolumeId :: !ByteString
      -- ^ The ID of the volume.
    } deriving (Eq, Show, Generic)

instance IsQuery EnableVolumeIO

instance IsXML EnableVolumeIO where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 EnableVolumeIO EnableVolumeIOResponse where
    request = req GET "EnableVolumeIO"

data EnableVolumeIOResponse = EnableVolumeIOResponse
    { evioRequestId :: !ByteString
      -- ^ The ID of the request.
    , evioReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML EnableVolumeIOResponse where
    xmlPickler = withNS ec2NS

-- | Gets the console output for the specified instance. Instances do not have a
-- physical monitor through which you can view their console output. They also
-- lack physical controls that allow you to power up, reboot, or shut them
-- down. To allow these actions, we provide them through the Amazon EC2 API
-- and command line interface.Instance console output is buffered and posted
-- shortly after instance boot, reboot, and termination. Amazon EC2 preserves
-- the most recent 64 KB output which will be available for at least one hour
-- after the most recent post.For Linux/UNIX instances, the instance console
-- output displays the exact console output that would normally be displayed
-- on a physical monitor attached to a machine. This output is buffered
-- because the instance produces it and then posts it to a store where the
-- instance's owner can retrieve it.For Windows instances, the instance
-- console output displays the last three system event log errors.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-GetConsoleOutput.html>

data GetConsoleOutput = GetConsoleOutput
    { gcoInstanceId :: !ByteString
      -- ^ The ID of the instance.
    } deriving (Eq, Show, Generic)

instance IsQuery GetConsoleOutput

instance IsXML GetConsoleOutput where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 GetConsoleOutput GetConsoleOutputResponse where
    request = req GET "GetConsoleOutput"

data GetConsoleOutputResponse = GetConsoleOutputResponse
    { gcoRequestId  :: !ByteString
      -- ^ The ID of the request.
    , gcpInstanceId :: !ByteString
      -- ^ The ID of the instance.
    , gcpTimestamp  :: !UTCTime
      -- ^ The time the output was last updated.
    , gcpOutput     :: !ByteString
      -- ^ The console output, Base64 encoded.
    } deriving (Eq, Show, Generic)

instance IsXML GetConsoleOutputResponse where
    xmlPickler = withNS ec2NS

-- | Retrieves the encrypted administrator password for an instance running
-- Windows.The Windows password is only generated the first time an AMI is
-- launched. It is not generated for rebundled AMIs or after the password is
-- changed on an instance.The password is encrypted using the key pair that
-- you specified when you launched the instance. You must provide the
-- corresponding key pair file.Password generation and encryption takes a few
-- moments. Please wait up to 15 minutes after launching an instance before
-- trying to retrieve the generated password.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-GetPasswordData.html>

data GetPasswordData = GetPasswordData
    { gpdInstanceId :: !ByteString
      -- ^ The ID of a Windows instance.
    } deriving (Eq, Show, Generic)

instance IsQuery GetPasswordData

instance IsXML GetPasswordData where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 GetPasswordData GetPasswordDataResponse where
    request = req GET "GetPasswordData"

data GetPasswordDataResponse = GetPasswordDataResponse
    { gpdRequestId    :: !ByteString
      -- ^ The ID of the request.
    , gpeInstanceId   :: !ByteString
      -- ^ The ID of the instance.
    , gpeTimestamp    :: !UTCTime
      -- ^ The time the data was last updated.
    , gpePasswordData :: !ByteString
      -- ^ The password of the instance.
    } deriving (Eq, Show, Generic)

instance IsXML GetPasswordDataResponse where
    xmlPickler = withNS ec2NS

-- | Creates an import instance task using metadata from the specified disk
-- image. After importing the image, you then upload it using the
-- ec2-upload-disk-image command in the EC2 command line tools. For more
-- information, see Using the Command Line Tools to Import Your Virtual
-- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportInstance.html>

data ImportInstance = ImportInstance
    { iiDescription         :: Maybe ByteString
      -- ^ A description for the instance being imported.
    , iiLaunchSpecification :: Members LaunchSpecificationType
      -- ^ The architecture of the instance.
    , iiDiskImage           :: Members DiskImageType
      -- ^ The file format of the disk image.
    , iiPlatform            :: Maybe ByteString
      -- ^ The instance operating system.
    } deriving (Eq, Show, Generic)

instance IsQuery ImportInstance

instance IsXML ImportInstance where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ImportInstance ImportInstanceResponse where
    request = req GET "ImportInstance"

data ImportInstanceResponse = ImportInstanceResponse
    { iiConversionTask :: !ConversionTaskType
      -- ^ Information about the import instance task.
    } deriving (Eq, Show, Generic)

instance IsXML ImportInstanceResponse where
    xmlPickler = withNS ec2NS

-- | Imports the public key from an RSA key pair that you created with a
-- third-party tool. Compare this with CreateKeyPair, in which AWS creates the
-- key pair and gives the keys to you (AWS keeps a copy of the public key).
-- With ImportKeyPair, you create the key pair and give AWS just the public
-- key. The private key is never transferred between you and AWS.You can
-- easily create an RSA key pair on Windows and Linux using the ssh-keygen
-- command line tool (provided with the standard OpenSSH installation).
-- Standard library support for RSA key pair creation is also available in
-- Java, Ruby, Python, and many other programming languages.Supported
-- formats:DSA keys are not supported. Make sure your key generator is set up
-- to create RSA keys.Supported lengths: 1024, 2048, and 4096.Note that you
-- can have up to five thousand key pairs per region.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportKeyPair.html>

data ImportKeyPair = ImportKeyPair
    { ikpKeyName           :: !ByteString
      -- ^ A unique name for the key pair.
    , ikpPublicKeyMaterial :: !ByteString
      -- ^ The public key. You must base64 encode the public key material
      -- before sending it to AWS.
    } deriving (Eq, Show, Generic)

instance IsQuery ImportKeyPair

instance IsXML ImportKeyPair where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ImportKeyPair ImportKeyPairResponse where
    request = req GET "ImportKeyPair"

data ImportKeyPairResponse = ImportKeyPairResponse
    { ikpRequestId      :: !ByteString
      -- ^ The ID of the request.
    , ikqKeyName        :: !ByteString
      -- ^ The key pair name you provided.
    , ikqKeyFingerprint :: !ByteString
      -- ^ The MD5 public key fingerprint as specified in section 4 of
      -- RFC4716.
    } deriving (Eq, Show, Generic)

instance IsXML ImportKeyPairResponse where
    xmlPickler = withNS ec2NS

-- | Creates an import volume task using metadata from the specified disk image.
-- After importing the image, you then upload it using the
-- ec2-upload-disk-image command in the EC2 command line tools. For more
-- information, see Using the Command Line Tools to Import Your Virtual
-- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportVolume.html>

data ImportVolume = ImportVolume
    { ivAvailabilityZone :: !ByteString
      -- ^ The Availability Zone for the resulting Amazon EBS volume.
    , ivImage            :: Members ImageType
      -- ^ The file format of the disk image.
    , ivDescription      :: Maybe ByteString
      -- ^ An optional description for the volume being imported.
    , ivVolume           :: Members VolumeType
      -- ^ The size, in GB (2^30 bytes), of an Amazon EBS volume to hold the
      -- converted image.
    } deriving (Eq, Show, Generic)

instance IsQuery ImportVolume

instance IsXML ImportVolume where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ImportVolume ImportVolumeResponse where
    request = req GET "ImportVolume"

data ImportVolumeResponse = ImportVolumeResponse
    { ivConversionTask :: !ConversionTaskType
      -- ^ Information about the import volume task.
    } deriving (Eq, Show, Generic)

instance IsXML ImportVolumeResponse where
    xmlPickler = withNS ec2NS

-- | Modifies the specified attribute of the specified AMI. You can specify only
-- one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyImageAttribute.html>

data ModifyImageAttribute = ModifyImageAttribute
    { miaImageId          :: !ByteString
      -- ^ The ID of the AMI.
    , miaLaunchPermission :: Members LaunchPermissionType
      -- ^ Adds the specified AWS account ID to the AMI's list of launch
      -- permissions.
    , miaProductCode      :: Members ByteString
      -- ^ Adds the specified product code to the specified instance
      -- store-backed AMI. After you add a product code to an AMI, it
      -- can't be removed.
    , miaDescription      :: Members DescriptionType
      -- ^ Changes the AMI's description to the specified value.
    } deriving (Eq, Show, Generic)

instance IsQuery ModifyImageAttribute

instance IsXML ModifyImageAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ModifyImageAttribute ModifyImageAttributeResponse where
    request = req GET "ModifyImageAttribute"

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    { miaRequestId :: !ByteString
      -- ^ The ID of the request.
    , miaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ModifyImageAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Modifies the specified attribute of the specified instance. You can specify
-- only one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyInstanceAttribute.html>

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { miaInstanceId                        :: !ByteString
      -- ^ The ID of the instance.
    , miaInstanceType                      :: Members InstanceTypeType
      -- ^ Changes the instance type to the specified value. See Available
      -- Instance Types for more information. An
      -- InvalidInstanceAttributeValue error will be returned if the
      -- instance type is not valid.
    , miaKernel                            :: Members KernelType
      -- ^ Changes the instance's kernel to the specified value.
    , miaRamdisk                           :: Members RamdiskType
      -- ^ Changes the instance's RAM disk to the specified value.
    , miaUserData                          :: Members UserDataType
      -- ^ Changes the instance's user data to the specified value.
    , miaDisableApiTermination             :: Members DisableApiTerminationType
      -- ^ If the value is true, you can't terminate the instance using the
      -- Amazon EC2 console, CLI, or API; otherwise you can.
    , miaInstanceInitiatedShutdownBehavior :: Members InstanceInitiatedShutdownBehaviorType
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , miaBlockDeviceMapping                :: Members BlockDeviceMappingType
      -- ^ Modifies the DeleteOnTermination attribute for volumes that are
      -- currently attached. The volume must be owned by the caller. If no
      -- value is specified for DeleteOnTermination, the default is true
      -- and the volume is deleted when the instance is terminated.
    , miaSourceDestCheck                   :: Members SourceDestCheckType
      -- ^ Indicates whether source/destination checking is enabled. A value
      -- of true means checking is enabled, and false means checking is
      -- disabled. This value must be false for a NAT instance to perform
      -- NAT.
    , miaGroupId                           :: Members ByteString
      -- ^ [EC2-VPC] Changes the instance's security group. You must specify
      -- at least one security group, even if it's just the default
      -- security group for the VPC. You must specify the security group
      -- ID, not the security group name.
    , miaEbsOptimized                      :: Maybe Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal EBS I/O
      -- performance. This optimization isn't available with all instance
      -- types. Additional usage charges apply when using an EBS Optimized
      -- instance.
    } deriving (Eq, Show, Generic)

instance IsQuery ModifyInstanceAttribute

instance IsXML ModifyInstanceAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ModifyInstanceAttribute ModifyInstanceAttributeResponse where
    request = req GET "ModifyInstanceAttribute"

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    { mibRequestId :: !ByteString
      -- ^ The ID of the request.
    , mibReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ModifyInstanceAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Modifies the specified network interface attribute. You can specify only
-- one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyNetworkInterfaceAttribute.html>

data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { mniaNetworkInterfaceId :: !ByteString
      -- ^ The ID of the network interface.
    , mniaDescription        :: Members DescriptionType
      -- ^ A description for the network interface.
    , mniaSecurityGroupId    :: Members ByteString
      -- ^ Changes the security groups that a network interface is in. The
      -- new set of groups you specify replaces the current set. You must
      -- specify at least one group, even if it's just the default
      -- security group in the VPC. You must specify the group ID and not
      -- the group name.
    , mniaSourceDestCheck    :: Members SourceDestCheckType
      -- ^ Indicates whether source/destination checking is enabled. A value
      -- of true means checking is enabled, and false means checking is
      -- disabled. This value must be false for a NAT instance to perform
      -- NAT.
    , mniaAttachment         :: Members AttachmentType
      -- ^ The ID of the interface attachment.
    } deriving (Eq, Show, Generic)

instance IsQuery ModifyNetworkInterfaceAttribute

instance IsXML ModifyNetworkInterfaceAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ModifyNetworkInterfaceAttribute ModifyNetworkInterfaceAttributeResponse where
    request = req GET "ModifyNetworkInterfaceAttribute"

data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
    { mniaRequestId :: !ByteString
      -- ^ The ID of the request.
    , mniaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ModifyNetworkInterfaceAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Adds or remove permission settings for the specified snapshot.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySnapshotAttribute.html>

data ModifySnapshotAttribute = ModifySnapshotAttribute
    { msaSnapshotId             :: !ByteString
      -- ^ The ID of the snapshot.
    , msaCreateVolumePermission :: Members CreateVolumePermissionType
      -- ^ Adds the specified AWS account ID to the volume's list of create
      -- volume permissions.
    } deriving (Eq, Show, Generic)

instance IsQuery ModifySnapshotAttribute

instance IsXML ModifySnapshotAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ModifySnapshotAttribute ModifySnapshotAttributeResponse where
    request = req GET "ModifySnapshotAttribute"

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    { msaRequestId :: !ByteString
      -- ^ The ID of the request.
    , msaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ModifySnapshotAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Modifies a volume attribute.By default, all I/O operations for the volume
-- are suspended when the data on the volume is determined to be potentially
-- inconsistent, to prevent undetectable, latent data corruption. The I/O
-- access to the volume can be resumed by first calling EnableVolumeIO action
-- to enable I/O access and then checking the data consistency on your
-- volume.You can change the default behavior to resume I/O operations without
-- calling EnableVolumeIO action by setting the AutoEnableIO attribute of the
-- volume to true. We recommend that you change this attribute only for
-- volumes that are stateless, or disposable, or for boot volumes.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVolumeAttribute.html>

data ModifyVolumeAttribute = ModifyVolumeAttribute
    { mvaVolumeId     :: !ByteString
      -- ^ The ID of the volume.
    , mvaAutoEnableIO :: Members AutoEnableIOType
      -- ^ Specifies whether the volume should be auto-enabled for I/O
      -- operations.
    } deriving (Eq, Show, Generic)

instance IsQuery ModifyVolumeAttribute

instance IsXML ModifyVolumeAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ModifyVolumeAttribute ModifyVolumeAttributeResponse where
    request = req GET "ModifyVolumeAttribute"

data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
    { mvaRequestId :: !ByteString
      -- ^ The ID of the request.
    , mvaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ModifyVolumeAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Modifies the specified attribute of the specified VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVpcAttribute.html>

data ModifyVpcAttribute = ModifyVpcAttribute
    { mvaVpcId              :: !ByteString
      -- ^ The ID of the VPC.
    , mvaEnableDnsSupport   :: Maybe Bool
      -- ^ Specifies whether DNS resolution is supported for the VPC. If
      -- this attribute is true, the Amazon DNS server resolves DNS
      -- hostnames for your instances to their corresponding IP addresses;
      -- otherwise, it does not.
    , mvaEnableDnsHostnames :: Maybe Bool
      -- ^ Specifies whether the instances launched in the VPC get DNS
      -- hostnames. If this attribute is true, instances in the VPC get
      -- DNS hostnames; otherwise, they do not.
    } deriving (Eq, Show, Generic)

instance IsQuery ModifyVpcAttribute

instance IsXML ModifyVpcAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ModifyVpcAttribute ModifyVpcAttributeResponse where
    request = req GET "ModifyVpcAttribute"

data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
    { mvbRequestId :: !ByteString
      -- ^ The ID of the request.
    , mvbReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ModifyVpcAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Enables monitoring for a running instance. For more information about
-- monitoring instances, see Monitoring Your Instances and Volumes in the
-- Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MonitorInstances.html>

data MonitorInstances = MonitorInstances
    { miInstanceId :: Members ByteString
      -- ^ One or more instance IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery MonitorInstances

instance IsXML MonitorInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 MonitorInstances MonitorInstancesResponse where
    request = req GET "MonitorInstances"

data MonitorInstancesResponse = MonitorInstancesResponse
    { miRequestId    :: !ByteString
      -- ^ The ID of the request.
    , miInstancesSet :: !MonitorInstancesResponseSetItemType
      -- ^ A list of instances. Each instance is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML MonitorInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Purchases a Reserved Instance for use with your account. With Amazon EC2
-- Reserved Instances, you obtain a capacity reservation for a certain
-- instance configuration over a specified period of time. You pay a lower
-- usage rate than with On-Demand instances for the time that you actually use
-- the capacity reservation. Starting with the 2011-11-01 API version, AWS
-- expanded its offering of Reserved Instances to address a range of projected
-- instance usage. There are three types of Reserved Instances based on
-- customer utilization levels: Heavy Utilization, Medium Utilization, and
-- Light Utilization.The Medium Utilization offering type is equivalent to the
-- Reserved Instance offering available before API version 2011-11-01. If you
-- are using tools that predate the 2011-11-01 API version,
-- DescribeReservedInstancesOfferings will only list information about the
-- Medium Utilization Reserved Instance offering type.For information about
-- Reserved Instance pricing tiers, go to Understanding Reserved Instance
-- pricing tiers in the Amazon Elastic Compute Cloud User Guide. For more
-- information about Reserved Instances, go to Reserved Instances also in the
-- Amazon Elastic Compute Cloud User Guide.You determine the type of the
-- Reserved Instances offerings by including the optional offeringType
-- parameter when calling DescribeReservedInstancesOfferings. After you've
-- identified the Reserved Instance with the offering type you want, specify
-- its ReservedInstancesOfferingId when you call
-- PurchaseReservedInstancesOffering. Starting with the 2012-08-15 API
-- version, you can also purchase Reserved Instances from the Reserved
-- Instance Marketplace. The Reserved Instance Marketplace matches sellers who
-- want to resell Reserved Instance capacity that they no longer need with
-- buyers who want to purchase additional capacity. Reserved Instances bought
-- and sold through the Reserved Instance Marketplace work like any other
-- Reserved Instances.By default, with the 2012-08-15 API version,
-- DescribeReservedInstancesOfferings returns information about Amazon EC2
-- Reserved Instances available directly from AWS, plus instance offerings
-- available on the Reserved Instance Marketplace. If you are using tools that
-- predate the 2012-08-15 API version, the DescribeReservedInstancesOfferings
-- action will only list information about Amazon EC2 Reserved Instances
-- available directly from AWS.For more information about the Reserved
-- Instance Marketplace, go to Reserved Instance Marketplace in the Amazon
-- Elastic Compute Cloud User Guide. You determine the Reserved Instance
-- Marketplace offerings by specifying true for the optional
-- includeMarketplace parameter when calling
-- DescribeReservedInstancesOfferings. After you've identified the Reserved
-- Instance with the offering type you want, specify its
-- reservedInstancesOfferingId when you call
-- PurchaseReservedInstancesOffering.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-PurchaseReservedInstancesOffering.html>

data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
    { prioReservedInstancesOfferingId :: !ByteString
      -- ^ The ID of the Reserved Instance offering you want to purchase.
    , prioInstanceCount               :: !Integer
      -- ^ The number of Reserved Instances to purchase.
    , prioLimitPrice                  :: Maybe ReservedInstanceLimitPriceType
      -- ^ Specified for Reserved Instance Marketplace offerings to limit
      -- the total order and ensure that the Reserved Instances are not
      -- purchased at unexpected prices.
    } deriving (Eq, Show, Generic)

instance IsQuery PurchaseReservedInstancesOffering

instance IsXML PurchaseReservedInstancesOffering where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 PurchaseReservedInstancesOffering PurchaseReservedInstancesOfferingResponse where
    request = req GET "PurchaseReservedInstancesOffering"

data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { prioRequestId           :: !ByteString
      -- ^ The ID of the request.
    , prioReservedInstancesId :: !ByteString
      -- ^ The IDs of the purchased Reserved Instances.
    } deriving (Eq, Show, Generic)

instance IsXML PurchaseReservedInstancesOfferingResponse where
    xmlPickler = withNS ec2NS

-- | Requests a reboot of one or more instances. This operation is asynchronous;
-- it only queues a request to reboot the specified instance(s). The operation
-- will succeed if the instances are valid and belong to you. Requests to
-- reboot terminated instances are ignored.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RebootInstances.html>

data RebootInstances = RebootInstances
    { riInstanceId :: Members ByteString
      -- ^ One or more instance IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery RebootInstances

instance IsXML RebootInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 RebootInstances RebootInstancesResponse where
    request = req GET "RebootInstances"

data RebootInstancesResponse = RebootInstancesResponse
    { riRequestId :: !ByteString
      -- ^ The ID of the request.
    , riReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML RebootInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Registers an AMI. When you're creating an AMI, this is the final step you
-- must complete before you can launch an instance from the AMI. For more
-- information about creating AMIs, see Creating Your Own AMIs in the Amazon
-- Elastic Compute Cloud User Guide.You can also use the RegisterImage action
-- to create an EBS-backed AMI from a snapshot of a root device volume. For
-- more information, see Launching an Instance from a Snapshot in the Amazon
-- Elastic Compute Cloud User Guide.If needed, you can deregister an AMI at
-- any time. Any modifications you make to an AMI backed by instance store
-- invalidates its registration. If you make changes to an image, deregister
-- the previous image and register the new image.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RegisterImage.html>

data RegisterImage = RegisterImage
    { riImageLocation      :: !ByteString
      -- ^ The full path to your AMI manifest in Amazon S3 storage.
    , riName               :: !ByteString
      -- ^ A name for your AMI.
    , riDescription        :: Maybe ByteString
      -- ^ A description for your AMI.
    , riArchitecture       :: Maybe ByteString
      -- ^ The architecture of the image.
    , riKernelId           :: Maybe ByteString
      -- ^ The ID of the kernel.
    , riRamdiskId          :: Maybe ByteString
      -- ^ The ID of the RAM disk.
    , riVirtualizationType :: Maybe ByteString
      -- ^ The type of virtualization.
    , riRootDeviceName     :: !ByteString
      -- ^ The name of the root device (for example, /dev/sda1, or xvda).
    , riBlockDeviceMapping :: Members BlockDeviceMappingType
      -- ^ The device name exposed to the instance (for example, /dev/sdh or
      -- xvdh).
    } deriving (Eq, Show, Generic)

instance IsQuery RegisterImage

instance IsXML RegisterImage where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 RegisterImage RegisterImageResponse where
    request = req GET "RegisterImage"

data RegisterImageResponse = RegisterImageResponse
    { rjRequestId :: !ByteString
      -- ^ The ID of the request.
    , rjImageId   :: !ByteString
      -- ^ The ID of the newly registered AMI.
    } deriving (Eq, Show, Generic)

instance IsXML RegisterImageResponse where
    xmlPickler = withNS ec2NS

-- | Releases the specified Elastic IP address. An Elastic IP address is for use
-- either in the EC2-Classic platform or in a VPC.[EC2-Classic, default VPC] Releasing an Elastic IP address
-- automatically disassociates it from any instance that it's associated with.
-- To disassociate an Elastic IP address without releasing it, use the
-- ec2-disassociate-address command. [Nondefault VPC] You must use the
-- ec2-disassociate-address command to disassociate the Elastic IP address
-- before you try to release it. Otherwise, Amazon EC2 returns an error
-- (InvalidIPAddress.InUse).
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReleaseAddress.html>

data ReleaseAddress = ReleaseAddress
    { raPublicIp     :: !ByteString
      -- ^ [EC2-Classic] The Elastic IP address.
    , raAllocationId :: !ByteString
      -- ^ [EC2-VPC] The allocation ID.
    } deriving (Eq, Show, Generic)

instance IsQuery ReleaseAddress

instance IsXML ReleaseAddress where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ReleaseAddress ReleaseAddressResponse where
    request = req GET "ReleaseAddress"

data ReleaseAddressResponse = ReleaseAddressResponse
    { raRequestId :: !ByteString
      -- ^ The ID of the request.
    , raReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ReleaseAddressResponse where
    xmlPickler = withNS ec2NS

-- | Changes which network ACL a subnet is associated with. By default when you
-- create a subnet, it's automatically associated with the default network
-- ACL. For more information about network ACLs, see Network ACLs in the
-- Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkAclAssociation.html>

data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { rnaaAssociationId :: !ByteString
      -- ^ The ID representing the current association between the original
      -- network ACL and the subnet.
    , rnaaNetworkAclId  :: !ByteString
      -- ^ The ID of the new ACL to associate with the subnet.
    } deriving (Eq, Show, Generic)

instance IsQuery ReplaceNetworkAclAssociation

instance IsXML ReplaceNetworkAclAssociation where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ReplaceNetworkAclAssociation ReplaceNetworkAclAssociationResponse where
    request = req GET "ReplaceNetworkAclAssociation"

data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { rnaaRequestId        :: !ByteString
      -- ^ The ID of the request.
    , rnaaNewAssociationId :: !ByteString
      -- ^ The ID of the new association.
    } deriving (Eq, Show, Generic)

instance IsXML ReplaceNetworkAclAssociationResponse where
    xmlPickler = withNS ec2NS

-- | Replaces an entry (rule) in a network ACL. For more information about
-- network ACLs, see Network ACLs in the Amazon Virtual Private Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkAclEntry.html>

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { rnaeNetworkAclId :: !ByteString
      -- ^ The ID of the ACL.
    , rnaeRuleNumber   :: !Integer
      -- ^ The rule number of the entry to replace.
    , rnaeProtocol     :: !Integer
      -- ^ The IP protocol the rule applies to. You can use -1 to mean all
      -- protocols.
    , rnaeRuleAction   :: !ByteString
      -- ^ Indicates whether to allow or deny traffic that matches the rule.
    , rnaeEgress       :: Maybe Bool
      -- ^ Indicates whether this rule applies to egress traffic from the
      -- subnet (true) or ingress traffic to the subnet (false).
    , rnaeCidrBlock    :: !ByteString
      -- ^ The CIDR range to allow or deny, in CIDR notation (for example,
      -- 172.16.0.0/24).
    , rnaeIcmp         :: Members IcmpType
      -- ^ For the ICMP protocol, the ICMP code. You can use -1 to specify
      -- all ICMP codes for the given ICMP type.
    , rnaePortRange    :: Members PortRangeType
      -- ^ The first port in the range.
    } deriving (Eq, Show, Generic)

instance IsQuery ReplaceNetworkAclEntry

instance IsXML ReplaceNetworkAclEntry where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ReplaceNetworkAclEntry ReplaceNetworkAclEntryResponse where
    request = req GET "ReplaceNetworkAclEntry"

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    { rnaeRequestId :: !ByteString
      -- ^ The ID of the request.
    , rnaeReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ReplaceNetworkAclEntryResponse where
    xmlPickler = withNS ec2NS

-- | Replaces an existing route within a route table in a VPC. For more
-- information about route tables, see Route Tables in the Amazon Virtual
-- Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRoute.html>

data ReplaceRoute = ReplaceRoute
    { rrRouteTableId         :: !ByteString
      -- ^ The ID of the route table.
    , rrDestinationCidrBlock :: !ByteString
      -- ^ The CIDR address block used for the destination match. The value
      -- you provide must match the CIDR of an existing route in the
      -- table.
    , rrGatewayId            :: !ByteString
      -- ^ The ID of a gateway attached to your VPC.
    , rrInstanceId           :: !ByteString
      -- ^ The ID of a NAT instance in your VPC.
    , rrNetworkInterfaceId   :: !ByteString
      -- ^ Allows routing to network interface attachments.
    } deriving (Eq, Show, Generic)

instance IsQuery ReplaceRoute

instance IsXML ReplaceRoute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ReplaceRoute ReplaceRouteResponse where
    request = req GET "ReplaceRoute"

data ReplaceRouteResponse = ReplaceRouteResponse
    { rrRequestId :: !ByteString
      -- ^ The ID of the request.
    , rrReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ReplaceRouteResponse where
    xmlPickler = withNS ec2NS

-- | Changes the route table associated with a given subnet in a VPC. After you
-- execute this action, the subnet uses the routes in the new route table it's
-- associated with. For more information about route tables, see Route Tables
-- in the Amazon Virtual Private Cloud User Guide.You can also use this action
-- to change which table is the main route table in the VPC. You just specify
-- the main route table's association ID and the route table that you want to
-- be the new main route table.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRouteTableAssociation.html>

data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { rrtaAssociationId :: !ByteString
      -- ^ The association ID.
    , rrtaRouteTableId  :: !ByteString
      -- ^ The ID of the new route table to associate with the subnet.
    } deriving (Eq, Show, Generic)

instance IsQuery ReplaceRouteTableAssociation

instance IsXML ReplaceRouteTableAssociation where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ReplaceRouteTableAssociation ReplaceRouteTableAssociationResponse where
    request = req GET "ReplaceRouteTableAssociation"

data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { rrtaRequestId        :: !ByteString
      -- ^ The ID of the request.
    , rrtaNewAssociationId :: !ByteString
      -- ^ The ID of the new association.
    } deriving (Eq, Show, Generic)

instance IsXML ReplaceRouteTableAssociationResponse where
    xmlPickler = withNS ec2NS

-- | Use this action to submit feedback about an instance's status. This action
-- works only for instances that are in the running state. If your experience
-- with the instance differs from the instance status returned by the
-- DescribeInstanceStatus action, use ReportInstanceStatus to report your
-- experience with the instance. Amazon EC2 collects this information to
-- improve the accuracy of status checks. To report an instance's status,
-- specify an instance ID with the InstanceId.n parameter and a reason code
-- with the ReasonCode.n parameter that applies to that instance. The
-- following table contains descriptions of all available reason codes.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReportInstanceStatus.html>

data ReportInstanceStatus = ReportInstanceStatus
    { risInstanceId  :: Members ByteString
      -- ^ One or more instance IDs.
    , risStatus      :: !ByteString
      -- ^ The status of all instances listed in the InstanceId.n parameter.
    , risStartTime   :: Maybe UTCTime
      -- ^ The time at which the reported instance health state began.
    , risEndTime     :: Maybe UTCTime
      -- ^ The time at which the reported instance health state ended.
    , risReasonCode  :: Members ByteString
      -- ^ A reason code that describes a specific instance's health state.
      -- Each code you supply corresponds to an instance ID that you
      -- supply with the InstanceId.n parameter. See the Description
      -- section for descriptions of each reason code.
    , risDescription :: Maybe ByteString
      -- ^ Descriptive text about the instance health state.
    } deriving (Eq, Show, Generic)

instance IsQuery ReportInstanceStatus

instance IsXML ReportInstanceStatus where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ReportInstanceStatus ReportInstanceStatusResponse where
    request = req GET "ReportInstanceStatus"

data ReportInstanceStatusResponse = ReportInstanceStatusResponse
    { risRequestId :: !ByteString
      -- ^ The ID of the request.
    , risReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ReportInstanceStatusResponse where
    xmlPickler = withNS ec2NS

-- | Creates a Spot Instance request. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current Spot Instance requests. For
-- more information about Spot Instances, see Spot Instances in the Amazon
-- Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotInstances.html>

data RequestSpotInstances = RequestSpotInstances
    { rsiSpotPrice             :: !ByteString
      -- ^ The maximum hourly price for any Spot Instance launched to
      -- fulfill the request.
    , rsiInstanceCount         :: Maybe Integer
      -- ^ The maximum number of Spot Instances to launch.
    , rsiType                  :: Maybe ByteString
      -- ^ The Spot Instance request type.
    , rsiValidFrom             :: Maybe UTCTime
      -- ^ The start date of the request. If this is a one-time request, the
      -- request becomes active at this date and time and remains active
      -- until all instances launch, the request expires, or the request
      -- is canceled. If the request is persistent, the request becomes
      -- active at this date and time and remains active until it expires
      -- or is canceled.
    , rsiValidUntil            :: Maybe UTCTime
      -- ^ The end date of the request. If this is a one-time request, the
      -- request remains active until all instances launch, the request is
      -- canceled, or this date is reached. If the request is persistent,
      -- it remains active until it is canceled or this date and time is
      -- reached.
    , rsiLaunchGroup           :: Maybe ByteString
      -- ^ The instance launch group. Launch groups are Spot Instances that
      -- launch together and terminate together.
    , rsiAvailabilityZoneGroup :: Maybe ByteString
      -- ^ The user-specified name for a logical grouping of bids.
    , rsiLaunchSpecification   :: Members LaunchSpecificationType
      -- ^ The ID of the AMI.
    } deriving (Eq, Show, Generic)

instance IsQuery RequestSpotInstances

instance IsXML RequestSpotInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 RequestSpotInstances RequestSpotInstancesResponse where
    request = req GET "RequestSpotInstances"

data RequestSpotInstancesResponse = RequestSpotInstancesResponse
    { rsiRequestId              :: !ByteString
      -- ^ The ID of the request.
    , rsiSpotInstanceRequestSet :: !SpotInstanceRequestSetItemType
      -- ^ Information about the Spot Instance request, wrapped in an item
      -- element.
    } deriving (Eq, Show, Generic)

instance IsXML RequestSpotInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Resets an attribute of an AMI to its default value.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetImageAttribute.html>

data ResetImageAttribute = ResetImageAttribute
    { riaImageId   :: !ByteString
      -- ^ The ID of the AMI.
    , riaAttribute :: !ByteString
      -- ^ The attribute to reset (currently you can only reset the launch
      -- permission attribute).
    } deriving (Eq, Show, Generic)

instance IsQuery ResetImageAttribute

instance IsXML ResetImageAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ResetImageAttribute ResetImageAttributeResponse where
    request = req GET "ResetImageAttribute"

data ResetImageAttributeResponse = ResetImageAttributeResponse
    { riaRequestId :: !ByteString
      -- ^ The ID of the request.
    , riaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ResetImageAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Resets an attribute of an instance to its default value. To reset the
-- kernel or RAM disk, the instance must be in a stopped state. To reset the
-- SourceDestCheck, the instance can be either running or stopped. The
-- SourceDestCheck attribute controls whether source/destination checking is
-- enabled. The default value is true, which means checking is enabled. This
-- value must be false for a NAT instance to perform NAT. For more
-- information, see NAT Instances in the Amazon Virtual Private Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetInstanceAttribute.html>

data ResetInstanceAttribute = ResetInstanceAttribute
    { riaInstanceId :: !ByteString
      -- ^ The ID of the instance.
    , ribAttribute  :: !ByteString
      -- ^ The attribute to reset.
    } deriving (Eq, Show, Generic)

instance IsQuery ResetInstanceAttribute

instance IsXML ResetInstanceAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ResetInstanceAttribute ResetInstanceAttributeResponse where
    request = req GET "ResetInstanceAttribute"

data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    { ribRequestId :: !ByteString
      -- ^ The ID of the request.
    , ribReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ResetInstanceAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Resets a network interface attribute. You can specify only one attribute at
-- a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetNetworkInterfaceAttribute.html>

data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
    { rniaNetworkInterfaceId :: !ByteString
      -- ^ The ID of the network interface.
    , rniaAttribute          :: !ByteString
      -- ^ The name of the attribute to reset.
    } deriving (Eq, Show, Generic)

instance IsQuery ResetNetworkInterfaceAttribute

instance IsXML ResetNetworkInterfaceAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ResetNetworkInterfaceAttribute ResetNetworkInterfaceAttributeResponse where
    request = req GET "ResetNetworkInterfaceAttribute"

data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse
    { rniaRequestId :: !ByteString
      -- ^ The ID of the request.
    , rniaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ResetNetworkInterfaceAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Resets permission settings for the specified snapshot.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetSnapshotAttribute.html>

data ResetSnapshotAttribute = ResetSnapshotAttribute
    { rsaSnapshotId :: !ByteString
      -- ^ The ID of the snapshot.
    , rsaAttribute  :: !ByteString
      -- ^ The attribute to reset (currently only the attribute for
      -- permission to create volumes can be reset)
    } deriving (Eq, Show, Generic)

instance IsQuery ResetSnapshotAttribute

instance IsXML ResetSnapshotAttribute where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 ResetSnapshotAttribute ResetSnapshotAttributeResponse where
    request = req GET "ResetSnapshotAttribute"

data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
    { rsaRequestId :: !ByteString
      -- ^ The ID of the request.
    , rsaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML ResetSnapshotAttributeResponse where
    xmlPickler = withNS ec2NS

-- | Removes one or more egress rules from a security group for EC2-VPC. The
-- values that you specify in the revoke request (for example, ports) must
-- match the existing rule's values for the rule to be revoked.Each rule
-- consists of the protocol and the CIDR range or destination security group.
-- For the TCP and UDP protocols, you must also specify the destination port
-- or range of ports. For the ICMP protocol, you must also specify the ICMP
-- type and code. Rule changes are propagated to instances within the security
-- group as quickly as possible. However, a small delay might occur. For more
-- information, see Security Groups in the Amazon Virtual Private Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupEgress.html>

data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { rsgeGroupId       :: !ByteString
      -- ^ The ID of the security group to modify.
    , rsgeIpPermissions :: Members IpPermissionType
      -- ^ The IP protocol name or number (see Protocol Numbers).
    } deriving (Eq, Show, Generic)

instance IsQuery RevokeSecurityGroupEgress

instance IsXML RevokeSecurityGroupEgress where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 RevokeSecurityGroupEgress RevokeSecurityGroupEgressResponse where
    request = req GET "RevokeSecurityGroupEgress"

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    { rsgeRequestId :: !ByteString
      -- ^ The ID of the request.
    , rsgeReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML RevokeSecurityGroupEgressResponse where
    xmlPickler = withNS ec2NS

-- | Removes one or more ingress rules from a security group. The values that
-- you specify in the revoke request (for example, ports) must match the
-- existing rule's values for the rule to be removed.A security group is for
-- use with instances either in the EC2-Classic platform or in a specific VPC.
--Each rule consists of the protocol and the
-- CIDR range or source security group. For the TCP and UDP protocols, you
-- must also specify the destination port or range of ports. For the ICMP
-- protocol, you must also specify the ICMP type and code. Rule changes are
-- propagated to instances within the security group as quickly as possible.
-- However, depending on the number of instances, a small delay might occur.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupIngress.html>

data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { rsgiUserId        :: Maybe ByteString
      -- ^ Deprecated
    , rsgiGroupId       :: !ByteString
      -- ^ The ID of the security group to modify. The security group must
      -- belong to your account.
    , rsgiGroupName     :: !ByteString
      -- ^ The name of the security group to modify.
    , rsgiIpPermissions :: Members IpPermissionType
      -- ^ The IP protocol name or number (see Protocol Numbers). For
      -- EC2-Classic, security groups can have rules only for TCP, UDP,
      -- and ICMP. For EC2-VPC, security groups can have rules assigned to
      -- any protocol number.
    } deriving (Eq, Show, Generic)

instance IsQuery RevokeSecurityGroupIngress

instance IsXML RevokeSecurityGroupIngress where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 RevokeSecurityGroupIngress RevokeSecurityGroupIngressResponse where
    request = req GET "RevokeSecurityGroupIngress"

data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
    { rsgiRequestId :: !ByteString
      -- ^ The ID of the request.
    , rsgiReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML RevokeSecurityGroupIngressResponse where
    xmlPickler = withNS ec2NS

-- | Launches the specified number of instances of an AMI for which you have
-- permissions.When you launch an instance, it enters the pending state. After
-- the instance is ready for you, it enters the running state. To check the
-- state of your instance, call DescribeInstances.If you don't specify a
-- security group when launching an instance, Amazon EC2 uses the default
-- security group.Linux instances have access to the public
-- key of the key pair at boot. You can use this key to provide secure access
-- to the instance. Amazon EC2 public images use this feature to provide
-- secure access without passwords.You can provide optional user data
-- when launching an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RunInstances.html>

data RunInstances = RunInstances
    { riImageId                           :: !ByteString
      -- ^ The ID of the AMI, which you can get by calling DescribeImages.
    , riMinCount                          :: !Integer
      -- ^ The minimum number of instances to launch. If you specify a
      -- minimum that is more instances than Amazon EC2 can launch in the
      -- target Availability Zone, Amazon EC2 launches no instances.
    , riMaxCount                          :: !Integer
      -- ^ The maximum number of instances to launch. If you specify more
      -- instances than Amazon EC2 can launch in the target Availability
      -- Zone, Amazon EC2 launches the largest possible number of
      -- instances above MinCount.
    , riKeyName                           :: Maybe ByteString
      -- ^ The name of the key pair. You can create a key pair using
      -- CreateKeyPair or ImportKeyPair.
    , riSecurityGroupId                   :: Members ByteString
      -- ^ One or more security group IDs. You can create a security group
      -- using CreateSecurityGroup.
    , riSecurityGroup                     :: Members ByteString
      -- ^ [EC2-Classic, default VPC] One or more security group names. For
      -- a nondefault VPC, you must use SecurityGroupId.n.
    , riUserData                          :: Maybe ByteString
      -- ^ The Base64-encoded MIME user data for the instances.
    , riInstanceType                      :: Maybe ByteString
      -- ^ The instance type. See Available Instance Types for more
      -- information.
    , riPlacement                         :: Members PlacementType
      -- ^ The Availability Zone for the instance.
    , rjKernelId                          :: Maybe ByteString
      -- ^ The ID of the kernel.
    , rjRamdiskId                         :: Maybe ByteString
      -- ^ The ID of the RAM disk.
    , rjBlockDeviceMapping                :: Members BlockDeviceMappingType
      -- ^ The device name exposed to the instance (for example, /dev/sdh or
      -- xvdh).
    , rjMonitoring                        :: Members MonitoringType
      -- ^ Enables monitoring for the instance.
    , rjSubnetId                          :: Maybe ByteString
      -- ^ [EC2-VPC] The ID of the subnet to launch the instance into.
    , rjDisableApiTermination             :: Maybe Bool
      -- ^ If you set this parameter to true, you can't terminate the
      -- instance using the Amazon EC2 console, CLI, or API; otherwise,
      -- you can. If you set this parameter to true and then later want to
      -- be able to terminate the instance, you must first change the
      -- value of the disableApiTermination attribute to false using
      -- ModifyInstanceAttribute. Alternatively, if you set
      -- InstanceInitiatedShutdownBehavior to terminate, you can terminate
      -- the instance by running the shutdown command from the instance.
    , rjInstanceInitiatedShutdownBehavior :: Maybe ByteString
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , rjPrivateIpAddress                  :: Maybe ByteString
      -- ^ [EC2-VPC] The primary IP address. You must specify a value from
      -- the IP address range of the subnet.
    , rjClientToken                       :: Maybe ByteString
      -- ^ Unique, case-sensitive identifier you provide to ensure
      -- idempotency of the request.
    , rjNetworkInterface                  :: Members NetworkInterfaceType
      -- ^ An existing interface to attach to a single instance. Requires
      -- n=1 instances.
    , rjIamInstanceProfile                :: Members IamInstanceProfileType
      -- ^ The Amazon Resource Name (ARN) of the IAM instance profile to
      -- associate with the instances.
    , rjEbsOptimized                      :: Maybe Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal Amazon EBS I/O
      -- performance. This optimization isn't available with all instance
      -- types. Additional usage charges apply when using an EBS-optimized
      -- instance.
    } deriving (Eq, Show, Generic)

instance IsQuery RunInstances

instance IsXML RunInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 RunInstances RunInstancesResponse where
    request = req GET "RunInstances"

data RunInstancesResponse = RunInstancesResponse
    { rkRequestId     :: !ByteString
      -- ^ The ID of the request.
    , rkReservationId :: !ByteString
      -- ^ The ID of the reservation.
    , rkOwnerId       :: !ByteString
      -- ^ The ID of the AWS account that owns the reservation.
    , rkGroupSet      :: !GroupItemType
      -- ^ A list of security groups the instance belongs to. Each group is
      -- wrapped in an item element.
    , rkInstancesSet  :: !RunningInstancesItemType
      -- ^ A list of instances. Each instance is wrapped in an item element.
    , rkRequesterId   :: !ByteString
      -- ^ The ID of the requester that launched the instances on your
      -- behalf (for example, AWS Management Console, Auto Scaling).
    } deriving (Eq, Show, Generic)

instance IsXML RunInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Starts an Amazon EBS-backed AMI that you've previously stopped. Instances
-- that use Amazon EBS volumes as their root devices can be quickly stopped
-- and started. When an instance is stopped, the compute resources are
-- released and you are not billed for hourly instance usage. However, your
-- root partition Amazon EBS volume remains, continues to persist your data,
-- and you are charged for Amazon EBS volume usage. You can restart your
-- instance at any time. Each time you transition an instance from stopped to
-- started, we charge a full instance hour, even if transitions happen
-- multiple times within a single hour. Before stopping an instance, make sure
-- it is in a state from which it can be restarted. Stopping an instance does
-- not preserve data stored in RAM. Performing this operation on an instance
-- that uses an instance store as its root device returns an error.For more
-- information, see Stopping Instances in the Amazon Elastic Compute Cloud
-- User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StartInstances.html>

data StartInstances = StartInstances
    { siInstanceId :: Members ByteString
      -- ^ One or more instance IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery StartInstances

instance IsXML StartInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 StartInstances StartInstancesResponse where
    request = req GET "StartInstances"

data StartInstancesResponse = StartInstancesResponse
    { siRequestId    :: !ByteString
      -- ^ The ID of the request.
    , siInstancesSet :: !InstanceStateChangeType
      -- ^ A list of instance state changes. Each change is wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML StartInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Stops an Amazon EBS-backed instance. Each time you transition an instance
-- from stopped to started, we charge a full instance hour, even if
-- transitions happen multiple times within a single hour.You can't start or
-- stop Spot Instances.Instances that use Amazon EBS volumes as their root
-- devices can be quickly stopped and started. When an instance is stopped,
-- the compute resources are released and you are not billed for hourly
-- instance usage. However, your root partition Amazon EBS volume remains,
-- continues to persist your data, and you are charged for Amazon EBS volume
-- usage. You can restart your instance at any time. Before stopping an
-- instance, make sure it is in a state from which it can be restarted.
-- Stopping an instance does not preserve data stored in RAM. Performing this
-- operation on an instance that uses an instance store as its root device
-- returns an error.You can stop, start, and terminate EBS-backed instances.
-- You can only terminate S3-backed instances. What happens to an instance
-- differs if you stop it or terminate it. For example, when you stop an
-- instance, the root device and any other devices attached to the instance
-- persist. When you terminate an instance, the root device and any other
-- devices attached during the instance launch are automatically deleted. For
-- more information about the differences between stopping and terminating
-- instances, see Stopping Instances in the Amazon Elastic Compute Cloud User
-- Guide
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StopInstances.html>

data StopInstances = StopInstances
    { sjInstanceId :: Members ByteString
      -- ^ One or more instance IDs.
    , sjForce      :: Maybe Bool
      -- ^ Forces the instances to stop. The instances will not have an
      -- opportunity to flush file system caches or file system metadata.
      -- If you use this option, you must perform file system check and
      -- repair procedures. This option is not recommended for Windows
      -- instances.
    } deriving (Eq, Show, Generic)

instance IsQuery StopInstances

instance IsXML StopInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 StopInstances StopInstancesResponse where
    request = req GET "StopInstances"

data StopInstancesResponse = StopInstancesResponse
    { sjRequestId    :: !ByteString
      -- ^ The ID of the request.
    , sjInstancesSet :: !InstanceStateChangeType
      -- ^ A list of instance state changes. Each change is wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML StopInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Shuts down one or more instances. This operation is idempotent; if you
-- terminate an instance more than once, each call will succeed. Terminated
-- instances will remain visible after termination (approximately one
-- hour).You can stop, start, and terminate EBS-backed instances. You can only
-- terminate S3-backed instances. What happens to an instance differs if you
-- stop it or terminate it. For example, when you stop an instance, the root
-- device and any other devices attached to the instance persist. When you
-- terminate an instance, the root device and any other devices attached
-- during the instance launch are automatically deleted. For more information
-- about the differences between stopping and terminating instances, see
-- Stopping Instances in the Amazon Elastic Compute Cloud User Guide
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-TerminateInstances.html>

data TerminateInstances = TerminateInstances
    { tiInstanceId :: Members ByteString
      -- ^ One or more instance IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery TerminateInstances

instance IsXML TerminateInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 TerminateInstances TerminateInstancesResponse where
    request = req GET "TerminateInstances"

data TerminateInstancesResponse = TerminateInstancesResponse
    { tiRequestId    :: !ByteString
      -- ^ The ID of the request.
    , tiInstancesSet :: !InstanceStateChangeType
      -- ^ A list of instance state changes. Each change is wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML TerminateInstancesResponse where
    xmlPickler = withNS ec2NS

-- | Unassigns one or more secondary private IP addresses from a network
-- interface.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnassignPrivateIpAddresses.html>

data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { upiaNetworkInterfaceId :: !ByteString
      -- ^ The network interface from which the secondary private IP address
      -- will be unassigned.
    , upiaPrivateIpAddress   :: Members AssignPrivateIpAddressesSetItemRequestType
      -- ^ The secondary private IP addresses that you want to unassign from
      -- the network interface. You can specify this option multiple times
      -- to unassign more than one IP address.
    } deriving (Eq, Show, Generic)

instance IsQuery UnassignPrivateIpAddresses

instance IsXML UnassignPrivateIpAddresses where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 UnassignPrivateIpAddresses UnassignPrivateIpAddressesResponse where
    request = req GET "UnassignPrivateIpAddresses"

data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
    { upiaRequestId :: !ByteString
      -- ^ The ID of the request.
    , upiaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML UnassignPrivateIpAddressesResponse where
    xmlPickler = withNS ec2NS

-- | Disables monitoring for a running instance. For more information about
-- monitoring instances, see Monitoring Your Instances and Volumes in the
-- Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnmonitorInstances.html>

data UnmonitorInstances = UnmonitorInstances
    { uiInstanceId :: Members ByteString
      -- ^ One or more instance IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery UnmonitorInstances

instance IsXML UnmonitorInstances where
    xmlPickler = withNS ec2NS

instance AWSRequest EC2 UnmonitorInstances UnmonitorInstancesResponse where
    request = req GET "UnmonitorInstances"

data UnmonitorInstancesResponse = UnmonitorInstancesResponse
    { uiRequestId    :: !ByteString
      -- ^ The ID of the request.
    , uiInstancesSet :: !MonitorInstancesResponseSetItemType
      -- ^ A list of monitoring information for one or more instances. Each
      -- set of information is wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML UnmonitorInstancesResponse where
    xmlPickler = withNS ec2NS
