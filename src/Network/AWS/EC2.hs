{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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
    -- * Actions
    -- ** AllocateAddress
      AllocateAddress                        (..)
    , AllocateAddressResponse                (..)

    -- ** AssignPrivateIpAddresses
    , AssignPrivateIpAddresses               (..)
    , AssignPrivateIpAddressesResponse       (..)

    -- ** AssociateAddress
    , AssociateAddress                       (..)
    , AssociateAddressResponse               (..)

    -- ** AssociateDhcpOptions
    , AssociateDhcpOptions                   (..)
    , AssociateDhcpOptionsResponse           (..)

    -- ** AssociateRouteTable
    , AssociateRouteTable                    (..)
    , AssociateRouteTableResponse            (..)

    -- ** AttachInternetGateway
    , AttachInternetGateway                  (..)
    , AttachInternetGatewayResponse          (..)

    -- ** AttachNetworkInterface
    , AttachNetworkInterface                 (..)
    , AttachNetworkInterfaceResponse         (..)

    -- ** AttachVolume
    , AttachVolume                           (..)
    , AttachVolumeResponse                   (..)

    -- ** AttachVpnGateway
    , AttachVpnGateway                       (..)
    , AttachVpnGatewayResponse               (..)

    -- ** AuthorizeSecurityGroupEgress
    , AuthorizeSecurityGroupEgress           (..)
    , AuthorizeSecurityGroupEgressResponse   (..)

    -- ** AuthorizeSecurityGroupIngress
    , AuthorizeSecurityGroupIngress          (..)
    , AuthorizeSecurityGroupIngressResponse  (..)

    -- ** BundleInstance
    , BundleInstance                         (..)
    , BundleInstanceResponse                 (..)

    -- ** CancelBundleTask
    , CancelBundleTask                       (..)
    , CancelBundleTaskResponse               (..)

    -- ** CancelConversionTask
    , CancelConversionTask                   (..)
    , CancelConversionTaskResponse           (..)

    -- ** CancelExportTask
    , CancelExportTask                       (..)
    , CancelExportTaskResponse               (..)

    -- ** CancelReservedInstancesListing
    , CancelReservedInstancesListing         (..)
    , CancelReservedInstancesListingResponse (..)

    -- ** CancelSpotInstanceRequests
    , CancelSpotInstanceRequests             (..)
    , CancelSpotInstanceRequestsResponse     (..)

    -- -- ** ConfirmProductInstance
    -- , ConfirmProductInstance              (..)

    -- -- ** CopyImage
    -- , CopyImage                           (..)

    -- -- ** CopySnapshot
    -- , CopySnapshot                        (..)

    -- -- ** CreateCustomerGateway
    -- , CreateCustomerGateway               (..)

    -- -- ** CreateDhcpOptions
    -- , CreateDhcpOptions                   (..)

    -- -- ** CreateImage
    -- , CreateImage                         (..)

    -- -- ** CreateInstanceExportTask
    -- , CreateInstanceExportTask            (..)

    -- -- ** CreateInternetGateway
    -- , CreateInternetGateway               (..)

    -- ** CreateKeyPair
    , CreateKeyPair                          (..)
    , CreateKeyPairResponse                  (..)

    -- -- ** CreateNetworkAcl
    -- , CreateNetworkAcl                    (..)

    -- -- ** CreateNetworkAclEntry
    -- , CreateNetworkAclEntry               (..)

    -- -- ** CreateNetworkInterface
    -- , CreateNetworkInterface              (..)

    -- -- ** CreatePlacementGroup
    -- , CreatePlacementGroup                (..)

    -- -- ** CreateReservedInstancesListing
    -- , CreateReservedInstancesListing      (..)

    -- -- ** CreateRoute
    -- , CreateRoute                         (..)

    -- -- ** CreateRouteTable
    -- , CreateRouteTable                    (..)

    -- ** CreateSecurityGroup
    , CreateSecurityGroup                    (..)
    , CreateSecurityGroupResponse            (..)

    -- -- ** CreateSnapshot
    -- , CreateSnapshot                      (..)

    -- -- ** CreateSpotDatafeedSubscription
    -- , CreateSpotDatafeedSubscription      (..)

    -- -- ** CreateSubnet
    -- , CreateSubnet                        (..)

    -- ** CreateTags
    , CreateTags                             (..)
    , CreateTagsResponse                     (..)

    -- -- ** CreateVolume
    -- , CreateVolume                        (..)

    -- -- ** CreateVpc
    -- , CreateVpc                           (..)

    -- -- ** CreateVpnConnection
    -- , CreateVpnConnection                 (..)

    -- -- ** CreateVpnConnectionRoute
    -- , CreateVpnConnectionRoute            (..)

    -- -- ** CreateVpnGateway
    -- , CreateVpnGateway                    (..)

    -- -- ** DeleteCustomerGateway
    -- , DeleteCustomerGateway               (..)

    -- -- ** DeleteDhcpOptions
    -- , DeleteDhcpOptions                   (..)

    -- -- ** DeleteInternetGateway
    -- , DeleteInternetGateway               (..)

    -- -- ** DeleteKeyPair
    -- , DeleteKeyPair                       (..)

    -- -- ** DeleteNetworkAcl
    -- , DeleteNetworkAcl                    (..)

    -- -- ** DeleteNetworkAclEntry
    -- , DeleteNetworkAclEntry               (..)

    -- -- ** DeleteNetworkInterface
    -- , DeleteNetworkInterface              (..)

    -- -- ** DeletePlacementGroup
    -- , DeletePlacementGroup                (..)

    -- -- ** DeleteRoute
    -- , DeleteRoute                         (..)

    -- -- ** DeleteRouteTable
    -- , DeleteRouteTable                    (..)

    -- ** DeleteSecurityGroup
    , DeleteSecurityGroup                    (..)
    , DeleteSecurityGroupResponse            (..)

    -- -- ** DeleteSnapshot
    -- , DeleteSnapshot                      (..)

    -- -- ** DeleteSpotDatafeedSubscription
    -- , DeleteSpotDatafeedSubscription      (..)

    -- -- ** DeleteSubnet
    -- , DeleteSubnet                        (..)

    -- -- ** DeleteTags
    -- , DeleteTags                          (..)

    -- -- ** DeleteVolume
    -- , DeleteVolume                        (..)

    -- -- ** DeleteVpc
    -- , DeleteVpc                           (..)

    -- -- ** DeleteVpnConnection
    -- , DeleteVpnConnection                 (..)

    -- -- ** DeleteVpnConnectionRoute
    -- , DeleteVpnConnectionRoute            (..)

    -- -- ** DeleteVpnGateway
    -- , DeleteVpnGateway                    (..)

    -- -- ** DeregisterImage
    -- , DeregisterImage                     (..)

    -- -- ** DescribeAccountAttributes
    -- , DescribeAccountAttributes           (..)

    -- -- ** DescribeAddresses
    -- , DescribeAddresses                   (..)

    -- ** DescribeAvailabilityZones
    , DescribeAvailabilityZones              (..)
    , DescribeAvailabilityZonesResponse      (..)

    -- -- ** DescribeBundleTasks
    -- , DescribeBundleTasks                 (..)

    -- -- ** DescribeConversionTasks
    -- , DescribeConversionTasks             (..)

    -- -- ** DescribeCustomerGateways
    -- , DescribeCustomerGateways            (..)

    -- -- ** DescribeDhcpOptions
    -- , DescribeDhcpOptions                 (..)

    -- -- ** DescribeExportTasks
    -- , DescribeExportTasks                 (..)

    -- -- ** DescribeImageAttribute
    -- , DescribeImageAttribute              (..)

    -- ** DescribeImages
    , DescribeImages                         (..)
    , DescribeImagesResponse                 (..)

    -- -- ** DescribeInstanceAttribute
    -- , DescribeInstanceAttribute           (..)

    -- ** DescribeInstances
    , DescribeInstances                      (..)
    , DescribeInstancesResponse              (..)

    -- -- ** DescribeInstanceStatus
    -- , DescribeInstanceStatus              (..)

    -- -- ** DescribeInternetGateways
    -- , DescribeInternetGateways            (..)

    -- ** DescribeKeyPairs
    , DescribeKeyPairs                       (..)
    , DescribeKeyPairsResponse               (..)

    -- -- ** DescribeNetworkAcls
    -- , DescribeNetworkAcls                 (..)

    -- -- ** DescribeNetworkInterfaceAttribute
    -- , DescribeNetworkInterfaceAttribute   (..)

    -- -- ** DescribeNetworkInterfaces
    -- , DescribeNetworkInterfaces           (..)

    -- -- ** DescribePlacementGroups
    -- , DescribePlacementGroups             (..)

    -- ** DescribeRegions
    , DescribeRegions                        (..)
    , DescribeRegionsResponse                (..)

    -- -- ** DescribeReservedInstances
    -- , DescribeReservedInstances           (..)

    -- -- ** DescribeReservedInstancesListings
    -- , DescribeReservedInstancesListings   (..)

    -- -- ** DescribeReservedInstancesOfferings
    -- , DescribeReservedInstancesOfferings  (..)

    -- -- ** DescribeRouteTables
    -- , DescribeRouteTables                 (..)

    -- ** DescribeSecurityGroups
    , DescribeSecurityGroups                 (..)
    , DescribeSecurityGroupsResponse         (..)

    -- -- ** DescribeSnapshotAttribute
    -- , DescribeSnapshotAttribute           (..)

    -- -- ** DescribeSnapshots
    -- , DescribeSnapshots                   (..)

    -- -- ** DescribeSpotDatafeedSubscription
    -- , DescribeSpotDatafeedSubscription    (..)

    -- -- ** DescribeSpotInstanceRequests
    -- , DescribeSpotInstanceRequests        (..)

    -- -- ** DescribeSpotPriceHistory
    -- , DescribeSpotPriceHistory            (..)

    -- -- ** DescribeSubnets
    -- , DescribeSubnets                     (..)

    -- ** DescribeTags
    , DescribeTags                           (..)
    , DescribeTagsResponse                   (..)

    -- -- ** DescribeVolumeAttribute
    -- , DescribeVolumeAttribute             (..)

    -- -- ** DescribeVolumes
    -- , DescribeVolumes                     (..)

    -- -- ** DescribeVolumeStatus
    -- , DescribeVolumeStatus                (..)

    -- -- ** DescribeVpcAttribute
    -- , DescribeVpcAttribute                (..)

    -- -- ** DescribeVpcs
    -- , DescribeVpcs                        (..)

    -- -- ** DescribeVpnConnections
    -- , DescribeVpnConnections              (..)

    -- -- ** DescribeVpnGateways
    -- , DescribeVpnGateways                 (..)

    -- -- ** DetachInternetGateway
    -- , DetachInternetGateway               (..)

    -- -- ** DetachNetworkInterface
    -- , DetachNetworkInterface              (..)

    -- -- ** DetachVolume
    -- , DetachVolume                        (..)

    -- -- ** DetachVpnGateway
    -- , DetachVpnGateway                    (..)

    -- -- ** DisableVgwRoutePropagation
    -- , DisableVgwRoutePropagation          (..)

    -- -- ** DisassociateAddress
    -- , DisassociateAddress                 (..)

    -- -- ** DisassociateRouteTable
    -- , DisassociateRouteTable              (..)

    -- -- ** EnableVgwRoutePropagation
    -- , EnableVgwRoutePropagation           (..)

    -- -- ** EnableVolumeIO
    -- , EnableVolumeIO                      (..)

    -- -- ** GetConsoleOutput
    -- , GetConsoleOutput                    (..)

    -- -- ** GetPasswordData
    -- , GetPasswordData                     (..)

    -- -- ** ImportInstance
    -- , ImportInstance                      (..)

    -- -- ** ImportKeyPair
    -- , ImportKeyPair                       (..)

    -- -- ** ImportVolume
    -- , ImportVolume                        (..)

    -- -- ** ModifyImageAttribute
    -- , ModifyImageAttribute                (..)

    -- -- ** ModifyInstanceAttribute
    -- , ModifyInstanceAttribute             (..)

    -- -- ** ModifyNetworkInterfaceAttribute
    -- , ModifyNetworkInterfaceAttribute     (..)

    -- -- ** ModifySnapshotAttribute
    -- , ModifySnapshotAttribute             (..)

    -- -- ** ModifyVolumeAttribute
    -- , ModifyVolumeAttribute               (..)

    -- -- ** ModifyVpcAttribute
    -- , ModifyVpcAttribute                  (..)

    -- -- ** MonitorInstances
    -- , MonitorInstances                    (..)

    -- -- ** PurchaseReservedInstancesOffering
    -- , PurchaseReservedInstancesOffering   (..)

    -- -- ** RebootInstances
    -- , RebootInstances                     (..)

    -- -- ** RegisterImage
    -- , RegisterImage                       (..)

    -- -- ** ReleaseAddress
    -- , ReleaseAddress                      (..)

    -- -- ** ReplaceNetworkAclAssociation
    -- , ReplaceNetworkAclAssociation        (..)

    -- -- ** ReplaceNetworkAclEntry
    -- , ReplaceNetworkAclEntry              (..)

    -- -- ** ReplaceRoute
    -- , ReplaceRoute                        (..)

    -- -- ** ReplaceRouteTableAssociation
    -- , ReplaceRouteTableAssociation        (..)

    -- -- ** ReportInstanceStatus
    -- , ReportInstanceStatus                (..)

    -- -- ** RequestSpotInstances
    -- , RequestSpotInstances                (..)

    -- -- ** ResetImageAttribute
    -- , ResetImageAttribute                 (..)

    -- -- ** ResetInstanceAttribute
    -- , ResetInstanceAttribute              (..)

    -- -- ** ResetNetworkInterfaceAttribute
    -- , ResetNetworkInterfaceAttribute      (..)

    -- -- ** ResetSnapshotAttribute
    -- , ResetSnapshotAttribute              (..)

    -- ** RevokeSecurityGroupEgress
    , RevokeSecurityGroupEgress           (..)
    , RevokeSecurityGroupEgressResponse   (..)

    -- ** RevokeSecurityGroupIngress
    , RevokeSecurityGroupIngress          (..)
    , RevokeSecurityGroupIngressResponse  (..)

    -- ** RunInstances
    , RunInstances                        (..)
    , RunInstancesResponse                (..)

    -- -- ** StartInstances
    -- , StartInstances                      (..)
    -- , StartInstancesResponse              (..)

    -- -- ** StopInstances
    -- , StopInstances                       (..)
    -- , StopInstancesResponse               (..)

    -- ** TerminateInstances
    , TerminateInstances                  (..)
    , TerminateInstancesResponse          (..)

    -- -- ** UnassignPrivateIpAddresses
    -- , UnassignPrivateIpAddresses          (..)

    -- -- ** UnmonitorInstances
    -- , UnmonitorInstances                  (..)

    -- * Data Types
    , module Network.AWS.EC2.Types

    -- * Common
    , module Network.AWS
    ) where

import Data.Text                 (Text)
import Data.Time
import Network.AWS
import Network.AWS.EC2.Types
import Network.AWS.Internal
import Network.HTTP.Types.Method

-- | Acquires an Elastic IP address.An Elastic IP address is for use either in
-- the EC2-Classic platform or in a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AllocateAddress.html>
data AllocateAddress = AllocateAddress
    { aaDomain :: Maybe AddressDomain
      -- ^ Set to vpc to allocate the address for use with instances in a
      -- VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery AllocateAddress

instance Rq AllocateAddress where
    type Er AllocateAddress = EC2ErrorResponse
    type Rs AllocateAddress = AllocateAddressResponse
    request = query4 ec2 GET "AllocateAddress"

data AllocateAddressResponse = AllocateAddressResponse
    { aarRequestId    :: !Text
      -- ^ The ID of the request.
    , aarPublicIp     :: !Text
      -- ^ The Elastic IP address.
    , aarDomain       :: !AddressDomain
      -- ^ Indicates whether this Elastic IP address is for use with
      -- instances in EC2-Classic (standaard) or instances in a VPC (vpc).
    , aarAllocationId :: Maybe Text
      -- ^ [EC2-VPC] The ID that AWS assigns to represent the allocation of
      -- the Elastic IP address for use with a VPC.
    } deriving (Eq, Show, Generic)

instance IsXML AllocateAddressResponse where
    xmlPickler = ec2XML

-- | Assigns one or more secondary private IP addresses to the specified network
-- interface.
--
-- You can specify one or more specific secondary IP addresses, or
-- you can specify the number of secondary IP addresses to be automatically
-- assigned within the subnet's CIDR block range.
--
-- The number of secondary IP
-- addresses that you can assign to an instance varies by instance type.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssignPrivateIpAddresses.html>
data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { apiaNetworkInterfaceId             :: !Text
      -- ^ The ID of the network interface.
    , apiaPrivateIpAddress               :: [Text]
      -- ^ The IP addresses to be assigned as a secondary private IP address
      -- to the network interface.
    , apiaSecondaryPrivateIpAddressCount :: Maybe Integer
      -- ^ The number of secondary IP addresses to assign to the network
      -- interface.
    , apiaAllowReassignment              :: Maybe Bool
      -- ^ Specifies whether to allow an IP address that is already assigned
      -- to another network interface or instance to be reassigned to the
      -- specified network interface.
    } deriving (Eq, Show, Generic)

instance IsQuery AssignPrivateIpAddresses

instance Rq AssignPrivateIpAddresses where
    type Er AssignPrivateIpAddresses = EC2ErrorResponse
    type Rs AssignPrivateIpAddresses = AssignPrivateIpAddressesResponse
    request = query4 ec2 GET "AssignPrivateIpAddresses"

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    { apiaRequestId :: !Text
      -- ^ The ID of the request.
    , apiaReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AssignPrivateIpAddressesResponse where
    xmlPickler = withRootNS ec2NS "AssignPrivateIpAddresses"

-- | Associates an Elastic IP address with an instance or a network interface.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateAddress.html>
data AssociateAddress = AssociateAddress
    { abPublicIp           :: !Text
      -- ^ The Elastic IP address.
    , abInstanceId         :: !Text
      -- ^ The ID of the instance. The operation fails if you specify an
      -- instance ID unless exactly one network interface is attached.
    , abAllocationId       :: Maybe Text
      -- ^ [EC2-VPC] The allocation ID.
    , abNetworkInterfaceId :: Maybe Text
      -- ^ [EC2-VPC] The ID of the network interface.
    , abPrivateIpAddress   :: Maybe Text
      -- ^ [EC2-VPC] The primary or secondary private IP address to
      -- associate with the Elastic IP address. If no private IP address
      -- is specified, the Elastic IP address is associated with the
      -- primary private IP address.
    , abAllowReassociation :: Maybe Bool
      -- ^ [EC2-VPC] Allows an Elastic IP address that is already associated
      -- with an instance or network interface to be re-associated with
      -- the specified instance or network interface. Otherwise, the
      -- operation fails.
    } deriving (Eq, Show, Generic)

instance IsQuery AssociateAddress

instance Rq AssociateAddress where
    type Er AssociateAddress = EC2ErrorResponse
    type Rs AssociateAddress = AssociateAddressResponse
    request = query4 ec2 GET "AssociateAddress"

data AssociateAddressResponse = AssociateAddressResponse
    { abrRequestId     :: !Text
      -- ^ The ID of the request.
    , abrReturn        :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an error.
    , abrAssociationId :: Maybe Text
      -- ^ [EC2-VPC] The ID that represents the association of the Elastic
      -- IP address with an instance.
    } deriving (Eq, Show, Generic)

instance IsXML AssociateAddressResponse where
    xmlPickler = ec2XML

-- | Associates a set of DHCP options (that you've previously created) with the
-- specified VPC, or associates no DHCP options with the VPC.
--
-- After you associate the options with the VPC, any existing instances and all new
-- instances that you launch in that VPC use the options. You don't need to
-- restart or relaunch the instances. They automatically pick up the changes
-- within a few hours, depending on how frequently the instance renews its
-- DHCP lease. You can explicitly renew the lease using the operating system
-- on the instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateDhcpOptions.html>
data AssociateDhcpOptions = AssociateDhcpOptions
    { adoDhcpOptionsId :: !Text
      -- ^ The ID of the DHCP options set, or default to associate no DHCP
      -- options with the VPC.
    , adoVpcId         :: !Text
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery AssociateDhcpOptions

instance Rq AssociateDhcpOptions where
    type Er AssociateDhcpOptions = EC2ErrorResponse
    type Rs AssociateDhcpOptions = AssociateDhcpOptionsResponse
    request = query4 ec2 GET "AssociateDhcpOptions"

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    { adorRequestId :: !Text
      -- ^ The ID of the request.
    , adorReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AssociateDhcpOptionsResponse where
    xmlPickler = ec2XML

-- | Associates a subnet with a route table.
--
-- The subnet and route table must be in the same VPC.
-- This association causes traffic originating from the
-- subnet to be routed according to the routes in the route table. The action
-- returns an association ID, which you need in order to disassociate the
-- route table from the subnet later. A route table can be associated with
-- multiple subnets.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateRouteTable.html>
data AssociateRouteTable = AssociateRouteTable
    { artRouteTableId :: !Text
      -- ^ The ID of the route table.
    , artSubnetId     :: !Text
      -- ^ The ID of the subnet.
    } deriving (Eq, Show, Generic)

instance IsQuery AssociateRouteTable

instance Rq AssociateRouteTable where
    type Er AssociateRouteTable = EC2ErrorResponse
    type Rs AssociateRouteTable = AssociateRouteTableResponse
    request = query4 ec2 GET "AssociateRouteTable"

data AssociateRouteTableResponse = AssociateRouteTableResponse
    { artrRequestId     :: !Text
      -- ^ The ID of the request.
    , artrAssociationId :: !Text
      -- ^ The route table association ID (needed to disassociate the route table).
    } deriving (Eq, Show, Generic)

instance IsXML AssociateRouteTableResponse where
    xmlPickler = ec2XML

-- | Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachInternetGateway.html>
data AttachInternetGateway = AttachInternetGateway
    { aigInternetGatewayId :: !Text
      -- ^ The ID of the Internet gateway.
    , aigVpcId             :: !Text
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery AttachInternetGateway

instance Rq AttachInternetGateway where
    type Er AttachInternetGateway = EC2ErrorResponse
    type Rs AttachInternetGateway = AttachInternetGatewayResponse
    request = query4 ec2 GET "AttachInternetGateway"

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    { aigrRequestId :: !Text
      -- ^ The ID of the request.
    , aigrReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an error.
    } deriving (Eq, Show, Generic)

instance IsXML AttachInternetGatewayResponse where
    xmlPickler = ec2XML

-- | Attaches a network interface to an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachNetworkInterface.html>
data AttachNetworkInterface = AttachNetworkInterface
    { aniNetworkInterfaceId :: !Text
      -- ^ The ID of the network interface.
    , aniInstanceId         :: !Text
      -- ^ The ID of the instance.
    , aniDeviceIndex        :: !Integer
      -- ^ The index of the device for the network interface attachment.
    } deriving (Eq, Show, Generic)

instance IsQuery AttachNetworkInterface

instance Rq AttachNetworkInterface where
    type Er AttachNetworkInterface = EC2ErrorResponse
    type Rs AttachNetworkInterface = AttachNetworkInterfaceResponse
    request = query4 ec2 GET "AttachNetworkInterface"

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { anirRequestId    :: !Text
      -- ^ The ID of the attachment request.
    , anirAttachmentId :: !Text
      -- ^ The ID of the network interface attachment.
    } deriving (Eq, Show, Generic)

instance IsXML AttachNetworkInterfaceResponse where
    xmlPickler = ec2XML

-- | Attaches an Amazon EBS volume to a running or stopped instance and exposes
-- it to the instance with the specified device name.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVolume.html>
data AttachVolume = AttachVolume
    { avVolumeId   :: !Text
      -- ^ The ID of the Amazon EBS volume. The volume and instance must be
      -- within the same Availability Zone.
    , avInstanceId :: !Text
      -- ^ The ID of the instance.
    , avDevice     :: !Text
      -- ^ The device name to expose to the instance (for example, /dev/sdh
      -- or xvdh).
    } deriving (Eq, Show, Generic)

instance IsQuery AttachVolume

instance Rq AttachVolume where
    type Er AttachVolume = EC2ErrorResponse
    type Rs AttachVolume = AttachVolumeResponse
    request = query4 ec2 GET "AttachVolume"

data AttachVolumeResponse = AttachVolumeResponse
    { avrRequestId  :: !Text
      -- ^ The ID of the request.
    , avrVolumeId   :: !Text
      -- ^ The ID of the volume.
    , avrInstanceId :: !Text
      -- ^ The ID of the instance.
    , avrDevice     :: !Text
      -- ^ The device name.
    , avrStatus     :: !VolumeStatus
      -- ^ The attachment state of the volume.
    , avrAttachTime :: !UTCTime
      -- ^ The time stamp when the attachment initiated.
    } deriving (Eq, Show, Generic)

instance IsXML AttachVolumeResponse where
    xmlPickler = ec2XML

-- | Attaches a virtual private gateway to a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVpnGateway.html>
data AttachVpnGateway = AttachVpnGateway
    { avgVpnGatewayId :: !Text
      -- ^ The ID of the virtual private gateway.
    , avgVpcId        :: !Text
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery AttachVpnGateway

instance Rq AttachVpnGateway where
    type Er AttachVpnGateway = EC2ErrorResponse
    type Rs AttachVpnGateway = AttachVpnGatewayResponse
    request = query4 ec2 GET "AttachVpnGateway"

data AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { avgrRequestId  :: !Text
      -- ^ The ID of the request.
    , avgrAttachment :: !Attachment
      -- ^ Information about the attachment.
    } deriving (Eq, Show, Generic)

instance IsXML AttachVpnGatewayResponse where
    xmlPickler = ec2XML

-- | Adds one or more egress rules to a security group for use with a VPC.
--
-- Specifically, this action permits instances to send traffic to one or more
-- destination CIDR IP address ranges, or to one or more destination security
-- groups for the same VPC.
--
-- This action doesn't apply to security groups for use in EC2-Classic.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupEgress.html>
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { asgeGroupId       :: !Text
      -- ^ The ID of the security group to modify.
    , asgeIpPermissions :: [IpPermissionType]
      -- ^ The IP protocol name or number (see Protocol Numbers).
    } deriving (Eq, Show, Generic)

instance IsQuery AuthorizeSecurityGroupEgress

instance Rq AuthorizeSecurityGroupEgress where
    type Er AuthorizeSecurityGroupEgress = EC2ErrorResponse
    type Rs AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgressResponse
    request = query4 ec2 GET "AuthorizeSecurityGroupEgress"

data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
    { asgerRequestId :: !Text
      -- ^ The ID of the request.
    , asgerReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AuthorizeSecurityGroupEgressResponse where
    xmlPickler = ec2XML

-- | Adds one or more ingress rules to a security group.
--
-- Rule changes are propagated to instances within the security group as quickly as possible.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupIngress.html>
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { asgiGroupId       :: Maybe Text
      -- ^ The ID of the security group to modify. The security group must
      -- belong to your account.
    , asgiGroupName     :: Maybe Text
      -- ^ The name of the security group to modify.
    , asgiIpPermissions :: [IpPermissionType]
      -- ^ The IP protocol name or number (see Protocol Numbers). For
      -- EC2-Classic, security groups can have rules only for TCP, UDP,
      -- and ICMP. For EC2-VPC, security groups can have rules assigned to
      -- any protocol number.
    } deriving (Eq, Show, Generic)

instance IsQuery AuthorizeSecurityGroupIngress

instance Rq AuthorizeSecurityGroupIngress where
    type Er AuthorizeSecurityGroupIngress = EC2ErrorResponse
    type Rs AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngressResponse
    request = query4 ec2 GET "AuthorizeSecurityGroupIngress"

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    { asgirRequestId :: !Text
      -- ^ The ID of the request.
    , asgirReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML AuthorizeSecurityGroupIngressResponse where
    xmlPickler = ec2XML

-- | Bundles an Amazon instance store-backed Windows instance.During bundling,
-- only the root device volume (C:\) is bundled. Data on other instance store
-- volumes is not preserved.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-BundleInstance.html>
data BundleInstance = BundleInstance
    { biInstanceId :: !Text
      -- ^ The ID of the instance to bundle.
    , biStorage    :: !BundleInstanceTaskStorage
      -- ^ Bundle storage instructions.
    } deriving (Eq, Show, Generic)

instance IsQuery BundleInstance

instance Rq BundleInstance where
    type Er BundleInstance = EC2ErrorResponse
    type Rs BundleInstance = BundleInstanceResponse
    request = query4 ec2 GET "BundleInstance"

data BundleInstanceResponse = BundleInstanceResponse
    { birRequestId          :: !Text
      -- ^ The ID of the request.
    , birBundleInstanceTask :: !BundleInstanceTask
      -- ^ The bundle task.
    } deriving (Eq, Show, Generic)

instance IsXML BundleInstanceResponse where
    xmlPickler = ec2XML

-- | Cancels a bundling operation for an instance store-backed Windows instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelBundleTask.html>

data CancelBundleTask = CancelBundleTask
    { cbtBundleId :: !Text
      -- ^ The ID of the bundle task.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelBundleTask

instance Rq CancelBundleTask where
    type Er CancelBundleTask = EC2ErrorResponse
    type Rs CancelBundleTask = CancelBundleTaskResponse
    request = query4 ec2 GET "CancelBundleTask"

data CancelBundleTaskResponse = CancelBundleTaskResponse
    { cbtRequestId          :: !Text
      -- ^ The ID of the request.
    , cbtBundleInstanceTask :: !BundleInstanceTask
      -- ^ The bundle task.
    } deriving (Eq, Show, Generic)

instance IsXML CancelBundleTaskResponse where
    xmlPickler = ec2XML

-- | Cancels an active conversion task.
--
-- The task can be the import of an
-- instance or volume. The action removes all artifacts of the conversion,
-- including a partially uploaded volume or instance. If the conversion is
-- complete or is in the process of transferring the final disk image, the
-- command fails and returns an exception.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelConversionTask.html>

data CancelConversionTask = CancelConversionTask
    { cctConversionTaskId :: !Text
      -- ^ The ID of the conversion task.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelConversionTask

instance Rq CancelConversionTask where
    type Er CancelConversionTask = EC2ErrorResponse
    type Rs CancelConversionTask = CancelConversionTaskResponse
    request = query4 ec2 GET "CancelConversionTask"

data CancelConversionTaskResponse = CancelConversionTaskResponse
    { cctRequestId :: !Text
      -- ^ The ID of the request.
    , cctReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an
      -- error.
    } deriving (Eq, Show, Generic)

instance IsXML CancelConversionTaskResponse where
    xmlPickler = ec2XML

-- | Cancels an active export task. The request removes all artifacts of the
-- export, including any partially created Amazon S3 objects. If the export
-- task is complete or is in the process of transferring the final disk image,
-- the command fails and returns an error.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelExportTask.html>

data CancelExportTask = CancelExportTask
    { cetExportTaskId :: !Text
      -- ^ The ID of the export task. This is the ID returned by
      -- CreateInstanceExportTask.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelExportTask

instance Rq CancelExportTask where
    type Er CancelExportTask = EC2ErrorResponse
    type Rs CancelExportTask = CancelExportTaskResponse
    request = query4 ec2 GET "CancelExportTask"

data CancelExportTaskResponse = CancelExportTaskResponse
    { cetRequestId :: !Text
      -- ^ The ID of the request.
    , cetReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an error.
    } deriving (Eq, Show, Generic)

instance IsXML CancelExportTaskResponse where
    xmlPickler = ec2XML

-- | Cancels the specified Reserved Instance listing in the Reserved Instance Marketplace.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelReservedInstancesListing.html>

data CancelReservedInstancesListing = CancelReservedInstancesListing
    { crilReservedInstancesListingId :: !Text
      -- ^ The ID of the Reserved Instance listing to be canceled.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelReservedInstancesListing

instance Rq CancelReservedInstancesListing where
    type Er CancelReservedInstancesListing = EC2ErrorResponse
    type Rs CancelReservedInstancesListing = CancelReservedInstancesListingResponse
    request = query4 ec2 GET "CancelReservedInstancesListing"

data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { crilRequestId                    :: !Text
      -- ^ The ID of the request.
    , crilReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSetItemType
      -- ^ The Reserved Instance listing for cancellation.
    } deriving (Eq, Show, Generic)

instance IsXML CancelReservedInstancesListingResponse where
    xmlPickler = ec2XML

-- | Cancels one or more Spot Instance requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotInstanceRequests.html>

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { csirSpotInstanceRequestId :: [Text]
      -- ^ One or more Spot Instance request IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery CancelSpotInstanceRequests

instance Rq CancelSpotInstanceRequests where
    type Er CancelSpotInstanceRequests = EC2ErrorResponse
    type Rs CancelSpotInstanceRequests = CancelSpotInstanceRequestsResponse
    request = query4 ec2 GET "CancelSpotInstanceRequests"

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { csirRequestId              :: !Text
      -- ^ The ID of the request.
    , csirSpotInstanceRequestSet :: [CancelSpotInstanceRequestsResponseSetItemType]
      -- ^ A list of Spot Instance requests. Each request is wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML CancelSpotInstanceRequestsResponse where
    xmlPickler = ec2XML

-- -- | Determines whether a product code is associated with an instance. This
-- -- action can only be used by the owner of the product code. It is useful when
-- -- a product code owner needs to verify whether another user's instance is
-- -- eligible for support.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ConfirmProductInstance.html>

-- data ConfirmProductInstance = ConfirmProductInstance
--     { cpiProductCode :: !Text
--       -- ^ The product code. This must be an Amazon DevPay product code that
--       -- you own.
--     , cpiInstanceId  :: !Text
--       -- ^ The instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ConfirmProductInstance

-- instance IsXML ConfirmProductInstance where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ConfirmProductInstance ConfirmProductInstanceResponse where
--     request = query4 ec2 GET "ConfirmProductInstance"

-- data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
--     { cpiRequestId :: !Text
--       -- ^ The ID of the request.
--     , cpiReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     , cpiOwnerId   :: !Text
--       -- ^ The instance owner's account ID. Only present if the product code
--       -- is attached to the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ConfirmProductInstanceResponse where
--     xmlPickler = ec2XML

-- -- | Initiates the copy of an AMI from the specified source region to the region
-- -- in which the request was made.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopyImage.html>

-- data CopyImage = CopyImage
--     { ciSourceRegion  :: !Text
--       -- ^ The name of the region that contains the AMI to be copied
--       -- (source).
--     , ciSourceImageId :: !Text
--       -- ^ The ID of the AMI to copy.
--     , ciName          :: Maybe Text
--       -- ^ The name of the new AMI in the destination region.
--     , ciDescription   :: Maybe Text
--       -- ^ A description for the new AMI in the destination region.
--     , ciClientToken   :: Maybe Text
--       -- ^ Unique, case-sensitive identifier you provide to ensure
--       -- idempotency of the request.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CopyImage

-- instance IsXML CopyImage where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 CopyImage CopyImageResponse where
--     request = query4 ec2 GET "CopyImage"

-- data CopyImageResponse = CopyImageResponse
--     { ciRequestId :: !Text
--       -- ^ The ID of the request.
--     , ciImageId   :: !Text
--       -- ^ The ID of the new AMI.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CopyImageResponse where
--     xmlPickler = ec2XML

-- -- | Copies a point-in-time snapshot of an Amazon Elastic Block Store (Amazon
-- -- EBS) volume and stores it in Amazon Simple Storage Service (Amazon S3). You
-- -- can copy the snapshot within the same region or from one region to another.
-- -- You can use the snapshot to create Amazon EBS volumes or Amazon Machine
-- -- Images (AMIs).For more information about Amazon EBS, see Amazon Elastic
-- -- Block Store (Amazon EBS).
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopySnapshot.html>

-- data CopySnapshot = CopySnapshot
--     { csSourceRegion     :: !Text
--       -- ^ The ID of the region that contains the snapshot to be copied.
--     , csSourceSnapshotId :: !Text
--       -- ^ The ID of the Amazon EBS snapshot to copy.
--     , csDescription      :: Maybe Text
--       -- ^ A description for the new Amazon EBS snapshot.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CopySnapshot

-- instance AWSRequest EC2 CopySnapshot CopySnapshotResponse where
--     request = query4 ec2 GET "CopySnapshot"

-- data CopySnapshotResponse = CopySnapshotResponse
--     { csRequestId  :: !Text
--       -- ^ The ID of the request.
--     , csSnapshotId :: !Text
--       -- ^ The ID of the new snapshot.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CopySnapshotResponse where
--     xmlPickler = ec2XML

-- -- | Provides information to AWS about your VPN customer gateway device. The
-- -- customer gateway is the appliance at your end of the VPN connection. (The
-- -- device on the AWS side of the VPN connection is the virtual private
-- -- gateway.) You must provide the Internet-routable IP address of the customer
-- -- gateway's external interface. The IP address must be static and can't be
-- -- behind a device performing network address translation (NAT).You must
-- -- provide the Internet-routable IP address of the customer gateway's external
-- -- interface. The IP address must be static and can't be behind a device
-- -- performing network address translation (NAT).For devices that use Border
-- -- Gateway Protocol (BGP), you can also provide the device's BGP Autonomous
-- -- System Number (ASN). You can use an existing ASN assigned to your network.
-- -- If you don't have an ASN already, you can use a private ASN (in the 64512 -
-- -- 65534 range).For more information about ASNs, see the Wikipedia article.For
-- -- more information about VPN customer gateways, see Adding a Hardware Virtual
-- -- Private Gateway to Your VPC in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateCustomerGateway.html>

-- data CreateCustomerGateway = CreateCustomerGateway
--     { ccgType      :: !Text
--       -- ^ The type of VPN connection this customer gateway supports.
--     , ccgIpAddress :: !Text
--       -- ^ The Internet-routable IP address for the customer gateway's
--       -- outside interface. The address must be static.
--     , ccgBgpAsn    :: Maybe Integer
--       -- ^ For devices that support BGP, the customer gateway's Border
--       -- Gateway Protocol (BGP) Autonomous System Number (ASN).
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateCustomerGateway

-- instance AWSRequest EC2 CreateCustomerGateway CreateCustomerGatewayResponse where
--     request = query4 ec2 GET "CreateCustomerGateway"

-- data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
--     { ccgRequestId       :: !Text
--       -- ^ The ID of the request.
--     , ccgCustomerGateway :: !CustomerGatewayType
--       -- ^ Information about the customer gateway.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateCustomerGatewayResponse where
--     xmlPickler = ec2XML

-- -- | Creates a set of DHCP options for your VPC. After creating the set, you
-- -- must associate it with the VPC, causing all existing and new instances that
-- -- you launch in the VPC to use this set of DHCP options. The following are
-- -- the individual DHCP options you can specify. For more information about the
-- -- options, see RFC 2132.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateDhcpOptions.html>

-- data CreateDhcpOptions = CreateDhcpOptions
--     { cdoDhcpConfiguration :: Members DhcConfigurationItemType
--       -- ^ A list of dhcp configurations.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateDhcpOptions

-- instance AWSRequest EC2 CreateDhcpOptions CreateDhcpOptionsResponse where
--     request = query4 ec2 GET "CreateDhcpOptions"

-- data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse
--     { cdoRequestId   :: !Text
--       -- ^ The ID of the request.
--     , cdoDhcpOptions :: !DhcpOptionsType
--       -- ^ A set of DHCP options.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateDhcpOptionsResponse where
--     xmlPickler = ec2XML

-- -- | Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that is
-- -- either running or stopped.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateImage.html>

-- data CreateImage = CreateImage
--     { ciInstanceId         :: !Text
--       -- ^ The ID of the instance.
--     , cjName               :: !Text
--       -- ^ A name for the new image.
--     , cjDescription        :: Maybe Text
--       -- ^ A description for the new image.
--     , cjNoReboot           :: Maybe Bool
--       -- ^ By default this parameter is set to false, which means Amazon EC2
--       -- attempts to cleanly shut down the instance before image creation
--       -- and then reboots the instance. When the parameter is set to true,
--       -- Amazon EC2 doesn't shut down the instance before creating the
--       -- image. When this option is used, file system integrity on the
--       -- created image can't be guaranteed.
--     , cjBlockDeviceMapping :: Members BlockDeviceMappingItemType
--       -- ^ The device name exposed to the instance (for example, /dev/sdh or
--       -- xvdh).
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateImage

-- instance IsXML CreateImage where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 CreateImage CreateImageResponse where
--     request = query4 ec2 GET "CreateImage"

-- data CreateImageResponse = CreateImageResponse
--     { cjRequestId :: !Text
--       -- ^ The ID of the request.
--     , cjImageId   :: !Text
--       -- ^ The ID of the new AMI.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateImageResponse where
--     xmlPickler = ec2XML

-- -- | Exports a running or stopped instance to an Amazon S3 bucket.For
-- -- information about the supported operating systems, image formats, and known
-- -- limitations for the types of instances you can export, see Exporting EC2
-- -- Instances in the Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInstanceExportTask.html>

-- data CreateInstanceExportTask = CreateInstanceExportTask
--     { cietDescription       :: Maybe Text
--       -- ^ A description for the conversion task or the resource being
--       -- exported. The maximum length is 255 bytes.
--     , cietInstanceId        :: !Text
--       -- ^ The ID of the instance.
--     , cietTargetEnvironment :: !Text
--       -- ^ The target virtualization environment.
--     , cietExportToS3        :: Maybe ExportToS3Task
--       -- ^ The format for the exported image.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateInstanceExportTask

-- instance AWSRequest EC2 CreateInstanceExportTask CreateInstanceExportTaskResponse where
--     request = query4 ec2 GET "CreateInstanceExportTask"

-- data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
--     { cietRequestId  :: !Text
--       -- ^ The ID of the request.
--     , cietExportTask :: !ExportTaskResponseType
--       -- ^ The details of the created ExportVM task.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateInstanceExportTaskResponse where
--     xmlPickler = ec2XML

-- -- | Creates an Internet gateway for use with a VPC. After creating the Internet
-- -- gateway, you attach it to a VPC using AttachInternetGateway.For more
-- -- information about your VPC and Internet gateway, see the Amazon Virtual
-- -- Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInternetGateway.html>

-- data CreateInternetGateway = CreateInternetGateway
--     deriving (Eq, Read, Show, Generic)

-- instance IsQuery CreateInternetGateway

-- instance IsXML CreateInternetGateway where
--     xmlPickler = xpEmpty $ Just ec2NS

-- instance AWSRequest EC2 CreateInternetGateway CreateInternetGatewayResponse where
--     request = query4 ec2 GET "CreateInternetGateway"

-- data CreateInternetGatewayResponse = CreateInternetGatewayResponse
--     { cigRequestId       :: !Text
--       -- ^ The ID of the request.
--     , cigInternetGateway :: !InternetGatewayType
--       -- ^ Information about the Internet gateway
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateInternetGatewayResponse where
--     xmlPickler = ec2XML

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
    { ckpKeyName :: !Text
      -- ^ A unique name for the key pair.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateKeyPair

instance Rq CreateKeyPair where
    type Er CreateKeyPair = EC2ErrorResponse
    type Rs CreateKeyPair = CreateKeyPairResponse
    request = query4 ec2 GET "CreateKeyPair"

data CreateKeyPairResponse = CreateKeyPairResponse
    { ckpRequestId      :: !Text
      -- ^ The ID of the request.
    , ckqKeyName        :: !Text
      -- ^ The name of the key pair name.
    , ckqKeyFingerprint :: !Text
      -- ^ A SHA-1 digest of the DER encoded private key.
    , ckqKeyMaterial    :: !Text
      -- ^ An unencrypted PEM encoded RSA private key.
    } deriving (Eq, Show, Generic)

instance IsXML CreateKeyPairResponse where
    xmlPickler = ec2XML

-- -- | Creates a network ACL in a VPC. Network ACLs provide an optional layer of
-- -- security (on top of security groups) for the instances in your VPC.For more
-- -- information about network ACLs, see Network ACLs in the Amazon Virtual
-- -- Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkAcl.html>

-- data CreateNetworkAcl = CreateNetworkAcl
--     { cnaVpcId :: !Text
--       -- ^ The ID of the VPC.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateNetworkAcl

-- instance AWSRequest EC2 CreateNetworkAcl CreateNetworkAclResponse where
--     request = query4 ec2 GET "CreateNetworkAcl"

-- data CreateNetworkAclResponse = CreateNetworkAclResponse
--     { cnaRequestId  :: !Text
--       -- ^ The ID of the request.
--     , cnaNetworkAcl :: !NetworkAclType
--       -- ^ Information about the new network ACL.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateNetworkAclResponse where
--     xmlPickler = ec2XML

-- -- | Creates an entry (a rule) in a network ACL with the specified rule number.
-- -- Each network ACL has a set of numbered ingress rules and a separate set of
-- -- numbered egress rules. When determining whether a packet should be allowed
-- -- in or out of a subnet associated with the ACL, we process the entries in
-- -- the ACL according to the rule numbers, in ascending order. Each network ACL
-- -- has a set of ingress rules and a separate set of egress rules.After you add
-- -- an entry, you can't modify it; you must either replace it, or create a new
-- -- entry and delete the old one.For more information about network ACLs, see
-- -- Network ACLs in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkAclEntry.html>

-- data CreateNetworkAclEntry = CreateNetworkAclEntry
--     { cnaeNetworkAclId :: !Text
--       -- ^ The ID of the ACL.
--     , cnaeRuleNumber   :: !Integer
--       -- ^ The rule number to assign to the entry (for example, 100). ACL
--       -- entries are processed in ascending order by rule number.
--     , cnaeProtocol     :: !Integer
--       -- ^ The IP protocol the rule applies to. You can use -1 to mean all
--       -- protocols.
--     , cnaeRuleAction   :: !Text
--       -- ^ Indicates whether to allow or deny traffic that matches the rule.
--     , cnaeEgress       :: Maybe Bool
--       -- ^ Indicates whether this rule applies to egress traffic from the
--       -- subnet (true) or ingress traffic to the subnet (false).
--     , cnaeCidrBlock    :: !Text
--       -- ^ The CIDR range to allow or deny, in CIDR notation (for example,
--       -- 172.16.0.0/24).
--     , cnaeIcmp         :: Maybe IcmpType
--       -- ^ For the ICMP protocol, the ICMP code and type.
--     , cnaePortRange    :: Maybe PortRangeType
--       -- ^ The first port in the range.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateNetworkAclEntry

-- instance AWSRequest EC2 CreateNetworkAclEntry CreateNetworkAclEntryResponse where
--     request = query4 ec2 GET "CreateNetworkAclEntry"

-- data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
--     { cnaeRequestId :: !Text
--       -- ^ The ID of the request.
--     , cnaeReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateNetworkAclEntryResponse where
--     xmlPickler = ec2XML

-- -- | Creates a network interface in the specified subnet.For more information
-- -- about network interfaces, see Elastic Network Interfaces in the Amazon
-- -- Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkInterface.html>

-- data CreateNetworkInterface = CreateNetworkInterface
--     { cniSubnetId                       :: !Text
--       -- ^ The ID of the subnet to associate with the network interface.
--     , cniPrivateIpAddress               :: Maybe Text
--       -- ^ The primary private IP address of the network interface. If you
--       -- don't specify an IP address, Amazon EC2 will select one for you
--       -- from the subnet range.
--     , cniPrivateIpAddresses             :: Members PrivateIpAddresses
--       -- ^ The private IP address of the specified network interface. You
--       -- can use this parameter multiple times to specify explicit private
--       -- IP addresses for a network interface, but only one private IP
--       -- address can be designated as primary.
--     , cniSecondaryPrivateIpAddressCount :: Maybe Integer
--       -- ^ The number of secondary private IP addresses to assign to a
--       -- network interface. When you specify a number of secondary IP
--       -- addresses, Amazon EC2 selects these IP addresses within the
--       -- subnet range.
--     , cniDescription                    :: Maybe Text
--       -- ^ A description for the network interface.
--     , cniSecurityGroupId                :: Members SecurityGroupIdSetItemType
--       -- ^ The list of security group IDs for the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateNetworkInterface

-- instance IsXML CreateNetworkInterface where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 CreateNetworkInterface CreateNetworkInterfaceResponse where
--     request = query4 ec2 GET "CreateNetworkInterface"

-- data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
--     { cniRequestId        :: !Text
--       -- ^ The ID of the request.
--     , cniNetworkInterface :: !NetworkInterfaceType
--       -- ^ The network interface that was created.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateNetworkInterfaceResponse where
--     xmlPickler = ec2XML

-- -- | Creates a placement group that you launch cluster instances into. You must
-- -- give the group a name unique within the scope of your account.For more
-- -- information about placement groups and cluster instances, see Cluster
-- -- Instances in the Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreatePlacementGroup.html>

-- data CreatePlacementGroup = CreatePlacementGroup
--     { cpgGroupName :: !Text
--       -- ^ A name for the placement group.
--     , cpgStrategy  :: !Text
--       -- ^ The placement strategy.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreatePlacementGroup

-- instance AWSRequest EC2 CreatePlacementGroup CreatePlacementGroupResponse where
--     request = query4 ec2 GET "CreatePlacementGroup"

-- data CreatePlacementGroupResponse = CreatePlacementGroupResponse
--     { cpgRequestId :: !Text
--       -- ^ The ID of the request.
--     , cpgReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreatePlacementGroupResponse where
--     xmlPickler = ec2XML

-- -- | Creates a listing for Amazon EC2 Reserved Instances that will be sold in
-- -- the Reserved Instance Marketplace. You can submit one Reserved Instance
-- -- listing at a time.The Reserved Instance Marketplace matches sellers who
-- -- want to resell Reserved Instance capacity that they no longer need with
-- -- buyers who want to purchase additional capacity. Reserved Instances bought
-- -- and sold through the Reserved Instance Marketplace work like any other
-- -- Reserved Instances.If you want to sell your Reserved Instances, you must
-- -- first register as a Seller in the Reserved Instance Marketplace. After
-- -- completing the registration process, you can create a Reserved Instance
-- -- Marketplace listing of some or all of your Reserved Instances, and specify
-- -- the upfront price you want to receive for them. Your Reserved Instance
-- -- listings then become available for purchase.For more information about
-- -- Reserved Instance Marketplace, see Reserved Instance Marketplace in the
-- -- Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateReservedInstancesListing.html>

-- data CreateReservedInstancesListing = CreateReservedInstancesListing
--     { crilReservedInstancesId :: !Text
--       -- ^ The ID of the active Reserved Instance.
--     , crilInstanceCount       :: !Integer
--       -- ^ The number of instances that are a part of a Reserved Instance
--       -- account that will be listed in the Reserved Instance Marketplace.
--       -- This number should be less than or equal to the instance count
--       -- associated with the Reserved Instance ID specified in this call.
--     , crilPriceSchedules      :: !PriceScheduleRequestSetItemType
--       -- ^ A list specifying the price of the Reserved Instance for each
--       -- month remaining in the Reserved Instance term.
--     , crilClientToken         :: !Text
--       -- ^ Unique, case-sensitive identifier you provide to ensure
--       -- idempotency of your listings. This helps avoid duplicate
--       -- listings.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateReservedInstancesListing

-- instance AWSRequest EC2 CreateReservedInstancesListing CreateReservedInstancesListingResponse where
--     request = query4 ec2 GET "CreateReservedInstancesListing"

-- data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
--     { crimRequestId                   :: !Text
--       -- ^ The ID of the request.
--     , crimReservedInstancesListingSet :: !DescribeReservedInstancesListingsResponseSetItemType
--       -- ^ The Reserved Instances listing that was created. The listing
--       -- information is wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateReservedInstancesListingResponse where
--     xmlPickler = ec2XML

-- -- | Creates a route in a route table within a VPC. The route's target can be
-- -- either a gateway attached to the VPC or a NAT instance in the VPC.When
-- -- determining how to route traffic, we use the route with the most specific
-- -- match. For example, let's say the traffic is destined for 192.0.2.3, and
-- -- the route table includes the following two routes: Both routes apply to the
-- -- traffic destined for 192.0.2.3. However, the second route in the list
-- -- covers a smaller number of IP addresses and is therefore more specific, so
-- -- we use that route to determine where to target the traffic.For more
-- -- information about route tables, see Route Tables in the Amazon Virtual
-- -- Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRoute.html>

-- data CreateRoute = CreateRoute
--     { crRouteTableId         :: !Text
--       -- ^ The ID of the route table for the route.
--     , crDestinationCidrBlock :: !Text
--       -- ^ The CIDR address block used for the destination match. Routing
--       -- decisions are based on the most specific match.
--     , crGatewayId            :: !Text
--       -- ^ The ID of an Internet gateway attached to your VPC.
--     , crInstanceId           :: !Text
--       -- ^ The ID of a NAT instance in your VPC. The operation fails if you
--       -- specify an instance ID unless exactly one network interface is
--       -- attached.
--     , crNetworkInterfaceId   :: !Text
--       -- ^ The ID of a network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateRoute

-- instance IsXML CreateRoute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 CreateRoute CreateRouteResponse where
--     request = query4 ec2 GET "CreateRoute"

-- data CreateRouteResponse = CreateRouteResponse
--     { crRequestId :: !Text
--       -- ^ The ID of the request.
--     , crReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateRouteResponse where
--     xmlPickler = ec2XML

-- -- | Creates a route table for the specified VPC. After you create a route
-- -- table, you can add routes and associate the table with a subnet.For more
-- -- information about route tables, see Route Tables in the Amazon Virtual
-- -- Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRouteTable.html>

-- data CreateRouteTable = CreateRouteTable
--     { crtVpcId :: !Text
--       -- ^ The ID of the VPC.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateRouteTable

-- instance IsXML CreateRouteTable where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 CreateRouteTable CreateRouteTableResponse where
--     request = query4 ec2 GET "CreateRouteTable"

-- data CreateRouteTableResponse = CreateRouteTableResponse
--     { crtRequestId  :: !Text
--       -- ^ The ID of the request.
--     , crtRouteTable :: !RouteTableType
--       -- ^ Information about the newly created route table.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateRouteTableResponse where
--     xmlPickler = ec2XML

-- | Creates a security group.
--
-- A security group is for use with instances either in the EC2-Classic platform
-- or in a specific VPC.
--
-- When you create a security group, you specify a friendly name of your
-- choice. You can have a security group for use in EC2-Classic with the same
-- name as a security group for use in a VPC. However, you can't have two
-- security groups for use in EC2-Classic with the same name or two security
-- groups for use in a VPC with the same name.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSecurityGroup.html>
data CreateSecurityGroup = CreateSecurityGroup
    { csgGroupName        :: !Text
      -- ^ The name of the security group.
    , csgGroupDescription :: !Text
      -- ^ A description for the security group. This is informational only.
    , csgVpcId            :: Maybe Text
      -- ^ [EC2-VPC] The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateSecurityGroup

instance Rq CreateSecurityGroup where
    type Er CreateSecurityGroup = EC2ErrorResponse
    type Rs CreateSecurityGroup = CreateSecurityGroupResponse
    request = query4 ec2 GET "CreateSecurityGroup"

data CreateSecurityGroupResponse = CreateSecurityGroupResponse
    { csgrRequestId :: !Text
      -- ^ The ID of the request.
    , csgrReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an error.
    , csgrGroupId   :: !Text
      -- ^ The ID of the new security group.
    } deriving (Eq, Show, Generic)

instance IsXML CreateSecurityGroupResponse where
    xmlPickler = ec2XML

-- -- | Creates a snapshot of an Amazon EBS volume and stores it in Amazon S3. You
-- -- can use snapshots for backups, to make copies of instance store volumes,
-- -- and to save data before shutting down an instance.When a snapshot is
-- -- created, any AWS Marketplace product codes from the volume are propagated
-- -- to the snapshot.You can take a snapshot of an attached volume that is in
-- -- use. However, snapshots only capture data that has been written to your
-- -- Amazon EBS volume at the time the snapshot command is issued. This may
-- -- exclude any data that has been cached by any applications or the operating
-- -- system. If you can pause any file writes to the volume long enough to take
-- -- a snapshot, your snapshot should be complete. However, if you can't pause
-- -- all file writes to the volume, you should unmount the volume from within
-- -- the instance, issue the snapshot command, and then remount the volume to
-- -- ensure a consistent and complete snapshot. You may remount and use your
-- -- volume while the snapshot status is pending.To create a snapshot for Amazon
-- -- EBS volumes that serve as root devices, you should stop the instance before
-- -- taking the snapshot.To unmount the volume in Linux/UNIX, use the following
-- -- command:Where device_name is the device name (for example, /dev/sdh).To
-- -- unmount the volume in Windows, open Disk Management, right-click the volume
-- -- to unmount, and select Change Drive Letter and Path. Select the mount point
-- -- to remove, and then click Remove.For more information about Amazon EBS, see
-- -- Amazon Elastic Block Store in the Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSnapshot.html>

-- data CreateSnapshot = CreateSnapshot
--     { csVolumeId    :: !Text
--       -- ^ The ID of the Amazon EBS volume.
--     , ctDescription :: Maybe Text
--       -- ^ A description for the snapshot.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateSnapshot

-- instance AWSRequest EC2 CreateSnapshot CreateSnapshotResponse where
--     request = query4 ec2 GET "CreateSnapshot"

-- data CreateSnapshotResponse = CreateSnapshotResponse
--     { ctRequestId   :: !Text
--       -- ^ The ID of the request.
--     , ctSnapshotId  :: !Text
--       -- ^ The ID of the snapshot.
--     , ctVolumeId    :: !Text
--       -- ^ The ID of the volume.
--     , ctStatus      :: !Text
--       -- ^ The snapshot state.
--     , ctStartTime   :: !UTCTime
--       -- ^ The time stamp when the snapshot was initiated.
--     , ctProgress    :: !Text
--       -- ^ The progress of the snapshot, as a percentage.
--     , ctOwnerId     :: !Text
--       -- ^ The AWS account ID of the Amazon EBS snapshot owner.
--     , ctVolumeSize  :: !Text
--       -- ^ The size of the volume, in GiB.
--     , cuDescription :: !Text
--       -- ^ The description for the snapshot.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateSnapshotResponse where
--     xmlPickler = ec2XML

-- -- | Creates the datafeed for Spot Instances, enabling you to view Spot Instance
-- -- usage logs. You can create one data feed per account. For more information
-- -- about Spot Instances, see Spot Instances in the Amazon Elastic Compute
-- -- Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSpotDatafeedSubscription.html>

-- data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
--     { csdsBucket :: !Text
--       -- ^ The Amazon S3 bucket in which to store the Spot Instance
--       -- datafeed.
--     , csdsPrefix :: Maybe Text
--       -- ^ A prefix for the datafeed file names.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateSpotDatafeedSubscription

-- instance AWSRequest EC2 CreateSpotDatafeedSubscription CreateSpotDatafeedSubscriptionResponse where
--     request = query4 ec2 GET "CreateSpotDatafeedSubscription"

-- data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
--     { csdsRequestId                :: !Text
--       -- ^ The ID of the request.
--     , csdsSpotDatafeedSubscription :: !SpotDatafeedSubscriptionType
--       -- ^ Type: SpotDatafeedSubscriptionType
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateSpotDatafeedSubscriptionResponse where
--     xmlPickler = ec2XML

-- -- | Creates a subnet in an existing VPC.When you create each subnet, you
-- -- provide the VPC ID and the CIDR block you want for the subnet. After you
-- -- create a subnet, you can't change its CIDR block. The subnet's CIDR block
-- -- can be the same as the VPC's CIDR block (assuming you want only a single
-- -- subnet in the VPC), or a subset of the VPC's CIDR block. If you create more
-- -- than one subnet in a VPC, the subnets' CIDR blocks must not overlap. The
-- -- smallest subnet (and VPC) you can create uses a /28 netmask (16 IP
-- -- addresses), and the largest uses a /16 netmask (65,536 IP addresses).If you
-- -- add more than one subnet to a VPC, they're set up in a star topology with a
-- -- logical router in the middle. By default, you can create up to 20 subnets
-- -- in a VPC. If you need more than 20 subnets, you can request more by going
-- -- to Request to Increase Amazon VPC Limits.If you launch an instance in a VPC
-- -- using an Amazon EBS-backed AMI, the IP address doesn't change if you stop
-- -- and restart the instance (unlike a similar instance launched outside a VPC,
-- -- which gets a new IP address when restarted). It's therefore possible to
-- -- have a subnet with no running instances (they're all stopped), but no
-- -- remaining IP addresses available. For more information about Amazon
-- -- EBS-backed AMIs, see AMI Basics in the Amazon Elastic Compute Cloud User
-- -- Guide. For more information about subnets, see Your VPC and Subnets in the
-- -- Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSubnet.html>

-- data CreateSubnet = CreateSubnet
--     { csVpcId            :: !Text
--       -- ^ The ID of the VPC.
--     , csCidrBlock        :: !Text
--       -- ^ The CIDR block for the subnet. For example, 10.0.0.0/24.
--     , csAvailabilityZone :: Maybe Text
--       -- ^ The Availability Zone for the subnet.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateSubnet

-- instance AWSRequest EC2 CreateSubnet CreateSubnetResponse where
--     request = query4 ec2 GET "CreateSubnet"

-- data CreateSubnetResponse = CreateSubnetResponse
--     { cuRequestId :: !Text
--       -- ^ The ID of the request.
--     , cuSubnet    :: !SubnetType
--       -- ^ Information about the subnet.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateSubnetResponse where
--     xmlPickler = ec2XML

-- | Adds or overwrites one or more tags for the specified EC2 resource or
-- resources. Each resource can have a maximum of 10 tags. Each tag consists
-- of a key and optional value. Tag keys must be unique per resource.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateTags.html>
data CreateTags = CreateTags
    { ctResourceId :: [Text]
      -- ^ The IDs of one or more resources to tag. For example, ami-1a2b3c4d.
    , ctTag        :: [ResourceTagSetItemType]
      -- ^ The key for a tag.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateTags

instance Rq CreateTags where
    type Er CreateTags = EC2ErrorResponse
    type Rs CreateTags = CreateTagsResponse
    request = query4 ec2 GET "CreateTags"

data CreateTagsResponse = CreateTagsResponse
    { cvRequestId :: !Text
      -- ^ The ID of the request.
    , cvReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an error.
    } deriving (Eq, Show, Generic)

instance IsXML CreateTagsResponse where
    xmlPickler = ec2XML

-- -- | Creates an Amazon EBS volume that can be attached to any instance in the
-- -- same Availability Zone.Any AWS Marketplace product codes from the snapshot
-- -- are propagated to the volume.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVolume.html>

-- data CreateVolume = CreateVolume
--     { cvSize             :: Maybe Text
--       -- ^ The size of the volume, in GiBs.
--     , cvSnapshotId       :: !Text
--       -- ^ The snapshot from which to create the volume.
--     , cvAvailabilityZone :: !Text
--       -- ^ The Availability Zone in which to create the volume. Use
--       -- DescribeAvailabilityZones to list the Availability Zones that are
--       -- currently available to you.
--     , cvVolumeType       :: Maybe Text
--       -- ^ The volume type.
--     , cvIops             :: !Integer
--       -- ^ The number of I/O operations per second (IOPS) that the volume
--       -- supports.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateVolume

-- instance IsXML CreateVolume where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 CreateVolume CreateVolumeResponse where
--     request = query4 ec2 GET "CreateVolume"

-- data CreateVolumeResponse = CreateVolumeResponse
--     { cwRequestId        :: !Text
--       -- ^ The ID of the request.
--     , cwVolumeId         :: !Text
--       -- ^ The ID of the volume.
--     , cwSize             :: !Text
--       -- ^ The size of the volume, in GiBs.
--     , cwSnapshotId       :: !Text
--       -- ^ The snapshot from which the volume was created, if applicable.
--     , cwAvailabilityZone :: !Text
--       -- ^ The Availability Zone for the volume.
--     , cwStatus           :: !Text
--       -- ^ The volume state.
--     , cwCreateTime       :: !UTCTime
--       -- ^ The time stamp when volume creation was initiated.
--     , cwVolumeType       :: !Text
--       -- ^ The volume type.
--     , cwIops             :: !Integer
--       -- ^ The number of I/O operations per second (IOPS) that the volume
--       -- supports.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateVolumeResponse where
--     xmlPickler = ec2XML

-- -- | Creates a VPC with the specified CIDR block.The smallest VPC you can create
-- -- uses a /28 netmask (16 IP addresses), and the largest uses a /16 netmask
-- -- (65,536 IP addresses). To help you decide how big to make your VPC, see
-- -- Your VPC and Subnets in the Amazon Virtual Private Cloud User Guide.By
-- -- default, each instance you launch in the VPC has the default DHCP options,
-- -- which includes only a default DNS server that we provide
-- -- (AmazonProvidedDNS).For more information about DHCP options, see DHCP
-- -- Options Sets in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpc.html>

-- data CreateVpc = CreateVpc
--     { cvCidrBlock       :: !Text
--       -- ^ The CIDR block for the VPC (for example, 10.0.0.0/16).
--     , cvInstanceTenancy :: Maybe Text
--       -- ^ The supported tenancy options for instances launched into the
--       -- VPC. A value of default means that instances can be launched with
--       -- any tenancy; a value of dedicated means all instances launched
--       -- into the VPC are launched as dedicated tenancy instances
--       -- regardless of the tenancy assigned to the instance at launch.
--       -- Dedicated tenancy instances runs on single-tenant hardware.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateVpc

-- instance AWSRequest EC2 CreateVpc CreateVpcResponse where
--     request = query4 ec2 GET "CreateVpc"

-- data CreateVpcResponse = CreateVpcResponse
--     { cxRequestId :: !Text
--       -- ^ The ID of the request.
--     , cxVpc       :: !VpcType
--       -- ^ Information about the VPC.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateVpcResponse where
--     xmlPickler = ec2XML

-- -- | Creates a VPN connection between an existing virtual private gateway and a
-- -- VPN customer gateway. The only supported connection type is ipsec.1. The
-- -- response includes information that you need to give to your network
-- -- administrator to configure your customer gateway. We recommend that you use
-- -- the command line version of this operation (ec2-create-vpn-connection),
-- -- which lets you get the configuration information formatted in a friendlier
-- -- way. For information about the command, see ec2-create-vpn-connection in
-- -- the Amazon Elastic Compute Cloud Command Line Reference.If you decide to
-- -- shut down your VPN connection for any reason and later create a new VPN
-- -- connection, you must reconfigure your customer gateway with the new
-- -- information returned from this call.For more information about VPN
-- -- connections, see Adding a Hardware Virtual Private Gateway to Your VPC in
-- -- the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnConnection.html>

-- data CreateVpnConnection = CreateVpnConnection
--     { cvcType              :: !Text
--       -- ^ The type of VPN connection.
--     , cvcCustomerGatewayId :: !Text
--       -- ^ The ID of the customer gateway.
--     , cvcVpnGatewayId      :: !Text
--       -- ^ The ID of the virtual private gateway.
--     , cvcOptions           :: Maybe VpnConnectionOptions
--       -- ^ Indicates whether the VPN connection requires static routes. If
--       -- you are creating a VPN connection for a device that does not
--       -- support BGP, you must specify true.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateVpnConnection

-- instance AWSRequest EC2 CreateVpnConnection CreateVpnConnectionResponse where
--     request = query4 ec2 GET "CreateVpnConnection"

-- data CreateVpnConnectionResponse = CreateVpnConnectionResponse
--     { cvcRequestId     :: !Text
--       -- ^ The ID of the request.
--     , cvcVpnConnection :: !VpnConnectionType
--       -- ^ Information about the VPN connection.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateVpnConnectionResponse where
--     xmlPickler = ec2XML

-- -- | Creates a static route associated with a VPN connection between an existing
-- -- virtual private gateway and a VPN customer gateway. The static route allows
-- -- traffic to be routed from the virtual private gateway to the VPN customer
-- -- gateway.For more information about VPN connections, see Adding a Hardware
-- -- Virtual Private Gateway to Your VPC in the Amazon Virtual Private Cloud
-- -- User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnConnectionRoute.html>

-- data CreateVpnConnectionRoute = CreateVpnConnectionRoute
--     { cvcrDestinationCidrBlock :: !Text
--       -- ^ The CIDR block associated with the local subnet of the customer
--       -- network.
--     , cvcrVpnConnectionId      :: !Text
--       -- ^ The ID of the VPN connection.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateVpnConnectionRoute

-- instance IsXML CreateVpnConnectionRoute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 CreateVpnConnectionRoute CreateVpnConnectionRouteResponse where
--     request = query4 ec2 GET "CreateVpnConnectionRoute"

-- data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
--     { cvcrRequestId :: !Text
--       -- ^ The ID of the request.
--     , cvcrReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateVpnConnectionRouteResponse where
--     xmlPickler = ec2XML

-- -- | Creates a virtual private gateway. A virtual private gateway is the
-- -- VPC-side endpoint for your VPN connection. You can create a virtual private
-- -- gateway before creating the VPC itself. For more information about virtual
-- -- private gateways, see Adding a Hardware Virtual Private Gateway to Your VPC
-- -- in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html>

-- data CreateVpnGateway = CreateVpnGateway
--     { cvgType :: !Text
--       -- ^ The type of VPN connection this virtual private gateway supports.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateVpnGateway

-- instance AWSRequest EC2 CreateVpnGateway CreateVpnGatewayResponse where
--     request = query4 ec2 GET "CreateVpnGateway"

-- data CreateVpnGatewayResponse = CreateVpnGatewayResponse
--     { cvgRequestId  :: !Text
--       -- ^ The ID of the request.
--     , cvgVpnGateway :: !VpnGatewayType
--       -- ^ Information about the virtual private gateway.
--     } deriving (Eq, Show, Generic)

-- instance IsXML CreateVpnGatewayResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified VPN customer gateway. You must delete the VPN
-- -- connection before you can delete the customer gateway.For more information
-- -- about VPN customer gateways, see Adding a Hardware Virtual Private Gateway
-- -- to Your VPC in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteCustomerGateway.html>

-- data DeleteCustomerGateway = DeleteCustomerGateway
--     { dcgCustomerGatewayId :: !Text
--       -- ^ The ID of the customer gateway.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteCustomerGateway

-- instance AWSRequest EC2 DeleteCustomerGateway DeleteCustomerGatewayResponse where
--     request = query4 ec2 GET "DeleteCustomerGateway"

-- data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
--     { dcgRequestId :: !Text
--       -- ^ The ID of the request.
--     , dcgReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteCustomerGatewayResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified set of DHCP options. You must disassociate the set of
-- -- DHCP options before you can delete it. You can disassociate the set of DHCP
-- -- options by associating either a new set of options or the default set of
-- -- options with the VPC. For more information about DHCP options sets, see
-- -- DHCP Options Sets in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteDhcpOptions.html>

-- data DeleteDhcpOptions = DeleteDhcpOptions
--     { ddoDhcpOptionsId :: !Text
--       -- ^ The ID of the DHCP options set.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteDhcpOptions

-- instance AWSRequest EC2 DeleteDhcpOptions DeleteDhcpOptionsResponse where
--     request = query4 ec2 GET "DeleteDhcpOptions"

-- data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
--     { ddoRequestId :: !Text
--       -- ^ The ID of the request.
--     , ddoReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteDhcpOptionsResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified Internet gateway. You must detach the Internet
-- -- gateway from the VPC before you can delete it. For more information about
-- -- your VPC and Internet gateway, see the Amazon Virtual Private Cloud User
-- -- Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteInternetGateway.html>

-- data DeleteInternetGateway = DeleteInternetGateway
--     { digInternetGatewayId :: !Text
--       -- ^ The ID of the Internet gateway.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteInternetGateway

-- instance AWSRequest EC2 DeleteInternetGateway DeleteInternetGatewayResponse where
--     request = query4 ec2 GET "DeleteInternetGateway"

-- data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
--     { digRequestId :: !Text
--       -- ^ The ID of the request.
--     , digReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteInternetGatewayResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified key pair, by removing the public key from Amazon EC2.
-- -- You must own the key pair.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteKeyPair.html>

-- data DeleteKeyPair = DeleteKeyPair
--     { dkpKeyName :: !Text
--       -- ^ The name of the key pair.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteKeyPair

-- instance AWSRequest EC2 DeleteKeyPair DeleteKeyPairResponse where
--     request = query4 ec2 GET "DeleteKeyPair"

-- data DeleteKeyPairResponse = DeleteKeyPairResponse
--     { dkpRequestId :: !Text
--       -- ^ The ID of the request.
--     , dkpReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteKeyPairResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified network ACL. You can't delete the ACL if it's
-- -- associated with any subnets. You can't delete the default network ACL. For
-- -- more information about network ACLs, see Network ACLs in the Amazon Virtual
-- -- Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkAcl.html>

-- data DeleteNetworkAcl = DeleteNetworkAcl
--     { dnaNetworkAclId :: !Text
--       -- ^ The ID of the network ACL.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteNetworkAcl

-- instance AWSRequest EC2 DeleteNetworkAcl DeleteNetworkAclResponse where
--     request = query4 ec2 GET "DeleteNetworkAcl"

-- data DeleteNetworkAclResponse = DeleteNetworkAclResponse
--     { dnaRequestId :: !Text
--       -- ^ The ID of the request.
--     , dnaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteNetworkAclResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified ingress or egress entry (rule) from the specified
-- -- network ACL. For more information about network ACLs, see Network ACLs in
-- -- the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkAclEntry.html>

-- data DeleteNetworkAclEntry = DeleteNetworkAclEntry
--     { dnaeNetworkAclId :: !Text
--       -- ^ The ID of the network ACL.
--     , dnaeRuleNumber   :: !Integer
--       -- ^ The rule number for the entry to delete.
--     , dnaeEgress       :: Maybe Bool
--       -- ^ Indicates whether the rule is an egress rule (true) or ingress
--       -- rule (false).
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteNetworkAclEntry

-- instance AWSRequest EC2 DeleteNetworkAclEntry DeleteNetworkAclEntryResponse where
--     request = query4 ec2 GET "DeleteNetworkAclEntry"

-- data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
--     { dnaeRequestId :: !Text
--       -- ^ The ID of the request.
--     , dnaeReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteNetworkAclEntryResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified network interface. You must detach the network
-- -- interface before you can delete it.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkInterface.html>

-- data DeleteNetworkInterface = DeleteNetworkInterface
--     { dniNetworkInterfaceId :: !Text
--       -- ^ The ID of the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteNetworkInterface

-- instance IsXML DeleteNetworkInterface where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DeleteNetworkInterface DeleteNetworkInterfaceResponse where
--     request = query4 ec2 GET "DeleteNetworkInterface"

-- data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
--     { dniRequestId :: !Text
--       -- ^ The ID of the request.
--     , dniReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteNetworkInterfaceResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified placement group. You must terminate all instances in
-- -- the placement group before you can delete the placement group. For more
-- -- information about placement groups and cluster instances, see Cluster
-- -- Instances in the Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeletePlacementGroup.html>

-- data DeletePlacementGroup = DeletePlacementGroup
--     { dpgGroupName :: !Text
--       -- ^ The name of the placement group.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeletePlacementGroup

-- instance AWSRequest EC2 DeletePlacementGroup DeletePlacementGroupResponse where
--     request = query4 ec2 GET "DeletePlacementGroup"

-- data DeletePlacementGroupResponse = DeletePlacementGroupResponse
--     { dpgRequestId :: !Text
--       -- ^ The ID of the request.
--     , dpgReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeletePlacementGroupResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified route from the specified route table. For more
-- -- information about route tables, see Route Tables in the Amazon Virtual
-- -- Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteRoute.html>

-- data DeleteRoute = DeleteRoute
--     { drRouteTableId         :: !Text
--       -- ^ The ID of the route table.
--     , drDestinationCidrBlock :: !Text
--       -- ^ The CIDR range for the route. The value you specify must match
--       -- the CIDR for the route exactly.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteRoute

-- instance IsXML DeleteRoute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DeleteRoute DeleteRouteResponse where
--     request = query4 ec2 GET "DeleteRoute"

-- data DeleteRouteResponse = DeleteRouteResponse
--     { drRequestId :: !Text
--       -- ^ The ID of the request.
--     , drReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteRouteResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified route table. You must disassociate the route table
-- -- from any subnets before you can delete it. You can't delete the main route
-- -- table. For more information about route tables, see Route Tables in the
-- -- Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteRouteTable.html>

-- data DeleteRouteTable = DeleteRouteTable
--     { drtRouteTableId :: !Text
--       -- ^ The ID of the route table.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteRouteTable

-- instance IsXML DeleteRouteTable where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DeleteRouteTable DeleteRouteTableResponse where
--     request = query4 ec2 GET "DeleteRouteTable"

-- data DeleteRouteTableResponse = DeleteRouteTableResponse
--     { drtRequestId :: !Text
--       -- ^ The ID of the request.
--     , drtReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteRouteTableResponse where
--     xmlPickler = ec2XML

-- | Deletes a security group.A security group is for use with instances either
-- in the EC2-Classic platform or in a specific VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSecurityGroup.html>
data DeleteSecurityGroup = DeleteSecurityGroup
    { dsgGroupName :: Maybe Text
      -- ^ The name of the security group.
    , dsgGroupId   :: Maybe Text
      -- ^ The ID of the security group.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteSecurityGroup

instance Rq DeleteSecurityGroup where
    type Er DeleteSecurityGroup = EC2ErrorResponse
    type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse
    request = query4 ec2 GET "DeleteSecurityGroup"

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    { dsgrRequestId :: !Text
      -- ^ The ID of the request.
    , dsgrReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an error.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteSecurityGroupResponse where
    xmlPickler = ec2XML

-- -- | Deletes the specified snapshot.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSnapshot.html>

-- data DeleteSnapshot = DeleteSnapshot
--     { dsSnapshotId :: !Text
--       -- ^ The ID of the Amazon EBS snapshot.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteSnapshot

-- instance AWSRequest EC2 DeleteSnapshot DeleteSnapshotResponse where
--     request = query4 ec2 GET "DeleteSnapshot"

-- data DeleteSnapshotResponse = DeleteSnapshotResponse
--     { dsRequestId :: !Text
--       -- ^ The ID of the request.
--     , dsReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteSnapshotResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the datafeed for Spot Instances. For more information about Spot
-- -- Instances, see Spot Instances in the Amazon Elastic Compute Cloud User
-- -- Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSpotDatafeedSubscription.html>

-- data DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
--     deriving (Eq, Read, Show, Generic)

-- instance IsQuery DeleteSpotDatafeedSubscription

-- instance IsXML DeleteSpotDatafeedSubscription where
--     xmlPickler = xpEmpty $ Just ec2NS

-- instance AWSRequest EC2 DeleteSpotDatafeedSubscription DeleteSpotDatafeedSubscriptionResponse where
--     request = query4 ec2 GET "DeleteSpotDatafeedSubscription"

-- data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
--     { dsdsRequestId :: !Text
--       -- ^ The ID of the request.
--     , dsdsReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteSpotDatafeedSubscriptionResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified subnet. You must terminate all running instances in
-- -- the subnet before you can delete the subnet.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSubnet.html>

-- data DeleteSubnet = DeleteSubnet
--     { dsSubnetId :: !Text
--       -- ^ The ID of the subnet.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteSubnet

-- instance AWSRequest EC2 DeleteSubnet DeleteSubnetResponse where
--     request = query4 ec2 GET "DeleteSubnet"

-- data DeleteSubnetResponse = DeleteSubnetResponse
--     { dtRequestId :: !Text
--       -- ^ The ID of the request.
--     , dtReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteSubnetResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified set of tags from the specified set of resources. This
-- -- call is designed to follow a DescribeTags call.For more information about
-- -- tags, see Tagging Your Resources in the Amazon Elastic Compute Cloud User
-- -- Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteTags.html>

-- data DeleteTags = DeleteTags
--     { dtResourceId :: Members Text
--       -- ^ The ID of the resource. For example, ami-1a2b3c4d. You can
--       -- specify more than one resource ID.
--     , dtTag        :: Members TagType
--       -- ^ The tag's key. You can specify more than one tag to delete.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteTags

-- instance AWSRequest EC2 DeleteTags DeleteTagsResponse where
--     request = query4 ec2 GET "DeleteTags"

-- data DeleteTagsResponse = DeleteTagsResponse
--     { duRequestId :: !Text
--       -- ^ The ID of the request.
--     , duReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteTagsResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified Amazon EBS volume. The volume must be in the
-- -- available state (not attached to an instance). For more information about
-- -- Amazon EBS, see Amazon Elastic Block Store in the Amazon Elastic Compute
-- -- Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVolume.html>

-- data DeleteVolume = DeleteVolume
--     { dvVolumeId :: !Text
--       -- ^ The ID of the volume.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteVolume

-- instance IsXML DeleteVolume where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DeleteVolume DeleteVolumeResponse where
--     request = query4 ec2 GET "DeleteVolume"

-- data DeleteVolumeResponse = DeleteVolumeResponse
--     { dvRequestId :: !Text
--       -- ^ The ID of the request.
--     , dvReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteVolumeResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified VPC. You must detach or delete all gateways and
-- -- resources that are associated with the VPC before you can delete it. For
-- -- example, you must terminate all instances running in the VPC, delete all
-- -- security groups associated with the VPC (except the default one), delete
-- -- all route tables associated with the VPC (except the default one), and so
-- -- on.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpc.html>

-- data DeleteVpc = DeleteVpc
--     { dvVpcId :: !Text
--       -- ^ The ID of the VPC.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteVpc

-- instance AWSRequest EC2 DeleteVpc DeleteVpcResponse where
--     request = query4 ec2 GET "DeleteVpc"

-- data DeleteVpcResponse = DeleteVpcResponse
--     { dwRequestId :: !Text
--       -- ^ The ID of the request.
--     , dwReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteVpcResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified VPN connection.If you're deleting the VPC and its
-- -- associated components, we recommend that you detach the virtual private
-- -- gateway from the VPC and delete the VPC before deleting the VPN
-- -- connection.Another reason to use this command is if you believe that the
-- -- tunnel credentials for your VPN connection have been compromised. In that
-- -- situation, you can delete the VPN connection and create a new one that has
-- -- new keys, without needing to delete the VPC or virtual private gateway. If
-- -- you create a new VPN connection, you must reconfigure the customer gateway
-- -- using the new configuration information returned with the new VPN
-- -- connection ID.For more information about VPN connections, see Adding a
-- -- Hardware Virtual Private Gateway to Your VPC in the Amazon Virtual Private
-- -- Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnConnection.html>

-- data DeleteVpnConnection = DeleteVpnConnection
--     { dvcVpnConnectionId :: !Text
--       -- ^ The ID of the VPN connection.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteVpnConnection

-- instance AWSRequest EC2 DeleteVpnConnection DeleteVpnConnectionResponse where
--     request = query4 ec2 GET "DeleteVpnConnection"

-- data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
--     { dvcRequestId :: !Text
--       -- ^ The ID of the request.
--     , dvcReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteVpnConnectionResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified static route associated with a VPN connection between
-- -- an existing virtual private gateway and a VPN customer gateway. The static
-- -- route allows traffic to be routed from the virtual private gateway to the
-- -- VPN customer gateway.For more information about VPN connections, see Adding
-- -- a Hardware Virtual Private Gateway to Your VPC in the Amazon Virtual
-- -- Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnConnectionRoute.html>

-- data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
--     { dvcrDestinationCidrBlock :: !Text
--       -- ^ The CIDR block associated with the local subnet of the customer
--       -- network.
--     , dvcrVpnConnectionId      :: !Text
--       -- ^ The ID of the VPN connection.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteVpnConnectionRoute

-- instance IsXML DeleteVpnConnectionRoute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DeleteVpnConnectionRoute DeleteVpnConnectionRouteResponse where
--     request = query4 ec2 GET "DeleteVpnConnectionRoute"

-- data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
--     { dvcrRequestId :: !Text
--       -- ^ The ID of the request.
--     , dvcrReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteVpnConnectionRouteResponse where
--     xmlPickler = ec2XML

-- -- | Deletes the specified virtual private gateway. We recommend that before you
-- -- delete a virtual private gateway, you detach it from the VPC and delete the
-- -- VPN connection. Note that you don't need to delete the virtual private
-- -- gateway if you plan to delete and recreate the VPN connection between your
-- -- VPC and your network.For more information about virtual private gateways,
-- -- see Adding a Hardware Virtual Private Gateway to Your VPC in the Amazon
-- -- Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnGateway.html>

-- data DeleteVpnGateway = DeleteVpnGateway
--     { dvgVpnGatewayId :: !Text
--       -- ^ The ID of the virtual private gateway.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteVpnGateway

-- instance AWSRequest EC2 DeleteVpnGateway DeleteVpnGatewayResponse where
--     request = query4 ec2 GET "DeleteVpnGateway"

-- data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
--     { dvgRequestId :: !Text
--       -- ^ The ID of the request.
--     , dvgReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteVpnGatewayResponse where
--     xmlPickler = ec2XML

-- -- | Deregisters the specified AMI. After you deregister an AMI, it can't be
-- -- used to launch new instances.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeregisterImage.html>

-- data DeregisterImage = DeregisterImage
--     { diImageId :: !Text
--       -- ^ The ID of the AMI.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeregisterImage

-- instance IsXML DeregisterImage where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DeregisterImage DeregisterImageResponse where
--     request = query4 ec2 GET "DeregisterImage"

-- data DeregisterImageResponse = DeregisterImageResponse
--     { diRequestId :: !Text
--       -- ^ The ID of the request.
--     , diReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeregisterImageResponse where
--     xmlPickler = ec2XML

-- -- | Describes the specified attribute of your AWS account.The following are the
-- -- supported account attributes.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAccountAttributes.html>

-- data DescribeAccountAttributes = DescribeAccountAttributes
--     { daaAttributeName :: Members Text
--       -- ^ One or more account attribute names.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeAccountAttributes

-- instance AWSRequest EC2 DescribeAccountAttributes DescribeAccountAttributesResponse where
--     request = query4 ec2 GET "DescribeAccountAttributes"

-- data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
--     { daaRequestId           :: !Text
--       -- ^ The ID of the request.
--     , daaAccountAttributeSet :: !AccountAttributeSetItemType
--       -- ^ A list of the names and values of the requested attributes, each
--       -- one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAccountAttributesResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your Elastic IP addresses.An Elastic IP address is
-- -- for use in either the EC2-Classic platform or in a VPC. For more
-- -- information, see Elastic IP Addresses in the Amazon Elastic Compute Cloud
-- -- User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAddresses.html>

-- -- data AddressFilter =
-- --     , daDomain                     :: !Text
-- --       -- ^ Indicates whether the address is for use in a VPC.
-- --     , daInstance-id                :: !Text
-- --       -- ^ The instance the address is associated with (if any).
-- --     , daPublic-ip                  :: !Text
-- --       -- ^ The Elastic IP address.
-- --     , daAllocation-id              :: !Text
-- --       -- ^ The allocation ID for the address (VPC only).
-- --     , daAssociation-id             :: !Text
-- --       -- ^ The association ID for the address (VPC only).
-- --     , daNetwork-interface-id       :: !Text
-- --       -- ^ The network interface (if any) that the address is associated
-- --       -- with (VPC only).
-- --     , daNetwork-interface-owner-id :: !Text
-- --       -- ^ The owner IID.
-- --     , daPrivate-ip-address         :: !Text
-- --       -- ^ The private IP address associated with the Elastic IP address
-- --       -- (VPC only).

-- data DescribeAddresses = DescribeAddresses
--     { daPublicIp                   :: Members Text
--       -- ^ [EC2-Classic] One or more Elastic IP addresses.
--     , daAllocationId               :: Members Text
--       -- ^ [EC2-VPC] One or more allocation IDs.
--     , daFilter                     :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeAddresses

-- instance AWSRequest EC2 DescribeAddresses DescribeAddressesResponse where
--     request = query4 ec2 GET "DescribeAddresses"

-- data DescribeAddressesResponse = DescribeAddressesResponse
--     { daRequestId    :: !Text
--       -- ^ The ID of the request.
--     , daAddressesSet :: !DescribeAddressesResponseItemType
--       -- ^ A list of Elastic IP addresses, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAddressesResponse where
--     xmlPickler = ec2XML

-- | Describes one or more of the Availability Zones that are available to you.
-- The results include zones only for the region you're currently using.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAvailabilityZones.html>
data DescribeAvailabilityZones = DescribeAvailabilityZones
    { dazZoneName    :: [Text]
      -- ^ One or more Availability Zone names.
    , dazFilter      :: [Filter]
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

-- data AvailabilityZoneFilter
--     , dazMessage     :: !Text
--       -- ^ Information about the Availability Zone.
--     , dazRegion-name :: !Text
--       -- ^ The region for the Availability Zone (for example, us-east-1).
--     , dazState       :: !Text
--       -- ^ The state of the Availability Zone
--     , dazZone-name   :: !Text
--       -- ^ The name of the zone.

instance IsQuery DescribeAvailabilityZones

instance Rq DescribeAvailabilityZones where
    type Er DescribeAvailabilityZones = EC2ErrorResponse
    type Rs DescribeAvailabilityZones = DescribeAvailabilityZonesResponse
    request = query4 ec2 GET "DescribeAvailabilityZones"

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { dazrRequestId            :: !Text
      -- ^ The ID of the request.
    , dazrAvailabilityZoneInfo :: [AvailabilityZoneItemType]
      -- ^ A list of Availability Zones.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAvailabilityZonesResponse where
    xmlPickler = ec2XML

-- -- | Describes one or more of your bundling tasks.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeBundleTasks.html>

-- -- data BundleFilter
-- --     , dbtBundle-id     :: !Text
-- --       -- ^ The ID of the bundle task.
-- --     , dbtError-code    :: !Text
-- --       -- ^ If the task failed, the error code returned.
-- --     , dbtError-message :: !Text
-- --       -- ^ If the task failed, the error message returned.
-- --     , dbtInstance-id   :: !Text
-- --       -- ^ The ID of the instance that was bundled.
-- --     , dbtProgress      :: !Text
-- --       -- ^ The level of task completion, as a percentage (for example, 20%).
-- --     , dbtS3-bucket     :: !Text
-- --       -- ^ The Amazon S3 bucket to store the AMI.
-- --     , dbtS3-prefix     :: !Text
-- --       -- ^ The beginning of the AMI name.
-- --     , dbtStart-time    :: !UTCTime
-- --       -- ^ The time the task started (for example,
-- --       -- 2008-09-15T17:15:20.000Z).
-- --     , dbtState         :: !Text
-- --       -- ^ The state of the task.
-- --     , dbtUpdate-time   :: !UTCTime
-- --       -- ^ The time of the most recent update for the task (for example,
-- --       -- 2008-09-15T17:15:20.000Z).


-- data DescribeBundleTasks = DescribeBundleTasks
--     { dbtBundleId      :: Members Text
--       -- ^ One or more bundle task IDs.
--     , dbtFilter        :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeBundleTasks

-- instance AWSRequest EC2 DescribeBundleTasks DescribeBundleTasksResponse where
--     request = query4 ec2 GET "DescribeBundleTasks"

-- data DescribeBundleTasksResponse = DescribeBundleTasksResponse
--     { dbtRequestId              :: !Text
--       -- ^ The ID of the request.
--     , dbtBundleInstanceTasksSet :: !BundleInstanceTaskType
--       -- ^ A list of bundle tasks, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeBundleTasksResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your conversion tasks.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeConversionTasks.html>

-- data DescribeConversionTasks = DescribeConversionTasks
--     { dctConversionTaskId :: Members Text
--       -- ^ One or more conversion task IDs.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeConversionTasks

-- instance AWSRequest EC2 DescribeConversionTasks DescribeConversionTasksResponse where
--     request = query4 ec2 GET "DescribeConversionTasks"

-- data DescribeConversionTasksResponse = DescribeConversionTasksResponse
--     { dctConversionTasks :: !ConversionTaskType
--       -- ^ A list of conversion tasks, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeConversionTasksResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your VPN customer gateways.For more information
-- -- about VPN customer gateways, see Adding a Hardware Virtual Private Gateway
-- -- to Your VPC in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeCustomerGateways.html>

-- -- data GatewayFilter
-- --     , dchBgp-asn             :: !Text
-- --       -- ^ The customer gateway's Border Gateway Protocol (BGP) Autonomous
-- --       -- System Number (ASN).
-- --     , dchCustomer-gateway-id :: !Text
-- --       -- ^ The ID of the customer gateway.
-- --     , dchIp-address          :: !Text
-- --       -- ^ The IP address of the customer gateway's Internet-routable
-- --       -- external interface (for example, 12.1.2.3).
-- --     , dchState               :: !Text
-- --       -- ^ The state of the customer gateway.
-- --     , dchType                :: !Text
-- --       -- ^ The type of customer gateway. Currently the only supported type
-- --       -- is ipsec.1.
-- --     , dchTag-key             :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , dchTag-value           :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , dchTag:                :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , dchKey                 :: !Text
-- --       -- ^

-- data DescribeCustomerGateways = DescribeCustomerGateways
--     { dchCustomerGatewayId   :: Members Text
--       -- ^ One or more customer gateway IDs.
--     , dchFilter              :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeCustomerGateways

-- instance AWSRequest EC2 DescribeCustomerGateways DescribeCustomerGatewaysResponse where
--     request = query4 ec2 GET "DescribeCustomerGateways"

-- data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
--     { dchRequestId          :: !Text
--       -- ^ The ID of the request.
--     , dchCustomerGatewaySet :: !CustomerGatewayType
--       -- ^ A list of customer gateways, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeCustomerGatewaysResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your DHCP options sets.For more information about
-- -- DHCP options sets, see DHCP Options Sets in the Amazon Virtual Private
-- -- Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeDhcpOptions.html>

-- -- data DhcpFilter
-- --     , ddpDhcp-options-id :: !Text
-- --       -- ^ The ID of a set of DHCP options.
-- --     , ddpKey             :: !Text
-- --       -- ^ The key for one of the options (for example, domain-name).
-- --     , ddpValue           :: !Text
-- --       -- ^ The value for one of the options.
-- --     , ddpTag-key         :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , ddpTag-value       :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , ddpTag:            :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , ddqKey             :: !Text
-- --       -- ^

-- data DescribeDhcpOptions = DescribeDhcpOptions
--     { ddpDhcpOptionsId   :: Members Text
--       -- ^ The IDs of one or more DHCP options sets.
--     , ddpFilter          :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeDhcpOptions

-- instance AWSRequest EC2 DescribeDhcpOptions DescribeDhcpOptionsResponse where
--     request = query4 ec2 GET "DescribeDhcpOptions"

-- data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
--     { ddqRequestId      :: !Text
--       -- ^ The ID of the request.
--     , ddqDhcpOptionsSet :: !DhcpOptionsType
--       -- ^ A list of DHCP options sets, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeDhcpOptionsResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your export tasks.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeExportTasks.html>

-- data DescribeExportTasks = DescribeExportTasks
--     { detExportTaskId :: Members Text
--       -- ^ One or more export task IDs.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeExportTasks

-- instance AWSRequest EC2 DescribeExportTasks DescribeExportTasksResponse where
--     request = query4 ec2 GET "DescribeExportTasks"

-- data DescribeExportTasksResponse = DescribeExportTasksResponse
--     { detRequestId     :: !Text
--       -- ^ The ID of the request.
--     , detExportTaskSet :: !ExportTaskResponseType
--       -- ^ A list of export tasks, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeExportTasksResponse where
--     xmlPickler = ec2XML

-- -- | Describes an attributes of an AMI. You can specify only one attribute at a
-- -- time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImageAttribute.html>

-- data DescribeImageAttribute = DescribeImageAttribute
--     { diaImageId   :: !Text
--       -- ^ The ID of the AMI.
--     , diaAttribute :: !Text
--       -- ^ The AMI attribute.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeImageAttribute

-- instance IsXML DescribeImageAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DescribeImageAttribute DescribeImageAttributeResponse where
--     request = query4 ec2 GET "DescribeImageAttribute"

-- data DescribeImageAttributeResponse = DescribeImageAttributeResponse
--     { diaRequestId          :: !Text
--       -- ^ The ID of the request.
--     , dibImageId            :: !Text
--       -- ^ The ID of the AMI.
--     , dibLaunchPermission   :: !LaunchPermissionItemType
--       -- ^ A list of launch permissions, each one wrapped in an item
--       -- element.
--     , dibProductCodes       :: !ProductCodeItemType
--       -- ^ A list of product codes, each one wrapped in an item element that
--       -- contains a product code and a product code type.
--     , dibKernel             :: !Text
--       -- ^ The kernel ID, wrapped in a value element.
--     , dibRamdisk            :: !Text
--       -- ^ The RAM disk ID, wrapped in a value element.
--     , dibDescription        :: !Text
--       -- ^ A user-created description for the AMI, wrapped in a value
--       -- element.
--     , dibBlockDeviceMapping :: !BlockDeviceMappingItemType
--       -- ^ One or more block device mapping entries, each one wrapped in an
--       -- item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeImageAttributeResponse where
--     xmlPickler = ec2XML

-- | Describes one or more of the images (AMIs, AKIs, and ARIs) available to
-- you. Images available to you include public images, private images that you
-- own, and private images owned by other AWS accounts but for which you have
-- explicit launch permissions.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImages.html>
data DescribeImages = DescribeImages
    { djExecutableBy         :: [Text]
      -- ^ Describes the images for which the specified user has explicit
      -- launch permissions. The user ID can be an AWS account ID, self to
      -- return images for which the sender of the request has explicit
      -- launch permissions, or all to return AMIs with public launch permissions.
    , djImageId              :: [Text]
      -- ^ One or more image IDs.
    , djOwner                :: [Text]
      -- ^ Describes images owned by the specified owners. Use the IDs
      -- amazon, aws-marketplace, and self to describe images owned by
      -- Amazon, AWS Marketplace, or you, respectively.
    , djFilter               :: [Filter]
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeImages

instance Rq DescribeImages where
    type Er DescribeImages = EC2ErrorResponse
    type Rs DescribeImages = DescribeImagesResponse
    request = query4 ec2 GET "DescribeImages"

data DescribeImagesResponse = DescribeImagesResponse
    { djRequestId :: !Text
      -- ^ The ID of the request.
    , djImagesSet :: [DescribeImagesResponseItemType]
      -- ^ A list of images.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeImagesResponse where
    xmlPickler = ec2XML

-- -- | Describes an attribute of the specified instance. You can specify only one
-- -- attribute at a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceAttribute.html>

-- data DescribeInstanceAttribute = DescribeInstanceAttribute
--     { diaInstanceId :: !Text
--       -- ^ The ID of the instance.
--     , dibAttribute  :: !Text
--       -- ^ The instance attribute.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeInstanceAttribute

-- instance IsXML DescribeInstanceAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DescribeInstanceAttribute DescribeInstanceAttributeResponse where
--     request = query4 ec2 GET "DescribeInstanceAttribute"

-- data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
--     { dibRequestId                         :: !Text
--       -- ^ The ID of the request.
--     , dibInstanceId                        :: !Text
--       -- ^ The ID of the instance.
--     , dicBlockDeviceMapping                :: !InstanceBlockDeviceMappingResponseItemType
--       -- ^ The block device mapping of the instance.
--     , dicDisableApiTermination             :: !Bool
--       -- ^ If the value is true, you can't terminate the instance through
--       -- the Amazon EC2 console, CLI, or API; otherwise, you can.
--     , dicEbsOptimized                      :: !Bool
--       -- ^ Indicates whether the instance is optimized for EBS I/O.
--     , dicGroupSet                          :: !GroupItemType
--       -- ^ The security groups associated with the instance.
--     , dicInstanceInitiatedShutdownBehavior :: !Text
--       -- ^ Indicates whether an instance stops or terminates when you
--       -- initiate shutdown from the instance (using the operating system
--       -- command for system shutdown).
--     , dicInstanceType                      :: !Text
--       -- ^ The instance type.
--     , dicKernel                            :: !Text
--       -- ^ The kernel ID.
--     , dicProductCodes                      :: !ProductCodesSetItemType
--       -- ^ A list of product codes.
--     , dicRamdisk                           :: !Text
--       -- ^ The RAM disk ID.
--     , dicRootDeviceName                    :: !Text
--       -- ^ The name of the root device (for example, /dev/sda1).
--     , dicSourceDestCheck                   :: !Bool
--       -- ^ Indicates whether source/destination checking is enabled. A value
--       -- of true means checking is enabled, and false means checking is
--       -- disabled. This value must be false for a NAT instance to perform
--       -- NAT.
--     , dicUserData                          :: !Text
--       -- ^ The Base64-encoded MIME user data.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeInstanceAttributeResponse where
--     xmlPickler = ec2XML

-- | Describes one or more of your instances.
--
--   * If you specify one or more instance IDs, Amazon EC2 returns information
--   for those instances.
--
--   * If you do not specify instance IDs, Amazon EC2 returns information for all
--   relevant instances.
--
--   * If you specify an invalid instance ID, an error is returned.
--
--   * If you specify an instance that you do not own, it is not included in the
--   returned results.
--
--   * Recently terminated instances might appear in the returned
--   results. This interval is usually less than one hour.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstances.html>
data DescribeInstances = DescribeInstances
    { diInstanceId :: [Text]
      -- ^ One or more instance IDs.
    , diFilter     :: [Filter]
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeInstances

instance Rq DescribeInstances where
    type Er DescribeInstances = EC2ErrorResponse
    type Rs DescribeInstances = DescribeInstancesResponse
    request = query4 ec2 GET "DescribeInstances"

data DescribeInstancesResponse = DescribeInstancesResponse
    { dirRequestId      :: !Text
      -- ^ The ID of the request.
    , dirReservationSet :: [ReservationInfoType]
      -- ^ A list of reservations, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeInstancesResponse where
    xmlPickler = ec2XML

-- -- | Describes the status of one or more instances, including any scheduled
-- -- events.Instance status has two main components: Instance status provides
-- -- information about four types of scheduled events for an instance that may
-- -- require your attention: When your instance is retired, it will either be
-- -- terminated (if its root device type is the instance-store) or stopped (if
-- -- its root device type is an EBS volume). Instances stopped due to retirement
-- -- will not be restarted, but you can do so manually. You can also avoid
-- -- retirement of EBS-backed instances by manually restarting your instance
-- -- when its event code is instance-retirement. This ensures that your instance
-- -- is started on a different underlying host.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceStatus.html>

-- -- data InstanceStatusFilter
-- --     , disAvailability-zone   :: !Text
-- --       -- ^ The Availability Zone of the instance.
-- --     , disEvent               :: Members eventType
-- --       -- ^ The code identifying the type of event.
-- --     , disInstance-state-name :: !Text
-- --       -- ^ The state of the instance.
-- --     , disInstance-state-code :: !Integer
-- --       -- ^ A code representing the state of the instance. The high byte is
-- --       -- an opaque internal value and should be ignored. The low byte is
-- --       -- set based on the state represented
-- --     , disSystem-status       :: Members system-statusType
-- --       -- ^ The system status of the instance.
-- --     , disInstance-status     :: Members instance-statusType
-- --       -- ^ The status of the instance.

-- data DescribeInstanceStatus = DescribeInstanceStatus
--     { disInstanceId          :: Maybe Text
--       -- ^ One or more instance IDs.
--     , disIncludeAllInstances :: Maybe Bool
--       -- ^ When true, includes the health status for all instances. When
--       -- false, includes the health status for running instances only.
--     , disMaxResults          :: Maybe Integer
--       -- ^ The maximum number of paginated instance items per response.
--     , disNextToken           :: Maybe Text
--       -- ^ The next paginated set of results to return.
--     , disFilter              :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeInstanceStatus

-- instance AWSRequest EC2 DescribeInstanceStatus DescribeInstanceStatusResponse where
--     request = query4 ec2 GET "DescribeInstanceStatus"

-- data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
--     { disRequestId         :: !Text
--       -- ^ The ID of the request.
--     , disInstanceStatusSet :: !InstanceStatusItemType
--       -- ^ A list of instances status descriptions, each one wrapped in an
--       -- item element.
--     , ditNextToken         :: !Text
--       -- ^ The next paginated set of results to return.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeInstanceStatusResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your Internet gateways.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInternetGateways.html>

-- -- data GatewayFilter
-- --     , dihAttachment          :: Members attachmentType
-- --       -- ^ The current state of the attachment between the gateway and the
-- --       -- VPC. Returned only if a VPC is attached.
-- --     , dihInternet-gateway-id :: !Text
-- --       -- ^ The ID of the Internet gateway.
-- --     , dihTag-key             :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , dihTag-value           :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , dihTag:                :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , dihKey                 :: !Text
-- --       -- ^

-- data DescribeInternetGateways = DescribeInternetGateways
--     { dihInternetGatewayId   :: Members Text
--       -- ^ One or more Internet gateway IDs.
--     , dihFilter              :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeInternetGateways

-- instance AWSRequest EC2 DescribeInternetGateways DescribeInternetGatewaysResponse where
--     request = query4 ec2 GET "DescribeInternetGateways"

-- data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
--     { dihRequestId          :: !Text
--       -- ^ The ID of the request.
--     , dihInternetGatewaySet :: !InternetGatewayType
--       -- ^ A list of Internet gateways, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeInternetGatewaysResponse where
--     xmlPickler = ec2XML

-- -- data KeyPairFilter
-- --     , dkqFingerprint :: !Text
-- --       -- ^ The fingerprint of the key pair.
-- --     , dkqKey-name    :: !Text
-- --       -- ^ The name of the key pair.

-- | Describes one or more of your key pairs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeKeyPairs.html>
data DescribeKeyPairs = DescribeKeyPairs
    { dkqKeyName :: [Text]
      -- ^ One or more key pair names.
    , dkqFilter  :: [Filter]
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeKeyPairs

instance Rq DescribeKeyPairs where
    type Er DescribeKeyPairs = EC2ErrorResponse
    type Rs DescribeKeyPairs = DescribeKeyPairsResponse
    request = query4 ec2 GET "DescribeKeyPairs"

data DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { dkqRequestId :: !Text
      -- ^ The ID of the request.
    , dkqKeySet    :: [DescribeKeyPairsResponseItemType]
      -- ^ A list of key pairs.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeKeyPairsResponse where
    xmlPickler = ec2XML

-- -- | Describes one or more of your network ACLs.For more information about
-- -- network ACLs, see Network ACLs in the Amazon Virtual Private Cloud User
-- -- Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkAcls.html>

-- -- data NetworkAclFilter
-- --     , dnbAssociation    :: Members associationType
-- --       -- ^ The ID of an association ID for the ACL.
-- --     , dnbDefault        :: !Bool
-- --       -- ^ Indicates whether the ACL is the default network ACL for the VPC.
-- --     , dnbEntry          :: Members entryType
-- --       -- ^ The CIDR range specified in the entry.
-- --     , dnbNetwork-acl-id :: !Text
-- --       -- ^ The ID of the network ACL.
-- --     , dnbTag-key        :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , dnbTag-value      :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , dnbTag:           :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , dnbKey            :: !Text
-- --       -- ^ The ID of the VPC for the network ACL.
-- --     , dnbVpc-id         :: !Text
-- --       -- ^

-- data DescribeNetworkAcls = DescribeNetworkAcls
--     { dnbNetworkAclId   :: Members Text
--       -- ^ One or more network ACL IDs.
--     , dnbFilter         :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeNetworkAcls

-- instance AWSRequest EC2 DescribeNetworkAcls DescribeNetworkAclsResponse where
--     request = query4 ec2 GET "DescribeNetworkAcls"

-- data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse
--     { dnbRequestId     :: !Text
--       -- ^ The ID of the request.
--     , dnbNetworkAclSet :: !NetworkAclType
--       -- ^ A list of network ACLs, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeNetworkAclsResponse where
--     xmlPickler = ec2XML

-- -- | Describes a network interface attribute. You can specify only one attribute
-- -- at a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaceAttribute.html>

-- data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
--     { dniaNetworkInterfaceId :: !Text
--       -- ^ The ID of the network interface.
--     , dniaAttribute          :: !Text
--       -- ^ The attribute of the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeNetworkInterfaceAttribute

-- instance IsXML DescribeNetworkInterfaceAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DescribeNetworkInterfaceAttribute DescribeNetworkInterfaceAttributeResponse where
--     request = query4 ec2 GET "DescribeNetworkInterfaceAttribute"

-- data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
--     { dniaRequestId          :: !Text
--       -- ^ The ID of the request.
--     , dnibNetworkInterfaceId :: !Text
--       -- ^ The ID of the network interface.
--     , dnibDescription        :: !Text
--       -- ^ The description of the network interface.
--     , dnibSourceDestCheck    :: !Bool
--       -- ^ Indicates whether source/destination checking is enabled.
--     , dnibGroupSet           :: !GroupItemType
--       -- ^ The security groups associated with the network interface.
--     , dnibAttachment         :: !NetworkInterfaceAttachmentType
--       -- ^ The attachment (if any) of the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeNetworkInterfaceAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your network interfaces.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaces.html>

-- -- data NetworkInterfaceFilter
--     -- , dnjAddresses            :: Members addressesType
--     --   -- ^ The private IP addresses associated with the network interface.
--     -- , dnjAssociation          :: Members associationType
--     --   -- ^ The association ID returned when the network interface was
--     --   -- associated with an IP address.
--     -- , dnjAttachment           :: Members attachmentType
--     --   -- ^ The ID of the interface attachment.
--     -- , dnjAvailability-zone    :: !Text
--     --   -- ^ The Availability Zone of the network interface.
--     -- , dnjDescription          :: !Text
--     --   -- ^ The description of the network interface.
--     -- , dnjGroup-id             :: !Text
--     --   -- ^ The ID of a security group associated with the network interface.
--     -- , dnjGroup-name           :: !Text
--     --   -- ^ The name of a security group associated with the network
--     --   -- interface.
--     -- , dnjMac-address          :: !Text
--     --   -- ^ The MAC address of the network interface.
--     -- , dnjNetwork-interface-id :: !Text
--     --   -- ^ The ID of the network interface.
--     -- , dnjOwner-id             :: !Text
--     --   -- ^ The AWS account ID of the network interface owner.
--     -- , dnjPrivate-ip-address   :: !Text
--     --   -- ^ The private IP address or addresses of the network interface.
--     -- , dnjPrivate-dns-name     :: !Text
--     --   -- ^ The private DNS name of the network interface.
--     -- , dnjRequester-id         :: !Text
--     --   -- ^ The ID of the entity that launched the instance on your behalf
--     --   -- (for example, AWS Management Console, Auto Scaling, and so on).
--     -- , dnjRequester-managed    :: !Bool
--     --   -- ^ Indicates whether the network interface is being managed by an
--     --   -- AWS service (for example, AWS Management Console, Auto Scaling,
--     --   -- and so on).
--     -- , dnjSource-dest-check    :: !Bool
--     --   -- ^ Indicates whether the network interface performs
--     --   -- source/destination checking. A value of true means checking is
--     --   -- enabled, and false means checking is disabled. The value must be
--     --   -- false for the network interface to perform Network Address
--     --   -- Translation (NAT) in your VPC.
--     -- , dnjStatus               :: !Text
--     --   -- ^ The status of the network interface. If the network interface is
--     --   -- not attached to an instance, the status shows available; if a
--     --   -- network interface is attached to an instance the status shows
--     --   -- in-use.
--     -- , dnjSubnet-id            :: !Text
--     --   -- ^ The ID of the subnet for the network interface.
--     -- , dnjTag-key              :: !Text
--     --   -- ^ The key of a tag assigned to the resource. This filter is
--     --   -- independent of the tag-value filter. For example, if you use both
--     --   -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--     --   -- get any resources assigned both the tag key Purpose (regardless
--     --   -- of what the tag's value is), and the tag value X (regardless of
--     --   -- what the tag's key is). If you want to list only resources where
--     --   -- Purpose is X, see the tag:key filter.
--     -- , dnjTag-value            :: !Text
--     --   -- ^ The value of a tag assigned to the resource. This filter is
--     --   -- independent of the tag-key filter.
--     -- , dnjTag:                 :: !Text
--     --   -- ^ Filters the response based on a specific tag/value combination.
--     -- , dnjKey                  :: !Text
--     --   -- ^ The ID of the VPC for the network interface.
--     -- , dnjVpc-id               :: !Text
--     --   -- ^

-- data DescribeNetworkInterfaces = DescribeNetworkInterfaces
--     { dnjNetworkInterfaceId   :: Members Text
--       -- ^ One or more network interface IDs.
--     , dnjFilter               :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeNetworkInterfaces

-- instance AWSRequest EC2 DescribeNetworkInterfaces DescribeNetworkInterfacesResponse where
--     request = query4 ec2 GET "DescribeNetworkInterfaces"

-- data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
--     { dnjRequestId           :: !Text
--       -- ^ The ID of the request.
--     , dnjNetworkInterfaceSet :: !NetworkInterfaceType
--       -- ^ Information about the network interfaces, each one wrapped in an
--       -- item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeNetworkInterfacesResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your placement groups. For more information about
-- -- placement groups and cluster instances, see Cluster Instances in the Amazon
-- -- Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePlacementGroups.html>

-- -- data PlacementGroupFilter
-- --     , dphGroup-name :: !Text
-- --       -- ^ The name of the placement group.
-- --     , dphState      :: !Text
-- --       -- ^ The state of the placement group.
-- --     , dphStrategy   :: !Text
-- --       -- ^ The strategy of the placement group.


-- data DescribePlacementGroups = DescribePlacementGroups
--     { dphGroupName  :: Members Text
--       -- ^ One or more placement group names.
--     , dphFilter     :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribePlacementGroups

-- instance AWSRequest EC2 DescribePlacementGroups DescribePlacementGroupsResponse where
--     request = query4 ec2 GET "DescribePlacementGroups"

-- data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
--     { dphRequestId         :: !Text
--       -- ^ The ID of the request.
--     , dphPlacementGroupSet :: !PlacementGroupInfoType
--       -- ^ A list of placement groups, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribePlacementGroupsResponse where
--     xmlPickler = ec2XML

-- -- data RegionFilter
-- --     , drEndpoint    :: !Text
-- --       -- ^ The endpoint of the region (for example,
-- --       -- ec2.us-east-1.amazonaws.com).
-- --     , drRegion-name :: !Text
-- --       -- ^ The name of the region.

-- | Describes one or more regions that are currently available to you.For a
-- list of the regions supported by Amazon EC2, see Regions and Endpoints.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html>
data DescribeRegions = DescribeRegions
    { drRegionName  :: [Region]
      -- ^ One or more region names.
    , drFilter      :: [Filter]
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeRegions

instance Rq DescribeRegions where
    type Er DescribeRegions = EC2ErrorResponse
    type Rs DescribeRegions = DescribeRegionsResponse
    request = query4 ec2 GET "DescribeRegions"

data DescribeRegionsResponse = DescribeRegionsResponse
    { drrRequestId  :: !Text
      -- ^ The ID of the request.
    , drrRegionInfo :: [RegionItemType]
      -- ^ A list of regions, each one wrapped in an item element.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeRegionsResponse where
    xmlPickler = ec2XML

-- -- | Describes one or more of the Reserved Instances that you purchased.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstances.html>

-- -- data ReservedInstanceFilter
-- --     , driAvailability-zone     :: !Text
-- --       -- ^ The Availability Zone where the Reserved Instance can be used.
-- --     , driDuration              :: !Integer
-- --       -- ^ The duration of the Reserved Instance (one year or three years),
-- --       -- in seconds.
-- --     , driFixed-price           :: !Double
-- --       -- ^ The purchase price of the Reserved Instance (for example, 9800.0)
-- --     , driInstance-type         :: !Text
-- --       -- ^ The instance type on which the Reserved Instance can be used.
-- --     , driProduct-description   :: !Text
-- --       -- ^ The product description of the Reserved Instance.
-- --     , driReserved-instances-id :: !Text
-- --       -- ^ The ID of the Reserved Instance.
-- --     , driStart                 :: !UTCTime
-- --       -- ^ The time at which the Reserved Instance purchase request was
-- --       -- placed (for example, 2010-08-07T11:54:42.000Z).
-- --     , driState                 :: !Text
-- --       -- ^ The state of the Reserved Instance.
-- --     , driTag-key               :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , driTag-value             :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , driTag:                  :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , driKey                   :: !Double
-- --       -- ^ The usage price of the Reserved Instance, per hour (for example,
-- --       -- 0.84)
-- --     , driUsage-price           :: !Text
-- --       -- ^

-- data DescribeReservedInstances = DescribeReservedInstances
--     { driReservedInstancesId   :: Members Text
--       -- ^ One or more Reserved Instance IDs.
--     , driOfferingType          :: Maybe Text
--       -- ^ The Reserved Instance offering type.
--     , driFilter                :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstances

-- instance AWSRequest EC2 DescribeReservedInstances DescribeReservedInstancesResponse where
--     request = query4 ec2 GET "DescribeReservedInstances"

-- data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
--     { driRequestId            :: !Text
--       -- ^ The ID of the request.
--     , driReservedInstancesSet :: !DescribeReservedInstancesResponseSetItemType
--       -- ^ A list of Reserved Instances, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeReservedInstancesResponse where
--     xmlPickler = ec2XML

-- -- | Describes your account's Reserved Instance listings in the Reserved
-- -- Instance Marketplace. This call returns information, such as the ID of the
-- -- Reserved Instance to which a listing is associated.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesListings.html>

-- -- data ReservedInstanceFilter
-- --     , drilStatus                        :: !Text
-- --       -- ^ Status of the Reserved Instance listing.
-- --     , drilStatus-message                :: !Text
-- --       -- ^ Reason for the status.
-- --     , drilReserved-instances-listing-id :: !Text
-- --       -- ^ The ID of the Reserved Instances listing.
-- --     , drilReserved-instances-id         :: !Text
-- --       -- ^ The ID of the Reserved Instances.

-- data DescribeReservedInstancesListings = DescribeReservedInstancesListings
--     { drilReservedInstancesListingId :: Members DescribeReservedInstancesListingSetItemType
--       -- ^ The information about the Reserved Instance listing wrapped in an
--       -- item element.
--     , drilReservedInstancesId        :: Members DescribeReservedInstancesSetItemType
--       -- ^ The set of Reserved Instances IDs which are used to see
--       -- associated listings.
--     , drilFilter                     :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstancesListings

-- instance AWSRequest EC2 DescribeReservedInstancesListings DescribeReservedInstancesListingsResponse where
--     request = query4 ec2 GET "DescribeReservedInstancesListings"

-- data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
--     { drilRequestId                    :: !Text
--       -- ^ The ID of the request.
--     , drilReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSetItemType
--       -- ^ The Reserved Instance listing information wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeReservedInstancesListingsResponse where
--     xmlPickler = ec2XML

-- -- | Describes Reserved Instance offerings that are available for purchase.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesOfferings.html>

-- data ReservedInstancesOfferingFilter
--     -- , drioInstanceTenancy                :: Maybe Text
--     --   -- ^ The tenancy of the Reserved Instance offering. A Reserved
--     --   -- Instance with tenancy of dedicated will run on single-tenant
--     --   -- hardware and can only be launched within a VPC.
--     -- , drioOfferingType                   :: Maybe Text
--     --   -- ^ The Reserved Instance offering type.
--     -- , drioIncludeMarketplace             :: Maybe Bool
--     --   -- ^ Include Marketplace offerings in the response.
--     -- , drioMinDuration                    :: Maybe Integer
--     --   -- ^ Minimum duration (in seconds) to filter when searching for
--     --   -- offerings.
--     -- , drioMaxDuration                    :: Maybe Integer
--     --   -- ^ Maximum duration (in seconds) to filter when searching for
--     --   -- offerings.
--     -- , drioMaxInstanceCount               :: Maybe Integer
--     --   -- ^ Maximum number of instances to filter when searching for
--     --   -- offerings.
--     -- , drioNextToken                      :: Maybe Text
--     --   -- ^ Token to use when requesting the next paginated set of offerings.
--     -- , drioMaxResults                     :: Maybe Integer
--     --   -- ^ Maximum number of offerings to return.
--     -- , drioAvailability-zone              :: !Text
--     --   -- ^ The Availability Zone where the Reserved Instance can be used.
--     -- , drioDuration                       :: !Integer
--     --   -- ^ The duration of the Reserved Instance (for example, one year or
--     --   -- three years), in seconds.
--     -- , drioFixed-price                    :: !Double
--     --   -- ^ The purchase price of the Reserved Instance (for example, 9800.0)
--     -- , drioInstance-type                  :: !Text
--     --   -- ^ The Amazon EC2 instance type on which the Reserved Instance can
--     --   -- be used.
--     -- , drioMarketplace                    :: !Bool
--     --   -- ^ Set to true to show only Reserved Instance Marketplace offerings.
--     --   -- When this filter is not used, which is the default behavior, all
--     --   -- offerings from AWS and Reserved Instance Marketplace are listed.
--     -- , drioProduct-description            :: !Text
--     --   -- ^ The description of the Reserved Instance.
--     -- , drioReserved-instances-offering-id :: !Text
--     --   -- ^ The Reserved Instances offering ID.
--     -- , drioUsage-price                    :: !Double
--     --   -- ^ The usage price of the Reserved Instance, per hour (for example,
--     --   -- 0.84)

-- data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
--     { drioReservedInstancesOfferingId    :: Members Text
--       -- ^ One or more Reserved Instances offering IDs.
--     , drioInstanceType                   :: Maybe Text
--       -- ^ The Amazon EC2 instance type on which the Reserved Instance can
--       -- be used. See Available Instance Types for more information.
--     , drioAvailabilityZone               :: Maybe Text
--       -- ^ The Availability Zone in which the Reserved Instance can be used.
--     , drioProductDescription             :: Maybe Text
--       -- ^ The Reserved Instance description. Instances that include (Amazon
--       -- VPC) in the description are for use with Amazon VPC.
--     , drioFilter                         :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstancesOfferings

-- instance AWSRequest EC2 DescribeReservedInstancesOfferings DescribeReservedInstancesOfferingsResponse where
--     request = query4 ec2 GET "DescribeReservedInstancesOfferings"

-- data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
--     { drioRequestId                     :: !Text
--       -- ^ The ID of the request.
--     , drioReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSetItemType
--       -- ^ A list of Reserved Instances offerings. Each offering's
--       -- information is wrapped in an item element.
--     , dripNextToken                     :: !Text
--       -- ^ The next paginated set of results to return.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeReservedInstancesOfferingsResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your route tables.For more information about route
-- -- tables, see Route Tables in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRouteTables.html>

-- -- data RouteTableFilter
-- --     , druAssociation    :: Members associationType
-- --       -- ^ The ID of an association ID for the route table.
-- --     , druRoute-table-id :: !Text
-- --       -- ^ The ID of the route table.
-- --     , druRoute          :: Members routeType
-- --       -- ^ The CIDR range specified in a route in the table.
-- --     , druTag-key        :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , druTag-value      :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , druTag:           :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , druKey            :: !Text
-- --       -- ^ The ID of the VPC for the route table.
-- --     , druVpc-id         :: !Text
-- --       -- ^

-- data DescribeRouteTables = DescribeRouteTables
--     { druRouteTableId :: Members Text
--       -- ^ One or more route table IDs.
--     , druFilter       :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeRouteTables

-- instance AWSRequest EC2 DescribeRouteTables DescribeRouteTablesResponse where
--     request = query4 ec2 GET "DescribeRouteTables"

-- data DescribeRouteTablesResponse = DescribeRouteTablesResponse
--     { druRequestId     :: !Text
--       -- ^ The ID of the request.
--     , druRouteTableSet :: !RouteTableType
--       -- ^ A list of route tables, each one wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeRouteTablesResponse where
--     xmlPickler = ec2XML

-- | A security group is for use with instances either in the EC2-Classic
-- platform or in a specific VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSecurityGroups.html>
data DescribeSecurityGroups = DescribeSecurityGroups
    { dshGroupName :: [Text]
      -- ^ A list of group names.
    , dshGroupId   :: [Text]
      -- ^ A list of group ids.
    , dshFilter    :: [Filter]
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeSecurityGroups

instance Rq DescribeSecurityGroups where
    type Er DescribeSecurityGroups = EC2ErrorResponse
    type Rs DescribeSecurityGroups = DescribeSecurityGroupsResponse
    request = query4 ec2 GET "DescribeSecurityGroups"

data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { dshrRequestId         :: !Text
      -- ^ The ID of the request.
    , dshrSecurityGroupInfo :: [SecurityGroupItemType]
      -- ^ A list of security groups.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeSecurityGroupsResponse where
    xmlPickler = ec2XML

-- -- | Describes an attribute of the specified snapshot. You can specify only one
-- -- attribute at a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshotAttribute.html>

-- data DescribeSnapshotAttribute = DescribeSnapshotAttribute
--     { dsaSnapshotId :: !Text
--       -- ^ The ID of the Amazon EBS snapshot.
--     , dsaAttribute  :: !Text
--       -- ^ The snapshot attribute.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeSnapshotAttribute

-- instance IsXML DescribeSnapshotAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DescribeSnapshotAttribute DescribeSnapshotAttributeResponse where
--     request = query4 ec2 GET "DescribeSnapshotAttribute"

-- data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
--     { dsaRequestId              :: !Text
--       -- ^ The ID of the request.
--     , dsbSnapshotId             :: !Text
--       -- ^ The ID of the Amazon EBS snapshot.
--     , dsbCreateVolumePermission :: !CreateVolumePermissionItemType
--       -- ^ A list of permissions for creating volumes from the snapshot.
--       -- Each permission is wrapped in an item element.
--     , dsbProductCodes           :: !ProductCodesSetItemType
--       -- ^ A list of product codes. Each product code is wrapped in an item
--       -- element type that contains a product code and a type.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeSnapshotAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of the Amazon EBS snapshots available to you.
-- -- Snapshots available to you include public snapshots available for any AWS
-- -- account to launch, private snapshots you own, and private snapshots owned
-- -- by another AWS account but for which you've been given explicit create
-- -- volume permissions.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html>

-- -- data SnapshotFilter
-- --     , dtOwner-alias  :: !Text
-- --       -- ^ The AWS account alias (for example, amazon) that owns the
-- --       -- snapshot.
-- --     , dtOwner-id     :: !Text
-- --       -- ^ The ID of the AWS account that owns the snapshot.
-- --     , dtProgress     :: !Text
-- --       -- ^ The progress of the snapshot, as a percentage (for example, 80%).
-- --     , dtSnapshot-id  :: !Text
-- --       -- ^ The snapshot ID.
-- --     , dtStart-time   :: !UTCTime
-- --       -- ^ The time stamp when the snapshot was initiated.
-- --     , dtStatus       :: !Text
-- --       -- ^ The status of the snapshot.
-- --     , dtTag-key      :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , dtTag-value    :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , dtTag:         :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , dtKey          :: !Text
-- --       -- ^ The ID of the volume the snapshot is for.
-- --     , dtVolume-id    :: !Text
-- --       -- ^ The size of the volume, in GiB (for example, 20).
-- --     , dtVolume-size  :: !Text
-- --       -- ^


-- data DescribeSnapshots = DescribeSnapshots
--     { dtSnapshotId   :: Members Text
--       -- ^ One or more snapshot IDs.
--     , dtOwner        :: Members Text
--       -- ^ Returns the snapshots owned by the specified owner. Multiple
--       -- owners can be specified.
--     , dtRestorableBy :: Members Text
--       -- ^ One or more AWS accounts IDs that can create volumes from the
--       -- snapshot.
--     , dtFilter       :: Members Text
--       -- ^ The name of a filter.
--     , dtDescription  :: !Text
--       -- ^ A description of the snapshot.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeSnapshots

-- instance AWSRequest EC2 DescribeSnapshots DescribeSnapshotsResponse where
--     request = query4 ec2 GET "DescribeSnapshots"

-- data DescribeSnapshotsResponse = DescribeSnapshotsResponse
--     { dyRequestId   :: !Text
--       -- ^ The ID of the request.
--     , dySnapshotSet :: !DescribeSnapshotsSetItemResponseType
--       -- ^ A list of snapshots. Each snapshot is wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeSnapshotsResponse where
--     xmlPickler = ec2XML

-- -- | Describes the datafeed for Spot Instances. For more information about Spot
-- -- Instances, see Spot Instances in the Amazon Elastic Compute Cloud User
-- -- Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotDatafeedSubscription.html>

-- data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
--     deriving (Eq, Read, Show, Generic)

-- instance IsQuery DescribeSpotDatafeedSubscription

-- instance IsXML DescribeSpotDatafeedSubscription where
--     xmlPickler = xpEmpty $ Just ec2NS

-- instance AWSRequest EC2 DescribeSpotDatafeedSubscription DescribeSpotDatafeedSubscriptionResponse where
--     request = query4 ec2 GET "DescribeSpotDatafeedSubscription"

-- data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
--     { dsdtRequestId                :: !Text
--       -- ^ The ID of the request.
--     , dsdtSpotDatafeedSubscription :: !SpotDatafeedSubscriptionType
--       -- ^ The Spot Instance datafeed subscription.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeSpotDatafeedSubscriptionResponse where
--     xmlPickler = ec2XML

-- -- | Describes the Spot Instance requests that belong to your account. Spot
-- -- Instances are instances that Amazon EC2 starts on your behalf when the
-- -- maximum price that you specify exceeds the current Spot Price. Amazon EC2
-- -- periodically sets the Spot Price based on available Spot Instance capacity
-- -- and current Spot Instance requests.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotInstanceRequests.html>

-- -- data SpotInstanceFilter
-- --     , dsirAvailability-zone-group    :: !Text
-- --       -- ^ The Availability Zone group. If you specify the same Availability
-- --       -- Zone group for all Spot Instance requests, all Spot Instances are
-- --       -- launched in the same Availability Zone.
-- --     , dsirCreate-time                :: !Text
-- --       -- ^ The time stamp when the Spot Instance request was created.
-- --     , dsirFault-code                 :: !Text
-- --       -- ^ The fault code related to the request.
-- --     , dsirFault-message              :: !Text
-- --       -- ^ The fault message related to the request.
-- --     , dsirInstance-id                :: !Text
-- --       -- ^ The ID of the instance that fulfilled the request.
-- --     , dsirLaunch-group               :: !Text
-- --       -- ^ The Spot Instance launch group. Launch groups are Spot Instances
-- --       -- that launch together and terminate together.
-- --     , dsirLaunch                     :: Members launchType
-- --       -- ^ Whether the Amazon EBS volume is deleted on instance termination.
-- --     , dsirProduct-description        :: !Text
-- --       -- ^ The product description associated with the instance.
-- --     , dsirSpot-instance-request-id   :: !Text
-- --       -- ^ The Spot Instance request ID.
-- --     , dsirSpot-price                 :: !Text
-- --       -- ^ The maximum hourly price for any Spot Instance launched to
-- --       -- fulfill the request.
-- --     , dsirState                      :: !Text
-- --       -- ^ The state of the Spot Instance request. Spot bid status
-- --       -- information can help you track your Amazon EC2 Spot Instance
-- --       -- requests. For information, see Tracking Spot Requests with Bid
-- --       -- Status Codes in the Amazon Elastic Compute Cloud User Guide.
-- --     , dsirStatus-code                :: !Text
-- --       -- ^ The short code describing the most recent evaluation of your Spot
-- --       -- Instance request.
-- --     , dsirStatus-message             :: !Text
-- --       -- ^ The message explaining the status of the Spot Instance request.
-- --     , dsirTag-key                    :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , dsirTag-value                  :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , dsirTag:                       :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , dsirKey                        :: !Text
-- --       -- ^ The type of Spot Instance request.
-- --     , dsirType                       :: !Text
-- --       -- ^ The Availability Zone in which the bid is launched.
-- --     , dsirLaunched-availability-zone :: !UTCTime
-- --       -- ^ The start date of the request.
-- --     , dsirValid-from                 :: !UTCTime
-- --       -- ^ The end date of the request.
-- --     , dsirValid-until                :: !Text
-- --       -- ^

-- data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests
--     { dsirSpotInstanceRequestId      :: Members Text
--       -- ^ One or more Spot Instance request IDs.
--     , dsirFilter                     :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeSpotInstanceRequests

-- instance AWSRequest EC2 DescribeSpotInstanceRequests DescribeSpotInstanceRequestsResponse where
--     request = query4 ec2 GET "DescribeSpotInstanceRequests"

-- data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse
--     { dsirRequestId              :: !Text
--       -- ^ The ID of the request.
--     , dsirSpotInstanceRequestSet :: !SpotInstanceRequestSetItemType
--       -- ^ A list of Spot Instance requests. Each request is wrapped in an
--       -- item element.
--     , dsirNetworkInterfaceSet    :: !InstanceNetworkInterfaceSetItemRequestType
--       -- ^ Information about the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeSpotInstanceRequestsResponse where
--     xmlPickler = ec2XML

-- -- | Describes the Spot Price history.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotPriceHistory.html>

-- -- data SpotPriceFilter
-- --     , dsphInstance-type       :: !Text
-- --       -- ^ The type of instance (for example, m1.small).
-- --     , dsphProduct-description :: !Text
-- --       -- ^ The product description for the Spot Price.
-- --     , dsphSpot-price          :: !Text
-- --       -- ^ The Spot Price. The value must match exactly (or use wildcards;
-- --       -- greater than or less than comparison is not supported).
--     -- , dsphAvailability-zone   :: !Text
--     --   -- ^ The Availability Zone for which prices should be returned.
--     -- , dsphTimestamp           :: !UTCTime
--     --   -- ^ The timestamp of the Spot Price history (for example,
--     --   -- 2010-08-16T05:06:11.000Z). You can use wildcards (* and ?).
--     --   -- Greater than or less than comparison is not supported.

-- data DescribeSpotPriceHistory = DescribeSpotPriceHistory
--     { dsphStartTime           :: Maybe UTCTime
--       -- ^ The start date and time of the Spot Instance price history data.
--     , dsphEndTime             :: Maybe UTCTime
--       -- ^ The end date and time of the Spot Instance price history data.
--     , dsphInstanceType        :: Members Text
--       -- ^ The instance type to return.
--     , dsphProductDescription  :: Members Text
--       -- ^ Filters the results by basic product description.
--     , dsphFilter              :: Members Text
--       -- ^ The name of a filter.
--     , dsphAvailabilityZone    :: Maybe Text
--       -- ^ Filters the results by availability zone.
--     , dsphMaxResults          :: Maybe Integer
--       -- ^ The number of rows to return.
--     , dsphNextToken           :: Maybe Text
--       -- ^ The next set of rows to return.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeSpotPriceHistory

-- instance AWSRequest EC2 DescribeSpotPriceHistory DescribeSpotPriceHistoryResponse where
--     request = query4 ec2 GET "DescribeSpotPriceHistory"

-- data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
--     { dsphRequestId           :: !Text
--       -- ^ The ID of the request.
--     , dsphSpotPriceHistorySet :: !SpotPriceHistorySetItemType
--       -- ^ A list of historical Spot Prices. Each price is wrapped in an
--       -- item element.
--     , dspiNextToken           :: !Text
--       -- ^ The string marking the next set of results returned. Displays
--       -- empty if there are no more results to be returned.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeSpotPriceHistoryResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your subnets.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSubnets.html>

-- -- data SubnetFilter
-- --     , duAvailability-zone          :: !Text
-- --       -- ^ The Availability Zone for the subnet.
-- --     , duAvailable-ip-address-count :: !Text
-- --       -- ^ The number of IP addresses in the subnet that are available.
-- --     , duCidr                       :: !Text
-- --       -- ^ The CIDR block of the subnet. The CIDR block you specify must
-- --       -- exactly match the subnet's CIDR block for information to be
-- --       -- returned for the subnet.
-- --     , duDefaultForAz               :: !Bool
-- --       -- ^ Indicates whether this is the default subnet for the Availability
-- --       -- Zone.
-- --     , duState                      :: !Text
-- --       -- ^ The state of the subnet.
-- --     , duSubnet-id                  :: !Text
-- --       -- ^ The ID of the subnet.
-- --     , duTag-key                    :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , duTag-value                  :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , duTag:                       :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , duKey                        :: !Text
-- --       -- ^ The ID of the VPC for the subnet.
-- --     , duVpc-id                     :: !Text
-- --       -- ^

-- data DescribeSubnets = DescribeSubnets
--     { dtSubnetId                   :: Members Text
--       -- ^ One or more subnet IDs.
--     , duFilter                     :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeSubnets

-- instance AWSRequest EC2 DescribeSubnets DescribeSubnetsResponse where
--     request = query4 ec2 GET "DescribeSubnets"

-- data DescribeSubnetsResponse = DescribeSubnetsResponse
--     { dzRequestId :: !Text
--       -- ^ The ID of the request.
--     , dzSubnetSet :: !SubnetType
--       -- ^ A list of subnets. Each subnet is wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeSubnetsResponse where
--     xmlPickler = ec2XML

-- | Describes one or more of the tags for your EC2 resources.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html>

data DescribeTags = DescribeTags
    { dtagsFilter :: [TagFilter]
      -- ^ The name of a filter.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeTags

instance Rq DescribeTags where
    type Er DescribeTags = EC2ErrorResponse
    type Rs DescribeTags = DescribeTagsResponse
    request = query4 ec2 GET "DescribeTags"

data DescribeTagsResponse = DescribeTagsResponse
    { dtagsrRequestId :: !Text
      -- ^ The ID of the request.
    , dtagsrTagSet    :: [TagSetItemType]
      -- ^ A list of tags.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeTagsResponse where
    xmlPickler = ec2XML

-- -- | Describes the specified attribute of the specified volume. You can specify
-- -- only one attribute at a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeAttribute.html>

-- data DescribeVolumeAttribute = DescribeVolumeAttribute
--     { dvaVolumeId  :: !Text
--       -- ^ The ID of the volume.
--     , dvaAttribute :: !Text
--       -- ^ The instance attribute.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeVolumeAttribute

-- instance IsXML DescribeVolumeAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DescribeVolumeAttribute DescribeVolumeAttributeResponse where
--     request = query4 ec2 GET "DescribeVolumeAttribute"

-- data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
--     { dvaRequestId    :: !Text
--       -- ^ The ID of the request.
--     , dvbVolumeId     :: !Text
--       -- ^ The ID of the volume.
--     , dvbAutoEnableIO :: Maybe Bool
--       -- ^ The state of autoEnableIO attribute.
--     , dvbProductCodes :: !ProductCodesSetItemType
--       -- ^ A list of product codes. Each product code is wrapped in an item
--       -- element that contains a product code and a type.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeVolumeAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Describes the specified Amazon EBS volumes.For more information about
-- -- Amazon EBS, see Amazon Elastic Block Store in the Amazon Elastic Compute
-- -- Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html>

-- -- data VolumeFilter
--     -- , dwAttachment        :: Members AttachmentType
--     --   -- ^ The time stamp when the attachment initiated.
--     -- , dwAvailability-zone :: !Text
--     --   -- ^ The Availability Zone in which the volume was created.
--     -- , dwCreate-time       :: !UTCTime
--     --   -- ^ The time stamp when the volume was created.
--     -- , dwSize              :: !Text
--     --   -- ^ The size of the volume, in GiB (for example, 20).
--     -- , dwSnapshot-id       :: !Text
--     --   -- ^ The snapshot from which the volume was created.
--     -- , dwStatus            :: !Text
--     --   -- ^ The status of the volume.
--     -- , dwTag-key           :: !Text
--     --   -- ^ The key of a tag assigned to the resource. This filter is
--     --   -- independent of the tag-value filter. For example, if you use both
--     --   -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--     --   -- get any resources assigned both the tag key Purpose (regardless
--     --   -- of what the tag's value is), and the tag value X (regardless of
--     --   -- what the tag's key is). If you want to list only resources where
--     --   -- Purpose is X, see the tag:key filter.
--     -- , dwTag-value         :: !Text
--     --   -- ^ The value of a tag assigned to the resource. This filter is
--     --   -- independent of the tag-key filter.
--     -- , dwTag:              :: !Text
--     --   -- ^ Filters the response based on a specific tag/value combination.
--     -- , dwKey               :: !Text
--     --   -- ^ The volume ID.
--     -- , dwVolume-id         :: !Text
--     --   -- ^ The Amazon EBS volume type. If the volume is an io1 volume, the
--     --   -- response includes the IOPS as well.
--     -- , dwVolume-type       :: !Text
--     --   -- ^

-- data DescribeVolumes = DescribeVolumes
--     { dwVolumeId :: Members Text
--       -- ^ One or more volume IDs.
--     , dwFilter   :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeVolumes

-- instance AWSRequest EC2 DescribeVolumes DescribeVolumesResponse where
--     request = query4 ec2 GET "DescribeVolumes"

-- data DescribeVolumesResponse = DescribeVolumesResponse
--     { ebRequestId :: !Text
--       -- ^ The ID of the request.
--     , ebVolumeSet :: !DescribeVolumesSetItemResponseType
--       -- ^ A list of volumes. Each volume is wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeVolumesResponse where
--     xmlPickler = ec2XML

-- -- | Describes the status of the specified volumes. Volume status provides the
-- -- result of the checks performed on your volumes to determine events that can
-- -- impair the performance of your volumes.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeStatus.html>

-- -- data VolumeFilter
-- --     , dvsAvailability-zone :: !Text
-- --       -- ^ The Availability Zone of the instance.
-- --     , dvsVolume-status     :: Members volume-statusType
-- --       -- ^ The status of the volume.
-- --     , dvsEvent             :: Members eventType
-- --       -- ^ A description of the event.
-- --     , dvsAction            :: Members actionType

-- data DescribeVolumeStatus = DescribeVolumeStatus
--     { dvsVolumeId   :: Members Text
--       -- ^ One or more volume IDs.
--     , dvsFilter     :: Members Text
--       -- ^ The name of a filter.
--     , dvsMaxResults :: Maybe Integer
--       -- ^ The maximum number of paginated volume items per response.
--     , dvsNextToken  :: Maybe Text
--       -- ^ A string specifying the next paginated set of results to return
--       -- using the pagination token returned by a previous call to this
--       -- API.
--       -- ^ The action code for the event, for example, enable-volume-io
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeVolumeStatus

-- instance AWSRequest EC2 DescribeVolumeStatus DescribeVolumeStatusResponse where
--     request = query4 ec2 GET "DescribeVolumeStatus"

-- data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
--     { dvsRequestId       :: !Text
--       -- ^ The ID of the request.
--     , dvsVolumeStatusSet :: !VolumeStatusItemType
--       -- ^ A list of volumes. Each volume is wrapped in an item element.
--     , dvtNextToken       :: !Text
--       -- ^ A string specifying the next paginated set of results to return.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeVolumeStatusResponse where
--     xmlPickler = ec2XML

-- -- | Describes the specified attribute of the specified VPC. You can specify
-- -- only one attribute at a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcAttribute.html>

-- data DescribeVpcAttribute = DescribeVpcAttribute
--     { dvaVpcId     :: !Text
--       -- ^ The ID of the VPC.
--     , dvbAttribute :: !Text
--       -- ^ The VPC attribute.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeVpcAttribute

-- instance IsXML DescribeVpcAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DescribeVpcAttribute DescribeVpcAttributeResponse where
--     request = query4 ec2 GET "DescribeVpcAttribute"

-- data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
--     { dvbRequestId          :: !Text
--       -- ^ The ID of the request.
--     , dvbEnableDnsSupport   :: !Bool
--       -- ^ Indicates whether DNS resolution is enabled for the VPC. If this
--       -- attribute is true, the Amazon DNS server resolves DNS hostnames
--       -- for your instances to their corresponding IP addresses;
--       -- otherwise, it does not.
--     , dvbEnableDnsHostnames :: !Bool
--       -- ^ Indicates whether the instances launched in the VPC get DNS
--       -- hostnames. If this attribute is true, instances in the VPC get
--       -- DNS hostnames; otherwise, they do not.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeVpcAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your VPCs.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcs.html>

-- -- data VpcFilter
-- --     , dxCidr            :: !Text
-- --       -- ^ The CIDR block of the VPC. The CIDR block you specify must
-- --       -- exactly match the VPC's CIDR block for information to be returned
-- --       -- for the VPC.
-- --     , dxDhcp-options-id :: !Text
-- --       -- ^ The ID of a set of DHCP options.
-- --     , dxIsDefault       :: !Bool
-- --       -- ^ Indicates whether the VPC is the default VPC.
-- --     , dxState           :: !Text
-- --       -- ^ The state of the VPC.
-- --     , dxTag-key         :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , dxTag-value       :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , dxTag:            :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , dxKey             :: !Text
-- --       -- ^ The ID of the VPC.
-- --     , dxVpc-id          :: !Text
-- --       -- ^

-- data DescribeVpcs = DescribeVpcs
--     { dwVpcId  :: Members Text
--       -- ^ One or more VPC IDs.
--     , dxFilter :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeVpcs

-- instance AWSRequest EC2 DescribeVpcs DescribeVpcsResponse where
--     request = query4 ec2 GET "DescribeVpcs"

-- data DescribeVpcsResponse = DescribeVpcsResponse
--     { ecRequestId :: !Text
--       -- ^ The ID of the request.
--     , ecVpcSet    :: !VpcType
--       -- ^ A list of VPCs. Each VPC is wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeVpcsResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your VPN connections.For more information about
-- -- VPN connections, see Adding a Hardware Virtual Private Gateway to Your VPC
-- -- in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpnConnections.html>

-- -- data VpcConnectionFilter
--     -- , dvdCustomer-gateway-configuration :: !Text
--     --   -- ^ The configuration information for the customer gateway.
--     -- , dvdCustomer-gateway-id            :: !Text
--     --   -- ^ The ID of a customer gateway associated with the VPN connection.
--     -- , dvdState                          :: !Text
--     --   -- ^ The state of the VPN connection.
--     -- , dvdOption                         :: Members optionType
--     --   -- ^ Indicates whether the connection has static routes only. Used for
--     --   -- devices that do not support Border Gateway Protocol (BGP).
--     -- , dvdRoute                          :: Members routeType
--     --   -- ^ The destination CIDR block. This corresponds to the subnet used
--     --   -- in a customer data center.
--     -- , dvdBgp-asn                        :: !Integer
--     --   -- ^ The BGP Autonomous System Number (ASN) associated with a BGP
--     --   -- device.
--     -- , dvdTag-key                        :: !Text
--     --   -- ^ The key of a tag assigned to the resource. This filter is
--     --   -- independent of the tag-value filter. For example, if you use both
--     --   -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
--     --   -- get any resources assigned both the tag key Purpose (regardless
--     --   -- of what the tag's value is), and the tag value X (regardless of
--     --   -- what the tag's key is). If you want to list only resources where
--     --   -- Purpose is X, see the tag:key filter.
--     -- , dvdTag-value                      :: !Text
--     --   -- ^ The value of a tag assigned to the resource. This filter is
--     --   -- independent of the tag-key filter.
--     -- , dvdTag:                           :: !Text
--     --   -- ^ Filters the response based on a specific tag/value combination.
--     -- , dvdKey                            :: !Text
--     --   -- ^ The type of VPN connection. Currently the only supported type is
--     --   -- ipsec.1.
--     -- , dvdType                           :: !Text
--     --   -- ^ The ID of the VPN connection.
--     -- , dvdVpn-connection-id              :: !Text
--     --   -- ^ The ID of a virtual private gateway associated with the VPN
--     --   -- connection.
--     -- , dvdVpn-gateway-id                 :: !Text
--     --   -- ^

-- data DescribeVpnConnections = DescribeVpnConnections
--     { dvdVpnConnectionId :: Members Text
--       -- ^ One or more VPN connection IDs.
--     , dvdFilter          :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeVpnConnections

-- instance AWSRequest EC2 DescribeVpnConnections DescribeVpnConnectionsResponse where
--     request = query4 ec2 GET "DescribeVpnConnections"

-- data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
--     { dvdRequestId        :: !Text
--       -- ^ The ID of the request.
--     , dvdVpnConnectionSet :: !VpnConnectionType
--       -- ^ A list of VPN connections. Each VPN connection is wrapped in an
--       -- item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeVpnConnectionsResponse where
--     xmlPickler = ec2XML

-- -- | Describes one or more of your virtual private gateways. For more
-- -- information about virtual private gateways, see Adding an IPsec Hardware
-- -- VPN to Your VPC in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpnGateways.html>

-- -- data VpnGatewayFilter
-- --     , dvhAttachment        :: Members AttachmentType
-- --       -- ^ The current state of the attachment between the gateway and the VPC.
-- --     , dvhAvailability-zone :: !Text
-- --       -- ^ The Availability Zone for the virtual private gateway.
-- --     , dvhState             :: !Text
-- --       -- ^ The state of the virtual private gateway.
-- --     , dvhTag-key           :: !Text
-- --       -- ^ The key of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-value filter. For example, if you use both
-- --       -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
-- --       -- get any resources assigned both the tag key Purpose (regardless
-- --       -- of what the tag's value is), and the tag value X (regardless of
-- --       -- what the tag's key is). If you want to list only resources where
-- --       -- Purpose is X, see the tag:key filter.
-- --     , dvhTag-value         :: !Text
-- --       -- ^ The value of a tag assigned to the resource. This filter is
-- --       -- independent of the tag-key filter.
-- --     , dvhTag:              :: !Text
-- --       -- ^ Filters the response based on a specific tag/value combination.
-- --     , dvhKey               :: !Text
-- --       -- ^ The type of virtual private gateway. Currently the only supported
-- --       -- type is ipsec.1.
-- --     , dvhType              :: !Text
-- --       -- ^ The ID of the virtual private gateway.
-- --     , dvhVpn-gateway-id    :: !Text
-- --       -- ^

-- data DescribeVpnGateways = DescribeVpnGateways
--     { dvhVpnGatewayId :: Members Text
--       -- ^ One or more virtual private gateway IDs.
--     , dvhFilter       :: Members Text
--       -- ^ The name of a filter.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeVpnGateways

-- instance AWSRequest EC2 DescribeVpnGateways DescribeVpnGatewaysResponse where
--     request = query4 ec2 GET "DescribeVpnGateways"

-- data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
--     { dvhRequestId     :: !Text
--       -- ^ The ID of the request.
--     , dvhVpnGatewaySet :: !VpnGatewayType
--       -- ^ A list of virtual private gateways. Each virtual private gateway
--       -- is wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeVpnGatewaysResponse where
--     xmlPickler = ec2XML

-- -- | Detaches an Internet gateway from a VPC, disabling connectivity between the
-- -- Internet and the VPC. The VPC must not contain any running instances with
-- -- Elastic IP addresses.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachInternetGateway.html>

-- data DetachInternetGateway = DetachInternetGateway
--     { diiInternetGatewayId :: !Text
--       -- ^ The ID of the Internet gateway.
--     , diiVpcId             :: !Text
--       -- ^ The ID of the VPC.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DetachInternetGateway

-- instance AWSRequest EC2 DetachInternetGateway DetachInternetGatewayResponse where
--     request = query4 ec2 GET "DetachInternetGateway"

-- data DetachInternetGatewayResponse = DetachInternetGatewayResponse
--     { diiRequestId :: !Text
--       -- ^ The ID of the request.
--     , diiReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DetachInternetGatewayResponse where
--     xmlPickler = ec2XML

-- -- | Detaches a network interface from an instance.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachNetworkInterface.html>

-- data DetachNetworkInterface = DetachNetworkInterface
--     { dniAttachmentId :: !Text
--       -- ^ The ID of the attachment.
--     , dniForce        :: Maybe Bool
--       -- ^ Set to true to force a detachment.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DetachNetworkInterface

-- instance IsXML DetachNetworkInterface where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DetachNetworkInterface DetachNetworkInterfaceResponse where
--     request = query4 ec2 GET "DetachNetworkInterface"

-- data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
--     { dnkRequestId :: !Text
--       -- ^ The ID of the request.
--     , dnkReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DetachNetworkInterfaceResponse where
--     xmlPickler = ec2XML

-- -- | Detaches an Amazon EBS volume from an instance. Make sure to unmount any
-- -- file systems on the device within your operating system before detaching
-- -- the volume. Failure to do so will result in the volume being stuck in
-- -- "busy" state while detaching. For more information about Amazon EBS, see
-- -- Amazon Elastic Block Store in the Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVolume.html>

-- data DetachVolume = DetachVolume
--     { dxVolumeId   :: !Text
--       -- ^ The ID of the volume.
--     , dxInstanceId :: Maybe Text
--       -- ^ The ID of the instance.
--     , dxDevice     :: Maybe Text
--       -- ^ The device name.
--     , dxForce      :: Maybe Bool
--       -- ^ Forces detachment if the previous detachment attempt did not
--       -- occur cleanly (logging into an instance, unmounting the volume,
--       -- and detaching normally). This option can lead to data loss or a
--       -- corrupted file system. Use this option only as a last resort to
--       -- detach a volume from a failed instance. The instance won't have
--       -- an opportunity to flush file system caches or file system
--       -- metadata. If you use this option, you must perform file system
--       -- check and repair procedures.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DetachVolume

-- instance IsXML DetachVolume where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DetachVolume DetachVolumeResponse where
--     request = query4 ec2 GET "DetachVolume"

-- data DetachVolumeResponse = DetachVolumeResponse
--     { edRequestId  :: !Text
--       -- ^ The ID of the request.
--     , edVolumeId   :: !Text
--       -- ^ The ID of the volume.
--     , edInstanceId :: !Text
--       -- ^ The ID of the instance.
--     , edDevice     :: !Text
--       -- ^ The device name exposed to the instance.
--     , edStatus     :: !Text
--       -- ^ The attachment state.
--     , edAttachTime :: !UTCTime
--       -- ^ The time stamp when the attachment initiated.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DetachVolumeResponse where
--     xmlPickler = ec2XML

-- -- | Detaches a virtual private gateway from a VPC. You do this if you're
-- -- planning to turn off the VPC and not use it anymore. You can confirm a
-- -- virtual private gateway has been completely detached from a VPC by
-- -- describing the virtual private gateway (any attachments to the virtual
-- -- private gateway are also described).
-- --
-- -- You must wait for the attachment's state to switch to detached before you
-- -- can delete the VPC or attach a different VPC to the virtual private gateway.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVpnGateway.html>

-- data DetachVpnGateway = DetachVpnGateway
--     { dviVpnGatewayId :: !Text
--       -- ^ The ID of the virtual private gateway.
--     , dviVpcId        :: !Text
--       -- ^ The ID of the VPC.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DetachVpnGateway

-- instance AWSRequest EC2 DetachVpnGateway DetachVpnGatewayResponse where
--     request = query4 ec2 GET "DetachVpnGateway"

-- data DetachVpnGatewayResponse = DetachVpnGatewayResponse
--     { dviRequestId :: !Text
--       -- ^ The ID of the request.
--     , dviReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DetachVpnGatewayResponse where
--     xmlPickler = ec2XML

-- -- | Disables a virtual private gateway (VGW) from propagating routes to the
-- -- routing tables of a VPC.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisableVgwRoutePropagation.html>

-- data DisableVgwRoutePropagation = DisableVgwRoutePropagation
--     { dvrpRouteTableId :: !Text
--       -- ^ The ID of the routing table.
--     , dvrpGatewayId    :: !Text
--       -- ^ The ID of the virtual private gateway.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DisableVgwRoutePropagation

-- instance AWSRequest EC2 DisableVgwRoutePropagation DisableVgwRoutePropagationResponse where
--     request = query4 ec2 GET "DisableVgwRoutePropagation"

-- data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
--     { dvrpRequestId :: !Text
--       -- ^ The ID of the request.
--     , dvrpReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DisableVgwRoutePropagationResponse where
--     xmlPickler = ec2XML

-- -- | Disassociates an Elastic IP address from the instance or network interface
-- -- it's associated with. An Elastic IP address is for use in either the
-- -- EC2-Classic platform or in a VPC.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateAddress.html>

-- data DisassociateAddress = DisassociateAddress
--     { dbPublicIp      :: !Text
--       -- ^ [EC2-Classic] The Elastic IP address.
--     , dbAssociationId :: !Text
--       -- ^ [EC2-VPC] The association ID.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DisassociateAddress

-- instance AWSRequest EC2 DisassociateAddress DisassociateAddressResponse where
--     request = query4 ec2 GET "DisassociateAddress"

-- data DisassociateAddressResponse = DisassociateAddressResponse
--     { dbRequestId :: !Text
--       -- ^ The ID of the request.
--     , dbReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DisassociateAddressResponse where
--     xmlPickler = ec2XML

-- -- | Disassociates a subnet from a route table.After you perform this action,
-- -- the subnet no longer uses the routes in the route table. Instead, it uses
-- -- the routes in the VPC's main route table. For more information about route
-- -- tables, see Route Tables in the Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateRouteTable.html>

-- data DisassociateRouteTable = DisassociateRouteTable
--     { drtAssociationId :: !Text
--       -- ^ The association ID representing the current association between
--       -- the route table and subnet.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DisassociateRouteTable

-- instance IsXML DisassociateRouteTable where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 DisassociateRouteTable DisassociateRouteTableResponse where
--     request = query4 ec2 GET "DisassociateRouteTable"

-- data DisassociateRouteTableResponse = DisassociateRouteTableResponse
--     { drvRequestId :: !Text
--       -- ^ The ID of the request.
--     , drvReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML DisassociateRouteTableResponse where
--     xmlPickler = ec2XML

-- -- | Enables a virtual private gateway (VGW) to propagate routes to the routing
-- -- tables of a VPC.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVgwRoutePropagation.html>

-- data EnableVgwRoutePropagation = EnableVgwRoutePropagation
--     { evrpRouteTableId :: !Text
--       -- ^ The ID of the routing table.
--     , evrpGatewayId    :: !Text
--       -- ^ The ID of the virtual private gateway.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery EnableVgwRoutePropagation

-- instance AWSRequest EC2 EnableVgwRoutePropagation EnableVgwRoutePropagationResponse where
--     request = query4 ec2 GET "EnableVgwRoutePropagation"

-- data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse
--     { evrpRequestId :: !Text
--       -- ^ The ID of the request.
--     , evrpReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML EnableVgwRoutePropagationResponse where
--     xmlPickler = ec2XML

-- -- | Enables I/O operations for a volume that had I/O operations disabled
-- -- because the data on the volume was potentially inconsistent.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVolumeIO.html>

-- data EnableVolumeIO = EnableVolumeIO
--     { evioVolumeId :: !Text
--       -- ^ The ID of the volume.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery EnableVolumeIO

-- instance AWSRequest EC2 EnableVolumeIO EnableVolumeIOResponse where
--     request = query4 ec2 GET "EnableVolumeIO"

-- data EnableVolumeIOResponse = EnableVolumeIOResponse
--     { evioRequestId :: !Text
--       -- ^ The ID of the request.
--     , evioReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML EnableVolumeIOResponse where
--     xmlPickler = ec2XML

-- -- | Gets the console output for the specified instance. Instances do not have a
-- -- physical monitor through which you can view their console output. They also
-- -- lack physical controls that allow you to power up, reboot, or shut them
-- -- down. To allow these actions, we provide them through the Amazon EC2 API
-- -- and command line interface.Instance console output is buffered and posted
-- -- shortly after instance boot, reboot, and termination. Amazon EC2 preserves
-- -- the most recent 64 KB output which will be available for at least one hour
-- -- after the most recent post.For Linux/UNIX instances, the instance console
-- -- output displays the exact console output that would normally be displayed
-- -- on a physical monitor attached to a machine. This output is buffered
-- -- because the instance produces it and then posts it to a store where the
-- -- instance's owner can retrieve it.For Windows instances, the instance
-- -- console output displays the last three system event log errors.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-GetConsoleOutput.html>

-- data GetConsoleOutput = GetConsoleOutput
--     { gcoInstanceId :: !Text
--       -- ^ The ID of the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery GetConsoleOutput

-- instance AWSRequest EC2 GetConsoleOutput GetConsoleOutputResponse where
--     request = query4 ec2 GET "GetConsoleOutput"

-- data GetConsoleOutputResponse = GetConsoleOutputResponse
--     { gcoRequestId  :: !Text
--       -- ^ The ID of the request.
--     , gcpInstanceId :: !Text
--       -- ^ The ID of the instance.
--     , gcpTimestamp  :: !UTCTime
--       -- ^ The time the output was last updated.
--     , gcpOutput     :: !Text
--       -- ^ The console output, Base64 encoded.
--     } deriving (Eq, Show, Generic)

-- instance IsXML GetConsoleOutputResponse where
--     xmlPickler = ec2XML

-- -- | Retrieves the encrypted administrator password for an instance running
-- -- Windows.The Windows password is only generated the first time an AMI is
-- -- launched. It is not generated for rebundled AMIs or after the password is
-- -- changed on an instance.The password is encrypted using the key pair that
-- -- you specified when you launched the instance. You must provide the
-- -- corresponding key pair file.Password generation and encryption takes a few
-- -- moments. Please wait up to 15 minutes after launching an instance before
-- -- trying to retrieve the generated password.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-GetPasswordData.html>

-- data GetPasswordData = GetPasswordData
--     { gpdInstanceId :: !Text
--       -- ^ The ID of a Windows instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery GetPasswordData

-- instance AWSRequest EC2 GetPasswordData GetPasswordDataResponse where
--     request = query4 ec2 GET "GetPasswordData"

-- data GetPasswordDataResponse = GetPasswordDataResponse
--     { gpdRequestId    :: !Text
--       -- ^ The ID of the request.
--     , gpeInstanceId   :: !Text
--       -- ^ The ID of the instance.
--     , gpeTimestamp    :: !UTCTime
--       -- ^ The time the data was last updated.
--     , gpePasswordData :: !Text
--       -- ^ The password of the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsXML GetPasswordDataResponse where
--     xmlPickler = ec2XML

-- -- | Creates an import instance task using metadata from the specified disk
-- -- image. After importing the image, you then upload it using the
-- -- ec2-upload-disk-image command in the EC2 command line tools. For more
-- -- information, see Using the Command Line Tools to Import Your Virtual
-- -- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportInstance.html>

-- data ImportInstance = ImportInstance
--     { iiDescription         :: Maybe Text
--       -- ^ A description for the instance being imported.
--     , iiLaunchSpecification :: Members LaunchSpecificationType
--       -- ^ The architecture of the instance.
--     , iiDiskImage           :: Members DiskImageType
--       -- ^ The file format of the disk image.
--     , iiPlatform            :: Maybe Text
--       -- ^ The instance operating system.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ImportInstance

-- instance IsXML ImportInstance where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ImportInstance ImportInstanceResponse where
--     request = query4 ec2 GET "ImportInstance"

-- data ImportInstanceResponse = ImportInstanceResponse
--     { iiConversionTask :: !ConversionTaskType
--       -- ^ Information about the import instance task.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ImportInstanceResponse where
--     xmlPickler = ec2XML

-- -- | Imports the public key from an RSA key pair that you created with a
-- -- third-party tool. Compare this with CreateKeyPair, in which AWS creates the
-- -- key pair and gives the keys to you (AWS keeps a copy of the public key).
-- -- With ImportKeyPair, you create the key pair and give AWS just the public
-- -- key. The private key is never transferred between you and AWS.You can
-- -- easily create an RSA key pair on Windows and Linux using the ssh-keygen
-- -- command line tool (provided with the standard OpenSSH installation).
-- -- Standard library support for RSA key pair creation is also available in
-- -- Java, Ruby, Python, and many other programming languages.Supported
-- -- formats:DSA keys are not supported. Make sure your key generator is set up
-- -- to create RSA keys.Supported lengths: 1024, 2048, and 4096.Note that you
-- -- can have up to five thousand key pairs per region.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportKeyPair.html>

-- data ImportKeyPair = ImportKeyPair
--     { ikpKeyName           :: !Text
--       -- ^ A unique name for the key pair.
--     , ikpPublicKeyMaterial :: !Text
--       -- ^ The public key. You must base64 encode the public key material
--       -- before sending it to AWS.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ImportKeyPair

-- instance AWSRequest EC2 ImportKeyPair ImportKeyPairResponse where
--     request = query4 ec2 GET "ImportKeyPair"

-- data ImportKeyPairResponse = ImportKeyPairResponse
--     { ikpRequestId      :: !Text
--       -- ^ The ID of the request.
--     , ikqKeyName        :: !Text
--       -- ^ The key pair name you provided.
--     , ikqKeyFingerprint :: !Text
--       -- ^ The MD5 public key fingerprint as specified in section 4 of
--       -- RFC4716.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ImportKeyPairResponse where
--     xmlPickler = ec2XML

-- -- | Creates an import volume task using metadata from the specified disk image.
-- -- After importing the image, you then upload it using the
-- -- ec2-upload-disk-image command in the EC2 command line tools. For more
-- -- information, see Using the Command Line Tools to Import Your Virtual
-- -- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportVolume.html>

-- data ImportVolume = ImportVolume
--     { ivAvailabilityZone :: !Text
--       -- ^ The Availability Zone for the resulting Amazon EBS volume.
--     , ivImage            :: Members ImageType
--       -- ^ The file format of the disk image.
--     , ivDescription      :: Maybe Text
--       -- ^ An optional description for the volume being imported.
--     , ivVolume           :: Members VolumeType
--       -- ^ The size, in GB (2^30 bytes), of an Amazon EBS volume to hold the
--       -- converted image.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ImportVolume

-- instance IsXML ImportVolume where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ImportVolume ImportVolumeResponse where
--     request = query4 ec2 GET "ImportVolume"

-- data ImportVolumeResponse = ImportVolumeResponse
--     { ivConversionTask :: !ConversionTaskType
--       -- ^ Information about the import volume task.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ImportVolumeResponse where
--     xmlPickler = ec2XML

-- -- | Modifies the specified attribute of the specified AMI. You can specify only
-- -- one attribute at a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyImageAttribute.html>

-- data ModifyImageAttribute = ModifyImageAttribute
--     { miaImageId          :: !Text
--       -- ^ The ID of the AMI.
--     , miaLaunchPermission :: Members LaunchPermissionType
--       -- ^ Adds the specified AWS account ID to the AMI's list of launch
--       -- permissions.
--     , miaProductCode      :: Members Text
--       -- ^ Adds the specified product code to the specified instance
--       -- store-backed AMI. After you add a product code to an AMI, it
--       -- can't be removed.
--     , miaDescription      :: Members DescriptionType
--       -- ^ Changes the AMI's description to the specified value.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ModifyImageAttribute

-- instance IsXML ModifyImageAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ModifyImageAttribute ModifyImageAttributeResponse where
--     request = query4 ec2 GET "ModifyImageAttribute"

-- data ModifyImageAttributeResponse = ModifyImageAttributeResponse
--     { miaRequestId :: !Text
--       -- ^ The ID of the request.
--     , miaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ModifyImageAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Modifies the specified attribute of the specified instance. You can specify
-- -- only one attribute at a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyInstanceAttribute.html>

-- data ModifyInstanceAttribute = ModifyInstanceAttribute
--     { miaInstanceId                        :: !Text
--       -- ^ The ID of the instance.
--     , miaInstanceType                      :: Maybe InstanceType
--       -- ^ Changes the instance type to the specified value. See Available
--       -- Instance Types for more information. An
--       -- InvalidInstanceAttributeValue error will be returned if the
--       -- instance type is not valid.
--     , miaKernel                            :: Maybe Text
--       -- ^ Changes the instance's kernel to the specified value.
--     , miaRamdisk                           :: Maybe Text
--       -- ^ Changes the instance's RAM disk to the specified value.
--     , miaUserData                          :: Maybe Text
--       -- ^ Changes the instance's user data to the specified value.
--     , miaDisableApiTermination             :: Maybe Bool
--       -- ^ If the value is true, you can't terminate the instance using the
--       -- Amazon EC2 console, CLI, or API; otherwise you can.
--     , miaInstanceInitiatedShutdownBehavior :: Maybe Text
--       -- ^ Indicates whether an instance stops or terminates when you
--       -- initiate shutdown from the instance (using the operating system
--       -- command for system shutdown).
--     , miaBlockDeviceMapping                :: Members InstanceBlockDeviceMappingItemType
--       -- ^ Modifies the DeleteOnTermination attribute for volumes that are
--       -- currently attached. The volume must be owned by the caller. If no
--       -- value is specified for DeleteOnTermination, the default is true
--       -- and the volume is deleted when the instance is terminated.
--     , miaSourceDestCheck                   :: Maybe Bool
--       -- ^ Indicates whether source/destination checking is enabled. A value
--       -- of true means checking is enabled, and false means checking is
--       -- disabled. This value must be false for a NAT instance to perform
--       -- NAT.
--     , miaGroupId                           :: Members Text
--       -- ^ [EC2-VPC] Changes the instance's security group. You must specify
--       -- at least one security group, even if it's just the default
--       -- security group for the VPC. You must specify the security group
--       -- ID, not the security group name.
--     , miaEbsOptimized                      :: Maybe Bool
--       -- ^ Indicates whether the instance is optimized for EBS I/O. This
--       -- optimization provides dedicated throughput to Amazon EBS and an
--       -- optimized configuration stack to provide optimal EBS I/O
--       -- performance. This optimization isn't available with all instance
--       -- types. Additional usage charges apply when using an EBS Optimized
--       -- instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ModifyInstanceAttribute

-- instance IsXML ModifyInstanceAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ModifyInstanceAttribute ModifyInstanceAttributeResponse where
--     request = query4 ec2 GET "ModifyInstanceAttribute"

-- data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
--     { mibRequestId :: !Text
--       -- ^ The ID of the request.
--     , mibReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ModifyInstanceAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Modifies the specified network interface attribute. You can specify only
-- -- one attribute at a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyNetworkInterfaceAttribute.html>

-- data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
--     { mniaNetworkInterfaceId :: !Text
--       -- ^ The ID of the network interface.
--     , mniaDescription        :: Members DescriptionType
--       -- ^ A description for the network interface.
--     , mniaSecurityGroupId    :: Members Text
--       -- ^ Changes the security groups that a network interface is in. The
--       -- new set of groups you specify replaces the current set. You must
--       -- specify at least one group, even if it's just the default
--       -- security group in the VPC. You must specify the group ID and not
--       -- the group name.
--     , mniaSourceDestCheck    :: Members SourceDestCheckType
--       -- ^ Indicates whether source/destination checking is enabled. A value
--       -- of true means checking is enabled, and false means checking is
--       -- disabled. This value must be false for a NAT instance to perform
--       -- NAT.
--     , mniaAttachment         :: Members AttachmentType
--       -- ^ The ID of the interface attachment.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ModifyNetworkInterfaceAttribute

-- instance IsXML ModifyNetworkInterfaceAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ModifyNetworkInterfaceAttribute ModifyNetworkInterfaceAttributeResponse where
--     request = query4 ec2 GET "ModifyNetworkInterfaceAttribute"

-- data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
--     { mniaRequestId :: !Text
--       -- ^ The ID of the request.
--     , mniaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ModifyNetworkInterfaceAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Adds or remove permission settings for the specified snapshot.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySnapshotAttribute.html>

-- data ModifySnapshotAttribute = ModifySnapshotAttribute
--     { msaSnapshotId             :: !Text
--       -- ^ The ID of the snapshot.
--     , msaCreateVolumePermission :: Members CreateVolumePermissionType
--       -- ^ Adds the specified AWS account ID to the volume's list of create
--       -- volume permissions.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ModifySnapshotAttribute

-- instance IsXML ModifySnapshotAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ModifySnapshotAttribute ModifySnapshotAttributeResponse where
--     request = query4 ec2 GET "ModifySnapshotAttribute"

-- data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
--     { msaRequestId :: !Text
--       -- ^ The ID of the request.
--     , msaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ModifySnapshotAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Modifies a volume attribute.By default, all I/O operations for the volume
-- -- are suspended when the data on the volume is determined to be potentially
-- -- inconsistent, to prevent undetectable, latent data corruption. The I/O
-- -- access to the volume can be resumed by first calling EnableVolumeIO action
-- -- to enable I/O access and then checking the data consistency on your
-- -- volume.You can change the default behavior to resume I/O operations without
-- -- calling EnableVolumeIO action by setting the AutoEnableIO attribute of the
-- -- volume to true. We recommend that you change this attribute only for
-- -- volumes that are stateless, or disposable, or for boot volumes.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVolumeAttribute.html>

-- data ModifyVolumeAttribute = ModifyVolumeAttribute
--     { mvaVolumeId     :: !Text
--       -- ^ The ID of the volume.
--     , mvaAutoEnableIO :: Members AutoEnableIOType
--       -- ^ Specifies whether the volume should be auto-enabled for I/O
--       -- operations.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ModifyVolumeAttribute

-- instance IsXML ModifyVolumeAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ModifyVolumeAttribute ModifyVolumeAttributeResponse where
--     request = query4 ec2 GET "ModifyVolumeAttribute"

-- data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
--     { mvaRequestId :: !Text
--       -- ^ The ID of the request.
--     , mvaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ModifyVolumeAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Modifies the specified attribute of the specified VPC.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVpcAttribute.html>

-- data ModifyVpcAttribute = ModifyVpcAttribute
--     { mvaVpcId              :: !Text
--       -- ^ The ID of the VPC.
--     , mvaEnableDnsSupport   :: Maybe Bool
--       -- ^ Specifies whether DNS resolution is supported for the VPC. If
--       -- this attribute is true, the Amazon DNS server resolves DNS
--       -- hostnames for your instances to their corresponding IP addresses;
--       -- otherwise, it does not.
--     , mvaEnableDnsHostnames :: Maybe Bool
--       -- ^ Specifies whether the instances launched in the VPC get DNS
--       -- hostnames. If this attribute is true, instances in the VPC get
--       -- DNS hostnames; otherwise, they do not.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ModifyVpcAttribute

-- instance IsXML ModifyVpcAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ModifyVpcAttribute ModifyVpcAttributeResponse where
--     request = query4 ec2 GET "ModifyVpcAttribute"

-- data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
--     { mvbRequestId :: !Text
--       -- ^ The ID of the request.
--     , mvbReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ModifyVpcAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Enables monitoring for a running instance. For more information about
-- -- monitoring instances, see Monitoring Your Instances and Volumes in the
-- -- Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MonitorInstances.html>

-- data MonitorInstances = MonitorInstances
--     { miInstanceId :: Members Text
--       -- ^ One or more instance IDs.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery MonitorInstances

-- instance AWSRequest EC2 MonitorInstances MonitorInstancesResponse where
--     request = query4 ec2 GET "MonitorInstances"

-- data MonitorInstancesResponse = MonitorInstancesResponse
--     { miRequestId    :: !Text
--       -- ^ The ID of the request.
--     , miInstancesSet :: !MonitorInstancesResponseSetItemType
--       -- ^ A list of instances. Each instance is wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML MonitorInstancesResponse where
--     xmlPickler = ec2XML

-- -- | Purchases a Reserved Instance for use with your account. With Amazon EC2
-- -- Reserved Instances, you obtain a capacity reservation for a certain
-- -- instance configuration over a specified period of time. You pay a lower
-- -- usage rate than with On-Demand instances for the time that you actually use
-- -- the capacity reservation. Starting with the 2011-11-01 API version, AWS
-- -- expanded its offering of Reserved Instances to address a range of projected
-- -- instance usage. There are three types of Reserved Instances based on
-- -- customer utilization levels: Heavy Utilization, Medium Utilization, and
-- -- Light Utilization.The Medium Utilization offering type is equivalent to the
-- -- Reserved Instance offering available before API version 2011-11-01. If you
-- -- are using tools that predate the 2011-11-01 API version,
-- -- DescribeReservedInstancesOfferings will only list information about the
-- -- Medium Utilization Reserved Instance offering type.For information about
-- -- Reserved Instance pricing tiers, go to Understanding Reserved Instance
-- -- pricing tiers in the Amazon Elastic Compute Cloud User Guide. For more
-- -- information about Reserved Instances, go to Reserved Instances also in the
-- -- Amazon Elastic Compute Cloud User Guide.You determine the type of the
-- -- Reserved Instances offerings by including the optional offeringType
-- -- parameter when calling DescribeReservedInstancesOfferings. After you've
-- -- identified the Reserved Instance with the offering type you want, specify
-- -- its ReservedInstancesOfferingId when you call
-- -- PurchaseReservedInstancesOffering. Starting with the 2012-08-15 API
-- -- version, you can also purchase Reserved Instances from the Reserved
-- -- Instance Marketplace. The Reserved Instance Marketplace matches sellers who
-- -- want to resell Reserved Instance capacity that they no longer need with
-- -- buyers who want to purchase additional capacity. Reserved Instances bought
-- -- and sold through the Reserved Instance Marketplace work like any other
-- -- Reserved Instances.By default, with the 2012-08-15 API version,
-- -- DescribeReservedInstancesOfferings returns information about Amazon EC2
-- -- Reserved Instances available directly from AWS, plus instance offerings
-- -- available on the Reserved Instance Marketplace. If you are using tools that
-- -- predate the 2012-08-15 API version, the DescribeReservedInstancesOfferings
-- -- action will only list information about Amazon EC2 Reserved Instances
-- -- available directly from AWS.For more information about the Reserved
-- -- Instance Marketplace, go to Reserved Instance Marketplace in the Amazon
-- -- Elastic Compute Cloud User Guide. You determine the Reserved Instance
-- -- Marketplace offerings by specifying true for the optional
-- -- includeMarketplace parameter when calling
-- -- DescribeReservedInstancesOfferings. After you've identified the Reserved
-- -- Instance with the offering type you want, specify its
-- -- reservedInstancesOfferingId when you call
-- -- PurchaseReservedInstancesOffering.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-PurchaseReservedInstancesOffering.html>

-- data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
--     { prioReservedInstancesOfferingId :: !Text
--       -- ^ The ID of the Reserved Instance offering you want to purchase.
--     , prioInstanceCount               :: !Integer
--       -- ^ The number of Reserved Instances to purchase.
--     , prioLimitPrice                  :: Maybe ReservedInstanceLimitPriceType
--       -- ^ Specified for Reserved Instance Marketplace offerings to limit
--       -- the total order and ensure that the Reserved Instances are not
--       -- purchased at unexpected prices.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PurchaseReservedInstancesOffering

-- instance AWSRequest EC2 PurchaseReservedInstancesOffering PurchaseReservedInstancesOfferingResponse where
--     request = query4 ec2 GET "PurchaseReservedInstancesOffering"

-- data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
--     { prioRequestId           :: !Text
--       -- ^ The ID of the request.
--     , prioReservedInstancesId :: !Text
--       -- ^ The IDs of the purchased Reserved Instances.
--     } deriving (Eq, Show, Generic)

-- instance IsXML PurchaseReservedInstancesOfferingResponse where
--     xmlPickler = ec2XML

-- -- | Requests a reboot of one or more instances. This operation is asynchronous;
-- -- it only queues a request to reboot the specified instance(s). The operation
-- -- will succeed if the instances are valid and belong to you. Requests to
-- -- reboot terminated instances are ignored.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RebootInstances.html>

-- data RebootInstances = RebootInstances
--     { riInstanceId :: Members Text
--       -- ^ One or more instance IDs.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RebootInstances

-- instance AWSRequest EC2 RebootInstances RebootInstancesResponse where
--     request = query4 ec2 GET "RebootInstances"

-- data RebootInstancesResponse = RebootInstancesResponse
--     { riRequestId :: !Text
--       -- ^ The ID of the request.
--     , riReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML RebootInstancesResponse where
--     xmlPickler = ec2XML

-- -- | Registers an AMI. When you're creating an AMI, this is the final step you
-- -- must complete before you can launch an instance from the AMI. For more
-- -- information about creating AMIs, see Creating Your Own AMIs in the Amazon
-- -- Elastic Compute Cloud User Guide.You can also use the RegisterImage action
-- -- to create an EBS-backed AMI from a snapshot of a root device volume. For
-- -- more information, see Launching an Instance from a Snapshot in the Amazon
-- -- Elastic Compute Cloud User Guide.If needed, you can deregister an AMI at
-- -- any time. Any modifications you make to an AMI backed by instance store
-- -- invalidates its registration. If you make changes to an image, deregister
-- -- the previous image and register the new image.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RegisterImage.html>

-- data RegisterImage = RegisterImage
--     { riImageLocation      :: !Text
--       -- ^ The full path to your AMI manifest in Amazon S3 storage.
--     , riName               :: !Text
--       -- ^ A name for your AMI.
--     , riDescription        :: Maybe Text
--       -- ^ A description for your AMI.
--     , riArchitecture       :: Maybe Text
--       -- ^ The architecture of the image.
--     , riKernelId           :: Maybe Text
--       -- ^ The ID of the kernel.
--     , riRamdiskId          :: Maybe Text
--       -- ^ The ID of the RAM disk.
--     , riVirtualizationType :: Maybe Text
--       -- ^ The type of virtualization.
--     , riRootDeviceName     :: !Text
--       -- ^ The name of the root device (for example, /dev/sda1, or xvda).
--     , riBlockDeviceMapping :: Members BlockDeviceMappingType
--       -- ^ The device name exposed to the instance (for example, /dev/sdh or
--       -- xvdh).
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RegisterImage

-- instance IsXML RegisterImage where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 RegisterImage RegisterImageResponse where
--     request = query4 ec2 GET "RegisterImage"

-- data RegisterImageResponse = RegisterImageResponse
--     { rjRequestId :: !Text
--       -- ^ The ID of the request.
--     , rjImageId   :: !Text
--       -- ^ The ID of the newly registered AMI.
--     } deriving (Eq, Show, Generic)

-- instance IsXML RegisterImageResponse where
--     xmlPickler = ec2XML

-- -- | Releases the specified Elastic IP address. An Elastic IP address is for use
-- -- either in the EC2-Classic platform or in a VPC.[EC2-Classic, default VPC] Releasing an Elastic IP address
-- -- automatically disassociates it from any instance that it's associated with.
-- -- To disassociate an Elastic IP address without releasing it, use the
-- -- ec2-disassociate-address command. [Nondefault VPC] You must use the
-- -- ec2-disassociate-address command to disassociate the Elastic IP address
-- -- before you try to release it. Otherwise, Amazon EC2 returns an error
-- -- (InvalidIPAddress.InUse).
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReleaseAddress.html>

-- data ReleaseAddress = ReleaseAddress
--     { raPublicIp     :: !Text
--       -- ^ [EC2-Classic] The Elastic IP address.
--     , raAllocationId :: !Text
--       -- ^ [EC2-VPC] The allocation ID.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ReleaseAddress

-- instance AWSRequest EC2 ReleaseAddress ReleaseAddressResponse where
--     request = query4 ec2 GET "ReleaseAddress"

-- data ReleaseAddressResponse = ReleaseAddressResponse
--     { raRequestId :: !Text
--       -- ^ The ID of the request.
--     , raReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ReleaseAddressResponse where
--     xmlPickler = ec2XML

-- -- | Changes which network ACL a subnet is associated with. By default when you
-- -- create a subnet, it's automatically associated with the default network
-- -- ACL. For more information about network ACLs, see Network ACLs in the
-- -- Amazon Virtual Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkAclAssociation.html>

-- data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
--     { rnaaAssociationId :: !Text
--       -- ^ The ID representing the current association between the original
--       -- network ACL and the subnet.
--     , rnaaNetworkAclId  :: !Text
--       -- ^ The ID of the new ACL to associate with the subnet.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ReplaceNetworkAclAssociation

-- instance AWSRequest EC2 ReplaceNetworkAclAssociation ReplaceNetworkAclAssociationResponse where
--     request = query4 ec2 GET "ReplaceNetworkAclAssociation"

-- data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
--     { rnaaRequestId        :: !Text
--       -- ^ The ID of the request.
--     , rnaaNewAssociationId :: !Text
--       -- ^ The ID of the new association.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ReplaceNetworkAclAssociationResponse where
--     xmlPickler = ec2XML

-- -- | Replaces an entry (rule) in a network ACL. For more information about
-- -- network ACLs, see Network ACLs in the Amazon Virtual Private Cloud User
-- -- Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkAclEntry.html>

-- data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
--     { rnaeNetworkAclId :: !Text
--       -- ^ The ID of the ACL.
--     , rnaeRuleNumber   :: !Integer
--       -- ^ The rule number of the entry to replace.
--     , rnaeProtocol     :: !Integer
--       -- ^ The IP protocol the rule applies to. You can use -1 to mean all
--       -- protocols.
--     , rnaeRuleAction   :: !Text
--       -- ^ Indicates whether to allow or deny traffic that matches the rule.
--     , rnaeEgress       :: Maybe Bool
--       -- ^ Indicates whether this rule applies to egress traffic from the
--       -- subnet (true) or ingress traffic to the subnet (false).
--     , rnaeCidrBlock    :: !Text
--       -- ^ The CIDR range to allow or deny, in CIDR notation (for example,
--       -- 172.16.0.0/24).
--     , rnaeIcmp         :: Members IcmpType
--       -- ^ For the ICMP protocol, the ICMP code. You can use -1 to specify
--       -- all ICMP codes for the given ICMP type.
--     , rnaePortRange    :: Members PortRangeType
--       -- ^ The first port in the range.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ReplaceNetworkAclEntry

-- instance AWSRequest EC2 ReplaceNetworkAclEntry ReplaceNetworkAclEntryResponse where
--     request = query4 ec2 GET "ReplaceNetworkAclEntry"

-- data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
--     { rnaeRequestId :: !Text
--       -- ^ The ID of the request.
--     , rnaeReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ReplaceNetworkAclEntryResponse where
--     xmlPickler = ec2XML

-- -- | Replaces an existing route within a route table in a VPC. For more
-- -- information about route tables, see Route Tables in the Amazon Virtual
-- -- Private Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRoute.html>

-- data ReplaceRoute = ReplaceRoute
--     { rrRouteTableId         :: !Text
--       -- ^ The ID of the route table.
--     , rrDestinationCidrBlock :: !Text
--       -- ^ The CIDR address block used for the destination match. The value
--       -- you provide must match the CIDR of an existing route in the
--       -- table.
--     , rrGatewayId            :: !Text
--       -- ^ The ID of a gateway attached to your VPC.
--     , rrInstanceId           :: !Text
--       -- ^ The ID of a NAT instance in your VPC.
--     , rrNetworkInterfaceId   :: !Text
--       -- ^ Allows routing to network interface attachments.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ReplaceRoute

-- instance IsXML ReplaceRoute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ReplaceRoute ReplaceRouteResponse where
--     request = query4 ec2 GET "ReplaceRoute"

-- data ReplaceRouteResponse = ReplaceRouteResponse
--     { rrRequestId :: !Text
--       -- ^ The ID of the request.
--     , rrReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ReplaceRouteResponse where
--     xmlPickler = ec2XML

-- -- | Changes the route table associated with a given subnet in a VPC. After you
-- -- execute this action, the subnet uses the routes in the new route table it's
-- -- associated with. For more information about route tables, see Route Tables
-- -- in the Amazon Virtual Private Cloud User Guide.You can also use this action
-- -- to change which table is the main route table in the VPC. You just specify
-- -- the main route table's association ID and the route table that you want to
-- -- be the new main route table.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRouteTableAssociation.html>

-- data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
--     { rrtaAssociationId :: !Text
--       -- ^ The association ID.
--     , rrtaRouteTableId  :: !Text
--       -- ^ The ID of the new route table to associate with the subnet.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ReplaceRouteTableAssociation

-- instance AWSRequest EC2 ReplaceRouteTableAssociation ReplaceRouteTableAssociationResponse where
--     request = query4 ec2 GET "ReplaceRouteTableAssociation"

-- data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
--     { rrtaRequestId        :: !Text
--       -- ^ The ID of the request.
--     , rrtaNewAssociationId :: !Text
--       -- ^ The ID of the new association.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ReplaceRouteTableAssociationResponse where
--     xmlPickler = ec2XML

-- -- | Use this action to submit feedback about an instance's status. This action
-- -- works only for instances that are in the running state. If your experience
-- -- with the instance differs from the instance status returned by the
-- -- DescribeInstanceStatus action, use ReportInstanceStatus to report your
-- -- experience with the instance. Amazon EC2 collects this information to
-- -- improve the accuracy of status checks. To report an instance's status,
-- -- specify an instance ID with the InstanceId.n parameter and a reason code
-- -- with the ReasonCode.n parameter that applies to that instance. The
-- -- following table contains descriptions of all available reason codes.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReportInstanceStatus.html>

-- data ReportInstanceStatus = ReportInstanceStatus
--     { risInstanceId  :: Members Text
--       -- ^ One or more instance IDs.
--     , risStatus      :: !Text
--       -- ^ The status of all instances listed in the InstanceId.n parameter.
--     , risStartTime   :: Maybe UTCTime
--       -- ^ The time at which the reported instance health state began.
--     , risEndTime     :: Maybe UTCTime
--       -- ^ The time at which the reported instance health state ended.
--     , risReasonCode  :: Members Text
--       -- ^ A reason code that describes a specific instance's health state.
--       -- Each code you supply corresponds to an instance ID that you
--       -- supply with the InstanceId.n parameter. See the Description
--       -- section for descriptions of each reason code.
--     , risDescription :: Maybe Text
--       -- ^ Descriptive text about the instance health state.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ReportInstanceStatus

-- instance AWSRequest EC2 ReportInstanceStatus ReportInstanceStatusResponse where
--     request = query4 ec2 GET "ReportInstanceStatus"

-- data ReportInstanceStatusResponse = ReportInstanceStatusResponse
--     { risRequestId :: !Text
--       -- ^ The ID of the request.
--     , risReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ReportInstanceStatusResponse where
--     xmlPickler = ec2XML

-- -- | Creates a Spot Instance request. Spot Instances are instances that Amazon
-- -- EC2 starts on your behalf when the maximum price that you specify exceeds
-- -- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- -- on available Spot Instance capacity and current Spot Instance requests. For
-- -- more information about Spot Instances, see Spot Instances in the Amazon
-- -- Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotInstances.html>

-- data RequestSpotInstances = RequestSpotInstances
--     { rsiSpotPrice             :: !Text
--       -- ^ The maximum hourly price for any Spot Instance launched to
--       -- fulfill the request.
--     , rsiInstanceCount         :: Maybe Integer
--       -- ^ The maximum number of Spot Instances to launch.
--     , rsiType                  :: Maybe Text
--       -- ^ The Spot Instance request type.
--     , rsiValidFrom             :: Maybe UTCTime
--       -- ^ The start date of the request. If this is a one-time request, the
--       -- request becomes active at this date and time and remains active
--       -- until all instances launch, the request expires, or the request
--       -- is canceled. If the request is persistent, the request becomes
--       -- active at this date and time and remains active until it expires
--       -- or is canceled.
--     , rsiValidUntil            :: Maybe UTCTime
--       -- ^ The end date of the request. If this is a one-time request, the
--       -- request remains active until all instances launch, the request is
--       -- canceled, or this date is reached. If the request is persistent,
--       -- it remains active until it is canceled or this date and time is
--       -- reached.
--     , rsiLaunchGroup           :: Maybe Text
--       -- ^ The instance launch group. Launch groups are Spot Instances that
--       -- launch together and terminate together.
--     , rsiAvailabilityZoneGroup :: Maybe Text
--       -- ^ The user-specified name for a logical grouping of bids.
--     , rsiLaunchSpecification   :: Members LaunchSpecificationType
--       -- ^ The ID of the AMI.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RequestSpotInstances

-- instance AWSRequest EC2 RequestSpotInstances RequestSpotInstancesResponse where
--     request = query4 ec2 GET "RequestSpotInstances"

-- data RequestSpotInstancesResponse = RequestSpotInstancesResponse
--     { rsiRequestId              :: !Text
--       -- ^ The ID of the request.
--     , rsiSpotInstanceRequestSet :: !SpotInstanceRequestSetItemType
--       -- ^ Information about the Spot Instance request, wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML RequestSpotInstancesResponse where
--     xmlPickler = ec2XML

-- -- | Resets an attribute of an AMI to its default value.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetImageAttribute.html>

-- data ResetImageAttribute = ResetImageAttribute
--     { riaImageId   :: !Text
--       -- ^ The ID of the AMI.
--     , riaAttribute :: !Text
--       -- ^ The attribute to reset (currently you can only reset the launch
--       -- permission attribute).
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ResetImageAttribute

-- instance IsXML ResetImageAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ResetImageAttribute ResetImageAttributeResponse where
--     request = query4 ec2 GET "ResetImageAttribute"

-- data ResetImageAttributeResponse = ResetImageAttributeResponse
--     { riaRequestId :: !Text
--       -- ^ The ID of the request.
--     , riaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ResetImageAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Resets an attribute of an instance to its default value. To reset the
-- -- kernel or RAM disk, the instance must be in a stopped state. To reset the
-- -- SourceDestCheck, the instance can be either running or stopped. The
-- -- SourceDestCheck attribute controls whether source/destination checking is
-- -- enabled. The default value is true, which means checking is enabled. This
-- -- value must be false for a NAT instance to perform NAT. For more
-- -- information, see NAT Instances in the Amazon Virtual Private Cloud User
-- -- Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetInstanceAttribute.html>

-- data ResetInstanceAttribute = ResetInstanceAttribute
--     { riaInstanceId :: !Text
--       -- ^ The ID of the instance.
--     , ribAttribute  :: !Text
--       -- ^ The attribute to reset.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ResetInstanceAttribute

-- instance IsXML ResetInstanceAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ResetInstanceAttribute ResetInstanceAttributeResponse where
--     request = query4 ec2 GET "ResetInstanceAttribute"

-- data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
--     { ribRequestId :: !Text
--       -- ^ The ID of the request.
--     , ribReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ResetInstanceAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Resets a network interface attribute. You can specify only one attribute at
-- -- a time.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetNetworkInterfaceAttribute.html>

-- data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
--     { rniaNetworkInterfaceId :: !Text
--       -- ^ The ID of the network interface.
--     , rniaAttribute          :: !Text
--       -- ^ The name of the attribute to reset.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ResetNetworkInterfaceAttribute

-- instance IsXML ResetNetworkInterfaceAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ResetNetworkInterfaceAttribute ResetNetworkInterfaceAttributeResponse where
--     request = query4 ec2 GET "ResetNetworkInterfaceAttribute"

-- data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse
--     { rniaRequestId :: !Text
--       -- ^ The ID of the request.
--     , rniaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ResetNetworkInterfaceAttributeResponse where
--     xmlPickler = ec2XML

-- -- | Resets permission settings for the specified snapshot.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetSnapshotAttribute.html>

-- data ResetSnapshotAttribute = ResetSnapshotAttribute
--     { rsaSnapshotId :: !Text
--       -- ^ The ID of the snapshot.
--     , rsaAttribute  :: !Text
--       -- ^ The attribute to reset (currently only the attribute for
--       -- permission to create volumes can be reset)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ResetSnapshotAttribute

-- instance IsXML ResetSnapshotAttribute where
--     xmlPickler = ec2XML

-- instance AWSRequest EC2 ResetSnapshotAttribute ResetSnapshotAttributeResponse where
--     request = query4 ec2 GET "ResetSnapshotAttribute"

-- data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
--     { rsaRequestId :: !Text
--       -- ^ The ID of the request.
--     , rsaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML ResetSnapshotAttributeResponse where
--     xmlPickler = ec2XML

-- | Removes one or more egress rules from a security group for EC2-VPC.
--
-- The values that you specify in the revoke request (for example, ports) must
-- match the existing rule's values for the rule to be revoked.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupEgress.html>
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { rsgeGroupId       :: !Text
      -- ^ The ID of the security group to modify.
    , rsgeIpPermissions :: [IpPermissionType]
      -- ^ The IP protocol name or number (see Protocol Numbers).
    } deriving (Eq, Show, Generic)

instance IsQuery RevokeSecurityGroupEgress

instance Rq RevokeSecurityGroupEgress where
    type Er RevokeSecurityGroupEgress = EC2ErrorResponse
    type Rs RevokeSecurityGroupEgress = RevokeSecurityGroupEgressResponse
    request = query4 ec2 GET "RevokeSecurityGroupEgress"

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    { rsgerRequestId :: !Text
      -- ^ The ID of the request.
    , rsgerReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an error.
    } deriving (Eq, Show, Generic)

instance IsXML RevokeSecurityGroupEgressResponse where
    xmlPickler = ec2XML

-- | Removes one or more ingress rules from a security group.
--
-- The values that you specify in the revoke request (for example, ports) must
-- match the existing rule's values for the rule to be removed.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupIngress.html>
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { rsgiGroupId       :: Maybe Text
      -- ^ The ID of the security group to modify. The security group must
      -- belong to your account.
    , rsgiGroupName     :: Maybe Text
      -- ^ The name of the security group to modify.
    , rsgiIpPermissions :: [IpPermissionType]
      -- ^ The IP protocol name or number (see Protocol Numbers). For
      -- EC2-Classic, security groups can have rules only for TCP, UDP,
      -- and ICMP. For EC2-VPC, security groups can have rules assigned to
      -- any protocol number.
    } deriving (Eq, Show, Generic)

instance IsQuery RevokeSecurityGroupIngress

instance Rq RevokeSecurityGroupIngress where
    type Er RevokeSecurityGroupIngress = EC2ErrorResponse
    type Rs RevokeSecurityGroupIngress = RevokeSecurityGroupIngressResponse
    request = query4 ec2 GET "RevokeSecurityGroupIngress"

data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
    { rsgirRequestId :: !Text
      -- ^ The ID of the request.
    , rsgirReturn    :: !Bool
      -- ^ Returns true if the request succeeds. Otherwise, returns an error.
    } deriving (Eq, Show, Generic)

instance IsXML RevokeSecurityGroupIngressResponse where
    xmlPickler = ec2XML

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
    { riImageId                           :: !Text
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
    , riKeyName                           :: Maybe Text
      -- ^ The name of the key pair. You can create a key pair using
      -- CreateKeyPair or ImportKeyPair.
    , riSecurityGroupId                   :: [Text]
      -- ^ One or more security group IDs. You can create a security group
      -- using CreateSecurityGroup.
    , riSecurityGroup                     :: [Text]
      -- ^ [EC2-Classic, default VPC] One or more security group names. For
      -- a nondefault VPC, you must use SecurityGroupId.
    , riUserData                          :: Maybe Text
      -- ^ The Base64-encoded MIME user data for the instances.
    , riInstanceType                      :: Maybe InstanceType
      -- ^ The instance type. Defaults to m1.small if absent.
    , riPlacement                         :: Maybe PlacementType
      -- ^ The Availability Zone for the instance.
    , rjKernelId                          :: Maybe Text
      -- ^ The ID of the kernel.
    , rjRamdiskId                         :: Maybe Text
      -- ^ The ID of the RAM disk.
    , rjBlockDeviceMapping                :: [InstanceBlockDeviceMappingItemType]
      -- ^ The device name exposed to the instance (for example, /dev/sdh or xvdh).
    , rjMonitoring                        :: Maybe MonitoringInstanceType
      -- ^ Enables monitoring for the instance.
    , rjSubnetId                          :: Maybe Text
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
    , rjInstanceInitiatedShutdownBehavior :: Maybe Text -- FIXME: Should be stop | terminate
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , rjPrivateIpAddress                  :: Maybe Text
      -- ^ [EC2-VPC] The primary IP address. You must specify a value from
      -- the IP address range of the subnet.
    , rjClientToken                       :: Maybe Text
      -- ^ Unique, case-sensitive identifier you provide to ensure
      -- idempotency of the request.
    , rjNetworkInterface                  :: [NetworkInterfaceType]
      -- ^ An existing interface to attach to a single instance. Requires
      -- n=1 instances.
    , rjIamInstanceProfile                :: [IamInstanceProfileRequestType]
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

instance Rq RunInstances where
    type Er RunInstances = EC2ErrorResponse
    type Rs RunInstances = RunInstancesResponse
    request = query4 ec2 GET "RunInstances"

data RunInstancesResponse = RunInstancesResponse
    { rirRequestId     :: !Text
      -- ^ The ID of the request.
    , rirReservationId :: !Text
      -- ^ The ID of the reservation.
    , rirOwnerId       :: !Text
      -- ^ The ID of the AWS account that owns the reservation.
    , rirGroupSet      :: [GroupItemType]
      -- ^ A list of security groups the instance belongs to. Each group is
      -- wrapped in an item element.
    , rirInstancesSet  :: [RunningInstancesItemType]
      -- ^ A list of instances. Each instance is wrapped in an item element.
    , rirRequesterId   :: Maybe Text
      -- ^ The ID of the requester that launched the instances on your
      -- behalf (for example, AWS Management Console, Auto Scaling).
    } deriving (Eq, Show, Generic)

instance IsXML RunInstancesResponse where
    xmlPickler = ec2XML

-- -- | Starts an Amazon EBS-backed AMI that you've previously stopped. Instances
-- -- that use Amazon EBS volumes as their root devices can be quickly stopped
-- -- and started. When an instance is stopped, the compute resources are
-- -- released and you are not billed for hourly instance usage. However, your
-- -- root partition Amazon EBS volume remains, continues to persist your data,
-- -- and you are charged for Amazon EBS volume usage. You can restart your
-- -- instance at any time. Each time you transition an instance from stopped to
-- -- started, we charge a full instance hour, even if transitions happen
-- -- multiple times within a single hour. Before stopping an instance, make sure
-- -- it is in a state from which it can be restarted. Stopping an instance does
-- -- not preserve data stored in RAM. Performing this operation on an instance
-- -- that uses an instance store as its root device returns an error.For more
-- -- information, see Stopping Instances in the Amazon Elastic Compute Cloud
-- -- User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StartInstances.html>

-- data StartInstances = StartInstances
--     { siInstanceId :: Members Text
--       -- ^ One or more instance IDs.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery StartInstances

-- instance AWSRequest EC2 StartInstances StartInstancesResponse where
--     request = query4 ec2 GET "StartInstances"

-- data StartInstancesResponse = StartInstancesResponse
--     { siRequestId    :: !Text
--       -- ^ The ID of the request.
--     , siInstancesSet :: !InstanceStateChangeType
--       -- ^ A list of instance state changes. Each change is wrapped in an
--       -- item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML StartInstancesResponse where
--     xmlPickler = ec2XML

-- -- | Stops an Amazon EBS-backed instance. Each time you transition an instance
-- -- from stopped to started, we charge a full instance hour, even if
-- -- transitions happen multiple times within a single hour.You can't start or
-- -- stop Spot Instances.Instances that use Amazon EBS volumes as their root
-- -- devices can be quickly stopped and started. When an instance is stopped,
-- -- the compute resources are released and you are not billed for hourly
-- -- instance usage. However, your root partition Amazon EBS volume remains,
-- -- continues to persist your data, and you are charged for Amazon EBS volume
-- -- usage. You can restart your instance at any time. Before stopping an
-- -- instance, make sure it is in a state from which it can be restarted.
-- -- Stopping an instance does not preserve data stored in RAM. Performing this
-- -- operation on an instance that uses an instance store as its root device
-- -- returns an error.You can stop, start, and terminate EBS-backed instances.
-- -- You can only terminate S3-backed instances. What happens to an instance
-- -- differs if you stop it or terminate it. For example, when you stop an
-- -- instance, the root device and any other devices attached to the instance
-- -- persist. When you terminate an instance, the root device and any other
-- -- devices attached during the instance launch are automatically deleted. For
-- -- more information about the differences between stopping and terminating
-- -- instances, see Stopping Instances in the Amazon Elastic Compute Cloud User
-- -- Guide
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StopInstances.html>

-- data StopInstances = StopInstances
--     { sjInstanceId :: Members Text
--       -- ^ One or more instance IDs.
--     , sjForce      :: Maybe Bool
--       -- ^ Forces the instances to stop. The instances will not have an
--       -- opportunity to flush file system caches or file system metadata.
--       -- If you use this option, you must perform file system check and
--       -- repair procedures. This option is not recommended for Windows
--       -- instances.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery StopInstances

-- instance AWSRequest EC2 StopInstances StopInstancesResponse where
--     request = query4 ec2 GET "StopInstances"

-- data StopInstancesResponse = StopInstancesResponse
--     { sjRequestId    :: !Text
--       -- ^ The ID of the request.
--     , sjInstancesSet :: !InstanceStateChangeType
--       -- ^ A list of instance state changes. Each change is wrapped in an
--       -- item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML StopInstancesResponse where
--     xmlPickler = ec2XML

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
    { tiInstanceId :: [Text]
      -- ^ One or more instance IDs.
    } deriving (Eq, Show, Generic)

instance IsQuery TerminateInstances

instance Rq TerminateInstances where
    type Er TerminateInstances = EC2ErrorResponse
    type Rs TerminateInstances = TerminateInstancesResponse
    request = query4 ec2 GET "TerminateInstances"

data TerminateInstancesResponse = TerminateInstancesResponse
    { tiRequestId    :: !Text
      -- ^ The ID of the request.
    , tiInstancesSet :: !InstanceStateChangeType
      -- ^ A list of instance state changes. Each change is wrapped in an
      -- item element.
    } deriving (Eq, Show, Generic)

instance IsXML TerminateInstancesResponse where
    xmlPickler = withNS ec2NS

-- instance IsXML TerminateInstancesResponse where
--     xmlPickler = ec2XML

-- -- | Unassigns one or more secondary private IP addresses from a network
-- -- interface.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnassignPrivateIpAddresses.html>

-- data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
--     { upiaNetworkInterfaceId :: !Text
--       -- ^ The network interface from which the secondary private IP address
--       -- will be unassigned.
--     , upiaPrivateIpAddress   :: Members AssignPrivateIpAddressesSetItemRequestType
--       -- ^ The secondary private IP addresses that you want to unassign from
--       -- the network interface. You can specify this option multiple times
--       -- to unassign more than one IP address.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery UnassignPrivateIpAddresses

-- instance AWSRequest EC2 UnassignPrivateIpAddresses UnassignPrivateIpAddressesResponse where
--     request = query4 ec2 GET "UnassignPrivateIpAddresses"

-- data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
--     { upiaRequestId :: !Text
--       -- ^ The ID of the request.
--     , upiaReturn    :: !Bool
--       -- ^ Returns true if the request succeeds. Otherwise, returns an
--       -- error.
--     } deriving (Eq, Show, Generic)

-- instance IsXML UnassignPrivateIpAddressesResponse where
--     xmlPickler = ec2XML

-- -- | Disables monitoring for a running instance. For more information about
-- -- monitoring instances, see Monitoring Your Instances and Volumes in the
-- -- Amazon Elastic Compute Cloud User Guide.
-- --
-- -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnmonitorInstances.html>

-- data UnmonitorInstances = UnmonitorInstances
--     { uiInstanceId :: Members Text
--       -- ^ One or more instance IDs.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery UnmonitorInstances

-- instance AWSRequest EC2 UnmonitorInstances UnmonitorInstancesResponse where
--     request = query4 ec2 GET "UnmonitorInstances"

-- data UnmonitorInstancesResponse = UnmonitorInstancesResponse
--     { uiRequestId    :: !Text
--       -- ^ The ID of the request.
--     , uiInstancesSet :: !MonitorInstancesResponseSetItemType
--       -- ^ A list of monitoring information for one or more instances. Each
--       -- set of information is wrapped in an item element.
--     } deriving (Eq, Show, Generic)

-- instance IsXML UnmonitorInstancesResponse where
--     xmlPickler = ec2XML
