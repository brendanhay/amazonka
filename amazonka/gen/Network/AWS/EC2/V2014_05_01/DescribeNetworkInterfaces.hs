{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DescribeNetworkInterfaces
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your network interfaces. Example This example
-- describes all your network interfaces.
-- https://ec2.amazonaws.com/?Action=DescribeNetworkInterfaces &amp;AUTHPARAMS
-- &lt;DescribeNetworkInterfacesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
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
module Network.AWS.EC2.V2014_05_01.DescribeNetworkInterfaces where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { _dnisDryRun :: Maybe Bool
      -- ^ 
    , _dnisFilters :: [Filter]
      -- ^ One or more filters. addresses.private-ip-address - The private
      -- IP addresses associated with the network interface.
      -- addresses.primary - Whether the private IP address is the primary
      -- IP address associated with the network interface.
      -- addresses.association.public-ip - The association ID returned
      -- when the network interface was associated with the Elastic IP
      -- address. addresses.association.owner-id - The owner ID of the
      -- addresses associated with the network interface.
      -- association.association-id - The association ID returned when the
      -- network interface was associated with an IP address.
      -- association.allocation-id - The allocation ID returned when you
      -- allocated the Elastic IP address for your network interface.
      -- association.ip-owner-id - The owner of the Elastic IP address
      -- associated with the network interface. association.public-ip -
      -- The address of the Elastic IP address bound to the network
      -- interface. association.public-dns-name - The public DNS name for
      -- the network interface. attachment.attachment-id - The ID of the
      -- interface attachment. attachment.instance-id - The ID of the
      -- instance to which the network interface is attached.
      -- attachment.instance-owner-id - The owner ID of the instance to
      -- which the network interface is attached. attachment.device-index
      -- - The device index to which the network interface is attached.
      -- attachment.status - The status of the attachment (attaching |
      -- attached | detaching | detached). attachment.attach.time - The
      -- time that the network interface was attached to an instance.
      -- attachment.delete-on-termination - Indicates whether the
      -- attachment is deleted when an instance is terminated.
      -- availability-zone - The Availability Zone of the network
      -- interface. description - The description of the network
      -- interface. group-id - The ID of a security group associated with
      -- the network interface. group-name - The name of a security group
      -- associated with the network interface. mac-address - The MAC
      -- address of the network interface. network-interface-id - The ID
      -- of the network interface. owner-id - The AWS account ID of the
      -- network interface owner. private-ip-address - The private IP
      -- address or addresses of the network interface. private-dns-name -
      -- The private DNS name of the network interface. requester-id - The
      -- ID of the entity that launched the instance on your behalf (for
      -- example, AWS Management Console, Auto Scaling, and so on).
      -- requester-managed - Indicates whether the network interface is
      -- being managed by an AWS service (for example, AWS Management
      -- Console, Auto Scaling, and so on). source-desk-check - Indicates
      -- whether the network interface performs source/destination
      -- checking. A value of true means checking is enabled, and false
      -- means checking is disabled. The value must be false for the
      -- network interface to perform Network Address Translation (NAT) in
      -- your VPC. status - The status of the network interface. If the
      -- network interface is not attached to an instance, the status is
      -- available; if a network interface is attached to an instance the
      -- status is in-use. subnet-id - The ID of the subnet for the
      -- network interface. tag:key=value - The key/value combination of a
      -- tag assigned to the resource. tag-key - The key of a tag assigned
      -- to the resource. This filter is independent of the tag-value
      -- filter. For example, if you use both the filter "tag-key=Purpose"
      -- and the filter "tag-value=X", you get any resources assigned both
      -- the tag key Purpose (regardless of what the tag's value is), and
      -- the tag value X (regardless of what the tag's key is). If you
      -- want to list only resources where Purpose is X, see the
      -- tag:key=value filter. tag-value - The value of a tag assigned to
      -- the resource. This filter is independent of the tag-key filter.
      -- vpc-id - The ID of the VPC for the network interface.
    , _dnisNetworkInterfaceIds :: [Text]
      -- ^ One or more network interface IDs. Default: Describes all your
      -- network interfaces.
    } deriving (Generic)

instance ToQuery DescribeNetworkInterfaces where
    toQuery = genericToQuery def

instance AWSRequest DescribeNetworkInterfaces where
    type Sv DescribeNetworkInterfaces = EC2
    type Rs DescribeNetworkInterfaces = DescribeNetworkInterfacesResponse

    request = post "DescribeNetworkInterfaces"
    response _ = xmlResponse

data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
    { _dnitNetworkInterfaces :: [NetworkInterface]
      -- ^ Information about one or more network interfaces.
    } deriving (Generic)

instance FromXML DescribeNetworkInterfacesResponse where
    fromXMLOptions = xmlOptions
