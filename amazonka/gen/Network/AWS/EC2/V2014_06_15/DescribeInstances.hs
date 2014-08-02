{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your instances. If you specify one or more
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
module Network.AWS.EC2.V2014_06_15.DescribeInstances where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeInstances' request.
describeInstances :: DescribeInstances
describeInstances = DescribeInstances
    { _disDryRun = Nothing
    , _disFilters = mempty
    , _disInstanceIds = mempty
    , _disMaxResults = Nothing
    , _disNextToken = Nothing
    }

data DescribeInstances = DescribeInstances
    { _disDryRun :: Maybe Bool
      -- ^ 
    , _disFilters :: [Filter]
      -- ^ One or more filters. architecture - The instance architecture
      -- (i386 | x86_64). availability-zone - The Availability Zone of the
      -- instance. block-device-mapping.attach-time - The attach time for
      -- an Amazon EBS volume mapped to the instance.
      -- block-device-mapping.delete-on-termination - A Boolean that
      -- indicates whether the Amazon EBS volume is deleted on instance
      -- termination. block-device-mapping.device-name - The device name
      -- for the Amazon EBS volume (for example, /dev/sdh).
      -- block-device-mapping.status - The status for the Amazon EBS
      -- volume (attaching | attached | detaching | detached).
      -- block-device-mapping.volume-id - The volume ID of the Amazon EBS
      -- volume. client-token - The idempotency token you provided when
      -- you launched the instance. dns-name - The public DNS name of the
      -- instance. group-id - The ID of the security group for the
      -- instance. If the instance is in EC2-Classic or a default VPC, you
      -- can use group-name instead. group-name - The name of the security
      -- group for the instance. If the instance is in a nondefault VPC,
      -- you must use group-id instead. hypervisor - The hypervisor type
      -- of the instance (ovm | xen). iam-instance-profile.arn - The
      -- instance profile associated with the instance. Specified as an
      -- ARN. image-id - The ID of the image used to launch the instance.
      -- instance-id - The ID of the instance. instance-lifecycle -
      -- Indicates whether this is a Spot Instance (spot).
      -- instance-state-code - The state of the instance, as a 16-bit
      -- unsigned integer. The high byte is an opaque internal value and
      -- should be ignored. The low byte is set based on the state
      -- represented. The valid values are: 0 (pending), 16 (running), 32
      -- (shutting-down), 48 (terminated), 64 (stopping), and 80
      -- (stopped). instance-state-name - The state of the instance
      -- (pending | running | shutting-down | terminated | stopping |
      -- stopped). instance-type - The type of instance (for example,
      -- m1.small). instance.group-id - The ID of the security group for
      -- the instance. If the instance is in EC2-Classic or a default VPC,
      -- you can use instance.group-name instead. instance.group-name -
      -- The name of the security group for the instance. If the instance
      -- is in a nondefault VPC, you must use instance.group-id instead.
      -- ip-address - The public IP address of the instance. kernel-id -
      -- The kernel ID. key-name - The name of the key pair used when the
      -- instance was launched. launch-index - When launching multiple
      -- instances, this is the index for the instance in the launch group
      -- (for example, 0, 1, 2, and so on). launch-time - The time when
      -- the instance was launched. monitoring-state - Indicates whether
      -- monitoring is enabled for the instance (disabled | enabled).
      -- owner-id - The AWS account ID of the instance owner.
      -- placement-group-name - The name of the placement group for the
      -- instance. platform - The platform. Use windows if you have
      -- Windows instances; otherwise, leave blank. private-dns-name - The
      -- private DNS name of the instance. private-ip-address - The
      -- private IP address of the instance. product-code - The product
      -- code associated with the AMI used to launch the instance.
      -- product-code.type - The type of product code (devpay |
      -- marketplace). ramdisk-id - The RAM disk ID. reason - The reason
      -- for the current state of the instance (for example, shows "User
      -- Initiated [date]" when you stop or terminate the instance).
      -- Similar to the state-reason-code filter. requester-id - The ID of
      -- the entity that launched the instance on your behalf (for
      -- example, AWS Management Console, Auto Scaling, and so on).
      -- reservation-id - The ID of the instance's reservation. A
      -- reservation ID is created any time you launch an instance. A
      -- reservation ID has a one-to-one relationship with an instance
      -- launch request, but can be associated with more than one instance
      -- if you launch multiple instances using the same launch request.
      -- For example, if you launch one instance, you'll get one
      -- reservation ID. If you launch ten instances using the same launch
      -- request, you'll also get one reservation ID. root-device-name -
      -- The name of the root device for the instance (for example,
      -- /dev/sda1). root-device-type - The type of root device that the
      -- instance uses (ebs | instance-store). source-dest-check -
      -- Indicates whether the instance performs source/destination
      -- checking. A value of true means that checking is enabled, and
      -- false means checking is disabled. The value must be false for the
      -- instance to perform network address translation (NAT) in your
      -- VPC. spot-instance-request-id - The ID of the Spot Instance
      -- request. state-reason-code - The reason code for the state
      -- change. state-reason-message - A message that describes the state
      -- change. subnet-id - The ID of the subnet for the instance.
      -- tag:key=value - The key/value combination of a tag assigned to
      -- the resource, where tag:key is the tag's key. tag-key - The key
      -- of a tag assigned to the resource. This filter is independent of
      -- the tag-value filter. For example, if you use both the filter
      -- "tag-key=Purpose" and the filter "tag-value=X", you get any
      -- resources assigned both the tag key Purpose (regardless of what
      -- the tag's value is), and the tag value X (regardless of what the
      -- tag's key is). If you want to list only resources where Purpose
      -- is X, see the tag:key=value filter. tag-value - The value of a
      -- tag assigned to the resource. This filter is independent of the
      -- tag-key filter. tenancy - The tenancy of an instance (dedicated |
      -- default). virtualization-type - The virtualization type of the
      -- instance (paravirtual | hvm). vpc-id - The ID of the VPC that the
      -- instance is running in. network-interface.description - The
      -- description of the network interface. network-interface.subnet-id
      -- - The ID of the subnet for the network interface.
      -- network-interface.vpc-id - The ID of the VPC for the network
      -- interface. network-interface.network-interface.id - The ID of the
      -- network interface. network-interface.owner-id - The ID of the
      -- owner of the network interface.
      -- network-interface.availability-zone - The Availability Zone for
      -- the network interface. network-interface.requester-id - The
      -- requester ID for the network interface.
      -- network-interface.requester-managed - Indicates whether the
      -- network interface is being managed by AWS.
      -- network-interface.status - The status of the network interface
      -- (available) | in-use). network-interface.mac-address - The MAC
      -- address of the network interface.
      -- network-interface-private-dns-name - The private DNS name of the
      -- network interface. network-interface.source-destination-check -
      -- Whether the network interface performs source/destination
      -- checking. A value of true means checking is enabled, and false
      -- means checking is disabled. The value must be false for the
      -- network interface to perform network address translation (NAT) in
      -- your VPC. network-interface.group-id - The ID of a security group
      -- associated with the network interface.
      -- network-interface.group-name - The name of a security group
      -- associated with the network interface.
      -- network-interface.attachment.attachment-id - The ID of the
      -- interface attachment. network-interface.attachment.instance-id -
      -- The ID of the instance to which the network interface is
      -- attached. network-interface.attachment.instance-owner-id - The
      -- owner ID of the instance to which the network interface is
      -- attached. network-interface.addresses.private-ip-address - The
      -- private IP address associated with the network interface.
      -- network-interface.attachment.device-index - The device index to
      -- which the network interface is attached.
      -- network-interface.attachment.status - The status of the
      -- attachment (attaching | attached | detaching | detached).
      -- network-interface.attachment.attach-time - The time that the
      -- network interface was attached to an instance.
      -- network-interface.attachment.delete-on-termination - Specifies
      -- whether the attachment is deleted when an instance is terminated.
      -- network-interface.addresses.primary - Specifies whether the IP
      -- address of the network interface is the primary private IP
      -- address. network-interface.addresses.association.public-ip - The
      -- ID of the association of an Elastic IP address with a network
      -- interface. network-interface.addresses.association.ip-owner-id -
      -- The owner ID of the private IP address associated with the
      -- network interface. association.public-ip - The address of the
      -- Elastic IP address bound to the network interface.
      -- association.ip-owner-id - The owner of the Elastic IP address
      -- associated with the network interface. association.allocation-id
      -- - The allocation ID returned when you allocated the Elastic IP
      -- address for your network interface. association.association-id -
      -- The association ID returned when the network interface was
      -- associated with an IP address.
    , _disInstanceIds :: [Text]
      -- ^ One or more instance IDs. Default: Describes all your instances.
    , _disMaxResults :: Maybe Integer
      -- ^ The maximum number of items to return for this call. The call
      -- also returns a token that you can specify in a subsequent call to
      -- get the next set of results. If the value is greater than 1000,
      -- we return only 1000 items.
    , _disNextToken :: Maybe Text
      -- ^ The token for the next set of items to return. (You received this
      -- token from a prior call.).
    } deriving (Generic)

makeLenses ''DescribeInstances

instance ToQuery DescribeInstances where
    toQuery = genericToQuery def

data DescribeInstancesResponse = DescribeInstancesResponse
    { _ditReservations :: [Reservation]
      -- ^ One or more reservations.
    , _ditNextToken :: Maybe Text
      -- ^ The token to use when requesting the next set of items. If there
      -- are no additional items to return, the string is empty.
    } deriving (Generic)

makeLenses ''DescribeInstancesResponse

instance FromXML DescribeInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeInstances where
    type Sv DescribeInstances = EC2
    type Rs DescribeInstances = DescribeInstancesResponse

    request = post "DescribeInstances"
    response _ = xmlResponse

instance AWSPager DescribeInstances where
    next rq rs = (\x -> rq { _disNextToken = Just x })
        <$> (_ditNextToken rs)
