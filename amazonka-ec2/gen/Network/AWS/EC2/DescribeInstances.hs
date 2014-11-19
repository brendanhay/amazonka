{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeInstances
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
-- hour.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstances.html>
module Network.AWS.EC2.DescribeInstances
    (
    -- * Request
      DescribeInstances
    -- ** Request constructor
    , describeInstances
    -- ** Request lenses
    , di1DryRun
    , di1Filters
    , di1InstanceIds
    , di1MaxResults
    , di1NextToken

    -- * Response
    , DescribeInstancesResponse
    -- ** Response constructor
    , describeInstancesResponse
    -- ** Response lenses
    , dirNextToken
    , dirReservations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeInstances = DescribeInstances
    { _di1DryRun      :: Maybe Bool
    , _di1Filters     :: List "Filter" Filter
    , _di1InstanceIds :: List "InstanceId" Text
    , _di1MaxResults  :: Maybe Int
    , _di1NextToken   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'di1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'di1Filters' @::@ ['Filter']
--
-- * 'di1InstanceIds' @::@ ['Text']
--
-- * 'di1MaxResults' @::@ 'Maybe' 'Int'
--
-- * 'di1NextToken' @::@ 'Maybe' 'Text'
--
describeInstances :: DescribeInstances
describeInstances = DescribeInstances
    { _di1DryRun      = Nothing
    , _di1InstanceIds = mempty
    , _di1Filters     = mempty
    , _di1NextToken   = Nothing
    , _di1MaxResults  = Nothing
    }

di1DryRun :: Lens' DescribeInstances (Maybe Bool)
di1DryRun = lens _di1DryRun (\s a -> s { _di1DryRun = a })

-- | One or more filters. architecture - The instance architecture (i386 |
-- x86_64). availability-zone - The Availability Zone of the instance.
-- block-device-mapping.attach-time - The attach time for an Amazon EBS
-- volume mapped to the instance, for example, 2010-09-15T17:15:20.000Z.
-- block-device-mapping.delete-on-termination - A Boolean that indicates
-- whether the Amazon EBS volume is deleted on instance termination.
-- block-device-mapping.device-name - The device name for the Amazon EBS
-- volume (for example, /dev/sdh). block-device-mapping.status - The status
-- for the Amazon EBS volume (attaching | attached | detaching | detached).
-- block-device-mapping.volume-id - The volume ID of the Amazon EBS volume.
-- client-token - The idempotency token you provided when you launched the
-- instance. dns-name - The public DNS name of the instance. group-id - The
-- ID of the security group for the instance. If the instance is in
-- EC2-Classic or a default VPC, you can use group-name instead. group-name
-- - The name of the security group for the instance. If the instance is in
-- a nondefault VPC, you must use group-id instead. hypervisor - The
-- hypervisor type of the instance (ovm | xen). iam-instance-profile.arn -
-- The instance profile associated with the instance. Specified as an ARN.
-- image-id - The ID of the image used to launch the instance. instance-id -
-- The ID of the instance. instance-lifecycle - Indicates whether this is a
-- Spot Instance (spot). instance-state-code - The state of the instance, as
-- a 16-bit unsigned integer. The high byte is an opaque internal value and
-- should be ignored. The low byte is set based on the state represented.
-- The valid values are: 0 (pending), 16 (running), 32 (shutting-down), 48
-- (terminated), 64 (stopping), and 80 (stopped). instance-state-name - The
-- state of the instance (pending | running | shutting-down | terminated |
-- stopping | stopped). instance-type - The type of instance (for example,
-- m1.small). instance.group-id - The ID of the security group for the
-- instance. If the instance is in EC2-Classic or a default VPC, you can use
-- instance.group-name instead. instance.group-name - The name of the
-- security group for the instance. If the instance is in a nondefault VPC,
-- you must use instance.group-id instead. ip-address - The public IP
-- address of the instance. kernel-id - The kernel ID. key-name - The name
-- of the key pair used when the instance was launched. launch-index - When
-- launching multiple instances, this is the index for the instance in the
-- launch group (for example, 0, 1, 2, and so on). launch-time - The time
-- when the instance was launched. monitoring-state - Indicates whether
-- monitoring is enabled for the instance (disabled | enabled). owner-id -
-- The AWS account ID of the instance owner. placement-group-name - The name
-- of the placement group for the instance. platform - The platform. Use
-- windows if you have Windows instances; otherwise, leave blank.
-- private-dns-name - The private DNS name of the instance.
-- private-ip-address - The private IP address of the instance. product-code
-- - The product code associated with the AMI used to launch the instance.
-- product-code.type - The type of product code (devpay | marketplace).
-- ramdisk-id - The RAM disk ID. reason - The reason for the current state
-- of the instance (for example, shows "User Initiated [date]" when you stop
-- or terminate the instance). Similar to the state-reason-code filter.
-- requester-id - The ID of the entity that launched the instance on your
-- behalf (for example, AWS Management Console, Auto Scaling, and so on).
-- reservation-id - The ID of the instance's reservation. A reservation ID
-- is created any time you launch an instance. A reservation ID has a
-- one-to-one relationship with an instance launch request, but can be
-- associated with more than one instance if you launch multiple instances
-- using the same launch request. For example, if you launch one instance,
-- you'll get one reservation ID. If you launch ten instances using the same
-- launch request, you'll also get one reservation ID. root-device-name -
-- The name of the root device for the instance (for example, /dev/sda1).
-- root-device-type - The type of root device that the instance uses (ebs |
-- instance-store). source-dest-check - Indicates whether the instance
-- performs source/destination checking. A value of true means that checking
-- is enabled, and false means checking is disabled. The value must be false
-- for the instance to perform network address translation (NAT) in your
-- VPC. spot-instance-request-id - The ID of the Spot Instance request.
-- state-reason-code - The reason code for the state change.
-- state-reason-message - A message that describes the state change.
-- subnet-id - The ID of the subnet for the instance. tag:key=value - The
-- key/value combination of a tag assigned to the resource, where tag:key is
-- the tag's key. tag-key - The key of a tag assigned to the resource. This
-- filter is independent of the tag-value filter. For example, if you use
-- both the filter "tag-key=Purpose" and the filter "tag-value=X", you get
-- any resources assigned both the tag key Purpose (regardless of what the
-- tag's value is), and the tag value X (regardless of what the tag's key
-- is). If you want to list only resources where Purpose is X, see the
-- tag:key=value filter. tag-value - The value of a tag assigned to the
-- resource. This filter is independent of the tag-key filter. tenancy - The
-- tenancy of an instance (dedicated | default). virtualization-type - The
-- virtualization type of the instance (paravirtual | hvm). vpc-id - The ID
-- of the VPC that the instance is running in. network-interface.description
-- - The description of the network interface. network-interface.subnet-id -
-- The ID of the subnet for the network interface. network-interface.vpc-id
-- - The ID of the VPC for the network interface.
-- network-interface.network-interface.id - The ID of the network interface.
-- network-interface.owner-id - The ID of the owner of the network
-- interface. network-interface.availability-zone - The Availability Zone
-- for the network interface. network-interface.requester-id - The requester
-- ID for the network interface. network-interface.requester-managed -
-- Indicates whether the network interface is being managed by AWS.
-- network-interface.status - The status of the network interface
-- (available) | in-use). network-interface.mac-address - The MAC address of
-- the network interface. network-interface-private-dns-name - The private
-- DNS name of the network interface.
-- network-interface.source-destination-check - Whether the network
-- interface performs source/destination checking. A value of true means
-- checking is enabled, and false means checking is disabled. The value must
-- be false for the network interface to perform network address translation
-- (NAT) in your VPC. network-interface.group-id - The ID of a security
-- group associated with the network interface. network-interface.group-name
-- - The name of a security group associated with the network interface.
-- network-interface.attachment.attachment-id - The ID of the interface
-- attachment. network-interface.attachment.instance-id - The ID of the
-- instance to which the network interface is attached.
-- network-interface.attachment.instance-owner-id - The owner ID of the
-- instance to which the network interface is attached.
-- network-interface.addresses.private-ip-address - The private IP address
-- associated with the network interface.
-- network-interface.attachment.device-index - The device index to which the
-- network interface is attached. network-interface.attachment.status - The
-- status of the attachment (attaching | attached | detaching | detached).
-- network-interface.attachment.attach-time - The time that the network
-- interface was attached to an instance.
-- network-interface.attachment.delete-on-termination - Specifies whether
-- the attachment is deleted when an instance is terminated.
-- network-interface.addresses.primary - Specifies whether the IP address of
-- the network interface is the primary private IP address.
-- network-interface.addresses.association.public-ip - The ID of the
-- association of an Elastic IP address with a network interface.
-- network-interface.addresses.association.ip-owner-id - The owner ID of the
-- private IP address associated with the network interface.
-- association.public-ip - The address of the Elastic IP address bound to
-- the network interface. association.ip-owner-id - The owner of the Elastic
-- IP address associated with the network interface.
-- association.allocation-id - The allocation ID returned when you allocated
-- the Elastic IP address for your network interface.
-- association.association-id - The association ID returned when the network
-- interface was associated with an IP address.
di1Filters :: Lens' DescribeInstances [Filter]
di1Filters = lens _di1Filters (\s a -> s { _di1Filters = a })
    . _List

-- | One or more instance IDs. Default: Describes all your instances.
di1InstanceIds :: Lens' DescribeInstances [Text]
di1InstanceIds = lens _di1InstanceIds (\s a -> s { _di1InstanceIds = a })
    . _List

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the next
-- set of results. If the value is greater than 1000, we return only 1000
-- items.
di1MaxResults :: Lens' DescribeInstances (Maybe Int)
di1MaxResults = lens _di1MaxResults (\s a -> s { _di1MaxResults = a })

-- | The token for the next set of items to return. (You received this token
-- from a prior call.).
di1NextToken :: Lens' DescribeInstances (Maybe Text)
di1NextToken = lens _di1NextToken (\s a -> s { _di1NextToken = a })

data DescribeInstancesResponse = DescribeInstancesResponse
    { _dirNextToken    :: Maybe Text
    , _dirReservations :: List "item" Reservation
    } deriving (Eq, Show, Generic)

-- | 'DescribeInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dirReservations' @::@ ['Reservation']
--
describeInstancesResponse :: DescribeInstancesResponse
describeInstancesResponse = DescribeInstancesResponse
    { _dirReservations = mempty
    , _dirNextToken    = Nothing
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dirNextToken :: Lens' DescribeInstancesResponse (Maybe Text)
dirNextToken = lens _dirNextToken (\s a -> s { _dirNextToken = a })

-- | One or more reservations.
dirReservations :: Lens' DescribeInstancesResponse [Reservation]
dirReservations = lens _dirReservations (\s a -> s { _dirReservations = a })
    . _List

instance ToPath DescribeInstances where
    toPath = const "/"

instance ToQuery DescribeInstances

instance ToHeaders DescribeInstances

instance AWSRequest DescribeInstances where
    type Sv DescribeInstances = EC2
    type Rs DescribeInstances = DescribeInstancesResponse

    request  = post "DescribeInstances"
    response = xmlResponse

instance FromXML DescribeInstancesResponse where
    parseXML x = DescribeInstancesResponse
        <$> x .@? "nextToken"
        <*> x .@ "reservationSet"

instance AWSPager DescribeInstances where
    next rq rs = (\x -> rq & di1NextToken ?~ x)
        <$> (rs ^. dirNextToken)
