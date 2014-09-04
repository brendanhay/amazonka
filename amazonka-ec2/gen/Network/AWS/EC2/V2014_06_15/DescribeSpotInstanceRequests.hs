{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeSpotInstanceRequests
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the Spot Instance requests that belong to your account. Spot
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
module Network.AWS.EC2.V2014_06_15.DescribeSpotInstanceRequests
    (
    -- * Request
      DescribeSpotInstanceRequests
    -- ** Request constructor
    , mkDescribeSpotInstanceRequestsRequest
    -- ** Request lenses
    , dsirrSpotInstanceRequestIds
    , dsirrFilters

    -- * Response
    , DescribeSpotInstanceRequestsResponse
    -- ** Response lenses
    , dsirsSpotInstanceRequests
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSpotInstanceRequests' request.
mkDescribeSpotInstanceRequestsRequest :: DescribeSpotInstanceRequests
mkDescribeSpotInstanceRequestsRequest = DescribeSpotInstanceRequests
    { _dsirrSpotInstanceRequestIds = mempty
    , _dsirrFilters = mempty
    }
{-# INLINE mkDescribeSpotInstanceRequestsRequest #-}

data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests
    { _dsirrSpotInstanceRequestIds :: [Text]
      -- ^ One or more Spot Instance request IDs.
    , _dsirrFilters :: [Filter]
      -- ^ One or more filters. availability-zone-group - The Availability
      -- Zone group. create-time - The time stamp when the Spot Instance
      -- request was created. fault-code - The fault code related to the
      -- request. fault-message - The fault message related to the
      -- request. instance-id - The ID of the instance that fulfilled the
      -- request. launch-group - The Spot Instance launch group.
      -- launch.block-device-mapping.delete-on-termination - Indicates
      -- whether the Amazon EBS volume is deleted on instance termination.
      -- launch.block-device-mapping.device-name - The device name for the
      -- Amazon EBS volume (for example, /dev/sdh).
      -- launch.block-device-mapping.snapshot-id - The ID of the snapshot
      -- used for the Amazon EBS volume.
      -- launch.block-device-mapping.volume-size - The size of the Amazon
      -- EBS volume, in GiB. launch.block-device-mapping.volume-type - The
      -- type of the Amazon EBS volume (gp2 | standard | io1).
      -- launch.group-id - The security group for the instance.
      -- launch.image-id - The ID of the AMI. launch.instance-type - The
      -- type of instance (for example, m1.small). launch.kernel-id - The
      -- kernel ID. launch.key-name - The name of the key pair the
      -- instance launched with. launch.monitoring-enabled - Whether
      -- monitoring is enabled for the Spot Instance. launch.ramdisk-id -
      -- The RAM disk ID. launch.network-interface.network-interface-id -
      -- The ID of the network interface.
      -- launch.network-interface.device-index - The index of the device
      -- for the network interface attachment on the instance.
      -- launch.network-interface.subnet-id - The ID of the subnet for the
      -- instance. launch.network-interface.description - A description of
      -- the network interface.
      -- launch.network-interface.private-ip-address - The primary private
      -- IP address of the network interface.
      -- launch.network-interface.delete-on-termination - Indicates
      -- whether the network interface is deleted when the instance is
      -- terminated. launch.network-interface.group-id - The ID of the
      -- security group associated with the network interface.
      -- launch.network-interface.group-name - The name of the security
      -- group associated with the network interface.
      -- launch.network-interface.addresses.primary - Indicates whether
      -- the IP address is the primary private IP address.
      -- product-description - The product description associated with the
      -- instance (Linux/UNIX | Windows). spot-instance-request-id - The
      -- Spot Instance request ID. spot-price - The maximum hourly price
      -- for any Spot Instance launched to fulfill the request. state -
      -- The state of the Spot Instance request (open | active | closed |
      -- cancelled | failed). status-code - The short code describing the
      -- most recent evaluation of your Spot Instance request.
      -- status-message - The message explaining the status of the Spot
      -- Instance request. tag:key=value - The key/value combination of a
      -- tag assigned to the resource. tag-key - The key of a tag assigned
      -- to the resource. This filter is independent of the tag-value
      -- filter. For example, if you use both the filter "tag-key=Purpose"
      -- and the filter "tag-value=X", you get any resources assigned both
      -- the tag key Purpose (regardless of what the tag's value is), and
      -- the tag value X (regardless of what the tag's key is). If you
      -- want to list only resources where Purpose is X, see the
      -- tag:key=value filter. tag-value - The value of a tag assigned to
      -- the resource. This filter is independent of the tag-key filter.
      -- type - The type of Spot Instance request (one-time | persistent).
      -- launched-availability-zone - The Availability Zone in which the
      -- bid is launched. valid-from - The start date of the request.
      -- valid-until - The end date of the request.
    } deriving (Show, Generic)

-- | One or more Spot Instance request IDs.
dsirrSpotInstanceRequestIds :: Lens' DescribeSpotInstanceRequests ([Text])
dsirrSpotInstanceRequestIds = lens _dsirrSpotInstanceRequestIds (\s a -> s { _dsirrSpotInstanceRequestIds = a })
{-# INLINE dsirrSpotInstanceRequestIds #-}

-- | One or more filters. availability-zone-group - The Availability Zone group.
-- create-time - The time stamp when the Spot Instance request was created.
-- fault-code - The fault code related to the request. fault-message - The
-- fault message related to the request. instance-id - The ID of the instance
-- that fulfilled the request. launch-group - The Spot Instance launch group.
-- launch.block-device-mapping.delete-on-termination - Indicates whether the
-- Amazon EBS volume is deleted on instance termination.
-- launch.block-device-mapping.device-name - The device name for the Amazon
-- EBS volume (for example, /dev/sdh). launch.block-device-mapping.snapshot-id
-- - The ID of the snapshot used for the Amazon EBS volume.
-- launch.block-device-mapping.volume-size - The size of the Amazon EBS
-- volume, in GiB. launch.block-device-mapping.volume-type - The type of the
-- Amazon EBS volume (gp2 | standard | io1). launch.group-id - The security
-- group for the instance. launch.image-id - The ID of the AMI.
-- launch.instance-type - The type of instance (for example, m1.small).
-- launch.kernel-id - The kernel ID. launch.key-name - The name of the key
-- pair the instance launched with. launch.monitoring-enabled - Whether
-- monitoring is enabled for the Spot Instance. launch.ramdisk-id - The RAM
-- disk ID. launch.network-interface.network-interface-id - The ID of the
-- network interface. launch.network-interface.device-index - The index of the
-- device for the network interface attachment on the instance.
-- launch.network-interface.subnet-id - The ID of the subnet for the instance.
-- launch.network-interface.description - A description of the network
-- interface. launch.network-interface.private-ip-address - The primary
-- private IP address of the network interface.
-- launch.network-interface.delete-on-termination - Indicates whether the
-- network interface is deleted when the instance is terminated.
-- launch.network-interface.group-id - The ID of the security group associated
-- with the network interface. launch.network-interface.group-name - The name
-- of the security group associated with the network interface.
-- launch.network-interface.addresses.primary - Indicates whether the IP
-- address is the primary private IP address. product-description - The
-- product description associated with the instance (Linux/UNIX | Windows).
-- spot-instance-request-id - The Spot Instance request ID. spot-price - The
-- maximum hourly price for any Spot Instance launched to fulfill the request.
-- state - The state of the Spot Instance request (open | active | closed |
-- cancelled | failed). status-code - The short code describing the most
-- recent evaluation of your Spot Instance request. status-message - The
-- message explaining the status of the Spot Instance request. tag:key=value -
-- The key/value combination of a tag assigned to the resource. tag-key - The
-- key of a tag assigned to the resource. This filter is independent of the
-- tag-value filter. For example, if you use both the filter "tag-key=Purpose"
-- and the filter "tag-value=X", you get any resources assigned both the tag
-- key Purpose (regardless of what the tag's value is), and the tag value X
-- (regardless of what the tag's key is). If you want to list only resources
-- where Purpose is X, see the tag:key=value filter. tag-value - The value of
-- a tag assigned to the resource. This filter is independent of the tag-key
-- filter. type - The type of Spot Instance request (one-time | persistent).
-- launched-availability-zone - The Availability Zone in which the bid is
-- launched. valid-from - The start date of the request. valid-until - The end
-- date of the request.
dsirrFilters :: Lens' DescribeSpotInstanceRequests ([Filter])
dsirrFilters = lens _dsirrFilters (\s a -> s { _dsirrFilters = a })
{-# INLINE dsirrFilters #-}

instance ToQuery DescribeSpotInstanceRequests where
    toQuery = genericQuery def

newtype DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse
    { _dsirsSpotInstanceRequests :: [SpotInstanceRequest]
      -- ^ One or more Spot Instance requests.
    } deriving (Show, Generic)

-- | One or more Spot Instance requests.
dsirsSpotInstanceRequests :: Lens' DescribeSpotInstanceRequestsResponse ([SpotInstanceRequest])
dsirsSpotInstanceRequests = lens _dsirsSpotInstanceRequests (\s a -> s { _dsirsSpotInstanceRequests = a })
{-# INLINE dsirsSpotInstanceRequests #-}

instance FromXML DescribeSpotInstanceRequestsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSpotInstanceRequests where
    type Sv DescribeSpotInstanceRequests = EC2
    type Rs DescribeSpotInstanceRequests = DescribeSpotInstanceRequestsResponse

    request = post "DescribeSpotInstanceRequests"
    response _ = xmlResponse
