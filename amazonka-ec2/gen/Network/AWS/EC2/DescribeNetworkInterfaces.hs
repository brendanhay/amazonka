{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DescribeNetworkInterfaces
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your network interfaces.
module Network.AWS.EC2.DescribeNetworkInterfaces
    (
    -- * Request
      DescribeNetworkInterfaces
    -- ** Request constructor
    , describeNetworkInterfaces
    -- ** Request lenses
    , dniDryRun
    , dniFilters
    , dniNetworkInterfaceIds

    -- * Response
    , DescribeNetworkInterfacesResult
    -- ** Response constructor
    , describeNetworkInterfacesResult
    -- ** Response lenses
    , dnirNetworkInterfaces
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { _dniDryRun              :: Maybe Bool
    , _dniFilters             :: [Filter]
    , _dniNetworkInterfaceIds :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeNetworkInterfaces' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dniFilters' @::@ ['Filter']
--
-- * 'dniNetworkInterfaceIds' @::@ ['Text']
--
describeNetworkInterfaces :: DescribeNetworkInterfaces
describeNetworkInterfaces = DescribeNetworkInterfaces
    { _dniDryRun              = Nothing
    , _dniNetworkInterfaceIds = mempty
    , _dniFilters             = mempty
    }

dniDryRun :: Lens' DescribeNetworkInterfaces (Maybe Bool)
dniDryRun = lens _dniDryRun (\s a -> s { _dniDryRun = a })

-- | One or more filters. addresses.private-ip-address - The private IP
-- addresses associated with the network interface. addresses.primary -
-- Whether the private IP address is the primary IP address associated with
-- the network interface. addresses.association.public-ip - The association
-- ID returned when the network interface was associated with the Elastic IP
-- address. addresses.association.owner-id - The owner ID of the addresses
-- associated with the network interface. association.association-id - The
-- association ID returned when the network interface was associated with an
-- IP address. association.allocation-id - The allocation ID returned when
-- you allocated the Elastic IP address for your network interface.
-- association.ip-owner-id - The owner of the Elastic IP address associated
-- with the network interface. association.public-ip - The address of the
-- Elastic IP address bound to the network interface.
-- association.public-dns-name - The public DNS name for the network
-- interface. attachment.attachment-id - The ID of the interface attachment.
-- attachment.instance-id - The ID of the instance to which the network
-- interface is attached. attachment.instance-owner-id - The owner ID of the
-- instance to which the network interface is attached.
-- attachment.device-index - The device index to which the network interface
-- is attached. attachment.status - The status of the attachment (attaching
-- | attached | detaching | detached). attachment.attach.time - The time
-- that the network interface was attached to an instance.
-- attachment.delete-on-termination - Indicates whether the attachment is
-- deleted when an instance is terminated. availability-zone - The
-- Availability Zone of the network interface. description - The description
-- of the network interface. group-id - The ID of a security group
-- associated with the network interface. group-name - The name of a
-- security group associated with the network interface. mac-address - The
-- MAC address of the network interface. network-interface-id - The ID of
-- the network interface. owner-id - The AWS account ID of the network
-- interface owner. private-ip-address - The private IP address or addresses
-- of the network interface. private-dns-name - The private DNS name of the
-- network interface. requester-id - The ID of the entity that launched the
-- instance on your behalf (for example, AWS Management Console, Auto
-- Scaling, and so on). requester-managed - Indicates whether the network
-- interface is being managed by an AWS service (for example, AWS Management
-- Console, Auto Scaling, and so on). source-desk-check - Indicates whether
-- the network interface performs source/destination checking. A value of
-- true means checking is enabled, and false means checking is disabled. The
-- value must be false for the network interface to perform Network Address
-- Translation (NAT) in your VPC. status - The status of the network
-- interface. If the network interface is not attached to an instance, the
-- status is available; if a network interface is attached to an instance
-- the status is in-use. subnet-id - The ID of the subnet for the network
-- interface. tag:key=value - The key/value combination of a tag assigned to
-- the resource. tag-key - The key of a tag assigned to the resource. This
-- filter is independent of the tag-value filter. For example, if you use
-- both the filter "tag-key=Purpose" and the filter "tag-value=X", you get
-- any resources assigned both the tag key Purpose (regardless of what the
-- tag's value is), and the tag value X (regardless of what the tag's key
-- is). If you want to list only resources where Purpose is X, see the
-- tag:key=value filter. tag-value - The value of a tag assigned to the
-- resource. This filter is independent of the tag-key filter. vpc-id - The
-- ID of the VPC for the network interface.
dniFilters :: Lens' DescribeNetworkInterfaces [Filter]
dniFilters = lens _dniFilters (\s a -> s { _dniFilters = a })

-- | One or more network interface IDs. Default: Describes all your network
-- interfaces.
dniNetworkInterfaceIds :: Lens' DescribeNetworkInterfaces [Text]
dniNetworkInterfaceIds =
    lens _dniNetworkInterfaceIds (\s a -> s { _dniNetworkInterfaceIds = a })

instance ToQuery DescribeNetworkInterfaces

instance ToPath DescribeNetworkInterfaces where
    toPath = const "/"

newtype DescribeNetworkInterfacesResult = DescribeNetworkInterfacesResult
    { _dnirNetworkInterfaces :: [NetworkInterface]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeNetworkInterfacesResult where
    type Item DescribeNetworkInterfacesResult = NetworkInterface

    fromList = DescribeNetworkInterfacesResult . fromList
    toList   = toList . _dnirNetworkInterfaces

-- | 'DescribeNetworkInterfacesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dnirNetworkInterfaces' @::@ ['NetworkInterface']
--
describeNetworkInterfacesResult :: DescribeNetworkInterfacesResult
describeNetworkInterfacesResult = DescribeNetworkInterfacesResult
    { _dnirNetworkInterfaces = mempty
    }

-- | Information about one or more network interfaces.
dnirNetworkInterfaces :: Lens' DescribeNetworkInterfacesResult [NetworkInterface]
dnirNetworkInterfaces =
    lens _dnirNetworkInterfaces (\s a -> s { _dnirNetworkInterfaces = a })

instance FromXML DescribeNetworkInterfacesResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeNetworkInterfacesResult"

instance AWSRequest DescribeNetworkInterfaces where
    type Sv DescribeNetworkInterfaces = EC2
    type Rs DescribeNetworkInterfaces = DescribeNetworkInterfacesResult

    request  = post "DescribeNetworkInterfaces"
    response = xmlResponse $ \h x -> DescribeNetworkInterfacesResult
        <$> x %| "networkInterfaceSet"
