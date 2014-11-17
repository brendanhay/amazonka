{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateSubnet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a subnet in an existing VPC. When you create each subnet, you
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
-- If you launch an instance in a VPC using an Amazon EBS-backed AMI, the IP
-- address doesn't change if you stop and restart the instance (unlike a
-- similar instance launched outside a VPC, which gets a new IP address when
-- restarted). It's therefore possible to have a subnet with no running
-- instances (they're all stopped), but no remaining IP addresses available.
-- For more information about subnets, see Your VPC and Subnets in the Amazon
-- Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSubnet.html>
module Network.AWS.EC2.CreateSubnet
    (
    -- * Request
      CreateSubnet
    -- ** Request constructor
    , createSubnet
    -- ** Request lenses
    , cs1AvailabilityZone
    , cs1CidrBlock
    , cs1DryRun
    , cs1VpcId

    -- * Response
    , CreateSubnetResponse
    -- ** Response constructor
    , createSubnetResponse
    -- ** Response lenses
    , csrSubnet
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateSubnet = CreateSubnet
    { _cs1AvailabilityZone :: Maybe Text
    , _cs1CidrBlock        :: Text
    , _cs1DryRun           :: Maybe Bool
    , _cs1VpcId            :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateSubnet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cs1AvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cs1CidrBlock' @::@ 'Text'
--
-- * 'cs1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cs1VpcId' @::@ 'Text'
--
createSubnet :: Text -- ^ 'cs1VpcId'
             -> Text -- ^ 'cs1CidrBlock'
             -> CreateSubnet
createSubnet p1 p2 = CreateSubnet
    { _cs1VpcId            = p1
    , _cs1CidrBlock        = p2
    , _cs1DryRun           = Nothing
    , _cs1AvailabilityZone = Nothing
    }

-- | The Availability Zone for the subnet. Default: Amazon EC2 selects one for
-- you (recommended).
cs1AvailabilityZone :: Lens' CreateSubnet (Maybe Text)
cs1AvailabilityZone =
    lens _cs1AvailabilityZone (\s a -> s { _cs1AvailabilityZone = a })

-- | The network range for the subnet, in CIDR notation. For example,
-- 10.0.0.0/24.
cs1CidrBlock :: Lens' CreateSubnet Text
cs1CidrBlock = lens _cs1CidrBlock (\s a -> s { _cs1CidrBlock = a })

cs1DryRun :: Lens' CreateSubnet (Maybe Bool)
cs1DryRun = lens _cs1DryRun (\s a -> s { _cs1DryRun = a })

-- | The ID of the VPC.
cs1VpcId :: Lens' CreateSubnet Text
cs1VpcId = lens _cs1VpcId (\s a -> s { _cs1VpcId = a })

newtype CreateSubnetResponse = CreateSubnetResponse
    { _csrSubnet :: Maybe Subnet
    } deriving (Eq, Show, Generic)

-- | 'CreateSubnetResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSubnet' @::@ 'Maybe' 'Subnet'
--
createSubnetResponse :: CreateSubnetResponse
createSubnetResponse = CreateSubnetResponse
    { _csrSubnet = Nothing
    }

-- | Information about the subnet.
csrSubnet :: Lens' CreateSubnetResponse (Maybe Subnet)
csrSubnet = lens _csrSubnet (\s a -> s { _csrSubnet = a })

instance ToPath CreateSubnet where
    toPath = const "/"

instance ToQuery CreateSubnet

instance ToHeaders CreateSubnet

instance AWSRequest CreateSubnet where
    type Sv CreateSubnet = EC2
    type Rs CreateSubnet = CreateSubnetResponse

    request  = post "CreateSubnet"
    response = xmlResponse

instance FromXML CreateSubnetResponse where
    parseXML c = CreateSubnetResponse
        <$> c .: "subnet"
