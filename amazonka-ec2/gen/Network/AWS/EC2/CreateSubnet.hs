{-# LANGUAGE DeriveGeneric               #-}
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
module Network.AWS.EC2.CreateSubnet
    (
    -- * Request
      CreateSubnet
    -- ** Request constructor
    , mkCreateSubnet
    -- ** Request lenses
    , cs2VpcId
    , cs2CidrBlock
    , cs2AvailabilityZone

    -- * Response
    , CreateSubnetResponse
    -- ** Response constructor
    , mkCreateSubnetResponse
    -- ** Response lenses
    , csr1Subnet
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CreateSubnet = CreateSubnet
    { _cs2VpcId :: !Text
    , _cs2CidrBlock :: !Text
    , _cs2AvailabilityZone :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSubnet' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcId ::@ @Text@
--
-- * @CidrBlock ::@ @Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
mkCreateSubnet :: Text -- ^ 'cs2VpcId'
               -> Text -- ^ 'cs2CidrBlock'
               -> CreateSubnet
mkCreateSubnet p1 p2 = CreateSubnet
    { _cs2VpcId = p1
    , _cs2CidrBlock = p2
    , _cs2AvailabilityZone = Nothing
    }

-- | The ID of the VPC.
cs2VpcId :: Lens' CreateSubnet Text
cs2VpcId = lens _cs2VpcId (\s a -> s { _cs2VpcId = a })

-- | The network range for the subnet, in CIDR notation. For example,
-- 10.0.0.0/24.
cs2CidrBlock :: Lens' CreateSubnet Text
cs2CidrBlock = lens _cs2CidrBlock (\s a -> s { _cs2CidrBlock = a })

-- | The Availability Zone for the subnet. Default: Amazon EC2 selects one for
-- you (recommended).
cs2AvailabilityZone :: Lens' CreateSubnet (Maybe Text)
cs2AvailabilityZone =
    lens _cs2AvailabilityZone (\s a -> s { _cs2AvailabilityZone = a })

instance ToQuery CreateSubnet where
    toQuery = genericQuery def

newtype CreateSubnetResponse = CreateSubnetResponse
    { _csr1Subnet :: Maybe Subnet
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSubnetResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Subnet ::@ @Maybe Subnet@
--
mkCreateSubnetResponse :: CreateSubnetResponse
mkCreateSubnetResponse = CreateSubnetResponse
    { _csr1Subnet = Nothing
    }

-- | Information about the subnet.
csr1Subnet :: Lens' CreateSubnetResponse (Maybe Subnet)
csr1Subnet = lens _csr1Subnet (\s a -> s { _csr1Subnet = a })

instance FromXML CreateSubnetResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSubnet where
    type Sv CreateSubnet = EC2
    type Rs CreateSubnet = CreateSubnetResponse

    request = post "CreateSubnet"
    response _ = xmlResponse
