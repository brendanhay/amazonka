{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateSubnet
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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;subnet&gt; &lt;subnetId&gt;subnet-9d4a7b6c&lt;/subnetId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.1.0/24&lt;/cidrBlock&gt;
-- &lt;availableIpAddressCount&gt;251&lt;/availableIpAddressCount&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt; &lt;tagSet/&gt;
-- &lt;/subnet&gt; &lt;/CreateSubnetResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateSubnet where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateSubnet' request.
createSubnet :: Text -- ^ '_cstVpcId'
             -> Text -- ^ '_cstCidrBlock'
             -> CreateSubnet
createSubnet p1 p2 = CreateSubnet
    { _cstVpcId = p1
    , _cstCidrBlock = p2
    , _cstDryRun = Nothing
    , _cstAvailabilityZone = Nothing
    }

data CreateSubnet = CreateSubnet
    { _cstVpcId :: Text
      -- ^ The ID of the VPC.
    , _cstCidrBlock :: Text
      -- ^ The network range for the subnet, in CIDR notation. For example,
      -- 10.0.0.0/24.
    , _cstDryRun :: Maybe Bool
      -- ^ 
    , _cstAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the subnet. Default: Amazon EC2 selects
      -- one for you (recommended).
    } deriving (Show, Generic)

makeLenses ''CreateSubnet

instance ToQuery CreateSubnet where
    toQuery = genericToQuery def

data CreateSubnetResponse = CreateSubnetResponse
    { _csuSubnet :: Maybe Subnet
      -- ^ Information about the subnet.
    } deriving (Show, Generic)

makeLenses ''CreateSubnetResponse

instance AWSRequest CreateSubnet where
    type Sv CreateSubnet = EC2
    type Rs CreateSubnet = CreateSubnetResponse

    request = post "CreateSubnet"
    response _ = cursorResponse $ \hs xml ->
        pure CreateSubnetResponse
            <*> xml %|? "Subnet"
