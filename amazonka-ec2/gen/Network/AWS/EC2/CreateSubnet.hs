{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CreateSubnet
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a subnet in an existing VPC.
--
-- When you create each subnet, you provide the VPC ID and the CIDR block
-- you want for the subnet. After you create a subnet, you can\'t change
-- its CIDR block. The subnet\'s CIDR block can be the same as the VPC\'s
-- CIDR block (assuming you want only a single subnet in the VPC), or a
-- subset of the VPC\'s CIDR block. If you create more than one subnet in a
-- VPC, the subnets\' CIDR blocks must not overlap. The smallest subnet
-- (and VPC) you can create uses a \/28 netmask (16 IP addresses), and the
-- largest uses a \/16 netmask (65,536 IP addresses).
--
-- AWS reserves both the first four and the last IP address in each
-- subnet\'s CIDR block. They\'re not available for use.
--
-- If you add more than one subnet to a VPC, they\'re set up in a star
-- topology with a logical router in the middle.
--
-- If you launch an instance in a VPC using an Amazon EBS-backed AMI, the
-- IP address doesn\'t change if you stop and restart the instance (unlike
-- a similar instance launched outside a VPC, which gets a new IP address
-- when restarted). It\'s therefore possible to have a subnet with no
-- running instances (they\'re all stopped), but no remaining IP addresses
-- available.
--
-- For more information about subnets, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html Your VPC and Subnets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSubnet.html>
module Network.AWS.EC2.CreateSubnet
    (
    -- * Request
      CreateSubnet
    -- ** Request constructor
    , createSubnet
    -- ** Request lenses
    , createAvailabilityZone
    , createDryRun
    , createVPCId
    , createCIDRBlock

    -- * Response
    , CreateSubnetResponse
    -- ** Response constructor
    , createSubnetResponse
    -- ** Response lenses
    , csrSubnet
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'createSubnet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'createAvailabilityZone'
--
-- * 'createDryRun'
--
-- * 'createVPCId'
--
-- * 'createCIDRBlock'
data CreateSubnet = CreateSubnet'{_createAvailabilityZone :: Maybe Text, _createDryRun :: Maybe Bool, _createVPCId :: Text, _createCIDRBlock :: Text} deriving (Eq, Read, Show)

-- | 'CreateSubnet' smart constructor.
createSubnet :: Text -> Text -> CreateSubnet
createSubnet pVPCId pCIDRBlock = CreateSubnet'{_createAvailabilityZone = Nothing, _createDryRun = Nothing, _createVPCId = pVPCId, _createCIDRBlock = pCIDRBlock};

-- | The Availability Zone for the subnet.
--
-- Default: Amazon EC2 selects one for you (recommended).
createAvailabilityZone :: Lens' CreateSubnet (Maybe Text)
createAvailabilityZone = lens _createAvailabilityZone (\ s a -> s{_createAvailabilityZone = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createDryRun :: Lens' CreateSubnet (Maybe Bool)
createDryRun = lens _createDryRun (\ s a -> s{_createDryRun = a});

-- | The ID of the VPC.
createVPCId :: Lens' CreateSubnet Text
createVPCId = lens _createVPCId (\ s a -> s{_createVPCId = a});

-- | The network range for the subnet, in CIDR notation. For example,
-- @10.0.0.0\/24@.
createCIDRBlock :: Lens' CreateSubnet Text
createCIDRBlock = lens _createCIDRBlock (\ s a -> s{_createCIDRBlock = a});

instance AWSRequest CreateSubnet where
        type Sv CreateSubnet = EC2
        type Rs CreateSubnet = CreateSubnetResponse
        request = post
        response
          = receiveXML
              (\ s h x -> CreateSubnetResponse' <$> x .@? "subnet")

instance ToHeaders CreateSubnet where
        toHeaders = const mempty

instance ToPath CreateSubnet where
        toPath = const "/"

instance ToQuery CreateSubnet where
        toQuery CreateSubnet'{..}
          = mconcat
              ["Action" =: ("CreateSubnet" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "AvailabilityZone" =: _createAvailabilityZone,
               "DryRun" =: _createDryRun, "VpcId" =: _createVPCId,
               "CidrBlock" =: _createCIDRBlock]

-- | /See:/ 'createSubnetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSubnet'
newtype CreateSubnetResponse = CreateSubnetResponse'{_csrSubnet :: Maybe Subnet} deriving (Eq, Read, Show)

-- | 'CreateSubnetResponse' smart constructor.
createSubnetResponse :: CreateSubnetResponse
createSubnetResponse = CreateSubnetResponse'{_csrSubnet = Nothing};

-- | Information about the subnet.
csrSubnet :: Lens' CreateSubnetResponse (Maybe Subnet)
csrSubnet = lens _csrSubnet (\ s a -> s{_csrSubnet = a});
