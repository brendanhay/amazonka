{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSubnet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a subnet in an existing VPC.
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
    , creaAvailabilityZone
    , creaDryRun
    , creaVPCId
    , creaCIdRBlock

    -- * Response
    , CreateSubnetResponse
    -- ** Response constructor
    , createSubnetResponse
    -- ** Response lenses
    , creSubnet
    , creStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createSubnet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creaAvailabilityZone'
--
-- * 'creaDryRun'
--
-- * 'creaVPCId'
--
-- * 'creaCIdRBlock'
data CreateSubnet = CreateSubnet'
    { _creaAvailabilityZone :: !(Maybe Text)
    , _creaDryRun           :: !(Maybe Bool)
    , _creaVPCId            :: !Text
    , _creaCIdRBlock        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSubnet' smart constructor.
createSubnet :: Text -> Text -> CreateSubnet
createSubnet pVPCId pCIdRBlock =
    CreateSubnet'
    { _creaAvailabilityZone = Nothing
    , _creaDryRun = Nothing
    , _creaVPCId = pVPCId
    , _creaCIdRBlock = pCIdRBlock
    }

-- | The Availability Zone for the subnet.
--
-- Default: Amazon EC2 selects one for you (recommended).
creaAvailabilityZone :: Lens' CreateSubnet (Maybe Text)
creaAvailabilityZone = lens _creaAvailabilityZone (\ s a -> s{_creaAvailabilityZone = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
creaDryRun :: Lens' CreateSubnet (Maybe Bool)
creaDryRun = lens _creaDryRun (\ s a -> s{_creaDryRun = a});

-- | The ID of the VPC.
creaVPCId :: Lens' CreateSubnet Text
creaVPCId = lens _creaVPCId (\ s a -> s{_creaVPCId = a});

-- | The network range for the subnet, in CIDR notation. For example,
-- @10.0.0.0\/24@.
creaCIdRBlock :: Lens' CreateSubnet Text
creaCIdRBlock = lens _creaCIdRBlock (\ s a -> s{_creaCIdRBlock = a});

instance AWSRequest CreateSubnet where
        type Sv CreateSubnet = EC2
        type Rs CreateSubnet = CreateSubnetResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateSubnetResponse' <$>
                   (x .@? "subnet") <*> (pure (fromEnum s)))

instance ToHeaders CreateSubnet where
        toHeaders = const mempty

instance ToPath CreateSubnet where
        toPath = const "/"

instance ToQuery CreateSubnet where
        toQuery CreateSubnet'{..}
          = mconcat
              ["Action" =: ("CreateSubnet" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "AvailabilityZone" =: _creaAvailabilityZone,
               "DryRun" =: _creaDryRun, "VpcId" =: _creaVPCId,
               "CidrBlock" =: _creaCIdRBlock]

-- | /See:/ 'createSubnetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creSubnet'
--
-- * 'creStatus'
data CreateSubnetResponse = CreateSubnetResponse'
    { _creSubnet :: !(Maybe Subnet)
    , _creStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSubnetResponse' smart constructor.
createSubnetResponse :: Int -> CreateSubnetResponse
createSubnetResponse pStatus =
    CreateSubnetResponse'
    { _creSubnet = Nothing
    , _creStatus = pStatus
    }

-- | Information about the subnet.
creSubnet :: Lens' CreateSubnetResponse (Maybe Subnet)
creSubnet = lens _creSubnet (\ s a -> s{_creSubnet = a});

-- | FIXME: Undocumented member.
creStatus :: Lens' CreateSubnetResponse Int
creStatus = lens _creStatus (\ s a -> s{_creStatus = a});
