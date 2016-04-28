{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSubnet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
module Network.AWS.EC2.CreateSubnet
    (
    -- * Creating a Request
      createSubnet
    , CreateSubnet
    -- * Request Lenses
    , cssAvailabilityZone
    , cssDryRun
    , cssVPCId
    , cssCIdRBlock

    -- * Destructuring the Response
    , createSubnetResponse
    , CreateSubnetResponse
    -- * Response Lenses
    , crsSubnet
    , crsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createSubnet' smart constructor.
data CreateSubnet = CreateSubnet'
    { _cssAvailabilityZone :: !(Maybe Text)
    , _cssDryRun           :: !(Maybe Bool)
    , _cssVPCId            :: !Text
    , _cssCIdRBlock        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSubnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssAvailabilityZone'
--
-- * 'cssDryRun'
--
-- * 'cssVPCId'
--
-- * 'cssCIdRBlock'
createSubnet
    :: Text -- ^ 'cssVPCId'
    -> Text -- ^ 'cssCIdRBlock'
    -> CreateSubnet
createSubnet pVPCId_ pCIdRBlock_ =
    CreateSubnet'
    { _cssAvailabilityZone = Nothing
    , _cssDryRun = Nothing
    , _cssVPCId = pVPCId_
    , _cssCIdRBlock = pCIdRBlock_
    }

-- | The Availability Zone for the subnet.
--
-- Default: AWS selects one for you. If you create more than one subnet in
-- your VPC, we may not necessarily select a different zone for each
-- subnet.
cssAvailabilityZone :: Lens' CreateSubnet (Maybe Text)
cssAvailabilityZone = lens _cssAvailabilityZone (\ s a -> s{_cssAvailabilityZone = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
cssDryRun :: Lens' CreateSubnet (Maybe Bool)
cssDryRun = lens _cssDryRun (\ s a -> s{_cssDryRun = a});

-- | The ID of the VPC.
cssVPCId :: Lens' CreateSubnet Text
cssVPCId = lens _cssVPCId (\ s a -> s{_cssVPCId = a});

-- | The network range for the subnet, in CIDR notation. For example,
-- '10.0.0.0\/24'.
cssCIdRBlock :: Lens' CreateSubnet Text
cssCIdRBlock = lens _cssCIdRBlock (\ s a -> s{_cssCIdRBlock = a});

instance AWSRequest CreateSubnet where
        type Rs CreateSubnet = CreateSubnetResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateSubnetResponse' <$>
                   (x .@? "subnet") <*> (pure (fromEnum s)))

instance Hashable CreateSubnet

instance NFData CreateSubnet

instance ToHeaders CreateSubnet where
        toHeaders = const mempty

instance ToPath CreateSubnet where
        toPath = const "/"

instance ToQuery CreateSubnet where
        toQuery CreateSubnet'{..}
          = mconcat
              ["Action" =: ("CreateSubnet" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "AvailabilityZone" =: _cssAvailabilityZone,
               "DryRun" =: _cssDryRun, "VpcId" =: _cssVPCId,
               "CidrBlock" =: _cssCIdRBlock]

-- | /See:/ 'createSubnetResponse' smart constructor.
data CreateSubnetResponse = CreateSubnetResponse'
    { _crsSubnet         :: !(Maybe Subnet)
    , _crsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSubnetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsSubnet'
--
-- * 'crsResponseStatus'
createSubnetResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateSubnetResponse
createSubnetResponse pResponseStatus_ =
    CreateSubnetResponse'
    { _crsSubnet = Nothing
    , _crsResponseStatus = pResponseStatus_
    }

-- | Information about the subnet.
crsSubnet :: Lens' CreateSubnetResponse (Maybe Subnet)
crsSubnet = lens _crsSubnet (\ s a -> s{_crsSubnet = a});

-- | The response status code.
crsResponseStatus :: Lens' CreateSubnetResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a});
