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
-- Module      : Network.AWS.EC2.CreateVPC
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC with the specified CIDR block.
--
-- The smallest VPC you can create uses a \/28 netmask (16 IP addresses),
-- and the largest uses a \/16 netmask (65,536 IP addresses). To help you
-- decide how big to make your VPC, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html Your VPC and Subnets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- By default, each instance you launch in the VPC has the default DHCP
-- options, which includes only a default DNS server that we provide
-- (AmazonProvidedDNS). For more information about DHCP options, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPC.html AWS API Reference> for CreateVPC.
module Network.AWS.EC2.CreateVPC
    (
    -- * Creating a Request
      CreateVPC
    , createVPC
    -- * Request Lenses
    , cvInstanceTenancy
    , cvDryRun
    , cvCIdRBlock

    -- * Destructuring the Response
    , CreateVPCResponse
    , createVPCResponse
    -- * Response Lenses
    , cvrsVPC
    , cvrsStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVPC' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvInstanceTenancy'
--
-- * 'cvDryRun'
--
-- * 'cvCIdRBlock'
data CreateVPC = CreateVPC'
    { _cvInstanceTenancy :: !(Maybe Tenancy)
    , _cvDryRun :: !(Maybe Bool)
    , _cvCIdRBlock :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPC' smart constructor.
createVPC :: Text -> CreateVPC
createVPC pCIdRBlock_ = 
    CreateVPC'
    { _cvInstanceTenancy = Nothing
    , _cvDryRun = Nothing
    , _cvCIdRBlock = pCIdRBlock_
    }

-- | The supported tenancy options for instances launched into the VPC. A
-- value of @default@ means that instances can be launched with any
-- tenancy; a value of @dedicated@ means all instances launched into the
-- VPC are launched as dedicated tenancy instances regardless of the
-- tenancy assigned to the instance at launch. Dedicated tenancy instances
-- run on single-tenant hardware.
--
-- Default: @default@
cvInstanceTenancy :: Lens' CreateVPC (Maybe Tenancy)
cvInstanceTenancy = lens _cvInstanceTenancy (\ s a -> s{_cvInstanceTenancy = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cvDryRun :: Lens' CreateVPC (Maybe Bool)
cvDryRun = lens _cvDryRun (\ s a -> s{_cvDryRun = a});

-- | The network range for the VPC, in CIDR notation. For example,
-- @10.0.0.0\/16@.
cvCIdRBlock :: Lens' CreateVPC Text
cvCIdRBlock = lens _cvCIdRBlock (\ s a -> s{_cvCIdRBlock = a});

instance AWSRequest CreateVPC where
        type Sv CreateVPC = EC2
        type Rs CreateVPC = CreateVPCResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCResponse' <$>
                   (x .@? "vpc") <*> (pure (fromEnum s)))

instance ToHeaders CreateVPC where
        toHeaders = const mempty

instance ToPath CreateVPC where
        toPath = const "/"

instance ToQuery CreateVPC where
        toQuery CreateVPC'{..}
          = mconcat
              ["Action" =: ("CreateVpc" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "InstanceTenancy" =: _cvInstanceTenancy,
               "DryRun" =: _cvDryRun, "CidrBlock" =: _cvCIdRBlock]

-- | /See:/ 'createVPCResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvrsVPC'
--
-- * 'cvrsStatus'
data CreateVPCResponse = CreateVPCResponse'
    { _cvrsVPC :: !(Maybe VPC)
    , _cvrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPCResponse' smart constructor.
createVPCResponse :: Int -> CreateVPCResponse
createVPCResponse pStatus_ = 
    CreateVPCResponse'
    { _cvrsVPC = Nothing
    , _cvrsStatus = pStatus_
    }

-- | Information about the VPC.
cvrsVPC :: Lens' CreateVPCResponse (Maybe VPC)
cvrsVPC = lens _cvrsVPC (\ s a -> s{_cvrsVPC = a});

-- | Undocumented member.
cvrsStatus :: Lens' CreateVPCResponse Int
cvrsStatus = lens _cvrsStatus (\ s a -> s{_cvrsStatus = a});
