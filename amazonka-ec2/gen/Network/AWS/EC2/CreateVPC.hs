{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CreateVPC
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

-- | Creates a VPC with the specified CIDR block.
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPC.html>
module Network.AWS.EC2.CreateVPC
    (
    -- * Request
      CreateVPC
    -- ** Request constructor
    , createVPC
    -- ** Request lenses
    , cvInstanceTenancy
    , cvDryRun
    , cvCIDRBlock

    -- * Response
    , CreateVPCResponse
    -- ** Response constructor
    , createVPCResponse
    -- ** Response lenses
    , cvrVPC
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'createVPC' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvInstanceTenancy'
--
-- * 'cvDryRun'
--
-- * 'cvCIDRBlock'
data CreateVPC = CreateVPC'{_cvInstanceTenancy :: Maybe Tenancy, _cvDryRun :: Maybe Bool, _cvCIDRBlock :: Text} deriving (Eq, Read, Show)

-- | 'CreateVPC' smart constructor.
createVPC :: Text -> CreateVPC
createVPC pCIDRBlock = CreateVPC'{_cvInstanceTenancy = Nothing, _cvDryRun = Nothing, _cvCIDRBlock = pCIDRBlock};

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
cvCIDRBlock :: Lens' CreateVPC Text
cvCIDRBlock = lens _cvCIDRBlock (\ s a -> s{_cvCIDRBlock = a});

instance AWSRequest CreateVPC where
        type Sv CreateVPC = EC2
        type Rs CreateVPC = CreateVPCResponse
        request = post
        response
          = receiveXML
              (\ s h x -> CreateVPCResponse' <$> (x .@? "vpc"))

instance ToHeaders CreateVPC where
        toHeaders = const mempty

instance ToPath CreateVPC where
        toPath = const "/"

instance ToQuery CreateVPC where
        toQuery CreateVPC'{..}
          = mconcat
              ["Action" =: ("CreateVPC" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "InstanceTenancy" =: _cvInstanceTenancy,
               "DryRun" =: _cvDryRun, "CidrBlock" =: _cvCIDRBlock]

-- | /See:/ 'createVPCResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvrVPC'
newtype CreateVPCResponse = CreateVPCResponse'{_cvrVPC :: Maybe VPC} deriving (Eq, Read, Show)

-- | 'CreateVPCResponse' smart constructor.
createVPCResponse :: CreateVPCResponse
createVPCResponse = CreateVPCResponse'{_cvrVPC = Nothing};

-- | Information about the VPC.
cvrVPC :: Lens' CreateVPCResponse (Maybe VPC)
cvrVPC = lens _cvrVPC (\ s a -> s{_cvrVPC = a});
