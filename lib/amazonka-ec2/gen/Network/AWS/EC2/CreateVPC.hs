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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC with the specified IPv4 CIDR block. The smallest VPC you can create uses a /28 netmask (16 IPv4 addresses), and the largest uses a /16 netmask (65,536 IPv4 addresses). To help you decide how big to make your VPC, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html Your VPC and Subnets> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
-- You can optionally request an Amazon-provided IPv6 CIDR block for the VPC. The IPv6 CIDR block uses a /56 prefix length, and is allocated from Amazon's pool of IPv6 addresses. You cannot choose the IPv6 range for your VPC.
--
-- By default, each instance you launch in the VPC has the default DHCP options, which includes only a default DNS server that we provide (AmazonProvidedDNS). For more information about DHCP options, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- You can specify the instance tenancy value for the VPC when you create it. You can't change this value for the VPC after you create it. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html Dedicated Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreateVPC
    (
    -- * Creating a Request
      createVPC
    , CreateVPC
    -- * Request Lenses
    , cvAmazonProvidedIPv6CidrBlock
    , cvInstanceTenancy
    , cvDryRun
    , cvCidrBlock

    -- * Destructuring the Response
    , createVPCResponse
    , CreateVPCResponse
    -- * Response Lenses
    , cvrsVPC
    , cvrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateVpc.
--
--
--
-- /See:/ 'createVPC' smart constructor.
data CreateVPC = CreateVPC'
  { _cvAmazonProvidedIPv6CidrBlock :: !(Maybe Bool)
  , _cvInstanceTenancy             :: !(Maybe Tenancy)
  , _cvDryRun                      :: !(Maybe Bool)
  , _cvCidrBlock                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvAmazonProvidedIPv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IP addresses, or the size of the CIDR block.
--
-- * 'cvInstanceTenancy' - The tenancy options for instances launched into the VPC. For @default@ , instances are launched with shared tenancy by default. You can launch instances with any tenancy into a shared tenancy VPC. For @dedicated@ , instances are launched as dedicated tenancy instances by default. You can only launch instances with a tenancy of @dedicated@ or @host@ into a dedicated tenancy VPC.  __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only. Default: @default@
--
-- * 'cvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cvCidrBlock' - The IPv4 network range for the VPC, in CIDR notation. For example, @10.0.0.0/16@ .
createVPC
    :: Text -- ^ 'cvCidrBlock'
    -> CreateVPC
createVPC pCidrBlock_ =
  CreateVPC'
    { _cvAmazonProvidedIPv6CidrBlock = Nothing
    , _cvInstanceTenancy = Nothing
    , _cvDryRun = Nothing
    , _cvCidrBlock = pCidrBlock_
    }


-- | Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IP addresses, or the size of the CIDR block.
cvAmazonProvidedIPv6CidrBlock :: Lens' CreateVPC (Maybe Bool)
cvAmazonProvidedIPv6CidrBlock = lens _cvAmazonProvidedIPv6CidrBlock (\ s a -> s{_cvAmazonProvidedIPv6CidrBlock = a})

-- | The tenancy options for instances launched into the VPC. For @default@ , instances are launched with shared tenancy by default. You can launch instances with any tenancy into a shared tenancy VPC. For @dedicated@ , instances are launched as dedicated tenancy instances by default. You can only launch instances with a tenancy of @dedicated@ or @host@ into a dedicated tenancy VPC.  __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only. Default: @default@
cvInstanceTenancy :: Lens' CreateVPC (Maybe Tenancy)
cvInstanceTenancy = lens _cvInstanceTenancy (\ s a -> s{_cvInstanceTenancy = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvDryRun :: Lens' CreateVPC (Maybe Bool)
cvDryRun = lens _cvDryRun (\ s a -> s{_cvDryRun = a})

-- | The IPv4 network range for the VPC, in CIDR notation. For example, @10.0.0.0/16@ .
cvCidrBlock :: Lens' CreateVPC Text
cvCidrBlock = lens _cvCidrBlock (\ s a -> s{_cvCidrBlock = a})

instance AWSRequest CreateVPC where
        type Rs CreateVPC = CreateVPCResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCResponse' <$>
                   (x .@? "vpc") <*> (pure (fromEnum s)))

instance Hashable CreateVPC where

instance NFData CreateVPC where

instance ToHeaders CreateVPC where
        toHeaders = const mempty

instance ToPath CreateVPC where
        toPath = const "/"

instance ToQuery CreateVPC where
        toQuery CreateVPC'{..}
          = mconcat
              ["Action" =: ("CreateVpc" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AmazonProvidedIpv6CidrBlock" =:
                 _cvAmazonProvidedIPv6CidrBlock,
               "InstanceTenancy" =: _cvInstanceTenancy,
               "DryRun" =: _cvDryRun, "CidrBlock" =: _cvCidrBlock]

-- | Contains the output of CreateVpc.
--
--
--
-- /See:/ 'createVPCResponse' smart constructor.
data CreateVPCResponse = CreateVPCResponse'
  { _cvrsVPC            :: !(Maybe VPC)
  , _cvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvrsVPC' - Information about the VPC.
--
-- * 'cvrsResponseStatus' - -- | The response status code.
createVPCResponse
    :: Int -- ^ 'cvrsResponseStatus'
    -> CreateVPCResponse
createVPCResponse pResponseStatus_ =
  CreateVPCResponse'
    {_cvrsVPC = Nothing, _cvrsResponseStatus = pResponseStatus_}


-- | Information about the VPC.
cvrsVPC :: Lens' CreateVPCResponse (Maybe VPC)
cvrsVPC = lens _cvrsVPC (\ s a -> s{_cvrsVPC = a})

-- | -- | The response status code.
cvrsResponseStatus :: Lens' CreateVPCResponse Int
cvrsResponseStatus = lens _cvrsResponseStatus (\ s a -> s{_cvrsResponseStatus = a})

instance NFData CreateVPCResponse where
