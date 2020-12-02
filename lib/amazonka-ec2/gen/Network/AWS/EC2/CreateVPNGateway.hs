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
-- Module      : Network.AWS.EC2.CreateVPNGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual private gateway. A virtual private gateway is the endpoint on the VPC side of your VPN connection. You can create a virtual private gateway before creating the VPC itself.
--
--
-- For more information about virtual private gateways, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html AWS Managed VPN Connections> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateVPNGateway
    (
    -- * Creating a Request
      createVPNGateway
    , CreateVPNGateway
    -- * Request Lenses
    , cvgAmazonSideASN
    , cvgAvailabilityZone
    , cvgDryRun
    , cvgType

    -- * Destructuring the Response
    , createVPNGatewayResponse
    , CreateVPNGatewayResponse
    -- * Response Lenses
    , cvgrsVPNGateway
    , cvgrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateVpnGateway.
--
--
--
-- /See:/ 'createVPNGateway' smart constructor.
data CreateVPNGateway = CreateVPNGateway'
  { _cvgAmazonSideASN    :: !(Maybe Integer)
  , _cvgAvailabilityZone :: !(Maybe Text)
  , _cvgDryRun           :: !(Maybe Bool)
  , _cvgType             :: !GatewayType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPNGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvgAmazonSideASN' - A private Autonomous System Number (ASN) for the Amazon side of a BGP session. If you're using a 16-bit ASN, it must be in the 64512 to 65534 range. If you're using a 32-bit ASN, it must be in the 4200000000 to 4294967294 range. Default: 64512
--
-- * 'cvgAvailabilityZone' - The Availability Zone for the virtual private gateway.
--
-- * 'cvgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cvgType' - The type of VPN connection this virtual private gateway supports.
createVPNGateway
    :: GatewayType -- ^ 'cvgType'
    -> CreateVPNGateway
createVPNGateway pType_ =
  CreateVPNGateway'
    { _cvgAmazonSideASN = Nothing
    , _cvgAvailabilityZone = Nothing
    , _cvgDryRun = Nothing
    , _cvgType = pType_
    }


-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. If you're using a 16-bit ASN, it must be in the 64512 to 65534 range. If you're using a 32-bit ASN, it must be in the 4200000000 to 4294967294 range. Default: 64512
cvgAmazonSideASN :: Lens' CreateVPNGateway (Maybe Integer)
cvgAmazonSideASN = lens _cvgAmazonSideASN (\ s a -> s{_cvgAmazonSideASN = a})

-- | The Availability Zone for the virtual private gateway.
cvgAvailabilityZone :: Lens' CreateVPNGateway (Maybe Text)
cvgAvailabilityZone = lens _cvgAvailabilityZone (\ s a -> s{_cvgAvailabilityZone = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvgDryRun :: Lens' CreateVPNGateway (Maybe Bool)
cvgDryRun = lens _cvgDryRun (\ s a -> s{_cvgDryRun = a})

-- | The type of VPN connection this virtual private gateway supports.
cvgType :: Lens' CreateVPNGateway GatewayType
cvgType = lens _cvgType (\ s a -> s{_cvgType = a})

instance AWSRequest CreateVPNGateway where
        type Rs CreateVPNGateway = CreateVPNGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateVPNGatewayResponse' <$>
                   (x .@? "vpnGateway") <*> (pure (fromEnum s)))

instance Hashable CreateVPNGateway where

instance NFData CreateVPNGateway where

instance ToHeaders CreateVPNGateway where
        toHeaders = const mempty

instance ToPath CreateVPNGateway where
        toPath = const "/"

instance ToQuery CreateVPNGateway where
        toQuery CreateVPNGateway'{..}
          = mconcat
              ["Action" =: ("CreateVpnGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AmazonSideAsn" =: _cvgAmazonSideASN,
               "AvailabilityZone" =: _cvgAvailabilityZone,
               "DryRun" =: _cvgDryRun, "Type" =: _cvgType]

-- | Contains the output of CreateVpnGateway.
--
--
--
-- /See:/ 'createVPNGatewayResponse' smart constructor.
data CreateVPNGatewayResponse = CreateVPNGatewayResponse'
  { _cvgrsVPNGateway     :: !(Maybe VPNGateway)
  , _cvgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPNGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvgrsVPNGateway' - Information about the virtual private gateway.
--
-- * 'cvgrsResponseStatus' - -- | The response status code.
createVPNGatewayResponse
    :: Int -- ^ 'cvgrsResponseStatus'
    -> CreateVPNGatewayResponse
createVPNGatewayResponse pResponseStatus_ =
  CreateVPNGatewayResponse'
    {_cvgrsVPNGateway = Nothing, _cvgrsResponseStatus = pResponseStatus_}


-- | Information about the virtual private gateway.
cvgrsVPNGateway :: Lens' CreateVPNGatewayResponse (Maybe VPNGateway)
cvgrsVPNGateway = lens _cvgrsVPNGateway (\ s a -> s{_cvgrsVPNGateway = a})

-- | -- | The response status code.
cvgrsResponseStatus :: Lens' CreateVPNGatewayResponse Int
cvgrsResponseStatus = lens _cvgrsResponseStatus (\ s a -> s{_cvgrsResponseStatus = a})

instance NFData CreateVPNGatewayResponse where
