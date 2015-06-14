{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CreateVPNGateway
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

-- | Creates a virtual private gateway. A virtual private gateway is the
-- endpoint on the VPC side of your VPN connection. You can create a
-- virtual private gateway before creating the VPC itself.
--
-- For more information about virtual private gateways, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPNGateway.html>
module Network.AWS.EC2.CreateVPNGateway
    (
    -- * Request
      CreateVPNGateway
    -- ** Request constructor
    , createVPNGateway
    -- ** Request lenses
    , cvgAvailabilityZone
    , cvgDryRun
    , cvgType

    -- * Response
    , CreateVPNGatewayResponse
    -- ** Response constructor
    , createVPNGatewayResponse
    -- ** Response lenses
    , cvgrVPNGateway
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'createVPNGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvgAvailabilityZone'
--
-- * 'cvgDryRun'
--
-- * 'cvgType'
data CreateVPNGateway = CreateVPNGateway'{_cvgAvailabilityZone :: Maybe Text, _cvgDryRun :: Maybe Bool, _cvgType :: GatewayType} deriving (Eq, Read, Show)

-- | 'CreateVPNGateway' smart constructor.
createVPNGateway :: GatewayType -> CreateVPNGateway
createVPNGateway pType = CreateVPNGateway'{_cvgAvailabilityZone = Nothing, _cvgDryRun = Nothing, _cvgType = pType};

-- | The Availability Zone for the virtual private gateway.
cvgAvailabilityZone :: Lens' CreateVPNGateway (Maybe Text)
cvgAvailabilityZone = lens _cvgAvailabilityZone (\ s a -> s{_cvgAvailabilityZone = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cvgDryRun :: Lens' CreateVPNGateway (Maybe Bool)
cvgDryRun = lens _cvgDryRun (\ s a -> s{_cvgDryRun = a});

-- | The type of VPN connection this virtual private gateway supports.
cvgType :: Lens' CreateVPNGateway GatewayType
cvgType = lens _cvgType (\ s a -> s{_cvgType = a});

instance AWSRequest CreateVPNGateway where
        type Sv CreateVPNGateway = EC2
        type Rs CreateVPNGateway = CreateVPNGatewayResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateVPNGatewayResponse' <$> x .@? "vpnGateway")

instance ToHeaders CreateVPNGateway where
        toHeaders = const mempty

instance ToPath CreateVPNGateway where
        toPath = const "/"

instance ToQuery CreateVPNGateway where
        toQuery CreateVPNGateway'{..}
          = mconcat
              ["Action" =: ("CreateVPNGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "AvailabilityZone" =: _cvgAvailabilityZone,
               "DryRun" =: _cvgDryRun, "Type" =: _cvgType]

-- | /See:/ 'createVPNGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvgrVPNGateway'
newtype CreateVPNGatewayResponse = CreateVPNGatewayResponse'{_cvgrVPNGateway :: Maybe VPNGateway} deriving (Eq, Read, Show)

-- | 'CreateVPNGatewayResponse' smart constructor.
createVPNGatewayResponse :: CreateVPNGatewayResponse
createVPNGatewayResponse = CreateVPNGatewayResponse'{_cvgrVPNGateway = Nothing};

-- | Information about the virtual private gateway.
cvgrVPNGateway :: Lens' CreateVPNGatewayResponse (Maybe VPNGateway)
cvgrVPNGateway = lens _cvgrVPNGateway (\ s a -> s{_cvgrVPNGateway = a});
