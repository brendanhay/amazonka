{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.CreateVPNConnectionRoute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a static route associated with a VPN connection between an
-- existing virtual private gateway and a VPN customer gateway. The static
-- route allows traffic to be routed from the virtual private gateway to
-- the VPN customer gateway.
--
-- For more information about VPN connections, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPNConnectionRoute.html>
module Network.AWS.EC2.CreateVPNConnectionRoute
    (
    -- * Request
      CreateVPNConnectionRoute
    -- ** Request constructor
    , createVPNConnectionRoute
    -- ** Request lenses
    , cvcrVPNConnectionId
    , cvcrDestinationCIDRBlock

    -- * Response
    , CreateVPNConnectionRouteResponse
    -- ** Response constructor
    , createVPNConnectionRouteResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVPNConnectionRoute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvcrVPNConnectionId'
--
-- * 'cvcrDestinationCIDRBlock'
data CreateVPNConnectionRoute = CreateVPNConnectionRoute'
    { _cvcrVPNConnectionId      :: !Text
    , _cvcrDestinationCIDRBlock :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNConnectionRoute' smart constructor.
createVPNConnectionRoute :: Text -> Text -> CreateVPNConnectionRoute
createVPNConnectionRoute pVPNConnectionId pDestinationCIDRBlock =
    CreateVPNConnectionRoute'
    { _cvcrVPNConnectionId = pVPNConnectionId
    , _cvcrDestinationCIDRBlock = pDestinationCIDRBlock
    }

-- | The ID of the VPN connection.
cvcrVPNConnectionId :: Lens' CreateVPNConnectionRoute Text
cvcrVPNConnectionId = lens _cvcrVPNConnectionId (\ s a -> s{_cvcrVPNConnectionId = a});

-- | The CIDR block associated with the local subnet of the customer network.
cvcrDestinationCIDRBlock :: Lens' CreateVPNConnectionRoute Text
cvcrDestinationCIDRBlock = lens _cvcrDestinationCIDRBlock (\ s a -> s{_cvcrDestinationCIDRBlock = a});

instance AWSRequest CreateVPNConnectionRoute where
        type Sv CreateVPNConnectionRoute = EC2
        type Rs CreateVPNConnectionRoute =
             CreateVPNConnectionRouteResponse
        request = post
        response
          = receiveNull CreateVPNConnectionRouteResponse'

instance ToHeaders CreateVPNConnectionRoute where
        toHeaders = const mempty

instance ToPath CreateVPNConnectionRoute where
        toPath = const "/"

instance ToQuery CreateVPNConnectionRoute where
        toQuery CreateVPNConnectionRoute'{..}
          = mconcat
              ["Action" =:
                 ("CreateVPNConnectionRoute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "VpnConnectionId" =: _cvcrVPNConnectionId,
               "DestinationCidrBlock" =: _cvcrDestinationCIDRBlock]

-- | /See:/ 'createVPNConnectionRouteResponse' smart constructor.
data CreateVPNConnectionRouteResponse =
    CreateVPNConnectionRouteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNConnectionRouteResponse' smart constructor.
createVPNConnectionRouteResponse :: CreateVPNConnectionRouteResponse
createVPNConnectionRouteResponse = CreateVPNConnectionRouteResponse'
