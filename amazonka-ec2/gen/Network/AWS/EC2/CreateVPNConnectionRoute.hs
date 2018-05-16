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
-- Module      : Network.AWS.EC2.CreateVPNConnectionRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route associated with a VPN connection between an existing virtual private gateway and a VPN customer gateway. The static route allows traffic to be routed from the virtual private gateway to the VPN customer gateway.
--
--
-- For more information about VPN connections, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html AWS Managed VPN Connections> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateVPNConnectionRoute
    (
    -- * Creating a Request
      createVPNConnectionRoute
    , CreateVPNConnectionRoute
    -- * Request Lenses
    , cvcrDestinationCidrBlock
    , cvcrVPNConnectionId

    -- * Destructuring the Response
    , createVPNConnectionRouteResponse
    , CreateVPNConnectionRouteResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateVpnConnectionRoute.
--
--
--
-- /See:/ 'createVPNConnectionRoute' smart constructor.
data CreateVPNConnectionRoute = CreateVPNConnectionRoute'
  { _cvcrDestinationCidrBlock :: !Text
  , _cvcrVPNConnectionId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPNConnectionRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvcrDestinationCidrBlock' - The CIDR block associated with the local subnet of the customer network.
--
-- * 'cvcrVPNConnectionId' - The ID of the VPN connection.
createVPNConnectionRoute
    :: Text -- ^ 'cvcrDestinationCidrBlock'
    -> Text -- ^ 'cvcrVPNConnectionId'
    -> CreateVPNConnectionRoute
createVPNConnectionRoute pDestinationCidrBlock_ pVPNConnectionId_ =
  CreateVPNConnectionRoute'
    { _cvcrDestinationCidrBlock = pDestinationCidrBlock_
    , _cvcrVPNConnectionId = pVPNConnectionId_
    }


-- | The CIDR block associated with the local subnet of the customer network.
cvcrDestinationCidrBlock :: Lens' CreateVPNConnectionRoute Text
cvcrDestinationCidrBlock = lens _cvcrDestinationCidrBlock (\ s a -> s{_cvcrDestinationCidrBlock = a})

-- | The ID of the VPN connection.
cvcrVPNConnectionId :: Lens' CreateVPNConnectionRoute Text
cvcrVPNConnectionId = lens _cvcrVPNConnectionId (\ s a -> s{_cvcrVPNConnectionId = a})

instance AWSRequest CreateVPNConnectionRoute where
        type Rs CreateVPNConnectionRoute =
             CreateVPNConnectionRouteResponse
        request = postQuery ec2
        response
          = receiveNull CreateVPNConnectionRouteResponse'

instance Hashable CreateVPNConnectionRoute where

instance NFData CreateVPNConnectionRoute where

instance ToHeaders CreateVPNConnectionRoute where
        toHeaders = const mempty

instance ToPath CreateVPNConnectionRoute where
        toPath = const "/"

instance ToQuery CreateVPNConnectionRoute where
        toQuery CreateVPNConnectionRoute'{..}
          = mconcat
              ["Action" =:
                 ("CreateVpnConnectionRoute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DestinationCidrBlock" =: _cvcrDestinationCidrBlock,
               "VpnConnectionId" =: _cvcrVPNConnectionId]

-- | /See:/ 'createVPNConnectionRouteResponse' smart constructor.
data CreateVPNConnectionRouteResponse =
  CreateVPNConnectionRouteResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPNConnectionRouteResponse' with the minimum fields required to make a request.
--
createVPNConnectionRouteResponse
    :: CreateVPNConnectionRouteResponse
createVPNConnectionRouteResponse = CreateVPNConnectionRouteResponse'


instance NFData CreateVPNConnectionRouteResponse
         where
