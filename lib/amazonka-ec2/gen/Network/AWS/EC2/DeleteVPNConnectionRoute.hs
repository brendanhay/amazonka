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
-- Module      : Network.AWS.EC2.DeleteVPNConnectionRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified static route associated with a VPN connection between an existing virtual private gateway and a VPN customer gateway. The static route allows traffic to be routed from the virtual private gateway to the VPN customer gateway.
--
--
module Network.AWS.EC2.DeleteVPNConnectionRoute
    (
    -- * Creating a Request
      deleteVPNConnectionRoute
    , DeleteVPNConnectionRoute
    -- * Request Lenses
    , dvcrDestinationCidrBlock
    , dvcrVPNConnectionId

    -- * Destructuring the Response
    , deleteVPNConnectionRouteResponse
    , DeleteVPNConnectionRouteResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeleteVpnConnectionRoute.
--
--
--
-- /See:/ 'deleteVPNConnectionRoute' smart constructor.
data DeleteVPNConnectionRoute = DeleteVPNConnectionRoute'
  { _dvcrDestinationCidrBlock :: !Text
  , _dvcrVPNConnectionId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPNConnectionRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcrDestinationCidrBlock' - The CIDR block associated with the local subnet of the customer network.
--
-- * 'dvcrVPNConnectionId' - The ID of the VPN connection.
deleteVPNConnectionRoute
    :: Text -- ^ 'dvcrDestinationCidrBlock'
    -> Text -- ^ 'dvcrVPNConnectionId'
    -> DeleteVPNConnectionRoute
deleteVPNConnectionRoute pDestinationCidrBlock_ pVPNConnectionId_ =
  DeleteVPNConnectionRoute'
    { _dvcrDestinationCidrBlock = pDestinationCidrBlock_
    , _dvcrVPNConnectionId = pVPNConnectionId_
    }


-- | The CIDR block associated with the local subnet of the customer network.
dvcrDestinationCidrBlock :: Lens' DeleteVPNConnectionRoute Text
dvcrDestinationCidrBlock = lens _dvcrDestinationCidrBlock (\ s a -> s{_dvcrDestinationCidrBlock = a})

-- | The ID of the VPN connection.
dvcrVPNConnectionId :: Lens' DeleteVPNConnectionRoute Text
dvcrVPNConnectionId = lens _dvcrVPNConnectionId (\ s a -> s{_dvcrVPNConnectionId = a})

instance AWSRequest DeleteVPNConnectionRoute where
        type Rs DeleteVPNConnectionRoute =
             DeleteVPNConnectionRouteResponse
        request = postQuery ec2
        response
          = receiveNull DeleteVPNConnectionRouteResponse'

instance Hashable DeleteVPNConnectionRoute where

instance NFData DeleteVPNConnectionRoute where

instance ToHeaders DeleteVPNConnectionRoute where
        toHeaders = const mempty

instance ToPath DeleteVPNConnectionRoute where
        toPath = const "/"

instance ToQuery DeleteVPNConnectionRoute where
        toQuery DeleteVPNConnectionRoute'{..}
          = mconcat
              ["Action" =:
                 ("DeleteVpnConnectionRoute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DestinationCidrBlock" =: _dvcrDestinationCidrBlock,
               "VpnConnectionId" =: _dvcrVPNConnectionId]

-- | /See:/ 'deleteVPNConnectionRouteResponse' smart constructor.
data DeleteVPNConnectionRouteResponse =
  DeleteVPNConnectionRouteResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPNConnectionRouteResponse' with the minimum fields required to make a request.
--
deleteVPNConnectionRouteResponse
    :: DeleteVPNConnectionRouteResponse
deleteVPNConnectionRouteResponse = DeleteVPNConnectionRouteResponse'


instance NFData DeleteVPNConnectionRouteResponse
         where
