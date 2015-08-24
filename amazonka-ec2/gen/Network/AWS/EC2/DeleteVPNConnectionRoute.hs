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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified static route associated with a VPN connection
-- between an existing virtual private gateway and a VPN customer gateway.
-- The static route allows traffic to be routed from the virtual private
-- gateway to the VPN customer gateway.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVPNConnectionRoute.html AWS API Reference> for DeleteVPNConnectionRoute.
module Network.AWS.EC2.DeleteVPNConnectionRoute
    (
    -- * Creating a Request
      deleteVPNConnectionRoute
    , DeleteVPNConnectionRoute
    -- * Request Lenses
    , dvcrVPNConnectionId
    , dvcrDestinationCIdRBlock

    -- * Destructuring the Response
    , deleteVPNConnectionRouteResponse
    , DeleteVPNConnectionRouteResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVPNConnectionRoute' smart constructor.
data DeleteVPNConnectionRoute = DeleteVPNConnectionRoute'
    { _dvcrVPNConnectionId      :: !Text
    , _dvcrDestinationCIdRBlock :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteVPNConnectionRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcrVPNConnectionId'
--
-- * 'dvcrDestinationCIdRBlock'
deleteVPNConnectionRoute
    :: Text -- ^ 'dvcrVPNConnectionId'
    -> Text -- ^ 'dvcrDestinationCIdRBlock'
    -> DeleteVPNConnectionRoute
deleteVPNConnectionRoute pVPNConnectionId_ pDestinationCIdRBlock_ =
    DeleteVPNConnectionRoute'
    { _dvcrVPNConnectionId = pVPNConnectionId_
    , _dvcrDestinationCIdRBlock = pDestinationCIdRBlock_
    }

-- | The ID of the VPN connection.
dvcrVPNConnectionId :: Lens' DeleteVPNConnectionRoute Text
dvcrVPNConnectionId = lens _dvcrVPNConnectionId (\ s a -> s{_dvcrVPNConnectionId = a});

-- | The CIDR block associated with the local subnet of the customer network.
dvcrDestinationCIdRBlock :: Lens' DeleteVPNConnectionRoute Text
dvcrDestinationCIdRBlock = lens _dvcrDestinationCIdRBlock (\ s a -> s{_dvcrDestinationCIdRBlock = a});

instance AWSRequest DeleteVPNConnectionRoute where
        type Rs DeleteVPNConnectionRoute =
             DeleteVPNConnectionRouteResponse
        request = postQuery eC2
        response
          = receiveNull DeleteVPNConnectionRouteResponse'

instance ToHeaders DeleteVPNConnectionRoute where
        toHeaders = const mempty

instance ToPath DeleteVPNConnectionRoute where
        toPath = const "/"

instance ToQuery DeleteVPNConnectionRoute where
        toQuery DeleteVPNConnectionRoute'{..}
          = mconcat
              ["Action" =:
                 ("DeleteVpnConnectionRoute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "VpnConnectionId" =: _dvcrVPNConnectionId,
               "DestinationCidrBlock" =: _dvcrDestinationCIdRBlock]

-- | /See:/ 'deleteVPNConnectionRouteResponse' smart constructor.
data DeleteVPNConnectionRouteResponse =
    DeleteVPNConnectionRouteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteVPNConnectionRouteResponse' with the minimum fields required to make a request.
--
deleteVPNConnectionRouteResponse
    :: DeleteVPNConnectionRouteResponse
deleteVPNConnectionRouteResponse = DeleteVPNConnectionRouteResponse'
