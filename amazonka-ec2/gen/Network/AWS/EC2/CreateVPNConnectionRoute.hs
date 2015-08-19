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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route associated with a VPN connection between an
-- existing virtual private gateway and a VPN customer gateway. The static
-- route allows traffic to be routed from the virtual private gateway to
-- the VPN customer gateway.
--
-- For more information about VPN connections, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPNConnectionRoute.html AWS API Reference> for CreateVPNConnectionRoute.
module Network.AWS.EC2.CreateVPNConnectionRoute
    (
    -- * Creating a Request
      createVPNConnectionRoute
    , CreateVPNConnectionRoute
    -- * Request Lenses
    , cvcrVPNConnectionId
    , cvcrDestinationCIdRBlock

    -- * Destructuring the Response
    , createVPNConnectionRouteResponse
    , CreateVPNConnectionRouteResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVPNConnectionRoute' smart constructor.
data CreateVPNConnectionRoute = CreateVPNConnectionRoute'
    { _cvcrVPNConnectionId      :: !Text
    , _cvcrDestinationCIdRBlock :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateVPNConnectionRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvcrVPNConnectionId'
--
-- * 'cvcrDestinationCIdRBlock'
createVPNConnectionRoute
    :: Text -- ^ 'cvcrVPNConnectionId'
    -> Text -- ^ 'cvcrDestinationCIdRBlock'
    -> CreateVPNConnectionRoute
createVPNConnectionRoute pVPNConnectionId_ pDestinationCIdRBlock_ =
    CreateVPNConnectionRoute'
    { _cvcrVPNConnectionId = pVPNConnectionId_
    , _cvcrDestinationCIdRBlock = pDestinationCIdRBlock_
    }

-- | The ID of the VPN connection.
cvcrVPNConnectionId :: Lens' CreateVPNConnectionRoute Text
cvcrVPNConnectionId = lens _cvcrVPNConnectionId (\ s a -> s{_cvcrVPNConnectionId = a});

-- | The CIDR block associated with the local subnet of the customer network.
cvcrDestinationCIdRBlock :: Lens' CreateVPNConnectionRoute Text
cvcrDestinationCIdRBlock = lens _cvcrDestinationCIdRBlock (\ s a -> s{_cvcrDestinationCIdRBlock = a});

instance AWSRequest CreateVPNConnectionRoute where
        type Sv CreateVPNConnectionRoute = EC2
        type Rs CreateVPNConnectionRoute =
             CreateVPNConnectionRouteResponse
        request = postQuery
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
                 ("CreateVpnConnectionRoute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "VpnConnectionId" =: _cvcrVPNConnectionId,
               "DestinationCidrBlock" =: _cvcrDestinationCIdRBlock]

-- | /See:/ 'createVPNConnectionRouteResponse' smart constructor.
data CreateVPNConnectionRouteResponse =
    CreateVPNConnectionRouteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateVPNConnectionRouteResponse' with the minimum fields required to make a request.
--
createVPNConnectionRouteResponse
    :: CreateVPNConnectionRouteResponse
createVPNConnectionRouteResponse = CreateVPNConnectionRouteResponse'
