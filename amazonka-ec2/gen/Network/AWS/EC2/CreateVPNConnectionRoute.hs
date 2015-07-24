{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPNConnectionRoute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPNConnectionRoute.html>
module Network.AWS.EC2.CreateVPNConnectionRoute
    (
    -- * Request
      CreateVPNConnectionRoute
    -- ** Request constructor
    , createVPNConnectionRoute
    -- ** Request lenses
    , cvcrVPNConnectionId
    , cvcrDestinationCIdRBlock

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
-- * 'cvcrDestinationCIdRBlock'
data CreateVPNConnectionRoute = CreateVPNConnectionRoute'
    { _cvcrVPNConnectionId      :: !Text
    , _cvcrDestinationCIdRBlock :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNConnectionRoute' smart constructor.
createVPNConnectionRoute :: Text -> Text -> CreateVPNConnectionRoute
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
        request = post "CreateVPNConnectionRoute"
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
               "DestinationCidrBlock" =: _cvcrDestinationCIdRBlock]

-- | /See:/ 'createVPNConnectionRouteResponse' smart constructor.
data CreateVPNConnectionRouteResponse =
    CreateVPNConnectionRouteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNConnectionRouteResponse' smart constructor.
createVPNConnectionRouteResponse :: CreateVPNConnectionRouteResponse
createVPNConnectionRouteResponse = CreateVPNConnectionRouteResponse'
