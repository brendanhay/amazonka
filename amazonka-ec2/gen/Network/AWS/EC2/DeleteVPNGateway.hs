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
-- Module      : Network.AWS.EC2.DeleteVPNGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual private gateway. We recommend that before you delete a virtual private gateway, you detach it from the VPC and delete the VPN connection. Note that you don't need to delete the virtual private gateway if you plan to delete and recreate the VPN connection between your VPC and your network.
--
--
module Network.AWS.EC2.DeleteVPNGateway
    (
    -- * Creating a Request
      deleteVPNGateway
    , DeleteVPNGateway
    -- * Request Lenses
    , dvgDryRun
    , dvgVPNGatewayId

    -- * Destructuring the Response
    , deleteVPNGatewayResponse
    , DeleteVPNGatewayResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeleteVpnGateway.
--
--
--
-- /See:/ 'deleteVPNGateway' smart constructor.
data DeleteVPNGateway = DeleteVPNGateway'
  { _dvgDryRun       :: !(Maybe Bool)
  , _dvgVPNGatewayId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPNGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvgVPNGatewayId' - The ID of the virtual private gateway.
deleteVPNGateway
    :: Text -- ^ 'dvgVPNGatewayId'
    -> DeleteVPNGateway
deleteVPNGateway pVPNGatewayId_ =
  DeleteVPNGateway' {_dvgDryRun = Nothing, _dvgVPNGatewayId = pVPNGatewayId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvgDryRun :: Lens' DeleteVPNGateway (Maybe Bool)
dvgDryRun = lens _dvgDryRun (\ s a -> s{_dvgDryRun = a})

-- | The ID of the virtual private gateway.
dvgVPNGatewayId :: Lens' DeleteVPNGateway Text
dvgVPNGatewayId = lens _dvgVPNGatewayId (\ s a -> s{_dvgVPNGatewayId = a})

instance AWSRequest DeleteVPNGateway where
        type Rs DeleteVPNGateway = DeleteVPNGatewayResponse
        request = postQuery ec2
        response = receiveNull DeleteVPNGatewayResponse'

instance Hashable DeleteVPNGateway where

instance NFData DeleteVPNGateway where

instance ToHeaders DeleteVPNGateway where
        toHeaders = const mempty

instance ToPath DeleteVPNGateway where
        toPath = const "/"

instance ToQuery DeleteVPNGateway where
        toQuery DeleteVPNGateway'{..}
          = mconcat
              ["Action" =: ("DeleteVpnGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dvgDryRun,
               "VpnGatewayId" =: _dvgVPNGatewayId]

-- | /See:/ 'deleteVPNGatewayResponse' smart constructor.
data DeleteVPNGatewayResponse =
  DeleteVPNGatewayResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPNGatewayResponse' with the minimum fields required to make a request.
--
deleteVPNGatewayResponse
    :: DeleteVPNGatewayResponse
deleteVPNGatewayResponse = DeleteVPNGatewayResponse'


instance NFData DeleteVPNGatewayResponse where
