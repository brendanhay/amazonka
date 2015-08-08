{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPNGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual private gateway. We recommend that before
-- you delete a virtual private gateway, you detach it from the VPC and
-- delete the VPN connection. Note that you don\'t need to delete the
-- virtual private gateway if you plan to delete and recreate the VPN
-- connection between your VPC and your network.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVPNGateway.html AWS API Reference> for DeleteVPNGateway.
module Network.AWS.EC2.DeleteVPNGateway
    (
    -- * Creating a Request
      DeleteVPNGateway
    , deleteVPNGateway
    -- * Request Lenses
    , dvgDryRun
    , dvgVPNGatewayId

    -- * Destructuring the Response
    , DeleteVPNGatewayResponse
    , deleteVPNGatewayResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVPNGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvgDryRun'
--
-- * 'dvgVPNGatewayId'
data DeleteVPNGateway = DeleteVPNGateway'
    { _dvgDryRun       :: !(Maybe Bool)
    , _dvgVPNGatewayId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVPNGateway' smart constructor.
deleteVPNGateway :: Text -> DeleteVPNGateway
deleteVPNGateway pVPNGatewayId_ =
    DeleteVPNGateway'
    { _dvgDryRun = Nothing
    , _dvgVPNGatewayId = pVPNGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvgDryRun :: Lens' DeleteVPNGateway (Maybe Bool)
dvgDryRun = lens _dvgDryRun (\ s a -> s{_dvgDryRun = a});

-- | The ID of the virtual private gateway.
dvgVPNGatewayId :: Lens' DeleteVPNGateway Text
dvgVPNGatewayId = lens _dvgVPNGatewayId (\ s a -> s{_dvgVPNGatewayId = a});

instance AWSRequest DeleteVPNGateway where
        type Sv DeleteVPNGateway = EC2
        type Rs DeleteVPNGateway = DeleteVPNGatewayResponse
        request = post
        response = receiveNull DeleteVPNGatewayResponse'

instance ToHeaders DeleteVPNGateway where
        toHeaders = const mempty

instance ToPath DeleteVPNGateway where
        toPath = const "/"

instance ToQuery DeleteVPNGateway where
        toQuery DeleteVPNGateway'{..}
          = mconcat
              ["Action" =: ("DeleteVpnGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dvgDryRun,
               "VpnGatewayId" =: _dvgVPNGatewayId]

-- | /See:/ 'deleteVPNGatewayResponse' smart constructor.
data DeleteVPNGatewayResponse =
    DeleteVPNGatewayResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVPNGatewayResponse' smart constructor.
deleteVPNGatewayResponse :: DeleteVPNGatewayResponse
deleteVPNGatewayResponse = DeleteVPNGatewayResponse'
