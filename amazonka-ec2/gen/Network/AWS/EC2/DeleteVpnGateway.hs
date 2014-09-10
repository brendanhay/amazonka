{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified virtual private gateway. We recommend that before you
-- delete a virtual private gateway, you detach it from the VPC and delete the
-- VPN connection. Note that you don't need to delete the virtual private
-- gateway if you plan to delete and recreate the VPN connection between your
-- VPC and your network. Example This example deletes the specified virtual
-- private gateway. https://ec2.amazonaws.com/?Action=DeleteVpnGateway
-- &amp;vpnGatewayId=vgw-8db04f81 &amp;AUTHPARAMS &lt;DeleteVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVpnGatewayResponse&gt;.
module Network.AWS.EC2.DeleteVpnGateway
    (
    -- * Request
      DeleteVpnGateway
    -- ** Request constructor
    , mkDeleteVpnGateway
    -- ** Request lenses
    , dvgVpnGatewayId

    -- * Response
    , DeleteVpnGatewayResponse
    -- ** Response constructor
    , mkDeleteVpnGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteVpnGateway = DeleteVpnGateway
    { _dvgVpnGatewayId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpnGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpnGatewayId ::@ @Text@
--
mkDeleteVpnGateway :: Text -- ^ 'dvgVpnGatewayId'
                   -> DeleteVpnGateway
mkDeleteVpnGateway p1 = DeleteVpnGateway
    { _dvgVpnGatewayId = p1
    }

-- | The ID of the virtual private gateway.
dvgVpnGatewayId :: Lens' DeleteVpnGateway Text
dvgVpnGatewayId = lens _dvgVpnGatewayId (\s a -> s { _dvgVpnGatewayId = a })

instance ToQuery DeleteVpnGateway where
    toQuery = genericQuery def

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpnGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteVpnGatewayResponse :: DeleteVpnGatewayResponse
mkDeleteVpnGatewayResponse = DeleteVpnGatewayResponse

instance AWSRequest DeleteVpnGateway where
    type Sv DeleteVpnGateway = EC2
    type Rs DeleteVpnGateway = DeleteVpnGatewayResponse

    request = post "DeleteVpnGateway"
    response _ = nullaryResponse DeleteVpnGatewayResponse
