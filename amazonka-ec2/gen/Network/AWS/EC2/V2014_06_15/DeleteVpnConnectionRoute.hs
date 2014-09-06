{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteVpnConnectionRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified static route associated with a VPN connection between
-- an existing virtual private gateway and a VPN customer gateway. The static
-- route allows traffic to be routed from the virtual private gateway to the
-- VPN customer gateway. Example This example deletes a static route to the
-- destination CIDR block 11.12.0.0/16 associated with the VPN connection with
-- the ID vpn-83ad48ea. Note that when using the Query API, the "/" is denoted
-- as "%2F". https://ec2.amazonaws.com/?Action=DeleteVpnConnectionRoute
-- &amp;DestinationCidrBlock=11.12.0.0%2F16 &amp;VpnConnectionId=vpn-83ad48ea
-- &amp;AUTHPARAMS &lt;DeleteVpnConnectionRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteVpnConnectionRouteResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteVpnConnectionRoute
    (
    -- * Request
      DeleteVpnConnectionRoute
    -- ** Request constructor
    , mkDeleteVpnConnectionRoute
    -- ** Request lenses
    , dvcrVpnConnectionId
    , dvcrDestinationCidrBlock

    -- * Response
    , DeleteVpnConnectionRouteResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { _dvcrVpnConnectionId :: Text
    , _dvcrDestinationCidrBlock :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpnConnectionRoute' request.
mkDeleteVpnConnectionRoute :: Text -- ^ 'dvcrVpnConnectionId'
                           -> Text -- ^ 'dvcrDestinationCidrBlock'
                           -> DeleteVpnConnectionRoute
mkDeleteVpnConnectionRoute p1 p2 = DeleteVpnConnectionRoute
    { _dvcrVpnConnectionId = p1
    , _dvcrDestinationCidrBlock = p2
    }
{-# INLINE mkDeleteVpnConnectionRoute #-}

-- | The ID of the VPN connection.
dvcrVpnConnectionId :: Lens' DeleteVpnConnectionRoute Text
dvcrVpnConnectionId =
    lens _dvcrVpnConnectionId (\s a -> s { _dvcrVpnConnectionId = a })
{-# INLINE dvcrVpnConnectionId #-}

-- | The CIDR block associated with the local subnet of the customer network.
dvcrDestinationCidrBlock :: Lens' DeleteVpnConnectionRoute Text
dvcrDestinationCidrBlock =
    lens _dvcrDestinationCidrBlock
         (\s a -> s { _dvcrDestinationCidrBlock = a })
{-# INLINE dvcrDestinationCidrBlock #-}

instance ToQuery DeleteVpnConnectionRoute where
    toQuery = genericQuery def

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteVpnConnectionRoute where
    type Sv DeleteVpnConnectionRoute = EC2
    type Rs DeleteVpnConnectionRoute = DeleteVpnConnectionRouteResponse

    request = post "DeleteVpnConnectionRoute"
    response _ = nullaryResponse DeleteVpnConnectionRouteResponse
