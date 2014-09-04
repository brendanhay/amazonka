{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteVpnGateway
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
module Network.AWS.EC2.V2014_06_15.DeleteVpnGateway
    (
    -- * Request
      DeleteVpnGateway
    -- ** Request constructor
    , deleteVpnGateway
    -- ** Request lenses
    , dvgrVpnGatewayId

    -- * Response
    , DeleteVpnGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteVpnGateway' request.
deleteVpnGateway :: Text -- ^ 'dvgrVpnGatewayId'
                 -> DeleteVpnGateway
deleteVpnGateway p1 = DeleteVpnGateway
    { _dvgrVpnGatewayId = p1
    }
{-# INLINE deleteVpnGateway #-}

data DeleteVpnGateway = DeleteVpnGateway
    { _dvgrVpnGatewayId :: Text
      -- ^ The ID of the virtual private gateway.
    } deriving (Show, Generic)

-- | The ID of the virtual private gateway.
dvgrVpnGatewayId :: Lens' DeleteVpnGateway (Text)
dvgrVpnGatewayId f x =
    f (_dvgrVpnGatewayId x)
        <&> \y -> x { _dvgrVpnGatewayId = y }
{-# INLINE dvgrVpnGatewayId #-}

instance ToQuery DeleteVpnGateway where
    toQuery = genericQuery def

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteVpnGateway where
    type Sv DeleteVpnGateway = EC2
    type Rs DeleteVpnGateway = DeleteVpnGatewayResponse

    request = post "DeleteVpnGateway"
    response _ = nullaryResponse DeleteVpnGatewayResponse
