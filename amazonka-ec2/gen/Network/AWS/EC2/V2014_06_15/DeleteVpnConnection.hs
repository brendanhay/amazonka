{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteVpnConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified VPN connection. If you're deleting the VPC and its
-- associated components, we recommend that you detach the virtual private
-- gateway from the VPC and delete the VPC before deleting the VPN connection.
-- Example This example deletes the specified VPN connection.
-- https://ec2.amazonaws.com/?Action=DeleteVpnConnection
-- &amp;vpnConnectionId=vpn-44a8938f &amp;AUTHPARAMS
-- &lt;DeleteVpnConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVpnConnectionResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteVpnConnection where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteVpnConnection' request.
deleteVpnConnection :: Text -- ^ '_dvcrVpnConnectionId'
                    -> DeleteVpnConnection
deleteVpnConnection p1 = DeleteVpnConnection
    { _dvcrVpnConnectionId = p1
    , _dvcrDryRun = Nothing
    }

data DeleteVpnConnection = DeleteVpnConnection
    { _dvcrVpnConnectionId :: Text
      -- ^ The ID of the VPN connection.
    , _dvcrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DeleteVpnConnection

instance ToQuery DeleteVpnConnection where
    toQuery = genericQuery def

data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteVpnConnectionResponse

instance AWSRequest DeleteVpnConnection where
    type Sv DeleteVpnConnection = EC2
    type Rs DeleteVpnConnection = DeleteVpnConnectionResponse

    request = post "DeleteVpnConnection"
    response _ = nullaryResponse DeleteVpnConnectionResponse
