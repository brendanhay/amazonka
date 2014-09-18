{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpnConnection
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
module Network.AWS.EC2.DeleteVpnConnection
    (
    -- * Request
      DeleteVpnConnection
    -- ** Request constructor
    , deleteVpnConnection
    -- ** Request lenses
    , dvcVpnConnectionId

    -- * Response
    , DeleteVpnConnectionResponse
    -- ** Response constructor
    , deleteVpnConnectionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteVpnConnection = DeleteVpnConnection
    { _dvcVpnConnectionId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpnConnection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpnConnectionId ::@ @Text@
--
deleteVpnConnection :: Text -- ^ 'dvcVpnConnectionId'
                      -> DeleteVpnConnection
deleteVpnConnection p1 = DeleteVpnConnection
    { _dvcVpnConnectionId = p1
    }

-- | The ID of the VPN connection.
dvcVpnConnectionId :: Lens' DeleteVpnConnection Text
dvcVpnConnectionId =
    lens _dvcVpnConnectionId (\s a -> s { _dvcVpnConnectionId = a })

instance ToQuery DeleteVpnConnection where
    toQuery = genericQuery def

data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpnConnectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteVpnConnectionResponse :: DeleteVpnConnectionResponse
deleteVpnConnectionResponse = DeleteVpnConnectionResponse

instance AWSRequest DeleteVpnConnection where
    type Sv DeleteVpnConnection = EC2
    type Rs DeleteVpnConnection = DeleteVpnConnectionResponse

    request = post "DeleteVpnConnection"
    response _ = nullaryResponse DeleteVpnConnectionResponse
