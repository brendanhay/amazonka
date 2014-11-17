{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpnConnectionRoute
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
-- VPN customer gateway.
--
-- <DeleteVpnConnectionRoute.html>
module Network.AWS.EC2.DeleteVpnConnectionRoute
    (
    -- * Request
      DeleteVpnConnectionRoute
    -- ** Request constructor
    , deleteVpnConnectionRoute
    -- ** Request lenses
    , dvcrDestinationCidrBlock
    , dvcrVpnConnectionId

    -- * Response
    , DeleteVpnConnectionRouteResponse
    -- ** Response constructor
    , deleteVpnConnectionRouteResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { _dvcrDestinationCidrBlock :: Text
    , _dvcrVpnConnectionId      :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpnConnectionRoute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvcrDestinationCidrBlock' @::@ 'Text'
--
-- * 'dvcrVpnConnectionId' @::@ 'Text'
--
deleteVpnConnectionRoute :: Text -- ^ 'dvcrVpnConnectionId'
                         -> Text -- ^ 'dvcrDestinationCidrBlock'
                         -> DeleteVpnConnectionRoute
deleteVpnConnectionRoute p1 p2 = DeleteVpnConnectionRoute
    { _dvcrVpnConnectionId      = p1
    , _dvcrDestinationCidrBlock = p2
    }

-- | The CIDR block associated with the local subnet of the customer network.
dvcrDestinationCidrBlock :: Lens' DeleteVpnConnectionRoute Text
dvcrDestinationCidrBlock =
    lens _dvcrDestinationCidrBlock
        (\s a -> s { _dvcrDestinationCidrBlock = a })

-- | The ID of the VPN connection.
dvcrVpnConnectionId :: Lens' DeleteVpnConnectionRoute Text
dvcrVpnConnectionId =
    lens _dvcrVpnConnectionId (\s a -> s { _dvcrVpnConnectionId = a })

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpnConnectionRouteResponse' constructor.
deleteVpnConnectionRouteResponse :: DeleteVpnConnectionRouteResponse
deleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse

instance AWSRequest DeleteVpnConnectionRoute where
    type Sv DeleteVpnConnectionRoute = EC2
    type Rs DeleteVpnConnectionRoute = DeleteVpnConnectionRouteResponse

    request  = post "DeleteVpnConnectionRoute"
    response = nullResponse DeleteVpnConnectionRouteResponse

instance ToPath DeleteVpnConnectionRoute where
    toPath = const "/"

instance ToHeaders DeleteVpnConnectionRoute

instance ToQuery DeleteVpnConnectionRoute
