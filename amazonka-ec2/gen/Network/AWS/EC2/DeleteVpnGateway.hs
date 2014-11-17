{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
-- VPC and your network.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnGateway.html>
module Network.AWS.EC2.DeleteVpnGateway
    (
    -- * Request
      DeleteVpnGateway
    -- ** Request constructor
    , deleteVpnGateway
    -- ** Request lenses
    , dvgDryRun
    , dvgVpnGatewayId

    -- * Response
    , DeleteVpnGatewayResponse
    -- ** Response constructor
    , deleteVpnGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteVpnGateway = DeleteVpnGateway
    { _dvgDryRun       :: Maybe Bool
    , _dvgVpnGatewayId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpnGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvgVpnGatewayId' @::@ 'Text'
--
deleteVpnGateway :: Text -- ^ 'dvgVpnGatewayId'
                 -> DeleteVpnGateway
deleteVpnGateway p1 = DeleteVpnGateway
    { _dvgVpnGatewayId = p1
    , _dvgDryRun       = Nothing
    }

dvgDryRun :: Lens' DeleteVpnGateway (Maybe Bool)
dvgDryRun = lens _dvgDryRun (\s a -> s { _dvgDryRun = a })

-- | The ID of the virtual private gateway.
dvgVpnGatewayId :: Lens' DeleteVpnGateway Text
dvgVpnGatewayId = lens _dvgVpnGatewayId (\s a -> s { _dvgVpnGatewayId = a })

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpnGatewayResponse' constructor.
deleteVpnGatewayResponse :: DeleteVpnGatewayResponse
deleteVpnGatewayResponse = DeleteVpnGatewayResponse

instance AWSRequest DeleteVpnGateway where
    type Sv DeleteVpnGateway = EC2
    type Rs DeleteVpnGateway = DeleteVpnGatewayResponse

    request  = post "DeleteVpnGateway"
    response = nullResponse DeleteVpnGatewayResponse

instance ToPath DeleteVpnGateway where
    toPath = const "/"

instance ToHeaders DeleteVpnGateway

instance ToQuery DeleteVpnGateway where
    toQuery DeleteVpnGateway{..} = mconcat
        [ "dryRun"       =? _dvgDryRun
        , "VpnGatewayId" =? _dvgVpnGatewayId
        ]

instance ToXML DeleteVpnGateway where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteVpnGateway"
