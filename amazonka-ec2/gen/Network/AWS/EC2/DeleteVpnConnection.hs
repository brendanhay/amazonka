{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- If you believe that the tunnel credentials for your VPN connection have
-- been compromised, you can delete the VPN connection and create a new one
-- that has new keys, without needing to delete the VPC or virtual private
-- gateway. If you create a new VPN connection, you must reconfigure the
-- customer gateway using the new configuration information returned with the
-- new VPN connection ID.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnConnection.html>
module Network.AWS.EC2.DeleteVpnConnection
    (
    -- * Request
      DeleteVpnConnection
    -- ** Request constructor
    , deleteVpnConnection
    -- ** Request lenses
    , dvcDryRun
    , dvcVpnConnectionId

    -- * Response
    , DeleteVpnConnectionResponse
    -- ** Response constructor
    , deleteVpnConnectionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteVpnConnection = DeleteVpnConnection
    { _dvcDryRun          :: Maybe Bool
    , _dvcVpnConnectionId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpnConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvcDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvcVpnConnectionId' @::@ 'Text'
--
deleteVpnConnection :: Text -- ^ 'dvcVpnConnectionId'
                    -> DeleteVpnConnection
deleteVpnConnection p1 = DeleteVpnConnection
    { _dvcVpnConnectionId = p1
    , _dvcDryRun          = Nothing
    }

dvcDryRun :: Lens' DeleteVpnConnection (Maybe Bool)
dvcDryRun = lens _dvcDryRun (\s a -> s { _dvcDryRun = a })

-- | The ID of the VPN connection.
dvcVpnConnectionId :: Lens' DeleteVpnConnection Text
dvcVpnConnectionId =
    lens _dvcVpnConnectionId (\s a -> s { _dvcVpnConnectionId = a })

data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpnConnectionResponse' constructor.
deleteVpnConnectionResponse :: DeleteVpnConnectionResponse
deleteVpnConnectionResponse = DeleteVpnConnectionResponse

instance ToPath DeleteVpnConnection where
    toPath = const "/"

instance ToQuery DeleteVpnConnection

instance ToHeaders DeleteVpnConnection

instance AWSRequest DeleteVpnConnection where
    type Sv DeleteVpnConnection = EC2
    type Rs DeleteVpnConnection = DeleteVpnConnectionResponse

    request  = post "DeleteVpnConnection"
    response = nullResponse DeleteVpnConnectionResponse
