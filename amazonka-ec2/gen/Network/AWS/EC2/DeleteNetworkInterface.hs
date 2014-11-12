{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DeleteNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified network interface. You must detach the network
-- interface before you can delete it.
module Network.AWS.EC2.DeleteNetworkInterface
    (
    -- * Request
      DeleteNetworkInterface
    -- ** Request constructor
    , deleteNetworkInterface
    -- ** Request lenses
    , dni1DryRun
    , dni1NetworkInterfaceId

    -- * Response
    , DeleteNetworkInterfaceResponse
    -- ** Response constructor
    , deleteNetworkInterfaceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteNetworkInterface = DeleteNetworkInterface
    { _dni1DryRun             :: Maybe Bool
    , _dni1NetworkInterfaceId :: Text
    } (Eq, Ord, Show, Generic)

-- | 'DeleteNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dni1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dni1NetworkInterfaceId' @::@ 'Text'
--
deleteNetworkInterface :: Text -- ^ 'dni1NetworkInterfaceId'
                       -> DeleteNetworkInterface
deleteNetworkInterface p1 = DeleteNetworkInterface
    { _dni1NetworkInterfaceId = p1
    , _dni1DryRun             = Nothing
    }

dni1DryRun :: Lens' DeleteNetworkInterface (Maybe Bool)
dni1DryRun = lens _dni1DryRun (\s a -> s { _dni1DryRun = a })

-- | The ID of the network interface.
dni1NetworkInterfaceId :: Lens' DeleteNetworkInterface Text
dni1NetworkInterfaceId =
    lens _dni1NetworkInterfaceId (\s a -> s { _dni1NetworkInterfaceId = a })
instance ToQuery DeleteNetworkInterface

instance ToPath DeleteNetworkInterface where
    toPath = const "/"

data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteNetworkInterfaceResponse' constructor.
deleteNetworkInterfaceResponse :: DeleteNetworkInterfaceResponse
deleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse

instance FromXML DeleteNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteNetworkInterfaceResponse"

instance AWSRequest DeleteNetworkInterface where
    type Sv DeleteNetworkInterface = EC2
    type Rs DeleteNetworkInterface = DeleteNetworkInterfaceResponse

    request  = post "DeleteNetworkInterface"
    response = nullaryResponse DeleteNetworkInterfaceResponse
