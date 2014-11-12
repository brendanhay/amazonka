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

-- Module      : Network.AWS.EC2.DeleteRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified route from the specified route table.
module Network.AWS.EC2.DeleteRoute
    (
    -- * Request
      DeleteRoute
    -- ** Request constructor
    , deleteRoute
    -- ** Request lenses
    , dr1DestinationCidrBlock
    , dr1DryRun
    , dr1RouteTableId

    -- * Response
    , DeleteRouteResponse
    -- ** Response constructor
    , deleteRouteResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteRoute = DeleteRoute
    { _dr1DestinationCidrBlock :: Text
    , _dr1DryRun               :: Maybe Bool
    , _dr1RouteTableId         :: Text
    } (Eq, Ord, Show, Generic)

-- | 'DeleteRoute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dr1DestinationCidrBlock' @::@ 'Text'
--
-- * 'dr1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dr1RouteTableId' @::@ 'Text'
--
deleteRoute :: Text -- ^ 'dr1RouteTableId'
            -> Text -- ^ 'dr1DestinationCidrBlock'
            -> DeleteRoute
deleteRoute p1 p2 = DeleteRoute
    { _dr1RouteTableId         = p1
    , _dr1DestinationCidrBlock = p2
    , _dr1DryRun               = Nothing
    }

-- | The CIDR range for the route. The value you specify must match the CIDR
-- for the route exactly.
dr1DestinationCidrBlock :: Lens' DeleteRoute Text
dr1DestinationCidrBlock =
    lens _dr1DestinationCidrBlock (\s a -> s { _dr1DestinationCidrBlock = a })

dr1DryRun :: Lens' DeleteRoute (Maybe Bool)
dr1DryRun = lens _dr1DryRun (\s a -> s { _dr1DryRun = a })

-- | The ID of the route table.
dr1RouteTableId :: Lens' DeleteRoute Text
dr1RouteTableId = lens _dr1RouteTableId (\s a -> s { _dr1RouteTableId = a })
instance ToQuery DeleteRoute

instance ToPath DeleteRoute where
    toPath = const "/"

data DeleteRouteResponse = DeleteRouteResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteRouteResponse' constructor.
deleteRouteResponse :: DeleteRouteResponse
deleteRouteResponse = DeleteRouteResponse

instance FromXML DeleteRouteResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteRouteResponse"

instance AWSRequest DeleteRoute where
    type Sv DeleteRoute = EC2
    type Rs DeleteRoute = DeleteRouteResponse

    request  = post "DeleteRoute"
    response = nullaryResponse DeleteRouteResponse
