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

-- Module      : Network.AWS.EC2.CreateRouteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a route table for the specified VPC. After you create a route
-- table, you can add routes and associate the table with a subnet. For more
-- information about route tables, see Route Tables in the Amazon Virtual
-- Private Cloud User Guide.
module Network.AWS.EC2.CreateRouteTable
    (
    -- * Request
      CreateRouteTable
    -- ** Request constructor
    , createRouteTable
    -- ** Request lenses
    , crtDryRun
    , crtVpcId

    -- * Response
    , CreateRouteTableResponse
    -- ** Response constructor
    , createRouteTableResponse
    -- ** Response lenses
    , crtrRouteTable
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateRouteTable = CreateRouteTable
    { _crtDryRun :: Maybe Bool
    , _crtVpcId  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateRouteTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crtDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'crtVpcId' @::@ 'Text'
--
createRouteTable :: Text -- ^ 'crtVpcId'
                 -> CreateRouteTable
createRouteTable p1 = CreateRouteTable
    { _crtVpcId  = p1
    , _crtDryRun = Nothing
    }

crtDryRun :: Lens' CreateRouteTable (Maybe Bool)
crtDryRun = lens _crtDryRun (\s a -> s { _crtDryRun = a })

-- | The ID of the VPC.
crtVpcId :: Lens' CreateRouteTable Text
crtVpcId = lens _crtVpcId (\s a -> s { _crtVpcId = a })

instance ToQuery CreateRouteTable

instance ToPath CreateRouteTable where
    toPath = const "/"

newtype CreateRouteTableResponse = CreateRouteTableResponse
    { _crtrRouteTable :: Maybe RouteTable
    } deriving (Eq, Show, Generic)

-- | 'CreateRouteTableResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crtrRouteTable' @::@ 'Maybe' 'RouteTable'
--
createRouteTableResponse :: CreateRouteTableResponse
createRouteTableResponse = CreateRouteTableResponse
    { _crtrRouteTable = Nothing
    }

-- | Information about the route table.
crtrRouteTable :: Lens' CreateRouteTableResponse (Maybe RouteTable)
crtrRouteTable = lens _crtrRouteTable (\s a -> s { _crtrRouteTable = a })

instance AWSRequest CreateRouteTable where
    type Sv CreateRouteTable = EC2
    type Rs CreateRouteTable = CreateRouteTableResponse

    request  = post "CreateRouteTable"
    response = xmlResponse $ \h x -> CreateRouteTableResponse
        <$> x %| "routeTable"
