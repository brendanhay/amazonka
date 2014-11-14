{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.EC2.DeleteRouteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified route table. You must disassociate the route table
-- from any subnets before you can delete it. You can't delete the main route
-- table.
module Network.AWS.EC2.DeleteRouteTable
    (
    -- * Request
      DeleteRouteTable
    -- ** Request constructor
    , deleteRouteTable
    -- ** Request lenses
    , drt1DryRun
    , drt1RouteTableId

    -- * Response
    , DeleteRouteTableResponse
    -- ** Response constructor
    , deleteRouteTableResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteRouteTable = DeleteRouteTable
    { _drt1DryRun       :: Maybe Bool
    , _drt1RouteTableId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteRouteTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drt1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'drt1RouteTableId' @::@ 'Text'
--
deleteRouteTable :: Text -- ^ 'drt1RouteTableId'
                 -> DeleteRouteTable
deleteRouteTable p1 = DeleteRouteTable
    { _drt1RouteTableId = p1
    , _drt1DryRun       = Nothing
    }

drt1DryRun :: Lens' DeleteRouteTable (Maybe Bool)
drt1DryRun = lens _drt1DryRun (\s a -> s { _drt1DryRun = a })

-- | The ID of the route table.
drt1RouteTableId :: Lens' DeleteRouteTable Text
drt1RouteTableId = lens _drt1RouteTableId (\s a -> s { _drt1RouteTableId = a })

instance ToQuery DeleteRouteTable

instance ToPath DeleteRouteTable where
    toPath = const "/"

data DeleteRouteTableResponse = DeleteRouteTableResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteRouteTableResponse' constructor.
deleteRouteTableResponse :: DeleteRouteTableResponse
deleteRouteTableResponse = DeleteRouteTableResponse

instance AWSRequest DeleteRouteTable where
    type Sv DeleteRouteTable = EC2
    type Rs DeleteRouteTable = DeleteRouteTableResponse

    request  = post "DeleteRouteTable"
    response = nullaryResponse DeleteRouteTableResponse
