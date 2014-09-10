{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- table. Example This example deletes the specified route table.
-- https://ec2.amazonaws.com/?Action=DeleteRouteTable
-- &amp;RouteTableId=rtb-e4ad488d &amp;AUTHPARAMS &lt;DeleteRouteTableResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteRouteTableResponse&gt;.
module Network.AWS.EC2.DeleteRouteTable
    (
    -- * Request
      DeleteRouteTable
    -- ** Request constructor
    , mkDeleteRouteTable
    -- ** Request lenses
    , drtRouteTableId

    -- * Response
    , DeleteRouteTableResponse
    -- ** Response constructor
    , mkDeleteRouteTableResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteRouteTable = DeleteRouteTable
    { _drtRouteTableId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRouteTable' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RouteTableId ::@ @Text@
--
mkDeleteRouteTable :: Text -- ^ 'drtRouteTableId'
                   -> DeleteRouteTable
mkDeleteRouteTable p1 = DeleteRouteTable
    { _drtRouteTableId = p1
    }

-- | The ID of the route table.
drtRouteTableId :: Lens' DeleteRouteTable Text
drtRouteTableId = lens _drtRouteTableId (\s a -> s { _drtRouteTableId = a })

instance ToQuery DeleteRouteTable where
    toQuery = genericQuery def

data DeleteRouteTableResponse = DeleteRouteTableResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRouteTableResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteRouteTableResponse :: DeleteRouteTableResponse
mkDeleteRouteTableResponse = DeleteRouteTableResponse

instance AWSRequest DeleteRouteTable where
    type Sv DeleteRouteTable = EC2
    type Rs DeleteRouteTable = DeleteRouteTableResponse

    request = post "DeleteRouteTable"
    response _ = nullaryResponse DeleteRouteTableResponse
