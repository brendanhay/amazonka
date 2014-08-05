{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteRouteTable
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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteRouteTableResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteRouteTable where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteRouteTable' request.
deleteRouteTable :: Text -- ^ '_drtsRouteTableId'
                 -> DeleteRouteTable
deleteRouteTable p1 = DeleteRouteTable
    { _drtsRouteTableId = p1
    , _drtsDryRun = Nothing
    }

data DeleteRouteTable = DeleteRouteTable
    { _drtsRouteTableId :: Text
      -- ^ The ID of the route table.
    , _drtsDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DeleteRouteTable

instance ToQuery DeleteRouteTable where
    toQuery = genericToQuery def

data DeleteRouteTableResponse = DeleteRouteTableResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteRouteTableResponse

instance AWSRequest DeleteRouteTable where
    type Sv DeleteRouteTable = EC2
    type Rs DeleteRouteTable = DeleteRouteTableResponse

    request = post "DeleteRouteTable"
    response _ _ = return (Right DeleteRouteTableResponse)
