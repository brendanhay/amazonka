{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DeleteRouteTable
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
module Network.AWS.EC2.V2014_05_01.DeleteRouteTable where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteRouteTable = DeleteRouteTable
    { _drtsRouteTableId :: Text
      -- ^ The ID of the route table.
    , _drtsDryRun :: Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DeleteRouteTable where
    toQuery = genericToQuery def

instance AWSRequest DeleteRouteTable where
    type Sv DeleteRouteTable = EC2
    type Rs DeleteRouteTable = DeleteRouteTableResponse

    request = post "DeleteRouteTable"

    response _ _ = return (Right DeleteRouteTableResponse)

data DeleteRouteTableResponse = DeleteRouteTableResponse
    deriving (Eq, Show, Generic)
