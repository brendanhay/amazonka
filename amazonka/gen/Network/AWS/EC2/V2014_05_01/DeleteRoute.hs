{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DeleteRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified route from the specified route table. Example This
-- example deletes the route with destination CIDR 172.16.1.0/24 from the
-- specified route table. https://ec2.amazonaws.com/?Action=DeleteRoute
-- &amp;RouteTableId=rtb-e4ad488d &amp;DestinationCidrBlock=172.16.1.0/24
-- &amp;AUTHPARMS &lt;DeleteRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteRouteResponse&gt;.
module Network.AWS.EC2.V2014_05_01.DeleteRoute where

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
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteRoute = DeleteRoute
    { _drrRouteTableId :: Text
      -- ^ The ID of the route table.
    , _drrDestinationCidrBlock :: Text
      -- ^ The CIDR range for the route. The value you specify must match
      -- the CIDR for the route exactly.
    , _drrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DeleteRoute where
    toQuery = genericToQuery def

instance AWSRequest DeleteRoute where
    type Sv DeleteRoute = EC2
    type Rs DeleteRoute = DeleteRouteResponse

    request = post "DeleteRoute"
    response _ _ = return (Right DeleteRouteResponse)

data DeleteRouteResponse = DeleteRouteResponse
    deriving (Eq, Show, Generic)
