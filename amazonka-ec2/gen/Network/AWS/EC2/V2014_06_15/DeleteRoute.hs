{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteRoute
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
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteRouteResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteRoute where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data DeleteRoute = DeleteRoute
    { _drrRouteTableId :: Text
      -- ^ The ID of the route table.
    , _drrDestinationCidrBlock :: Text
      -- ^ The CIDR range for the route. The value you specify must match
      -- the CIDR for the route exactly.
    } deriving (Show, Generic)

makeLenses ''DeleteRoute

instance ToQuery DeleteRoute where
    toQuery = genericQuery def

data DeleteRouteResponse = DeleteRouteResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteRouteResponse

instance AWSRequest DeleteRoute where
    type Sv DeleteRoute = EC2
    type Rs DeleteRoute = DeleteRouteResponse

    request = post "DeleteRoute"
    response _ = nullaryResponse DeleteRouteResponse
