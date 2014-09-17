{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteRoute
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
module Network.AWS.EC2.DeleteRoute
    (
    -- * Request
      DeleteRoute
    -- ** Request constructor
    , mkDeleteRoute
    -- ** Request lenses
    , drRouteTableId
    , drDestinationCidrBlock

    -- * Response
    , DeleteRouteResponse
    -- ** Response constructor
    , mkDeleteRouteResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DeleteRoute = DeleteRoute
    { _drRouteTableId :: Text
    , _drDestinationCidrBlock :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRoute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RouteTableId ::@ @Text@
--
-- * @DestinationCidrBlock ::@ @Text@
--
mkDeleteRoute :: Text -- ^ 'drRouteTableId'
              -> Text -- ^ 'drDestinationCidrBlock'
              -> DeleteRoute
mkDeleteRoute p1 p2 = DeleteRoute
    { _drRouteTableId = p1
    , _drDestinationCidrBlock = p2
    }

-- | The ID of the route table.
drRouteTableId :: Lens' DeleteRoute Text
drRouteTableId = lens _drRouteTableId (\s a -> s { _drRouteTableId = a })

-- | The CIDR range for the route. The value you specify must match the CIDR for
-- the route exactly.
drDestinationCidrBlock :: Lens' DeleteRoute Text
drDestinationCidrBlock =
    lens _drDestinationCidrBlock (\s a -> s { _drDestinationCidrBlock = a })

instance ToQuery DeleteRoute where
    toQuery = genericQuery def

data DeleteRouteResponse = DeleteRouteResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRouteResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteRouteResponse :: DeleteRouteResponse
mkDeleteRouteResponse = DeleteRouteResponse

instance AWSRequest DeleteRoute where
    type Sv DeleteRoute = EC2
    type Rs DeleteRoute = DeleteRouteResponse

    request = post "DeleteRoute"
    response _ = nullaryResponse DeleteRouteResponse
