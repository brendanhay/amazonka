{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DisableVgwRoutePropagation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables a virtual private gateway (VGW) from propagating routes to the
-- routing tables of a VPC. Example This example disables the virtual private
-- gateway vgw-d8e09e8a from automatically propagating routes to the routing
-- table with ID rtb-c98a35a0.
-- https://ec2.amazonaws.com/?Action=DisableVgwRoutePropagationResponse
-- &amp;RouteTableID=rtb-c98a35a0 &amp;GatewayId= vgw-d8e09e8a &amp;AUTHPARAMS
-- &lt;DisableVgwRoutePropagationResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DisableVgwRoutePropagationResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DisableVgwRoutePropagation
    (
    -- * Request
      DisableVgwRoutePropagation
    -- ** Request constructor
    , mkDisableVgwRoutePropagation
    -- ** Request lenses
    , dvrpRouteTableId
    , dvrpGatewayId

    -- * Response
    , DisableVgwRoutePropagationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DisableVgwRoutePropagation = DisableVgwRoutePropagation
    { _dvrpRouteTableId :: Text
    , _dvrpGatewayId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableVgwRoutePropagation' request.
mkDisableVgwRoutePropagation :: Text -- ^ 'dvrpRouteTableId'
                             -> Text -- ^ 'dvrpGatewayId'
                             -> DisableVgwRoutePropagation
mkDisableVgwRoutePropagation p1 p2 = DisableVgwRoutePropagation
    { _dvrpRouteTableId = p1
    , _dvrpGatewayId = p2
    }
{-# INLINE mkDisableVgwRoutePropagation #-}

-- | The ID of the routing table.
dvrpRouteTableId :: Lens' DisableVgwRoutePropagation Text
dvrpRouteTableId =
    lens _dvrpRouteTableId (\s a -> s { _dvrpRouteTableId = a })
{-# INLINE dvrpRouteTableId #-}

-- | The ID of the virtual private gateway.
dvrpGatewayId :: Lens' DisableVgwRoutePropagation Text
dvrpGatewayId = lens _dvrpGatewayId (\s a -> s { _dvrpGatewayId = a })
{-# INLINE dvrpGatewayId #-}

instance ToQuery DisableVgwRoutePropagation where
    toQuery = genericQuery def

data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DisableVgwRoutePropagation where
    type Sv DisableVgwRoutePropagation = EC2
    type Rs DisableVgwRoutePropagation = DisableVgwRoutePropagationResponse

    request = post "DisableVgwRoutePropagation"
    response _ = nullaryResponse DisableVgwRoutePropagationResponse
