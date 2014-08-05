{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DisableVgwRoutePropagationResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DisableVgwRoutePropagation where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data DisableVgwRoutePropagation = DisableVgwRoutePropagation
    { _dvrprRouteTableId :: Text
      -- ^ The ID of the routing table.
    , _dvrprGatewayId :: Text
      -- ^ The ID of the virtual private gateway.
    } deriving (Show, Generic)

makeLenses ''DisableVgwRoutePropagation

instance ToQuery DisableVgwRoutePropagation where
    toQuery = genericToQuery def

data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
    deriving (Eq, Show, Generic)

makeLenses ''DisableVgwRoutePropagationResponse

instance AWSRequest DisableVgwRoutePropagation where
    type Sv DisableVgwRoutePropagation = EC2
    type Rs DisableVgwRoutePropagation = DisableVgwRoutePropagationResponse

    request = post "DisableVgwRoutePropagation"
    response _ _ = return (Right DisableVgwRoutePropagationResponse)
