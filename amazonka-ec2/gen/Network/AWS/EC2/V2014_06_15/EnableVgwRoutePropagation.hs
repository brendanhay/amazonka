{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.EnableVgwRoutePropagation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables a virtual private gateway (VGW) to propagate routes to the routing
-- tables of a VPC. Example This example enables the specified virtual private
-- gateway to propagate routes automatically to the routing table with the ID
-- rtb-c98a35a0. https://ec2.amazonaws.com/?Action=EnableVgwRoutePropagation
-- &amp;RouteTableID=rtb-c98a35a0 &amp;GatewayId= vgw-d8e09e8a &amp;AUTHPARAMS
-- &lt;EnableVgwRoutePropagation
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/EnableVgwRoutePropagation&gt;.
module Network.AWS.EC2.V2014_06_15.EnableVgwRoutePropagation where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data EnableVgwRoutePropagation = EnableVgwRoutePropagation
    { _evrprGatewayId :: Text
      -- ^ The ID of the virtual private gateway.
    , _evrprRouteTableId :: Text
      -- ^ The ID of the routing table.
    } deriving (Show, Generic)

makeLenses ''EnableVgwRoutePropagation

instance ToQuery EnableVgwRoutePropagation where
    toQuery = genericQuery def

data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse
    deriving (Eq, Show, Generic)

makeLenses ''EnableVgwRoutePropagationResponse

instance AWSRequest EnableVgwRoutePropagation where
    type Sv EnableVgwRoutePropagation = EC2
    type Rs EnableVgwRoutePropagation = EnableVgwRoutePropagationResponse

    request = post "EnableVgwRoutePropagation"
    response _ = nullaryResponse EnableVgwRoutePropagationResponse
