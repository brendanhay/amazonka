{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DetachVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches a virtual private gateway from a VPC. You do this if you're
-- planning to turn off the VPC and not use it anymore. You can confirm a
-- virtual private gateway has been completely detached from a VPC by
-- describing the virtual private gateway (any attachments to the virtual
-- private gateway are also described). You must wait for the attachment's
-- state to switch to detached before you can delete the VPC or attach a
-- different VPC to the virtual private gateway. Example This example detaches
-- the specified virtual private gateway from the specified VPC.
-- https://ec2.amazonaws.com/?Action=DetachVpnGateway
-- &amp;VpnGatewayId=vgw-8db04f81 &amp;VpcId=vpc-1a2b3c4d &amp;AUTHPARAMS
-- &lt;DetachVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DetachVpnGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DetachVpnGateway
    (
    -- * Request
      DetachVpnGateway
    -- ** Default constructor
    , detachVpnGateway
    -- ** Accessors and lenses
    , _dvguVpnGatewayId
    , dvguVpnGatewayId
    , _dvguVpcId
    , dvguVpcId

    -- * Response
    , DetachVpnGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DetachVpnGateway' request.
detachVpnGateway :: Text -- ^ 'dvguVpnGatewayId'
                 -> Text -- ^ 'dvguVpcId'
                 -> DetachVpnGateway
detachVpnGateway p1 p2 = DetachVpnGateway
    { _dvguVpnGatewayId = p1
    , _dvguVpcId = p2
    }

data DetachVpnGateway = DetachVpnGateway

makeSiglessLenses ''DetachVpnGateway

instance ToQuery DetachVpnGateway where
    toQuery = genericQuery def

data DetachVpnGatewayResponse = DetachVpnGatewayResponse
    deriving (Eq, Show, Generic)

makeSiglessLenses ''DetachVpnGatewayResponse

instance AWSRequest DetachVpnGateway where
    type Sv DetachVpnGateway = EC2
    type Rs DetachVpnGateway = DetachVpnGatewayResponse

    request = post "DetachVpnGateway"
    response _ = nullaryResponse DetachVpnGatewayResponse

-- | The ID of the virtual private gateway.
dvguVpnGatewayId :: Lens' DetachVpnGateway (Text)

-- | The ID of the VPC.
dvguVpcId :: Lens' DetachVpnGateway (Text)
