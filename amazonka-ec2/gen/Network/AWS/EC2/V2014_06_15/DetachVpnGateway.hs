{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
    -- ** Request constructor
    , mkDetachVpnGateway
    -- ** Request lenses
    , dvg2VpnGatewayId
    , dvg2VpcId

    -- * Response
    , DetachVpnGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DetachVpnGateway = DetachVpnGateway
    { _dvg2VpnGatewayId :: Text
    , _dvg2VpcId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachVpnGateway' request.
mkDetachVpnGateway :: Text -- ^ 'dvg2VpnGatewayId'
                   -> Text -- ^ 'dvg2VpcId'
                   -> DetachVpnGateway
mkDetachVpnGateway p1 p2 = DetachVpnGateway
    { _dvg2VpnGatewayId = p1
    , _dvg2VpcId = p2
    }

-- | The ID of the virtual private gateway.
dvg2VpnGatewayId :: Lens' DetachVpnGateway Text
dvg2VpnGatewayId =
    lens _dvg2VpnGatewayId (\s a -> s { _dvg2VpnGatewayId = a })

-- | The ID of the VPC.
dvg2VpcId :: Lens' DetachVpnGateway Text
dvg2VpcId = lens _dvg2VpcId (\s a -> s { _dvg2VpcId = a })

instance ToQuery DetachVpnGateway where
    toQuery = genericQuery def

data DetachVpnGatewayResponse = DetachVpnGatewayResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DetachVpnGateway where
    type Sv DetachVpnGateway = EC2
    type Rs DetachVpnGateway = DetachVpnGatewayResponse

    request = post "DetachVpnGateway"
    response _ = nullaryResponse DetachVpnGatewayResponse
