{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AttachVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches a virtual private gateway to a VPC. For more information, see
-- Adding a Hardware Virtual Private Gateway to Your VPC in the Amazon Virtual
-- Private Cloud User Guide. Example This example attaches the virtual private
-- gateway with the ID vgw-8db04f81 to the VPC with the ID vpc-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=AttachVpnGateway
-- &amp;VpnGatewayId=vgw-8db04f81 &amp;VpcId=vpc-1a2b3c4d &amp;AUTHPARAMS
-- &lt;AttachVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;attachment&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;state&gt;attaching&lt;/state&gt; &lt;/attachment&gt;
-- &lt;/AttachVpnGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.AttachVpnGateway
    (
    -- * Request
      AttachVpnGateway
    -- ** Default constructor
    , attachVpnGateway
    -- ** Accessors and lenses
    , _avgrVpnGatewayId
    , avgrVpnGatewayId
    , _avgrVpcId
    , avgrVpcId

    -- * Response
    , AttachVpnGatewayResponse
    -- ** Accessors and lenses
    , _avgsVpcAttachment
    , avgsVpcAttachment
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AttachVpnGateway' request.
attachVpnGateway :: Text -- ^ 'avgrVpnGatewayId'
                 -> Text -- ^ 'avgrVpcId'
                 -> AttachVpnGateway
attachVpnGateway p1 p2 = AttachVpnGateway
    { _avgrVpnGatewayId = p1
    , _avgrVpcId = p2
    }

data AttachVpnGateway = AttachVpnGateway

makeSiglessLenses ''AttachVpnGateway

instance ToQuery AttachVpnGateway where
    toQuery = genericQuery def

data AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { _avgsVpcAttachment :: Maybe VpcAttachment
      -- ^ Information about the attachment.
    } deriving (Show, Generic)

makeSiglessLenses ''AttachVpnGatewayResponse

instance FromXML AttachVpnGatewayResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachVpnGateway where
    type Sv AttachVpnGateway = EC2
    type Rs AttachVpnGateway = AttachVpnGatewayResponse

    request = post "AttachVpnGateway"
    response _ = xmlResponse

-- | The ID of the virtual private gateway.
avgrVpnGatewayId :: Lens' AttachVpnGateway (Text)

-- | The ID of the VPC.
avgrVpcId :: Lens' AttachVpnGateway (Text)

-- | Information about the attachment.
avgsVpcAttachment :: Lens' AttachVpnGatewayResponse (Maybe VpcAttachment)
