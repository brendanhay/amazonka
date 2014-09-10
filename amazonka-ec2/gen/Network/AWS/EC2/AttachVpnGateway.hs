{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
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
module Network.AWS.EC2
    (
    -- * Request
      AttachVpnGateway
    -- ** Request constructor
    , mkAttachVpnGateway
    -- ** Request lenses
    , avgVpnGatewayId
    , avgVpcId

    -- * Response
    , AttachVpnGatewayResponse
    -- ** Response constructor
    , mkAttachVpnGatewayResponse
    -- ** Response lenses
    , avgrVpcAttachment
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data AttachVpnGateway = AttachVpnGateway
    { _avgVpnGatewayId :: !Text
    , _avgVpcId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachVpnGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpnGatewayId ::@ @Text@
--
-- * @VpcId ::@ @Text@
--
mkAttachVpnGateway :: Text -- ^ 'avgVpnGatewayId'
                   -> Text -- ^ 'avgVpcId'
                   -> AttachVpnGateway
mkAttachVpnGateway p1 p2 = AttachVpnGateway
    { _avgVpnGatewayId = p1
    , _avgVpcId = p2
    }

-- | The ID of the virtual private gateway.
avgVpnGatewayId :: Lens' AttachVpnGateway Text
avgVpnGatewayId = lens _avgVpnGatewayId (\s a -> s { _avgVpnGatewayId = a })

-- | The ID of the VPC.
avgVpcId :: Lens' AttachVpnGateway Text
avgVpcId = lens _avgVpcId (\s a -> s { _avgVpcId = a })

instance ToQuery AttachVpnGateway where
    toQuery = genericQuery def

newtype AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { _avgrVpcAttachment :: Maybe VpcAttachment
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachVpnGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcAttachment ::@ @Maybe VpcAttachment@
--
mkAttachVpnGatewayResponse :: AttachVpnGatewayResponse
mkAttachVpnGatewayResponse = AttachVpnGatewayResponse
    { _avgrVpcAttachment = Nothing
    }

-- | Information about the attachment.
avgrVpcAttachment :: Lens' AttachVpnGatewayResponse (Maybe VpcAttachment)
avgrVpcAttachment =
    lens _avgrVpcAttachment (\s a -> s { _avgrVpcAttachment = a })

instance FromXML AttachVpnGatewayResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachVpnGateway where
    type Sv AttachVpnGateway = EC2
    type Rs AttachVpnGateway = AttachVpnGatewayResponse

    request = post "AttachVpnGateway"
    response _ = xmlResponse
