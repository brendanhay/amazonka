{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.AttachVpnGateway
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
-- Private Cloud User Guide.
module Network.AWS.EC2.AttachVpnGateway
    (
    -- * Request
      AttachVpnGateway
    -- ** Request constructor
    , attachVpnGateway
    -- ** Request lenses
    , avgDryRun
    , avgVpcId
    , avgVpnGatewayId

    -- * Response
    , AttachVpnGatewayResult
    -- ** Response constructor
    , attachVpnGatewayResult
    -- ** Response lenses
    , avgrVpcAttachment
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data AttachVpnGateway = AttachVpnGateway
    { _avgDryRun       :: Maybe Bool
    , _avgVpcId        :: Text
    , _avgVpnGatewayId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AttachVpnGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'avgVpcId' @::@ 'Text'
--
-- * 'avgVpnGatewayId' @::@ 'Text'
--
attachVpnGateway :: Text -- ^ 'avgVpnGatewayId'
                 -> Text -- ^ 'avgVpcId'
                 -> AttachVpnGateway
attachVpnGateway p1 p2 = AttachVpnGateway
    { _avgVpnGatewayId = p1
    , _avgVpcId        = p2
    , _avgDryRun       = Nothing
    }

avgDryRun :: Lens' AttachVpnGateway (Maybe Bool)
avgDryRun = lens _avgDryRun (\s a -> s { _avgDryRun = a })

-- | The ID of the VPC.
avgVpcId :: Lens' AttachVpnGateway Text
avgVpcId = lens _avgVpcId (\s a -> s { _avgVpcId = a })

-- | The ID of the virtual private gateway.
avgVpnGatewayId :: Lens' AttachVpnGateway Text
avgVpnGatewayId = lens _avgVpnGatewayId (\s a -> s { _avgVpnGatewayId = a })

instance ToPath AttachVpnGateway where
    toPath = const "/"

instance ToQuery AttachVpnGateway

newtype AttachVpnGatewayResult = AttachVpnGatewayResult
    { _avgrVpcAttachment :: Maybe VpcAttachment
    } deriving (Eq, Show, Generic)

-- | 'AttachVpnGatewayResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avgrVpcAttachment' @::@ 'Maybe' 'VpcAttachment'
--
attachVpnGatewayResult :: AttachVpnGatewayResult
attachVpnGatewayResult = AttachVpnGatewayResult
    { _avgrVpcAttachment = Nothing
    }

-- | Information about the attachment.
avgrVpcAttachment :: Lens' AttachVpnGatewayResult (Maybe VpcAttachment)
avgrVpcAttachment =
    lens _avgrVpcAttachment (\s a -> s { _avgrVpcAttachment = a })

instance AWSRequest AttachVpnGateway where
    type Sv AttachVpnGateway = EC2
    type Rs AttachVpnGateway = AttachVpnGatewayResult

    request  = post "AttachVpnGateway"
    response = const . xmlResponse $ \h x -> AttachVpnGatewayResult
newtype
