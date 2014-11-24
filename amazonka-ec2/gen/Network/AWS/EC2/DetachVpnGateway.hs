{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DetachVpnGateway
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
-- state to switch to 'detached' before you can delete the VPC or attach a
-- different VPC to the virtual private gateway.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVpnGateway.html>
module Network.AWS.EC2.DetachVpnGateway
    (
    -- * Request
      DetachVpnGateway
    -- ** Request constructor
    , detachVpnGateway
    -- ** Request lenses
    , dvg1DryRun
    , dvg1VpcId
    , dvg1VpnGatewayId

    -- * Response
    , DetachVpnGatewayResponse
    -- ** Response constructor
    , detachVpnGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DetachVpnGateway = DetachVpnGateway
    { _dvg1DryRun       :: Maybe Bool
    , _dvg1VpcId        :: Text
    , _dvg1VpnGatewayId :: Text
    } deriving (Eq, Ord, Show)

-- | 'DetachVpnGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvg1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvg1VpcId' @::@ 'Text'
--
-- * 'dvg1VpnGatewayId' @::@ 'Text'
--
detachVpnGateway :: Text -- ^ 'dvg1VpnGatewayId'
                 -> Text -- ^ 'dvg1VpcId'
                 -> DetachVpnGateway
detachVpnGateway p1 p2 = DetachVpnGateway
    { _dvg1VpnGatewayId = p1
    , _dvg1VpcId        = p2
    , _dvg1DryRun       = Nothing
    }

dvg1DryRun :: Lens' DetachVpnGateway (Maybe Bool)
dvg1DryRun = lens _dvg1DryRun (\s a -> s { _dvg1DryRun = a })

-- | The ID of the VPC.
dvg1VpcId :: Lens' DetachVpnGateway Text
dvg1VpcId = lens _dvg1VpcId (\s a -> s { _dvg1VpcId = a })

-- | The ID of the virtual private gateway.
dvg1VpnGatewayId :: Lens' DetachVpnGateway Text
dvg1VpnGatewayId = lens _dvg1VpnGatewayId (\s a -> s { _dvg1VpnGatewayId = a })

data DetachVpnGatewayResponse = DetachVpnGatewayResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DetachVpnGatewayResponse' constructor.
detachVpnGatewayResponse :: DetachVpnGatewayResponse
detachVpnGatewayResponse = DetachVpnGatewayResponse

instance ToPath DetachVpnGateway where
    toPath = const "/"

instance ToQuery DetachVpnGateway where
    toQuery DetachVpnGateway{..} = mconcat
        [ "dryRun"       =? _dvg1DryRun
        , "VpcId"        =? _dvg1VpcId
        , "VpnGatewayId" =? _dvg1VpnGatewayId
        ]

instance ToHeaders DetachVpnGateway

instance AWSRequest DetachVpnGateway where
    type Sv DetachVpnGateway = EC2
    type Rs DetachVpnGateway = DetachVpnGatewayResponse

    request  = post "DetachVpnGateway"
    response = nullResponse DetachVpnGatewayResponse
