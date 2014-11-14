{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.EC2.DisableVgwRoutePropagation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables a virtual private gateway (VGW) from propagating routes to a
-- specified route table of a VPC.
module Network.AWS.EC2.DisableVgwRoutePropagation
    (
    -- * Request
      DisableVgwRoutePropagation
    -- ** Request constructor
    , disableVgwRoutePropagation
    -- ** Request lenses
    , dvrpGatewayId
    , dvrpRouteTableId

    -- * Response
    , DisableVgwRoutePropagationResponse
    -- ** Response constructor
    , disableVgwRoutePropagationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DisableVgwRoutePropagation = DisableVgwRoutePropagation
    { _dvrpGatewayId    :: Text
    , _dvrpRouteTableId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DisableVgwRoutePropagation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrpGatewayId' @::@ 'Text'
--
-- * 'dvrpRouteTableId' @::@ 'Text'
--
disableVgwRoutePropagation :: Text -- ^ 'dvrpRouteTableId'
                           -> Text -- ^ 'dvrpGatewayId'
                           -> DisableVgwRoutePropagation
disableVgwRoutePropagation p1 p2 = DisableVgwRoutePropagation
    { _dvrpRouteTableId = p1
    , _dvrpGatewayId    = p2
    }

-- | The ID of the virtual private gateway.
dvrpGatewayId :: Lens' DisableVgwRoutePropagation Text
dvrpGatewayId = lens _dvrpGatewayId (\s a -> s { _dvrpGatewayId = a })

-- | The ID of the route table.
dvrpRouteTableId :: Lens' DisableVgwRoutePropagation Text
dvrpRouteTableId = lens _dvrpRouteTableId (\s a -> s { _dvrpRouteTableId = a })

instance ToQuery DisableVgwRoutePropagation

instance ToPath DisableVgwRoutePropagation where
    toPath = const "/"

data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisableVgwRoutePropagationResponse' constructor.
disableVgwRoutePropagationResponse :: DisableVgwRoutePropagationResponse
disableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse

instance AWSRequest DisableVgwRoutePropagation where
    type Sv DisableVgwRoutePropagation = EC2
    type Rs DisableVgwRoutePropagation = DisableVgwRoutePropagationResponse

    request  = post "DisableVgwRoutePropagation"
    response = nullaryResponse DisableVgwRoutePropagationResponse
