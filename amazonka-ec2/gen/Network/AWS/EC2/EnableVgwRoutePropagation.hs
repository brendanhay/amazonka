{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.EnableVgwRoutePropagation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables a virtual private gateway (VGW) to propagate routes to the
-- specified route table of a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVgwRoutePropagation.html>
module Network.AWS.EC2.EnableVgwRoutePropagation
    (
    -- * Request
      EnableVgwRoutePropagation
    -- ** Request constructor
    , enableVgwRoutePropagation
    -- ** Request lenses
    , evrpGatewayId
    , evrpRouteTableId

    -- * Response
    , EnableVgwRoutePropagationResponse
    -- ** Response constructor
    , enableVgwRoutePropagationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data EnableVgwRoutePropagation = EnableVgwRoutePropagation
    { _evrpGatewayId    :: Text
    , _evrpRouteTableId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnableVgwRoutePropagation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evrpGatewayId' @::@ 'Text'
--
-- * 'evrpRouteTableId' @::@ 'Text'
--
enableVgwRoutePropagation :: Text -- ^ 'evrpRouteTableId'
                          -> Text -- ^ 'evrpGatewayId'
                          -> EnableVgwRoutePropagation
enableVgwRoutePropagation p1 p2 = EnableVgwRoutePropagation
    { _evrpRouteTableId = p1
    , _evrpGatewayId    = p2
    }

-- | The ID of the virtual private gateway.
evrpGatewayId :: Lens' EnableVgwRoutePropagation Text
evrpGatewayId = lens _evrpGatewayId (\s a -> s { _evrpGatewayId = a })

-- | The ID of the route table.
evrpRouteTableId :: Lens' EnableVgwRoutePropagation Text
evrpRouteTableId = lens _evrpRouteTableId (\s a -> s { _evrpRouteTableId = a })

data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableVgwRoutePropagationResponse' constructor.
enableVgwRoutePropagationResponse :: EnableVgwRoutePropagationResponse
enableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse

instance AWSRequest EnableVgwRoutePropagation where
    type Sv EnableVgwRoutePropagation = EC2
    type Rs EnableVgwRoutePropagation = EnableVgwRoutePropagationResponse

    request  = post "EnableVgwRoutePropagation"
    response = nullResponse EnableVgwRoutePropagationResponse

instance ToPath EnableVgwRoutePropagation where
    toPath = const "/"

instance ToHeaders EnableVgwRoutePropagation

instance ToQuery EnableVgwRoutePropagation
