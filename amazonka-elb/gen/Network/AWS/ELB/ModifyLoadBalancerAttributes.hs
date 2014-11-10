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

-- Module      : Network.AWS.ELB.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the attributes of a specified load balancer. You can modify the
-- load balancer attributes, such as AccessLogs, ConnectionDraining, and
-- CrossZoneLoadBalancing by either enabling or disabling them. Or, you can
-- modify the load balancer attribute ConnectionSettings by specifying an idle
-- connection timeout value for your load balancer. For more information, see
-- the following: Cross-Zone Load Balancing Connection Draining Access Logs
-- Idle Connection Timeout.
module Network.AWS.ELB.ModifyLoadBalancerAttributes
    (
    -- * Request
      ModifyLoadBalancerAttributesInput
    -- ** Request constructor
    , modifyLoadBalancerAttributes
    -- ** Request lenses
    , mlbaiLoadBalancerAttributes
    , mlbaiLoadBalancerName

    -- * Response
    , ModifyLoadBalancerAttributesOutput
    -- ** Response constructor
    , modifyLoadBalancerAttributesResponse
    -- ** Response lenses
    , mlbaoLoadBalancerAttributes
    , mlbaoLoadBalancerName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data ModifyLoadBalancerAttributesInput = ModifyLoadBalancerAttributesInput
    { _mlbaiLoadBalancerAttributes :: LoadBalancerAttributes
    , _mlbaiLoadBalancerName       :: Text
    } deriving (Eq, Show, Generic)

-- | 'ModifyLoadBalancerAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlbaiLoadBalancerAttributes' @::@ 'LoadBalancerAttributes'
--
-- * 'mlbaiLoadBalancerName' @::@ 'Text'
--
modifyLoadBalancerAttributes :: Text -- ^ 'mlbaiLoadBalancerName'
                             -> LoadBalancerAttributes -- ^ 'mlbaiLoadBalancerAttributes'
                             -> ModifyLoadBalancerAttributesInput
modifyLoadBalancerAttributes p1 p2 = ModifyLoadBalancerAttributesInput
    { _mlbaiLoadBalancerName       = p1
    , _mlbaiLoadBalancerAttributes = p2
    }

-- | Attributes of the load balancer.
mlbaiLoadBalancerAttributes :: Lens' ModifyLoadBalancerAttributesInput LoadBalancerAttributes
mlbaiLoadBalancerAttributes =
    lens _mlbaiLoadBalancerAttributes
        (\s a -> s { _mlbaiLoadBalancerAttributes = a })

-- | The name of the load balancer.
mlbaiLoadBalancerName :: Lens' ModifyLoadBalancerAttributesInput Text
mlbaiLoadBalancerName =
    lens _mlbaiLoadBalancerName (\s a -> s { _mlbaiLoadBalancerName = a })

instance ToPath ModifyLoadBalancerAttributesInput where
    toPath = const "/"

instance ToQuery ModifyLoadBalancerAttributesInput

data ModifyLoadBalancerAttributesOutput = ModifyLoadBalancerAttributesOutput
    { _mlbaoLoadBalancerAttributes :: Maybe LoadBalancerAttributes
    , _mlbaoLoadBalancerName       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ModifyLoadBalancerAttributesOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlbaoLoadBalancerAttributes' @::@ 'Maybe' 'LoadBalancerAttributes'
--
-- * 'mlbaoLoadBalancerName' @::@ 'Maybe' 'Text'
--
modifyLoadBalancerAttributesResponse :: ModifyLoadBalancerAttributesOutput
modifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesOutput
    { _mlbaoLoadBalancerName       = Nothing
    , _mlbaoLoadBalancerAttributes = Nothing
    }

mlbaoLoadBalancerAttributes :: Lens' ModifyLoadBalancerAttributesOutput (Maybe LoadBalancerAttributes)
mlbaoLoadBalancerAttributes =
    lens _mlbaoLoadBalancerAttributes
        (\s a -> s { _mlbaoLoadBalancerAttributes = a })

-- | The name of the load balancer.
mlbaoLoadBalancerName :: Lens' ModifyLoadBalancerAttributesOutput (Maybe Text)
mlbaoLoadBalancerName =
    lens _mlbaoLoadBalancerName (\s a -> s { _mlbaoLoadBalancerName = a })

instance AWSRequest ModifyLoadBalancerAttributesInput where
    type Sv ModifyLoadBalancerAttributesInput = ELB
    type Rs ModifyLoadBalancerAttributesInput = ModifyLoadBalancerAttributesOutput

    request  = post "ModifyLoadBalancerAttributes"
    response = xmlResponse $ \h x -> ModifyLoadBalancerAttributesOutput
        <$> x %| "LoadBalancerAttributes"
        <*> x %| "LoadBalancerName"
