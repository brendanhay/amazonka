{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <ModifyLoadBalancerAttributes.html>
module Network.AWS.ELB.ModifyLoadBalancerAttributes
    (
    -- * Request
      ModifyLoadBalancerAttributes
    -- ** Request constructor
    , modifyLoadBalancerAttributes
    -- ** Request lenses
    , mlbaLoadBalancerAttributes
    , mlbaLoadBalancerName

    -- * Response
    , ModifyLoadBalancerAttributesResponse
    -- ** Response constructor
    , modifyLoadBalancerAttributesResponse
    -- ** Response lenses
    , mlbarLoadBalancerAttributes
    , mlbarLoadBalancerName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes
    { _mlbaLoadBalancerAttributes :: LoadBalancerAttributes
    , _mlbaLoadBalancerName       :: Text
    } deriving (Eq, Show, Generic)

-- | 'ModifyLoadBalancerAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlbaLoadBalancerAttributes' @::@ 'LoadBalancerAttributes'
--
-- * 'mlbaLoadBalancerName' @::@ 'Text'
--
modifyLoadBalancerAttributes :: Text -- ^ 'mlbaLoadBalancerName'
                             -> LoadBalancerAttributes -- ^ 'mlbaLoadBalancerAttributes'
                             -> ModifyLoadBalancerAttributes
modifyLoadBalancerAttributes p1 p2 = ModifyLoadBalancerAttributes
    { _mlbaLoadBalancerName       = p1
    , _mlbaLoadBalancerAttributes = p2
    }

-- | Attributes of the load balancer.
mlbaLoadBalancerAttributes :: Lens' ModifyLoadBalancerAttributes LoadBalancerAttributes
mlbaLoadBalancerAttributes =
    lens _mlbaLoadBalancerAttributes
        (\s a -> s { _mlbaLoadBalancerAttributes = a })

-- | The name of the load balancer.
mlbaLoadBalancerName :: Lens' ModifyLoadBalancerAttributes Text
mlbaLoadBalancerName =
    lens _mlbaLoadBalancerName (\s a -> s { _mlbaLoadBalancerName = a })

data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse
    { _mlbarLoadBalancerAttributes :: Maybe LoadBalancerAttributes
    , _mlbarLoadBalancerName       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ModifyLoadBalancerAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlbarLoadBalancerAttributes' @::@ 'Maybe' 'LoadBalancerAttributes'
--
-- * 'mlbarLoadBalancerName' @::@ 'Maybe' 'Text'
--
modifyLoadBalancerAttributesResponse :: ModifyLoadBalancerAttributesResponse
modifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse
    { _mlbarLoadBalancerName       = Nothing
    , _mlbarLoadBalancerAttributes = Nothing
    }

mlbarLoadBalancerAttributes :: Lens' ModifyLoadBalancerAttributesResponse (Maybe LoadBalancerAttributes)
mlbarLoadBalancerAttributes =
    lens _mlbarLoadBalancerAttributes
        (\s a -> s { _mlbarLoadBalancerAttributes = a })

-- | The name of the load balancer.
mlbarLoadBalancerName :: Lens' ModifyLoadBalancerAttributesResponse (Maybe Text)
mlbarLoadBalancerName =
    lens _mlbarLoadBalancerName (\s a -> s { _mlbarLoadBalancerName = a })

instance AWSRequest ModifyLoadBalancerAttributes where
    type Sv ModifyLoadBalancerAttributes = ELB
    type Rs ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributesResponse

    request  = post "ModifyLoadBalancerAttributes"
    response = xmlResponse

instance FromXML ModifyLoadBalancerAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyLoadBalancerAttributesResponse"

instance ToPath ModifyLoadBalancerAttributes where
    toPath = const "/"

instance ToHeaders ModifyLoadBalancerAttributes

instance ToQuery ModifyLoadBalancerAttributes
