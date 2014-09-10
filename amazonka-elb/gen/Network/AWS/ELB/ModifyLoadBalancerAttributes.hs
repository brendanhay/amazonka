{-# LANGUAGE DeriveGeneric               #-}
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
-- Idle Connection Timeout Enable Cross-Zone Load Balancing
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &LoadBalancerAttributes.CrossZoneLoadBalancing.Enabled=true
-- &Version=2012-06-01 &Action=ModifyLoadBalancerAttributes &AUTHPARAMS
-- my-test-loadbalancer true 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Enable
-- Access Log
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &LoadBalancerAttributes.AccessLog.Enabled=true
-- &LoadBalancerAttributes.AccessLog.S3BucketName=my-loadbalancer-logs
-- &LoadBalancerAttributes.AccessLog.S3BucketPrefix=my-bucket-prefix/prod
-- &LoadBalancerAttributes.AccessLog.EmitInterval=60 &Version=2012-06-01
-- &Action=ModifyLoadBalancerAttributes &AUTHPARAMS my-test-loadbalancer true
-- my-loadbalancer-logs my-bucket-prefix/prod 60
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Enable Connection Draining
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &LoadBalancerAttributes.ConnectionDraining.Enabled=true
-- &LoadBalancerAttributes.ConnectionDraining.Timeout=60 &Version=2012-06-01
-- &Action=ModifyLoadBalancerAttributes &AUTHPARAMS my-test-loadbalancer true
-- 60 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Configure Connection Settings
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &LoadBalancerAttributes.ConnectionSettings.IdleTimeout=30
-- &Version=2012-06-01 &Action=ModifyLoadBalancerAttributes &AUTHPARAMS
-- my-test-loadbalancer 30 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB
    (
    -- * Request
      ModifyLoadBalancerAttributes
    -- ** Request constructor
    , mkModifyLoadBalancerAttributes
    -- ** Request lenses
    , mlbaLoadBalancerName
    , mlbaLoadBalancerAttributes

    -- * Response
    , ModifyLoadBalancerAttributesResponse
    -- ** Response constructor
    , mkModifyLoadBalancerAttributesResponse
    -- ** Response lenses
    , mlbarLoadBalancerName
    , mlbarLoadBalancerAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the ModifyLoadBalancerAttributes action.
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes
    { _mlbaLoadBalancerName :: !Text
    , _mlbaLoadBalancerAttributes :: LoadBalancerAttributes
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyLoadBalancerAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @LoadBalancerAttributes ::@ @LoadBalancerAttributes@
--
mkModifyLoadBalancerAttributes :: Text -- ^ 'mlbaLoadBalancerName'
                               -> LoadBalancerAttributes -- ^ 'mlbaLoadBalancerAttributes'
                               -> ModifyLoadBalancerAttributes
mkModifyLoadBalancerAttributes p1 p2 = ModifyLoadBalancerAttributes
    { _mlbaLoadBalancerName = p1
    , _mlbaLoadBalancerAttributes = p2
    }

-- | The name of the load balancer.
mlbaLoadBalancerName :: Lens' ModifyLoadBalancerAttributes Text
mlbaLoadBalancerName =
    lens _mlbaLoadBalancerName (\s a -> s { _mlbaLoadBalancerName = a })

-- | Attributes of the load balancer.
mlbaLoadBalancerAttributes :: Lens' ModifyLoadBalancerAttributes LoadBalancerAttributes
mlbaLoadBalancerAttributes =
    lens _mlbaLoadBalancerAttributes
         (\s a -> s { _mlbaLoadBalancerAttributes = a })

instance ToQuery ModifyLoadBalancerAttributes where
    toQuery = genericQuery def

-- | The output for the ModifyLoadBalancerAttributes action.
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse
    { _mlbarLoadBalancerName :: !(Maybe Text)
    , _mlbarLoadBalancerAttributes :: Maybe LoadBalancerAttributes
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyLoadBalancerAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Maybe Text@
--
-- * @LoadBalancerAttributes ::@ @Maybe LoadBalancerAttributes@
--
mkModifyLoadBalancerAttributesResponse :: ModifyLoadBalancerAttributesResponse
mkModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse
    { _mlbarLoadBalancerName = Nothing
    , _mlbarLoadBalancerAttributes = Nothing
    }

-- | The name of the load balancer.
mlbarLoadBalancerName :: Lens' ModifyLoadBalancerAttributesResponse (Maybe Text)
mlbarLoadBalancerName =
    lens _mlbarLoadBalancerName (\s a -> s { _mlbarLoadBalancerName = a })

-- | The LoadBalancerAttributes data type.
mlbarLoadBalancerAttributes :: Lens' ModifyLoadBalancerAttributesResponse (Maybe LoadBalancerAttributes)
mlbarLoadBalancerAttributes =
    lens _mlbarLoadBalancerAttributes
         (\s a -> s { _mlbarLoadBalancerAttributes = a })

instance FromXML ModifyLoadBalancerAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyLoadBalancerAttributes where
    type Sv ModifyLoadBalancerAttributes = ELB
    type Rs ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributesResponse

    request = post "ModifyLoadBalancerAttributes"
    response _ = xmlResponse
