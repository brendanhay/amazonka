{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.ELB.V2012_06_01.ModifyLoadBalancerAttributes
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
-- modify the load balancer attribute ConnectionSettings, by specifying an
-- idle connection timeout value for your load balancer. For more information,
-- see the following: Cross-Zone Load Balancing Connection Draining Access
-- Logs. Connection Settings. Enable Cross-Zone Load Balancing
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
module Network.AWS.ELB.V2012_06_01.ModifyLoadBalancerAttributes where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes
    { _mlbaiLoadBalancerName :: Text
      -- ^ The name of the load balancer.
    , _mlbaiLoadBalancerAttributes :: LoadBalancerAttributes
      -- ^ Attributes of the load balancer.
    } deriving (Generic)

instance ToQuery ModifyLoadBalancerAttributes where
    toQuery = genericToQuery def

instance AWSRequest ModifyLoadBalancerAttributes where
    type Sv ModifyLoadBalancerAttributes = ELB
    type Rs ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributesResponse

    request = post "ModifyLoadBalancerAttributes"
    response _ = xmlResponse

data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse
    { _mlbaoLoadBalancerName :: Maybe Text
      -- ^ The name of the load balancer.
    , _mlbaoLoadBalancerAttributes :: Maybe LoadBalancerAttributes
      -- ^ The LoadBalancerAttributes data type.
    } deriving (Generic)

instance FromXML ModifyLoadBalancerAttributesResponse where
    fromXMLOptions = xmlOptions
