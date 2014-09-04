{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.CreateAppCookieStickinessPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates a stickiness policy with sticky session lifetimes that follow
-- that of an application-generated cookie. This policy can be associated only
-- with HTTP/HTTPS listeners. This policy is similar to the policy created by
-- CreateLBCookieStickinessPolicy, except that the lifetime of the special
-- Elastic Load Balancing cookie follows the lifetime of the
-- application-generated cookie specified in the policy configuration. The
-- load balancer only inserts a new stickiness cookie when the application
-- response includes a new application cookie. If the application cookie is
-- explicitly removed or expires, the session stops being sticky until a new
-- application cookie is issued. An application client must receive and send
-- two cookies: the application-generated cookie and the special Elastic Load
-- Balancing cookie named AWSELB. This is the default behavior for many common
-- web browsers. For more information, see Enabling Application-Controlled
-- Session Stickiness in the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?CookieName=MyAppCookie
-- &LoadBalancerName=MyLoadBalancer &PolicyName=MyAppStickyPolicy
-- &Version=2012-06-01 &Action=CreateAppCookieStickinessPolicy &AUTHPARAMS
-- 99a693e9-12b8-11e3-9ad6-bf3e4EXAMPLE.
module Network.AWS.ELB.V2012_06_01.CreateAppCookieStickinessPolicy
    (
    -- * Request
      CreateAppCookieStickinessPolicy
    -- ** Request constructor
    , mkCreateAppCookieStickinessPolicyInput
    -- ** Request lenses
    , cacspiLoadBalancerName
    , cacspiPolicyName
    , cacspiCookieName

    -- * Response
    , CreateAppCookieStickinessPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateAppCookieStickinessPolicy' request.
mkCreateAppCookieStickinessPolicyInput :: Text -- ^ 'cacspiLoadBalancerName'
                                       -> Text -- ^ 'cacspiPolicyName'
                                       -> Text -- ^ 'cacspiCookieName'
                                       -> CreateAppCookieStickinessPolicy
mkCreateAppCookieStickinessPolicyInput p1 p2 p3 = CreateAppCookieStickinessPolicy
    { _cacspiLoadBalancerName = p1
    , _cacspiPolicyName = p2
    , _cacspiCookieName = p3
    }
{-# INLINE mkCreateAppCookieStickinessPolicyInput #-}

data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy
    { _cacspiLoadBalancerName :: Text
      -- ^ The name of the load balancer.
    , _cacspiPolicyName :: Text
      -- ^ The name of the policy being created. The name must be unique
      -- within the set of policies for this load balancer.
    , _cacspiCookieName :: Text
      -- ^ Name of the application cookie used for stickiness.
    } deriving (Show, Generic)

-- | The name of the load balancer.
cacspiLoadBalancerName :: Lens' CreateAppCookieStickinessPolicy (Text)
cacspiLoadBalancerName = lens _cacspiLoadBalancerName (\s a -> s { _cacspiLoadBalancerName = a })
{-# INLINE cacspiLoadBalancerName #-}

-- | The name of the policy being created. The name must be unique within the
-- set of policies for this load balancer.
cacspiPolicyName :: Lens' CreateAppCookieStickinessPolicy (Text)
cacspiPolicyName = lens _cacspiPolicyName (\s a -> s { _cacspiPolicyName = a })
{-# INLINE cacspiPolicyName #-}

-- | Name of the application cookie used for stickiness.
cacspiCookieName :: Lens' CreateAppCookieStickinessPolicy (Text)
cacspiCookieName = lens _cacspiCookieName (\s a -> s { _cacspiCookieName = a })
{-# INLINE cacspiCookieName #-}

instance ToQuery CreateAppCookieStickinessPolicy where
    toQuery = genericQuery def

    deriving (Eq, Show, Generic)

instance AWSRequest CreateAppCookieStickinessPolicy where
    type Sv CreateAppCookieStickinessPolicy = ELB
    type Rs CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicyResponse

    request = post "CreateAppCookieStickinessPolicy"
    response _ = nullaryResponse CreateAppCookieStickinessPolicyResponse
