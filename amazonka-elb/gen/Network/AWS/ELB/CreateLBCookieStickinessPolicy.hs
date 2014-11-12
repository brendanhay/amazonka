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

-- Module      : Network.AWS.ELB.CreateLBCookieStickinessPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates a stickiness policy with sticky session lifetimes controlled by
-- the lifetime of the browser (user-agent) or a specified expiration period.
-- This policy can be associated only with HTTP/HTTPS listeners. When a load
-- balancer implements this policy, the load balancer uses a special cookie to
-- track the backend server instance for each request. When the load balancer
-- receives a request, it first checks to see if this cookie is present in the
-- request. If so, the load balancer sends the request to the application
-- server specified in the cookie. If not, the load balancer sends the request
-- to a server that is chosen based on the existing load balancing algorithm.
-- A cookie is inserted into the response for binding subsequent requests from
-- the same user to that server. The validity of the cookie is based on the
-- cookie expiration time, which is specified in the policy configuration. For
-- more information, see Enabling Duration-Based Session Stickiness in the
-- Elastic Load Balancing Developer Guide.
module Network.AWS.ELB.CreateLBCookieStickinessPolicy
    (
    -- * Request
      CreateLBCookieStickinessPolicyInput
    -- ** Request constructor
    , createLBCookieStickinessPolicy
    -- ** Request lenses
    , clbcspiCookieExpirationPeriod
    , clbcspiLoadBalancerName
    , clbcspiPolicyName

    -- * Response
    , CreateLBCookieStickinessPolicyResponse
    -- ** Response constructor
    , createLBCookieStickinessPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data CreateLBCookieStickinessPolicyInput = CreateLBCookieStickinessPolicyInput
    { _clbcspiCookieExpirationPeriod :: Maybe Integer
    , _clbcspiLoadBalancerName       :: Text
    , _clbcspiPolicyName             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateLBCookieStickinessPolicyInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbcspiCookieExpirationPeriod' @::@ 'Maybe' 'Integer'
--
-- * 'clbcspiLoadBalancerName' @::@ 'Text'
--
-- * 'clbcspiPolicyName' @::@ 'Text'
--
createLBCookieStickinessPolicy :: Text -- ^ 'clbcspiLoadBalancerName'
                               -> Text -- ^ 'clbcspiPolicyName'
                               -> CreateLBCookieStickinessPolicyInput
createLBCookieStickinessPolicy p1 p2 = CreateLBCookieStickinessPolicyInput
    { _clbcspiLoadBalancerName       = p1
    , _clbcspiPolicyName             = p2
    , _clbcspiCookieExpirationPeriod = Nothing
    }

-- | The time period in seconds after which the cookie should be considered
-- stale. Not specifying this parameter indicates that the sticky session
-- will last for the duration of the browser session.
clbcspiCookieExpirationPeriod :: Lens' CreateLBCookieStickinessPolicyInput (Maybe Integer)
clbcspiCookieExpirationPeriod =
    lens _clbcspiCookieExpirationPeriod
        (\s a -> s { _clbcspiCookieExpirationPeriod = a })

-- | The name associated with the load balancer.
clbcspiLoadBalancerName :: Lens' CreateLBCookieStickinessPolicyInput Text
clbcspiLoadBalancerName =
    lens _clbcspiLoadBalancerName (\s a -> s { _clbcspiLoadBalancerName = a })

-- | The name of the policy being created. The name must be unique within the
-- set of policies for this load balancer.
clbcspiPolicyName :: Lens' CreateLBCookieStickinessPolicyInput Text
clbcspiPolicyName =
    lens _clbcspiPolicyName (\s a -> s { _clbcspiPolicyName = a })

instance ToQuery CreateLBCookieStickinessPolicyInput

instance ToPath CreateLBCookieStickinessPolicyInput where
    toPath = const "/"

data CreateLBCookieStickinessPolicyResponse = CreateLBCookieStickinessPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateLBCookieStickinessPolicyResponse' constructor.
createLBCookieStickinessPolicyResponse :: CreateLBCookieStickinessPolicyResponse
createLBCookieStickinessPolicyResponse = CreateLBCookieStickinessPolicyResponse

instance FromXML CreateLBCookieStickinessPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateLBCookieStickinessPolicyResponse"

instance AWSRequest CreateLBCookieStickinessPolicyInput where
    type Sv CreateLBCookieStickinessPolicyInput = ELB
    type Rs CreateLBCookieStickinessPolicyInput = CreateLBCookieStickinessPolicyResponse

    request  = post "CreateLBCookieStickinessPolicy"
    response = nullaryResponse CreateLBCookieStickinessPolicyResponse
