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

-- Module      : Network.AWS.ELB.CreateLBCookieStickinessPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Generates a stickiness policy with sticky session lifetimes controlled by
-- the lifetime of the browser (user-agent) or a specified expiration period.
-- This policy can be associated only with HTTP/HTTPS listeners.
--
-- When a load balancer implements this policy, the load balancer uses a
-- special cookie to track the backend server instance for each request. When
-- the load balancer receives a request, it first checks to see if this cookie
-- is present in the request. If so, the load balancer sends the request to the
-- application server specified in the cookie. If not, the load balancer sends
-- the request to a server that is chosen based on the existing load balancing
-- algorithm.
--
-- A cookie is inserted into the response for binding subsequent requests from
-- the same user to that server. The validity of the cookie is based on the
-- cookie expiration time, which is specified in the policy configuration.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/US_StickySessions.html#US_EnableStickySessionsLBCookies Enabling Duration-Based Session Stickiness> in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLBCookieStickinessPolicy.html>
module Network.AWS.ELB.CreateLBCookieStickinessPolicy
    (
    -- * Request
      CreateLBCookieStickinessPolicy
    -- ** Request constructor
    , createLBCookieStickinessPolicy
    -- ** Request lenses
    , clbcspCookieExpirationPeriod
    , clbcspLoadBalancerName
    , clbcspPolicyName

    -- * Response
    , CreateLBCookieStickinessPolicyResponse
    -- ** Response constructor
    , createLBCookieStickinessPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicy
    { _clbcspCookieExpirationPeriod :: Maybe Integer
    , _clbcspLoadBalancerName       :: Text
    , _clbcspPolicyName             :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateLBCookieStickinessPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbcspCookieExpirationPeriod' @::@ 'Maybe' 'Integer'
--
-- * 'clbcspLoadBalancerName' @::@ 'Text'
--
-- * 'clbcspPolicyName' @::@ 'Text'
--
createLBCookieStickinessPolicy :: Text -- ^ 'clbcspLoadBalancerName'
                               -> Text -- ^ 'clbcspPolicyName'
                               -> CreateLBCookieStickinessPolicy
createLBCookieStickinessPolicy p1 p2 = CreateLBCookieStickinessPolicy
    { _clbcspLoadBalancerName       = p1
    , _clbcspPolicyName             = p2
    , _clbcspCookieExpirationPeriod = Nothing
    }

-- | The time period in seconds after which the cookie should be considered
-- stale. Not specifying this parameter indicates that the sticky session will
-- last for the duration of the browser session.
clbcspCookieExpirationPeriod :: Lens' CreateLBCookieStickinessPolicy (Maybe Integer)
clbcspCookieExpirationPeriod =
    lens _clbcspCookieExpirationPeriod
        (\s a -> s { _clbcspCookieExpirationPeriod = a })

-- | The name associated with the load balancer.
clbcspLoadBalancerName :: Lens' CreateLBCookieStickinessPolicy Text
clbcspLoadBalancerName =
    lens _clbcspLoadBalancerName (\s a -> s { _clbcspLoadBalancerName = a })

-- | The name of the policy being created. The name must be unique within the set
-- of policies for this load balancer.
clbcspPolicyName :: Lens' CreateLBCookieStickinessPolicy Text
clbcspPolicyName = lens _clbcspPolicyName (\s a -> s { _clbcspPolicyName = a })

data CreateLBCookieStickinessPolicyResponse = CreateLBCookieStickinessPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'CreateLBCookieStickinessPolicyResponse' constructor.
createLBCookieStickinessPolicyResponse :: CreateLBCookieStickinessPolicyResponse
createLBCookieStickinessPolicyResponse = CreateLBCookieStickinessPolicyResponse

instance ToPath CreateLBCookieStickinessPolicy where
    toPath = const "/"

instance ToQuery CreateLBCookieStickinessPolicy where
    toQuery CreateLBCookieStickinessPolicy{..} = mconcat
        [ "CookieExpirationPeriod" =? _clbcspCookieExpirationPeriod
        , "LoadBalancerName"       =? _clbcspLoadBalancerName
        , "PolicyName"             =? _clbcspPolicyName
        ]

instance ToHeaders CreateLBCookieStickinessPolicy

instance AWSRequest CreateLBCookieStickinessPolicy where
    type Sv CreateLBCookieStickinessPolicy = ELB
    type Rs CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicyResponse

    request  = post "CreateLBCookieStickinessPolicy"
    response = nullResponse CreateLBCookieStickinessPolicyResponse
