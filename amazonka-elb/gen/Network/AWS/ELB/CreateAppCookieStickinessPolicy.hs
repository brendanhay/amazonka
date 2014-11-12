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

-- Module      : Network.AWS.ELB.CreateAppCookieStickinessPolicy
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
module Network.AWS.ELB.CreateAppCookieStickinessPolicy
    (
    -- * Request
      CreateAppCookieStickinessPolicyInput
    -- ** Request constructor
    , createAppCookieStickinessPolicyInput
    -- ** Request lenses
    , cacspiCookieName
    , cacspiLoadBalancerName
    , cacspiPolicyName

    -- * Response
    , CreateAppCookieStickinessPolicyResponse
    -- ** Response constructor
    , createAppCookieStickinessPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data CreateAppCookieStickinessPolicyInput = CreateAppCookieStickinessPolicyInput
    { _cacspiCookieName       :: Text
    , _cacspiLoadBalancerName :: Text
    , _cacspiPolicyName       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateAppCookieStickinessPolicyInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cacspiCookieName' @::@ 'Text'
--
-- * 'cacspiLoadBalancerName' @::@ 'Text'
--
-- * 'cacspiPolicyName' @::@ 'Text'
--
createAppCookieStickinessPolicyInput :: Text -- ^ 'cacspiLoadBalancerName'
                                     -> Text -- ^ 'cacspiPolicyName'
                                     -> Text -- ^ 'cacspiCookieName'
                                     -> CreateAppCookieStickinessPolicyInput
createAppCookieStickinessPolicyInput p1 p2 p3 = CreateAppCookieStickinessPolicyInput
    { _cacspiLoadBalancerName = p1
    , _cacspiPolicyName       = p2
    , _cacspiCookieName       = p3
    }

-- | Name of the application cookie used for stickiness.
cacspiCookieName :: Lens' CreateAppCookieStickinessPolicyInput Text
cacspiCookieName = lens _cacspiCookieName (\s a -> s { _cacspiCookieName = a })

-- | The name of the load balancer.
cacspiLoadBalancerName :: Lens' CreateAppCookieStickinessPolicyInput Text
cacspiLoadBalancerName =
    lens _cacspiLoadBalancerName (\s a -> s { _cacspiLoadBalancerName = a })

-- | The name of the policy being created. The name must be unique within the
-- set of policies for this load balancer.
cacspiPolicyName :: Lens' CreateAppCookieStickinessPolicyInput Text
cacspiPolicyName = lens _cacspiPolicyName (\s a -> s { _cacspiPolicyName = a })

instance ToQuery CreateAppCookieStickinessPolicyInput

instance ToPath CreateAppCookieStickinessPolicyInput where
    toPath = const "/"

data CreateAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateAppCookieStickinessPolicyResponse' constructor.
createAppCookieStickinessPolicyResponse :: CreateAppCookieStickinessPolicyResponse
createAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse

instance FromXML CreateAppCookieStickinessPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateAppCookieStickinessPolicyResponse"

instance AWSRequest CreateAppCookieStickinessPolicyInput where
    type Sv CreateAppCookieStickinessPolicyInput = ELB
    type Rs CreateAppCookieStickinessPolicyInput = CreateAppCookieStickinessPolicyResponse

    request  = post "CreateAppCookieStickinessPolicy"
    response = nullaryResponse CreateAppCookieStickinessPolicyResponse
