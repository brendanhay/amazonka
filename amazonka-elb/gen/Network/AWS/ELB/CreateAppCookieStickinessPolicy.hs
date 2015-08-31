{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateAppCookieStickinessPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a stickiness policy with sticky session lifetimes that follow
-- that of an application-generated cookie. This policy can be associated
-- only with HTTP\/HTTPS listeners.
--
-- This policy is similar to the policy created by
-- CreateLBCookieStickinessPolicy, except that the lifetime of the special
-- Elastic Load Balancing cookie, 'AWSELB', follows the lifetime of the
-- application-generated cookie specified in the policy configuration. The
-- load balancer only inserts a new stickiness cookie when the application
-- response includes a new application cookie.
--
-- If the application cookie is explicitly removed or expires, the session
-- stops being sticky until a new application cookie is issued.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-sticky-sessions.html#enable-sticky-sessions-application Application-Controlled Session Stickiness>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateAppCookieStickinessPolicy.html AWS API Reference> for CreateAppCookieStickinessPolicy.
module Network.AWS.ELB.CreateAppCookieStickinessPolicy
    (
    -- * Creating a Request
      createAppCookieStickinessPolicy
    , CreateAppCookieStickinessPolicy
    -- * Request Lenses
    , cacspLoadBalancerName
    , cacspPolicyName
    , cacspCookieName

    -- * Destructuring the Response
    , createAppCookieStickinessPolicyResponse
    , CreateAppCookieStickinessPolicyResponse
    -- * Response Lenses
    , cacsprsResponseStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAppCookieStickinessPolicy' smart constructor.
data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy'
    { _cacspLoadBalancerName :: !Text
    , _cacspPolicyName       :: !Text
    , _cacspCookieName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAppCookieStickinessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacspLoadBalancerName'
--
-- * 'cacspPolicyName'
--
-- * 'cacspCookieName'
createAppCookieStickinessPolicy
    :: Text -- ^ 'cacspLoadBalancerName'
    -> Text -- ^ 'cacspPolicyName'
    -> Text -- ^ 'cacspCookieName'
    -> CreateAppCookieStickinessPolicy
createAppCookieStickinessPolicy pLoadBalancerName_ pPolicyName_ pCookieName_ =
    CreateAppCookieStickinessPolicy'
    { _cacspLoadBalancerName = pLoadBalancerName_
    , _cacspPolicyName = pPolicyName_
    , _cacspCookieName = pCookieName_
    }

-- | The name of the load balancer.
cacspLoadBalancerName :: Lens' CreateAppCookieStickinessPolicy Text
cacspLoadBalancerName = lens _cacspLoadBalancerName (\ s a -> s{_cacspLoadBalancerName = a});

-- | The name of the policy being created. This name must be unique within
-- the set of policies for this load balancer.
cacspPolicyName :: Lens' CreateAppCookieStickinessPolicy Text
cacspPolicyName = lens _cacspPolicyName (\ s a -> s{_cacspPolicyName = a});

-- | The name of the application cookie used for stickiness.
cacspCookieName :: Lens' CreateAppCookieStickinessPolicy Text
cacspCookieName = lens _cacspCookieName (\ s a -> s{_cacspCookieName = a});

instance AWSRequest CreateAppCookieStickinessPolicy
         where
        type Rs CreateAppCookieStickinessPolicy =
             CreateAppCookieStickinessPolicyResponse
        request = postQuery eLB
        response
          = receiveXMLWrapper
              "CreateAppCookieStickinessPolicyResult"
              (\ s h x ->
                 CreateAppCookieStickinessPolicyResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders CreateAppCookieStickinessPolicy
         where
        toHeaders = const mempty

instance ToPath CreateAppCookieStickinessPolicy where
        toPath = const "/"

instance ToQuery CreateAppCookieStickinessPolicy
         where
        toQuery CreateAppCookieStickinessPolicy'{..}
          = mconcat
              ["Action" =:
                 ("CreateAppCookieStickinessPolicy" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _cacspLoadBalancerName,
               "PolicyName" =: _cacspPolicyName,
               "CookieName" =: _cacspCookieName]

-- | /See:/ 'createAppCookieStickinessPolicyResponse' smart constructor.
newtype CreateAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse'
    { _cacsprsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAppCookieStickinessPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacsprsResponseStatus'
createAppCookieStickinessPolicyResponse
    :: Int -- ^ 'cacsprsResponseStatus'
    -> CreateAppCookieStickinessPolicyResponse
createAppCookieStickinessPolicyResponse pResponseStatus_ =
    CreateAppCookieStickinessPolicyResponse'
    { _cacsprsResponseStatus = pResponseStatus_
    }

-- | The response status code.
cacsprsResponseStatus :: Lens' CreateAppCookieStickinessPolicyResponse Int
cacsprsResponseStatus = lens _cacsprsResponseStatus (\ s a -> s{_cacsprsResponseStatus = a});
