{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateAppCookieStickinessPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generates a stickiness policy with sticky session lifetimes that follow
-- that of an application-generated cookie. This policy can be associated
-- only with HTTP\/HTTPS listeners.
--
-- This policy is similar to the policy created by
-- CreateLBCookieStickinessPolicy, except that the lifetime of the special
-- Elastic Load Balancing cookie, @AWSELB@, follows the lifetime of the
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
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateAppCookieStickinessPolicy.html>
module Network.AWS.ELB.CreateAppCookieStickinessPolicy
    (
    -- * Request
      CreateAppCookieStickinessPolicy
    -- ** Request constructor
    , createAppCookieStickinessPolicy
    -- ** Request lenses
    , cacspLoadBalancerName
    , cacspPolicyName
    , cacspCookieName

    -- * Response
    , CreateAppCookieStickinessPolicyResponse
    -- ** Response constructor
    , createAppCookieStickinessPolicyResponse
    -- ** Response lenses
    , cacsprsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAppCookieStickinessPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cacspLoadBalancerName'
--
-- * 'cacspPolicyName'
--
-- * 'cacspCookieName'
data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy'
    { _cacspLoadBalancerName :: !Text
    , _cacspPolicyName       :: !Text
    , _cacspCookieName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAppCookieStickinessPolicy' smart constructor.
createAppCookieStickinessPolicy :: Text -> Text -> Text -> CreateAppCookieStickinessPolicy
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
        type Sv CreateAppCookieStickinessPolicy = ELB
        type Rs CreateAppCookieStickinessPolicy =
             CreateAppCookieStickinessPolicyResponse
        request = post "CreateAppCookieStickinessPolicy"
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cacsprsStatus'
newtype CreateAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse'
    { _cacsprsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAppCookieStickinessPolicyResponse' smart constructor.
createAppCookieStickinessPolicyResponse :: Int -> CreateAppCookieStickinessPolicyResponse
createAppCookieStickinessPolicyResponse pStatus_ =
    CreateAppCookieStickinessPolicyResponse'
    { _cacsprsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
cacsprsStatus :: Lens' CreateAppCookieStickinessPolicyResponse Int
cacsprsStatus = lens _cacsprsStatus (\ s a -> s{_cacsprsStatus = a});
