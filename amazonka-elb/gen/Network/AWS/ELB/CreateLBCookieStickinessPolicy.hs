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
-- Module      : Network.AWS.ELB.CreateLBCookieStickinessPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a stickiness policy with sticky session lifetimes controlled by the lifetime of the browser (user-agent) or a specified expiration period. This policy can be associated only with HTTP/HTTPS listeners.
--
--
-- When a load balancer implements this policy, the load balancer uses a special cookie to track the instance for each request. When the load balancer receives a request, it first checks to see if this cookie is present in the request. If so, the load balancer sends the request to the application server specified in the cookie. If not, the load balancer sends the request to a server that is chosen based on the existing load-balancing algorithm.
--
-- A cookie is inserted into the response for binding subsequent requests from the same user to that server. The validity of the cookie is based on the cookie expiration time, which is specified in the policy configuration.
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration Duration-Based Session Stickiness> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.CreateLBCookieStickinessPolicy
    (
    -- * Creating a Request
      createLBCookieStickinessPolicy
    , CreateLBCookieStickinessPolicy
    -- * Request Lenses
    , clbcspCookieExpirationPeriod
    , clbcspLoadBalancerName
    , clbcspPolicyName

    -- * Destructuring the Response
    , createLBCookieStickinessPolicyResponse
    , CreateLBCookieStickinessPolicyResponse
    -- * Response Lenses
    , clbcsprsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateLBCookieStickinessPolicy.
--
--
--
-- /See:/ 'createLBCookieStickinessPolicy' smart constructor.
data CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicy'
  { _clbcspCookieExpirationPeriod :: !(Maybe Integer)
  , _clbcspLoadBalancerName       :: !Text
  , _clbcspPolicyName             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLBCookieStickinessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbcspCookieExpirationPeriod' - The time period, in seconds, after which the cookie should be considered stale. If you do not specify this parameter, the default value is 0, which indicates that the sticky session should last for the duration of the browser session.
--
-- * 'clbcspLoadBalancerName' - The name of the load balancer.
--
-- * 'clbcspPolicyName' - The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
createLBCookieStickinessPolicy
    :: Text -- ^ 'clbcspLoadBalancerName'
    -> Text -- ^ 'clbcspPolicyName'
    -> CreateLBCookieStickinessPolicy
createLBCookieStickinessPolicy pLoadBalancerName_ pPolicyName_ =
  CreateLBCookieStickinessPolicy'
    { _clbcspCookieExpirationPeriod = Nothing
    , _clbcspLoadBalancerName = pLoadBalancerName_
    , _clbcspPolicyName = pPolicyName_
    }


-- | The time period, in seconds, after which the cookie should be considered stale. If you do not specify this parameter, the default value is 0, which indicates that the sticky session should last for the duration of the browser session.
clbcspCookieExpirationPeriod :: Lens' CreateLBCookieStickinessPolicy (Maybe Integer)
clbcspCookieExpirationPeriod = lens _clbcspCookieExpirationPeriod (\ s a -> s{_clbcspCookieExpirationPeriod = a})

-- | The name of the load balancer.
clbcspLoadBalancerName :: Lens' CreateLBCookieStickinessPolicy Text
clbcspLoadBalancerName = lens _clbcspLoadBalancerName (\ s a -> s{_clbcspLoadBalancerName = a})

-- | The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
clbcspPolicyName :: Lens' CreateLBCookieStickinessPolicy Text
clbcspPolicyName = lens _clbcspPolicyName (\ s a -> s{_clbcspPolicyName = a})

instance AWSRequest CreateLBCookieStickinessPolicy
         where
        type Rs CreateLBCookieStickinessPolicy =
             CreateLBCookieStickinessPolicyResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "CreateLBCookieStickinessPolicyResult"
              (\ s h x ->
                 CreateLBCookieStickinessPolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateLBCookieStickinessPolicy
         where

instance NFData CreateLBCookieStickinessPolicy where

instance ToHeaders CreateLBCookieStickinessPolicy
         where
        toHeaders = const mempty

instance ToPath CreateLBCookieStickinessPolicy where
        toPath = const "/"

instance ToQuery CreateLBCookieStickinessPolicy where
        toQuery CreateLBCookieStickinessPolicy'{..}
          = mconcat
              ["Action" =:
                 ("CreateLBCookieStickinessPolicy" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "CookieExpirationPeriod" =:
                 _clbcspCookieExpirationPeriod,
               "LoadBalancerName" =: _clbcspLoadBalancerName,
               "PolicyName" =: _clbcspPolicyName]

-- | Contains the output for CreateLBCookieStickinessPolicy.
--
--
--
-- /See:/ 'createLBCookieStickinessPolicyResponse' smart constructor.
newtype CreateLBCookieStickinessPolicyResponse = CreateLBCookieStickinessPolicyResponse'
  { _clbcsprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLBCookieStickinessPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbcsprsResponseStatus' - -- | The response status code.
createLBCookieStickinessPolicyResponse
    :: Int -- ^ 'clbcsprsResponseStatus'
    -> CreateLBCookieStickinessPolicyResponse
createLBCookieStickinessPolicyResponse pResponseStatus_ =
  CreateLBCookieStickinessPolicyResponse'
    {_clbcsprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
clbcsprsResponseStatus :: Lens' CreateLBCookieStickinessPolicyResponse Int
clbcsprsResponseStatus = lens _clbcsprsResponseStatus (\ s a -> s{_clbcsprsResponseStatus = a})

instance NFData
           CreateLBCookieStickinessPolicyResponse
         where
