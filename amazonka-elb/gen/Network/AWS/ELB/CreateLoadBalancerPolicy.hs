{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.CreateLoadBalancerPolicy
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

-- | Creates a policy with the specified attributes for the specified load
-- balancer.
--
-- Policies are settings that are saved for your load balancer and that can
-- be applied to the front-end listener or the back-end application server,
-- depending on the policy type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerPolicy.html>
module Network.AWS.ELB.CreateLoadBalancerPolicy
    (
    -- * Request
      CreateLoadBalancerPolicy
    -- ** Request constructor
    , createLoadBalancerPolicy
    -- ** Request lenses
    , clbpPolicyAttributes
    , clbpLoadBalancerName
    , clbpPolicyName
    , clbpPolicyTypeName

    -- * Response
    , CreateLoadBalancerPolicyResponse
    -- ** Response constructor
    , createLoadBalancerPolicyResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ELB.Types

-- | /See:/ 'createLoadBalancerPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbpPolicyAttributes'
--
-- * 'clbpLoadBalancerName'
--
-- * 'clbpPolicyName'
--
-- * 'clbpPolicyTypeName'
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy'{_clbpPolicyAttributes :: Maybe [PolicyAttribute], _clbpLoadBalancerName :: Text, _clbpPolicyName :: Text, _clbpPolicyTypeName :: Text} deriving (Eq, Read, Show)

-- | 'CreateLoadBalancerPolicy' smart constructor.
createLoadBalancerPolicy :: Text -> Text -> Text -> CreateLoadBalancerPolicy
createLoadBalancerPolicy pLoadBalancerName pPolicyName pPolicyTypeName = CreateLoadBalancerPolicy'{_clbpPolicyAttributes = Nothing, _clbpLoadBalancerName = pLoadBalancerName, _clbpPolicyName = pPolicyName, _clbpPolicyTypeName = pPolicyTypeName};

-- | The attributes for the policy.
clbpPolicyAttributes :: Lens' CreateLoadBalancerPolicy [PolicyAttribute]
clbpPolicyAttributes = lens _clbpPolicyAttributes (\ s a -> s{_clbpPolicyAttributes = a}) . _Default;

-- | The name of the load balancer.
clbpLoadBalancerName :: Lens' CreateLoadBalancerPolicy Text
clbpLoadBalancerName = lens _clbpLoadBalancerName (\ s a -> s{_clbpLoadBalancerName = a});

-- | The name of the load balancer policy to be created. This name must be
-- unique within the set of policies for this load balancer.
clbpPolicyName :: Lens' CreateLoadBalancerPolicy Text
clbpPolicyName = lens _clbpPolicyName (\ s a -> s{_clbpPolicyName = a});

-- | The name of the base policy type. To get the list of policy types, use
-- DescribeLoadBalancerPolicyTypes.
clbpPolicyTypeName :: Lens' CreateLoadBalancerPolicy Text
clbpPolicyTypeName = lens _clbpPolicyTypeName (\ s a -> s{_clbpPolicyTypeName = a});

instance AWSRequest CreateLoadBalancerPolicy where
        type Sv CreateLoadBalancerPolicy = ELB
        type Rs CreateLoadBalancerPolicy =
             CreateLoadBalancerPolicyResponse
        request = post
        response
          = receiveNull CreateLoadBalancerPolicyResponse'

instance ToHeaders CreateLoadBalancerPolicy where
        toHeaders = const mempty

instance ToPath CreateLoadBalancerPolicy where
        toPath = const "/"

instance ToQuery CreateLoadBalancerPolicy where
        toQuery CreateLoadBalancerPolicy'{..}
          = mconcat
              ["Action" =:
                 ("CreateLoadBalancerPolicy" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "PolicyAttributes" =:
                 toQuery
                   (toQueryList "member" <$> _clbpPolicyAttributes),
               "LoadBalancerName" =: _clbpLoadBalancerName,
               "PolicyName" =: _clbpPolicyName,
               "PolicyTypeName" =: _clbpPolicyTypeName]

-- | /See:/ 'createLoadBalancerPolicyResponse' smart constructor.
data CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse' deriving (Eq, Read, Show)

-- | 'CreateLoadBalancerPolicyResponse' smart constructor.
createLoadBalancerPolicyResponse :: CreateLoadBalancerPolicyResponse
createLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse';
