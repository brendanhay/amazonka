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
-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified policies.
--
-- If you specify a load balancer name, the action returns the descriptions
-- of all policies created for the load balancer. If you specify a policy
-- name associated with your load balancer, the action returns the
-- description of that policy. If you don\'t specify a load balancer name,
-- the action returns descriptions of the specified sample policies, or
-- descriptions of all sample policies. The names of the sample policies
-- have the 'ELBSample-' prefix.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPolicies.html AWS API Reference> for DescribeLoadBalancerPolicies.
module Network.AWS.ELB.DescribeLoadBalancerPolicies
    (
    -- * Creating a Request
      describeLoadBalancerPolicies
    , DescribeLoadBalancerPolicies
    -- * Request Lenses
    , dlbpPolicyNames
    , dlbpLoadBalancerName

    -- * Destructuring the Response
    , describeLoadBalancerPoliciesResponse
    , DescribeLoadBalancerPoliciesResponse
    -- * Response Lenses
    , dlbprsPolicyDescriptions
    , dlbprsResponseStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancerPolicies' smart constructor.
data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies'
    { _dlbpPolicyNames      :: !(Maybe [Text])
    , _dlbpLoadBalancerName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLoadBalancerPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbpPolicyNames'
--
-- * 'dlbpLoadBalancerName'
describeLoadBalancerPolicies
    :: DescribeLoadBalancerPolicies
describeLoadBalancerPolicies =
    DescribeLoadBalancerPolicies'
    { _dlbpPolicyNames = Nothing
    , _dlbpLoadBalancerName = Nothing
    }

-- | The names of the policies.
dlbpPolicyNames :: Lens' DescribeLoadBalancerPolicies [Text]
dlbpPolicyNames = lens _dlbpPolicyNames (\ s a -> s{_dlbpPolicyNames = a}) . _Default . _Coerce;

-- | The name of the load balancer.
dlbpLoadBalancerName :: Lens' DescribeLoadBalancerPolicies (Maybe Text)
dlbpLoadBalancerName = lens _dlbpLoadBalancerName (\ s a -> s{_dlbpLoadBalancerName = a});

instance AWSRequest DescribeLoadBalancerPolicies
         where
        type Rs DescribeLoadBalancerPolicies =
             DescribeLoadBalancerPoliciesResponse
        request = postQuery eLB
        response
          = receiveXMLWrapper
              "DescribeLoadBalancerPoliciesResult"
              (\ s h x ->
                 DescribeLoadBalancerPoliciesResponse' <$>
                   (x .@? "PolicyDescriptions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeLoadBalancerPolicies where
        toHeaders = const mempty

instance ToPath DescribeLoadBalancerPolicies where
        toPath = const "/"

instance ToQuery DescribeLoadBalancerPolicies where
        toQuery DescribeLoadBalancerPolicies'{..}
          = mconcat
              ["Action" =:
                 ("DescribeLoadBalancerPolicies" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "PolicyNames" =:
                 toQuery (toQueryList "member" <$> _dlbpPolicyNames),
               "LoadBalancerName" =: _dlbpLoadBalancerName]

-- | /See:/ 'describeLoadBalancerPoliciesResponse' smart constructor.
data DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse'
    { _dlbprsPolicyDescriptions :: !(Maybe [PolicyDescription])
    , _dlbprsResponseStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLoadBalancerPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbprsPolicyDescriptions'
--
-- * 'dlbprsResponseStatus'
describeLoadBalancerPoliciesResponse
    :: Int -- ^ 'dlbprsResponseStatus'
    -> DescribeLoadBalancerPoliciesResponse
describeLoadBalancerPoliciesResponse pResponseStatus_ =
    DescribeLoadBalancerPoliciesResponse'
    { _dlbprsPolicyDescriptions = Nothing
    , _dlbprsResponseStatus = pResponseStatus_
    }

-- | Information about the policies.
dlbprsPolicyDescriptions :: Lens' DescribeLoadBalancerPoliciesResponse [PolicyDescription]
dlbprsPolicyDescriptions = lens _dlbprsPolicyDescriptions (\ s a -> s{_dlbprsPolicyDescriptions = a}) . _Default . _Coerce;

-- | The response status code.
dlbprsResponseStatus :: Lens' DescribeLoadBalancerPoliciesResponse Int
dlbprsResponseStatus = lens _dlbprsResponseStatus (\ s a -> s{_dlbprsResponseStatus = a});
