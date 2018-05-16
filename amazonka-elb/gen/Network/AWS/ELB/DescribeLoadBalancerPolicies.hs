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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified policies.
--
--
-- If you specify a load balancer name, the action returns the descriptions of all policies created for the load balancer. If you specify a policy name associated with your load balancer, the action returns the description of that policy. If you don't specify a load balancer name, the action returns descriptions of the specified sample policies, or descriptions of all sample policies. The names of the sample policies have the @ELBSample-@ prefix.
--
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

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeLoadBalancerPolicies.
--
--
--
-- /See:/ 'describeLoadBalancerPolicies' smart constructor.
data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies'
  { _dlbpPolicyNames      :: !(Maybe [Text])
  , _dlbpLoadBalancerName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancerPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbpPolicyNames' - The names of the policies.
--
-- * 'dlbpLoadBalancerName' - The name of the load balancer.
describeLoadBalancerPolicies
    :: DescribeLoadBalancerPolicies
describeLoadBalancerPolicies =
  DescribeLoadBalancerPolicies'
    {_dlbpPolicyNames = Nothing, _dlbpLoadBalancerName = Nothing}


-- | The names of the policies.
dlbpPolicyNames :: Lens' DescribeLoadBalancerPolicies [Text]
dlbpPolicyNames = lens _dlbpPolicyNames (\ s a -> s{_dlbpPolicyNames = a}) . _Default . _Coerce

-- | The name of the load balancer.
dlbpLoadBalancerName :: Lens' DescribeLoadBalancerPolicies (Maybe Text)
dlbpLoadBalancerName = lens _dlbpLoadBalancerName (\ s a -> s{_dlbpLoadBalancerName = a})

instance AWSRequest DescribeLoadBalancerPolicies
         where
        type Rs DescribeLoadBalancerPolicies =
             DescribeLoadBalancerPoliciesResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "DescribeLoadBalancerPoliciesResult"
              (\ s h x ->
                 DescribeLoadBalancerPoliciesResponse' <$>
                   (x .@? "PolicyDescriptions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLoadBalancerPolicies where

instance NFData DescribeLoadBalancerPolicies where

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

-- | Contains the output of DescribeLoadBalancerPolicies.
--
--
--
-- /See:/ 'describeLoadBalancerPoliciesResponse' smart constructor.
data DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse'
  { _dlbprsPolicyDescriptions :: !(Maybe [PolicyDescription])
  , _dlbprsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancerPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbprsPolicyDescriptions' - Information about the policies.
--
-- * 'dlbprsResponseStatus' - -- | The response status code.
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
dlbprsPolicyDescriptions = lens _dlbprsPolicyDescriptions (\ s a -> s{_dlbprsPolicyDescriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
dlbprsResponseStatus :: Lens' DescribeLoadBalancerPoliciesResponse Int
dlbprsResponseStatus = lens _dlbprsResponseStatus (\ s a -> s{_dlbprsResponseStatus = a})

instance NFData DescribeLoadBalancerPoliciesResponse
         where
