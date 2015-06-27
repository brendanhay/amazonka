{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicies
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

-- | Describes the specified policies.
--
-- If you specify a load balancer name, the action returns the descriptions
-- of all policies created for the load balancer. If you specify a policy
-- name associated with your load balancer, the action returns the
-- description of that policy. If you don\'t specify a load balancer name,
-- the action returns descriptions of the specified sample policies, or
-- descriptions of all sample policies. The names of the sample policies
-- have the @ELBSample-@ prefix.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPolicies.html>
module Network.AWS.ELB.DescribeLoadBalancerPolicies
    (
    -- * Request
      DescribeLoadBalancerPolicies
    -- ** Request constructor
    , describeLoadBalancerPolicies
    -- ** Request lenses
    , dlbpPolicyNames
    , dlbpLoadBalancerName

    -- * Response
    , DescribeLoadBalancerPoliciesResponse
    -- ** Response constructor
    , describeLoadBalancerPoliciesResponse
    -- ** Response lenses
    , dlbprPolicyDescriptions
    , dlbprStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancerPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbpPolicyNames'
--
-- * 'dlbpLoadBalancerName'
data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies'
    { _dlbpPolicyNames      :: Maybe [Text]
    , _dlbpLoadBalancerName :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'DescribeLoadBalancerPolicies' smart constructor.
describeLoadBalancerPolicies :: DescribeLoadBalancerPolicies
describeLoadBalancerPolicies =
    DescribeLoadBalancerPolicies'
    { _dlbpPolicyNames = Nothing
    , _dlbpLoadBalancerName = Nothing
    }

-- | The names of the policies.
dlbpPolicyNames :: Lens' DescribeLoadBalancerPolicies [Text]
dlbpPolicyNames = lens _dlbpPolicyNames (\ s a -> s{_dlbpPolicyNames = a}) . _Default;

-- | The name of the load balancer.
dlbpLoadBalancerName :: Lens' DescribeLoadBalancerPolicies (Maybe Text)
dlbpLoadBalancerName = lens _dlbpLoadBalancerName (\ s a -> s{_dlbpLoadBalancerName = a});

instance AWSRequest DescribeLoadBalancerPolicies
         where
        type Sv DescribeLoadBalancerPolicies = ELB
        type Rs DescribeLoadBalancerPolicies =
             DescribeLoadBalancerPoliciesResponse
        request = post
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbprPolicyDescriptions'
--
-- * 'dlbprStatus'
data DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse'
    { _dlbprPolicyDescriptions :: Maybe [PolicyDescription]
    , _dlbprStatus             :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeLoadBalancerPoliciesResponse' smart constructor.
describeLoadBalancerPoliciesResponse :: Int -> DescribeLoadBalancerPoliciesResponse
describeLoadBalancerPoliciesResponse pStatus =
    DescribeLoadBalancerPoliciesResponse'
    { _dlbprPolicyDescriptions = Nothing
    , _dlbprStatus = pStatus
    }

-- | Information about the policies.
dlbprPolicyDescriptions :: Lens' DescribeLoadBalancerPoliciesResponse [PolicyDescription]
dlbprPolicyDescriptions = lens _dlbprPolicyDescriptions (\ s a -> s{_dlbprPolicyDescriptions = a}) . _Default;

-- | FIXME: Undocumented member.
dlbprStatus :: Lens' DescribeLoadBalancerPoliciesResponse Int
dlbprStatus = lens _dlbprStatus (\ s a -> s{_dlbprStatus = a});
