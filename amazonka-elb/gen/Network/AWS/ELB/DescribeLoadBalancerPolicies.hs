{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    , dlbprqPolicyNames
    , dlbprqLoadBalancerName

    -- * Response
    , DescribeLoadBalancerPoliciesResponse
    -- ** Response constructor
    , describeLoadBalancerPoliciesResponse
    -- ** Response lenses
    , dlbprsPolicyDescriptions
    , dlbprsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancerPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbprqPolicyNames'
--
-- * 'dlbprqLoadBalancerName'
data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies'
    { _dlbprqPolicyNames      :: !(Maybe [Text])
    , _dlbprqLoadBalancerName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancerPolicies' smart constructor.
describeLoadBalancerPolicies :: DescribeLoadBalancerPolicies
describeLoadBalancerPolicies =
    DescribeLoadBalancerPolicies'
    { _dlbprqPolicyNames = Nothing
    , _dlbprqLoadBalancerName = Nothing
    }

-- | The names of the policies.
dlbprqPolicyNames :: Lens' DescribeLoadBalancerPolicies [Text]
dlbprqPolicyNames = lens _dlbprqPolicyNames (\ s a -> s{_dlbprqPolicyNames = a}) . _Default;

-- | The name of the load balancer.
dlbprqLoadBalancerName :: Lens' DescribeLoadBalancerPolicies (Maybe Text)
dlbprqLoadBalancerName = lens _dlbprqLoadBalancerName (\ s a -> s{_dlbprqLoadBalancerName = a});

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
                 toQuery
                   (toQueryList "member" <$> _dlbprqPolicyNames),
               "LoadBalancerName" =: _dlbprqLoadBalancerName]

-- | /See:/ 'describeLoadBalancerPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbprsPolicyDescriptions'
--
-- * 'dlbprsStatus'
data DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse'
    { _dlbprsPolicyDescriptions :: !(Maybe [PolicyDescription])
    , _dlbprsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancerPoliciesResponse' smart constructor.
describeLoadBalancerPoliciesResponse :: Int -> DescribeLoadBalancerPoliciesResponse
describeLoadBalancerPoliciesResponse pStatus =
    DescribeLoadBalancerPoliciesResponse'
    { _dlbprsPolicyDescriptions = Nothing
    , _dlbprsStatus = pStatus
    }

-- | Information about the policies.
dlbprsPolicyDescriptions :: Lens' DescribeLoadBalancerPoliciesResponse [PolicyDescription]
dlbprsPolicyDescriptions = lens _dlbprsPolicyDescriptions (\ s a -> s{_dlbprsPolicyDescriptions = a}) . _Default;

-- | FIXME: Undocumented member.
dlbprsStatus :: Lens' DescribeLoadBalancerPoliciesResponse Int
dlbprsStatus = lens _dlbprsStatus (\ s a -> s{_dlbprsStatus = a});
