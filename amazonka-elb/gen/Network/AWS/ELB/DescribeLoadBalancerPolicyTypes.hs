{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
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

-- | Describes the specified load balancer policy types.
--
-- You can use these policy types with CreateLoadBalancerPolicy to create
-- policy configurations for a load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPolicyTypes.html>
module Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
    (
    -- * Request
      DescribeLoadBalancerPolicyTypes
    -- ** Request constructor
    , describeLoadBalancerPolicyTypes
    -- ** Request lenses
    , dlbptPolicyTypeNames

    -- * Response
    , DescribeLoadBalancerPolicyTypesResponse
    -- ** Response constructor
    , describeLoadBalancerPolicyTypesResponse
    -- ** Response lenses
    , dlbptrPolicyTypeDescriptions
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ELB.Types

-- | /See:/ 'describeLoadBalancerPolicyTypes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbptPolicyTypeNames'
newtype DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes'{_dlbptPolicyTypeNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeLoadBalancerPolicyTypes' smart constructor.
describeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypes
describeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes'{_dlbptPolicyTypeNames = Nothing};

-- | The names of the policy types. If no names are specified, describes all
-- policy types defined by Elastic Load Balancing.
dlbptPolicyTypeNames :: Lens' DescribeLoadBalancerPolicyTypes [Text]
dlbptPolicyTypeNames = lens _dlbptPolicyTypeNames (\ s a -> s{_dlbptPolicyTypeNames = a}) . _Default;

instance AWSRequest DescribeLoadBalancerPolicyTypes
         where
        type Sv DescribeLoadBalancerPolicyTypes = ELB
        type Rs DescribeLoadBalancerPolicyTypes =
             DescribeLoadBalancerPolicyTypesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeLoadBalancerPolicyTypesResult"
              (\ s h x ->
                 DescribeLoadBalancerPolicyTypesResponse' <$>
                   (x .@? "PolicyTypeDescriptions" .!@ mempty >>=
                      may (parseXMLList "member")))

instance ToHeaders DescribeLoadBalancerPolicyTypes
         where
        toHeaders = const mempty

instance ToPath DescribeLoadBalancerPolicyTypes where
        toPath = const "/"

instance ToQuery DescribeLoadBalancerPolicyTypes
         where
        toQuery DescribeLoadBalancerPolicyTypes'{..}
          = mconcat
              ["Action" =:
                 ("DescribeLoadBalancerPolicyTypes" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "PolicyTypeNames" =:
                 toQuery
                   (toQueryList "member" <$> _dlbptPolicyTypeNames)]

-- | /See:/ 'describeLoadBalancerPolicyTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbptrPolicyTypeDescriptions'
newtype DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse'{_dlbptrPolicyTypeDescriptions :: Maybe [PolicyTypeDescription]} deriving (Eq, Read, Show)

-- | 'DescribeLoadBalancerPolicyTypesResponse' smart constructor.
describeLoadBalancerPolicyTypesResponse :: DescribeLoadBalancerPolicyTypesResponse
describeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse'{_dlbptrPolicyTypeDescriptions = Nothing};

-- | Information about the policy types.
dlbptrPolicyTypeDescriptions :: Lens' DescribeLoadBalancerPolicyTypesResponse [PolicyTypeDescription]
dlbptrPolicyTypeDescriptions = lens _dlbptrPolicyTypeDescriptions (\ s a -> s{_dlbptrPolicyTypeDescriptions = a}) . _Default;
