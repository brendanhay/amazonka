{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified load balancer policy types.
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
    , dlbptrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancerPolicyTypes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbptPolicyTypeNames'
newtype DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes'
    { _dlbptPolicyTypeNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancerPolicyTypes' smart constructor.
describeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypes
describeLoadBalancerPolicyTypes =
    DescribeLoadBalancerPolicyTypes'
    { _dlbptPolicyTypeNames = Nothing
    }

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
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
--
-- * 'dlbptrStatus'
data DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse'
    { _dlbptrPolicyTypeDescriptions :: !(Maybe [PolicyTypeDescription])
    , _dlbptrStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancerPolicyTypesResponse' smart constructor.
describeLoadBalancerPolicyTypesResponse :: Int -> DescribeLoadBalancerPolicyTypesResponse
describeLoadBalancerPolicyTypesResponse pStatus =
    DescribeLoadBalancerPolicyTypesResponse'
    { _dlbptrPolicyTypeDescriptions = Nothing
    , _dlbptrStatus = pStatus
    }

-- | Information about the policy types.
dlbptrPolicyTypeDescriptions :: Lens' DescribeLoadBalancerPolicyTypesResponse [PolicyTypeDescription]
dlbptrPolicyTypeDescriptions = lens _dlbptrPolicyTypeDescriptions (\ s a -> s{_dlbptrPolicyTypeDescriptions = a}) . _Default;

-- | FIXME: Undocumented member.
dlbptrStatus :: Lens' DescribeLoadBalancerPolicyTypesResponse Int
dlbptrStatus = lens _dlbptrStatus (\ s a -> s{_dlbptrStatus = a});
