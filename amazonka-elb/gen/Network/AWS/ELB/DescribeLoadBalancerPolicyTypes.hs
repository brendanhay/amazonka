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
-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified load balancer policy types.
--
-- You can use these policy types with CreateLoadBalancerPolicy to create
-- policy configurations for a load balancer.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPolicyTypes.html AWS API Reference> for DescribeLoadBalancerPolicyTypes.
module Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
    (
    -- * Creating a Request
      describeLoadBalancerPolicyTypes
    , DescribeLoadBalancerPolicyTypes
    -- * Request Lenses
    , dlbptPolicyTypeNames

    -- * Destructuring the Response
    , describeLoadBalancerPolicyTypesResponse
    , DescribeLoadBalancerPolicyTypesResponse
    -- * Response Lenses
    , dlbptrsPolicyTypeDescriptions
    , dlbptrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancerPolicyTypes' smart constructor.
newtype DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes'
    { _dlbptPolicyTypeNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLoadBalancerPolicyTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbptPolicyTypeNames'
describeLoadBalancerPolicyTypes
    :: DescribeLoadBalancerPolicyTypes
describeLoadBalancerPolicyTypes =
    DescribeLoadBalancerPolicyTypes'
    { _dlbptPolicyTypeNames = Nothing
    }

-- | The names of the policy types. If no names are specified, describes all
-- policy types defined by Elastic Load Balancing.
dlbptPolicyTypeNames :: Lens' DescribeLoadBalancerPolicyTypes [Text]
dlbptPolicyTypeNames = lens _dlbptPolicyTypeNames (\ s a -> s{_dlbptPolicyTypeNames = a}) . _Default . _Coerce;

instance AWSRequest DescribeLoadBalancerPolicyTypes
         where
        type Rs DescribeLoadBalancerPolicyTypes =
             DescribeLoadBalancerPolicyTypesResponse
        request = postQuery eLB
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
data DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse'
    { _dlbptrsPolicyTypeDescriptions :: !(Maybe [PolicyTypeDescription])
    , _dlbptrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLoadBalancerPolicyTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbptrsPolicyTypeDescriptions'
--
-- * 'dlbptrsStatus'
describeLoadBalancerPolicyTypesResponse
    :: Int -- ^ 'dlbptrsStatus'
    -> DescribeLoadBalancerPolicyTypesResponse
describeLoadBalancerPolicyTypesResponse pStatus_ =
    DescribeLoadBalancerPolicyTypesResponse'
    { _dlbptrsPolicyTypeDescriptions = Nothing
    , _dlbptrsStatus = pStatus_
    }

-- | Information about the policy types.
dlbptrsPolicyTypeDescriptions :: Lens' DescribeLoadBalancerPolicyTypesResponse [PolicyTypeDescription]
dlbptrsPolicyTypeDescriptions = lens _dlbptrsPolicyTypeDescriptions (\ s a -> s{_dlbptrsPolicyTypeDescriptions = a}) . _Default . _Coerce;

-- | The response status code.
dlbptrsStatus :: Lens' DescribeLoadBalancerPolicyTypesResponse Int
dlbptrsStatus = lens _dlbptrsStatus (\ s a -> s{_dlbptrsStatus = a});
