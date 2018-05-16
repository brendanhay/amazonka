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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified load balancer policy types or all load balancer policy types.
--
--
-- The description of each type indicates how it can be used. For example, some policies can be used only with layer 7 listeners, some policies can be used only with layer 4 listeners, and some policies can be used only with your EC2 instances.
--
-- You can use 'CreateLoadBalancerPolicy' to create a policy configuration for any of these policy types. Then, depending on the policy type, use either 'SetLoadBalancerPoliciesOfListener' or 'SetLoadBalancerPoliciesForBackendServer' to set the policy.
--
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
    , dlbptrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeLoadBalancerPolicyTypes.
--
--
--
-- /See:/ 'describeLoadBalancerPolicyTypes' smart constructor.
newtype DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes'
  { _dlbptPolicyTypeNames :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancerPolicyTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbptPolicyTypeNames' - The names of the policy types. If no names are specified, describes all policy types defined by Elastic Load Balancing.
describeLoadBalancerPolicyTypes
    :: DescribeLoadBalancerPolicyTypes
describeLoadBalancerPolicyTypes =
  DescribeLoadBalancerPolicyTypes' {_dlbptPolicyTypeNames = Nothing}


-- | The names of the policy types. If no names are specified, describes all policy types defined by Elastic Load Balancing.
dlbptPolicyTypeNames :: Lens' DescribeLoadBalancerPolicyTypes [Text]
dlbptPolicyTypeNames = lens _dlbptPolicyTypeNames (\ s a -> s{_dlbptPolicyTypeNames = a}) . _Default . _Coerce

instance AWSRequest DescribeLoadBalancerPolicyTypes
         where
        type Rs DescribeLoadBalancerPolicyTypes =
             DescribeLoadBalancerPolicyTypesResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "DescribeLoadBalancerPolicyTypesResult"
              (\ s h x ->
                 DescribeLoadBalancerPolicyTypesResponse' <$>
                   (x .@? "PolicyTypeDescriptions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLoadBalancerPolicyTypes
         where

instance NFData DescribeLoadBalancerPolicyTypes where

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

-- | Contains the output of DescribeLoadBalancerPolicyTypes.
--
--
--
-- /See:/ 'describeLoadBalancerPolicyTypesResponse' smart constructor.
data DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse'
  { _dlbptrsPolicyTypeDescriptions :: !(Maybe [PolicyTypeDescription])
  , _dlbptrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancerPolicyTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbptrsPolicyTypeDescriptions' - Information about the policy types.
--
-- * 'dlbptrsResponseStatus' - -- | The response status code.
describeLoadBalancerPolicyTypesResponse
    :: Int -- ^ 'dlbptrsResponseStatus'
    -> DescribeLoadBalancerPolicyTypesResponse
describeLoadBalancerPolicyTypesResponse pResponseStatus_ =
  DescribeLoadBalancerPolicyTypesResponse'
    { _dlbptrsPolicyTypeDescriptions = Nothing
    , _dlbptrsResponseStatus = pResponseStatus_
    }


-- | Information about the policy types.
dlbptrsPolicyTypeDescriptions :: Lens' DescribeLoadBalancerPolicyTypesResponse [PolicyTypeDescription]
dlbptrsPolicyTypeDescriptions = lens _dlbptrsPolicyTypeDescriptions (\ s a -> s{_dlbptrsPolicyTypeDescriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
dlbptrsResponseStatus :: Lens' DescribeLoadBalancerPolicyTypesResponse Int
dlbptrsResponseStatus = lens _dlbptrsResponseStatus (\ s a -> s{_dlbptrsResponseStatus = a})

instance NFData
           DescribeLoadBalancerPolicyTypesResponse
         where
