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
-- Module      : Network.AWS.ELB.CreateLoadBalancerPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy with the specified attributes for the specified load balancer.
--
--
-- Policies are settings that are saved for your load balancer and that can be applied to the listener or the application server, depending on the policy type.
--
module Network.AWS.ELB.CreateLoadBalancerPolicy
    (
    -- * Creating a Request
      createLoadBalancerPolicy
    , CreateLoadBalancerPolicy
    -- * Request Lenses
    , clbpPolicyAttributes
    , clbpLoadBalancerName
    , clbpPolicyName
    , clbpPolicyTypeName

    -- * Destructuring the Response
    , createLoadBalancerPolicyResponse
    , CreateLoadBalancerPolicyResponse
    -- * Response Lenses
    , clbprsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateLoadBalancerPolicy.
--
--
--
-- /See:/ 'createLoadBalancerPolicy' smart constructor.
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy'
  { _clbpPolicyAttributes :: !(Maybe [PolicyAttribute])
  , _clbpLoadBalancerName :: !Text
  , _clbpPolicyName       :: !Text
  , _clbpPolicyTypeName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoadBalancerPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbpPolicyAttributes' - The policy attributes.
--
-- * 'clbpLoadBalancerName' - The name of the load balancer.
--
-- * 'clbpPolicyName' - The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
--
-- * 'clbpPolicyTypeName' - The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
createLoadBalancerPolicy
    :: Text -- ^ 'clbpLoadBalancerName'
    -> Text -- ^ 'clbpPolicyName'
    -> Text -- ^ 'clbpPolicyTypeName'
    -> CreateLoadBalancerPolicy
createLoadBalancerPolicy pLoadBalancerName_ pPolicyName_ pPolicyTypeName_ =
  CreateLoadBalancerPolicy'
    { _clbpPolicyAttributes = Nothing
    , _clbpLoadBalancerName = pLoadBalancerName_
    , _clbpPolicyName = pPolicyName_
    , _clbpPolicyTypeName = pPolicyTypeName_
    }


-- | The policy attributes.
clbpPolicyAttributes :: Lens' CreateLoadBalancerPolicy [PolicyAttribute]
clbpPolicyAttributes = lens _clbpPolicyAttributes (\ s a -> s{_clbpPolicyAttributes = a}) . _Default . _Coerce

-- | The name of the load balancer.
clbpLoadBalancerName :: Lens' CreateLoadBalancerPolicy Text
clbpLoadBalancerName = lens _clbpLoadBalancerName (\ s a -> s{_clbpLoadBalancerName = a})

-- | The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
clbpPolicyName :: Lens' CreateLoadBalancerPolicy Text
clbpPolicyName = lens _clbpPolicyName (\ s a -> s{_clbpPolicyName = a})

-- | The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
clbpPolicyTypeName :: Lens' CreateLoadBalancerPolicy Text
clbpPolicyTypeName = lens _clbpPolicyTypeName (\ s a -> s{_clbpPolicyTypeName = a})

instance AWSRequest CreateLoadBalancerPolicy where
        type Rs CreateLoadBalancerPolicy =
             CreateLoadBalancerPolicyResponse
        request = postQuery elb
        response
          = receiveXMLWrapper "CreateLoadBalancerPolicyResult"
              (\ s h x ->
                 CreateLoadBalancerPolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateLoadBalancerPolicy where

instance NFData CreateLoadBalancerPolicy where

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

-- | Contains the output of CreateLoadBalancerPolicy.
--
--
--
-- /See:/ 'createLoadBalancerPolicyResponse' smart constructor.
newtype CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse'
  { _clbprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoadBalancerPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbprsResponseStatus' - -- | The response status code.
createLoadBalancerPolicyResponse
    :: Int -- ^ 'clbprsResponseStatus'
    -> CreateLoadBalancerPolicyResponse
createLoadBalancerPolicyResponse pResponseStatus_ =
  CreateLoadBalancerPolicyResponse' {_clbprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
clbprsResponseStatus :: Lens' CreateLoadBalancerPolicyResponse Int
clbprsResponseStatus = lens _clbprsResponseStatus (\ s a -> s{_clbprsResponseStatus = a})

instance NFData CreateLoadBalancerPolicyResponse
         where
