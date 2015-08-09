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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy with the specified attributes for the specified load
-- balancer.
--
-- Policies are settings that are saved for your load balancer and that can
-- be applied to the front-end listener or the back-end application server,
-- depending on the policy type.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerPolicy.html AWS API Reference> for CreateLoadBalancerPolicy.
module Network.AWS.ELB.CreateLoadBalancerPolicy
    (
    -- * Creating a Request
      CreateLoadBalancerPolicy
    , createLoadBalancerPolicy
    -- * Request Lenses
    , clbpPolicyAttributes
    , clbpLoadBalancerName
    , clbpPolicyName
    , clbpPolicyTypeName

    -- * Destructuring the Response
    , CreateLoadBalancerPolicyResponse
    , createLoadBalancerPolicyResponse
    -- * Response Lenses
    , clbprsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

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
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy'
    { _clbpPolicyAttributes :: !(Maybe [PolicyAttribute])
    , _clbpLoadBalancerName :: !Text
    , _clbpPolicyName       :: !Text
    , _clbpPolicyTypeName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoadBalancerPolicy' smart constructor.
createLoadBalancerPolicy :: Text -> Text -> Text -> CreateLoadBalancerPolicy
createLoadBalancerPolicy pLoadBalancerName_ pPolicyName_ pPolicyTypeName_ =
    CreateLoadBalancerPolicy'
    { _clbpPolicyAttributes = Nothing
    , _clbpLoadBalancerName = pLoadBalancerName_
    , _clbpPolicyName = pPolicyName_
    , _clbpPolicyTypeName = pPolicyTypeName_
    }

-- | The attributes for the policy.
clbpPolicyAttributes :: Lens' CreateLoadBalancerPolicy [PolicyAttribute]
clbpPolicyAttributes = lens _clbpPolicyAttributes (\ s a -> s{_clbpPolicyAttributes = a}) . _Default . _Coerce;

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
        request = postQuery
        response
          = receiveXMLWrapper "CreateLoadBalancerPolicyResult"
              (\ s h x ->
                 CreateLoadBalancerPolicyResponse' <$>
                   (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbprsStatus'
newtype CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse'
    { _clbprsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoadBalancerPolicyResponse' smart constructor.
createLoadBalancerPolicyResponse :: Int -> CreateLoadBalancerPolicyResponse
createLoadBalancerPolicyResponse pStatus_ =
    CreateLoadBalancerPolicyResponse'
    { _clbprsStatus = pStatus_
    }

-- | Undocumented member.
clbprsStatus :: Lens' CreateLoadBalancerPolicyResponse Int
clbprsStatus = lens _clbprsStatus (\ s a -> s{_clbprsStatus = a});
