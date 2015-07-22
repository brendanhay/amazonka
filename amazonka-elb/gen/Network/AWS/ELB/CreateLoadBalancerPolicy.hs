{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateLoadBalancerPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy with the specified attributes for the specified load
-- balancer.
--
-- Policies are settings that are saved for your load balancer and that can
-- be applied to the front-end listener or the back-end application server,
-- depending on the policy type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerPolicy.html>
module Network.AWS.ELB.CreateLoadBalancerPolicy
    (
    -- * Request
      CreateLoadBalancerPolicy
    -- ** Request constructor
    , createLoadBalancerPolicy
    -- ** Request lenses
    , clbprqPolicyAttributes
    , clbprqLoadBalancerName
    , clbprqPolicyName
    , clbprqPolicyTypeName

    -- * Response
    , CreateLoadBalancerPolicyResponse
    -- ** Response constructor
    , createLoadBalancerPolicyResponse
    -- ** Response lenses
    , clbprsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLoadBalancerPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbprqPolicyAttributes'
--
-- * 'clbprqLoadBalancerName'
--
-- * 'clbprqPolicyName'
--
-- * 'clbprqPolicyTypeName'
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy'
    { _clbprqPolicyAttributes :: !(Maybe [PolicyAttribute])
    , _clbprqLoadBalancerName :: !Text
    , _clbprqPolicyName       :: !Text
    , _clbprqPolicyTypeName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoadBalancerPolicy' smart constructor.
createLoadBalancerPolicy :: Text -> Text -> Text -> CreateLoadBalancerPolicy
createLoadBalancerPolicy pLoadBalancerName pPolicyName pPolicyTypeName =
    CreateLoadBalancerPolicy'
    { _clbprqPolicyAttributes = Nothing
    , _clbprqLoadBalancerName = pLoadBalancerName
    , _clbprqPolicyName = pPolicyName
    , _clbprqPolicyTypeName = pPolicyTypeName
    }

-- | The attributes for the policy.
clbprqPolicyAttributes :: Lens' CreateLoadBalancerPolicy [PolicyAttribute]
clbprqPolicyAttributes = lens _clbprqPolicyAttributes (\ s a -> s{_clbprqPolicyAttributes = a}) . _Default;

-- | The name of the load balancer.
clbprqLoadBalancerName :: Lens' CreateLoadBalancerPolicy Text
clbprqLoadBalancerName = lens _clbprqLoadBalancerName (\ s a -> s{_clbprqLoadBalancerName = a});

-- | The name of the load balancer policy to be created. This name must be
-- unique within the set of policies for this load balancer.
clbprqPolicyName :: Lens' CreateLoadBalancerPolicy Text
clbprqPolicyName = lens _clbprqPolicyName (\ s a -> s{_clbprqPolicyName = a});

-- | The name of the base policy type. To get the list of policy types, use
-- DescribeLoadBalancerPolicyTypes.
clbprqPolicyTypeName :: Lens' CreateLoadBalancerPolicy Text
clbprqPolicyTypeName = lens _clbprqPolicyTypeName (\ s a -> s{_clbprqPolicyTypeName = a});

instance AWSRequest CreateLoadBalancerPolicy where
        type Sv CreateLoadBalancerPolicy = ELB
        type Rs CreateLoadBalancerPolicy =
             CreateLoadBalancerPolicyResponse
        request = post
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
                   (toQueryList "member" <$> _clbprqPolicyAttributes),
               "LoadBalancerName" =: _clbprqLoadBalancerName,
               "PolicyName" =: _clbprqPolicyName,
               "PolicyTypeName" =: _clbprqPolicyTypeName]

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
createLoadBalancerPolicyResponse pStatus =
    CreateLoadBalancerPolicyResponse'
    { _clbprsStatus = pStatus
    }

-- | FIXME: Undocumented member.
clbprsStatus :: Lens' CreateLoadBalancerPolicyResponse Int
clbprsStatus = lens _clbprsStatus (\ s a -> s{_clbprsStatus = a});
