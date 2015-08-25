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
-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from the specified load balancer. This
-- policy must not be enabled for any listeners.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerPolicy.html AWS API Reference> for DeleteLoadBalancerPolicy.
module Network.AWS.ELB.DeleteLoadBalancerPolicy
    (
    -- * Creating a Request
      deleteLoadBalancerPolicy
    , DeleteLoadBalancerPolicy
    -- * Request Lenses
    , dLoadBalancerName
    , dPolicyName

    -- * Destructuring the Response
    , deleteLoadBalancerPolicyResponse
    , DeleteLoadBalancerPolicyResponse
    -- * Response Lenses
    , delrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | =
--
-- /See:/ 'deleteLoadBalancerPolicy' smart constructor.
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
    { _dLoadBalancerName :: !Text
    , _dPolicyName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteLoadBalancerPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLoadBalancerName'
--
-- * 'dPolicyName'
deleteLoadBalancerPolicy
    :: Text -- ^ 'dLoadBalancerName'
    -> Text -- ^ 'dPolicyName'
    -> DeleteLoadBalancerPolicy
deleteLoadBalancerPolicy pLoadBalancerName_ pPolicyName_ =
    DeleteLoadBalancerPolicy'
    { _dLoadBalancerName = pLoadBalancerName_
    , _dPolicyName = pPolicyName_
    }

-- | The name of the load balancer.
dLoadBalancerName :: Lens' DeleteLoadBalancerPolicy Text
dLoadBalancerName = lens _dLoadBalancerName (\ s a -> s{_dLoadBalancerName = a});

-- | The name of the policy.
dPolicyName :: Lens' DeleteLoadBalancerPolicy Text
dPolicyName = lens _dPolicyName (\ s a -> s{_dPolicyName = a});

instance AWSRequest DeleteLoadBalancerPolicy where
        type Rs DeleteLoadBalancerPolicy =
             DeleteLoadBalancerPolicyResponse
        request = postQuery eLB
        response
          = receiveXMLWrapper "DeleteLoadBalancerPolicyResult"
              (\ s h x ->
                 DeleteLoadBalancerPolicyResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders DeleteLoadBalancerPolicy where
        toHeaders = const mempty

instance ToPath DeleteLoadBalancerPolicy where
        toPath = const "/"

instance ToQuery DeleteLoadBalancerPolicy where
        toQuery DeleteLoadBalancerPolicy'{..}
          = mconcat
              ["Action" =:
                 ("DeleteLoadBalancerPolicy" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _dLoadBalancerName,
               "PolicyName" =: _dPolicyName]

-- | /See:/ 'deleteLoadBalancerPolicyResponse' smart constructor.
newtype DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
    { _delrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteLoadBalancerPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsStatus'
deleteLoadBalancerPolicyResponse
    :: Int -- ^ 'delrsStatus'
    -> DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicyResponse pStatus_ =
    DeleteLoadBalancerPolicyResponse'
    { _delrsStatus = pStatus_
    }

-- | The response status code.
delrsStatus :: Lens' DeleteLoadBalancerPolicyResponse Int
delrsStatus = lens _delrsStatus (\ s a -> s{_delrsStatus = a});
