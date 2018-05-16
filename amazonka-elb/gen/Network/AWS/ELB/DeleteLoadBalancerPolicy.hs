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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from the specified load balancer. This policy must not be enabled for any listeners.
--
--
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
    , delrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeleteLoadBalancerPolicy.
--
--
--
-- /See:/ 'deleteLoadBalancerPolicy' smart constructor.
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
  { _dLoadBalancerName :: !Text
  , _dPolicyName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoadBalancerPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLoadBalancerName' - The name of the load balancer.
--
-- * 'dPolicyName' - The name of the policy.
deleteLoadBalancerPolicy
    :: Text -- ^ 'dLoadBalancerName'
    -> Text -- ^ 'dPolicyName'
    -> DeleteLoadBalancerPolicy
deleteLoadBalancerPolicy pLoadBalancerName_ pPolicyName_ =
  DeleteLoadBalancerPolicy'
    {_dLoadBalancerName = pLoadBalancerName_, _dPolicyName = pPolicyName_}


-- | The name of the load balancer.
dLoadBalancerName :: Lens' DeleteLoadBalancerPolicy Text
dLoadBalancerName = lens _dLoadBalancerName (\ s a -> s{_dLoadBalancerName = a})

-- | The name of the policy.
dPolicyName :: Lens' DeleteLoadBalancerPolicy Text
dPolicyName = lens _dPolicyName (\ s a -> s{_dPolicyName = a})

instance AWSRequest DeleteLoadBalancerPolicy where
        type Rs DeleteLoadBalancerPolicy =
             DeleteLoadBalancerPolicyResponse
        request = postQuery elb
        response
          = receiveXMLWrapper "DeleteLoadBalancerPolicyResult"
              (\ s h x ->
                 DeleteLoadBalancerPolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteLoadBalancerPolicy where

instance NFData DeleteLoadBalancerPolicy where

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

-- | Contains the output of DeleteLoadBalancerPolicy.
--
--
--
-- /See:/ 'deleteLoadBalancerPolicyResponse' smart constructor.
newtype DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoadBalancerPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteLoadBalancerPolicyResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicyResponse pResponseStatus_ =
  DeleteLoadBalancerPolicyResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteLoadBalancerPolicyResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteLoadBalancerPolicyResponse
         where
