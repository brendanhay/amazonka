{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
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

-- | Deletes the specified policy from the specified load balancer. This
-- policy must not be enabled for any listeners.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerPolicy.html>
module Network.AWS.ELB.DeleteLoadBalancerPolicy
    (
    -- * Request
      DeleteLoadBalancerPolicy
    -- ** Request constructor
    , deleteLoadBalancerPolicy
    -- ** Request lenses
    , delLoadBalancerName
    , delPolicyName

    -- * Response
    , DeleteLoadBalancerPolicyResponse
    -- ** Response constructor
    , deleteLoadBalancerPolicyResponse
    ) where

import Network.AWS.ELB.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLoadBalancerPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delLoadBalancerName'
--
-- * 'delPolicyName'
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'{_delLoadBalancerName :: Text, _delPolicyName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteLoadBalancerPolicy' smart constructor.
deleteLoadBalancerPolicy :: Text -> Text -> DeleteLoadBalancerPolicy
deleteLoadBalancerPolicy pLoadBalancerName pPolicyName = DeleteLoadBalancerPolicy'{_delLoadBalancerName = pLoadBalancerName, _delPolicyName = pPolicyName};

-- | The name of the load balancer.
delLoadBalancerName :: Lens' DeleteLoadBalancerPolicy Text
delLoadBalancerName = lens _delLoadBalancerName (\ s a -> s{_delLoadBalancerName = a});

-- | The name of the policy.
delPolicyName :: Lens' DeleteLoadBalancerPolicy Text
delPolicyName = lens _delPolicyName (\ s a -> s{_delPolicyName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteLoadBalancerPolicy where
        type Sv DeleteLoadBalancerPolicy = ELB
        type Rs DeleteLoadBalancerPolicy =
             DeleteLoadBalancerPolicyResponse
        request = post
        response
          = receiveNull DeleteLoadBalancerPolicyResponse'

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
               "LoadBalancerName" =: _delLoadBalancerName,
               "PolicyName" =: _delPolicyName]

-- | /See:/ 'deleteLoadBalancerPolicyResponse' smart constructor.
data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse' deriving (Eq, Read, Show)

-- | 'DeleteLoadBalancerPolicyResponse' smart constructor.
deleteLoadBalancerPolicyResponse :: DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse';
