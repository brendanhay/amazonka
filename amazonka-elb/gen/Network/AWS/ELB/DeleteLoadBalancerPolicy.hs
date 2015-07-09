{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    -- ** Response lenses
    , dStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | =
--
-- /See:/ 'deleteLoadBalancerPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delLoadBalancerName'
--
-- * 'delPolicyName'
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
    { _delLoadBalancerName :: !Text
    , _delPolicyName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerPolicy' smart constructor.
deleteLoadBalancerPolicy :: Text -> Text -> DeleteLoadBalancerPolicy
deleteLoadBalancerPolicy pLoadBalancerName pPolicyName =
    DeleteLoadBalancerPolicy'
    { _delLoadBalancerName = pLoadBalancerName
    , _delPolicyName = pPolicyName
    }

-- | The name of the load balancer.
delLoadBalancerName :: Lens' DeleteLoadBalancerPolicy Text
delLoadBalancerName = lens _delLoadBalancerName (\ s a -> s{_delLoadBalancerName = a});

-- | The name of the policy.
delPolicyName :: Lens' DeleteLoadBalancerPolicy Text
delPolicyName = lens _delPolicyName (\ s a -> s{_delPolicyName = a});

instance AWSRequest DeleteLoadBalancerPolicy where
        type Sv DeleteLoadBalancerPolicy = ELB
        type Rs DeleteLoadBalancerPolicy =
             DeleteLoadBalancerPolicyResponse
        request = post
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
               "LoadBalancerName" =: _delLoadBalancerName,
               "PolicyName" =: _delPolicyName]

-- | /See:/ 'deleteLoadBalancerPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dStatus'
newtype DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
    { _dStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerPolicyResponse' smart constructor.
deleteLoadBalancerPolicyResponse :: Int -> DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicyResponse pStatus =
    DeleteLoadBalancerPolicyResponse'
    { _dStatus = pStatus
    }

-- | FIXME: Undocumented member.
dStatus :: Lens' DeleteLoadBalancerPolicyResponse Int
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});
