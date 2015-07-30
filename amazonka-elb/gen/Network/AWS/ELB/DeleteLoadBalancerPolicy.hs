{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from the specified load balancer. This
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
    , dLoadBalancerName
    , dPolicyName

    -- * Response
    , DeleteLoadBalancerPolicyResponse
    -- ** Response constructor
    , deleteLoadBalancerPolicyResponse
    -- ** Response lenses
    , delrsStatus
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
-- * 'dLoadBalancerName'
--
-- * 'dPolicyName'
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
    { _dLoadBalancerName :: !Text
    , _dPolicyName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerPolicy' smart constructor.
deleteLoadBalancerPolicy :: Text -> Text -> DeleteLoadBalancerPolicy
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
        type Sv DeleteLoadBalancerPolicy = ELB
        type Rs DeleteLoadBalancerPolicy =
             DeleteLoadBalancerPolicyResponse
        request = postQuery
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delrsStatus'
newtype DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
    { _delrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerPolicyResponse' smart constructor.
deleteLoadBalancerPolicyResponse :: Int -> DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicyResponse pStatus_ =
    DeleteLoadBalancerPolicyResponse'
    { _delrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
delrsStatus :: Lens' DeleteLoadBalancerPolicyResponse Int
delrsStatus = lens _delrsStatus (\ s a -> s{_delrsStatus = a});
