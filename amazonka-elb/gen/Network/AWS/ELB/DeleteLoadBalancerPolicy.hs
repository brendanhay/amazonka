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
    , drqLoadBalancerName
    , drqPolicyName

    -- * Response
    , DeleteLoadBalancerPolicyResponse
    -- ** Response constructor
    , deleteLoadBalancerPolicyResponse
    -- ** Response lenses
    , drsStatus
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
-- * 'drqLoadBalancerName'
--
-- * 'drqPolicyName'
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
    { _drqLoadBalancerName :: !Text
    , _drqPolicyName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerPolicy' smart constructor.
deleteLoadBalancerPolicy :: Text -> Text -> DeleteLoadBalancerPolicy
deleteLoadBalancerPolicy pLoadBalancerName pPolicyName =
    DeleteLoadBalancerPolicy'
    { _drqLoadBalancerName = pLoadBalancerName
    , _drqPolicyName = pPolicyName
    }

-- | The name of the load balancer.
drqLoadBalancerName :: Lens' DeleteLoadBalancerPolicy Text
drqLoadBalancerName = lens _drqLoadBalancerName (\ s a -> s{_drqLoadBalancerName = a});

-- | The name of the policy.
drqPolicyName :: Lens' DeleteLoadBalancerPolicy Text
drqPolicyName = lens _drqPolicyName (\ s a -> s{_drqPolicyName = a});

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
               "LoadBalancerName" =: _drqLoadBalancerName,
               "PolicyName" =: _drqPolicyName]

-- | /See:/ 'deleteLoadBalancerPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsStatus'
newtype DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerPolicyResponse' smart constructor.
deleteLoadBalancerPolicyResponse :: Int -> DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicyResponse pStatus =
    DeleteLoadBalancerPolicyResponse'
    { _drsStatus = pStatus
    }

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeleteLoadBalancerPolicyResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
