{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.ELB.DeleteLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified load balancer.
--
-- If you are attempting to recreate a load balancer, you must reconfigure
-- all settings. The DNS name associated with a deleted load balancer are
-- no longer usable. The name and associated DNS record of the deleted load
-- balancer no longer exist and traffic sent to any of its IP addresses is
-- no longer delivered to back-end instances.
--
-- If the load balancer does not exist or has already been deleted, the
-- call to @DeleteLoadBalancer@ still succeeds.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancer.html>
module Network.AWS.ELB.DeleteLoadBalancer
    (
    -- * Request
      DeleteLoadBalancer
    -- ** Request constructor
    , deleteLoadBalancer
    -- ** Request lenses
    , dlbLoadBalancerName

    -- * Response
    , DeleteLoadBalancerResponse
    -- ** Response constructor
    , deleteLoadBalancerResponse
    -- ** Response lenses
    , delStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbLoadBalancerName'
newtype DeleteLoadBalancer = DeleteLoadBalancer'
    { _dlbLoadBalancerName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancer' smart constructor.
deleteLoadBalancer :: Text -> DeleteLoadBalancer
deleteLoadBalancer pLoadBalancerName =
    DeleteLoadBalancer'
    { _dlbLoadBalancerName = pLoadBalancerName
    }

-- | The name of the load balancer.
dlbLoadBalancerName :: Lens' DeleteLoadBalancer Text
dlbLoadBalancerName = lens _dlbLoadBalancerName (\ s a -> s{_dlbLoadBalancerName = a});

instance AWSRequest DeleteLoadBalancer where
        type Sv DeleteLoadBalancer = ELB
        type Rs DeleteLoadBalancer =
             DeleteLoadBalancerResponse
        request = post
        response
          = receiveXMLWrapper "DeleteLoadBalancerResult"
              (\ s h x ->
                 DeleteLoadBalancerResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteLoadBalancer where
        toHeaders = const mempty

instance ToPath DeleteLoadBalancer where
        toPath = const "/"

instance ToQuery DeleteLoadBalancer where
        toQuery DeleteLoadBalancer'{..}
          = mconcat
              ["Action" =: ("DeleteLoadBalancer" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _dlbLoadBalancerName]

-- | /See:/ 'deleteLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delStatus'
newtype DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
    { _delStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerResponse' smart constructor.
deleteLoadBalancerResponse :: Int -> DeleteLoadBalancerResponse
deleteLoadBalancerResponse pStatus =
    DeleteLoadBalancerResponse'
    { _delStatus = pStatus
    }

-- | FIXME: Undocumented member.
delStatus :: Lens' DeleteLoadBalancerResponse Int
delStatus = lens _delStatus (\ s a -> s{_delStatus = a});
