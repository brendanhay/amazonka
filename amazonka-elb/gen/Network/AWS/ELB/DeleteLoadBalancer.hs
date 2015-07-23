{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified load balancer.
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
    , dlbrqLoadBalancerName

    -- * Response
    , DeleteLoadBalancerResponse
    -- ** Response constructor
    , deleteLoadBalancerResponse
    -- ** Response lenses
    , drsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbrqLoadBalancerName'
newtype DeleteLoadBalancer = DeleteLoadBalancer'
    { _dlbrqLoadBalancerName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancer' smart constructor.
deleteLoadBalancer :: Text -> DeleteLoadBalancer
deleteLoadBalancer pLoadBalancerName_ =
    DeleteLoadBalancer'
    { _dlbrqLoadBalancerName = pLoadBalancerName_
    }

-- | The name of the load balancer.
dlbrqLoadBalancerName :: Lens' DeleteLoadBalancer Text
dlbrqLoadBalancerName = lens _dlbrqLoadBalancerName (\ s a -> s{_dlbrqLoadBalancerName = a});

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
               "LoadBalancerName" =: _dlbrqLoadBalancerName]

-- | /See:/ 'deleteLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsStatus'
newtype DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerResponse' smart constructor.
deleteLoadBalancerResponse :: Int -> DeleteLoadBalancerResponse
deleteLoadBalancerResponse pStatus_ =
    DeleteLoadBalancerResponse'
    { _drsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeleteLoadBalancerResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
