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
-- Module      : Network.AWS.ELB.DeleteLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- call to 'DeleteLoadBalancer' still succeeds.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancer.html AWS API Reference> for DeleteLoadBalancer.
module Network.AWS.ELB.DeleteLoadBalancer
    (
    -- * Creating a Request
      deleteLoadBalancer
    , DeleteLoadBalancer
    -- * Request Lenses
    , dlbLoadBalancerName

    -- * Destructuring the Response
    , deleteLoadBalancerResponse
    , DeleteLoadBalancerResponse
    -- * Response Lenses
    , drsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLoadBalancer' smart constructor.
newtype DeleteLoadBalancer = DeleteLoadBalancer'
    { _dlbLoadBalancerName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbLoadBalancerName'
deleteLoadBalancer
    :: Text -- ^ 'dlbLoadBalancerName'
    -> DeleteLoadBalancer
deleteLoadBalancer pLoadBalancerName_ =
    DeleteLoadBalancer'
    { _dlbLoadBalancerName = pLoadBalancerName_
    }

-- | The name of the load balancer.
dlbLoadBalancerName :: Lens' DeleteLoadBalancer Text
dlbLoadBalancerName = lens _dlbLoadBalancerName (\ s a -> s{_dlbLoadBalancerName = a});

instance AWSRequest DeleteLoadBalancer where
        type Rs DeleteLoadBalancer =
             DeleteLoadBalancerResponse
        request = postQuery eLB
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
newtype DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsStatus'
deleteLoadBalancerResponse
    :: Int -- ^ 'drsStatus'
    -> DeleteLoadBalancerResponse
deleteLoadBalancerResponse pStatus_ =
    DeleteLoadBalancerResponse'
    { _drsStatus = pStatus_
    }

-- | The response status code.
drsStatus :: Lens' DeleteLoadBalancerResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
