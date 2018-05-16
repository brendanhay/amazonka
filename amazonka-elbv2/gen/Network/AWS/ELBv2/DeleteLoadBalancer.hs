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
-- Module      : Network.AWS.ELBv2.DeleteLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Application Load Balancer or Network Load Balancer and its attached listeners.
--
--
-- You can't delete a load balancer if deletion protection is enabled. If the load balancer does not exist or has already been deleted, the call succeeds.
--
-- Deleting a load balancer does not affect its registered targets. For example, your EC2 instances continue to run and are still registered to their target groups. If you no longer need these EC2 instances, you can stop or terminate them.
--
module Network.AWS.ELBv2.DeleteLoadBalancer
    (
    -- * Creating a Request
      deleteLoadBalancer
    , DeleteLoadBalancer
    -- * Request Lenses
    , dlbLoadBalancerARN

    -- * Destructuring the Response
    , deleteLoadBalancerResponse
    , DeleteLoadBalancerResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLoadBalancer' smart constructor.
newtype DeleteLoadBalancer = DeleteLoadBalancer'
  { _dlbLoadBalancerARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
deleteLoadBalancer
    :: Text -- ^ 'dlbLoadBalancerARN'
    -> DeleteLoadBalancer
deleteLoadBalancer pLoadBalancerARN_ =
  DeleteLoadBalancer' {_dlbLoadBalancerARN = pLoadBalancerARN_}


-- | The Amazon Resource Name (ARN) of the load balancer.
dlbLoadBalancerARN :: Lens' DeleteLoadBalancer Text
dlbLoadBalancerARN = lens _dlbLoadBalancerARN (\ s a -> s{_dlbLoadBalancerARN = a})

instance AWSRequest DeleteLoadBalancer where
        type Rs DeleteLoadBalancer =
             DeleteLoadBalancerResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DeleteLoadBalancerResult"
              (\ s h x ->
                 DeleteLoadBalancerResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteLoadBalancer where

instance NFData DeleteLoadBalancer where

instance ToHeaders DeleteLoadBalancer where
        toHeaders = const mempty

instance ToPath DeleteLoadBalancer where
        toPath = const "/"

instance ToQuery DeleteLoadBalancer where
        toQuery DeleteLoadBalancer'{..}
          = mconcat
              ["Action" =: ("DeleteLoadBalancer" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "LoadBalancerArn" =: _dlbLoadBalancerARN]

-- | /See:/ 'deleteLoadBalancerResponse' smart constructor.
newtype DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteLoadBalancerResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteLoadBalancerResponse
deleteLoadBalancerResponse pResponseStatus_ =
  DeleteLoadBalancerResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteLoadBalancerResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteLoadBalancerResponse where
