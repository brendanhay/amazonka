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
-- Module      : Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified instances from a Lightsail load balancer.
--
--
-- This operation waits until the instances are no longer needed before they are detached from the load balancer.
--
module Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
    (
    -- * Creating a Request
      detachInstancesFromLoadBalancer
    , DetachInstancesFromLoadBalancer
    -- * Request Lenses
    , diflbLoadBalancerName
    , diflbInstanceNames

    -- * Destructuring the Response
    , detachInstancesFromLoadBalancerResponse
    , DetachInstancesFromLoadBalancerResponse
    -- * Response Lenses
    , diflbrsOperations
    , diflbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachInstancesFromLoadBalancer' smart constructor.
data DetachInstancesFromLoadBalancer = DetachInstancesFromLoadBalancer'
  { _diflbLoadBalancerName :: !Text
  , _diflbInstanceNames    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachInstancesFromLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diflbLoadBalancerName' - The name of the Lightsail load balancer.
--
-- * 'diflbInstanceNames' - An array of strings containing the names of the instances you want to detach from the load balancer.
detachInstancesFromLoadBalancer
    :: Text -- ^ 'diflbLoadBalancerName'
    -> DetachInstancesFromLoadBalancer
detachInstancesFromLoadBalancer pLoadBalancerName_ =
  DetachInstancesFromLoadBalancer'
    {_diflbLoadBalancerName = pLoadBalancerName_, _diflbInstanceNames = mempty}


-- | The name of the Lightsail load balancer.
diflbLoadBalancerName :: Lens' DetachInstancesFromLoadBalancer Text
diflbLoadBalancerName = lens _diflbLoadBalancerName (\ s a -> s{_diflbLoadBalancerName = a})

-- | An array of strings containing the names of the instances you want to detach from the load balancer.
diflbInstanceNames :: Lens' DetachInstancesFromLoadBalancer [Text]
diflbInstanceNames = lens _diflbInstanceNames (\ s a -> s{_diflbInstanceNames = a}) . _Coerce

instance AWSRequest DetachInstancesFromLoadBalancer
         where
        type Rs DetachInstancesFromLoadBalancer =
             DetachInstancesFromLoadBalancerResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DetachInstancesFromLoadBalancerResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DetachInstancesFromLoadBalancer
         where

instance NFData DetachInstancesFromLoadBalancer where

instance ToHeaders DetachInstancesFromLoadBalancer
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DetachInstancesFromLoadBalancer"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetachInstancesFromLoadBalancer where
        toJSON DetachInstancesFromLoadBalancer'{..}
          = object
              (catMaybes
                 [Just ("loadBalancerName" .= _diflbLoadBalancerName),
                  Just ("instanceNames" .= _diflbInstanceNames)])

instance ToPath DetachInstancesFromLoadBalancer where
        toPath = const "/"

instance ToQuery DetachInstancesFromLoadBalancer
         where
        toQuery = const mempty

-- | /See:/ 'detachInstancesFromLoadBalancerResponse' smart constructor.
data DetachInstancesFromLoadBalancerResponse = DetachInstancesFromLoadBalancerResponse'
  { _diflbrsOperations     :: !(Maybe [Operation])
  , _diflbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachInstancesFromLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diflbrsOperations' - An object describing the API operations.
--
-- * 'diflbrsResponseStatus' - -- | The response status code.
detachInstancesFromLoadBalancerResponse
    :: Int -- ^ 'diflbrsResponseStatus'
    -> DetachInstancesFromLoadBalancerResponse
detachInstancesFromLoadBalancerResponse pResponseStatus_ =
  DetachInstancesFromLoadBalancerResponse'
    {_diflbrsOperations = Nothing, _diflbrsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
diflbrsOperations :: Lens' DetachInstancesFromLoadBalancerResponse [Operation]
diflbrsOperations = lens _diflbrsOperations (\ s a -> s{_diflbrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
diflbrsResponseStatus :: Lens' DetachInstancesFromLoadBalancerResponse Int
diflbrsResponseStatus = lens _diflbrsResponseStatus (\ s a -> s{_diflbrsResponseStatus = a})

instance NFData
           DetachInstancesFromLoadBalancerResponse
         where
