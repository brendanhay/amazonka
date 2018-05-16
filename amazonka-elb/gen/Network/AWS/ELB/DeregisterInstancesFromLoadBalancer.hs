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
-- Module      : Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified instances from the specified load balancer. After the instance is deregistered, it no longer receives traffic from the load balancer.
--
--
-- You can use 'DescribeLoadBalancers' to verify that the instance is deregistered from the load balancer.
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html Register or De-Register EC2 Instances> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
    (
    -- * Creating a Request
      deregisterInstancesFromLoadBalancer
    , DeregisterInstancesFromLoadBalancer
    -- * Request Lenses
    , diflbLoadBalancerName
    , diflbInstances

    -- * Destructuring the Response
    , deregisterInstancesFromLoadBalancerResponse
    , DeregisterInstancesFromLoadBalancerResponse
    -- * Response Lenses
    , diflbrsInstances
    , diflbrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeregisterInstancesFromLoadBalancer.
--
--
--
-- /See:/ 'deregisterInstancesFromLoadBalancer' smart constructor.
data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer'
  { _diflbLoadBalancerName :: !Text
  , _diflbInstances        :: ![Instance]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterInstancesFromLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diflbLoadBalancerName' - The name of the load balancer.
--
-- * 'diflbInstances' - The IDs of the instances.
deregisterInstancesFromLoadBalancer
    :: Text -- ^ 'diflbLoadBalancerName'
    -> DeregisterInstancesFromLoadBalancer
deregisterInstancesFromLoadBalancer pLoadBalancerName_ =
  DeregisterInstancesFromLoadBalancer'
    {_diflbLoadBalancerName = pLoadBalancerName_, _diflbInstances = mempty}


-- | The name of the load balancer.
diflbLoadBalancerName :: Lens' DeregisterInstancesFromLoadBalancer Text
diflbLoadBalancerName = lens _diflbLoadBalancerName (\ s a -> s{_diflbLoadBalancerName = a})

-- | The IDs of the instances.
diflbInstances :: Lens' DeregisterInstancesFromLoadBalancer [Instance]
diflbInstances = lens _diflbInstances (\ s a -> s{_diflbInstances = a}) . _Coerce

instance AWSRequest
           DeregisterInstancesFromLoadBalancer
         where
        type Rs DeregisterInstancesFromLoadBalancer =
             DeregisterInstancesFromLoadBalancerResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "DeregisterInstancesFromLoadBalancerResult"
              (\ s h x ->
                 DeregisterInstancesFromLoadBalancerResponse' <$>
                   (x .@? "Instances" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DeregisterInstancesFromLoadBalancer
         where

instance NFData DeregisterInstancesFromLoadBalancer
         where

instance ToHeaders
           DeregisterInstancesFromLoadBalancer
         where
        toHeaders = const mempty

instance ToPath DeregisterInstancesFromLoadBalancer
         where
        toPath = const "/"

instance ToQuery DeregisterInstancesFromLoadBalancer
         where
        toQuery DeregisterInstancesFromLoadBalancer'{..}
          = mconcat
              ["Action" =:
                 ("DeregisterInstancesFromLoadBalancer" ::
                    ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _diflbLoadBalancerName,
               "Instances" =: toQueryList "member" _diflbInstances]

-- | Contains the output of DeregisterInstancesFromLoadBalancer.
--
--
--
-- /See:/ 'deregisterInstancesFromLoadBalancerResponse' smart constructor.
data DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse'
  { _diflbrsInstances      :: !(Maybe [Instance])
  , _diflbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterInstancesFromLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diflbrsInstances' - The remaining instances registered with the load balancer.
--
-- * 'diflbrsResponseStatus' - -- | The response status code.
deregisterInstancesFromLoadBalancerResponse
    :: Int -- ^ 'diflbrsResponseStatus'
    -> DeregisterInstancesFromLoadBalancerResponse
deregisterInstancesFromLoadBalancerResponse pResponseStatus_ =
  DeregisterInstancesFromLoadBalancerResponse'
    {_diflbrsInstances = Nothing, _diflbrsResponseStatus = pResponseStatus_}


-- | The remaining instances registered with the load balancer.
diflbrsInstances :: Lens' DeregisterInstancesFromLoadBalancerResponse [Instance]
diflbrsInstances = lens _diflbrsInstances (\ s a -> s{_diflbrsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
diflbrsResponseStatus :: Lens' DeregisterInstancesFromLoadBalancerResponse Int
diflbrsResponseStatus = lens _diflbrsResponseStatus (\ s a -> s{_diflbrsResponseStatus = a})

instance NFData
           DeregisterInstancesFromLoadBalancerResponse
         where
