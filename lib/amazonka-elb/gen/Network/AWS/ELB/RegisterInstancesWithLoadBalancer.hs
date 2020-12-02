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
-- Module      : Network.AWS.ELB.RegisterInstancesWithLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified instances to the specified load balancer.
--
--
-- The instance must be a running instance in the same network as the load balancer (EC2-Classic or the same VPC). If you have EC2-Classic instances and a load balancer in a VPC with ClassicLink enabled, you can link the EC2-Classic instances to that VPC and then register the linked EC2-Classic instances with the load balancer in the VPC.
--
-- Note that @RegisterInstanceWithLoadBalancer@ completes when the request has been registered. Instance registration takes a little time to complete. To check the state of the registered instances, use 'DescribeLoadBalancers' or 'DescribeInstanceHealth' .
--
-- After the instance is registered, it starts receiving traffic and requests from the load balancer. Any instance that is not in one of the Availability Zones registered for the load balancer is moved to the @OutOfService@ state. If an Availability Zone is added to the load balancer later, any instances registered with the load balancer move to the @InService@ state.
--
-- To deregister instances from a load balancer, use 'DeregisterInstancesFromLoadBalancer' .
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html Register or De-Register EC2 Instances> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.RegisterInstancesWithLoadBalancer
    (
    -- * Creating a Request
      registerInstancesWithLoadBalancer
    , RegisterInstancesWithLoadBalancer
    -- * Request Lenses
    , riwlbLoadBalancerName
    , riwlbInstances

    -- * Destructuring the Response
    , registerInstancesWithLoadBalancerResponse
    , RegisterInstancesWithLoadBalancerResponse
    -- * Response Lenses
    , riwlbrsInstances
    , riwlbrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for RegisterInstancesWithLoadBalancer.
--
--
--
-- /See:/ 'registerInstancesWithLoadBalancer' smart constructor.
data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer'
  { _riwlbLoadBalancerName :: !Text
  , _riwlbInstances        :: ![Instance]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterInstancesWithLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riwlbLoadBalancerName' - The name of the load balancer.
--
-- * 'riwlbInstances' - The IDs of the instances.
registerInstancesWithLoadBalancer
    :: Text -- ^ 'riwlbLoadBalancerName'
    -> RegisterInstancesWithLoadBalancer
registerInstancesWithLoadBalancer pLoadBalancerName_ =
  RegisterInstancesWithLoadBalancer'
    {_riwlbLoadBalancerName = pLoadBalancerName_, _riwlbInstances = mempty}


-- | The name of the load balancer.
riwlbLoadBalancerName :: Lens' RegisterInstancesWithLoadBalancer Text
riwlbLoadBalancerName = lens _riwlbLoadBalancerName (\ s a -> s{_riwlbLoadBalancerName = a})

-- | The IDs of the instances.
riwlbInstances :: Lens' RegisterInstancesWithLoadBalancer [Instance]
riwlbInstances = lens _riwlbInstances (\ s a -> s{_riwlbInstances = a}) . _Coerce

instance AWSRequest RegisterInstancesWithLoadBalancer
         where
        type Rs RegisterInstancesWithLoadBalancer =
             RegisterInstancesWithLoadBalancerResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "RegisterInstancesWithLoadBalancerResult"
              (\ s h x ->
                 RegisterInstancesWithLoadBalancerResponse' <$>
                   (x .@? "Instances" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable RegisterInstancesWithLoadBalancer
         where

instance NFData RegisterInstancesWithLoadBalancer
         where

instance ToHeaders RegisterInstancesWithLoadBalancer
         where
        toHeaders = const mempty

instance ToPath RegisterInstancesWithLoadBalancer
         where
        toPath = const "/"

instance ToQuery RegisterInstancesWithLoadBalancer
         where
        toQuery RegisterInstancesWithLoadBalancer'{..}
          = mconcat
              ["Action" =:
                 ("RegisterInstancesWithLoadBalancer" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _riwlbLoadBalancerName,
               "Instances" =: toQueryList "member" _riwlbInstances]

-- | Contains the output of RegisterInstancesWithLoadBalancer.
--
--
--
-- /See:/ 'registerInstancesWithLoadBalancerResponse' smart constructor.
data RegisterInstancesWithLoadBalancerResponse = RegisterInstancesWithLoadBalancerResponse'
  { _riwlbrsInstances      :: !(Maybe [Instance])
  , _riwlbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterInstancesWithLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riwlbrsInstances' - The updated list of instances for the load balancer.
--
-- * 'riwlbrsResponseStatus' - -- | The response status code.
registerInstancesWithLoadBalancerResponse
    :: Int -- ^ 'riwlbrsResponseStatus'
    -> RegisterInstancesWithLoadBalancerResponse
registerInstancesWithLoadBalancerResponse pResponseStatus_ =
  RegisterInstancesWithLoadBalancerResponse'
    {_riwlbrsInstances = Nothing, _riwlbrsResponseStatus = pResponseStatus_}


-- | The updated list of instances for the load balancer.
riwlbrsInstances :: Lens' RegisterInstancesWithLoadBalancerResponse [Instance]
riwlbrsInstances = lens _riwlbrsInstances (\ s a -> s{_riwlbrsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
riwlbrsResponseStatus :: Lens' RegisterInstancesWithLoadBalancerResponse Int
riwlbrsResponseStatus = lens _riwlbrsResponseStatus (\ s a -> s{_riwlbrsResponseStatus = a})

instance NFData
           RegisterInstancesWithLoadBalancerResponse
         where
