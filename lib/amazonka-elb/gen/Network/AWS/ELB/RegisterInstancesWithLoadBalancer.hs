{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.RegisterInstancesWithLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified instances to the specified load balancer.
--
-- The instance must be a running instance in the same network as the load balancer (EC2-Classic or the same VPC). If you have EC2-Classic instances and a load balancer in a VPC with ClassicLink enabled, you can link the EC2-Classic instances to that VPC and then register the linked EC2-Classic instances with the load balancer in the VPC.
-- Note that @RegisterInstanceWithLoadBalancer@ completes when the request has been registered. Instance registration takes a little time to complete. To check the state of the registered instances, use 'DescribeLoadBalancers' or 'DescribeInstanceHealth' .
-- After the instance is registered, it starts receiving traffic and requests from the load balancer. Any instance that is not in one of the Availability Zones registered for the load balancer is moved to the @OutOfService@ state. If an Availability Zone is added to the load balancer later, any instances registered with the load balancer move to the @InService@ state.
-- To deregister instances from a load balancer, use 'DeregisterInstancesFromLoadBalancer' .
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html Register or De-Register EC2 Instances> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.RegisterInstancesWithLoadBalancer
  ( -- * Creating a request
    RegisterInstancesWithLoadBalancer (..),
    mkRegisterInstancesWithLoadBalancer,

    -- ** Request lenses
    riwlbLoadBalancerName,
    riwlbInstances,

    -- * Destructuring the response
    RegisterInstancesWithLoadBalancerResponse (..),
    mkRegisterInstancesWithLoadBalancerResponse,

    -- ** Response lenses
    riwlbrsInstances,
    riwlbrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for RegisterInstancesWithLoadBalancer.
--
-- /See:/ 'mkRegisterInstancesWithLoadBalancer' smart constructor.
data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer'
  { loadBalancerName ::
      Lude.Text,
    instances :: [Instance]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterInstancesWithLoadBalancer' with the minimum fields required to make a request.
--
-- * 'instances' - The IDs of the instances.
-- * 'loadBalancerName' - The name of the load balancer.
mkRegisterInstancesWithLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  RegisterInstancesWithLoadBalancer
mkRegisterInstancesWithLoadBalancer pLoadBalancerName_ =
  RegisterInstancesWithLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      instances = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riwlbLoadBalancerName :: Lens.Lens' RegisterInstancesWithLoadBalancer Lude.Text
riwlbLoadBalancerName = Lens.lens (loadBalancerName :: RegisterInstancesWithLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: RegisterInstancesWithLoadBalancer)
{-# DEPRECATED riwlbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riwlbInstances :: Lens.Lens' RegisterInstancesWithLoadBalancer [Instance]
riwlbInstances = Lens.lens (instances :: RegisterInstancesWithLoadBalancer -> [Instance]) (\s a -> s {instances = a} :: RegisterInstancesWithLoadBalancer)
{-# DEPRECATED riwlbInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Lude.AWSRequest RegisterInstancesWithLoadBalancer where
  type
    Rs RegisterInstancesWithLoadBalancer =
      RegisterInstancesWithLoadBalancerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "RegisterInstancesWithLoadBalancerResult"
      ( \s h x ->
          RegisterInstancesWithLoadBalancerResponse'
            Lude.<$> ( x Lude..@? "Instances" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterInstancesWithLoadBalancer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RegisterInstancesWithLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterInstancesWithLoadBalancer where
  toQuery RegisterInstancesWithLoadBalancer' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RegisterInstancesWithLoadBalancer" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "Instances" Lude.=: Lude.toQueryList "member" instances
      ]

-- | Contains the output of RegisterInstancesWithLoadBalancer.
--
-- /See:/ 'mkRegisterInstancesWithLoadBalancerResponse' smart constructor.
data RegisterInstancesWithLoadBalancerResponse = RegisterInstancesWithLoadBalancerResponse'
  { instances ::
      Lude.Maybe
        [Instance],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterInstancesWithLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'instances' - The updated list of instances for the load balancer.
-- * 'responseStatus' - The response status code.
mkRegisterInstancesWithLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterInstancesWithLoadBalancerResponse
mkRegisterInstancesWithLoadBalancerResponse pResponseStatus_ =
  RegisterInstancesWithLoadBalancerResponse'
    { instances =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated list of instances for the load balancer.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riwlbrsInstances :: Lens.Lens' RegisterInstancesWithLoadBalancerResponse (Lude.Maybe [Instance])
riwlbrsInstances = Lens.lens (instances :: RegisterInstancesWithLoadBalancerResponse -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: RegisterInstancesWithLoadBalancerResponse)
{-# DEPRECATED riwlbrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riwlbrsResponseStatus :: Lens.Lens' RegisterInstancesWithLoadBalancerResponse Lude.Int
riwlbrsResponseStatus = Lens.lens (responseStatus :: RegisterInstancesWithLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterInstancesWithLoadBalancerResponse)
{-# DEPRECATED riwlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
