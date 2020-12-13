{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified instances from the specified load balancer. After the instance is deregistered, it no longer receives traffic from the load balancer.
--
-- You can use 'DescribeLoadBalancers' to verify that the instance is deregistered from the load balancer.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html Register or De-Register EC2 Instances> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
  ( -- * Creating a request
    DeregisterInstancesFromLoadBalancer (..),
    mkDeregisterInstancesFromLoadBalancer,

    -- ** Request lenses
    diflbLoadBalancerName,
    diflbInstances,

    -- * Destructuring the response
    DeregisterInstancesFromLoadBalancerResponse (..),
    mkDeregisterInstancesFromLoadBalancerResponse,

    -- ** Response lenses
    diflbrsInstances,
    diflbrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeregisterInstancesFromLoadBalancer.
--
-- /See:/ 'mkDeregisterInstancesFromLoadBalancer' smart constructor.
data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The IDs of the instances.
    instances :: [Instance]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterInstancesFromLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'instances' - The IDs of the instances.
mkDeregisterInstancesFromLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DeregisterInstancesFromLoadBalancer
mkDeregisterInstancesFromLoadBalancer pLoadBalancerName_ =
  DeregisterInstancesFromLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      instances = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbLoadBalancerName :: Lens.Lens' DeregisterInstancesFromLoadBalancer Lude.Text
diflbLoadBalancerName = Lens.lens (loadBalancerName :: DeregisterInstancesFromLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DeregisterInstancesFromLoadBalancer)
{-# DEPRECATED diflbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbInstances :: Lens.Lens' DeregisterInstancesFromLoadBalancer [Instance]
diflbInstances = Lens.lens (instances :: DeregisterInstancesFromLoadBalancer -> [Instance]) (\s a -> s {instances = a} :: DeregisterInstancesFromLoadBalancer)
{-# DEPRECATED diflbInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Lude.AWSRequest DeregisterInstancesFromLoadBalancer where
  type
    Rs DeregisterInstancesFromLoadBalancer =
      DeregisterInstancesFromLoadBalancerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DeregisterInstancesFromLoadBalancerResult"
      ( \s h x ->
          DeregisterInstancesFromLoadBalancerResponse'
            Lude.<$> ( x Lude..@? "Instances" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterInstancesFromLoadBalancer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeregisterInstancesFromLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterInstancesFromLoadBalancer where
  toQuery DeregisterInstancesFromLoadBalancer' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeregisterInstancesFromLoadBalancer" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "Instances" Lude.=: Lude.toQueryList "member" instances
      ]

-- | Contains the output of DeregisterInstancesFromLoadBalancer.
--
-- /See:/ 'mkDeregisterInstancesFromLoadBalancerResponse' smart constructor.
data DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse'
  { -- | The remaining instances registered with the load balancer.
    instances :: Lude.Maybe [Instance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterInstancesFromLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'instances' - The remaining instances registered with the load balancer.
-- * 'responseStatus' - The response status code.
mkDeregisterInstancesFromLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterInstancesFromLoadBalancerResponse
mkDeregisterInstancesFromLoadBalancerResponse pResponseStatus_ =
  DeregisterInstancesFromLoadBalancerResponse'
    { instances =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The remaining instances registered with the load balancer.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbrsInstances :: Lens.Lens' DeregisterInstancesFromLoadBalancerResponse (Lude.Maybe [Instance])
diflbrsInstances = Lens.lens (instances :: DeregisterInstancesFromLoadBalancerResponse -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: DeregisterInstancesFromLoadBalancerResponse)
{-# DEPRECATED diflbrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbrsResponseStatus :: Lens.Lens' DeregisterInstancesFromLoadBalancerResponse Lude.Int
diflbrsResponseStatus = Lens.lens (responseStatus :: DeregisterInstancesFromLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterInstancesFromLoadBalancerResponse)
{-# DEPRECATED diflbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
