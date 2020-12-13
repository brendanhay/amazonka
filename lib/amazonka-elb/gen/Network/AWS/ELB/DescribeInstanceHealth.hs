{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of the specified instances with respect to the specified load balancer. If no instances are specified, the call describes the state of all instances that are currently registered with the load balancer. If instances are specified, their state is returned even if they are no longer registered with the load balancer. The state of terminated instances is not returned.
module Network.AWS.ELB.DescribeInstanceHealth
  ( -- * Creating a request
    DescribeInstanceHealth (..),
    mkDescribeInstanceHealth,

    -- ** Request lenses
    dihLoadBalancerName,
    dihInstances,

    -- * Destructuring the response
    DescribeInstanceHealthResponse (..),
    mkDescribeInstanceHealthResponse,

    -- ** Response lenses
    dihrsInstanceStates,
    dihrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeInstanceHealth.
--
-- /See:/ 'mkDescribeInstanceHealth' smart constructor.
data DescribeInstanceHealth = DescribeInstanceHealth'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The IDs of the instances.
    instances :: Lude.Maybe [Instance]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceHealth' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'instances' - The IDs of the instances.
mkDescribeInstanceHealth ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DescribeInstanceHealth
mkDescribeInstanceHealth pLoadBalancerName_ =
  DescribeInstanceHealth'
    { loadBalancerName = pLoadBalancerName_,
      instances = Lude.Nothing
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihLoadBalancerName :: Lens.Lens' DescribeInstanceHealth Lude.Text
dihLoadBalancerName = Lens.lens (loadBalancerName :: DescribeInstanceHealth -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DescribeInstanceHealth)
{-# DEPRECATED dihLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihInstances :: Lens.Lens' DescribeInstanceHealth (Lude.Maybe [Instance])
dihInstances = Lens.lens (instances :: DescribeInstanceHealth -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: DescribeInstanceHealth)
{-# DEPRECATED dihInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Lude.AWSRequest DescribeInstanceHealth where
  type Rs DescribeInstanceHealth = DescribeInstanceHealthResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DescribeInstanceHealthResult"
      ( \s h x ->
          DescribeInstanceHealthResponse'
            Lude.<$> ( x Lude..@? "InstanceStates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceHealth where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstanceHealth where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceHealth where
  toQuery DescribeInstanceHealth' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInstanceHealth" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "Instances"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> instances)
      ]

-- | Contains the output for DescribeInstanceHealth.
--
-- /See:/ 'mkDescribeInstanceHealthResponse' smart constructor.
data DescribeInstanceHealthResponse = DescribeInstanceHealthResponse'
  { -- | Information about the health of the instances.
    instanceStates :: Lude.Maybe [InstanceState],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceHealthResponse' with the minimum fields required to make a request.
--
-- * 'instanceStates' - Information about the health of the instances.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceHealthResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceHealthResponse
mkDescribeInstanceHealthResponse pResponseStatus_ =
  DescribeInstanceHealthResponse'
    { instanceStates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the health of the instances.
--
-- /Note:/ Consider using 'instanceStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrsInstanceStates :: Lens.Lens' DescribeInstanceHealthResponse (Lude.Maybe [InstanceState])
dihrsInstanceStates = Lens.lens (instanceStates :: DescribeInstanceHealthResponse -> Lude.Maybe [InstanceState]) (\s a -> s {instanceStates = a} :: DescribeInstanceHealthResponse)
{-# DEPRECATED dihrsInstanceStates "Use generic-lens or generic-optics with 'instanceStates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrsResponseStatus :: Lens.Lens' DescribeInstanceHealthResponse Lude.Int
dihrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceHealthResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceHealthResponse)
{-# DEPRECATED dihrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
