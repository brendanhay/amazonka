{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateLoadBalancerListeners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more listeners for the specified load balancer. If a listener with the specified port does not already exist, it is created; otherwise, the properties of the new listener must match the properties of the existing listener.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.CreateLoadBalancerListeners
  ( -- * Creating a request
    CreateLoadBalancerListeners (..),
    mkCreateLoadBalancerListeners,

    -- ** Request lenses
    clblLoadBalancerName,
    clblListeners,

    -- * Destructuring the response
    CreateLoadBalancerListenersResponse (..),
    mkCreateLoadBalancerListenersResponse,

    -- ** Response lenses
    clblrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateLoadBalancerListeners.
--
-- /See:/ 'mkCreateLoadBalancerListeners' smart constructor.
data CreateLoadBalancerListeners = CreateLoadBalancerListeners'
  { loadBalancerName ::
      Lude.Text,
    listeners :: [Listener]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancerListeners' with the minimum fields required to make a request.
--
-- * 'listeners' - The listeners.
-- * 'loadBalancerName' - The name of the load balancer.
mkCreateLoadBalancerListeners ::
  -- | 'loadBalancerName'
  Lude.Text ->
  CreateLoadBalancerListeners
mkCreateLoadBalancerListeners pLoadBalancerName_ =
  CreateLoadBalancerListeners'
    { loadBalancerName =
        pLoadBalancerName_,
      listeners = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clblLoadBalancerName :: Lens.Lens' CreateLoadBalancerListeners Lude.Text
clblLoadBalancerName = Lens.lens (loadBalancerName :: CreateLoadBalancerListeners -> Lude.Text) (\s a -> s {loadBalancerName = a} :: CreateLoadBalancerListeners)
{-# DEPRECATED clblLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The listeners.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clblListeners :: Lens.Lens' CreateLoadBalancerListeners [Listener]
clblListeners = Lens.lens (listeners :: CreateLoadBalancerListeners -> [Listener]) (\s a -> s {listeners = a} :: CreateLoadBalancerListeners)
{-# DEPRECATED clblListeners "Use generic-lens or generic-optics with 'listeners' instead." #-}

instance Lude.AWSRequest CreateLoadBalancerListeners where
  type
    Rs CreateLoadBalancerListeners =
      CreateLoadBalancerListenersResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "CreateLoadBalancerListenersResult"
      ( \s h x ->
          CreateLoadBalancerListenersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLoadBalancerListeners where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLoadBalancerListeners where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLoadBalancerListeners where
  toQuery CreateLoadBalancerListeners' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateLoadBalancerListeners" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "Listeners" Lude.=: Lude.toQueryList "member" listeners
      ]

-- | Contains the parameters for CreateLoadBalancerListener.
--
-- /See:/ 'mkCreateLoadBalancerListenersResponse' smart constructor.
newtype CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancerListenersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateLoadBalancerListenersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLoadBalancerListenersResponse
mkCreateLoadBalancerListenersResponse pResponseStatus_ =
  CreateLoadBalancerListenersResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clblrsResponseStatus :: Lens.Lens' CreateLoadBalancerListenersResponse Lude.Int
clblrsResponseStatus = Lens.lens (responseStatus :: CreateLoadBalancerListenersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoadBalancerListenersResponse)
{-# DEPRECATED clblrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
