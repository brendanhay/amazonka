{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listeners from the specified load balancer.
module Network.AWS.ELB.DeleteLoadBalancerListeners
  ( -- * Creating a request
    DeleteLoadBalancerListeners (..),
    mkDeleteLoadBalancerListeners,

    -- ** Request lenses
    dlblLoadBalancerName,
    dlblLoadBalancerPorts,

    -- * Destructuring the response
    DeleteLoadBalancerListenersResponse (..),
    mkDeleteLoadBalancerListenersResponse,

    -- ** Response lenses
    dlblrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteLoadBalancerListeners.
--
-- /See:/ 'mkDeleteLoadBalancerListeners' smart constructor.
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners'
  { loadBalancerName ::
      Lude.Text,
    loadBalancerPorts :: [Lude.Int]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancerListeners' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'loadBalancerPorts' - The client port numbers of the listeners.
mkDeleteLoadBalancerListeners ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DeleteLoadBalancerListeners
mkDeleteLoadBalancerListeners pLoadBalancerName_ =
  DeleteLoadBalancerListeners'
    { loadBalancerName =
        pLoadBalancerName_,
      loadBalancerPorts = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlblLoadBalancerName :: Lens.Lens' DeleteLoadBalancerListeners Lude.Text
dlblLoadBalancerName = Lens.lens (loadBalancerName :: DeleteLoadBalancerListeners -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DeleteLoadBalancerListeners)
{-# DEPRECATED dlblLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The client port numbers of the listeners.
--
-- /Note:/ Consider using 'loadBalancerPorts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlblLoadBalancerPorts :: Lens.Lens' DeleteLoadBalancerListeners [Lude.Int]
dlblLoadBalancerPorts = Lens.lens (loadBalancerPorts :: DeleteLoadBalancerListeners -> [Lude.Int]) (\s a -> s {loadBalancerPorts = a} :: DeleteLoadBalancerListeners)
{-# DEPRECATED dlblLoadBalancerPorts "Use generic-lens or generic-optics with 'loadBalancerPorts' instead." #-}

instance Lude.AWSRequest DeleteLoadBalancerListeners where
  type
    Rs DeleteLoadBalancerListeners =
      DeleteLoadBalancerListenersResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DeleteLoadBalancerListenersResult"
      ( \s h x ->
          DeleteLoadBalancerListenersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLoadBalancerListeners where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLoadBalancerListeners where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLoadBalancerListeners where
  toQuery DeleteLoadBalancerListeners' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteLoadBalancerListeners" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "LoadBalancerPorts"
          Lude.=: Lude.toQueryList "member" loadBalancerPorts
      ]

-- | Contains the output of DeleteLoadBalancerListeners.
--
-- /See:/ 'mkDeleteLoadBalancerListenersResponse' smart constructor.
newtype DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse'
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

-- | Creates a value of 'DeleteLoadBalancerListenersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLoadBalancerListenersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLoadBalancerListenersResponse
mkDeleteLoadBalancerListenersResponse pResponseStatus_ =
  DeleteLoadBalancerListenersResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlblrsResponseStatus :: Lens.Lens' DeleteLoadBalancerListenersResponse Lude.Int
dlblrsResponseStatus = Lens.lens (responseStatus :: DeleteLoadBalancerListenersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLoadBalancerListenersResponse)
{-# DEPRECATED dlblrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
