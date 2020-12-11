{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified load balancer.
--
-- If you are attempting to recreate a load balancer, you must reconfigure all settings. The DNS name associated with a deleted load balancer are no longer usable. The name and associated DNS record of the deleted load balancer no longer exist and traffic sent to any of its IP addresses is no longer delivered to your instances.
-- If the load balancer does not exist or has already been deleted, the call to @DeleteLoadBalancer@ still succeeds.
module Network.AWS.ELB.DeleteLoadBalancer
  ( -- * Creating a request
    DeleteLoadBalancer (..),
    mkDeleteLoadBalancer,

    -- ** Request lenses
    dlbLoadBalancerName,

    -- * Destructuring the response
    DeleteLoadBalancerResponse (..),
    mkDeleteLoadBalancerResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteLoadBalancer.
--
-- /See:/ 'mkDeleteLoadBalancer' smart constructor.
newtype DeleteLoadBalancer = DeleteLoadBalancer'
  { loadBalancerName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
mkDeleteLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DeleteLoadBalancer
mkDeleteLoadBalancer pLoadBalancerName_ =
  DeleteLoadBalancer' {loadBalancerName = pLoadBalancerName_}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerName :: Lens.Lens' DeleteLoadBalancer Lude.Text
dlbLoadBalancerName = Lens.lens (loadBalancerName :: DeleteLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DeleteLoadBalancer)
{-# DEPRECATED dlbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.AWSRequest DeleteLoadBalancer where
  type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DeleteLoadBalancerResult"
      ( \s h x ->
          DeleteLoadBalancerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLoadBalancer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLoadBalancer where
  toQuery DeleteLoadBalancer' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteLoadBalancer" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName
      ]

-- | Contains the output of DeleteLoadBalancer.
--
-- /See:/ 'mkDeleteLoadBalancerResponse' smart constructor.
newtype DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
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

-- | Creates a value of 'DeleteLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLoadBalancerResponse
mkDeleteLoadBalancerResponse pResponseStatus_ =
  DeleteLoadBalancerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteLoadBalancerResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLoadBalancerResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
