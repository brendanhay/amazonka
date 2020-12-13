{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeleteLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Application Load Balancer, Network Load Balancer, or Gateway Load Balancer. Deleting a load balancer also deletes its listeners.
--
-- You can't delete a load balancer if deletion protection is enabled. If the load balancer does not exist or has already been deleted, the call succeeds.
-- Deleting a load balancer does not affect its registered targets. For example, your EC2 instances continue to run and are still registered to their target groups. If you no longer need these EC2 instances, you can stop or terminate them.
module Network.AWS.ELBv2.DeleteLoadBalancer
  ( -- * Creating a request
    DeleteLoadBalancer (..),
    mkDeleteLoadBalancer,

    -- ** Request lenses
    dlbLoadBalancerARN,

    -- * Destructuring the response
    DeleteLoadBalancerResponse (..),
    mkDeleteLoadBalancerResponse,

    -- ** Response lenses
    dlbrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLoadBalancer' smart constructor.
newtype DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
mkDeleteLoadBalancer ::
  -- | 'loadBalancerARN'
  Lude.Text ->
  DeleteLoadBalancer
mkDeleteLoadBalancer pLoadBalancerARN_ =
  DeleteLoadBalancer' {loadBalancerARN = pLoadBalancerARN_}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerARN :: Lens.Lens' DeleteLoadBalancer Lude.Text
dlbLoadBalancerARN = Lens.lens (loadBalancerARN :: DeleteLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerARN = a} :: DeleteLoadBalancer)
{-# DEPRECATED dlbLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

instance Lude.AWSRequest DeleteLoadBalancer where
  type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse
  request = Req.postQuery eLBv2Service
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
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "LoadBalancerArn" Lude.=: loadBalancerARN
      ]

-- | /See:/ 'mkDeleteLoadBalancerResponse' smart constructor.
newtype DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
dlbrsResponseStatus :: Lens.Lens' DeleteLoadBalancerResponse Lude.Int
dlbrsResponseStatus = Lens.lens (responseStatus :: DeleteLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLoadBalancerResponse)
{-# DEPRECATED dlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
