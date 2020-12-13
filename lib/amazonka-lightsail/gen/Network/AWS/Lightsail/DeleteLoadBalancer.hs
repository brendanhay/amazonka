{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lightsail load balancer and all its associated SSL/TLS certificates. Once the load balancer is deleted, you will need to create a new load balancer, create a new certificate, and verify domain ownership again.
--
-- The @delete load balancer@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteLoadBalancer
  ( -- * Creating a request
    DeleteLoadBalancer (..),
    mkDeleteLoadBalancer,

    -- ** Request lenses
    dlbLoadBalancerName,

    -- * Destructuring the response
    DeleteLoadBalancerResponse (..),
    mkDeleteLoadBalancerResponse,

    -- ** Response lenses
    dlbrsOperations,
    dlbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLoadBalancer' smart constructor.
newtype DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The name of the load balancer you want to delete.
    loadBalancerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer you want to delete.
mkDeleteLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DeleteLoadBalancer
mkDeleteLoadBalancer pLoadBalancerName_ =
  DeleteLoadBalancer' {loadBalancerName = pLoadBalancerName_}

-- | The name of the load balancer you want to delete.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerName :: Lens.Lens' DeleteLoadBalancer Lude.Text
dlbLoadBalancerName = Lens.lens (loadBalancerName :: DeleteLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DeleteLoadBalancer)
{-# DEPRECATED dlbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.AWSRequest DeleteLoadBalancer where
  type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteLoadBalancerResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLoadBalancer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteLoadBalancer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLoadBalancer where
  toJSON DeleteLoadBalancer' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("loadBalancerName" Lude..= loadBalancerName)]
      )

instance Lude.ToPath DeleteLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLoadBalancer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLoadBalancerResponse' smart constructor.
data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLoadBalancerResponse
mkDeleteLoadBalancerResponse pResponseStatus_ =
  DeleteLoadBalancerResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsOperations :: Lens.Lens' DeleteLoadBalancerResponse (Lude.Maybe [Operation])
dlbrsOperations = Lens.lens (operations :: DeleteLoadBalancerResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteLoadBalancerResponse)
{-# DEPRECATED dlbrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsResponseStatus :: Lens.Lens' DeleteLoadBalancerResponse Lude.Int
dlbrsResponseStatus = Lens.lens (responseStatus :: DeleteLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLoadBalancerResponse)
{-# DEPRECATED dlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
