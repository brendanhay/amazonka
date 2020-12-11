{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Lightsail load balancer.
module Network.AWS.Lightsail.GetLoadBalancer
  ( -- * Creating a request
    GetLoadBalancer (..),
    mkGetLoadBalancer,

    -- ** Request lenses
    glbLoadBalancerName,

    -- * Destructuring the response
    GetLoadBalancerResponse (..),
    mkGetLoadBalancerResponse,

    -- ** Response lenses
    glbrsLoadBalancer,
    glbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLoadBalancer' smart constructor.
newtype GetLoadBalancer = GetLoadBalancer'
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

-- | Creates a value of 'GetLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
mkGetLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  GetLoadBalancer
mkGetLoadBalancer pLoadBalancerName_ =
  GetLoadBalancer' {loadBalancerName = pLoadBalancerName_}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbLoadBalancerName :: Lens.Lens' GetLoadBalancer Lude.Text
glbLoadBalancerName = Lens.lens (loadBalancerName :: GetLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: GetLoadBalancer)
{-# DEPRECATED glbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.AWSRequest GetLoadBalancer where
  type Rs GetLoadBalancer = GetLoadBalancerResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLoadBalancerResponse'
            Lude.<$> (x Lude..?> "loadBalancer") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLoadBalancer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetLoadBalancer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLoadBalancer where
  toJSON GetLoadBalancer' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("loadBalancerName" Lude..= loadBalancerName)]
      )

instance Lude.ToPath GetLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLoadBalancer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLoadBalancerResponse' smart constructor.
data GetLoadBalancerResponse = GetLoadBalancerResponse'
  { loadBalancer ::
      Lude.Maybe LoadBalancer,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancer' - An object containing information about your load balancer.
-- * 'responseStatus' - The response status code.
mkGetLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLoadBalancerResponse
mkGetLoadBalancerResponse pResponseStatus_ =
  GetLoadBalancerResponse'
    { loadBalancer = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object containing information about your load balancer.
--
-- /Note:/ Consider using 'loadBalancer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbrsLoadBalancer :: Lens.Lens' GetLoadBalancerResponse (Lude.Maybe LoadBalancer)
glbrsLoadBalancer = Lens.lens (loadBalancer :: GetLoadBalancerResponse -> Lude.Maybe LoadBalancer) (\s a -> s {loadBalancer = a} :: GetLoadBalancerResponse)
{-# DEPRECATED glbrsLoadBalancer "Use generic-lens or generic-optics with 'loadBalancer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbrsResponseStatus :: Lens.Lens' GetLoadBalancerResponse Lude.Int
glbrsResponseStatus = Lens.lens (responseStatus :: GetLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLoadBalancerResponse)
{-# DEPRECATED glbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
