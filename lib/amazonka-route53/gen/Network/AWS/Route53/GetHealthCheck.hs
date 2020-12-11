{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified health check.
module Network.AWS.Route53.GetHealthCheck
  ( -- * Creating a request
    GetHealthCheck (..),
    mkGetHealthCheck,

    -- ** Request lenses
    ghcHealthCheckId,

    -- * Destructuring the response
    GetHealthCheckResponse (..),
    mkGetHealthCheckResponse,

    -- ** Response lenses
    ghcrsResponseStatus,
    ghcrsHealthCheck,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to get information about a specified health check.
--
-- /See:/ 'mkGetHealthCheck' smart constructor.
newtype GetHealthCheck = GetHealthCheck'
  { healthCheckId ::
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

-- | Creates a value of 'GetHealthCheck' with the minimum fields required to make a request.
--
-- * 'healthCheckId' - The identifier that Amazon Route 53 assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
mkGetHealthCheck ::
  -- | 'healthCheckId'
  Lude.Text ->
  GetHealthCheck
mkGetHealthCheck pHealthCheckId_ =
  GetHealthCheck' {healthCheckId = pHealthCheckId_}

-- | The identifier that Amazon Route 53 assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcHealthCheckId :: Lens.Lens' GetHealthCheck Lude.Text
ghcHealthCheckId = Lens.lens (healthCheckId :: GetHealthCheck -> Lude.Text) (\s a -> s {healthCheckId = a} :: GetHealthCheck)
{-# DEPRECATED ghcHealthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead." #-}

instance Lude.AWSRequest GetHealthCheck where
  type Rs GetHealthCheck = GetHealthCheckResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetHealthCheckResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "HealthCheck")
      )

instance Lude.ToHeaders GetHealthCheck where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetHealthCheck where
  toPath GetHealthCheck' {..} =
    Lude.mconcat
      ["/2013-04-01/healthcheck/", Lude.toBS healthCheckId]

instance Lude.ToQuery GetHealthCheck where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
-- /See:/ 'mkGetHealthCheckResponse' smart constructor.
data GetHealthCheckResponse = GetHealthCheckResponse'
  { responseStatus ::
      Lude.Int,
    healthCheck :: HealthCheck
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHealthCheckResponse' with the minimum fields required to make a request.
--
-- * 'healthCheck' - A complex type that contains information about one health check that is associated with the current AWS account.
-- * 'responseStatus' - The response status code.
mkGetHealthCheckResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'healthCheck'
  HealthCheck ->
  GetHealthCheckResponse
mkGetHealthCheckResponse pResponseStatus_ pHealthCheck_ =
  GetHealthCheckResponse'
    { responseStatus = pResponseStatus_,
      healthCheck = pHealthCheck_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcrsResponseStatus :: Lens.Lens' GetHealthCheckResponse Lude.Int
ghcrsResponseStatus = Lens.lens (responseStatus :: GetHealthCheckResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHealthCheckResponse)
{-# DEPRECATED ghcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains information about one health check that is associated with the current AWS account.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcrsHealthCheck :: Lens.Lens' GetHealthCheckResponse HealthCheck
ghcrsHealthCheck = Lens.lens (healthCheck :: GetHealthCheckResponse -> HealthCheck) (\s a -> s {healthCheck = a} :: GetHealthCheckResponse)
{-# DEPRECATED ghcrsHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}
