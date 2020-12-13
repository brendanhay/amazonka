{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status of a specified health check.
module Network.AWS.Route53.GetHealthCheckStatus
  ( -- * Creating a request
    GetHealthCheckStatus (..),
    mkGetHealthCheckStatus,

    -- ** Request lenses
    ghcsHealthCheckId,

    -- * Destructuring the response
    GetHealthCheckStatusResponse (..),
    mkGetHealthCheckStatusResponse,

    -- ** Response lenses
    ghcsrsHealthCheckObservations,
    ghcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to get the status for a health check.
--
-- /See:/ 'mkGetHealthCheckStatus' smart constructor.
newtype GetHealthCheckStatus = GetHealthCheckStatus'
  { -- | The ID for the health check that you want the current status for. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
    healthCheckId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHealthCheckStatus' with the minimum fields required to make a request.
--
-- * 'healthCheckId' - The ID for the health check that you want the current status for. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
mkGetHealthCheckStatus ::
  -- | 'healthCheckId'
  Lude.Text ->
  GetHealthCheckStatus
mkGetHealthCheckStatus pHealthCheckId_ =
  GetHealthCheckStatus' {healthCheckId = pHealthCheckId_}

-- | The ID for the health check that you want the current status for. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcsHealthCheckId :: Lens.Lens' GetHealthCheckStatus Lude.Text
ghcsHealthCheckId = Lens.lens (healthCheckId :: GetHealthCheckStatus -> Lude.Text) (\s a -> s {healthCheckId = a} :: GetHealthCheckStatus)
{-# DEPRECATED ghcsHealthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead." #-}

instance Lude.AWSRequest GetHealthCheckStatus where
  type Rs GetHealthCheckStatus = GetHealthCheckStatusResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetHealthCheckStatusResponse'
            Lude.<$> ( x Lude..@? "HealthCheckObservations" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "HealthCheckObservation"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetHealthCheckStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetHealthCheckStatus where
  toPath GetHealthCheckStatus' {..} =
    Lude.mconcat
      ["/2013-04-01/healthcheck/", Lude.toBS healthCheckId, "/status"]

instance Lude.ToQuery GetHealthCheckStatus where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
-- /See:/ 'mkGetHealthCheckStatusResponse' smart constructor.
data GetHealthCheckStatusResponse = GetHealthCheckStatusResponse'
  { -- | A list that contains one @HealthCheckObservation@ element for each Amazon Route 53 health checker that is reporting a status about the health check endpoint.
    healthCheckObservations :: [HealthCheckObservation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHealthCheckStatusResponse' with the minimum fields required to make a request.
--
-- * 'healthCheckObservations' - A list that contains one @HealthCheckObservation@ element for each Amazon Route 53 health checker that is reporting a status about the health check endpoint.
-- * 'responseStatus' - The response status code.
mkGetHealthCheckStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetHealthCheckStatusResponse
mkGetHealthCheckStatusResponse pResponseStatus_ =
  GetHealthCheckStatusResponse'
    { healthCheckObservations =
        Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list that contains one @HealthCheckObservation@ element for each Amazon Route 53 health checker that is reporting a status about the health check endpoint.
--
-- /Note:/ Consider using 'healthCheckObservations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcsrsHealthCheckObservations :: Lens.Lens' GetHealthCheckStatusResponse [HealthCheckObservation]
ghcsrsHealthCheckObservations = Lens.lens (healthCheckObservations :: GetHealthCheckStatusResponse -> [HealthCheckObservation]) (\s a -> s {healthCheckObservations = a} :: GetHealthCheckStatusResponse)
{-# DEPRECATED ghcsrsHealthCheckObservations "Use generic-lens or generic-optics with 'healthCheckObservations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcsrsResponseStatus :: Lens.Lens' GetHealthCheckStatusResponse Lude.Int
ghcsrsResponseStatus = Lens.lens (responseStatus :: GetHealthCheckStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHealthCheckStatusResponse)
{-# DEPRECATED ghcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
