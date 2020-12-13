{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckLastFailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the reason that a specified health check failed most recently.
module Network.AWS.Route53.GetHealthCheckLastFailureReason
  ( -- * Creating a request
    GetHealthCheckLastFailureReason (..),
    mkGetHealthCheckLastFailureReason,

    -- ** Request lenses
    ghclfrHealthCheckId,

    -- * Destructuring the response
    GetHealthCheckLastFailureReasonResponse (..),
    mkGetHealthCheckLastFailureReasonResponse,

    -- ** Response lenses
    ghclfrrsHealthCheckObservations,
    ghclfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request for the reason that a health check failed most recently.
--
-- /See:/ 'mkGetHealthCheckLastFailureReason' smart constructor.
newtype GetHealthCheckLastFailureReason = GetHealthCheckLastFailureReason'
  { -- | The ID for the health check for which you want the last failure reason. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
    healthCheckId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHealthCheckLastFailureReason' with the minimum fields required to make a request.
--
-- * 'healthCheckId' - The ID for the health check for which you want the last failure reason. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
mkGetHealthCheckLastFailureReason ::
  -- | 'healthCheckId'
  Lude.Text ->
  GetHealthCheckLastFailureReason
mkGetHealthCheckLastFailureReason pHealthCheckId_ =
  GetHealthCheckLastFailureReason' {healthCheckId = pHealthCheckId_}

-- | The ID for the health check for which you want the last failure reason. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghclfrHealthCheckId :: Lens.Lens' GetHealthCheckLastFailureReason Lude.Text
ghclfrHealthCheckId = Lens.lens (healthCheckId :: GetHealthCheckLastFailureReason -> Lude.Text) (\s a -> s {healthCheckId = a} :: GetHealthCheckLastFailureReason)
{-# DEPRECATED ghclfrHealthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead." #-}

instance Lude.AWSRequest GetHealthCheckLastFailureReason where
  type
    Rs GetHealthCheckLastFailureReason =
      GetHealthCheckLastFailureReasonResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetHealthCheckLastFailureReasonResponse'
            Lude.<$> ( x Lude..@? "HealthCheckObservations" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "HealthCheckObservation"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetHealthCheckLastFailureReason where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetHealthCheckLastFailureReason where
  toPath GetHealthCheckLastFailureReason' {..} =
    Lude.mconcat
      [ "/2013-04-01/healthcheck/",
        Lude.toBS healthCheckId,
        "/lastfailurereason"
      ]

instance Lude.ToQuery GetHealthCheckLastFailureReason where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the response to a @GetHealthCheckLastFailureReason@ request.
--
-- /See:/ 'mkGetHealthCheckLastFailureReasonResponse' smart constructor.
data GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse'
  { -- | A list that contains one @Observation@ element for each Amazon Route 53 health checker that is reporting a last failure reason.
    healthCheckObservations :: [HealthCheckObservation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHealthCheckLastFailureReasonResponse' with the minimum fields required to make a request.
--
-- * 'healthCheckObservations' - A list that contains one @Observation@ element for each Amazon Route 53 health checker that is reporting a last failure reason.
-- * 'responseStatus' - The response status code.
mkGetHealthCheckLastFailureReasonResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetHealthCheckLastFailureReasonResponse
mkGetHealthCheckLastFailureReasonResponse pResponseStatus_ =
  GetHealthCheckLastFailureReasonResponse'
    { healthCheckObservations =
        Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list that contains one @Observation@ element for each Amazon Route 53 health checker that is reporting a last failure reason.
--
-- /Note:/ Consider using 'healthCheckObservations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghclfrrsHealthCheckObservations :: Lens.Lens' GetHealthCheckLastFailureReasonResponse [HealthCheckObservation]
ghclfrrsHealthCheckObservations = Lens.lens (healthCheckObservations :: GetHealthCheckLastFailureReasonResponse -> [HealthCheckObservation]) (\s a -> s {healthCheckObservations = a} :: GetHealthCheckLastFailureReasonResponse)
{-# DEPRECATED ghclfrrsHealthCheckObservations "Use generic-lens or generic-optics with 'healthCheckObservations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghclfrrsResponseStatus :: Lens.Lens' GetHealthCheckLastFailureReasonResponse Lude.Int
ghclfrrsResponseStatus = Lens.lens (responseStatus :: GetHealthCheckLastFailureReasonResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHealthCheckLastFailureReasonResponse)
{-# DEPRECATED ghclfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
