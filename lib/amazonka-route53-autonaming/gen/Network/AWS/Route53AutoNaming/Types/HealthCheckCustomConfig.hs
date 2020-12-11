-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
  ( HealthCheckCustomConfig (..),

    -- * Smart constructor
    mkHealthCheckCustomConfig,

    -- * Lenses
    hcccFailureThreshold,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains information about an optional custom health check. A custom health check, which requires that you use a third-party health checker to evaluate the health of your resources, is useful in the following circumstances:
--
--
--     * You can't use a health check that is defined by @HealthCheckConfig@ because the resource isn't available over the internet. For example, you can use a custom health check when the instance is in an Amazon VPC. (To check the health of resources in a VPC, the health checker must also be in the VPC.)
--
--
--     * You want to use a third-party health checker regardless of where your resources are.
--
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- To change the status of a custom health check, submit an @UpdateInstanceCustomHealthStatus@ request. AWS Cloud Map doesn't monitor the status of the resource, it just keeps a record of the status specified in the most recent @UpdateInstanceCustomHealthStatus@ request.
-- Here's how custom health checks work:
--
--     * You create a service and specify a value for @FailureThreshold@ .
-- The failure threshold indicates the number of 30-second intervals you want AWS Cloud Map to wait between the time that your application sends an <https://docs.aws.amazon.com/cloud-map/latest/api/API_UpdateInstanceCustomHealthStatus.html UpdateInstanceCustomHealthStatus> request and the time that AWS Cloud Map stops routing internet traffic to the corresponding resource.
--
--
--     * You register an instance.
--
--
--     * You configure a third-party health checker to monitor the resource that is associated with the new instance.
--
--
--     * The third-party health-checker determines that the resource is unhealthy and notifies your application.
--
--
--     * Your application submits an @UpdateInstanceCustomHealthStatus@ request.
--
--
--     * AWS Cloud Map waits for (@FailureThreshold@ x 30) seconds.
--
--
--     * If another @UpdateInstanceCustomHealthStatus@ request doesn't arrive during that time to change the status back to healthy, AWS Cloud Map stops routing traffic to the resource.
--
--
--
-- /See:/ 'mkHealthCheckCustomConfig' smart constructor.
newtype HealthCheckCustomConfig = HealthCheckCustomConfig'
  { failureThreshold ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HealthCheckCustomConfig' with the minimum fields required to make a request.
--
-- * 'failureThreshold' - /Important:/ This parameter has been deprecated and is always set to 1. AWS Cloud Map waits for approximately 30 seconds after receiving an @UpdateInstanceCustomHealthStatus@ request before changing the status of the service instance.
--
-- The number of 30-second intervals that you want AWS Cloud Map to wait after receiving an @UpdateInstanceCustomHealthStatus@ request before it changes the health status of a service instance.
-- Sending a second or subsequent @UpdateInstanceCustomHealthStatus@ request with the same value before 30 seconds has passed doesn't accelerate the change. AWS Cloud Map still waits @30@ seconds after the first request to make the change.
mkHealthCheckCustomConfig ::
  HealthCheckCustomConfig
mkHealthCheckCustomConfig =
  HealthCheckCustomConfig' {failureThreshold = Lude.Nothing}

-- | /Important:/ This parameter has been deprecated and is always set to 1. AWS Cloud Map waits for approximately 30 seconds after receiving an @UpdateInstanceCustomHealthStatus@ request before changing the status of the service instance.
--
-- The number of 30-second intervals that you want AWS Cloud Map to wait after receiving an @UpdateInstanceCustomHealthStatus@ request before it changes the health status of a service instance.
-- Sending a second or subsequent @UpdateInstanceCustomHealthStatus@ request with the same value before 30 seconds has passed doesn't accelerate the change. AWS Cloud Map still waits @30@ seconds after the first request to make the change.
--
-- /Note:/ Consider using 'failureThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcccFailureThreshold :: Lens.Lens' HealthCheckCustomConfig (Lude.Maybe Lude.Natural)
hcccFailureThreshold = Lens.lens (failureThreshold :: HealthCheckCustomConfig -> Lude.Maybe Lude.Natural) (\s a -> s {failureThreshold = a} :: HealthCheckCustomConfig)
{-# DEPRECATED hcccFailureThreshold "Use generic-lens or generic-optics with 'failureThreshold' instead." #-}

instance Lude.FromJSON HealthCheckCustomConfig where
  parseJSON =
    Lude.withObject
      "HealthCheckCustomConfig"
      ( \x ->
          HealthCheckCustomConfig' Lude.<$> (x Lude..:? "FailureThreshold")
      )

instance Lude.ToJSON HealthCheckCustomConfig where
  toJSON HealthCheckCustomConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [("FailureThreshold" Lude..=) Lude.<$> failureThreshold]
      )
