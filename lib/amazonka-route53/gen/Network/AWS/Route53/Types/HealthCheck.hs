{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheck
  ( HealthCheck (..),

    -- * Smart constructor
    mkHealthCheck,

    -- * Lenses
    hcLinkedService,
    hcHealthCheckConfig,
    hcCloudWatchAlarmConfiguration,
    hcId,
    hcHealthCheckVersion,
    hcCallerReference,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
import Network.AWS.Route53.Types.HealthCheckConfig
import Network.AWS.Route53.Types.LinkedService

-- | A complex type that contains information about one health check that is associated with the current AWS account.
--
-- /See:/ 'mkHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { -- | If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
    linkedService :: Lude.Maybe LinkedService,
    -- | A complex type that contains detailed information about one health check.
    healthCheckConfig :: HealthCheckConfig,
    -- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
    cloudWatchAlarmConfiguration :: Lude.Maybe CloudWatchAlarmConfiguration,
    -- | The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
    id :: Lude.Text,
    -- | The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
    healthCheckVersion :: Lude.Natural,
    -- | A unique string that you specified when you created the health check.
    callerReference :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- * 'linkedService' - If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
-- * 'healthCheckConfig' - A complex type that contains detailed information about one health check.
-- * 'cloudWatchAlarmConfiguration' - A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
-- * 'id' - The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
-- * 'healthCheckVersion' - The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
-- * 'callerReference' - A unique string that you specified when you created the health check.
mkHealthCheck ::
  -- | 'healthCheckConfig'
  HealthCheckConfig ->
  -- | 'id'
  Lude.Text ->
  -- | 'healthCheckVersion'
  Lude.Natural ->
  -- | 'callerReference'
  Lude.Text ->
  HealthCheck
mkHealthCheck
  pHealthCheckConfig_
  pId_
  pHealthCheckVersion_
  pCallerReference_ =
    HealthCheck'
      { linkedService = Lude.Nothing,
        healthCheckConfig = pHealthCheckConfig_,
        cloudWatchAlarmConfiguration = Lude.Nothing,
        id = pId_,
        healthCheckVersion = pHealthCheckVersion_,
        callerReference = pCallerReference_
      }

-- | If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- /Note:/ Consider using 'linkedService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcLinkedService :: Lens.Lens' HealthCheck (Lude.Maybe LinkedService)
hcLinkedService = Lens.lens (linkedService :: HealthCheck -> Lude.Maybe LinkedService) (\s a -> s {linkedService = a} :: HealthCheck)
{-# DEPRECATED hcLinkedService "Use generic-lens or generic-optics with 'linkedService' instead." #-}

-- | A complex type that contains detailed information about one health check.
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHealthCheckConfig :: Lens.Lens' HealthCheck HealthCheckConfig
hcHealthCheckConfig = Lens.lens (healthCheckConfig :: HealthCheck -> HealthCheckConfig) (\s a -> s {healthCheckConfig = a} :: HealthCheck)
{-# DEPRECATED hcHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

-- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
--
-- /Note:/ Consider using 'cloudWatchAlarmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcCloudWatchAlarmConfiguration :: Lens.Lens' HealthCheck (Lude.Maybe CloudWatchAlarmConfiguration)
hcCloudWatchAlarmConfiguration = Lens.lens (cloudWatchAlarmConfiguration :: HealthCheck -> Lude.Maybe CloudWatchAlarmConfiguration) (\s a -> s {cloudWatchAlarmConfiguration = a} :: HealthCheck)
{-# DEPRECATED hcCloudWatchAlarmConfiguration "Use generic-lens or generic-optics with 'cloudWatchAlarmConfiguration' instead." #-}

-- | The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcId :: Lens.Lens' HealthCheck Lude.Text
hcId = Lens.lens (id :: HealthCheck -> Lude.Text) (\s a -> s {id = a} :: HealthCheck)
{-# DEPRECATED hcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
--
-- /Note:/ Consider using 'healthCheckVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHealthCheckVersion :: Lens.Lens' HealthCheck Lude.Natural
hcHealthCheckVersion = Lens.lens (healthCheckVersion :: HealthCheck -> Lude.Natural) (\s a -> s {healthCheckVersion = a} :: HealthCheck)
{-# DEPRECATED hcHealthCheckVersion "Use generic-lens or generic-optics with 'healthCheckVersion' instead." #-}

-- | A unique string that you specified when you created the health check.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcCallerReference :: Lens.Lens' HealthCheck Lude.Text
hcCallerReference = Lens.lens (callerReference :: HealthCheck -> Lude.Text) (\s a -> s {callerReference = a} :: HealthCheck)
{-# DEPRECATED hcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

instance Lude.FromXML HealthCheck where
  parseXML x =
    HealthCheck'
      Lude.<$> (x Lude..@? "LinkedService")
      Lude.<*> (x Lude..@ "HealthCheckConfig")
      Lude.<*> (x Lude..@? "CloudWatchAlarmConfiguration")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "HealthCheckVersion")
      Lude.<*> (x Lude..@ "CallerReference")
