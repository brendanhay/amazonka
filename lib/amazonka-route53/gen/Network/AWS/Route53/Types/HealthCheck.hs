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
    hcId,
    hcCallerReference,
    hcHealthCheckConfig,
    hcHealthCheckVersion,
    hcCloudWatchAlarmConfiguration,
    hcLinkedService,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.CallerReference as Types
import qualified Network.AWS.Route53.Types.CloudWatchAlarmConfiguration as Types
import qualified Network.AWS.Route53.Types.HealthCheckConfig as Types
import qualified Network.AWS.Route53.Types.Id as Types
import qualified Network.AWS.Route53.Types.LinkedService as Types

-- | A complex type that contains information about one health check that is associated with the current AWS account.
--
-- /See:/ 'mkHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { -- | The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
    id :: Types.Id,
    -- | A unique string that you specified when you created the health check.
    callerReference :: Types.CallerReference,
    -- | A complex type that contains detailed information about one health check.
    healthCheckConfig :: Types.HealthCheckConfig,
    -- | The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
    healthCheckVersion :: Core.Natural,
    -- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
    cloudWatchAlarmConfiguration :: Core.Maybe Types.CloudWatchAlarmConfiguration,
    -- | If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
    linkedService :: Core.Maybe Types.LinkedService
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HealthCheck' value with any optional fields omitted.
mkHealthCheck ::
  -- | 'id'
  Types.Id ->
  -- | 'callerReference'
  Types.CallerReference ->
  -- | 'healthCheckConfig'
  Types.HealthCheckConfig ->
  -- | 'healthCheckVersion'
  Core.Natural ->
  HealthCheck
mkHealthCheck
  id
  callerReference
  healthCheckConfig
  healthCheckVersion =
    HealthCheck'
      { id,
        callerReference,
        healthCheckConfig,
        healthCheckVersion,
        cloudWatchAlarmConfiguration = Core.Nothing,
        linkedService = Core.Nothing
      }

-- | The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcId :: Lens.Lens' HealthCheck Types.Id
hcId = Lens.field @"id"
{-# DEPRECATED hcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A unique string that you specified when you created the health check.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcCallerReference :: Lens.Lens' HealthCheck Types.CallerReference
hcCallerReference = Lens.field @"callerReference"
{-# DEPRECATED hcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex type that contains detailed information about one health check.
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHealthCheckConfig :: Lens.Lens' HealthCheck Types.HealthCheckConfig
hcHealthCheckConfig = Lens.field @"healthCheckConfig"
{-# DEPRECATED hcHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

-- | The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
--
-- /Note:/ Consider using 'healthCheckVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHealthCheckVersion :: Lens.Lens' HealthCheck Core.Natural
hcHealthCheckVersion = Lens.field @"healthCheckVersion"
{-# DEPRECATED hcHealthCheckVersion "Use generic-lens or generic-optics with 'healthCheckVersion' instead." #-}

-- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
--
-- /Note:/ Consider using 'cloudWatchAlarmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcCloudWatchAlarmConfiguration :: Lens.Lens' HealthCheck (Core.Maybe Types.CloudWatchAlarmConfiguration)
hcCloudWatchAlarmConfiguration = Lens.field @"cloudWatchAlarmConfiguration"
{-# DEPRECATED hcCloudWatchAlarmConfiguration "Use generic-lens or generic-optics with 'cloudWatchAlarmConfiguration' instead." #-}

-- | If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- /Note:/ Consider using 'linkedService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcLinkedService :: Lens.Lens' HealthCheck (Core.Maybe Types.LinkedService)
hcLinkedService = Lens.field @"linkedService"
{-# DEPRECATED hcLinkedService "Use generic-lens or generic-optics with 'linkedService' instead." #-}

instance Core.FromXML HealthCheck where
  parseXML x =
    HealthCheck'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "HealthCheckConfig")
      Core.<*> (x Core..@ "HealthCheckVersion")
      Core.<*> (x Core..@? "CloudWatchAlarmConfiguration")
      Core.<*> (x Core..@? "LinkedService")
