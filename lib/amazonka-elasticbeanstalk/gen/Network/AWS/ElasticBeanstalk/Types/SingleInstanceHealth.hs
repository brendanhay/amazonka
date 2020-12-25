{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
  ( SingleInstanceHealth (..),

    -- * Smart constructor
    mkSingleInstanceHealth,

    -- * Lenses
    sihApplicationMetrics,
    sihAvailabilityZone,
    sihCauses,
    sihColor,
    sihDeployment,
    sihHealthStatus,
    sihInstanceId,
    sihInstanceType,
    sihLaunchedAt,
    sihSystem,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Cause as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Deployment as Types
import qualified Network.AWS.ElasticBeanstalk.Types.InstanceId as Types
import qualified Network.AWS.ElasticBeanstalk.Types.String as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SystemStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed health information about an Amazon EC2 instance in your Elastic Beanstalk environment.
--
-- /See:/ 'mkSingleInstanceHealth' smart constructor.
data SingleInstanceHealth = SingleInstanceHealth'
  { -- | Request metrics from your application.
    applicationMetrics :: Core.Maybe Types.ApplicationMetrics,
    -- | The availability zone in which the instance runs.
    availabilityZone :: Core.Maybe Types.String,
    -- | Represents the causes, which provide more information about the current health status.
    causes :: Core.Maybe [Types.Cause],
    -- | Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
    color :: Core.Maybe Types.String,
    -- | Information about the most recent deployment to an instance.
    deployment :: Core.Maybe Types.Deployment,
    -- | Returns the health status of the specified instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
    healthStatus :: Core.Maybe Types.String,
    -- | The ID of the Amazon EC2 instance.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The instance's type.
    instanceType :: Core.Maybe Types.String,
    -- | The time at which the EC2 instance was launched.
    launchedAt :: Core.Maybe Core.UTCTime,
    -- | Operating system metrics from the instance.
    system :: Core.Maybe Types.SystemStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SingleInstanceHealth' value with any optional fields omitted.
mkSingleInstanceHealth ::
  SingleInstanceHealth
mkSingleInstanceHealth =
  SingleInstanceHealth'
    { applicationMetrics = Core.Nothing,
      availabilityZone = Core.Nothing,
      causes = Core.Nothing,
      color = Core.Nothing,
      deployment = Core.Nothing,
      healthStatus = Core.Nothing,
      instanceId = Core.Nothing,
      instanceType = Core.Nothing,
      launchedAt = Core.Nothing,
      system = Core.Nothing
    }

-- | Request metrics from your application.
--
-- /Note:/ Consider using 'applicationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihApplicationMetrics :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.ApplicationMetrics)
sihApplicationMetrics = Lens.field @"applicationMetrics"
{-# DEPRECATED sihApplicationMetrics "Use generic-lens or generic-optics with 'applicationMetrics' instead." #-}

-- | The availability zone in which the instance runs.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihAvailabilityZone :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.String)
sihAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED sihAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Represents the causes, which provide more information about the current health status.
--
-- /Note:/ Consider using 'causes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihCauses :: Lens.Lens' SingleInstanceHealth (Core.Maybe [Types.Cause])
sihCauses = Lens.field @"causes"
{-# DEPRECATED sihCauses "Use generic-lens or generic-optics with 'causes' instead." #-}

-- | Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /Note:/ Consider using 'color' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihColor :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.String)
sihColor = Lens.field @"color"
{-# DEPRECATED sihColor "Use generic-lens or generic-optics with 'color' instead." #-}

-- | Information about the most recent deployment to an instance.
--
-- /Note:/ Consider using 'deployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihDeployment :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.Deployment)
sihDeployment = Lens.field @"deployment"
{-# DEPRECATED sihDeployment "Use generic-lens or generic-optics with 'deployment' instead." #-}

-- | Returns the health status of the specified instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihHealthStatus :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.String)
sihHealthStatus = Lens.field @"healthStatus"
{-# DEPRECATED sihHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | The ID of the Amazon EC2 instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihInstanceId :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.InstanceId)
sihInstanceId = Lens.field @"instanceId"
{-# DEPRECATED sihInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The instance's type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihInstanceType :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.String)
sihInstanceType = Lens.field @"instanceType"
{-# DEPRECATED sihInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The time at which the EC2 instance was launched.
--
-- /Note:/ Consider using 'launchedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihLaunchedAt :: Lens.Lens' SingleInstanceHealth (Core.Maybe Core.UTCTime)
sihLaunchedAt = Lens.field @"launchedAt"
{-# DEPRECATED sihLaunchedAt "Use generic-lens or generic-optics with 'launchedAt' instead." #-}

-- | Operating system metrics from the instance.
--
-- /Note:/ Consider using 'system' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihSystem :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.SystemStatus)
sihSystem = Lens.field @"system"
{-# DEPRECATED sihSystem "Use generic-lens or generic-optics with 'system' instead." #-}

instance Core.FromXML SingleInstanceHealth where
  parseXML x =
    SingleInstanceHealth'
      Core.<$> (x Core..@? "ApplicationMetrics")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "Causes" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "Color")
      Core.<*> (x Core..@? "Deployment")
      Core.<*> (x Core..@? "HealthStatus")
      Core.<*> (x Core..@? "InstanceId")
      Core.<*> (x Core..@? "InstanceType")
      Core.<*> (x Core..@? "LaunchedAt")
      Core.<*> (x Core..@? "System")
