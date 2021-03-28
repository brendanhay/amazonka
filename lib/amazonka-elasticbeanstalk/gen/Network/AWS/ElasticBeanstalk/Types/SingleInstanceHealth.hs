{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
  ( SingleInstanceHealth (..)
  -- * Smart constructor
  , mkSingleInstanceHealth
  -- * Lenses
  , sihApplicationMetrics
  , sihAvailabilityZone
  , sihCauses
  , sihColor
  , sihDeployment
  , sihHealthStatus
  , sihInstanceId
  , sihInstanceType
  , sihLaunchedAt
  , sihSystem
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Cause as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Deployment as Types
import qualified Network.AWS.ElasticBeanstalk.Types.InstanceId as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SystemStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed health information about an Amazon EC2 instance in your Elastic Beanstalk environment.
--
-- /See:/ 'mkSingleInstanceHealth' smart constructor.
data SingleInstanceHealth = SingleInstanceHealth'
  { applicationMetrics :: Core.Maybe Types.ApplicationMetrics
    -- ^ Request metrics from your application.
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The availability zone in which the instance runs.
  , causes :: Core.Maybe [Types.Cause]
    -- ^ Represents the causes, which provide more information about the current health status.
  , color :: Core.Maybe Core.Text
    -- ^ Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
  , deployment :: Core.Maybe Types.Deployment
    -- ^ Information about the most recent deployment to an instance.
  , healthStatus :: Core.Maybe Core.Text
    -- ^ Returns the health status of the specified instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The ID of the Amazon EC2 instance.
  , instanceType :: Core.Maybe Core.Text
    -- ^ The instance's type.
  , launchedAt :: Core.Maybe Core.UTCTime
    -- ^ The time at which the EC2 instance was launched.
  , system :: Core.Maybe Types.SystemStatus
    -- ^ Operating system metrics from the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SingleInstanceHealth' value with any optional fields omitted.
mkSingleInstanceHealth
    :: SingleInstanceHealth
mkSingleInstanceHealth
  = SingleInstanceHealth'{applicationMetrics = Core.Nothing,
                          availabilityZone = Core.Nothing, causes = Core.Nothing,
                          color = Core.Nothing, deployment = Core.Nothing,
                          healthStatus = Core.Nothing, instanceId = Core.Nothing,
                          instanceType = Core.Nothing, launchedAt = Core.Nothing,
                          system = Core.Nothing}

-- | Request metrics from your application.
--
-- /Note:/ Consider using 'applicationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihApplicationMetrics :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.ApplicationMetrics)
sihApplicationMetrics = Lens.field @"applicationMetrics"
{-# INLINEABLE sihApplicationMetrics #-}
{-# DEPRECATED applicationMetrics "Use generic-lens or generic-optics with 'applicationMetrics' instead"  #-}

-- | The availability zone in which the instance runs.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihAvailabilityZone :: Lens.Lens' SingleInstanceHealth (Core.Maybe Core.Text)
sihAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE sihAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | Represents the causes, which provide more information about the current health status.
--
-- /Note:/ Consider using 'causes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihCauses :: Lens.Lens' SingleInstanceHealth (Core.Maybe [Types.Cause])
sihCauses = Lens.field @"causes"
{-# INLINEABLE sihCauses #-}
{-# DEPRECATED causes "Use generic-lens or generic-optics with 'causes' instead"  #-}

-- | Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /Note:/ Consider using 'color' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihColor :: Lens.Lens' SingleInstanceHealth (Core.Maybe Core.Text)
sihColor = Lens.field @"color"
{-# INLINEABLE sihColor #-}
{-# DEPRECATED color "Use generic-lens or generic-optics with 'color' instead"  #-}

-- | Information about the most recent deployment to an instance.
--
-- /Note:/ Consider using 'deployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihDeployment :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.Deployment)
sihDeployment = Lens.field @"deployment"
{-# INLINEABLE sihDeployment #-}
{-# DEPRECATED deployment "Use generic-lens or generic-optics with 'deployment' instead"  #-}

-- | Returns the health status of the specified instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihHealthStatus :: Lens.Lens' SingleInstanceHealth (Core.Maybe Core.Text)
sihHealthStatus = Lens.field @"healthStatus"
{-# INLINEABLE sihHealthStatus #-}
{-# DEPRECATED healthStatus "Use generic-lens or generic-optics with 'healthStatus' instead"  #-}

-- | The ID of the Amazon EC2 instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihInstanceId :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.InstanceId)
sihInstanceId = Lens.field @"instanceId"
{-# INLINEABLE sihInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The instance's type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihInstanceType :: Lens.Lens' SingleInstanceHealth (Core.Maybe Core.Text)
sihInstanceType = Lens.field @"instanceType"
{-# INLINEABLE sihInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The time at which the EC2 instance was launched.
--
-- /Note:/ Consider using 'launchedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihLaunchedAt :: Lens.Lens' SingleInstanceHealth (Core.Maybe Core.UTCTime)
sihLaunchedAt = Lens.field @"launchedAt"
{-# INLINEABLE sihLaunchedAt #-}
{-# DEPRECATED launchedAt "Use generic-lens or generic-optics with 'launchedAt' instead"  #-}

-- | Operating system metrics from the instance.
--
-- /Note:/ Consider using 'system' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihSystem :: Lens.Lens' SingleInstanceHealth (Core.Maybe Types.SystemStatus)
sihSystem = Lens.field @"system"
{-# INLINEABLE sihSystem #-}
{-# DEPRECATED system "Use generic-lens or generic-optics with 'system' instead"  #-}

instance Core.FromXML SingleInstanceHealth where
        parseXML x
          = SingleInstanceHealth' Core.<$>
              (x Core..@? "ApplicationMetrics") Core.<*>
                x Core..@? "AvailabilityZone"
                Core.<*> x Core..@? "Causes" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Color"
                Core.<*> x Core..@? "Deployment"
                Core.<*> x Core..@? "HealthStatus"
                Core.<*> x Core..@? "InstanceId"
                Core.<*> x Core..@? "InstanceType"
                Core.<*> x Core..@? "LaunchedAt"
                Core.<*> x Core..@? "System"
