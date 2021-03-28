{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Instance
  ( Instance (..)
  -- * Smart constructor
  , mkInstance
  -- * Lenses
  , iInstanceId
  , iAvailabilityZone
  , iLifecycleState
  , iHealthStatus
  , iProtectedFromScaleIn
  , iInstanceType
  , iLaunchConfigurationName
  , iLaunchTemplate
  , iWeightedCapacity
  ) where

import qualified Network.AWS.AutoScaling.Types.LaunchTemplateSpecification as Types
import qualified Network.AWS.AutoScaling.Types.LifecycleState as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen19 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen32 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an EC2 instance.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { instanceId :: Types.XmlStringMaxLen19
    -- ^ The ID of the instance.
  , availabilityZone :: Types.XmlStringMaxLen255
    -- ^ The Availability Zone in which the instance is running.
  , lifecycleState :: Types.LifecycleState
    -- ^ A description of the current lifecycle state. The @Quarantined@ state is not used.
  , healthStatus :: Types.XmlStringMaxLen32
    -- ^ The last reported health status of the instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and that Amazon EC2 Auto Scaling should terminate and replace it.
  , protectedFromScaleIn :: Core.Bool
    -- ^ Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
  , instanceType :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The instance type of the EC2 instance.
  , launchConfigurationName :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The launch configuration associated with the instance.
  , launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification
    -- ^ The launch template for the instance.
  , weightedCapacity :: Core.Maybe Types.XmlStringMaxLen32
    -- ^ The number of capacity units contributed by the instance based on its instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance
    :: Types.XmlStringMaxLen19 -- ^ 'instanceId'
    -> Types.XmlStringMaxLen255 -- ^ 'availabilityZone'
    -> Types.LifecycleState -- ^ 'lifecycleState'
    -> Types.XmlStringMaxLen32 -- ^ 'healthStatus'
    -> Core.Bool -- ^ 'protectedFromScaleIn'
    -> Instance
mkInstance instanceId availabilityZone lifecycleState healthStatus
  protectedFromScaleIn
  = Instance'{instanceId, availabilityZone, lifecycleState,
              healthStatus, protectedFromScaleIn, instanceType = Core.Nothing,
              launchConfigurationName = Core.Nothing,
              launchTemplate = Core.Nothing, weightedCapacity = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' Instance Types.XmlStringMaxLen19
iInstanceId = Lens.field @"instanceId"
{-# INLINEABLE iInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The Availability Zone in which the instance is running.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAvailabilityZone :: Lens.Lens' Instance Types.XmlStringMaxLen255
iAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE iAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | A description of the current lifecycle state. The @Quarantined@ state is not used.
--
-- /Note:/ Consider using 'lifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLifecycleState :: Lens.Lens' Instance Types.LifecycleState
iLifecycleState = Lens.field @"lifecycleState"
{-# INLINEABLE iLifecycleState #-}
{-# DEPRECATED lifecycleState "Use generic-lens or generic-optics with 'lifecycleState' instead"  #-}

-- | The last reported health status of the instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and that Amazon EC2 Auto Scaling should terminate and replace it.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHealthStatus :: Lens.Lens' Instance Types.XmlStringMaxLen32
iHealthStatus = Lens.field @"healthStatus"
{-# INLINEABLE iHealthStatus #-}
{-# DEPRECATED healthStatus "Use generic-lens or generic-optics with 'healthStatus' instead"  #-}

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
--
-- /Note:/ Consider using 'protectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iProtectedFromScaleIn :: Lens.Lens' Instance Core.Bool
iProtectedFromScaleIn = Lens.field @"protectedFromScaleIn"
{-# INLINEABLE iProtectedFromScaleIn #-}
{-# DEPRECATED protectedFromScaleIn "Use generic-lens or generic-optics with 'protectedFromScaleIn' instead"  #-}

-- | The instance type of the EC2 instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceType :: Lens.Lens' Instance (Core.Maybe Types.XmlStringMaxLen255)
iInstanceType = Lens.field @"instanceType"
{-# INLINEABLE iInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The launch configuration associated with the instance.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLaunchConfigurationName :: Lens.Lens' Instance (Core.Maybe Types.XmlStringMaxLen255)
iLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# INLINEABLE iLaunchConfigurationName #-}
{-# DEPRECATED launchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead"  #-}

-- | The launch template for the instance.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLaunchTemplate :: Lens.Lens' Instance (Core.Maybe Types.LaunchTemplateSpecification)
iLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE iLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

-- | The number of capacity units contributed by the instance based on its instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iWeightedCapacity :: Lens.Lens' Instance (Core.Maybe Types.XmlStringMaxLen32)
iWeightedCapacity = Lens.field @"weightedCapacity"
{-# INLINEABLE iWeightedCapacity #-}
{-# DEPRECATED weightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead"  #-}

instance Core.FromXML Instance where
        parseXML x
          = Instance' Core.<$>
              (x Core..@ "InstanceId") Core.<*> x Core..@ "AvailabilityZone"
                Core.<*> x Core..@ "LifecycleState"
                Core.<*> x Core..@ "HealthStatus"
                Core.<*> x Core..@ "ProtectedFromScaleIn"
                Core.<*> x Core..@? "InstanceType"
                Core.<*> x Core..@? "LaunchConfigurationName"
                Core.<*> x Core..@? "LaunchTemplate"
                Core.<*> x Core..@? "WeightedCapacity"
