{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SpotProvisioningSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.SpotProvisioningSpecification
  ( SpotProvisioningSpecification (..)
  -- * Smart constructor
  , mkSpotProvisioningSpecification
  -- * Lenses
  , spsTimeoutDurationMinutes
  , spsTimeoutAction
  , spsAllocationStrategy
  , spsBlockDurationMinutes
  ) where

import qualified Network.AWS.EMR.Types.SpotProvisioningAllocationStrategy as Types
import qualified Network.AWS.EMR.Types.SpotProvisioningTimeoutAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The launch specification for Spot Instances in the instance fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
-- /See:/ 'mkSpotProvisioningSpecification' smart constructor.
data SpotProvisioningSpecification = SpotProvisioningSpecification'
  { timeoutDurationMinutes :: Core.Natural
    -- ^ The spot provisioning timeout period in minutes. If Spot Instances are not provisioned within this time period, the @TimeOutAction@ is taken. Minimum value is 5 and maximum value is 1440. The timeout applies only during initial provisioning, when the cluster is first created.
  , timeoutAction :: Types.SpotProvisioningTimeoutAction
    -- ^ The action to take when @TargetSpotCapacity@ has not been fulfilled when the @TimeoutDurationMinutes@ has expired; that is, when all Spot Instances could not be provisioned within the Spot provisioning timeout. Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@ . SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available, On-Demand Instances should be provisioned to fulfill any remaining Spot capacity.
  , allocationStrategy :: Core.Maybe Types.SpotProvisioningAllocationStrategy
    -- ^ Specifies the strategy to use in launching Spot Instance fleets. Currently, the only option is capacity-optimized (the default), which launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching. 
  , blockDurationMinutes :: Core.Maybe Core.Natural
    -- ^ The defined duration for Spot Instances (also known as Spot blocks) in minutes. When specified, the Spot Instance does not terminate before the defined duration expires, and defined duration pricing for Spot instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The duration period starts as soon as a Spot Instance receives its instance ID. At the end of the duration, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpotProvisioningSpecification' value with any optional fields omitted.
mkSpotProvisioningSpecification
    :: Core.Natural -- ^ 'timeoutDurationMinutes'
    -> Types.SpotProvisioningTimeoutAction -- ^ 'timeoutAction'
    -> SpotProvisioningSpecification
mkSpotProvisioningSpecification timeoutDurationMinutes
  timeoutAction
  = SpotProvisioningSpecification'{timeoutDurationMinutes,
                                   timeoutAction, allocationStrategy = Core.Nothing,
                                   blockDurationMinutes = Core.Nothing}

-- | The spot provisioning timeout period in minutes. If Spot Instances are not provisioned within this time period, the @TimeOutAction@ is taken. Minimum value is 5 and maximum value is 1440. The timeout applies only during initial provisioning, when the cluster is first created.
--
-- /Note:/ Consider using 'timeoutDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsTimeoutDurationMinutes :: Lens.Lens' SpotProvisioningSpecification Core.Natural
spsTimeoutDurationMinutes = Lens.field @"timeoutDurationMinutes"
{-# INLINEABLE spsTimeoutDurationMinutes #-}
{-# DEPRECATED timeoutDurationMinutes "Use generic-lens or generic-optics with 'timeoutDurationMinutes' instead"  #-}

-- | The action to take when @TargetSpotCapacity@ has not been fulfilled when the @TimeoutDurationMinutes@ has expired; that is, when all Spot Instances could not be provisioned within the Spot provisioning timeout. Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@ . SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available, On-Demand Instances should be provisioned to fulfill any remaining Spot capacity.
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsTimeoutAction :: Lens.Lens' SpotProvisioningSpecification Types.SpotProvisioningTimeoutAction
spsTimeoutAction = Lens.field @"timeoutAction"
{-# INLINEABLE spsTimeoutAction #-}
{-# DEPRECATED timeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead"  #-}

-- | Specifies the strategy to use in launching Spot Instance fleets. Currently, the only option is capacity-optimized (the default), which launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching. 
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsAllocationStrategy :: Lens.Lens' SpotProvisioningSpecification (Core.Maybe Types.SpotProvisioningAllocationStrategy)
spsAllocationStrategy = Lens.field @"allocationStrategy"
{-# INLINEABLE spsAllocationStrategy #-}
{-# DEPRECATED allocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead"  #-}

-- | The defined duration for Spot Instances (also known as Spot blocks) in minutes. When specified, the Spot Instance does not terminate before the defined duration expires, and defined duration pricing for Spot instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The duration period starts as soon as a Spot Instance receives its instance ID. At the end of the duration, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates. 
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsBlockDurationMinutes :: Lens.Lens' SpotProvisioningSpecification (Core.Maybe Core.Natural)
spsBlockDurationMinutes = Lens.field @"blockDurationMinutes"
{-# INLINEABLE spsBlockDurationMinutes #-}
{-# DEPRECATED blockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead"  #-}

instance Core.FromJSON SpotProvisioningSpecification where
        toJSON SpotProvisioningSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("TimeoutDurationMinutes" Core..= timeoutDurationMinutes),
                  Core.Just ("TimeoutAction" Core..= timeoutAction),
                  ("AllocationStrategy" Core..=) Core.<$> allocationStrategy,
                  ("BlockDurationMinutes" Core..=) Core.<$> blockDurationMinutes])

instance Core.FromJSON SpotProvisioningSpecification where
        parseJSON
          = Core.withObject "SpotProvisioningSpecification" Core.$
              \ x ->
                SpotProvisioningSpecification' Core.<$>
                  (x Core..: "TimeoutDurationMinutes") Core.<*>
                    x Core..: "TimeoutAction"
                    Core.<*> x Core..:? "AllocationStrategy"
                    Core.<*> x Core..:? "BlockDurationMinutes"
