-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SpotProvisioningSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SpotProvisioningSpecification
  ( SpotProvisioningSpecification (..),

    -- * Smart constructor
    mkSpotProvisioningSpecification,

    -- * Lenses
    spsBlockDurationMinutes,
    spsAllocationStrategy,
    spsTimeoutDurationMinutes,
    spsTimeoutAction,
  )
where

import Network.AWS.EMR.Types.SpotProvisioningAllocationStrategy
import Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The launch specification for Spot Instances in the instance fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
-- /See:/ 'mkSpotProvisioningSpecification' smart constructor.
data SpotProvisioningSpecification = SpotProvisioningSpecification'
  { blockDurationMinutes ::
      Lude.Maybe Lude.Natural,
    allocationStrategy ::
      Lude.Maybe
        SpotProvisioningAllocationStrategy,
    timeoutDurationMinutes ::
      Lude.Natural,
    timeoutAction ::
      SpotProvisioningTimeoutAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotProvisioningSpecification' with the minimum fields required to make a request.
--
-- * 'allocationStrategy' - Specifies the strategy to use in launching Spot Instance fleets. Currently, the only option is capacity-optimized (the default), which launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
-- * 'blockDurationMinutes' - The defined duration for Spot Instances (also known as Spot blocks) in minutes. When specified, the Spot Instance does not terminate before the defined duration expires, and defined duration pricing for Spot instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The duration period starts as soon as a Spot Instance receives its instance ID. At the end of the duration, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
-- * 'timeoutAction' - The action to take when @TargetSpotCapacity@ has not been fulfilled when the @TimeoutDurationMinutes@ has expired; that is, when all Spot Instances could not be provisioned within the Spot provisioning timeout. Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@ . SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available, On-Demand Instances should be provisioned to fulfill any remaining Spot capacity.
-- * 'timeoutDurationMinutes' - The spot provisioning timeout period in minutes. If Spot Instances are not provisioned within this time period, the @TimeOutAction@ is taken. Minimum value is 5 and maximum value is 1440. The timeout applies only during initial provisioning, when the cluster is first created.
mkSpotProvisioningSpecification ::
  -- | 'timeoutDurationMinutes'
  Lude.Natural ->
  -- | 'timeoutAction'
  SpotProvisioningTimeoutAction ->
  SpotProvisioningSpecification
mkSpotProvisioningSpecification
  pTimeoutDurationMinutes_
  pTimeoutAction_ =
    SpotProvisioningSpecification'
      { blockDurationMinutes =
          Lude.Nothing,
        allocationStrategy = Lude.Nothing,
        timeoutDurationMinutes = pTimeoutDurationMinutes_,
        timeoutAction = pTimeoutAction_
      }

-- | The defined duration for Spot Instances (also known as Spot blocks) in minutes. When specified, the Spot Instance does not terminate before the defined duration expires, and defined duration pricing for Spot instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The duration period starts as soon as a Spot Instance receives its instance ID. At the end of the duration, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsBlockDurationMinutes :: Lens.Lens' SpotProvisioningSpecification (Lude.Maybe Lude.Natural)
spsBlockDurationMinutes = Lens.lens (blockDurationMinutes :: SpotProvisioningSpecification -> Lude.Maybe Lude.Natural) (\s a -> s {blockDurationMinutes = a} :: SpotProvisioningSpecification)
{-# DEPRECATED spsBlockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead." #-}

-- | Specifies the strategy to use in launching Spot Instance fleets. Currently, the only option is capacity-optimized (the default), which launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsAllocationStrategy :: Lens.Lens' SpotProvisioningSpecification (Lude.Maybe SpotProvisioningAllocationStrategy)
spsAllocationStrategy = Lens.lens (allocationStrategy :: SpotProvisioningSpecification -> Lude.Maybe SpotProvisioningAllocationStrategy) (\s a -> s {allocationStrategy = a} :: SpotProvisioningSpecification)
{-# DEPRECATED spsAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

-- | The spot provisioning timeout period in minutes. If Spot Instances are not provisioned within this time period, the @TimeOutAction@ is taken. Minimum value is 5 and maximum value is 1440. The timeout applies only during initial provisioning, when the cluster is first created.
--
-- /Note:/ Consider using 'timeoutDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsTimeoutDurationMinutes :: Lens.Lens' SpotProvisioningSpecification Lude.Natural
spsTimeoutDurationMinutes = Lens.lens (timeoutDurationMinutes :: SpotProvisioningSpecification -> Lude.Natural) (\s a -> s {timeoutDurationMinutes = a} :: SpotProvisioningSpecification)
{-# DEPRECATED spsTimeoutDurationMinutes "Use generic-lens or generic-optics with 'timeoutDurationMinutes' instead." #-}

-- | The action to take when @TargetSpotCapacity@ has not been fulfilled when the @TimeoutDurationMinutes@ has expired; that is, when all Spot Instances could not be provisioned within the Spot provisioning timeout. Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@ . SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available, On-Demand Instances should be provisioned to fulfill any remaining Spot capacity.
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsTimeoutAction :: Lens.Lens' SpotProvisioningSpecification SpotProvisioningTimeoutAction
spsTimeoutAction = Lens.lens (timeoutAction :: SpotProvisioningSpecification -> SpotProvisioningTimeoutAction) (\s a -> s {timeoutAction = a} :: SpotProvisioningSpecification)
{-# DEPRECATED spsTimeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead." #-}

instance Lude.FromJSON SpotProvisioningSpecification where
  parseJSON =
    Lude.withObject
      "SpotProvisioningSpecification"
      ( \x ->
          SpotProvisioningSpecification'
            Lude.<$> (x Lude..:? "BlockDurationMinutes")
            Lude.<*> (x Lude..:? "AllocationStrategy")
            Lude.<*> (x Lude..: "TimeoutDurationMinutes")
            Lude.<*> (x Lude..: "TimeoutAction")
      )

instance Lude.ToJSON SpotProvisioningSpecification where
  toJSON SpotProvisioningSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BlockDurationMinutes" Lude..=) Lude.<$> blockDurationMinutes,
            ("AllocationStrategy" Lude..=) Lude.<$> allocationStrategy,
            Lude.Just
              ("TimeoutDurationMinutes" Lude..= timeoutDurationMinutes),
            Lude.Just ("TimeoutAction" Lude..= timeoutAction)
          ]
      )
