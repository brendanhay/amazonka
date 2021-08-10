{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SpotProvisioningSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SpotProvisioningSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.SpotProvisioningAllocationStrategy
import Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The launch specification for Spot Instances in the instance fleet, which
-- determines the defined duration, provisioning timeout behavior, and
-- allocation strategy.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions. Spot Instance
-- allocation strategy is available in Amazon EMR version 5.12.1 and later.
--
-- /See:/ 'newSpotProvisioningSpecification' smart constructor.
data SpotProvisioningSpecification = SpotProvisioningSpecification'
  { -- | The defined duration for Spot Instances (also known as Spot blocks) in
    -- minutes. When specified, the Spot Instance does not terminate before the
    -- defined duration expires, and defined duration pricing for Spot
    -- Instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The
    -- duration period starts as soon as a Spot Instance receives its instance
    -- ID. At the end of the duration, Amazon EC2 marks the Spot Instance for
    -- termination and provides a Spot Instance termination notice, which gives
    -- the instance a two-minute warning before it terminates.
    blockDurationMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the strategy to use in launching Spot Instance fleets.
    -- Currently, the only option is capacity-optimized (the default), which
    -- launches instances from Spot Instance pools with optimal capacity for
    -- the number of instances that are launching.
    allocationStrategy :: Prelude.Maybe SpotProvisioningAllocationStrategy,
    -- | The spot provisioning timeout period in minutes. If Spot Instances are
    -- not provisioned within this time period, the @TimeOutAction@ is taken.
    -- Minimum value is 5 and maximum value is 1440. The timeout applies only
    -- during initial provisioning, when the cluster is first created.
    timeoutDurationMinutes :: Prelude.Natural,
    -- | The action to take when @TargetSpotCapacity@ has not been fulfilled when
    -- the @TimeoutDurationMinutes@ has expired; that is, when all Spot
    -- Instances could not be provisioned within the Spot provisioning timeout.
    -- Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@.
    -- SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available,
    -- On-Demand Instances should be provisioned to fulfill any remaining Spot
    -- capacity.
    timeoutAction :: SpotProvisioningTimeoutAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotProvisioningSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDurationMinutes', 'spotProvisioningSpecification_blockDurationMinutes' - The defined duration for Spot Instances (also known as Spot blocks) in
-- minutes. When specified, the Spot Instance does not terminate before the
-- defined duration expires, and defined duration pricing for Spot
-- Instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The
-- duration period starts as soon as a Spot Instance receives its instance
-- ID. At the end of the duration, Amazon EC2 marks the Spot Instance for
-- termination and provides a Spot Instance termination notice, which gives
-- the instance a two-minute warning before it terminates.
--
-- 'allocationStrategy', 'spotProvisioningSpecification_allocationStrategy' - Specifies the strategy to use in launching Spot Instance fleets.
-- Currently, the only option is capacity-optimized (the default), which
-- launches instances from Spot Instance pools with optimal capacity for
-- the number of instances that are launching.
--
-- 'timeoutDurationMinutes', 'spotProvisioningSpecification_timeoutDurationMinutes' - The spot provisioning timeout period in minutes. If Spot Instances are
-- not provisioned within this time period, the @TimeOutAction@ is taken.
-- Minimum value is 5 and maximum value is 1440. The timeout applies only
-- during initial provisioning, when the cluster is first created.
--
-- 'timeoutAction', 'spotProvisioningSpecification_timeoutAction' - The action to take when @TargetSpotCapacity@ has not been fulfilled when
-- the @TimeoutDurationMinutes@ has expired; that is, when all Spot
-- Instances could not be provisioned within the Spot provisioning timeout.
-- Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@.
-- SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available,
-- On-Demand Instances should be provisioned to fulfill any remaining Spot
-- capacity.
newSpotProvisioningSpecification ::
  -- | 'timeoutDurationMinutes'
  Prelude.Natural ->
  -- | 'timeoutAction'
  SpotProvisioningTimeoutAction ->
  SpotProvisioningSpecification
newSpotProvisioningSpecification
  pTimeoutDurationMinutes_
  pTimeoutAction_ =
    SpotProvisioningSpecification'
      { blockDurationMinutes =
          Prelude.Nothing,
        allocationStrategy = Prelude.Nothing,
        timeoutDurationMinutes =
          pTimeoutDurationMinutes_,
        timeoutAction = pTimeoutAction_
      }

-- | The defined duration for Spot Instances (also known as Spot blocks) in
-- minutes. When specified, the Spot Instance does not terminate before the
-- defined duration expires, and defined duration pricing for Spot
-- Instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The
-- duration period starts as soon as a Spot Instance receives its instance
-- ID. At the end of the duration, Amazon EC2 marks the Spot Instance for
-- termination and provides a Spot Instance termination notice, which gives
-- the instance a two-minute warning before it terminates.
spotProvisioningSpecification_blockDurationMinutes :: Lens.Lens' SpotProvisioningSpecification (Prelude.Maybe Prelude.Natural)
spotProvisioningSpecification_blockDurationMinutes = Lens.lens (\SpotProvisioningSpecification' {blockDurationMinutes} -> blockDurationMinutes) (\s@SpotProvisioningSpecification' {} a -> s {blockDurationMinutes = a} :: SpotProvisioningSpecification)

-- | Specifies the strategy to use in launching Spot Instance fleets.
-- Currently, the only option is capacity-optimized (the default), which
-- launches instances from Spot Instance pools with optimal capacity for
-- the number of instances that are launching.
spotProvisioningSpecification_allocationStrategy :: Lens.Lens' SpotProvisioningSpecification (Prelude.Maybe SpotProvisioningAllocationStrategy)
spotProvisioningSpecification_allocationStrategy = Lens.lens (\SpotProvisioningSpecification' {allocationStrategy} -> allocationStrategy) (\s@SpotProvisioningSpecification' {} a -> s {allocationStrategy = a} :: SpotProvisioningSpecification)

-- | The spot provisioning timeout period in minutes. If Spot Instances are
-- not provisioned within this time period, the @TimeOutAction@ is taken.
-- Minimum value is 5 and maximum value is 1440. The timeout applies only
-- during initial provisioning, when the cluster is first created.
spotProvisioningSpecification_timeoutDurationMinutes :: Lens.Lens' SpotProvisioningSpecification Prelude.Natural
spotProvisioningSpecification_timeoutDurationMinutes = Lens.lens (\SpotProvisioningSpecification' {timeoutDurationMinutes} -> timeoutDurationMinutes) (\s@SpotProvisioningSpecification' {} a -> s {timeoutDurationMinutes = a} :: SpotProvisioningSpecification)

-- | The action to take when @TargetSpotCapacity@ has not been fulfilled when
-- the @TimeoutDurationMinutes@ has expired; that is, when all Spot
-- Instances could not be provisioned within the Spot provisioning timeout.
-- Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@.
-- SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available,
-- On-Demand Instances should be provisioned to fulfill any remaining Spot
-- capacity.
spotProvisioningSpecification_timeoutAction :: Lens.Lens' SpotProvisioningSpecification SpotProvisioningTimeoutAction
spotProvisioningSpecification_timeoutAction = Lens.lens (\SpotProvisioningSpecification' {timeoutAction} -> timeoutAction) (\s@SpotProvisioningSpecification' {} a -> s {timeoutAction = a} :: SpotProvisioningSpecification)

instance Core.FromJSON SpotProvisioningSpecification where
  parseJSON =
    Core.withObject
      "SpotProvisioningSpecification"
      ( \x ->
          SpotProvisioningSpecification'
            Prelude.<$> (x Core..:? "BlockDurationMinutes")
            Prelude.<*> (x Core..:? "AllocationStrategy")
            Prelude.<*> (x Core..: "TimeoutDurationMinutes")
            Prelude.<*> (x Core..: "TimeoutAction")
      )

instance
  Prelude.Hashable
    SpotProvisioningSpecification

instance Prelude.NFData SpotProvisioningSpecification

instance Core.ToJSON SpotProvisioningSpecification where
  toJSON SpotProvisioningSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BlockDurationMinutes" Core..=)
              Prelude.<$> blockDurationMinutes,
            ("AllocationStrategy" Core..=)
              Prelude.<$> allocationStrategy,
            Prelude.Just
              ( "TimeoutDurationMinutes"
                  Core..= timeoutDurationMinutes
              ),
            Prelude.Just
              ("TimeoutAction" Core..= timeoutAction)
          ]
      )
