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
-- Module      : Amazonka.SageMaker.Types.TrafficRoutingConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrafficRoutingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CapacitySize
import Amazonka.SageMaker.Types.TrafficRoutingConfigType

-- | Defines the traffic routing strategy during an endpoint deployment to
-- shift traffic from the old fleet to the new fleet.
--
-- /See:/ 'newTrafficRoutingConfig' smart constructor.
data TrafficRoutingConfig = TrafficRoutingConfig'
  { -- | Batch size for each step to turn on traffic on the new endpoint fleet.
    -- @Value@ must be 10-50% of the variant\'s total instance count.
    linearStepSize :: Prelude.Maybe CapacitySize,
    -- | Batch size for the first step to turn on traffic on the new endpoint
    -- fleet. @Value@ must be less than or equal to 50% of the variant\'s total
    -- instance count.
    canarySize :: Prelude.Maybe CapacitySize,
    -- | Traffic routing strategy type.
    --
    -- -   @ALL_AT_ONCE@: Endpoint traffic shifts to the new fleet in a single
    --     step.
    --
    -- -   @CANARY@: Endpoint traffic shifts to the new fleet in two steps. The
    --     first step is the canary, which is a small portion of the traffic.
    --     The second step is the remainder of the traffic.
    --
    -- -   @LINEAR@: Endpoint traffic shifts to the new fleet in n steps of a
    --     configurable size.
    type' :: TrafficRoutingConfigType,
    -- | The waiting time (in seconds) between incremental steps to turn on
    -- traffic on the new endpoint fleet.
    waitIntervalInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficRoutingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linearStepSize', 'trafficRoutingConfig_linearStepSize' - Batch size for each step to turn on traffic on the new endpoint fleet.
-- @Value@ must be 10-50% of the variant\'s total instance count.
--
-- 'canarySize', 'trafficRoutingConfig_canarySize' - Batch size for the first step to turn on traffic on the new endpoint
-- fleet. @Value@ must be less than or equal to 50% of the variant\'s total
-- instance count.
--
-- 'type'', 'trafficRoutingConfig_type' - Traffic routing strategy type.
--
-- -   @ALL_AT_ONCE@: Endpoint traffic shifts to the new fleet in a single
--     step.
--
-- -   @CANARY@: Endpoint traffic shifts to the new fleet in two steps. The
--     first step is the canary, which is a small portion of the traffic.
--     The second step is the remainder of the traffic.
--
-- -   @LINEAR@: Endpoint traffic shifts to the new fleet in n steps of a
--     configurable size.
--
-- 'waitIntervalInSeconds', 'trafficRoutingConfig_waitIntervalInSeconds' - The waiting time (in seconds) between incremental steps to turn on
-- traffic on the new endpoint fleet.
newTrafficRoutingConfig ::
  -- | 'type''
  TrafficRoutingConfigType ->
  -- | 'waitIntervalInSeconds'
  Prelude.Natural ->
  TrafficRoutingConfig
newTrafficRoutingConfig
  pType_
  pWaitIntervalInSeconds_ =
    TrafficRoutingConfig'
      { linearStepSize =
          Prelude.Nothing,
        canarySize = Prelude.Nothing,
        type' = pType_,
        waitIntervalInSeconds = pWaitIntervalInSeconds_
      }

-- | Batch size for each step to turn on traffic on the new endpoint fleet.
-- @Value@ must be 10-50% of the variant\'s total instance count.
trafficRoutingConfig_linearStepSize :: Lens.Lens' TrafficRoutingConfig (Prelude.Maybe CapacitySize)
trafficRoutingConfig_linearStepSize = Lens.lens (\TrafficRoutingConfig' {linearStepSize} -> linearStepSize) (\s@TrafficRoutingConfig' {} a -> s {linearStepSize = a} :: TrafficRoutingConfig)

-- | Batch size for the first step to turn on traffic on the new endpoint
-- fleet. @Value@ must be less than or equal to 50% of the variant\'s total
-- instance count.
trafficRoutingConfig_canarySize :: Lens.Lens' TrafficRoutingConfig (Prelude.Maybe CapacitySize)
trafficRoutingConfig_canarySize = Lens.lens (\TrafficRoutingConfig' {canarySize} -> canarySize) (\s@TrafficRoutingConfig' {} a -> s {canarySize = a} :: TrafficRoutingConfig)

-- | Traffic routing strategy type.
--
-- -   @ALL_AT_ONCE@: Endpoint traffic shifts to the new fleet in a single
--     step.
--
-- -   @CANARY@: Endpoint traffic shifts to the new fleet in two steps. The
--     first step is the canary, which is a small portion of the traffic.
--     The second step is the remainder of the traffic.
--
-- -   @LINEAR@: Endpoint traffic shifts to the new fleet in n steps of a
--     configurable size.
trafficRoutingConfig_type :: Lens.Lens' TrafficRoutingConfig TrafficRoutingConfigType
trafficRoutingConfig_type = Lens.lens (\TrafficRoutingConfig' {type'} -> type') (\s@TrafficRoutingConfig' {} a -> s {type' = a} :: TrafficRoutingConfig)

-- | The waiting time (in seconds) between incremental steps to turn on
-- traffic on the new endpoint fleet.
trafficRoutingConfig_waitIntervalInSeconds :: Lens.Lens' TrafficRoutingConfig Prelude.Natural
trafficRoutingConfig_waitIntervalInSeconds = Lens.lens (\TrafficRoutingConfig' {waitIntervalInSeconds} -> waitIntervalInSeconds) (\s@TrafficRoutingConfig' {} a -> s {waitIntervalInSeconds = a} :: TrafficRoutingConfig)

instance Core.FromJSON TrafficRoutingConfig where
  parseJSON =
    Core.withObject
      "TrafficRoutingConfig"
      ( \x ->
          TrafficRoutingConfig'
            Prelude.<$> (x Core..:? "LinearStepSize")
            Prelude.<*> (x Core..:? "CanarySize")
            Prelude.<*> (x Core..: "Type")
            Prelude.<*> (x Core..: "WaitIntervalInSeconds")
      )

instance Prelude.Hashable TrafficRoutingConfig where
  hashWithSalt _salt TrafficRoutingConfig' {..} =
    _salt `Prelude.hashWithSalt` linearStepSize
      `Prelude.hashWithSalt` canarySize
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` waitIntervalInSeconds

instance Prelude.NFData TrafficRoutingConfig where
  rnf TrafficRoutingConfig' {..} =
    Prelude.rnf linearStepSize
      `Prelude.seq` Prelude.rnf canarySize
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf waitIntervalInSeconds

instance Core.ToJSON TrafficRoutingConfig where
  toJSON TrafficRoutingConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LinearStepSize" Core..=)
              Prelude.<$> linearStepSize,
            ("CanarySize" Core..=) Prelude.<$> canarySize,
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just
              ( "WaitIntervalInSeconds"
                  Core..= waitIntervalInSeconds
              )
          ]
      )
