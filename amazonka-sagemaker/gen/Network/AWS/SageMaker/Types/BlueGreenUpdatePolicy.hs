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
-- Module      : Network.AWS.SageMaker.Types.BlueGreenUpdatePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.BlueGreenUpdatePolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.TrafficRoutingConfig

-- | Currently, the @BlueGreenUpdatePolicy@ API is not supported.
--
-- /See:/ 'newBlueGreenUpdatePolicy' smart constructor.
data BlueGreenUpdatePolicy = BlueGreenUpdatePolicy'
  { terminationWaitInSeconds :: Core.Maybe Core.Natural,
    maximumExecutionTimeoutInSeconds :: Core.Maybe Core.Natural,
    trafficRoutingConfiguration :: TrafficRoutingConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BlueGreenUpdatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminationWaitInSeconds', 'blueGreenUpdatePolicy_terminationWaitInSeconds' -
--
-- 'maximumExecutionTimeoutInSeconds', 'blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds' -
--
-- 'trafficRoutingConfiguration', 'blueGreenUpdatePolicy_trafficRoutingConfiguration' -
newBlueGreenUpdatePolicy ::
  -- | 'trafficRoutingConfiguration'
  TrafficRoutingConfig ->
  BlueGreenUpdatePolicy
newBlueGreenUpdatePolicy
  pTrafficRoutingConfiguration_ =
    BlueGreenUpdatePolicy'
      { terminationWaitInSeconds =
          Core.Nothing,
        maximumExecutionTimeoutInSeconds = Core.Nothing,
        trafficRoutingConfiguration =
          pTrafficRoutingConfiguration_
      }

-- |
blueGreenUpdatePolicy_terminationWaitInSeconds :: Lens.Lens' BlueGreenUpdatePolicy (Core.Maybe Core.Natural)
blueGreenUpdatePolicy_terminationWaitInSeconds = Lens.lens (\BlueGreenUpdatePolicy' {terminationWaitInSeconds} -> terminationWaitInSeconds) (\s@BlueGreenUpdatePolicy' {} a -> s {terminationWaitInSeconds = a} :: BlueGreenUpdatePolicy)

-- |
blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds :: Lens.Lens' BlueGreenUpdatePolicy (Core.Maybe Core.Natural)
blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds = Lens.lens (\BlueGreenUpdatePolicy' {maximumExecutionTimeoutInSeconds} -> maximumExecutionTimeoutInSeconds) (\s@BlueGreenUpdatePolicy' {} a -> s {maximumExecutionTimeoutInSeconds = a} :: BlueGreenUpdatePolicy)

-- |
blueGreenUpdatePolicy_trafficRoutingConfiguration :: Lens.Lens' BlueGreenUpdatePolicy TrafficRoutingConfig
blueGreenUpdatePolicy_trafficRoutingConfiguration = Lens.lens (\BlueGreenUpdatePolicy' {trafficRoutingConfiguration} -> trafficRoutingConfiguration) (\s@BlueGreenUpdatePolicy' {} a -> s {trafficRoutingConfiguration = a} :: BlueGreenUpdatePolicy)

instance Core.FromJSON BlueGreenUpdatePolicy where
  parseJSON =
    Core.withObject
      "BlueGreenUpdatePolicy"
      ( \x ->
          BlueGreenUpdatePolicy'
            Core.<$> (x Core..:? "TerminationWaitInSeconds")
            Core.<*> (x Core..:? "MaximumExecutionTimeoutInSeconds")
            Core.<*> (x Core..: "TrafficRoutingConfiguration")
      )

instance Core.Hashable BlueGreenUpdatePolicy

instance Core.NFData BlueGreenUpdatePolicy

instance Core.ToJSON BlueGreenUpdatePolicy where
  toJSON BlueGreenUpdatePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TerminationWaitInSeconds" Core..=)
              Core.<$> terminationWaitInSeconds,
            ("MaximumExecutionTimeoutInSeconds" Core..=)
              Core.<$> maximumExecutionTimeoutInSeconds,
            Core.Just
              ( "TrafficRoutingConfiguration"
                  Core..= trafficRoutingConfiguration
              )
          ]
      )
