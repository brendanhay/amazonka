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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TrafficRoutingConfig

-- | Currently, the @BlueGreenUpdatePolicy@ API is not supported.
--
-- /See:/ 'newBlueGreenUpdatePolicy' smart constructor.
data BlueGreenUpdatePolicy = BlueGreenUpdatePolicy'
  { terminationWaitInSeconds :: Prelude.Maybe Prelude.Natural,
    maximumExecutionTimeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    trafficRoutingConfiguration :: TrafficRoutingConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
          Prelude.Nothing,
        maximumExecutionTimeoutInSeconds = Prelude.Nothing,
        trafficRoutingConfiguration =
          pTrafficRoutingConfiguration_
      }

-- |
blueGreenUpdatePolicy_terminationWaitInSeconds :: Lens.Lens' BlueGreenUpdatePolicy (Prelude.Maybe Prelude.Natural)
blueGreenUpdatePolicy_terminationWaitInSeconds = Lens.lens (\BlueGreenUpdatePolicy' {terminationWaitInSeconds} -> terminationWaitInSeconds) (\s@BlueGreenUpdatePolicy' {} a -> s {terminationWaitInSeconds = a} :: BlueGreenUpdatePolicy)

-- |
blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds :: Lens.Lens' BlueGreenUpdatePolicy (Prelude.Maybe Prelude.Natural)
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
            Prelude.<$> (x Core..:? "TerminationWaitInSeconds")
            Prelude.<*> (x Core..:? "MaximumExecutionTimeoutInSeconds")
            Prelude.<*> (x Core..: "TrafficRoutingConfiguration")
      )

instance Prelude.Hashable BlueGreenUpdatePolicy

instance Prelude.NFData BlueGreenUpdatePolicy

instance Core.ToJSON BlueGreenUpdatePolicy where
  toJSON BlueGreenUpdatePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TerminationWaitInSeconds" Core..=)
              Prelude.<$> terminationWaitInSeconds,
            ("MaximumExecutionTimeoutInSeconds" Core..=)
              Prelude.<$> maximumExecutionTimeoutInSeconds,
            Prelude.Just
              ( "TrafficRoutingConfiguration"
                  Core..= trafficRoutingConfiguration
              )
          ]
      )
