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
-- Module      : Amazonka.GreengrassV2.Types.IoTJobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.IoTJobExecutionsRolloutConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types.IoTJobExponentialRolloutRate
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the rollout configuration for a job. This
-- configuration defines the rate at which the job deploys a configuration
-- to a fleet of target devices.
--
-- /See:/ 'newIoTJobExecutionsRolloutConfig' smart constructor.
data IoTJobExecutionsRolloutConfig = IoTJobExecutionsRolloutConfig'
  { -- | The maximum number of devices that receive a pending job notification,
    -- per minute.
    maximumPerMinute :: Prelude.Maybe Prelude.Natural,
    -- | The exponential rate to increase the job rollout rate.
    exponentialRate :: Prelude.Maybe IoTJobExponentialRolloutRate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IoTJobExecutionsRolloutConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumPerMinute', 'ioTJobExecutionsRolloutConfig_maximumPerMinute' - The maximum number of devices that receive a pending job notification,
-- per minute.
--
-- 'exponentialRate', 'ioTJobExecutionsRolloutConfig_exponentialRate' - The exponential rate to increase the job rollout rate.
newIoTJobExecutionsRolloutConfig ::
  IoTJobExecutionsRolloutConfig
newIoTJobExecutionsRolloutConfig =
  IoTJobExecutionsRolloutConfig'
    { maximumPerMinute =
        Prelude.Nothing,
      exponentialRate = Prelude.Nothing
    }

-- | The maximum number of devices that receive a pending job notification,
-- per minute.
ioTJobExecutionsRolloutConfig_maximumPerMinute :: Lens.Lens' IoTJobExecutionsRolloutConfig (Prelude.Maybe Prelude.Natural)
ioTJobExecutionsRolloutConfig_maximumPerMinute = Lens.lens (\IoTJobExecutionsRolloutConfig' {maximumPerMinute} -> maximumPerMinute) (\s@IoTJobExecutionsRolloutConfig' {} a -> s {maximumPerMinute = a} :: IoTJobExecutionsRolloutConfig)

-- | The exponential rate to increase the job rollout rate.
ioTJobExecutionsRolloutConfig_exponentialRate :: Lens.Lens' IoTJobExecutionsRolloutConfig (Prelude.Maybe IoTJobExponentialRolloutRate)
ioTJobExecutionsRolloutConfig_exponentialRate = Lens.lens (\IoTJobExecutionsRolloutConfig' {exponentialRate} -> exponentialRate) (\s@IoTJobExecutionsRolloutConfig' {} a -> s {exponentialRate = a} :: IoTJobExecutionsRolloutConfig)

instance Core.FromJSON IoTJobExecutionsRolloutConfig where
  parseJSON =
    Core.withObject
      "IoTJobExecutionsRolloutConfig"
      ( \x ->
          IoTJobExecutionsRolloutConfig'
            Prelude.<$> (x Core..:? "maximumPerMinute")
            Prelude.<*> (x Core..:? "exponentialRate")
      )

instance
  Prelude.Hashable
    IoTJobExecutionsRolloutConfig
  where
  hashWithSalt _salt IoTJobExecutionsRolloutConfig' {..} =
    _salt `Prelude.hashWithSalt` maximumPerMinute
      `Prelude.hashWithSalt` exponentialRate

instance Prelude.NFData IoTJobExecutionsRolloutConfig where
  rnf IoTJobExecutionsRolloutConfig' {..} =
    Prelude.rnf maximumPerMinute
      `Prelude.seq` Prelude.rnf exponentialRate

instance Core.ToJSON IoTJobExecutionsRolloutConfig where
  toJSON IoTJobExecutionsRolloutConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maximumPerMinute" Core..=)
              Prelude.<$> maximumPerMinute,
            ("exponentialRate" Core..=)
              Prelude.<$> exponentialRate
          ]
      )
