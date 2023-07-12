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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.IoTJobExecutionsRolloutConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.IoTJobExponentialRolloutRate
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the rollout configuration for a job. This
-- configuration defines the rate at which the job deploys a configuration
-- to a fleet of target devices.
--
-- /See:/ 'newIoTJobExecutionsRolloutConfig' smart constructor.
data IoTJobExecutionsRolloutConfig = IoTJobExecutionsRolloutConfig'
  { -- | The exponential rate to increase the job rollout rate.
    exponentialRate :: Prelude.Maybe IoTJobExponentialRolloutRate,
    -- | The maximum number of devices that receive a pending job notification,
    -- per minute.
    maximumPerMinute :: Prelude.Maybe Prelude.Natural
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
-- 'exponentialRate', 'ioTJobExecutionsRolloutConfig_exponentialRate' - The exponential rate to increase the job rollout rate.
--
-- 'maximumPerMinute', 'ioTJobExecutionsRolloutConfig_maximumPerMinute' - The maximum number of devices that receive a pending job notification,
-- per minute.
newIoTJobExecutionsRolloutConfig ::
  IoTJobExecutionsRolloutConfig
newIoTJobExecutionsRolloutConfig =
  IoTJobExecutionsRolloutConfig'
    { exponentialRate =
        Prelude.Nothing,
      maximumPerMinute = Prelude.Nothing
    }

-- | The exponential rate to increase the job rollout rate.
ioTJobExecutionsRolloutConfig_exponentialRate :: Lens.Lens' IoTJobExecutionsRolloutConfig (Prelude.Maybe IoTJobExponentialRolloutRate)
ioTJobExecutionsRolloutConfig_exponentialRate = Lens.lens (\IoTJobExecutionsRolloutConfig' {exponentialRate} -> exponentialRate) (\s@IoTJobExecutionsRolloutConfig' {} a -> s {exponentialRate = a} :: IoTJobExecutionsRolloutConfig)

-- | The maximum number of devices that receive a pending job notification,
-- per minute.
ioTJobExecutionsRolloutConfig_maximumPerMinute :: Lens.Lens' IoTJobExecutionsRolloutConfig (Prelude.Maybe Prelude.Natural)
ioTJobExecutionsRolloutConfig_maximumPerMinute = Lens.lens (\IoTJobExecutionsRolloutConfig' {maximumPerMinute} -> maximumPerMinute) (\s@IoTJobExecutionsRolloutConfig' {} a -> s {maximumPerMinute = a} :: IoTJobExecutionsRolloutConfig)

instance Data.FromJSON IoTJobExecutionsRolloutConfig where
  parseJSON =
    Data.withObject
      "IoTJobExecutionsRolloutConfig"
      ( \x ->
          IoTJobExecutionsRolloutConfig'
            Prelude.<$> (x Data..:? "exponentialRate")
            Prelude.<*> (x Data..:? "maximumPerMinute")
      )

instance
  Prelude.Hashable
    IoTJobExecutionsRolloutConfig
  where
  hashWithSalt _salt IoTJobExecutionsRolloutConfig' {..} =
    _salt
      `Prelude.hashWithSalt` exponentialRate
      `Prelude.hashWithSalt` maximumPerMinute

instance Prelude.NFData IoTJobExecutionsRolloutConfig where
  rnf IoTJobExecutionsRolloutConfig' {..} =
    Prelude.rnf exponentialRate
      `Prelude.seq` Prelude.rnf maximumPerMinute

instance Data.ToJSON IoTJobExecutionsRolloutConfig where
  toJSON IoTJobExecutionsRolloutConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exponentialRate" Data..=)
              Prelude.<$> exponentialRate,
            ("maximumPerMinute" Data..=)
              Prelude.<$> maximumPerMinute
          ]
      )
