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
-- Module      : Amazonka.IoT.Types.JobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobExecutionsRolloutConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ExponentialRolloutRate
import qualified Amazonka.Prelude as Prelude

-- | Allows you to create a staged rollout of a job.
--
-- /See:/ 'newJobExecutionsRolloutConfig' smart constructor.
data JobExecutionsRolloutConfig = JobExecutionsRolloutConfig'
  { -- | The rate of increase for a job rollout. This parameter allows you to
    -- define an exponential rate for a job rollout.
    exponentialRate :: Prelude.Maybe ExponentialRolloutRate,
    -- | The maximum number of things that will be notified of a pending job, per
    -- minute. This parameter allows you to create a staged rollout.
    maximumPerMinute :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionsRolloutConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exponentialRate', 'jobExecutionsRolloutConfig_exponentialRate' - The rate of increase for a job rollout. This parameter allows you to
-- define an exponential rate for a job rollout.
--
-- 'maximumPerMinute', 'jobExecutionsRolloutConfig_maximumPerMinute' - The maximum number of things that will be notified of a pending job, per
-- minute. This parameter allows you to create a staged rollout.
newJobExecutionsRolloutConfig ::
  JobExecutionsRolloutConfig
newJobExecutionsRolloutConfig =
  JobExecutionsRolloutConfig'
    { exponentialRate =
        Prelude.Nothing,
      maximumPerMinute = Prelude.Nothing
    }

-- | The rate of increase for a job rollout. This parameter allows you to
-- define an exponential rate for a job rollout.
jobExecutionsRolloutConfig_exponentialRate :: Lens.Lens' JobExecutionsRolloutConfig (Prelude.Maybe ExponentialRolloutRate)
jobExecutionsRolloutConfig_exponentialRate = Lens.lens (\JobExecutionsRolloutConfig' {exponentialRate} -> exponentialRate) (\s@JobExecutionsRolloutConfig' {} a -> s {exponentialRate = a} :: JobExecutionsRolloutConfig)

-- | The maximum number of things that will be notified of a pending job, per
-- minute. This parameter allows you to create a staged rollout.
jobExecutionsRolloutConfig_maximumPerMinute :: Lens.Lens' JobExecutionsRolloutConfig (Prelude.Maybe Prelude.Natural)
jobExecutionsRolloutConfig_maximumPerMinute = Lens.lens (\JobExecutionsRolloutConfig' {maximumPerMinute} -> maximumPerMinute) (\s@JobExecutionsRolloutConfig' {} a -> s {maximumPerMinute = a} :: JobExecutionsRolloutConfig)

instance Data.FromJSON JobExecutionsRolloutConfig where
  parseJSON =
    Data.withObject
      "JobExecutionsRolloutConfig"
      ( \x ->
          JobExecutionsRolloutConfig'
            Prelude.<$> (x Data..:? "exponentialRate")
            Prelude.<*> (x Data..:? "maximumPerMinute")
      )

instance Prelude.Hashable JobExecutionsRolloutConfig where
  hashWithSalt _salt JobExecutionsRolloutConfig' {..} =
    _salt `Prelude.hashWithSalt` exponentialRate
      `Prelude.hashWithSalt` maximumPerMinute

instance Prelude.NFData JobExecutionsRolloutConfig where
  rnf JobExecutionsRolloutConfig' {..} =
    Prelude.rnf exponentialRate
      `Prelude.seq` Prelude.rnf maximumPerMinute

instance Data.ToJSON JobExecutionsRolloutConfig where
  toJSON JobExecutionsRolloutConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exponentialRate" Data..=)
              Prelude.<$> exponentialRate,
            ("maximumPerMinute" Data..=)
              Prelude.<$> maximumPerMinute
          ]
      )
