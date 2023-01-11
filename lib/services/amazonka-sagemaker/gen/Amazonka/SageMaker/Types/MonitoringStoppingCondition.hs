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
-- Module      : Amazonka.SageMaker.Types.MonitoringStoppingCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringStoppingCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A time limit for how long the monitoring job is allowed to run before
-- stopping.
--
-- /See:/ 'newMonitoringStoppingCondition' smart constructor.
data MonitoringStoppingCondition = MonitoringStoppingCondition'
  { -- | The maximum runtime allowed in seconds.
    --
    -- The @MaxRuntimeInSeconds@ cannot exceed the frequency of the job. For
    -- data quality and model explainability, this can be up to 3600 seconds
    -- for an hourly schedule. For model bias and model quality hourly
    -- schedules, this can be up to 1800 seconds.
    maxRuntimeInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringStoppingCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRuntimeInSeconds', 'monitoringStoppingCondition_maxRuntimeInSeconds' - The maximum runtime allowed in seconds.
--
-- The @MaxRuntimeInSeconds@ cannot exceed the frequency of the job. For
-- data quality and model explainability, this can be up to 3600 seconds
-- for an hourly schedule. For model bias and model quality hourly
-- schedules, this can be up to 1800 seconds.
newMonitoringStoppingCondition ::
  -- | 'maxRuntimeInSeconds'
  Prelude.Natural ->
  MonitoringStoppingCondition
newMonitoringStoppingCondition pMaxRuntimeInSeconds_ =
  MonitoringStoppingCondition'
    { maxRuntimeInSeconds =
        pMaxRuntimeInSeconds_
    }

-- | The maximum runtime allowed in seconds.
--
-- The @MaxRuntimeInSeconds@ cannot exceed the frequency of the job. For
-- data quality and model explainability, this can be up to 3600 seconds
-- for an hourly schedule. For model bias and model quality hourly
-- schedules, this can be up to 1800 seconds.
monitoringStoppingCondition_maxRuntimeInSeconds :: Lens.Lens' MonitoringStoppingCondition Prelude.Natural
monitoringStoppingCondition_maxRuntimeInSeconds = Lens.lens (\MonitoringStoppingCondition' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@MonitoringStoppingCondition' {} a -> s {maxRuntimeInSeconds = a} :: MonitoringStoppingCondition)

instance Data.FromJSON MonitoringStoppingCondition where
  parseJSON =
    Data.withObject
      "MonitoringStoppingCondition"
      ( \x ->
          MonitoringStoppingCondition'
            Prelude.<$> (x Data..: "MaxRuntimeInSeconds")
      )

instance Prelude.Hashable MonitoringStoppingCondition where
  hashWithSalt _salt MonitoringStoppingCondition' {..} =
    _salt `Prelude.hashWithSalt` maxRuntimeInSeconds

instance Prelude.NFData MonitoringStoppingCondition where
  rnf MonitoringStoppingCondition' {..} =
    Prelude.rnf maxRuntimeInSeconds

instance Data.ToJSON MonitoringStoppingCondition where
  toJSON MonitoringStoppingCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MaxRuntimeInSeconds" Data..= maxRuntimeInSeconds)
          ]
      )
