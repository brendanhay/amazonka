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
-- Module      : Network.AWS.SageMaker.Types.MonitoringStoppingCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringStoppingCondition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A time limit for how long the monitoring job is allowed to run before
-- stopping.
--
-- /See:/ 'newMonitoringStoppingCondition' smart constructor.
data MonitoringStoppingCondition = MonitoringStoppingCondition'
  { -- | The maximum runtime allowed in seconds.
    maxRuntimeInSeconds :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MonitoringStoppingCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRuntimeInSeconds', 'monitoringStoppingCondition_maxRuntimeInSeconds' - The maximum runtime allowed in seconds.
newMonitoringStoppingCondition ::
  -- | 'maxRuntimeInSeconds'
  Core.Natural ->
  MonitoringStoppingCondition
newMonitoringStoppingCondition pMaxRuntimeInSeconds_ =
  MonitoringStoppingCondition'
    { maxRuntimeInSeconds =
        pMaxRuntimeInSeconds_
    }

-- | The maximum runtime allowed in seconds.
monitoringStoppingCondition_maxRuntimeInSeconds :: Lens.Lens' MonitoringStoppingCondition Core.Natural
monitoringStoppingCondition_maxRuntimeInSeconds = Lens.lens (\MonitoringStoppingCondition' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@MonitoringStoppingCondition' {} a -> s {maxRuntimeInSeconds = a} :: MonitoringStoppingCondition)

instance Core.FromJSON MonitoringStoppingCondition where
  parseJSON =
    Core.withObject
      "MonitoringStoppingCondition"
      ( \x ->
          MonitoringStoppingCondition'
            Core.<$> (x Core..: "MaxRuntimeInSeconds")
      )

instance Core.Hashable MonitoringStoppingCondition

instance Core.NFData MonitoringStoppingCondition

instance Core.ToJSON MonitoringStoppingCondition where
  toJSON MonitoringStoppingCondition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("MaxRuntimeInSeconds" Core..= maxRuntimeInSeconds)
          ]
      )
