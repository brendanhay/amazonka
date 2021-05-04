{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A time limit for how long the monitoring job is allowed to run before
-- stopping.
--
-- /See:/ 'newMonitoringStoppingCondition' smart constructor.
data MonitoringStoppingCondition = MonitoringStoppingCondition'
  { -- | The maximum runtime allowed in seconds.
    maxRuntimeInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Natural ->
  MonitoringStoppingCondition
newMonitoringStoppingCondition pMaxRuntimeInSeconds_ =
  MonitoringStoppingCondition'
    { maxRuntimeInSeconds =
        pMaxRuntimeInSeconds_
    }

-- | The maximum runtime allowed in seconds.
monitoringStoppingCondition_maxRuntimeInSeconds :: Lens.Lens' MonitoringStoppingCondition Prelude.Natural
monitoringStoppingCondition_maxRuntimeInSeconds = Lens.lens (\MonitoringStoppingCondition' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@MonitoringStoppingCondition' {} a -> s {maxRuntimeInSeconds = a} :: MonitoringStoppingCondition)

instance Prelude.FromJSON MonitoringStoppingCondition where
  parseJSON =
    Prelude.withObject
      "MonitoringStoppingCondition"
      ( \x ->
          MonitoringStoppingCondition'
            Prelude.<$> (x Prelude..: "MaxRuntimeInSeconds")
      )

instance Prelude.Hashable MonitoringStoppingCondition

instance Prelude.NFData MonitoringStoppingCondition

instance Prelude.ToJSON MonitoringStoppingCondition where
  toJSON MonitoringStoppingCondition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MaxRuntimeInSeconds"
                  Prelude..= maxRuntimeInSeconds
              )
          ]
      )
