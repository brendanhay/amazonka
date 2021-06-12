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
-- Module      : Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule

-- | Describes an instance\'s time-based auto scaling configuration.
--
-- /See:/ 'newTimeBasedAutoScalingConfiguration' smart constructor.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration'
  { -- | The instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
    autoScalingSchedule :: Core.Maybe WeeklyAutoScalingSchedule
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimeBasedAutoScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'timeBasedAutoScalingConfiguration_instanceId' - The instance ID.
--
-- 'autoScalingSchedule', 'timeBasedAutoScalingConfiguration_autoScalingSchedule' - A @WeeklyAutoScalingSchedule@ object with the instance schedule.
newTimeBasedAutoScalingConfiguration ::
  TimeBasedAutoScalingConfiguration
newTimeBasedAutoScalingConfiguration =
  TimeBasedAutoScalingConfiguration'
    { instanceId =
        Core.Nothing,
      autoScalingSchedule = Core.Nothing
    }

-- | The instance ID.
timeBasedAutoScalingConfiguration_instanceId :: Lens.Lens' TimeBasedAutoScalingConfiguration (Core.Maybe Core.Text)
timeBasedAutoScalingConfiguration_instanceId = Lens.lens (\TimeBasedAutoScalingConfiguration' {instanceId} -> instanceId) (\s@TimeBasedAutoScalingConfiguration' {} a -> s {instanceId = a} :: TimeBasedAutoScalingConfiguration)

-- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
timeBasedAutoScalingConfiguration_autoScalingSchedule :: Lens.Lens' TimeBasedAutoScalingConfiguration (Core.Maybe WeeklyAutoScalingSchedule)
timeBasedAutoScalingConfiguration_autoScalingSchedule = Lens.lens (\TimeBasedAutoScalingConfiguration' {autoScalingSchedule} -> autoScalingSchedule) (\s@TimeBasedAutoScalingConfiguration' {} a -> s {autoScalingSchedule = a} :: TimeBasedAutoScalingConfiguration)

instance
  Core.FromJSON
    TimeBasedAutoScalingConfiguration
  where
  parseJSON =
    Core.withObject
      "TimeBasedAutoScalingConfiguration"
      ( \x ->
          TimeBasedAutoScalingConfiguration'
            Core.<$> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "AutoScalingSchedule")
      )

instance
  Core.Hashable
    TimeBasedAutoScalingConfiguration

instance
  Core.NFData
    TimeBasedAutoScalingConfiguration
