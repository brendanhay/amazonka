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
-- Module      : Amazonka.OpsWorks.Types.TimeBasedAutoScalingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.TimeBasedAutoScalingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types.WeeklyAutoScalingSchedule
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance\'s time-based auto scaling configuration.
--
-- /See:/ 'newTimeBasedAutoScalingConfiguration' smart constructor.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
    autoScalingSchedule :: Prelude.Maybe WeeklyAutoScalingSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      autoScalingSchedule = Prelude.Nothing
    }

-- | The instance ID.
timeBasedAutoScalingConfiguration_instanceId :: Lens.Lens' TimeBasedAutoScalingConfiguration (Prelude.Maybe Prelude.Text)
timeBasedAutoScalingConfiguration_instanceId = Lens.lens (\TimeBasedAutoScalingConfiguration' {instanceId} -> instanceId) (\s@TimeBasedAutoScalingConfiguration' {} a -> s {instanceId = a} :: TimeBasedAutoScalingConfiguration)

-- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
timeBasedAutoScalingConfiguration_autoScalingSchedule :: Lens.Lens' TimeBasedAutoScalingConfiguration (Prelude.Maybe WeeklyAutoScalingSchedule)
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
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "AutoScalingSchedule")
      )

instance
  Prelude.Hashable
    TimeBasedAutoScalingConfiguration
  where
  hashWithSalt
    _salt
    TimeBasedAutoScalingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` autoScalingSchedule

instance
  Prelude.NFData
    TimeBasedAutoScalingConfiguration
  where
  rnf TimeBasedAutoScalingConfiguration' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf autoScalingSchedule
