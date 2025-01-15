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
-- Module      : Amazonka.IoT.Types.SchedulingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.SchedulingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.JobEndBehavior
import qualified Amazonka.Prelude as Prelude

-- | Specifies the date and time that a job will begin the rollout of the job
-- document to all devices in the target group. Additionally, you can
-- specify the end behavior for each job execution when it reaches the
-- scheduled end time.
--
-- /See:/ 'newSchedulingConfig' smart constructor.
data SchedulingConfig = SchedulingConfig'
  { -- | Specifies the end behavior for all job executions after a job reaches
    -- the selected @endTime@. If @endTime@ is not selected when creating the
    -- job, then @endBehavior@ does not apply.
    endBehavior :: Prelude.Maybe JobEndBehavior,
    -- | The time a job will stop rollout of the job document to all devices in
    -- the target group for a job. The @endTime@ must take place no later than
    -- two years from the current time and be scheduled a minimum of thirty
    -- minutes from the current time. The minimum duration between @startTime@
    -- and @endTime@ is thirty minutes. The maximum duration between
    -- @startTime@ and @endTime@ is two years.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The time a job will begin rollout of the job document to all devices in
    -- the target group for a job. The @startTime@ can be scheduled up to a
    -- year in advance and must be scheduled a minimum of thirty minutes from
    -- the current time.
    startTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchedulingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endBehavior', 'schedulingConfig_endBehavior' - Specifies the end behavior for all job executions after a job reaches
-- the selected @endTime@. If @endTime@ is not selected when creating the
-- job, then @endBehavior@ does not apply.
--
-- 'endTime', 'schedulingConfig_endTime' - The time a job will stop rollout of the job document to all devices in
-- the target group for a job. The @endTime@ must take place no later than
-- two years from the current time and be scheduled a minimum of thirty
-- minutes from the current time. The minimum duration between @startTime@
-- and @endTime@ is thirty minutes. The maximum duration between
-- @startTime@ and @endTime@ is two years.
--
-- 'startTime', 'schedulingConfig_startTime' - The time a job will begin rollout of the job document to all devices in
-- the target group for a job. The @startTime@ can be scheduled up to a
-- year in advance and must be scheduled a minimum of thirty minutes from
-- the current time.
newSchedulingConfig ::
  SchedulingConfig
newSchedulingConfig =
  SchedulingConfig'
    { endBehavior = Prelude.Nothing,
      endTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Specifies the end behavior for all job executions after a job reaches
-- the selected @endTime@. If @endTime@ is not selected when creating the
-- job, then @endBehavior@ does not apply.
schedulingConfig_endBehavior :: Lens.Lens' SchedulingConfig (Prelude.Maybe JobEndBehavior)
schedulingConfig_endBehavior = Lens.lens (\SchedulingConfig' {endBehavior} -> endBehavior) (\s@SchedulingConfig' {} a -> s {endBehavior = a} :: SchedulingConfig)

-- | The time a job will stop rollout of the job document to all devices in
-- the target group for a job. The @endTime@ must take place no later than
-- two years from the current time and be scheduled a minimum of thirty
-- minutes from the current time. The minimum duration between @startTime@
-- and @endTime@ is thirty minutes. The maximum duration between
-- @startTime@ and @endTime@ is two years.
schedulingConfig_endTime :: Lens.Lens' SchedulingConfig (Prelude.Maybe Prelude.Text)
schedulingConfig_endTime = Lens.lens (\SchedulingConfig' {endTime} -> endTime) (\s@SchedulingConfig' {} a -> s {endTime = a} :: SchedulingConfig)

-- | The time a job will begin rollout of the job document to all devices in
-- the target group for a job. The @startTime@ can be scheduled up to a
-- year in advance and must be scheduled a minimum of thirty minutes from
-- the current time.
schedulingConfig_startTime :: Lens.Lens' SchedulingConfig (Prelude.Maybe Prelude.Text)
schedulingConfig_startTime = Lens.lens (\SchedulingConfig' {startTime} -> startTime) (\s@SchedulingConfig' {} a -> s {startTime = a} :: SchedulingConfig)

instance Data.FromJSON SchedulingConfig where
  parseJSON =
    Data.withObject
      "SchedulingConfig"
      ( \x ->
          SchedulingConfig'
            Prelude.<$> (x Data..:? "endBehavior")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable SchedulingConfig where
  hashWithSalt _salt SchedulingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` endBehavior
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData SchedulingConfig where
  rnf SchedulingConfig' {..} =
    Prelude.rnf endBehavior `Prelude.seq`
      Prelude.rnf endTime `Prelude.seq`
        Prelude.rnf startTime

instance Data.ToJSON SchedulingConfig where
  toJSON SchedulingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endBehavior" Data..=) Prelude.<$> endBehavior,
            ("endTime" Data..=) Prelude.<$> endTime,
            ("startTime" Data..=) Prelude.<$> startTime
          ]
      )
