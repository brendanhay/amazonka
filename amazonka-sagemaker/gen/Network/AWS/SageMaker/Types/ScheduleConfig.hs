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
-- Module      : Network.AWS.SageMaker.Types.ScheduleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ScheduleConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration details about the monitoring schedule.
--
-- /See:/ 'newScheduleConfig' smart constructor.
data ScheduleConfig = ScheduleConfig'
  { -- | A cron expression that describes details about the monitoring schedule.
    --
    -- Currently the only supported cron expressions are:
    --
    -- -   If you want to set the job to start every hour, please use the
    --     following:
    --
    --     @Hourly: cron(0 * ? * * *)@
    --
    -- -   If you want to start the job daily:
    --
    --     @cron(0 [00-23] ? * * *)@
    --
    -- For example, the following are valid cron expressions:
    --
    -- -   Daily at noon UTC: @cron(0 12 ? * * *)@
    --
    -- -   Daily at midnight UTC: @cron(0 0 ? * * *)@
    --
    -- To support running every 6, 12 hours, the following are also supported:
    --
    -- @cron(0 [00-23]\/[01-24] ? * * *)@
    --
    -- For example, the following are valid cron expressions:
    --
    -- -   Every 12 hours, starting at 5pm UTC: @cron(0 17\/12 ? * * *)@
    --
    -- -   Every two hours starting at midnight: @cron(0 0\/2 ? * * *)@
    --
    -- -   Even though the cron expression is set to start at 5PM UTC, note
    --     that there could be a delay of 0-20 minutes from the actual
    --     requested time to run the execution.
    --
    -- -   We recommend that if you would like a daily schedule, you do not
    --     provide this parameter. Amazon SageMaker will pick a time for
    --     running every day.
    scheduleExpression :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleExpression', 'scheduleConfig_scheduleExpression' - A cron expression that describes details about the monitoring schedule.
--
-- Currently the only supported cron expressions are:
--
-- -   If you want to set the job to start every hour, please use the
--     following:
--
--     @Hourly: cron(0 * ? * * *)@
--
-- -   If you want to start the job daily:
--
--     @cron(0 [00-23] ? * * *)@
--
-- For example, the following are valid cron expressions:
--
-- -   Daily at noon UTC: @cron(0 12 ? * * *)@
--
-- -   Daily at midnight UTC: @cron(0 0 ? * * *)@
--
-- To support running every 6, 12 hours, the following are also supported:
--
-- @cron(0 [00-23]\/[01-24] ? * * *)@
--
-- For example, the following are valid cron expressions:
--
-- -   Every 12 hours, starting at 5pm UTC: @cron(0 17\/12 ? * * *)@
--
-- -   Every two hours starting at midnight: @cron(0 0\/2 ? * * *)@
--
-- -   Even though the cron expression is set to start at 5PM UTC, note
--     that there could be a delay of 0-20 minutes from the actual
--     requested time to run the execution.
--
-- -   We recommend that if you would like a daily schedule, you do not
--     provide this parameter. Amazon SageMaker will pick a time for
--     running every day.
newScheduleConfig ::
  -- | 'scheduleExpression'
  Core.Text ->
  ScheduleConfig
newScheduleConfig pScheduleExpression_ =
  ScheduleConfig'
    { scheduleExpression =
        pScheduleExpression_
    }

-- | A cron expression that describes details about the monitoring schedule.
--
-- Currently the only supported cron expressions are:
--
-- -   If you want to set the job to start every hour, please use the
--     following:
--
--     @Hourly: cron(0 * ? * * *)@
--
-- -   If you want to start the job daily:
--
--     @cron(0 [00-23] ? * * *)@
--
-- For example, the following are valid cron expressions:
--
-- -   Daily at noon UTC: @cron(0 12 ? * * *)@
--
-- -   Daily at midnight UTC: @cron(0 0 ? * * *)@
--
-- To support running every 6, 12 hours, the following are also supported:
--
-- @cron(0 [00-23]\/[01-24] ? * * *)@
--
-- For example, the following are valid cron expressions:
--
-- -   Every 12 hours, starting at 5pm UTC: @cron(0 17\/12 ? * * *)@
--
-- -   Every two hours starting at midnight: @cron(0 0\/2 ? * * *)@
--
-- -   Even though the cron expression is set to start at 5PM UTC, note
--     that there could be a delay of 0-20 minutes from the actual
--     requested time to run the execution.
--
-- -   We recommend that if you would like a daily schedule, you do not
--     provide this parameter. Amazon SageMaker will pick a time for
--     running every day.
scheduleConfig_scheduleExpression :: Lens.Lens' ScheduleConfig Core.Text
scheduleConfig_scheduleExpression = Lens.lens (\ScheduleConfig' {scheduleExpression} -> scheduleExpression) (\s@ScheduleConfig' {} a -> s {scheduleExpression = a} :: ScheduleConfig)

instance Core.FromJSON ScheduleConfig where
  parseJSON =
    Core.withObject
      "ScheduleConfig"
      ( \x ->
          ScheduleConfig'
            Core.<$> (x Core..: "ScheduleExpression")
      )

instance Core.Hashable ScheduleConfig

instance Core.NFData ScheduleConfig

instance Core.ToJSON ScheduleConfig where
  toJSON ScheduleConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ScheduleExpression" Core..= scheduleExpression)
          ]
      )
