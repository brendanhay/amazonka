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
-- Module      : Amazonka.KinesisVideo.Types.ScheduleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.ScheduleConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This API enables you to specify the duration that the camera, or local
-- media file, should record onto the Edge Agent. The @ScheduleConfig@
-- consists of the @ScheduleExpression@ and the @DurationInMinutes@
-- attributes.
--
-- If the @ScheduleExpression@ is not provided, then the Edge Agent will
-- always be set to recording mode.
--
-- /See:/ 'newScheduleConfig' smart constructor.
data ScheduleConfig = ScheduleConfig'
  { -- | The Quartz cron expression that takes care of scheduling jobs to record
    -- from the camera, or local media file, onto the Edge Agent. If the
    -- @ScheduleExpression@ is not provided for the @RecorderConfig@, then the
    -- Edge Agent will always be set to recording mode.
    --
    -- For more information about Quartz, refer to the
    -- <http://www.quartz-scheduler.org/documentation/quartz-2.3.0/tutorials/crontrigger.html Cron Trigger Tutorial>
    -- page to understand the valid expressions and its use.
    scheduleExpression :: Prelude.Text,
    -- | The total duration to record the media. If the @ScheduleExpression@
    -- attribute is provided, then the @DurationInSeconds@ attribute should
    -- also be specified.
    durationInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleExpression', 'scheduleConfig_scheduleExpression' - The Quartz cron expression that takes care of scheduling jobs to record
-- from the camera, or local media file, onto the Edge Agent. If the
-- @ScheduleExpression@ is not provided for the @RecorderConfig@, then the
-- Edge Agent will always be set to recording mode.
--
-- For more information about Quartz, refer to the
-- <http://www.quartz-scheduler.org/documentation/quartz-2.3.0/tutorials/crontrigger.html Cron Trigger Tutorial>
-- page to understand the valid expressions and its use.
--
-- 'durationInSeconds', 'scheduleConfig_durationInSeconds' - The total duration to record the media. If the @ScheduleExpression@
-- attribute is provided, then the @DurationInSeconds@ attribute should
-- also be specified.
newScheduleConfig ::
  -- | 'scheduleExpression'
  Prelude.Text ->
  -- | 'durationInSeconds'
  Prelude.Natural ->
  ScheduleConfig
newScheduleConfig
  pScheduleExpression_
  pDurationInSeconds_ =
    ScheduleConfig'
      { scheduleExpression =
          pScheduleExpression_,
        durationInSeconds = pDurationInSeconds_
      }

-- | The Quartz cron expression that takes care of scheduling jobs to record
-- from the camera, or local media file, onto the Edge Agent. If the
-- @ScheduleExpression@ is not provided for the @RecorderConfig@, then the
-- Edge Agent will always be set to recording mode.
--
-- For more information about Quartz, refer to the
-- <http://www.quartz-scheduler.org/documentation/quartz-2.3.0/tutorials/crontrigger.html Cron Trigger Tutorial>
-- page to understand the valid expressions and its use.
scheduleConfig_scheduleExpression :: Lens.Lens' ScheduleConfig Prelude.Text
scheduleConfig_scheduleExpression = Lens.lens (\ScheduleConfig' {scheduleExpression} -> scheduleExpression) (\s@ScheduleConfig' {} a -> s {scheduleExpression = a} :: ScheduleConfig)

-- | The total duration to record the media. If the @ScheduleExpression@
-- attribute is provided, then the @DurationInSeconds@ attribute should
-- also be specified.
scheduleConfig_durationInSeconds :: Lens.Lens' ScheduleConfig Prelude.Natural
scheduleConfig_durationInSeconds = Lens.lens (\ScheduleConfig' {durationInSeconds} -> durationInSeconds) (\s@ScheduleConfig' {} a -> s {durationInSeconds = a} :: ScheduleConfig)

instance Data.FromJSON ScheduleConfig where
  parseJSON =
    Data.withObject
      "ScheduleConfig"
      ( \x ->
          ScheduleConfig'
            Prelude.<$> (x Data..: "ScheduleExpression")
            Prelude.<*> (x Data..: "DurationInSeconds")
      )

instance Prelude.Hashable ScheduleConfig where
  hashWithSalt _salt ScheduleConfig' {..} =
    _salt
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` durationInSeconds

instance Prelude.NFData ScheduleConfig where
  rnf ScheduleConfig' {..} =
    Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf durationInSeconds

instance Data.ToJSON ScheduleConfig where
  toJSON ScheduleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScheduleExpression" Data..= scheduleExpression),
            Prelude.Just
              ("DurationInSeconds" Data..= durationInSeconds)
          ]
      )
