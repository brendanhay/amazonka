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
-- Module      : Network.AWS.ElasticSearch.Types.AutoTuneMaintenanceSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AutoTuneMaintenanceSchedule where

import Network.AWS.ElasticSearch.Types.Duration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies Auto-Tune maitenance schedule. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- /See:/ 'newAutoTuneMaintenanceSchedule' smart constructor.
data AutoTuneMaintenanceSchedule = AutoTuneMaintenanceSchedule'
  { -- | Specifies maintenance schedule duration: duration value and duration
    -- unit. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    duration :: Prelude.Maybe Duration,
    -- | Specifies timestamp at which Auto-Tune maintenance schedule start.
    startAt :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies cron expression for a recurring maintenance schedule. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    cronExpressionForRecurrence :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneMaintenanceSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'autoTuneMaintenanceSchedule_duration' - Specifies maintenance schedule duration: duration value and duration
-- unit. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- 'startAt', 'autoTuneMaintenanceSchedule_startAt' - Specifies timestamp at which Auto-Tune maintenance schedule start.
--
-- 'cronExpressionForRecurrence', 'autoTuneMaintenanceSchedule_cronExpressionForRecurrence' - Specifies cron expression for a recurring maintenance schedule. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
newAutoTuneMaintenanceSchedule ::
  AutoTuneMaintenanceSchedule
newAutoTuneMaintenanceSchedule =
  AutoTuneMaintenanceSchedule'
    { duration =
        Prelude.Nothing,
      startAt = Prelude.Nothing,
      cronExpressionForRecurrence = Prelude.Nothing
    }

-- | Specifies maintenance schedule duration: duration value and duration
-- unit. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTuneMaintenanceSchedule_duration :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Duration)
autoTuneMaintenanceSchedule_duration = Lens.lens (\AutoTuneMaintenanceSchedule' {duration} -> duration) (\s@AutoTuneMaintenanceSchedule' {} a -> s {duration = a} :: AutoTuneMaintenanceSchedule)

-- | Specifies timestamp at which Auto-Tune maintenance schedule start.
autoTuneMaintenanceSchedule_startAt :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Prelude.UTCTime)
autoTuneMaintenanceSchedule_startAt = Lens.lens (\AutoTuneMaintenanceSchedule' {startAt} -> startAt) (\s@AutoTuneMaintenanceSchedule' {} a -> s {startAt = a} :: AutoTuneMaintenanceSchedule) Prelude.. Lens.mapping Prelude._Time

-- | Specifies cron expression for a recurring maintenance schedule. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTuneMaintenanceSchedule_cronExpressionForRecurrence :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Prelude.Text)
autoTuneMaintenanceSchedule_cronExpressionForRecurrence = Lens.lens (\AutoTuneMaintenanceSchedule' {cronExpressionForRecurrence} -> cronExpressionForRecurrence) (\s@AutoTuneMaintenanceSchedule' {} a -> s {cronExpressionForRecurrence = a} :: AutoTuneMaintenanceSchedule)

instance Prelude.FromJSON AutoTuneMaintenanceSchedule where
  parseJSON =
    Prelude.withObject
      "AutoTuneMaintenanceSchedule"
      ( \x ->
          AutoTuneMaintenanceSchedule'
            Prelude.<$> (x Prelude..:? "Duration")
            Prelude.<*> (x Prelude..:? "StartAt")
            Prelude.<*> (x Prelude..:? "CronExpressionForRecurrence")
      )

instance Prelude.Hashable AutoTuneMaintenanceSchedule

instance Prelude.NFData AutoTuneMaintenanceSchedule

instance Prelude.ToJSON AutoTuneMaintenanceSchedule where
  toJSON AutoTuneMaintenanceSchedule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Duration" Prelude..=) Prelude.<$> duration,
            ("StartAt" Prelude..=) Prelude.<$> startAt,
            ("CronExpressionForRecurrence" Prelude..=)
              Prelude.<$> cronExpressionForRecurrence
          ]
      )
