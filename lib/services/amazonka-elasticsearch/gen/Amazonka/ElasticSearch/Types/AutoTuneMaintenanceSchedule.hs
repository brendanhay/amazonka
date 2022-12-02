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
-- Module      : Amazonka.ElasticSearch.Types.AutoTuneMaintenanceSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.AutoTuneMaintenanceSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.Duration
import qualified Amazonka.Prelude as Prelude

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
    -- | Specifies cron expression for a recurring maintenance schedule. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    cronExpressionForRecurrence :: Prelude.Maybe Prelude.Text,
    -- | Specifies timestamp at which Auto-Tune maintenance schedule start.
    startAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'cronExpressionForRecurrence', 'autoTuneMaintenanceSchedule_cronExpressionForRecurrence' - Specifies cron expression for a recurring maintenance schedule. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- 'startAt', 'autoTuneMaintenanceSchedule_startAt' - Specifies timestamp at which Auto-Tune maintenance schedule start.
newAutoTuneMaintenanceSchedule ::
  AutoTuneMaintenanceSchedule
newAutoTuneMaintenanceSchedule =
  AutoTuneMaintenanceSchedule'
    { duration =
        Prelude.Nothing,
      cronExpressionForRecurrence = Prelude.Nothing,
      startAt = Prelude.Nothing
    }

-- | Specifies maintenance schedule duration: duration value and duration
-- unit. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTuneMaintenanceSchedule_duration :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Duration)
autoTuneMaintenanceSchedule_duration = Lens.lens (\AutoTuneMaintenanceSchedule' {duration} -> duration) (\s@AutoTuneMaintenanceSchedule' {} a -> s {duration = a} :: AutoTuneMaintenanceSchedule)

-- | Specifies cron expression for a recurring maintenance schedule. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTuneMaintenanceSchedule_cronExpressionForRecurrence :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Prelude.Text)
autoTuneMaintenanceSchedule_cronExpressionForRecurrence = Lens.lens (\AutoTuneMaintenanceSchedule' {cronExpressionForRecurrence} -> cronExpressionForRecurrence) (\s@AutoTuneMaintenanceSchedule' {} a -> s {cronExpressionForRecurrence = a} :: AutoTuneMaintenanceSchedule)

-- | Specifies timestamp at which Auto-Tune maintenance schedule start.
autoTuneMaintenanceSchedule_startAt :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Prelude.UTCTime)
autoTuneMaintenanceSchedule_startAt = Lens.lens (\AutoTuneMaintenanceSchedule' {startAt} -> startAt) (\s@AutoTuneMaintenanceSchedule' {} a -> s {startAt = a} :: AutoTuneMaintenanceSchedule) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AutoTuneMaintenanceSchedule where
  parseJSON =
    Data.withObject
      "AutoTuneMaintenanceSchedule"
      ( \x ->
          AutoTuneMaintenanceSchedule'
            Prelude.<$> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "CronExpressionForRecurrence")
            Prelude.<*> (x Data..:? "StartAt")
      )

instance Prelude.Hashable AutoTuneMaintenanceSchedule where
  hashWithSalt _salt AutoTuneMaintenanceSchedule' {..} =
    _salt `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` cronExpressionForRecurrence
      `Prelude.hashWithSalt` startAt

instance Prelude.NFData AutoTuneMaintenanceSchedule where
  rnf AutoTuneMaintenanceSchedule' {..} =
    Prelude.rnf duration
      `Prelude.seq` Prelude.rnf cronExpressionForRecurrence
      `Prelude.seq` Prelude.rnf startAt

instance Data.ToJSON AutoTuneMaintenanceSchedule where
  toJSON AutoTuneMaintenanceSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Duration" Data..=) Prelude.<$> duration,
            ("CronExpressionForRecurrence" Data..=)
              Prelude.<$> cronExpressionForRecurrence,
            ("StartAt" Data..=) Prelude.<$> startAt
          ]
      )
