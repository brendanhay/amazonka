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
-- Module      : Amazonka.ElasticSearch.Types.ScheduledAutoTuneDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ScheduledAutoTuneDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.ScheduledAutoTuneActionType
import Amazonka.ElasticSearch.Types.ScheduledAutoTuneSeverityType
import qualified Amazonka.Prelude as Prelude

-- | Specifies details of the scheduled Auto-Tune action. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- /See:/ 'newScheduledAutoTuneDetails' smart constructor.
data ScheduledAutoTuneDetails = ScheduledAutoTuneDetails'
  { -- | Specifies Auto-Tune action description.
    action :: Prelude.Maybe Prelude.Text,
    -- | Specifies Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING
    -- and JVM_YOUNG_GEN_TUNING.
    actionType :: Prelude.Maybe ScheduledAutoTuneActionType,
    -- | Specifies timestamp for the Auto-Tune action scheduled for the domain.
    date :: Prelude.Maybe Data.POSIX,
    -- | Specifies Auto-Tune action severity. Valid values are LOW, MEDIUM and
    -- HIGH.
    severity :: Prelude.Maybe ScheduledAutoTuneSeverityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledAutoTuneDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'scheduledAutoTuneDetails_action' - Specifies Auto-Tune action description.
--
-- 'actionType', 'scheduledAutoTuneDetails_actionType' - Specifies Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING
-- and JVM_YOUNG_GEN_TUNING.
--
-- 'date', 'scheduledAutoTuneDetails_date' - Specifies timestamp for the Auto-Tune action scheduled for the domain.
--
-- 'severity', 'scheduledAutoTuneDetails_severity' - Specifies Auto-Tune action severity. Valid values are LOW, MEDIUM and
-- HIGH.
newScheduledAutoTuneDetails ::
  ScheduledAutoTuneDetails
newScheduledAutoTuneDetails =
  ScheduledAutoTuneDetails'
    { action = Prelude.Nothing,
      actionType = Prelude.Nothing,
      date = Prelude.Nothing,
      severity = Prelude.Nothing
    }

-- | Specifies Auto-Tune action description.
scheduledAutoTuneDetails_action :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe Prelude.Text)
scheduledAutoTuneDetails_action = Lens.lens (\ScheduledAutoTuneDetails' {action} -> action) (\s@ScheduledAutoTuneDetails' {} a -> s {action = a} :: ScheduledAutoTuneDetails)

-- | Specifies Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING
-- and JVM_YOUNG_GEN_TUNING.
scheduledAutoTuneDetails_actionType :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe ScheduledAutoTuneActionType)
scheduledAutoTuneDetails_actionType = Lens.lens (\ScheduledAutoTuneDetails' {actionType} -> actionType) (\s@ScheduledAutoTuneDetails' {} a -> s {actionType = a} :: ScheduledAutoTuneDetails)

-- | Specifies timestamp for the Auto-Tune action scheduled for the domain.
scheduledAutoTuneDetails_date :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe Prelude.UTCTime)
scheduledAutoTuneDetails_date = Lens.lens (\ScheduledAutoTuneDetails' {date} -> date) (\s@ScheduledAutoTuneDetails' {} a -> s {date = a} :: ScheduledAutoTuneDetails) Prelude.. Lens.mapping Data._Time

-- | Specifies Auto-Tune action severity. Valid values are LOW, MEDIUM and
-- HIGH.
scheduledAutoTuneDetails_severity :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe ScheduledAutoTuneSeverityType)
scheduledAutoTuneDetails_severity = Lens.lens (\ScheduledAutoTuneDetails' {severity} -> severity) (\s@ScheduledAutoTuneDetails' {} a -> s {severity = a} :: ScheduledAutoTuneDetails)

instance Data.FromJSON ScheduledAutoTuneDetails where
  parseJSON =
    Data.withObject
      "ScheduledAutoTuneDetails"
      ( \x ->
          ScheduledAutoTuneDetails'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "ActionType")
            Prelude.<*> (x Data..:? "Date")
            Prelude.<*> (x Data..:? "Severity")
      )

instance Prelude.Hashable ScheduledAutoTuneDetails where
  hashWithSalt _salt ScheduledAutoTuneDetails' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` severity

instance Prelude.NFData ScheduledAutoTuneDetails where
  rnf ScheduledAutoTuneDetails' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf date
      `Prelude.seq` Prelude.rnf severity
