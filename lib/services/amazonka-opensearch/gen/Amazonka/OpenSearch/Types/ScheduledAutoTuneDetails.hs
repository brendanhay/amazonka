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
-- Module      : Amazonka.OpenSearch.Types.ScheduledAutoTuneDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ScheduledAutoTuneDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.ScheduledAutoTuneActionType
import Amazonka.OpenSearch.Types.ScheduledAutoTuneSeverityType
import qualified Amazonka.Prelude as Prelude

-- | Specifies details about the scheduled Auto-Tune action. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
--
-- /See:/ 'newScheduledAutoTuneDetails' smart constructor.
data ScheduledAutoTuneDetails = ScheduledAutoTuneDetails'
  { -- | The Auto-Tune action severity. Valid values are LOW, MEDIUM, and HIGH.
    severity :: Prelude.Maybe ScheduledAutoTuneSeverityType,
    -- | The Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING and
    -- JVM_YOUNG_GEN_TUNING.
    actionType :: Prelude.Maybe ScheduledAutoTuneActionType,
    -- | The timestamp of the Auto-Tune action scheduled for the domain.
    date :: Prelude.Maybe Core.POSIX,
    -- | The Auto-Tune action description.
    action :: Prelude.Maybe Prelude.Text
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
-- 'severity', 'scheduledAutoTuneDetails_severity' - The Auto-Tune action severity. Valid values are LOW, MEDIUM, and HIGH.
--
-- 'actionType', 'scheduledAutoTuneDetails_actionType' - The Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING and
-- JVM_YOUNG_GEN_TUNING.
--
-- 'date', 'scheduledAutoTuneDetails_date' - The timestamp of the Auto-Tune action scheduled for the domain.
--
-- 'action', 'scheduledAutoTuneDetails_action' - The Auto-Tune action description.
newScheduledAutoTuneDetails ::
  ScheduledAutoTuneDetails
newScheduledAutoTuneDetails =
  ScheduledAutoTuneDetails'
    { severity =
        Prelude.Nothing,
      actionType = Prelude.Nothing,
      date = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The Auto-Tune action severity. Valid values are LOW, MEDIUM, and HIGH.
scheduledAutoTuneDetails_severity :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe ScheduledAutoTuneSeverityType)
scheduledAutoTuneDetails_severity = Lens.lens (\ScheduledAutoTuneDetails' {severity} -> severity) (\s@ScheduledAutoTuneDetails' {} a -> s {severity = a} :: ScheduledAutoTuneDetails)

-- | The Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING and
-- JVM_YOUNG_GEN_TUNING.
scheduledAutoTuneDetails_actionType :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe ScheduledAutoTuneActionType)
scheduledAutoTuneDetails_actionType = Lens.lens (\ScheduledAutoTuneDetails' {actionType} -> actionType) (\s@ScheduledAutoTuneDetails' {} a -> s {actionType = a} :: ScheduledAutoTuneDetails)

-- | The timestamp of the Auto-Tune action scheduled for the domain.
scheduledAutoTuneDetails_date :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe Prelude.UTCTime)
scheduledAutoTuneDetails_date = Lens.lens (\ScheduledAutoTuneDetails' {date} -> date) (\s@ScheduledAutoTuneDetails' {} a -> s {date = a} :: ScheduledAutoTuneDetails) Prelude.. Lens.mapping Core._Time

-- | The Auto-Tune action description.
scheduledAutoTuneDetails_action :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe Prelude.Text)
scheduledAutoTuneDetails_action = Lens.lens (\ScheduledAutoTuneDetails' {action} -> action) (\s@ScheduledAutoTuneDetails' {} a -> s {action = a} :: ScheduledAutoTuneDetails)

instance Core.FromJSON ScheduledAutoTuneDetails where
  parseJSON =
    Core.withObject
      "ScheduledAutoTuneDetails"
      ( \x ->
          ScheduledAutoTuneDetails'
            Prelude.<$> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "ActionType")
            Prelude.<*> (x Core..:? "Date")
            Prelude.<*> (x Core..:? "Action")
      )

instance Prelude.Hashable ScheduledAutoTuneDetails where
  hashWithSalt _salt ScheduledAutoTuneDetails' {..} =
    _salt `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` action

instance Prelude.NFData ScheduledAutoTuneDetails where
  rnf ScheduledAutoTuneDetails' {..} =
    Prelude.rnf severity
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf date
      `Prelude.seq` Prelude.rnf action
