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
-- Module      : Network.AWS.OpenSearch.Types.ScheduledAutoTuneDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.ScheduledAutoTuneDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types.ScheduledAutoTuneActionType
import Network.AWS.OpenSearch.Types.ScheduledAutoTuneSeverityType
import qualified Network.AWS.Prelude as Prelude

-- | Specifies details about the scheduled Auto-Tune action. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
--
-- /See:/ 'newScheduledAutoTuneDetails' smart constructor.
data ScheduledAutoTuneDetails = ScheduledAutoTuneDetails'
  { -- | The Auto-Tune action severity. Valid values are LOW, MEDIUM, and HIGH.
    severity :: Prelude.Maybe ScheduledAutoTuneSeverityType,
    -- | The Auto-Tune action description.
    action :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the Auto-Tune action scheduled for the domain.
    date :: Prelude.Maybe Core.POSIX,
    -- | The Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING and
    -- JVM_YOUNG_GEN_TUNING.
    actionType :: Prelude.Maybe ScheduledAutoTuneActionType
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
-- 'action', 'scheduledAutoTuneDetails_action' - The Auto-Tune action description.
--
-- 'date', 'scheduledAutoTuneDetails_date' - The timestamp of the Auto-Tune action scheduled for the domain.
--
-- 'actionType', 'scheduledAutoTuneDetails_actionType' - The Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING and
-- JVM_YOUNG_GEN_TUNING.
newScheduledAutoTuneDetails ::
  ScheduledAutoTuneDetails
newScheduledAutoTuneDetails =
  ScheduledAutoTuneDetails'
    { severity =
        Prelude.Nothing,
      action = Prelude.Nothing,
      date = Prelude.Nothing,
      actionType = Prelude.Nothing
    }

-- | The Auto-Tune action severity. Valid values are LOW, MEDIUM, and HIGH.
scheduledAutoTuneDetails_severity :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe ScheduledAutoTuneSeverityType)
scheduledAutoTuneDetails_severity = Lens.lens (\ScheduledAutoTuneDetails' {severity} -> severity) (\s@ScheduledAutoTuneDetails' {} a -> s {severity = a} :: ScheduledAutoTuneDetails)

-- | The Auto-Tune action description.
scheduledAutoTuneDetails_action :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe Prelude.Text)
scheduledAutoTuneDetails_action = Lens.lens (\ScheduledAutoTuneDetails' {action} -> action) (\s@ScheduledAutoTuneDetails' {} a -> s {action = a} :: ScheduledAutoTuneDetails)

-- | The timestamp of the Auto-Tune action scheduled for the domain.
scheduledAutoTuneDetails_date :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe Prelude.UTCTime)
scheduledAutoTuneDetails_date = Lens.lens (\ScheduledAutoTuneDetails' {date} -> date) (\s@ScheduledAutoTuneDetails' {} a -> s {date = a} :: ScheduledAutoTuneDetails) Prelude.. Lens.mapping Core._Time

-- | The Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING and
-- JVM_YOUNG_GEN_TUNING.
scheduledAutoTuneDetails_actionType :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe ScheduledAutoTuneActionType)
scheduledAutoTuneDetails_actionType = Lens.lens (\ScheduledAutoTuneDetails' {actionType} -> actionType) (\s@ScheduledAutoTuneDetails' {} a -> s {actionType = a} :: ScheduledAutoTuneDetails)

instance Core.FromJSON ScheduledAutoTuneDetails where
  parseJSON =
    Core.withObject
      "ScheduledAutoTuneDetails"
      ( \x ->
          ScheduledAutoTuneDetails'
            Prelude.<$> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "Action")
            Prelude.<*> (x Core..:? "Date")
            Prelude.<*> (x Core..:? "ActionType")
      )

instance Prelude.Hashable ScheduledAutoTuneDetails

instance Prelude.NFData ScheduledAutoTuneDetails
