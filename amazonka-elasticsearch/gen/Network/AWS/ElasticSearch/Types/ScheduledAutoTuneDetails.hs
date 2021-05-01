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
-- Module      : Network.AWS.ElasticSearch.Types.ScheduledAutoTuneDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ScheduledAutoTuneDetails where

import Network.AWS.ElasticSearch.Types.ScheduledAutoTuneActionType
import Network.AWS.ElasticSearch.Types.ScheduledAutoTuneSeverityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies details of the scheduled Auto-Tune action. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- /See:/ 'newScheduledAutoTuneDetails' smart constructor.
data ScheduledAutoTuneDetails = ScheduledAutoTuneDetails'
  { -- | Specifies Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING
    -- and JVM_YOUNG_GEN_TUNING.
    actionType :: Prelude.Maybe ScheduledAutoTuneActionType,
    -- | Specifies Auto-Tune action severity. Valid values are LOW, MEDIUM and
    -- HIGH.
    severity :: Prelude.Maybe ScheduledAutoTuneSeverityType,
    -- | Specifies timestamp for the Auto-Tune action scheduled for the domain.
    date :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies Auto-Tune action description.
    action :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledAutoTuneDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'scheduledAutoTuneDetails_actionType' - Specifies Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING
-- and JVM_YOUNG_GEN_TUNING.
--
-- 'severity', 'scheduledAutoTuneDetails_severity' - Specifies Auto-Tune action severity. Valid values are LOW, MEDIUM and
-- HIGH.
--
-- 'date', 'scheduledAutoTuneDetails_date' - Specifies timestamp for the Auto-Tune action scheduled for the domain.
--
-- 'action', 'scheduledAutoTuneDetails_action' - Specifies Auto-Tune action description.
newScheduledAutoTuneDetails ::
  ScheduledAutoTuneDetails
newScheduledAutoTuneDetails =
  ScheduledAutoTuneDetails'
    { actionType =
        Prelude.Nothing,
      severity = Prelude.Nothing,
      date = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | Specifies Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING
-- and JVM_YOUNG_GEN_TUNING.
scheduledAutoTuneDetails_actionType :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe ScheduledAutoTuneActionType)
scheduledAutoTuneDetails_actionType = Lens.lens (\ScheduledAutoTuneDetails' {actionType} -> actionType) (\s@ScheduledAutoTuneDetails' {} a -> s {actionType = a} :: ScheduledAutoTuneDetails)

-- | Specifies Auto-Tune action severity. Valid values are LOW, MEDIUM and
-- HIGH.
scheduledAutoTuneDetails_severity :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe ScheduledAutoTuneSeverityType)
scheduledAutoTuneDetails_severity = Lens.lens (\ScheduledAutoTuneDetails' {severity} -> severity) (\s@ScheduledAutoTuneDetails' {} a -> s {severity = a} :: ScheduledAutoTuneDetails)

-- | Specifies timestamp for the Auto-Tune action scheduled for the domain.
scheduledAutoTuneDetails_date :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe Prelude.UTCTime)
scheduledAutoTuneDetails_date = Lens.lens (\ScheduledAutoTuneDetails' {date} -> date) (\s@ScheduledAutoTuneDetails' {} a -> s {date = a} :: ScheduledAutoTuneDetails) Prelude.. Lens.mapping Prelude._Time

-- | Specifies Auto-Tune action description.
scheduledAutoTuneDetails_action :: Lens.Lens' ScheduledAutoTuneDetails (Prelude.Maybe Prelude.Text)
scheduledAutoTuneDetails_action = Lens.lens (\ScheduledAutoTuneDetails' {action} -> action) (\s@ScheduledAutoTuneDetails' {} a -> s {action = a} :: ScheduledAutoTuneDetails)

instance Prelude.FromJSON ScheduledAutoTuneDetails where
  parseJSON =
    Prelude.withObject
      "ScheduledAutoTuneDetails"
      ( \x ->
          ScheduledAutoTuneDetails'
            Prelude.<$> (x Prelude..:? "ActionType")
            Prelude.<*> (x Prelude..:? "Severity")
            Prelude.<*> (x Prelude..:? "Date")
            Prelude.<*> (x Prelude..:? "Action")
      )

instance Prelude.Hashable ScheduledAutoTuneDetails

instance Prelude.NFData ScheduledAutoTuneDetails
