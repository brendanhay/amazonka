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
-- Module      : Amazonka.IoT.Types.ScheduledAuditMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ScheduledAuditMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.AuditFrequency
import Amazonka.IoT.Types.DayOfWeek
import qualified Amazonka.Prelude as Prelude

-- | Information about the scheduled audit.
--
-- /See:/ 'newScheduledAuditMetadata' smart constructor.
data ScheduledAuditMetadata = ScheduledAuditMetadata'
  { -- | How often the scheduled audit occurs.
    frequency :: Prelude.Maybe AuditFrequency,
    -- | The day of the week on which the scheduled audit is run (if the
    -- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | The day of the month on which the scheduled audit is run (if the
    -- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
    -- does not have that many days, the audit takes place on the \"LAST\" day
    -- of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled audit.
    scheduledAuditName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledAuditMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frequency', 'scheduledAuditMetadata_frequency' - How often the scheduled audit occurs.
--
-- 'dayOfWeek', 'scheduledAuditMetadata_dayOfWeek' - The day of the week on which the scheduled audit is run (if the
-- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
--
-- 'dayOfMonth', 'scheduledAuditMetadata_dayOfMonth' - The day of the month on which the scheduled audit is run (if the
-- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
-- does not have that many days, the audit takes place on the \"LAST\" day
-- of the month.
--
-- 'scheduledAuditArn', 'scheduledAuditMetadata_scheduledAuditArn' - The ARN of the scheduled audit.
--
-- 'scheduledAuditName', 'scheduledAuditMetadata_scheduledAuditName' - The name of the scheduled audit.
newScheduledAuditMetadata ::
  ScheduledAuditMetadata
newScheduledAuditMetadata =
  ScheduledAuditMetadata'
    { frequency =
        Prelude.Nothing,
      dayOfWeek = Prelude.Nothing,
      dayOfMonth = Prelude.Nothing,
      scheduledAuditArn = Prelude.Nothing,
      scheduledAuditName = Prelude.Nothing
    }

-- | How often the scheduled audit occurs.
scheduledAuditMetadata_frequency :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe AuditFrequency)
scheduledAuditMetadata_frequency = Lens.lens (\ScheduledAuditMetadata' {frequency} -> frequency) (\s@ScheduledAuditMetadata' {} a -> s {frequency = a} :: ScheduledAuditMetadata)

-- | The day of the week on which the scheduled audit is run (if the
-- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
scheduledAuditMetadata_dayOfWeek :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe DayOfWeek)
scheduledAuditMetadata_dayOfWeek = Lens.lens (\ScheduledAuditMetadata' {dayOfWeek} -> dayOfWeek) (\s@ScheduledAuditMetadata' {} a -> s {dayOfWeek = a} :: ScheduledAuditMetadata)

-- | The day of the month on which the scheduled audit is run (if the
-- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
-- does not have that many days, the audit takes place on the \"LAST\" day
-- of the month.
scheduledAuditMetadata_dayOfMonth :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_dayOfMonth = Lens.lens (\ScheduledAuditMetadata' {dayOfMonth} -> dayOfMonth) (\s@ScheduledAuditMetadata' {} a -> s {dayOfMonth = a} :: ScheduledAuditMetadata)

-- | The ARN of the scheduled audit.
scheduledAuditMetadata_scheduledAuditArn :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_scheduledAuditArn = Lens.lens (\ScheduledAuditMetadata' {scheduledAuditArn} -> scheduledAuditArn) (\s@ScheduledAuditMetadata' {} a -> s {scheduledAuditArn = a} :: ScheduledAuditMetadata)

-- | The name of the scheduled audit.
scheduledAuditMetadata_scheduledAuditName :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_scheduledAuditName = Lens.lens (\ScheduledAuditMetadata' {scheduledAuditName} -> scheduledAuditName) (\s@ScheduledAuditMetadata' {} a -> s {scheduledAuditName = a} :: ScheduledAuditMetadata)

instance Core.FromJSON ScheduledAuditMetadata where
  parseJSON =
    Core.withObject
      "ScheduledAuditMetadata"
      ( \x ->
          ScheduledAuditMetadata'
            Prelude.<$> (x Core..:? "frequency")
            Prelude.<*> (x Core..:? "dayOfWeek")
            Prelude.<*> (x Core..:? "dayOfMonth")
            Prelude.<*> (x Core..:? "scheduledAuditArn")
            Prelude.<*> (x Core..:? "scheduledAuditName")
      )

instance Prelude.Hashable ScheduledAuditMetadata where
  hashWithSalt _salt ScheduledAuditMetadata' {..} =
    _salt `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` dayOfWeek
      `Prelude.hashWithSalt` dayOfMonth
      `Prelude.hashWithSalt` scheduledAuditArn
      `Prelude.hashWithSalt` scheduledAuditName

instance Prelude.NFData ScheduledAuditMetadata where
  rnf ScheduledAuditMetadata' {..} =
    Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf scheduledAuditArn
      `Prelude.seq` Prelude.rnf scheduledAuditName
