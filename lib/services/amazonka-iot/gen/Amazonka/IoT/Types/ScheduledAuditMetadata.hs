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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ScheduledAuditMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AuditFrequency
import Amazonka.IoT.Types.DayOfWeek
import qualified Amazonka.Prelude as Prelude

-- | Information about the scheduled audit.
--
-- /See:/ 'newScheduledAuditMetadata' smart constructor.
data ScheduledAuditMetadata = ScheduledAuditMetadata'
  { -- | The day of the month on which the scheduled audit is run (if the
    -- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
    -- does not have that many days, the audit takes place on the \"LAST\" day
    -- of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Text,
    -- | The day of the week on which the scheduled audit is run (if the
    -- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | How often the scheduled audit occurs.
    frequency :: Prelude.Maybe AuditFrequency,
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
-- 'dayOfMonth', 'scheduledAuditMetadata_dayOfMonth' - The day of the month on which the scheduled audit is run (if the
-- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
-- does not have that many days, the audit takes place on the \"LAST\" day
-- of the month.
--
-- 'dayOfWeek', 'scheduledAuditMetadata_dayOfWeek' - The day of the week on which the scheduled audit is run (if the
-- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
--
-- 'frequency', 'scheduledAuditMetadata_frequency' - How often the scheduled audit occurs.
--
-- 'scheduledAuditArn', 'scheduledAuditMetadata_scheduledAuditArn' - The ARN of the scheduled audit.
--
-- 'scheduledAuditName', 'scheduledAuditMetadata_scheduledAuditName' - The name of the scheduled audit.
newScheduledAuditMetadata ::
  ScheduledAuditMetadata
newScheduledAuditMetadata =
  ScheduledAuditMetadata'
    { dayOfMonth =
        Prelude.Nothing,
      dayOfWeek = Prelude.Nothing,
      frequency = Prelude.Nothing,
      scheduledAuditArn = Prelude.Nothing,
      scheduledAuditName = Prelude.Nothing
    }

-- | The day of the month on which the scheduled audit is run (if the
-- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
-- does not have that many days, the audit takes place on the \"LAST\" day
-- of the month.
scheduledAuditMetadata_dayOfMonth :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_dayOfMonth = Lens.lens (\ScheduledAuditMetadata' {dayOfMonth} -> dayOfMonth) (\s@ScheduledAuditMetadata' {} a -> s {dayOfMonth = a} :: ScheduledAuditMetadata)

-- | The day of the week on which the scheduled audit is run (if the
-- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
scheduledAuditMetadata_dayOfWeek :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe DayOfWeek)
scheduledAuditMetadata_dayOfWeek = Lens.lens (\ScheduledAuditMetadata' {dayOfWeek} -> dayOfWeek) (\s@ScheduledAuditMetadata' {} a -> s {dayOfWeek = a} :: ScheduledAuditMetadata)

-- | How often the scheduled audit occurs.
scheduledAuditMetadata_frequency :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe AuditFrequency)
scheduledAuditMetadata_frequency = Lens.lens (\ScheduledAuditMetadata' {frequency} -> frequency) (\s@ScheduledAuditMetadata' {} a -> s {frequency = a} :: ScheduledAuditMetadata)

-- | The ARN of the scheduled audit.
scheduledAuditMetadata_scheduledAuditArn :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_scheduledAuditArn = Lens.lens (\ScheduledAuditMetadata' {scheduledAuditArn} -> scheduledAuditArn) (\s@ScheduledAuditMetadata' {} a -> s {scheduledAuditArn = a} :: ScheduledAuditMetadata)

-- | The name of the scheduled audit.
scheduledAuditMetadata_scheduledAuditName :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_scheduledAuditName = Lens.lens (\ScheduledAuditMetadata' {scheduledAuditName} -> scheduledAuditName) (\s@ScheduledAuditMetadata' {} a -> s {scheduledAuditName = a} :: ScheduledAuditMetadata)

instance Data.FromJSON ScheduledAuditMetadata where
  parseJSON =
    Data.withObject
      "ScheduledAuditMetadata"
      ( \x ->
          ScheduledAuditMetadata'
            Prelude.<$> (x Data..:? "dayOfMonth")
            Prelude.<*> (x Data..:? "dayOfWeek")
            Prelude.<*> (x Data..:? "frequency")
            Prelude.<*> (x Data..:? "scheduledAuditArn")
            Prelude.<*> (x Data..:? "scheduledAuditName")
      )

instance Prelude.Hashable ScheduledAuditMetadata where
  hashWithSalt _salt ScheduledAuditMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` dayOfMonth
      `Prelude.hashWithSalt` dayOfWeek
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` scheduledAuditArn
      `Prelude.hashWithSalt` scheduledAuditName

instance Prelude.NFData ScheduledAuditMetadata where
  rnf ScheduledAuditMetadata' {..} =
    Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf scheduledAuditArn
      `Prelude.seq` Prelude.rnf scheduledAuditName
