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
-- Module      : Network.AWS.IoT.Types.ScheduledAuditMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ScheduledAuditMetadata where

import Network.AWS.IoT.Types.AuditFrequency
import Network.AWS.IoT.Types.DayOfWeek
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the scheduled audit.
--
-- /See:/ 'newScheduledAuditMetadata' smart constructor.
data ScheduledAuditMetadata = ScheduledAuditMetadata'
  { -- | The day of the week on which the scheduled audit is run (if the
    -- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled audit.
    scheduledAuditName :: Prelude.Maybe Prelude.Text,
    -- | The day of the month on which the scheduled audit is run (if the
    -- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
    -- does not have that many days, the audit takes place on the \"LAST\" day
    -- of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Text,
    -- | How often the scheduled audit occurs.
    frequency :: Prelude.Maybe AuditFrequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledAuditMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'scheduledAuditMetadata_dayOfWeek' - The day of the week on which the scheduled audit is run (if the
-- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
--
-- 'scheduledAuditArn', 'scheduledAuditMetadata_scheduledAuditArn' - The ARN of the scheduled audit.
--
-- 'scheduledAuditName', 'scheduledAuditMetadata_scheduledAuditName' - The name of the scheduled audit.
--
-- 'dayOfMonth', 'scheduledAuditMetadata_dayOfMonth' - The day of the month on which the scheduled audit is run (if the
-- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
-- does not have that many days, the audit takes place on the \"LAST\" day
-- of the month.
--
-- 'frequency', 'scheduledAuditMetadata_frequency' - How often the scheduled audit occurs.
newScheduledAuditMetadata ::
  ScheduledAuditMetadata
newScheduledAuditMetadata =
  ScheduledAuditMetadata'
    { dayOfWeek =
        Prelude.Nothing,
      scheduledAuditArn = Prelude.Nothing,
      scheduledAuditName = Prelude.Nothing,
      dayOfMonth = Prelude.Nothing,
      frequency = Prelude.Nothing
    }

-- | The day of the week on which the scheduled audit is run (if the
-- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
scheduledAuditMetadata_dayOfWeek :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe DayOfWeek)
scheduledAuditMetadata_dayOfWeek = Lens.lens (\ScheduledAuditMetadata' {dayOfWeek} -> dayOfWeek) (\s@ScheduledAuditMetadata' {} a -> s {dayOfWeek = a} :: ScheduledAuditMetadata)

-- | The ARN of the scheduled audit.
scheduledAuditMetadata_scheduledAuditArn :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_scheduledAuditArn = Lens.lens (\ScheduledAuditMetadata' {scheduledAuditArn} -> scheduledAuditArn) (\s@ScheduledAuditMetadata' {} a -> s {scheduledAuditArn = a} :: ScheduledAuditMetadata)

-- | The name of the scheduled audit.
scheduledAuditMetadata_scheduledAuditName :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_scheduledAuditName = Lens.lens (\ScheduledAuditMetadata' {scheduledAuditName} -> scheduledAuditName) (\s@ScheduledAuditMetadata' {} a -> s {scheduledAuditName = a} :: ScheduledAuditMetadata)

-- | The day of the month on which the scheduled audit is run (if the
-- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
-- does not have that many days, the audit takes place on the \"LAST\" day
-- of the month.
scheduledAuditMetadata_dayOfMonth :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe Prelude.Text)
scheduledAuditMetadata_dayOfMonth = Lens.lens (\ScheduledAuditMetadata' {dayOfMonth} -> dayOfMonth) (\s@ScheduledAuditMetadata' {} a -> s {dayOfMonth = a} :: ScheduledAuditMetadata)

-- | How often the scheduled audit occurs.
scheduledAuditMetadata_frequency :: Lens.Lens' ScheduledAuditMetadata (Prelude.Maybe AuditFrequency)
scheduledAuditMetadata_frequency = Lens.lens (\ScheduledAuditMetadata' {frequency} -> frequency) (\s@ScheduledAuditMetadata' {} a -> s {frequency = a} :: ScheduledAuditMetadata)

instance Prelude.FromJSON ScheduledAuditMetadata where
  parseJSON =
    Prelude.withObject
      "ScheduledAuditMetadata"
      ( \x ->
          ScheduledAuditMetadata'
            Prelude.<$> (x Prelude..:? "dayOfWeek")
            Prelude.<*> (x Prelude..:? "scheduledAuditArn")
            Prelude.<*> (x Prelude..:? "scheduledAuditName")
            Prelude.<*> (x Prelude..:? "dayOfMonth")
            Prelude.<*> (x Prelude..:? "frequency")
      )

instance Prelude.Hashable ScheduledAuditMetadata

instance Prelude.NFData ScheduledAuditMetadata
