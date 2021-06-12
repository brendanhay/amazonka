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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AuditFrequency
import Network.AWS.IoT.Types.DayOfWeek
import qualified Network.AWS.Lens as Lens

-- | Information about the scheduled audit.
--
-- /See:/ 'newScheduledAuditMetadata' smart constructor.
data ScheduledAuditMetadata = ScheduledAuditMetadata'
  { -- | The day of the week on which the scheduled audit is run (if the
    -- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
    dayOfWeek :: Core.Maybe DayOfWeek,
    -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Core.Maybe Core.Text,
    -- | The name of the scheduled audit.
    scheduledAuditName :: Core.Maybe Core.Text,
    -- | The day of the month on which the scheduled audit is run (if the
    -- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
    -- does not have that many days, the audit takes place on the \"LAST\" day
    -- of the month.
    dayOfMonth :: Core.Maybe Core.Text,
    -- | How often the scheduled audit occurs.
    frequency :: Core.Maybe AuditFrequency
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { dayOfWeek = Core.Nothing,
      scheduledAuditArn = Core.Nothing,
      scheduledAuditName = Core.Nothing,
      dayOfMonth = Core.Nothing,
      frequency = Core.Nothing
    }

-- | The day of the week on which the scheduled audit is run (if the
-- @frequency@ is \"WEEKLY\" or \"BIWEEKLY\").
scheduledAuditMetadata_dayOfWeek :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe DayOfWeek)
scheduledAuditMetadata_dayOfWeek = Lens.lens (\ScheduledAuditMetadata' {dayOfWeek} -> dayOfWeek) (\s@ScheduledAuditMetadata' {} a -> s {dayOfWeek = a} :: ScheduledAuditMetadata)

-- | The ARN of the scheduled audit.
scheduledAuditMetadata_scheduledAuditArn :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe Core.Text)
scheduledAuditMetadata_scheduledAuditArn = Lens.lens (\ScheduledAuditMetadata' {scheduledAuditArn} -> scheduledAuditArn) (\s@ScheduledAuditMetadata' {} a -> s {scheduledAuditArn = a} :: ScheduledAuditMetadata)

-- | The name of the scheduled audit.
scheduledAuditMetadata_scheduledAuditName :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe Core.Text)
scheduledAuditMetadata_scheduledAuditName = Lens.lens (\ScheduledAuditMetadata' {scheduledAuditName} -> scheduledAuditName) (\s@ScheduledAuditMetadata' {} a -> s {scheduledAuditName = a} :: ScheduledAuditMetadata)

-- | The day of the month on which the scheduled audit is run (if the
-- @frequency@ is \"MONTHLY\"). If days 29-31 are specified, and the month
-- does not have that many days, the audit takes place on the \"LAST\" day
-- of the month.
scheduledAuditMetadata_dayOfMonth :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe Core.Text)
scheduledAuditMetadata_dayOfMonth = Lens.lens (\ScheduledAuditMetadata' {dayOfMonth} -> dayOfMonth) (\s@ScheduledAuditMetadata' {} a -> s {dayOfMonth = a} :: ScheduledAuditMetadata)

-- | How often the scheduled audit occurs.
scheduledAuditMetadata_frequency :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe AuditFrequency)
scheduledAuditMetadata_frequency = Lens.lens (\ScheduledAuditMetadata' {frequency} -> frequency) (\s@ScheduledAuditMetadata' {} a -> s {frequency = a} :: ScheduledAuditMetadata)

instance Core.FromJSON ScheduledAuditMetadata where
  parseJSON =
    Core.withObject
      "ScheduledAuditMetadata"
      ( \x ->
          ScheduledAuditMetadata'
            Core.<$> (x Core..:? "dayOfWeek")
            Core.<*> (x Core..:? "scheduledAuditArn")
            Core.<*> (x Core..:? "scheduledAuditName")
            Core.<*> (x Core..:? "dayOfMonth")
            Core.<*> (x Core..:? "frequency")
      )

instance Core.Hashable ScheduledAuditMetadata

instance Core.NFData ScheduledAuditMetadata
