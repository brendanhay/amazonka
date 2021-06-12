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
-- Module      : Network.AWS.CloudWatchEvents.Types.Archive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Archive where

import Network.AWS.CloudWatchEvents.Types.ArchiveState
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An @Archive@ object that contains details about an archive.
--
-- /See:/ 'newArchive' smart constructor.
data Archive = Archive'
  { -- | The number of events in the archive.
    eventCount :: Core.Maybe Core.Integer,
    -- | The ARN of the event bus associated with the archive. Only events from
    -- this event bus are sent to the archive.
    eventSourceArn :: Core.Maybe Core.Text,
    -- | The time stamp for the time that the archive was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | A description for the reason that the archive is in the current state.
    stateReason :: Core.Maybe Core.Text,
    -- | The name of the archive.
    archiveName :: Core.Maybe Core.Text,
    -- | The current state of the archive.
    state :: Core.Maybe ArchiveState,
    -- | The size of the archive, in bytes.
    sizeBytes :: Core.Maybe Core.Integer,
    -- | The number of days to retain events in the archive before they are
    -- deleted.
    retentionDays :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Archive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventCount', 'archive_eventCount' - The number of events in the archive.
--
-- 'eventSourceArn', 'archive_eventSourceArn' - The ARN of the event bus associated with the archive. Only events from
-- this event bus are sent to the archive.
--
-- 'creationTime', 'archive_creationTime' - The time stamp for the time that the archive was created.
--
-- 'stateReason', 'archive_stateReason' - A description for the reason that the archive is in the current state.
--
-- 'archiveName', 'archive_archiveName' - The name of the archive.
--
-- 'state', 'archive_state' - The current state of the archive.
--
-- 'sizeBytes', 'archive_sizeBytes' - The size of the archive, in bytes.
--
-- 'retentionDays', 'archive_retentionDays' - The number of days to retain events in the archive before they are
-- deleted.
newArchive ::
  Archive
newArchive =
  Archive'
    { eventCount = Core.Nothing,
      eventSourceArn = Core.Nothing,
      creationTime = Core.Nothing,
      stateReason = Core.Nothing,
      archiveName = Core.Nothing,
      state = Core.Nothing,
      sizeBytes = Core.Nothing,
      retentionDays = Core.Nothing
    }

-- | The number of events in the archive.
archive_eventCount :: Lens.Lens' Archive (Core.Maybe Core.Integer)
archive_eventCount = Lens.lens (\Archive' {eventCount} -> eventCount) (\s@Archive' {} a -> s {eventCount = a} :: Archive)

-- | The ARN of the event bus associated with the archive. Only events from
-- this event bus are sent to the archive.
archive_eventSourceArn :: Lens.Lens' Archive (Core.Maybe Core.Text)
archive_eventSourceArn = Lens.lens (\Archive' {eventSourceArn} -> eventSourceArn) (\s@Archive' {} a -> s {eventSourceArn = a} :: Archive)

-- | The time stamp for the time that the archive was created.
archive_creationTime :: Lens.Lens' Archive (Core.Maybe Core.UTCTime)
archive_creationTime = Lens.lens (\Archive' {creationTime} -> creationTime) (\s@Archive' {} a -> s {creationTime = a} :: Archive) Core.. Lens.mapping Core._Time

-- | A description for the reason that the archive is in the current state.
archive_stateReason :: Lens.Lens' Archive (Core.Maybe Core.Text)
archive_stateReason = Lens.lens (\Archive' {stateReason} -> stateReason) (\s@Archive' {} a -> s {stateReason = a} :: Archive)

-- | The name of the archive.
archive_archiveName :: Lens.Lens' Archive (Core.Maybe Core.Text)
archive_archiveName = Lens.lens (\Archive' {archiveName} -> archiveName) (\s@Archive' {} a -> s {archiveName = a} :: Archive)

-- | The current state of the archive.
archive_state :: Lens.Lens' Archive (Core.Maybe ArchiveState)
archive_state = Lens.lens (\Archive' {state} -> state) (\s@Archive' {} a -> s {state = a} :: Archive)

-- | The size of the archive, in bytes.
archive_sizeBytes :: Lens.Lens' Archive (Core.Maybe Core.Integer)
archive_sizeBytes = Lens.lens (\Archive' {sizeBytes} -> sizeBytes) (\s@Archive' {} a -> s {sizeBytes = a} :: Archive)

-- | The number of days to retain events in the archive before they are
-- deleted.
archive_retentionDays :: Lens.Lens' Archive (Core.Maybe Core.Natural)
archive_retentionDays = Lens.lens (\Archive' {retentionDays} -> retentionDays) (\s@Archive' {} a -> s {retentionDays = a} :: Archive)

instance Core.FromJSON Archive where
  parseJSON =
    Core.withObject
      "Archive"
      ( \x ->
          Archive'
            Core.<$> (x Core..:? "EventCount")
            Core.<*> (x Core..:? "EventSourceArn")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (x Core..:? "ArchiveName")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "SizeBytes")
            Core.<*> (x Core..:? "RetentionDays")
      )

instance Core.Hashable Archive

instance Core.NFData Archive
