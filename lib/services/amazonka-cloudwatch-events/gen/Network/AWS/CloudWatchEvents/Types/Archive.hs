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
import qualified Network.AWS.Prelude as Prelude

-- | An @Archive@ object that contains details about an archive.
--
-- /See:/ 'newArchive' smart constructor.
data Archive = Archive'
  { -- | The time stamp for the time that the archive was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The size of the archive, in bytes.
    sizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the event bus associated with the archive. Only events from
    -- this event bus are sent to the archive.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the archive.
    state :: Prelude.Maybe ArchiveState,
    -- | The number of events in the archive.
    eventCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of the archive.
    archiveName :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain events in the archive before they are
    -- deleted.
    retentionDays :: Prelude.Maybe Prelude.Natural,
    -- | A description for the reason that the archive is in the current state.
    stateReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Archive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'archive_creationTime' - The time stamp for the time that the archive was created.
--
-- 'sizeBytes', 'archive_sizeBytes' - The size of the archive, in bytes.
--
-- 'eventSourceArn', 'archive_eventSourceArn' - The ARN of the event bus associated with the archive. Only events from
-- this event bus are sent to the archive.
--
-- 'state', 'archive_state' - The current state of the archive.
--
-- 'eventCount', 'archive_eventCount' - The number of events in the archive.
--
-- 'archiveName', 'archive_archiveName' - The name of the archive.
--
-- 'retentionDays', 'archive_retentionDays' - The number of days to retain events in the archive before they are
-- deleted.
--
-- 'stateReason', 'archive_stateReason' - A description for the reason that the archive is in the current state.
newArchive ::
  Archive
newArchive =
  Archive'
    { creationTime = Prelude.Nothing,
      sizeBytes = Prelude.Nothing,
      eventSourceArn = Prelude.Nothing,
      state = Prelude.Nothing,
      eventCount = Prelude.Nothing,
      archiveName = Prelude.Nothing,
      retentionDays = Prelude.Nothing,
      stateReason = Prelude.Nothing
    }

-- | The time stamp for the time that the archive was created.
archive_creationTime :: Lens.Lens' Archive (Prelude.Maybe Prelude.UTCTime)
archive_creationTime = Lens.lens (\Archive' {creationTime} -> creationTime) (\s@Archive' {} a -> s {creationTime = a} :: Archive) Prelude.. Lens.mapping Core._Time

-- | The size of the archive, in bytes.
archive_sizeBytes :: Lens.Lens' Archive (Prelude.Maybe Prelude.Integer)
archive_sizeBytes = Lens.lens (\Archive' {sizeBytes} -> sizeBytes) (\s@Archive' {} a -> s {sizeBytes = a} :: Archive)

-- | The ARN of the event bus associated with the archive. Only events from
-- this event bus are sent to the archive.
archive_eventSourceArn :: Lens.Lens' Archive (Prelude.Maybe Prelude.Text)
archive_eventSourceArn = Lens.lens (\Archive' {eventSourceArn} -> eventSourceArn) (\s@Archive' {} a -> s {eventSourceArn = a} :: Archive)

-- | The current state of the archive.
archive_state :: Lens.Lens' Archive (Prelude.Maybe ArchiveState)
archive_state = Lens.lens (\Archive' {state} -> state) (\s@Archive' {} a -> s {state = a} :: Archive)

-- | The number of events in the archive.
archive_eventCount :: Lens.Lens' Archive (Prelude.Maybe Prelude.Integer)
archive_eventCount = Lens.lens (\Archive' {eventCount} -> eventCount) (\s@Archive' {} a -> s {eventCount = a} :: Archive)

-- | The name of the archive.
archive_archiveName :: Lens.Lens' Archive (Prelude.Maybe Prelude.Text)
archive_archiveName = Lens.lens (\Archive' {archiveName} -> archiveName) (\s@Archive' {} a -> s {archiveName = a} :: Archive)

-- | The number of days to retain events in the archive before they are
-- deleted.
archive_retentionDays :: Lens.Lens' Archive (Prelude.Maybe Prelude.Natural)
archive_retentionDays = Lens.lens (\Archive' {retentionDays} -> retentionDays) (\s@Archive' {} a -> s {retentionDays = a} :: Archive)

-- | A description for the reason that the archive is in the current state.
archive_stateReason :: Lens.Lens' Archive (Prelude.Maybe Prelude.Text)
archive_stateReason = Lens.lens (\Archive' {stateReason} -> stateReason) (\s@Archive' {} a -> s {stateReason = a} :: Archive)

instance Core.FromJSON Archive where
  parseJSON =
    Core.withObject
      "Archive"
      ( \x ->
          Archive'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "SizeBytes")
            Prelude.<*> (x Core..:? "EventSourceArn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "EventCount")
            Prelude.<*> (x Core..:? "ArchiveName")
            Prelude.<*> (x Core..:? "RetentionDays")
            Prelude.<*> (x Core..:? "StateReason")
      )

instance Prelude.Hashable Archive

instance Prelude.NFData Archive
