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
-- Module      : Amazonka.CloudWatchEvents.Types.Archive
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Archive where

import Amazonka.CloudWatchEvents.Types.ArchiveState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An @Archive@ object that contains details about an archive.
--
-- /See:/ 'newArchive' smart constructor.
data Archive = Archive'
  { -- | The name of the archive.
    archiveName :: Prelude.Maybe Prelude.Text,
    -- | The time stamp for the time that the archive was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The number of events in the archive.
    eventCount :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the event bus associated with the archive. Only events from
    -- this event bus are sent to the archive.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain events in the archive before they are
    -- deleted.
    retentionDays :: Prelude.Maybe Prelude.Natural,
    -- | The size of the archive, in bytes.
    sizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The current state of the archive.
    state :: Prelude.Maybe ArchiveState,
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
-- 'archiveName', 'archive_archiveName' - The name of the archive.
--
-- 'creationTime', 'archive_creationTime' - The time stamp for the time that the archive was created.
--
-- 'eventCount', 'archive_eventCount' - The number of events in the archive.
--
-- 'eventSourceArn', 'archive_eventSourceArn' - The ARN of the event bus associated with the archive. Only events from
-- this event bus are sent to the archive.
--
-- 'retentionDays', 'archive_retentionDays' - The number of days to retain events in the archive before they are
-- deleted.
--
-- 'sizeBytes', 'archive_sizeBytes' - The size of the archive, in bytes.
--
-- 'state', 'archive_state' - The current state of the archive.
--
-- 'stateReason', 'archive_stateReason' - A description for the reason that the archive is in the current state.
newArchive ::
  Archive
newArchive =
  Archive'
    { archiveName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      eventCount = Prelude.Nothing,
      eventSourceArn = Prelude.Nothing,
      retentionDays = Prelude.Nothing,
      sizeBytes = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing
    }

-- | The name of the archive.
archive_archiveName :: Lens.Lens' Archive (Prelude.Maybe Prelude.Text)
archive_archiveName = Lens.lens (\Archive' {archiveName} -> archiveName) (\s@Archive' {} a -> s {archiveName = a} :: Archive)

-- | The time stamp for the time that the archive was created.
archive_creationTime :: Lens.Lens' Archive (Prelude.Maybe Prelude.UTCTime)
archive_creationTime = Lens.lens (\Archive' {creationTime} -> creationTime) (\s@Archive' {} a -> s {creationTime = a} :: Archive) Prelude.. Lens.mapping Data._Time

-- | The number of events in the archive.
archive_eventCount :: Lens.Lens' Archive (Prelude.Maybe Prelude.Integer)
archive_eventCount = Lens.lens (\Archive' {eventCount} -> eventCount) (\s@Archive' {} a -> s {eventCount = a} :: Archive)

-- | The ARN of the event bus associated with the archive. Only events from
-- this event bus are sent to the archive.
archive_eventSourceArn :: Lens.Lens' Archive (Prelude.Maybe Prelude.Text)
archive_eventSourceArn = Lens.lens (\Archive' {eventSourceArn} -> eventSourceArn) (\s@Archive' {} a -> s {eventSourceArn = a} :: Archive)

-- | The number of days to retain events in the archive before they are
-- deleted.
archive_retentionDays :: Lens.Lens' Archive (Prelude.Maybe Prelude.Natural)
archive_retentionDays = Lens.lens (\Archive' {retentionDays} -> retentionDays) (\s@Archive' {} a -> s {retentionDays = a} :: Archive)

-- | The size of the archive, in bytes.
archive_sizeBytes :: Lens.Lens' Archive (Prelude.Maybe Prelude.Integer)
archive_sizeBytes = Lens.lens (\Archive' {sizeBytes} -> sizeBytes) (\s@Archive' {} a -> s {sizeBytes = a} :: Archive)

-- | The current state of the archive.
archive_state :: Lens.Lens' Archive (Prelude.Maybe ArchiveState)
archive_state = Lens.lens (\Archive' {state} -> state) (\s@Archive' {} a -> s {state = a} :: Archive)

-- | A description for the reason that the archive is in the current state.
archive_stateReason :: Lens.Lens' Archive (Prelude.Maybe Prelude.Text)
archive_stateReason = Lens.lens (\Archive' {stateReason} -> stateReason) (\s@Archive' {} a -> s {stateReason = a} :: Archive)

instance Data.FromJSON Archive where
  parseJSON =
    Data.withObject
      "Archive"
      ( \x ->
          Archive'
            Prelude.<$> (x Data..:? "ArchiveName")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "EventCount")
            Prelude.<*> (x Data..:? "EventSourceArn")
            Prelude.<*> (x Data..:? "RetentionDays")
            Prelude.<*> (x Data..:? "SizeBytes")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateReason")
      )

instance Prelude.Hashable Archive where
  hashWithSalt _salt Archive' {..} =
    _salt
      `Prelude.hashWithSalt` archiveName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` eventCount
      `Prelude.hashWithSalt` eventSourceArn
      `Prelude.hashWithSalt` retentionDays
      `Prelude.hashWithSalt` sizeBytes
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateReason

instance Prelude.NFData Archive where
  rnf Archive' {..} =
    Prelude.rnf archiveName `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf eventCount `Prelude.seq`
          Prelude.rnf eventSourceArn `Prelude.seq`
            Prelude.rnf retentionDays `Prelude.seq`
              Prelude.rnf sizeBytes `Prelude.seq`
                Prelude.rnf state `Prelude.seq`
                  Prelude.rnf stateReason
