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
-- Module      : Amazonka.Proton.Types.RepositorySyncAttempt
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.RepositorySyncAttempt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.RepositorySyncEvent
import Amazonka.Proton.Types.RepositorySyncStatus

-- | Detail data for a repository sync attempt activated by a push to a
-- repository.
--
-- /See:/ 'newRepositorySyncAttempt' smart constructor.
data RepositorySyncAttempt = RepositorySyncAttempt'
  { -- | Detail data for sync attempt events.
    events :: [RepositorySyncEvent],
    -- | The time when the sync attempt started.
    startedAt :: Core.POSIX,
    -- | The sync attempt status.
    status :: RepositorySyncStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositorySyncAttempt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'repositorySyncAttempt_events' - Detail data for sync attempt events.
--
-- 'startedAt', 'repositorySyncAttempt_startedAt' - The time when the sync attempt started.
--
-- 'status', 'repositorySyncAttempt_status' - The sync attempt status.
newRepositorySyncAttempt ::
  -- | 'startedAt'
  Prelude.UTCTime ->
  -- | 'status'
  RepositorySyncStatus ->
  RepositorySyncAttempt
newRepositorySyncAttempt pStartedAt_ pStatus_ =
  RepositorySyncAttempt'
    { events = Prelude.mempty,
      startedAt = Core._Time Lens.# pStartedAt_,
      status = pStatus_
    }

-- | Detail data for sync attempt events.
repositorySyncAttempt_events :: Lens.Lens' RepositorySyncAttempt [RepositorySyncEvent]
repositorySyncAttempt_events = Lens.lens (\RepositorySyncAttempt' {events} -> events) (\s@RepositorySyncAttempt' {} a -> s {events = a} :: RepositorySyncAttempt) Prelude.. Lens.coerced

-- | The time when the sync attempt started.
repositorySyncAttempt_startedAt :: Lens.Lens' RepositorySyncAttempt Prelude.UTCTime
repositorySyncAttempt_startedAt = Lens.lens (\RepositorySyncAttempt' {startedAt} -> startedAt) (\s@RepositorySyncAttempt' {} a -> s {startedAt = a} :: RepositorySyncAttempt) Prelude.. Core._Time

-- | The sync attempt status.
repositorySyncAttempt_status :: Lens.Lens' RepositorySyncAttempt RepositorySyncStatus
repositorySyncAttempt_status = Lens.lens (\RepositorySyncAttempt' {status} -> status) (\s@RepositorySyncAttempt' {} a -> s {status = a} :: RepositorySyncAttempt)

instance Core.FromJSON RepositorySyncAttempt where
  parseJSON =
    Core.withObject
      "RepositorySyncAttempt"
      ( \x ->
          RepositorySyncAttempt'
            Prelude.<$> (x Core..:? "events" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "startedAt")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable RepositorySyncAttempt where
  hashWithSalt _salt RepositorySyncAttempt' {..} =
    _salt `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData RepositorySyncAttempt where
  rnf RepositorySyncAttempt' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf status
