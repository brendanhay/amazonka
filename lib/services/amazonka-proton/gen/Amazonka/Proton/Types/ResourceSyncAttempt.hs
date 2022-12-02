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
-- Module      : Amazonka.Proton.Types.ResourceSyncAttempt
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ResourceSyncAttempt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.ResourceSyncEvent
import Amazonka.Proton.Types.ResourceSyncStatus
import Amazonka.Proton.Types.Revision

-- | Detail data for a resource sync attempt activated by a push to a
-- repository.
--
-- /See:/ 'newResourceSyncAttempt' smart constructor.
data ResourceSyncAttempt = ResourceSyncAttempt'
  { -- | An array of events with detail data.
    events :: [ResourceSyncEvent],
    -- | Detail data for the initial repository commit, path and push.
    initialRevision :: Revision,
    -- | The time when the sync attempt started.
    startedAt :: Data.POSIX,
    -- | The status of the sync attempt.
    status :: ResourceSyncStatus,
    -- | The resource that is synced to.
    target :: Prelude.Text,
    -- | Detail data for the target revision.
    targetRevision :: Revision
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSyncAttempt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'resourceSyncAttempt_events' - An array of events with detail data.
--
-- 'initialRevision', 'resourceSyncAttempt_initialRevision' - Detail data for the initial repository commit, path and push.
--
-- 'startedAt', 'resourceSyncAttempt_startedAt' - The time when the sync attempt started.
--
-- 'status', 'resourceSyncAttempt_status' - The status of the sync attempt.
--
-- 'target', 'resourceSyncAttempt_target' - The resource that is synced to.
--
-- 'targetRevision', 'resourceSyncAttempt_targetRevision' - Detail data for the target revision.
newResourceSyncAttempt ::
  -- | 'initialRevision'
  Revision ->
  -- | 'startedAt'
  Prelude.UTCTime ->
  -- | 'status'
  ResourceSyncStatus ->
  -- | 'target'
  Prelude.Text ->
  -- | 'targetRevision'
  Revision ->
  ResourceSyncAttempt
newResourceSyncAttempt
  pInitialRevision_
  pStartedAt_
  pStatus_
  pTarget_
  pTargetRevision_ =
    ResourceSyncAttempt'
      { events = Prelude.mempty,
        initialRevision = pInitialRevision_,
        startedAt = Data._Time Lens.# pStartedAt_,
        status = pStatus_,
        target = pTarget_,
        targetRevision = pTargetRevision_
      }

-- | An array of events with detail data.
resourceSyncAttempt_events :: Lens.Lens' ResourceSyncAttempt [ResourceSyncEvent]
resourceSyncAttempt_events = Lens.lens (\ResourceSyncAttempt' {events} -> events) (\s@ResourceSyncAttempt' {} a -> s {events = a} :: ResourceSyncAttempt) Prelude.. Lens.coerced

-- | Detail data for the initial repository commit, path and push.
resourceSyncAttempt_initialRevision :: Lens.Lens' ResourceSyncAttempt Revision
resourceSyncAttempt_initialRevision = Lens.lens (\ResourceSyncAttempt' {initialRevision} -> initialRevision) (\s@ResourceSyncAttempt' {} a -> s {initialRevision = a} :: ResourceSyncAttempt)

-- | The time when the sync attempt started.
resourceSyncAttempt_startedAt :: Lens.Lens' ResourceSyncAttempt Prelude.UTCTime
resourceSyncAttempt_startedAt = Lens.lens (\ResourceSyncAttempt' {startedAt} -> startedAt) (\s@ResourceSyncAttempt' {} a -> s {startedAt = a} :: ResourceSyncAttempt) Prelude.. Data._Time

-- | The status of the sync attempt.
resourceSyncAttempt_status :: Lens.Lens' ResourceSyncAttempt ResourceSyncStatus
resourceSyncAttempt_status = Lens.lens (\ResourceSyncAttempt' {status} -> status) (\s@ResourceSyncAttempt' {} a -> s {status = a} :: ResourceSyncAttempt)

-- | The resource that is synced to.
resourceSyncAttempt_target :: Lens.Lens' ResourceSyncAttempt Prelude.Text
resourceSyncAttempt_target = Lens.lens (\ResourceSyncAttempt' {target} -> target) (\s@ResourceSyncAttempt' {} a -> s {target = a} :: ResourceSyncAttempt)

-- | Detail data for the target revision.
resourceSyncAttempt_targetRevision :: Lens.Lens' ResourceSyncAttempt Revision
resourceSyncAttempt_targetRevision = Lens.lens (\ResourceSyncAttempt' {targetRevision} -> targetRevision) (\s@ResourceSyncAttempt' {} a -> s {targetRevision = a} :: ResourceSyncAttempt)

instance Data.FromJSON ResourceSyncAttempt where
  parseJSON =
    Data.withObject
      "ResourceSyncAttempt"
      ( \x ->
          ResourceSyncAttempt'
            Prelude.<$> (x Data..:? "events" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "initialRevision")
            Prelude.<*> (x Data..: "startedAt")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "target")
            Prelude.<*> (x Data..: "targetRevision")
      )

instance Prelude.Hashable ResourceSyncAttempt where
  hashWithSalt _salt ResourceSyncAttempt' {..} =
    _salt `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` initialRevision
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` targetRevision

instance Prelude.NFData ResourceSyncAttempt where
  rnf ResourceSyncAttempt' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf initialRevision
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf targetRevision
