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
-- Module      : Amazonka.IotTwinMaker.Types.SyncJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SyncJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.SyncJobStatus
import qualified Amazonka.Prelude as Prelude

-- | The SyncJob summary.
--
-- /See:/ 'newSyncJobSummary' smart constructor.
data SyncJobSummary = SyncJobSummary'
  { -- | The SyncJob summary ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The SyncJob summaries status.
    status :: Prelude.Maybe SyncJobStatus,
    -- | The sync source.
    syncSource :: Prelude.Maybe Prelude.Text,
    -- | The update date and time.
    updateDateTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the workspace that contains the sync job.
    workspaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'syncJobSummary_arn' - The SyncJob summary ARN.
--
-- 'creationDateTime', 'syncJobSummary_creationDateTime' - The creation date and time.
--
-- 'status', 'syncJobSummary_status' - The SyncJob summaries status.
--
-- 'syncSource', 'syncJobSummary_syncSource' - The sync source.
--
-- 'updateDateTime', 'syncJobSummary_updateDateTime' - The update date and time.
--
-- 'workspaceId', 'syncJobSummary_workspaceId' - The ID of the workspace that contains the sync job.
newSyncJobSummary ::
  SyncJobSummary
newSyncJobSummary =
  SyncJobSummary'
    { arn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      syncSource = Prelude.Nothing,
      updateDateTime = Prelude.Nothing,
      workspaceId = Prelude.Nothing
    }

-- | The SyncJob summary ARN.
syncJobSummary_arn :: Lens.Lens' SyncJobSummary (Prelude.Maybe Prelude.Text)
syncJobSummary_arn = Lens.lens (\SyncJobSummary' {arn} -> arn) (\s@SyncJobSummary' {} a -> s {arn = a} :: SyncJobSummary)

-- | The creation date and time.
syncJobSummary_creationDateTime :: Lens.Lens' SyncJobSummary (Prelude.Maybe Prelude.UTCTime)
syncJobSummary_creationDateTime = Lens.lens (\SyncJobSummary' {creationDateTime} -> creationDateTime) (\s@SyncJobSummary' {} a -> s {creationDateTime = a} :: SyncJobSummary) Prelude.. Lens.mapping Data._Time

-- | The SyncJob summaries status.
syncJobSummary_status :: Lens.Lens' SyncJobSummary (Prelude.Maybe SyncJobStatus)
syncJobSummary_status = Lens.lens (\SyncJobSummary' {status} -> status) (\s@SyncJobSummary' {} a -> s {status = a} :: SyncJobSummary)

-- | The sync source.
syncJobSummary_syncSource :: Lens.Lens' SyncJobSummary (Prelude.Maybe Prelude.Text)
syncJobSummary_syncSource = Lens.lens (\SyncJobSummary' {syncSource} -> syncSource) (\s@SyncJobSummary' {} a -> s {syncSource = a} :: SyncJobSummary)

-- | The update date and time.
syncJobSummary_updateDateTime :: Lens.Lens' SyncJobSummary (Prelude.Maybe Prelude.UTCTime)
syncJobSummary_updateDateTime = Lens.lens (\SyncJobSummary' {updateDateTime} -> updateDateTime) (\s@SyncJobSummary' {} a -> s {updateDateTime = a} :: SyncJobSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the workspace that contains the sync job.
syncJobSummary_workspaceId :: Lens.Lens' SyncJobSummary (Prelude.Maybe Prelude.Text)
syncJobSummary_workspaceId = Lens.lens (\SyncJobSummary' {workspaceId} -> workspaceId) (\s@SyncJobSummary' {} a -> s {workspaceId = a} :: SyncJobSummary)

instance Data.FromJSON SyncJobSummary where
  parseJSON =
    Data.withObject
      "SyncJobSummary"
      ( \x ->
          SyncJobSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "syncSource")
            Prelude.<*> (x Data..:? "updateDateTime")
            Prelude.<*> (x Data..:? "workspaceId")
      )

instance Prelude.Hashable SyncJobSummary where
  hashWithSalt _salt SyncJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` syncSource
      `Prelude.hashWithSalt` updateDateTime
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData SyncJobSummary where
  rnf SyncJobSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf syncSource
      `Prelude.seq` Prelude.rnf updateDateTime
      `Prelude.seq` Prelude.rnf workspaceId
