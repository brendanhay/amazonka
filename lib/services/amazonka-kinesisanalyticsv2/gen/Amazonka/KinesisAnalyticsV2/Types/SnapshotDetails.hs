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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.SnapshotDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.SnapshotDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.SnapshotStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides details about a snapshot of application state.
--
-- /See:/ 'newSnapshotDetails' smart constructor.
data SnapshotDetails = SnapshotDetails'
  { -- | The timestamp of the application snapshot.
    snapshotCreationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The identifier for the application snapshot.
    snapshotName :: Prelude.Text,
    -- | The status of the application snapshot.
    snapshotStatus :: SnapshotStatus,
    -- | The current application version ID when the snapshot was created.
    applicationVersionId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotCreationTimestamp', 'snapshotDetails_snapshotCreationTimestamp' - The timestamp of the application snapshot.
--
-- 'snapshotName', 'snapshotDetails_snapshotName' - The identifier for the application snapshot.
--
-- 'snapshotStatus', 'snapshotDetails_snapshotStatus' - The status of the application snapshot.
--
-- 'applicationVersionId', 'snapshotDetails_applicationVersionId' - The current application version ID when the snapshot was created.
newSnapshotDetails ::
  -- | 'snapshotName'
  Prelude.Text ->
  -- | 'snapshotStatus'
  SnapshotStatus ->
  -- | 'applicationVersionId'
  Prelude.Natural ->
  SnapshotDetails
newSnapshotDetails
  pSnapshotName_
  pSnapshotStatus_
  pApplicationVersionId_ =
    SnapshotDetails'
      { snapshotCreationTimestamp =
          Prelude.Nothing,
        snapshotName = pSnapshotName_,
        snapshotStatus = pSnapshotStatus_,
        applicationVersionId = pApplicationVersionId_
      }

-- | The timestamp of the application snapshot.
snapshotDetails_snapshotCreationTimestamp :: Lens.Lens' SnapshotDetails (Prelude.Maybe Prelude.UTCTime)
snapshotDetails_snapshotCreationTimestamp = Lens.lens (\SnapshotDetails' {snapshotCreationTimestamp} -> snapshotCreationTimestamp) (\s@SnapshotDetails' {} a -> s {snapshotCreationTimestamp = a} :: SnapshotDetails) Prelude.. Lens.mapping Core._Time

-- | The identifier for the application snapshot.
snapshotDetails_snapshotName :: Lens.Lens' SnapshotDetails Prelude.Text
snapshotDetails_snapshotName = Lens.lens (\SnapshotDetails' {snapshotName} -> snapshotName) (\s@SnapshotDetails' {} a -> s {snapshotName = a} :: SnapshotDetails)

-- | The status of the application snapshot.
snapshotDetails_snapshotStatus :: Lens.Lens' SnapshotDetails SnapshotStatus
snapshotDetails_snapshotStatus = Lens.lens (\SnapshotDetails' {snapshotStatus} -> snapshotStatus) (\s@SnapshotDetails' {} a -> s {snapshotStatus = a} :: SnapshotDetails)

-- | The current application version ID when the snapshot was created.
snapshotDetails_applicationVersionId :: Lens.Lens' SnapshotDetails Prelude.Natural
snapshotDetails_applicationVersionId = Lens.lens (\SnapshotDetails' {applicationVersionId} -> applicationVersionId) (\s@SnapshotDetails' {} a -> s {applicationVersionId = a} :: SnapshotDetails)

instance Core.FromJSON SnapshotDetails where
  parseJSON =
    Core.withObject
      "SnapshotDetails"
      ( \x ->
          SnapshotDetails'
            Prelude.<$> (x Core..:? "SnapshotCreationTimestamp")
            Prelude.<*> (x Core..: "SnapshotName")
            Prelude.<*> (x Core..: "SnapshotStatus")
            Prelude.<*> (x Core..: "ApplicationVersionId")
      )

instance Prelude.Hashable SnapshotDetails where
  hashWithSalt _salt SnapshotDetails' {..} =
    _salt
      `Prelude.hashWithSalt` snapshotCreationTimestamp
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` snapshotStatus
      `Prelude.hashWithSalt` applicationVersionId

instance Prelude.NFData SnapshotDetails where
  rnf SnapshotDetails' {..} =
    Prelude.rnf snapshotCreationTimestamp
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf snapshotStatus
      `Prelude.seq` Prelude.rnf applicationVersionId
