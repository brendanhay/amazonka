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
-- Module      : Amazonka.Panorama.Types.PackageImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageImportJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Panorama.Types.PackageImportJobStatus
import Amazonka.Panorama.Types.PackageImportJobType
import qualified Amazonka.Prelude as Prelude

-- | A job to import a package version.
--
-- /See:/ 'newPackageImportJob' smart constructor.
data PackageImportJob = PackageImportJob'
  { -- | The job\'s status.
    status :: Prelude.Maybe PackageImportJobStatus,
    -- | The job\'s type.
    jobType :: Prelude.Maybe PackageImportJobType,
    -- | When the job was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The job\'s ID.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | When the job was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The job\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'packageImportJob_status' - The job\'s status.
--
-- 'jobType', 'packageImportJob_jobType' - The job\'s type.
--
-- 'lastUpdatedTime', 'packageImportJob_lastUpdatedTime' - When the job was updated.
--
-- 'jobId', 'packageImportJob_jobId' - The job\'s ID.
--
-- 'createdTime', 'packageImportJob_createdTime' - When the job was created.
--
-- 'statusMessage', 'packageImportJob_statusMessage' - The job\'s status message.
newPackageImportJob ::
  PackageImportJob
newPackageImportJob =
  PackageImportJob'
    { status = Prelude.Nothing,
      jobType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      jobId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The job\'s status.
packageImportJob_status :: Lens.Lens' PackageImportJob (Prelude.Maybe PackageImportJobStatus)
packageImportJob_status = Lens.lens (\PackageImportJob' {status} -> status) (\s@PackageImportJob' {} a -> s {status = a} :: PackageImportJob)

-- | The job\'s type.
packageImportJob_jobType :: Lens.Lens' PackageImportJob (Prelude.Maybe PackageImportJobType)
packageImportJob_jobType = Lens.lens (\PackageImportJob' {jobType} -> jobType) (\s@PackageImportJob' {} a -> s {jobType = a} :: PackageImportJob)

-- | When the job was updated.
packageImportJob_lastUpdatedTime :: Lens.Lens' PackageImportJob (Prelude.Maybe Prelude.UTCTime)
packageImportJob_lastUpdatedTime = Lens.lens (\PackageImportJob' {lastUpdatedTime} -> lastUpdatedTime) (\s@PackageImportJob' {} a -> s {lastUpdatedTime = a} :: PackageImportJob) Prelude.. Lens.mapping Core._Time

-- | The job\'s ID.
packageImportJob_jobId :: Lens.Lens' PackageImportJob (Prelude.Maybe Prelude.Text)
packageImportJob_jobId = Lens.lens (\PackageImportJob' {jobId} -> jobId) (\s@PackageImportJob' {} a -> s {jobId = a} :: PackageImportJob)

-- | When the job was created.
packageImportJob_createdTime :: Lens.Lens' PackageImportJob (Prelude.Maybe Prelude.UTCTime)
packageImportJob_createdTime = Lens.lens (\PackageImportJob' {createdTime} -> createdTime) (\s@PackageImportJob' {} a -> s {createdTime = a} :: PackageImportJob) Prelude.. Lens.mapping Core._Time

-- | The job\'s status message.
packageImportJob_statusMessage :: Lens.Lens' PackageImportJob (Prelude.Maybe Prelude.Text)
packageImportJob_statusMessage = Lens.lens (\PackageImportJob' {statusMessage} -> statusMessage) (\s@PackageImportJob' {} a -> s {statusMessage = a} :: PackageImportJob)

instance Core.FromJSON PackageImportJob where
  parseJSON =
    Core.withObject
      "PackageImportJob"
      ( \x ->
          PackageImportJob'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "JobType")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "StatusMessage")
      )

instance Prelude.Hashable PackageImportJob where
  hashWithSalt _salt PackageImportJob' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData PackageImportJob where
  rnf PackageImportJob' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf statusMessage
