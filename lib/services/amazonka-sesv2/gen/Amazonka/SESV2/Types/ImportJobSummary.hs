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
-- Module      : Amazonka.SESV2.Types.ImportJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ImportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.ImportDestination
import Amazonka.SESV2.Types.JobStatus

-- | A summary of the import job.
--
-- /See:/ 'newImportJobSummary' smart constructor.
data ImportJobSummary = ImportJobSummary'
  { jobId :: Prelude.Maybe Prelude.Text,
    importDestination :: Prelude.Maybe ImportDestination,
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The date and time when the import job was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'importJobSummary_jobId' - Undocumented member.
--
-- 'importDestination', 'importJobSummary_importDestination' - Undocumented member.
--
-- 'jobStatus', 'importJobSummary_jobStatus' - Undocumented member.
--
-- 'createdTimestamp', 'importJobSummary_createdTimestamp' - The date and time when the import job was created.
newImportJobSummary ::
  ImportJobSummary
newImportJobSummary =
  ImportJobSummary'
    { jobId = Prelude.Nothing,
      importDestination = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing
    }

-- | Undocumented member.
importJobSummary_jobId :: Lens.Lens' ImportJobSummary (Prelude.Maybe Prelude.Text)
importJobSummary_jobId = Lens.lens (\ImportJobSummary' {jobId} -> jobId) (\s@ImportJobSummary' {} a -> s {jobId = a} :: ImportJobSummary)

-- | Undocumented member.
importJobSummary_importDestination :: Lens.Lens' ImportJobSummary (Prelude.Maybe ImportDestination)
importJobSummary_importDestination = Lens.lens (\ImportJobSummary' {importDestination} -> importDestination) (\s@ImportJobSummary' {} a -> s {importDestination = a} :: ImportJobSummary)

-- | Undocumented member.
importJobSummary_jobStatus :: Lens.Lens' ImportJobSummary (Prelude.Maybe JobStatus)
importJobSummary_jobStatus = Lens.lens (\ImportJobSummary' {jobStatus} -> jobStatus) (\s@ImportJobSummary' {} a -> s {jobStatus = a} :: ImportJobSummary)

-- | The date and time when the import job was created.
importJobSummary_createdTimestamp :: Lens.Lens' ImportJobSummary (Prelude.Maybe Prelude.UTCTime)
importJobSummary_createdTimestamp = Lens.lens (\ImportJobSummary' {createdTimestamp} -> createdTimestamp) (\s@ImportJobSummary' {} a -> s {createdTimestamp = a} :: ImportJobSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ImportJobSummary where
  parseJSON =
    Core.withObject
      "ImportJobSummary"
      ( \x ->
          ImportJobSummary'
            Prelude.<$> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "ImportDestination")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
      )

instance Prelude.Hashable ImportJobSummary where
  hashWithSalt _salt ImportJobSummary' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` importDestination
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData ImportJobSummary where
  rnf ImportJobSummary' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf importDestination
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf createdTimestamp
