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
-- Module      : Network.AWS.SESv2.Types.ImportJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.ImportJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.ImportDestination
import Network.AWS.SESv2.Types.JobStatus

-- | A summary of the import job.
--
-- /See:/ 'newImportJobSummary' smart constructor.
data ImportJobSummary = ImportJobSummary'
  { -- | The date and time when the import job was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    jobStatus :: Prelude.Maybe JobStatus,
    importDestination :: Prelude.Maybe ImportDestination,
    jobId :: Prelude.Maybe Prelude.Text
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
-- 'createdTimestamp', 'importJobSummary_createdTimestamp' - The date and time when the import job was created.
--
-- 'jobStatus', 'importJobSummary_jobStatus' - Undocumented member.
--
-- 'importDestination', 'importJobSummary_importDestination' - Undocumented member.
--
-- 'jobId', 'importJobSummary_jobId' - Undocumented member.
newImportJobSummary ::
  ImportJobSummary
newImportJobSummary =
  ImportJobSummary'
    { createdTimestamp =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      importDestination = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The date and time when the import job was created.
importJobSummary_createdTimestamp :: Lens.Lens' ImportJobSummary (Prelude.Maybe Prelude.UTCTime)
importJobSummary_createdTimestamp = Lens.lens (\ImportJobSummary' {createdTimestamp} -> createdTimestamp) (\s@ImportJobSummary' {} a -> s {createdTimestamp = a} :: ImportJobSummary) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
importJobSummary_jobStatus :: Lens.Lens' ImportJobSummary (Prelude.Maybe JobStatus)
importJobSummary_jobStatus = Lens.lens (\ImportJobSummary' {jobStatus} -> jobStatus) (\s@ImportJobSummary' {} a -> s {jobStatus = a} :: ImportJobSummary)

-- | Undocumented member.
importJobSummary_importDestination :: Lens.Lens' ImportJobSummary (Prelude.Maybe ImportDestination)
importJobSummary_importDestination = Lens.lens (\ImportJobSummary' {importDestination} -> importDestination) (\s@ImportJobSummary' {} a -> s {importDestination = a} :: ImportJobSummary)

-- | Undocumented member.
importJobSummary_jobId :: Lens.Lens' ImportJobSummary (Prelude.Maybe Prelude.Text)
importJobSummary_jobId = Lens.lens (\ImportJobSummary' {jobId} -> jobId) (\s@ImportJobSummary' {} a -> s {jobId = a} :: ImportJobSummary)

instance Core.FromJSON ImportJobSummary where
  parseJSON =
    Core.withObject
      "ImportJobSummary"
      ( \x ->
          ImportJobSummary'
            Prelude.<$> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "ImportDestination")
            Prelude.<*> (x Core..:? "JobId")
      )

instance Prelude.Hashable ImportJobSummary

instance Prelude.NFData ImportJobSummary
