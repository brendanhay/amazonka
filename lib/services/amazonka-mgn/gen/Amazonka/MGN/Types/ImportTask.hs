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
-- Module      : Amazonka.MGN.Types.ImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ImportTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ImportStatus
import Amazonka.MGN.Types.ImportTaskSummary
import Amazonka.MGN.Types.S3BucketSource
import qualified Amazonka.Prelude as Prelude

-- | Import task.
--
-- /See:/ 'newImportTask' smart constructor.
data ImportTask = ImportTask'
  { -- | Import task creation datetime.
    creationDateTime :: Prelude.Maybe Prelude.Text,
    -- | Import task end datetime.
    endDateTime :: Prelude.Maybe Prelude.Text,
    -- | Import task id.
    importID :: Prelude.Maybe Prelude.Text,
    -- | Import task progress percentage.
    progressPercentage :: Prelude.Maybe Prelude.Double,
    -- | Import task s3 bucket source.
    s3BucketSource :: Prelude.Maybe S3BucketSource,
    -- | Import task status.
    status :: Prelude.Maybe ImportStatus,
    -- | Import task summary.
    summary :: Prelude.Maybe ImportTaskSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'importTask_creationDateTime' - Import task creation datetime.
--
-- 'endDateTime', 'importTask_endDateTime' - Import task end datetime.
--
-- 'importID', 'importTask_importID' - Import task id.
--
-- 'progressPercentage', 'importTask_progressPercentage' - Import task progress percentage.
--
-- 's3BucketSource', 'importTask_s3BucketSource' - Import task s3 bucket source.
--
-- 'status', 'importTask_status' - Import task status.
--
-- 'summary', 'importTask_summary' - Import task summary.
newImportTask ::
  ImportTask
newImportTask =
  ImportTask'
    { creationDateTime = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      importID = Prelude.Nothing,
      progressPercentage = Prelude.Nothing,
      s3BucketSource = Prelude.Nothing,
      status = Prelude.Nothing,
      summary = Prelude.Nothing
    }

-- | Import task creation datetime.
importTask_creationDateTime :: Lens.Lens' ImportTask (Prelude.Maybe Prelude.Text)
importTask_creationDateTime = Lens.lens (\ImportTask' {creationDateTime} -> creationDateTime) (\s@ImportTask' {} a -> s {creationDateTime = a} :: ImportTask)

-- | Import task end datetime.
importTask_endDateTime :: Lens.Lens' ImportTask (Prelude.Maybe Prelude.Text)
importTask_endDateTime = Lens.lens (\ImportTask' {endDateTime} -> endDateTime) (\s@ImportTask' {} a -> s {endDateTime = a} :: ImportTask)

-- | Import task id.
importTask_importID :: Lens.Lens' ImportTask (Prelude.Maybe Prelude.Text)
importTask_importID = Lens.lens (\ImportTask' {importID} -> importID) (\s@ImportTask' {} a -> s {importID = a} :: ImportTask)

-- | Import task progress percentage.
importTask_progressPercentage :: Lens.Lens' ImportTask (Prelude.Maybe Prelude.Double)
importTask_progressPercentage = Lens.lens (\ImportTask' {progressPercentage} -> progressPercentage) (\s@ImportTask' {} a -> s {progressPercentage = a} :: ImportTask)

-- | Import task s3 bucket source.
importTask_s3BucketSource :: Lens.Lens' ImportTask (Prelude.Maybe S3BucketSource)
importTask_s3BucketSource = Lens.lens (\ImportTask' {s3BucketSource} -> s3BucketSource) (\s@ImportTask' {} a -> s {s3BucketSource = a} :: ImportTask)

-- | Import task status.
importTask_status :: Lens.Lens' ImportTask (Prelude.Maybe ImportStatus)
importTask_status = Lens.lens (\ImportTask' {status} -> status) (\s@ImportTask' {} a -> s {status = a} :: ImportTask)

-- | Import task summary.
importTask_summary :: Lens.Lens' ImportTask (Prelude.Maybe ImportTaskSummary)
importTask_summary = Lens.lens (\ImportTask' {summary} -> summary) (\s@ImportTask' {} a -> s {summary = a} :: ImportTask)

instance Data.FromJSON ImportTask where
  parseJSON =
    Data.withObject
      "ImportTask"
      ( \x ->
          ImportTask'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "endDateTime")
            Prelude.<*> (x Data..:? "importID")
            Prelude.<*> (x Data..:? "progressPercentage")
            Prelude.<*> (x Data..:? "s3BucketSource")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "summary")
      )

instance Prelude.Hashable ImportTask where
  hashWithSalt _salt ImportTask' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` importID
      `Prelude.hashWithSalt` progressPercentage
      `Prelude.hashWithSalt` s3BucketSource
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` summary

instance Prelude.NFData ImportTask where
  rnf ImportTask' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf importID
      `Prelude.seq` Prelude.rnf progressPercentage
      `Prelude.seq` Prelude.rnf s3BucketSource
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf summary
