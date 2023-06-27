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
-- Module      : Amazonka.MGN.Types.ExportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ExportTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ExportStatus
import Amazonka.MGN.Types.ExportTaskSummary
import qualified Amazonka.Prelude as Prelude

-- | Export task.
--
-- /See:/ 'newExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | Export task creation datetime.
    creationDateTime :: Prelude.Maybe Prelude.Text,
    -- | Export task end datetime.
    endDateTime :: Prelude.Maybe Prelude.Text,
    -- | Export task id.
    exportID :: Prelude.Maybe Prelude.Text,
    -- | Export task progress percentage.
    progressPercentage :: Prelude.Maybe Prelude.Double,
    -- | Export task s3 bucket.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | Export task s3 bucket owner.
    s3BucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Export task s3 key.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | Export task status.
    status :: Prelude.Maybe ExportStatus,
    -- | Export task summary.
    summary :: Prelude.Maybe ExportTaskSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'exportTask_creationDateTime' - Export task creation datetime.
--
-- 'endDateTime', 'exportTask_endDateTime' - Export task end datetime.
--
-- 'exportID', 'exportTask_exportID' - Export task id.
--
-- 'progressPercentage', 'exportTask_progressPercentage' - Export task progress percentage.
--
-- 's3Bucket', 'exportTask_s3Bucket' - Export task s3 bucket.
--
-- 's3BucketOwner', 'exportTask_s3BucketOwner' - Export task s3 bucket owner.
--
-- 's3Key', 'exportTask_s3Key' - Export task s3 key.
--
-- 'status', 'exportTask_status' - Export task status.
--
-- 'summary', 'exportTask_summary' - Export task summary.
newExportTask ::
  ExportTask
newExportTask =
  ExportTask'
    { creationDateTime = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      exportID = Prelude.Nothing,
      progressPercentage = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3BucketOwner = Prelude.Nothing,
      s3Key = Prelude.Nothing,
      status = Prelude.Nothing,
      summary = Prelude.Nothing
    }

-- | Export task creation datetime.
exportTask_creationDateTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_creationDateTime = Lens.lens (\ExportTask' {creationDateTime} -> creationDateTime) (\s@ExportTask' {} a -> s {creationDateTime = a} :: ExportTask)

-- | Export task end datetime.
exportTask_endDateTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_endDateTime = Lens.lens (\ExportTask' {endDateTime} -> endDateTime) (\s@ExportTask' {} a -> s {endDateTime = a} :: ExportTask)

-- | Export task id.
exportTask_exportID :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_exportID = Lens.lens (\ExportTask' {exportID} -> exportID) (\s@ExportTask' {} a -> s {exportID = a} :: ExportTask)

-- | Export task progress percentage.
exportTask_progressPercentage :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Double)
exportTask_progressPercentage = Lens.lens (\ExportTask' {progressPercentage} -> progressPercentage) (\s@ExportTask' {} a -> s {progressPercentage = a} :: ExportTask)

-- | Export task s3 bucket.
exportTask_s3Bucket :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3Bucket = Lens.lens (\ExportTask' {s3Bucket} -> s3Bucket) (\s@ExportTask' {} a -> s {s3Bucket = a} :: ExportTask)

-- | Export task s3 bucket owner.
exportTask_s3BucketOwner :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3BucketOwner = Lens.lens (\ExportTask' {s3BucketOwner} -> s3BucketOwner) (\s@ExportTask' {} a -> s {s3BucketOwner = a} :: ExportTask)

-- | Export task s3 key.
exportTask_s3Key :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3Key = Lens.lens (\ExportTask' {s3Key} -> s3Key) (\s@ExportTask' {} a -> s {s3Key = a} :: ExportTask)

-- | Export task status.
exportTask_status :: Lens.Lens' ExportTask (Prelude.Maybe ExportStatus)
exportTask_status = Lens.lens (\ExportTask' {status} -> status) (\s@ExportTask' {} a -> s {status = a} :: ExportTask)

-- | Export task summary.
exportTask_summary :: Lens.Lens' ExportTask (Prelude.Maybe ExportTaskSummary)
exportTask_summary = Lens.lens (\ExportTask' {summary} -> summary) (\s@ExportTask' {} a -> s {summary = a} :: ExportTask)

instance Data.FromJSON ExportTask where
  parseJSON =
    Data.withObject
      "ExportTask"
      ( \x ->
          ExportTask'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "endDateTime")
            Prelude.<*> (x Data..:? "exportID")
            Prelude.<*> (x Data..:? "progressPercentage")
            Prelude.<*> (x Data..:? "s3Bucket")
            Prelude.<*> (x Data..:? "s3BucketOwner")
            Prelude.<*> (x Data..:? "s3Key")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "summary")
      )

instance Prelude.Hashable ExportTask where
  hashWithSalt _salt ExportTask' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` exportID
      `Prelude.hashWithSalt` progressPercentage
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3BucketOwner
      `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` summary

instance Prelude.NFData ExportTask where
  rnf ExportTask' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf exportID
      `Prelude.seq` Prelude.rnf progressPercentage
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3BucketOwner
      `Prelude.seq` Prelude.rnf s3Key
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf summary
