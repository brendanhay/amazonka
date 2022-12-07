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
-- Module      : Amazonka.RDS.Types.ExportTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ExportTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.ExportSourceType

-- | Contains the details of a snapshot export to Amazon S3.
--
-- This data type is used as a response element in the
-- @DescribeExportTasks@ action.
--
-- /See:/ 'newExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | The Amazon S3 bucket that the snapshot is exported to.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The progress of the snapshot export task as a percentage.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | The time that the snapshot export task started.
    taskStartTime :: Prelude.Maybe Data.ISO8601,
    -- | The data exported from the snapshot. Valid values are the following:
    --
    -- -   @database@ - Export all the data from a specified database.
    --
    -- -   @database.table@ /table-name/ - Export a table of the snapshot. This
    --     format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora
    --     MySQL.
    --
    -- -   @database.schema@ /schema-name/ - Export a database schema of the
    --     snapshot. This format is valid only for RDS for PostgreSQL and
    --     Aurora PostgreSQL.
    --
    -- -   @database.schema.table@ /table-name/ - Export a table of the
    --     database schema. This format is valid only for RDS for PostgreSQL
    --     and Aurora PostgreSQL.
    exportOnly :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The total amount of data exported, in gigabytes.
    totalExtractedDataInGB :: Prelude.Maybe Prelude.Int,
    -- | The progress status of the export task.
    status :: Prelude.Maybe Prelude.Text,
    -- | The type of source for the export.
    sourceType :: Prelude.Maybe ExportSourceType,
    -- | A warning about the snapshot export task.
    warningMessage :: Prelude.Maybe Prelude.Text,
    -- | The time that the snapshot was created.
    snapshotTime :: Prelude.Maybe Data.ISO8601,
    -- | A unique identifier for the snapshot export task. This ID isn\'t an
    -- identifier for the Amazon S3 bucket where the snapshot is exported to.
    exportTaskIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role that is used to write to Amazon S3 when
    -- exporting a snapshot.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The key identifier of the Amazon Web Services KMS key that is used to
    -- encrypt the snapshot when it\'s exported to Amazon S3. The KMS key
    -- identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
    -- role used for the snapshot export must have encryption and decryption
    -- permissions to use this KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The reason the export failed, if it failed.
    failureCause :: Prelude.Maybe Prelude.Text,
    -- | The time that the snapshot export task completed.
    taskEndTime :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon S3 bucket prefix that is the file name and path of the
    -- exported snapshot.
    s3Prefix :: Prelude.Maybe Prelude.Text
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
-- 's3Bucket', 'exportTask_s3Bucket' - The Amazon S3 bucket that the snapshot is exported to.
--
-- 'percentProgress', 'exportTask_percentProgress' - The progress of the snapshot export task as a percentage.
--
-- 'taskStartTime', 'exportTask_taskStartTime' - The time that the snapshot export task started.
--
-- 'exportOnly', 'exportTask_exportOnly' - The data exported from the snapshot. Valid values are the following:
--
-- -   @database@ - Export all the data from a specified database.
--
-- -   @database.table@ /table-name/ - Export a table of the snapshot. This
--     format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora
--     MySQL.
--
-- -   @database.schema@ /schema-name/ - Export a database schema of the
--     snapshot. This format is valid only for RDS for PostgreSQL and
--     Aurora PostgreSQL.
--
-- -   @database.schema.table@ /table-name/ - Export a table of the
--     database schema. This format is valid only for RDS for PostgreSQL
--     and Aurora PostgreSQL.
--
-- 'sourceArn', 'exportTask_sourceArn' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- 'totalExtractedDataInGB', 'exportTask_totalExtractedDataInGB' - The total amount of data exported, in gigabytes.
--
-- 'status', 'exportTask_status' - The progress status of the export task.
--
-- 'sourceType', 'exportTask_sourceType' - The type of source for the export.
--
-- 'warningMessage', 'exportTask_warningMessage' - A warning about the snapshot export task.
--
-- 'snapshotTime', 'exportTask_snapshotTime' - The time that the snapshot was created.
--
-- 'exportTaskIdentifier', 'exportTask_exportTaskIdentifier' - A unique identifier for the snapshot export task. This ID isn\'t an
-- identifier for the Amazon S3 bucket where the snapshot is exported to.
--
-- 'iamRoleArn', 'exportTask_iamRoleArn' - The name of the IAM role that is used to write to Amazon S3 when
-- exporting a snapshot.
--
-- 'kmsKeyId', 'exportTask_kmsKeyId' - The key identifier of the Amazon Web Services KMS key that is used to
-- encrypt the snapshot when it\'s exported to Amazon S3. The KMS key
-- identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
-- role used for the snapshot export must have encryption and decryption
-- permissions to use this KMS key.
--
-- 'failureCause', 'exportTask_failureCause' - The reason the export failed, if it failed.
--
-- 'taskEndTime', 'exportTask_taskEndTime' - The time that the snapshot export task completed.
--
-- 's3Prefix', 'exportTask_s3Prefix' - The Amazon S3 bucket prefix that is the file name and path of the
-- exported snapshot.
newExportTask ::
  ExportTask
newExportTask =
  ExportTask'
    { s3Bucket = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      exportOnly = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      totalExtractedDataInGB = Prelude.Nothing,
      status = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      warningMessage = Prelude.Nothing,
      snapshotTime = Prelude.Nothing,
      exportTaskIdentifier = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      failureCause = Prelude.Nothing,
      taskEndTime = Prelude.Nothing,
      s3Prefix = Prelude.Nothing
    }

-- | The Amazon S3 bucket that the snapshot is exported to.
exportTask_s3Bucket :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3Bucket = Lens.lens (\ExportTask' {s3Bucket} -> s3Bucket) (\s@ExportTask' {} a -> s {s3Bucket = a} :: ExportTask)

-- | The progress of the snapshot export task as a percentage.
exportTask_percentProgress :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Int)
exportTask_percentProgress = Lens.lens (\ExportTask' {percentProgress} -> percentProgress) (\s@ExportTask' {} a -> s {percentProgress = a} :: ExportTask)

-- | The time that the snapshot export task started.
exportTask_taskStartTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_taskStartTime = Lens.lens (\ExportTask' {taskStartTime} -> taskStartTime) (\s@ExportTask' {} a -> s {taskStartTime = a} :: ExportTask) Prelude.. Lens.mapping Data._Time

-- | The data exported from the snapshot. Valid values are the following:
--
-- -   @database@ - Export all the data from a specified database.
--
-- -   @database.table@ /table-name/ - Export a table of the snapshot. This
--     format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora
--     MySQL.
--
-- -   @database.schema@ /schema-name/ - Export a database schema of the
--     snapshot. This format is valid only for RDS for PostgreSQL and
--     Aurora PostgreSQL.
--
-- -   @database.schema.table@ /table-name/ - Export a table of the
--     database schema. This format is valid only for RDS for PostgreSQL
--     and Aurora PostgreSQL.
exportTask_exportOnly :: Lens.Lens' ExportTask (Prelude.Maybe [Prelude.Text])
exportTask_exportOnly = Lens.lens (\ExportTask' {exportOnly} -> exportOnly) (\s@ExportTask' {} a -> s {exportOnly = a} :: ExportTask) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
exportTask_sourceArn :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_sourceArn = Lens.lens (\ExportTask' {sourceArn} -> sourceArn) (\s@ExportTask' {} a -> s {sourceArn = a} :: ExportTask)

-- | The total amount of data exported, in gigabytes.
exportTask_totalExtractedDataInGB :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Int)
exportTask_totalExtractedDataInGB = Lens.lens (\ExportTask' {totalExtractedDataInGB} -> totalExtractedDataInGB) (\s@ExportTask' {} a -> s {totalExtractedDataInGB = a} :: ExportTask)

-- | The progress status of the export task.
exportTask_status :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_status = Lens.lens (\ExportTask' {status} -> status) (\s@ExportTask' {} a -> s {status = a} :: ExportTask)

-- | The type of source for the export.
exportTask_sourceType :: Lens.Lens' ExportTask (Prelude.Maybe ExportSourceType)
exportTask_sourceType = Lens.lens (\ExportTask' {sourceType} -> sourceType) (\s@ExportTask' {} a -> s {sourceType = a} :: ExportTask)

-- | A warning about the snapshot export task.
exportTask_warningMessage :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_warningMessage = Lens.lens (\ExportTask' {warningMessage} -> warningMessage) (\s@ExportTask' {} a -> s {warningMessage = a} :: ExportTask)

-- | The time that the snapshot was created.
exportTask_snapshotTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_snapshotTime = Lens.lens (\ExportTask' {snapshotTime} -> snapshotTime) (\s@ExportTask' {} a -> s {snapshotTime = a} :: ExportTask) Prelude.. Lens.mapping Data._Time

-- | A unique identifier for the snapshot export task. This ID isn\'t an
-- identifier for the Amazon S3 bucket where the snapshot is exported to.
exportTask_exportTaskIdentifier :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_exportTaskIdentifier = Lens.lens (\ExportTask' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@ExportTask' {} a -> s {exportTaskIdentifier = a} :: ExportTask)

-- | The name of the IAM role that is used to write to Amazon S3 when
-- exporting a snapshot.
exportTask_iamRoleArn :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_iamRoleArn = Lens.lens (\ExportTask' {iamRoleArn} -> iamRoleArn) (\s@ExportTask' {} a -> s {iamRoleArn = a} :: ExportTask)

-- | The key identifier of the Amazon Web Services KMS key that is used to
-- encrypt the snapshot when it\'s exported to Amazon S3. The KMS key
-- identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
-- role used for the snapshot export must have encryption and decryption
-- permissions to use this KMS key.
exportTask_kmsKeyId :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_kmsKeyId = Lens.lens (\ExportTask' {kmsKeyId} -> kmsKeyId) (\s@ExportTask' {} a -> s {kmsKeyId = a} :: ExportTask)

-- | The reason the export failed, if it failed.
exportTask_failureCause :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_failureCause = Lens.lens (\ExportTask' {failureCause} -> failureCause) (\s@ExportTask' {} a -> s {failureCause = a} :: ExportTask)

-- | The time that the snapshot export task completed.
exportTask_taskEndTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_taskEndTime = Lens.lens (\ExportTask' {taskEndTime} -> taskEndTime) (\s@ExportTask' {} a -> s {taskEndTime = a} :: ExportTask) Prelude.. Lens.mapping Data._Time

-- | The Amazon S3 bucket prefix that is the file name and path of the
-- exported snapshot.
exportTask_s3Prefix :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3Prefix = Lens.lens (\ExportTask' {s3Prefix} -> s3Prefix) (\s@ExportTask' {} a -> s {s3Prefix = a} :: ExportTask)

instance Data.FromXML ExportTask where
  parseXML x =
    ExportTask'
      Prelude.<$> (x Data..@? "S3Bucket")
      Prelude.<*> (x Data..@? "PercentProgress")
      Prelude.<*> (x Data..@? "TaskStartTime")
      Prelude.<*> ( x Data..@? "ExportOnly" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "SourceArn")
      Prelude.<*> (x Data..@? "TotalExtractedDataInGB")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "SourceType")
      Prelude.<*> (x Data..@? "WarningMessage")
      Prelude.<*> (x Data..@? "SnapshotTime")
      Prelude.<*> (x Data..@? "ExportTaskIdentifier")
      Prelude.<*> (x Data..@? "IamRoleArn")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "FailureCause")
      Prelude.<*> (x Data..@? "TaskEndTime")
      Prelude.<*> (x Data..@? "S3Prefix")

instance Prelude.Hashable ExportTask where
  hashWithSalt _salt ExportTask' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` taskStartTime
      `Prelude.hashWithSalt` exportOnly
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` totalExtractedDataInGB
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` warningMessage
      `Prelude.hashWithSalt` snapshotTime
      `Prelude.hashWithSalt` exportTaskIdentifier
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` failureCause
      `Prelude.hashWithSalt` taskEndTime
      `Prelude.hashWithSalt` s3Prefix

instance Prelude.NFData ExportTask where
  rnf ExportTask' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf taskStartTime
      `Prelude.seq` Prelude.rnf exportOnly
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf totalExtractedDataInGB
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf warningMessage
      `Prelude.seq` Prelude.rnf snapshotTime
      `Prelude.seq` Prelude.rnf exportTaskIdentifier
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf failureCause
      `Prelude.seq` Prelude.rnf taskEndTime
      `Prelude.seq` Prelude.rnf s3Prefix
