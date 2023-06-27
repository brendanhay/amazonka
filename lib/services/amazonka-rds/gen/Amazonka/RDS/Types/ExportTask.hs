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
-- Copyright   : (c) 2013-2023 Brendan Hay
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

-- | Contains the details of a snapshot or cluster export to Amazon S3.
--
-- This data type is used as a response element in the
-- @DescribeExportTasks@ action.
--
-- /See:/ 'newExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | The data exported from the snapshot or cluster. Valid values are the
    -- following:
    --
    -- -   @database@ - Export all the data from a specified database.
    --
    -- -   @database.table@ /table-name/ - Export a table of the snapshot or
    --     cluster. This format is valid only for RDS for MySQL, RDS for
    --     MariaDB, and Aurora MySQL.
    --
    -- -   @database.schema@ /schema-name/ - Export a database schema of the
    --     snapshot or cluster. This format is valid only for RDS for
    --     PostgreSQL and Aurora PostgreSQL.
    --
    -- -   @database.schema.table@ /table-name/ - Export a table of the
    --     database schema. This format is valid only for RDS for PostgreSQL
    --     and Aurora PostgreSQL.
    exportOnly :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for the snapshot or cluster export task. This ID
    -- isn\'t an identifier for the Amazon S3 bucket where the data is
    -- exported.
    exportTaskIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The reason the export failed, if it failed.
    failureCause :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role that is used to write to Amazon S3 when
    -- exporting a snapshot or cluster.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The key identifier of the Amazon Web Services KMS key that is used to
    -- encrypt the data when it\'s exported to Amazon S3. The KMS key
    -- identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
    -- role used for the export must have encryption and decryption permissions
    -- to use this KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The progress of the snapshot or cluster export task as a percentage.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | The Amazon S3 bucket that the snapshot or cluster is exported to.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket prefix that is the file name and path of the
    -- exported data.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The time that the snapshot was created.
    snapshotTime :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the snapshot or cluster exported to
    -- Amazon S3.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of source for the export.
    sourceType :: Prelude.Maybe ExportSourceType,
    -- | The progress status of the export task. The status can be one of the
    -- following:
    --
    -- -   @CANCELED@
    --
    -- -   @CANCELING@
    --
    -- -   @COMPLETE@
    --
    -- -   @FAILED@
    --
    -- -   @IN_PROGRESS@
    --
    -- -   @STARTING@
    status :: Prelude.Maybe Prelude.Text,
    -- | The time that the snapshot or cluster export task ended.
    taskEndTime :: Prelude.Maybe Data.ISO8601,
    -- | The time that the snapshot or cluster export task started.
    taskStartTime :: Prelude.Maybe Data.ISO8601,
    -- | The total amount of data exported, in gigabytes.
    totalExtractedDataInGB :: Prelude.Maybe Prelude.Int,
    -- | A warning about the snapshot or cluster export task.
    warningMessage :: Prelude.Maybe Prelude.Text
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
-- 'exportOnly', 'exportTask_exportOnly' - The data exported from the snapshot or cluster. Valid values are the
-- following:
--
-- -   @database@ - Export all the data from a specified database.
--
-- -   @database.table@ /table-name/ - Export a table of the snapshot or
--     cluster. This format is valid only for RDS for MySQL, RDS for
--     MariaDB, and Aurora MySQL.
--
-- -   @database.schema@ /schema-name/ - Export a database schema of the
--     snapshot or cluster. This format is valid only for RDS for
--     PostgreSQL and Aurora PostgreSQL.
--
-- -   @database.schema.table@ /table-name/ - Export a table of the
--     database schema. This format is valid only for RDS for PostgreSQL
--     and Aurora PostgreSQL.
--
-- 'exportTaskIdentifier', 'exportTask_exportTaskIdentifier' - A unique identifier for the snapshot or cluster export task. This ID
-- isn\'t an identifier for the Amazon S3 bucket where the data is
-- exported.
--
-- 'failureCause', 'exportTask_failureCause' - The reason the export failed, if it failed.
--
-- 'iamRoleArn', 'exportTask_iamRoleArn' - The name of the IAM role that is used to write to Amazon S3 when
-- exporting a snapshot or cluster.
--
-- 'kmsKeyId', 'exportTask_kmsKeyId' - The key identifier of the Amazon Web Services KMS key that is used to
-- encrypt the data when it\'s exported to Amazon S3. The KMS key
-- identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
-- role used for the export must have encryption and decryption permissions
-- to use this KMS key.
--
-- 'percentProgress', 'exportTask_percentProgress' - The progress of the snapshot or cluster export task as a percentage.
--
-- 's3Bucket', 'exportTask_s3Bucket' - The Amazon S3 bucket that the snapshot or cluster is exported to.
--
-- 's3Prefix', 'exportTask_s3Prefix' - The Amazon S3 bucket prefix that is the file name and path of the
-- exported data.
--
-- 'snapshotTime', 'exportTask_snapshotTime' - The time that the snapshot was created.
--
-- 'sourceArn', 'exportTask_sourceArn' - The Amazon Resource Name (ARN) of the snapshot or cluster exported to
-- Amazon S3.
--
-- 'sourceType', 'exportTask_sourceType' - The type of source for the export.
--
-- 'status', 'exportTask_status' - The progress status of the export task. The status can be one of the
-- following:
--
-- -   @CANCELED@
--
-- -   @CANCELING@
--
-- -   @COMPLETE@
--
-- -   @FAILED@
--
-- -   @IN_PROGRESS@
--
-- -   @STARTING@
--
-- 'taskEndTime', 'exportTask_taskEndTime' - The time that the snapshot or cluster export task ended.
--
-- 'taskStartTime', 'exportTask_taskStartTime' - The time that the snapshot or cluster export task started.
--
-- 'totalExtractedDataInGB', 'exportTask_totalExtractedDataInGB' - The total amount of data exported, in gigabytes.
--
-- 'warningMessage', 'exportTask_warningMessage' - A warning about the snapshot or cluster export task.
newExportTask ::
  ExportTask
newExportTask =
  ExportTask'
    { exportOnly = Prelude.Nothing,
      exportTaskIdentifier = Prelude.Nothing,
      failureCause = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3Prefix = Prelude.Nothing,
      snapshotTime = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      taskEndTime = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      totalExtractedDataInGB = Prelude.Nothing,
      warningMessage = Prelude.Nothing
    }

-- | The data exported from the snapshot or cluster. Valid values are the
-- following:
--
-- -   @database@ - Export all the data from a specified database.
--
-- -   @database.table@ /table-name/ - Export a table of the snapshot or
--     cluster. This format is valid only for RDS for MySQL, RDS for
--     MariaDB, and Aurora MySQL.
--
-- -   @database.schema@ /schema-name/ - Export a database schema of the
--     snapshot or cluster. This format is valid only for RDS for
--     PostgreSQL and Aurora PostgreSQL.
--
-- -   @database.schema.table@ /table-name/ - Export a table of the
--     database schema. This format is valid only for RDS for PostgreSQL
--     and Aurora PostgreSQL.
exportTask_exportOnly :: Lens.Lens' ExportTask (Prelude.Maybe [Prelude.Text])
exportTask_exportOnly = Lens.lens (\ExportTask' {exportOnly} -> exportOnly) (\s@ExportTask' {} a -> s {exportOnly = a} :: ExportTask) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the snapshot or cluster export task. This ID
-- isn\'t an identifier for the Amazon S3 bucket where the data is
-- exported.
exportTask_exportTaskIdentifier :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_exportTaskIdentifier = Lens.lens (\ExportTask' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@ExportTask' {} a -> s {exportTaskIdentifier = a} :: ExportTask)

-- | The reason the export failed, if it failed.
exportTask_failureCause :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_failureCause = Lens.lens (\ExportTask' {failureCause} -> failureCause) (\s@ExportTask' {} a -> s {failureCause = a} :: ExportTask)

-- | The name of the IAM role that is used to write to Amazon S3 when
-- exporting a snapshot or cluster.
exportTask_iamRoleArn :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_iamRoleArn = Lens.lens (\ExportTask' {iamRoleArn} -> iamRoleArn) (\s@ExportTask' {} a -> s {iamRoleArn = a} :: ExportTask)

-- | The key identifier of the Amazon Web Services KMS key that is used to
-- encrypt the data when it\'s exported to Amazon S3. The KMS key
-- identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
-- role used for the export must have encryption and decryption permissions
-- to use this KMS key.
exportTask_kmsKeyId :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_kmsKeyId = Lens.lens (\ExportTask' {kmsKeyId} -> kmsKeyId) (\s@ExportTask' {} a -> s {kmsKeyId = a} :: ExportTask)

-- | The progress of the snapshot or cluster export task as a percentage.
exportTask_percentProgress :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Int)
exportTask_percentProgress = Lens.lens (\ExportTask' {percentProgress} -> percentProgress) (\s@ExportTask' {} a -> s {percentProgress = a} :: ExportTask)

-- | The Amazon S3 bucket that the snapshot or cluster is exported to.
exportTask_s3Bucket :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3Bucket = Lens.lens (\ExportTask' {s3Bucket} -> s3Bucket) (\s@ExportTask' {} a -> s {s3Bucket = a} :: ExportTask)

-- | The Amazon S3 bucket prefix that is the file name and path of the
-- exported data.
exportTask_s3Prefix :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3Prefix = Lens.lens (\ExportTask' {s3Prefix} -> s3Prefix) (\s@ExportTask' {} a -> s {s3Prefix = a} :: ExportTask)

-- | The time that the snapshot was created.
exportTask_snapshotTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_snapshotTime = Lens.lens (\ExportTask' {snapshotTime} -> snapshotTime) (\s@ExportTask' {} a -> s {snapshotTime = a} :: ExportTask) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the snapshot or cluster exported to
-- Amazon S3.
exportTask_sourceArn :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_sourceArn = Lens.lens (\ExportTask' {sourceArn} -> sourceArn) (\s@ExportTask' {} a -> s {sourceArn = a} :: ExportTask)

-- | The type of source for the export.
exportTask_sourceType :: Lens.Lens' ExportTask (Prelude.Maybe ExportSourceType)
exportTask_sourceType = Lens.lens (\ExportTask' {sourceType} -> sourceType) (\s@ExportTask' {} a -> s {sourceType = a} :: ExportTask)

-- | The progress status of the export task. The status can be one of the
-- following:
--
-- -   @CANCELED@
--
-- -   @CANCELING@
--
-- -   @COMPLETE@
--
-- -   @FAILED@
--
-- -   @IN_PROGRESS@
--
-- -   @STARTING@
exportTask_status :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_status = Lens.lens (\ExportTask' {status} -> status) (\s@ExportTask' {} a -> s {status = a} :: ExportTask)

-- | The time that the snapshot or cluster export task ended.
exportTask_taskEndTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_taskEndTime = Lens.lens (\ExportTask' {taskEndTime} -> taskEndTime) (\s@ExportTask' {} a -> s {taskEndTime = a} :: ExportTask) Prelude.. Lens.mapping Data._Time

-- | The time that the snapshot or cluster export task started.
exportTask_taskStartTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_taskStartTime = Lens.lens (\ExportTask' {taskStartTime} -> taskStartTime) (\s@ExportTask' {} a -> s {taskStartTime = a} :: ExportTask) Prelude.. Lens.mapping Data._Time

-- | The total amount of data exported, in gigabytes.
exportTask_totalExtractedDataInGB :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Int)
exportTask_totalExtractedDataInGB = Lens.lens (\ExportTask' {totalExtractedDataInGB} -> totalExtractedDataInGB) (\s@ExportTask' {} a -> s {totalExtractedDataInGB = a} :: ExportTask)

-- | A warning about the snapshot or cluster export task.
exportTask_warningMessage :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_warningMessage = Lens.lens (\ExportTask' {warningMessage} -> warningMessage) (\s@ExportTask' {} a -> s {warningMessage = a} :: ExportTask)

instance Data.FromXML ExportTask where
  parseXML x =
    ExportTask'
      Prelude.<$> ( x
                      Data..@? "ExportOnly"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "ExportTaskIdentifier")
      Prelude.<*> (x Data..@? "FailureCause")
      Prelude.<*> (x Data..@? "IamRoleArn")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "PercentProgress")
      Prelude.<*> (x Data..@? "S3Bucket")
      Prelude.<*> (x Data..@? "S3Prefix")
      Prelude.<*> (x Data..@? "SnapshotTime")
      Prelude.<*> (x Data..@? "SourceArn")
      Prelude.<*> (x Data..@? "SourceType")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "TaskEndTime")
      Prelude.<*> (x Data..@? "TaskStartTime")
      Prelude.<*> (x Data..@? "TotalExtractedDataInGB")
      Prelude.<*> (x Data..@? "WarningMessage")

instance Prelude.Hashable ExportTask where
  hashWithSalt _salt ExportTask' {..} =
    _salt
      `Prelude.hashWithSalt` exportOnly
      `Prelude.hashWithSalt` exportTaskIdentifier
      `Prelude.hashWithSalt` failureCause
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` snapshotTime
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` taskEndTime
      `Prelude.hashWithSalt` taskStartTime
      `Prelude.hashWithSalt` totalExtractedDataInGB
      `Prelude.hashWithSalt` warningMessage

instance Prelude.NFData ExportTask where
  rnf ExportTask' {..} =
    Prelude.rnf exportOnly
      `Prelude.seq` Prelude.rnf exportTaskIdentifier
      `Prelude.seq` Prelude.rnf failureCause
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Prefix
      `Prelude.seq` Prelude.rnf snapshotTime
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf taskEndTime
      `Prelude.seq` Prelude.rnf taskStartTime
      `Prelude.seq` Prelude.rnf totalExtractedDataInGB
      `Prelude.seq` Prelude.rnf warningMessage
