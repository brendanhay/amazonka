{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.RDS.Types.ExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ExportTask where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the details of a snapshot export to Amazon S3.
--
-- This data type is used as a response element in the
-- @DescribeExportTasks@ action.
--
-- /See:/ 'newExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | The time that the snapshot export task completed.
    taskEndTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The name of the IAM role that is used to write to Amazon S3 when
    -- exporting a snapshot.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The progress status of the export task.
    status :: Prelude.Maybe Prelude.Text,
    -- | The total amount of data exported, in gigabytes.
    totalExtractedDataInGB :: Prelude.Maybe Prelude.Int,
    -- | A warning about the snapshot export task.
    warningMessage :: Prelude.Maybe Prelude.Text,
    -- | The time that the snapshot was created.
    snapshotTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The Amazon S3 bucket that the snapshot is exported to.
    s3Bucket :: Prelude.Maybe Prelude.Text,
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
    -- | The key identifier of the AWS KMS customer master key (CMK) that is used
    -- to encrypt the snapshot when it\'s exported to Amazon S3. The AWS KMS
    -- CMK identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
    -- role used for the snapshot export must have encryption and decryption
    -- permissions to use this AWS KMS CMK.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The reason the export failed, if it failed.
    failureCause :: Prelude.Maybe Prelude.Text,
    -- | The progress of the snapshot export task as a percentage.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket prefix that is the file name and path of the
    -- exported snapshot.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The time that the snapshot export task started.
    taskStartTime :: Prelude.Maybe Prelude.ISO8601,
    -- | A unique identifier for the snapshot export task. This ID isn\'t an
    -- identifier for the Amazon S3 bucket where the snapshot is exported to.
    exportTaskIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskEndTime', 'exportTask_taskEndTime' - The time that the snapshot export task completed.
--
-- 'iamRoleArn', 'exportTask_iamRoleArn' - The name of the IAM role that is used to write to Amazon S3 when
-- exporting a snapshot.
--
-- 'status', 'exportTask_status' - The progress status of the export task.
--
-- 'totalExtractedDataInGB', 'exportTask_totalExtractedDataInGB' - The total amount of data exported, in gigabytes.
--
-- 'warningMessage', 'exportTask_warningMessage' - A warning about the snapshot export task.
--
-- 'snapshotTime', 'exportTask_snapshotTime' - The time that the snapshot was created.
--
-- 's3Bucket', 'exportTask_s3Bucket' - The Amazon S3 bucket that the snapshot is exported to.
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
-- 'kmsKeyId', 'exportTask_kmsKeyId' - The key identifier of the AWS KMS customer master key (CMK) that is used
-- to encrypt the snapshot when it\'s exported to Amazon S3. The AWS KMS
-- CMK identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
-- role used for the snapshot export must have encryption and decryption
-- permissions to use this AWS KMS CMK.
--
-- 'failureCause', 'exportTask_failureCause' - The reason the export failed, if it failed.
--
-- 'percentProgress', 'exportTask_percentProgress' - The progress of the snapshot export task as a percentage.
--
-- 'sourceArn', 'exportTask_sourceArn' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- 's3Prefix', 'exportTask_s3Prefix' - The Amazon S3 bucket prefix that is the file name and path of the
-- exported snapshot.
--
-- 'taskStartTime', 'exportTask_taskStartTime' - The time that the snapshot export task started.
--
-- 'exportTaskIdentifier', 'exportTask_exportTaskIdentifier' - A unique identifier for the snapshot export task. This ID isn\'t an
-- identifier for the Amazon S3 bucket where the snapshot is exported to.
newExportTask ::
  ExportTask
newExportTask =
  ExportTask'
    { taskEndTime = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      totalExtractedDataInGB = Prelude.Nothing,
      warningMessage = Prelude.Nothing,
      snapshotTime = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      exportOnly = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      failureCause = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      s3Prefix = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      exportTaskIdentifier = Prelude.Nothing
    }

-- | The time that the snapshot export task completed.
exportTask_taskEndTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_taskEndTime = Lens.lens (\ExportTask' {taskEndTime} -> taskEndTime) (\s@ExportTask' {} a -> s {taskEndTime = a} :: ExportTask) Prelude.. Lens.mapping Prelude._Time

-- | The name of the IAM role that is used to write to Amazon S3 when
-- exporting a snapshot.
exportTask_iamRoleArn :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_iamRoleArn = Lens.lens (\ExportTask' {iamRoleArn} -> iamRoleArn) (\s@ExportTask' {} a -> s {iamRoleArn = a} :: ExportTask)

-- | The progress status of the export task.
exportTask_status :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_status = Lens.lens (\ExportTask' {status} -> status) (\s@ExportTask' {} a -> s {status = a} :: ExportTask)

-- | The total amount of data exported, in gigabytes.
exportTask_totalExtractedDataInGB :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Int)
exportTask_totalExtractedDataInGB = Lens.lens (\ExportTask' {totalExtractedDataInGB} -> totalExtractedDataInGB) (\s@ExportTask' {} a -> s {totalExtractedDataInGB = a} :: ExportTask)

-- | A warning about the snapshot export task.
exportTask_warningMessage :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_warningMessage = Lens.lens (\ExportTask' {warningMessage} -> warningMessage) (\s@ExportTask' {} a -> s {warningMessage = a} :: ExportTask)

-- | The time that the snapshot was created.
exportTask_snapshotTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_snapshotTime = Lens.lens (\ExportTask' {snapshotTime} -> snapshotTime) (\s@ExportTask' {} a -> s {snapshotTime = a} :: ExportTask) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon S3 bucket that the snapshot is exported to.
exportTask_s3Bucket :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3Bucket = Lens.lens (\ExportTask' {s3Bucket} -> s3Bucket) (\s@ExportTask' {} a -> s {s3Bucket = a} :: ExportTask)

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
exportTask_exportOnly = Lens.lens (\ExportTask' {exportOnly} -> exportOnly) (\s@ExportTask' {} a -> s {exportOnly = a} :: ExportTask) Prelude.. Lens.mapping Prelude._Coerce

-- | The key identifier of the AWS KMS customer master key (CMK) that is used
-- to encrypt the snapshot when it\'s exported to Amazon S3. The AWS KMS
-- CMK identifier is its key ARN, key ID, alias ARN, or alias name. The IAM
-- role used for the snapshot export must have encryption and decryption
-- permissions to use this AWS KMS CMK.
exportTask_kmsKeyId :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_kmsKeyId = Lens.lens (\ExportTask' {kmsKeyId} -> kmsKeyId) (\s@ExportTask' {} a -> s {kmsKeyId = a} :: ExportTask)

-- | The reason the export failed, if it failed.
exportTask_failureCause :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_failureCause = Lens.lens (\ExportTask' {failureCause} -> failureCause) (\s@ExportTask' {} a -> s {failureCause = a} :: ExportTask)

-- | The progress of the snapshot export task as a percentage.
exportTask_percentProgress :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Int)
exportTask_percentProgress = Lens.lens (\ExportTask' {percentProgress} -> percentProgress) (\s@ExportTask' {} a -> s {percentProgress = a} :: ExportTask)

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
exportTask_sourceArn :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_sourceArn = Lens.lens (\ExportTask' {sourceArn} -> sourceArn) (\s@ExportTask' {} a -> s {sourceArn = a} :: ExportTask)

-- | The Amazon S3 bucket prefix that is the file name and path of the
-- exported snapshot.
exportTask_s3Prefix :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_s3Prefix = Lens.lens (\ExportTask' {s3Prefix} -> s3Prefix) (\s@ExportTask' {} a -> s {s3Prefix = a} :: ExportTask)

-- | The time that the snapshot export task started.
exportTask_taskStartTime :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.UTCTime)
exportTask_taskStartTime = Lens.lens (\ExportTask' {taskStartTime} -> taskStartTime) (\s@ExportTask' {} a -> s {taskStartTime = a} :: ExportTask) Prelude.. Lens.mapping Prelude._Time

-- | A unique identifier for the snapshot export task. This ID isn\'t an
-- identifier for the Amazon S3 bucket where the snapshot is exported to.
exportTask_exportTaskIdentifier :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_exportTaskIdentifier = Lens.lens (\ExportTask' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@ExportTask' {} a -> s {exportTaskIdentifier = a} :: ExportTask)

instance Prelude.FromXML ExportTask where
  parseXML x =
    ExportTask'
      Prelude.<$> (x Prelude..@? "TaskEndTime")
      Prelude.<*> (x Prelude..@? "IamRoleArn")
      Prelude.<*> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "TotalExtractedDataInGB")
      Prelude.<*> (x Prelude..@? "WarningMessage")
      Prelude.<*> (x Prelude..@? "SnapshotTime")
      Prelude.<*> (x Prelude..@? "S3Bucket")
      Prelude.<*> ( x Prelude..@? "ExportOnly"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "KmsKeyId")
      Prelude.<*> (x Prelude..@? "FailureCause")
      Prelude.<*> (x Prelude..@? "PercentProgress")
      Prelude.<*> (x Prelude..@? "SourceArn")
      Prelude.<*> (x Prelude..@? "S3Prefix")
      Prelude.<*> (x Prelude..@? "TaskStartTime")
      Prelude.<*> (x Prelude..@? "ExportTaskIdentifier")

instance Prelude.Hashable ExportTask

instance Prelude.NFData ExportTask
