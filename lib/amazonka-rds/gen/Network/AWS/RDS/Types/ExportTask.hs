{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ExportTask
  ( ExportTask (..),

    -- * Smart constructor
    mkExportTask,

    -- * Lenses
    etTotalExtractedDataInGB,
    etStatus,
    etIAMRoleARN,
    etSourceARN,
    etExportOnly,
    etTaskStartTime,
    etWarningMessage,
    etSnapshotTime,
    etKMSKeyId,
    etTaskEndTime,
    etExportTaskIdentifier,
    etS3Prefix,
    etPercentProgress,
    etS3Bucket,
    etFailureCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the details of a snapshot export to Amazon S3.
--
-- This data type is used as a response element in the @DescribeExportTasks@ action.
--
-- /See:/ 'mkExportTask' smart constructor.
data ExportTask = ExportTask'
  { totalExtractedDataInGB ::
      Lude.Maybe Lude.Int,
    status :: Lude.Maybe Lude.Text,
    iamRoleARN :: Lude.Maybe Lude.Text,
    sourceARN :: Lude.Maybe Lude.Text,
    exportOnly :: Lude.Maybe [Lude.Text],
    taskStartTime :: Lude.Maybe Lude.DateTime,
    warningMessage :: Lude.Maybe Lude.Text,
    snapshotTime :: Lude.Maybe Lude.DateTime,
    kmsKeyId :: Lude.Maybe Lude.Text,
    taskEndTime :: Lude.Maybe Lude.DateTime,
    exportTaskIdentifier :: Lude.Maybe Lude.Text,
    s3Prefix :: Lude.Maybe Lude.Text,
    percentProgress :: Lude.Maybe Lude.Int,
    s3Bucket :: Lude.Maybe Lude.Text,
    failureCause :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- * 'exportOnly' - The data exported from the snapshot. Valid values are the following:
--
--
--     * @database@ - Export all the data from a specified database.
--
--
--     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.
--
--
--     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
--
--     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
--
-- * 'exportTaskIdentifier' - A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is exported to.
-- * 'failureCause' - The reason the export failed, if it failed.
-- * 'iamRoleARN' - The name of the IAM role that is used to write to Amazon S3 when exporting a snapshot.
-- * 'kmsKeyId' - The ID of the AWS KMS key that is used to encrypt the snapshot when it's exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The IAM role used for the snapshot export must have encryption and decryption permissions to use this KMS key.
-- * 'percentProgress' - The progress of the snapshot export task as a percentage.
-- * 's3Bucket' - The Amazon S3 bucket that the snapshot is exported to.
-- * 's3Prefix' - The Amazon S3 bucket prefix that is the file name and path of the exported snapshot.
-- * 'snapshotTime' - The time that the snapshot was created.
-- * 'sourceARN' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
-- * 'status' - The progress status of the export task.
-- * 'taskEndTime' - The time that the snapshot export task completed.
-- * 'taskStartTime' - The time that the snapshot export task started.
-- * 'totalExtractedDataInGB' - The total amount of data exported, in gigabytes.
-- * 'warningMessage' - A warning about the snapshot export task.
mkExportTask ::
  ExportTask
mkExportTask =
  ExportTask'
    { totalExtractedDataInGB = Lude.Nothing,
      status = Lude.Nothing,
      iamRoleARN = Lude.Nothing,
      sourceARN = Lude.Nothing,
      exportOnly = Lude.Nothing,
      taskStartTime = Lude.Nothing,
      warningMessage = Lude.Nothing,
      snapshotTime = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      taskEndTime = Lude.Nothing,
      exportTaskIdentifier = Lude.Nothing,
      s3Prefix = Lude.Nothing,
      percentProgress = Lude.Nothing,
      s3Bucket = Lude.Nothing,
      failureCause = Lude.Nothing
    }

-- | The total amount of data exported, in gigabytes.
--
-- /Note:/ Consider using 'totalExtractedDataInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTotalExtractedDataInGB :: Lens.Lens' ExportTask (Lude.Maybe Lude.Int)
etTotalExtractedDataInGB = Lens.lens (totalExtractedDataInGB :: ExportTask -> Lude.Maybe Lude.Int) (\s a -> s {totalExtractedDataInGB = a} :: ExportTask)
{-# DEPRECATED etTotalExtractedDataInGB "Use generic-lens or generic-optics with 'totalExtractedDataInGB' instead." #-}

-- | The progress status of the export task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatus :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etStatus = Lens.lens (status :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ExportTask)
{-# DEPRECATED etStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the IAM role that is used to write to Amazon S3 when exporting a snapshot.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etIAMRoleARN :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etIAMRoleARN = Lens.lens (iamRoleARN :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: ExportTask)
{-# DEPRECATED etIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etSourceARN :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etSourceARN = Lens.lens (sourceARN :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: ExportTask)
{-# DEPRECATED etSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | The data exported from the snapshot. Valid values are the following:
--
--
--     * @database@ - Export all the data from a specified database.
--
--
--     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.
--
--
--     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
--
--     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
--
--
-- /Note:/ Consider using 'exportOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportOnly :: Lens.Lens' ExportTask (Lude.Maybe [Lude.Text])
etExportOnly = Lens.lens (exportOnly :: ExportTask -> Lude.Maybe [Lude.Text]) (\s a -> s {exportOnly = a} :: ExportTask)
{-# DEPRECATED etExportOnly "Use generic-lens or generic-optics with 'exportOnly' instead." #-}

-- | The time that the snapshot export task started.
--
-- /Note:/ Consider using 'taskStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskStartTime :: Lens.Lens' ExportTask (Lude.Maybe Lude.DateTime)
etTaskStartTime = Lens.lens (taskStartTime :: ExportTask -> Lude.Maybe Lude.DateTime) (\s a -> s {taskStartTime = a} :: ExportTask)
{-# DEPRECATED etTaskStartTime "Use generic-lens or generic-optics with 'taskStartTime' instead." #-}

-- | A warning about the snapshot export task.
--
-- /Note:/ Consider using 'warningMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etWarningMessage :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etWarningMessage = Lens.lens (warningMessage :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {warningMessage = a} :: ExportTask)
{-# DEPRECATED etWarningMessage "Use generic-lens or generic-optics with 'warningMessage' instead." #-}

-- | The time that the snapshot was created.
--
-- /Note:/ Consider using 'snapshotTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etSnapshotTime :: Lens.Lens' ExportTask (Lude.Maybe Lude.DateTime)
etSnapshotTime = Lens.lens (snapshotTime :: ExportTask -> Lude.Maybe Lude.DateTime) (\s a -> s {snapshotTime = a} :: ExportTask)
{-# DEPRECATED etSnapshotTime "Use generic-lens or generic-optics with 'snapshotTime' instead." #-}

-- | The ID of the AWS KMS key that is used to encrypt the snapshot when it's exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The IAM role used for the snapshot export must have encryption and decryption permissions to use this KMS key.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etKMSKeyId :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etKMSKeyId = Lens.lens (kmsKeyId :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ExportTask)
{-# DEPRECATED etKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The time that the snapshot export task completed.
--
-- /Note:/ Consider using 'taskEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskEndTime :: Lens.Lens' ExportTask (Lude.Maybe Lude.DateTime)
etTaskEndTime = Lens.lens (taskEndTime :: ExportTask -> Lude.Maybe Lude.DateTime) (\s a -> s {taskEndTime = a} :: ExportTask)
{-# DEPRECATED etTaskEndTime "Use generic-lens or generic-optics with 'taskEndTime' instead." #-}

-- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is exported to.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportTaskIdentifier :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etExportTaskIdentifier = Lens.lens (exportTaskIdentifier :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {exportTaskIdentifier = a} :: ExportTask)
{-# DEPRECATED etExportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead." #-}

-- | The Amazon S3 bucket prefix that is the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etS3Prefix :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etS3Prefix = Lens.lens (s3Prefix :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: ExportTask)
{-# DEPRECATED etS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The progress of the snapshot export task as a percentage.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etPercentProgress :: Lens.Lens' ExportTask (Lude.Maybe Lude.Int)
etPercentProgress = Lens.lens (percentProgress :: ExportTask -> Lude.Maybe Lude.Int) (\s a -> s {percentProgress = a} :: ExportTask)
{-# DEPRECATED etPercentProgress "Use generic-lens or generic-optics with 'percentProgress' instead." #-}

-- | The Amazon S3 bucket that the snapshot is exported to.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etS3Bucket :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etS3Bucket = Lens.lens (s3Bucket :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: ExportTask)
{-# DEPRECATED etS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The reason the export failed, if it failed.
--
-- /Note:/ Consider using 'failureCause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etFailureCause :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etFailureCause = Lens.lens (failureCause :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {failureCause = a} :: ExportTask)
{-# DEPRECATED etFailureCause "Use generic-lens or generic-optics with 'failureCause' instead." #-}

instance Lude.FromXML ExportTask where
  parseXML x =
    ExportTask'
      Lude.<$> (x Lude..@? "TotalExtractedDataInGB")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "IamRoleArn")
      Lude.<*> (x Lude..@? "SourceArn")
      Lude.<*> ( x Lude..@? "ExportOnly" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "TaskStartTime")
      Lude.<*> (x Lude..@? "WarningMessage")
      Lude.<*> (x Lude..@? "SnapshotTime")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "TaskEndTime")
      Lude.<*> (x Lude..@? "ExportTaskIdentifier")
      Lude.<*> (x Lude..@? "S3Prefix")
      Lude.<*> (x Lude..@? "PercentProgress")
      Lude.<*> (x Lude..@? "S3Bucket")
      Lude.<*> (x Lude..@? "FailureCause")
