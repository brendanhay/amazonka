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
    etExportOnly,
    etExportTaskIdentifier,
    etFailureCause,
    etIamRoleArn,
    etKmsKeyId,
    etPercentProgress,
    etS3Bucket,
    etS3Prefix,
    etSnapshotTime,
    etSourceArn,
    etStatus,
    etTaskEndTime,
    etTaskStartTime,
    etTotalExtractedDataInGB,
    etWarningMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains the details of a snapshot export to Amazon S3.
--
-- This data type is used as a response element in the @DescribeExportTasks@ action.
--
-- /See:/ 'mkExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | The data exported from the snapshot. Valid values are the following:
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
    exportOnly :: Core.Maybe [Types.String],
    -- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is exported to.
    exportTaskIdentifier :: Core.Maybe Types.String,
    -- | The reason the export failed, if it failed.
    failureCause :: Core.Maybe Types.String,
    -- | The name of the IAM role that is used to write to Amazon S3 when exporting a snapshot.
    iamRoleArn :: Core.Maybe Types.String,
    -- | The ID of the AWS KMS key that is used to encrypt the snapshot when it's exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The IAM role used for the snapshot export must have encryption and decryption permissions to use this KMS key.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The progress of the snapshot export task as a percentage.
    percentProgress :: Core.Maybe Core.Int,
    -- | The Amazon S3 bucket that the snapshot is exported to.
    s3Bucket :: Core.Maybe Types.String,
    -- | The Amazon S3 bucket prefix that is the file name and path of the exported snapshot.
    s3Prefix :: Core.Maybe Types.String,
    -- | The time that the snapshot was created.
    snapshotTime :: Core.Maybe Core.UTCTime,
    -- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
    sourceArn :: Core.Maybe Types.String,
    -- | The progress status of the export task.
    status :: Core.Maybe Types.String,
    -- | The time that the snapshot export task completed.
    taskEndTime :: Core.Maybe Core.UTCTime,
    -- | The time that the snapshot export task started.
    taskStartTime :: Core.Maybe Core.UTCTime,
    -- | The total amount of data exported, in gigabytes.
    totalExtractedDataInGB :: Core.Maybe Core.Int,
    -- | A warning about the snapshot export task.
    warningMessage :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ExportTask' value with any optional fields omitted.
mkExportTask ::
  ExportTask
mkExportTask =
  ExportTask'
    { exportOnly = Core.Nothing,
      exportTaskIdentifier = Core.Nothing,
      failureCause = Core.Nothing,
      iamRoleArn = Core.Nothing,
      kmsKeyId = Core.Nothing,
      percentProgress = Core.Nothing,
      s3Bucket = Core.Nothing,
      s3Prefix = Core.Nothing,
      snapshotTime = Core.Nothing,
      sourceArn = Core.Nothing,
      status = Core.Nothing,
      taskEndTime = Core.Nothing,
      taskStartTime = Core.Nothing,
      totalExtractedDataInGB = Core.Nothing,
      warningMessage = Core.Nothing
    }

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
etExportOnly :: Lens.Lens' ExportTask (Core.Maybe [Types.String])
etExportOnly = Lens.field @"exportOnly"
{-# DEPRECATED etExportOnly "Use generic-lens or generic-optics with 'exportOnly' instead." #-}

-- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is exported to.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportTaskIdentifier :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etExportTaskIdentifier = Lens.field @"exportTaskIdentifier"
{-# DEPRECATED etExportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead." #-}

-- | The reason the export failed, if it failed.
--
-- /Note:/ Consider using 'failureCause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etFailureCause :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etFailureCause = Lens.field @"failureCause"
{-# DEPRECATED etFailureCause "Use generic-lens or generic-optics with 'failureCause' instead." #-}

-- | The name of the IAM role that is used to write to Amazon S3 when exporting a snapshot.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etIamRoleArn :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etIamRoleArn = Lens.field @"iamRoleArn"
{-# DEPRECATED etIamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead." #-}

-- | The ID of the AWS KMS key that is used to encrypt the snapshot when it's exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The IAM role used for the snapshot export must have encryption and decryption permissions to use this KMS key.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etKmsKeyId :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED etKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The progress of the snapshot export task as a percentage.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etPercentProgress :: Lens.Lens' ExportTask (Core.Maybe Core.Int)
etPercentProgress = Lens.field @"percentProgress"
{-# DEPRECATED etPercentProgress "Use generic-lens or generic-optics with 'percentProgress' instead." #-}

-- | The Amazon S3 bucket that the snapshot is exported to.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etS3Bucket :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED etS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The Amazon S3 bucket prefix that is the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etS3Prefix :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etS3Prefix = Lens.field @"s3Prefix"
{-# DEPRECATED etS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The time that the snapshot was created.
--
-- /Note:/ Consider using 'snapshotTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etSnapshotTime :: Lens.Lens' ExportTask (Core.Maybe Core.UTCTime)
etSnapshotTime = Lens.field @"snapshotTime"
{-# DEPRECATED etSnapshotTime "Use generic-lens or generic-optics with 'snapshotTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etSourceArn :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED etSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | The progress status of the export task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatus :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etStatus = Lens.field @"status"
{-# DEPRECATED etStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time that the snapshot export task completed.
--
-- /Note:/ Consider using 'taskEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskEndTime :: Lens.Lens' ExportTask (Core.Maybe Core.UTCTime)
etTaskEndTime = Lens.field @"taskEndTime"
{-# DEPRECATED etTaskEndTime "Use generic-lens or generic-optics with 'taskEndTime' instead." #-}

-- | The time that the snapshot export task started.
--
-- /Note:/ Consider using 'taskStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskStartTime :: Lens.Lens' ExportTask (Core.Maybe Core.UTCTime)
etTaskStartTime = Lens.field @"taskStartTime"
{-# DEPRECATED etTaskStartTime "Use generic-lens or generic-optics with 'taskStartTime' instead." #-}

-- | The total amount of data exported, in gigabytes.
--
-- /Note:/ Consider using 'totalExtractedDataInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTotalExtractedDataInGB :: Lens.Lens' ExportTask (Core.Maybe Core.Int)
etTotalExtractedDataInGB = Lens.field @"totalExtractedDataInGB"
{-# DEPRECATED etTotalExtractedDataInGB "Use generic-lens or generic-optics with 'totalExtractedDataInGB' instead." #-}

-- | A warning about the snapshot export task.
--
-- /Note:/ Consider using 'warningMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etWarningMessage :: Lens.Lens' ExportTask (Core.Maybe Types.String)
etWarningMessage = Lens.field @"warningMessage"
{-# DEPRECATED etWarningMessage "Use generic-lens or generic-optics with 'warningMessage' instead." #-}

instance Core.FromXML ExportTask where
  parseXML x =
    ExportTask'
      Core.<$> (x Core..@? "ExportOnly" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "ExportTaskIdentifier")
      Core.<*> (x Core..@? "FailureCause")
      Core.<*> (x Core..@? "IamRoleArn")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "PercentProgress")
      Core.<*> (x Core..@? "S3Bucket")
      Core.<*> (x Core..@? "S3Prefix")
      Core.<*> (x Core..@? "SnapshotTime")
      Core.<*> (x Core..@? "SourceArn")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "TaskEndTime")
      Core.<*> (x Core..@? "TaskStartTime")
      Core.<*> (x Core..@? "TotalExtractedDataInGB")
      Core.<*> (x Core..@? "WarningMessage")
