{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ExportTask
  ( ExportTask (..)
  -- * Smart constructor
  , mkExportTask
  -- * Lenses
  , etExportOnly
  , etExportTaskIdentifier
  , etFailureCause
  , etIamRoleArn
  , etKmsKeyId
  , etPercentProgress
  , etS3Bucket
  , etS3Prefix
  , etSnapshotTime
  , etSourceArn
  , etStatus
  , etTaskEndTime
  , etTaskStartTime
  , etTotalExtractedDataInGB
  , etWarningMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the details of a snapshot export to Amazon S3. 
--
-- This data type is used as a response element in the @DescribeExportTasks@ action. 
--
-- /See:/ 'mkExportTask' smart constructor.
data ExportTask = ExportTask'
  { exportOnly :: Core.Maybe [Core.Text]
    -- ^ The data exported from the snapshot. Valid values are the following:
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
  , exportTaskIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is exported to. 
  , failureCause :: Core.Maybe Core.Text
    -- ^ The reason the export failed, if it failed.
  , iamRoleArn :: Core.Maybe Core.Text
    -- ^ The name of the IAM role that is used to write to Amazon S3 when exporting a snapshot. 
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS KMS key that is used to encrypt the snapshot when it's exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The IAM role used for the snapshot export must have encryption and decryption permissions to use this KMS key. 
  , percentProgress :: Core.Maybe Core.Int
    -- ^ The progress of the snapshot export task as a percentage.
  , s3Bucket :: Core.Maybe Core.Text
    -- ^ The Amazon S3 bucket that the snapshot is exported to.
  , s3Prefix :: Core.Maybe Core.Text
    -- ^ The Amazon S3 bucket prefix that is the file name and path of the exported snapshot.
  , snapshotTime :: Core.Maybe Core.UTCTime
    -- ^ The time that the snapshot was created.
  , sourceArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
  , status :: Core.Maybe Core.Text
    -- ^ The progress status of the export task.
  , taskEndTime :: Core.Maybe Core.UTCTime
    -- ^ The time that the snapshot export task completed.
  , taskStartTime :: Core.Maybe Core.UTCTime
    -- ^ The time that the snapshot export task started.
  , totalExtractedDataInGB :: Core.Maybe Core.Int
    -- ^ The total amount of data exported, in gigabytes.
  , warningMessage :: Core.Maybe Core.Text
    -- ^ A warning about the snapshot export task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ExportTask' value with any optional fields omitted.
mkExportTask
    :: ExportTask
mkExportTask
  = ExportTask'{exportOnly = Core.Nothing,
                exportTaskIdentifier = Core.Nothing, failureCause = Core.Nothing,
                iamRoleArn = Core.Nothing, kmsKeyId = Core.Nothing,
                percentProgress = Core.Nothing, s3Bucket = Core.Nothing,
                s3Prefix = Core.Nothing, snapshotTime = Core.Nothing,
                sourceArn = Core.Nothing, status = Core.Nothing,
                taskEndTime = Core.Nothing, taskStartTime = Core.Nothing,
                totalExtractedDataInGB = Core.Nothing,
                warningMessage = Core.Nothing}

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
etExportOnly :: Lens.Lens' ExportTask (Core.Maybe [Core.Text])
etExportOnly = Lens.field @"exportOnly"
{-# INLINEABLE etExportOnly #-}
{-# DEPRECATED exportOnly "Use generic-lens or generic-optics with 'exportOnly' instead"  #-}

-- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is exported to. 
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportTaskIdentifier :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etExportTaskIdentifier = Lens.field @"exportTaskIdentifier"
{-# INLINEABLE etExportTaskIdentifier #-}
{-# DEPRECATED exportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead"  #-}

-- | The reason the export failed, if it failed.
--
-- /Note:/ Consider using 'failureCause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etFailureCause :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etFailureCause = Lens.field @"failureCause"
{-# INLINEABLE etFailureCause #-}
{-# DEPRECATED failureCause "Use generic-lens or generic-optics with 'failureCause' instead"  #-}

-- | The name of the IAM role that is used to write to Amazon S3 when exporting a snapshot. 
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etIamRoleArn :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE etIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

-- | The ID of the AWS KMS key that is used to encrypt the snapshot when it's exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The IAM role used for the snapshot export must have encryption and decryption permissions to use this KMS key. 
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etKmsKeyId :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE etKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The progress of the snapshot export task as a percentage.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etPercentProgress :: Lens.Lens' ExportTask (Core.Maybe Core.Int)
etPercentProgress = Lens.field @"percentProgress"
{-# INLINEABLE etPercentProgress #-}
{-# DEPRECATED percentProgress "Use generic-lens or generic-optics with 'percentProgress' instead"  #-}

-- | The Amazon S3 bucket that the snapshot is exported to.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etS3Bucket :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE etS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

-- | The Amazon S3 bucket prefix that is the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etS3Prefix :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etS3Prefix = Lens.field @"s3Prefix"
{-# INLINEABLE etS3Prefix #-}
{-# DEPRECATED s3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead"  #-}

-- | The time that the snapshot was created.
--
-- /Note:/ Consider using 'snapshotTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etSnapshotTime :: Lens.Lens' ExportTask (Core.Maybe Core.UTCTime)
etSnapshotTime = Lens.field @"snapshotTime"
{-# INLINEABLE etSnapshotTime #-}
{-# DEPRECATED snapshotTime "Use generic-lens or generic-optics with 'snapshotTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etSourceArn :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etSourceArn = Lens.field @"sourceArn"
{-# INLINEABLE etSourceArn #-}
{-# DEPRECATED sourceArn "Use generic-lens or generic-optics with 'sourceArn' instead"  #-}

-- | The progress status of the export task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatus :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etStatus = Lens.field @"status"
{-# INLINEABLE etStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The time that the snapshot export task completed.
--
-- /Note:/ Consider using 'taskEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskEndTime :: Lens.Lens' ExportTask (Core.Maybe Core.UTCTime)
etTaskEndTime = Lens.field @"taskEndTime"
{-# INLINEABLE etTaskEndTime #-}
{-# DEPRECATED taskEndTime "Use generic-lens or generic-optics with 'taskEndTime' instead"  #-}

-- | The time that the snapshot export task started.
--
-- /Note:/ Consider using 'taskStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskStartTime :: Lens.Lens' ExportTask (Core.Maybe Core.UTCTime)
etTaskStartTime = Lens.field @"taskStartTime"
{-# INLINEABLE etTaskStartTime #-}
{-# DEPRECATED taskStartTime "Use generic-lens or generic-optics with 'taskStartTime' instead"  #-}

-- | The total amount of data exported, in gigabytes.
--
-- /Note:/ Consider using 'totalExtractedDataInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTotalExtractedDataInGB :: Lens.Lens' ExportTask (Core.Maybe Core.Int)
etTotalExtractedDataInGB = Lens.field @"totalExtractedDataInGB"
{-# INLINEABLE etTotalExtractedDataInGB #-}
{-# DEPRECATED totalExtractedDataInGB "Use generic-lens or generic-optics with 'totalExtractedDataInGB' instead"  #-}

-- | A warning about the snapshot export task.
--
-- /Note:/ Consider using 'warningMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etWarningMessage :: Lens.Lens' ExportTask (Core.Maybe Core.Text)
etWarningMessage = Lens.field @"warningMessage"
{-# INLINEABLE etWarningMessage #-}
{-# DEPRECATED warningMessage "Use generic-lens or generic-optics with 'warningMessage' instead"  #-}

instance Core.FromXML ExportTask where
        parseXML x
          = ExportTask' Core.<$>
              (x Core..@? "ExportOnly" Core..<@> Core.parseXMLList "member")
                Core.<*> x Core..@? "ExportTaskIdentifier"
                Core.<*> x Core..@? "FailureCause"
                Core.<*> x Core..@? "IamRoleArn"
                Core.<*> x Core..@? "KmsKeyId"
                Core.<*> x Core..@? "PercentProgress"
                Core.<*> x Core..@? "S3Bucket"
                Core.<*> x Core..@? "S3Prefix"
                Core.<*> x Core..@? "SnapshotTime"
                Core.<*> x Core..@? "SourceArn"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "TaskEndTime"
                Core.<*> x Core..@? "TaskStartTime"
                Core.<*> x Core..@? "TotalExtractedDataInGB"
                Core.<*> x Core..@? "WarningMessage"
