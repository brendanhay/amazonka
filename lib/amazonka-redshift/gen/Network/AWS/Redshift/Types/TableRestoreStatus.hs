{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.TableRestoreStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.TableRestoreStatus
  ( TableRestoreStatus (..)
  -- * Smart constructor
  , mkTableRestoreStatus
  -- * Lenses
  , trsClusterIdentifier
  , trsMessage
  , trsNewTableName
  , trsProgressInMegaBytes
  , trsRequestTime
  , trsSnapshotIdentifier
  , trsSourceDatabaseName
  , trsSourceSchemaName
  , trsSourceTableName
  , trsStatus
  , trsTableRestoreRequestId
  , trsTargetDatabaseName
  , trsTargetSchemaName
  , trsTotalDataInMegaBytes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.TableRestoreStatusType as Types

-- | Describes the status of a 'RestoreTableFromClusterSnapshot' operation.
--
-- /See:/ 'mkTableRestoreStatus' smart constructor.
data TableRestoreStatus = TableRestoreStatus'
  { clusterIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the Amazon Redshift cluster that the table is being restored to.
  , message :: Core.Maybe Core.Text
    -- ^ A description of the status of the table restore request. Status values include @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ .
  , newTableName :: Core.Maybe Core.Text
    -- ^ The name of the table to create as a result of the table restore request.
  , progressInMegaBytes :: Core.Maybe Core.Integer
    -- ^ The amount of data restored to the new table so far, in megabytes (MB).
  , requestTime :: Core.Maybe Core.UTCTime
    -- ^ The time that the table restore request was made, in Universal Coordinated Time (UTC).
  , snapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the snapshot that the table is being restored from.
  , sourceDatabaseName :: Core.Maybe Core.Text
    -- ^ The name of the source database that contains the table being restored.
  , sourceSchemaName :: Core.Maybe Core.Text
    -- ^ The name of the source schema that contains the table being restored.
  , sourceTableName :: Core.Maybe Core.Text
    -- ^ The name of the source table being restored.
  , status :: Core.Maybe Types.TableRestoreStatusType
    -- ^ A value that describes the current state of the table restore request.
--
-- Valid Values: @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ 
  , tableRestoreRequestId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the table restore request.
  , targetDatabaseName :: Core.Maybe Core.Text
    -- ^ The name of the database to restore the table to.
  , targetSchemaName :: Core.Maybe Core.Text
    -- ^ The name of the schema to restore the table to.
  , totalDataInMegaBytes :: Core.Maybe Core.Integer
    -- ^ The total amount of data to restore to the new table, in megabytes (MB).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TableRestoreStatus' value with any optional fields omitted.
mkTableRestoreStatus
    :: TableRestoreStatus
mkTableRestoreStatus
  = TableRestoreStatus'{clusterIdentifier = Core.Nothing,
                        message = Core.Nothing, newTableName = Core.Nothing,
                        progressInMegaBytes = Core.Nothing, requestTime = Core.Nothing,
                        snapshotIdentifier = Core.Nothing,
                        sourceDatabaseName = Core.Nothing, sourceSchemaName = Core.Nothing,
                        sourceTableName = Core.Nothing, status = Core.Nothing,
                        tableRestoreRequestId = Core.Nothing,
                        targetDatabaseName = Core.Nothing, targetSchemaName = Core.Nothing,
                        totalDataInMegaBytes = Core.Nothing}

-- | The identifier of the Amazon Redshift cluster that the table is being restored to.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsClusterIdentifier :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE trsClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | A description of the status of the table restore request. Status values include @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsMessage :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsMessage = Lens.field @"message"
{-# INLINEABLE trsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The name of the table to create as a result of the table restore request.
--
-- /Note:/ Consider using 'newTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsNewTableName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsNewTableName = Lens.field @"newTableName"
{-# INLINEABLE trsNewTableName #-}
{-# DEPRECATED newTableName "Use generic-lens or generic-optics with 'newTableName' instead"  #-}

-- | The amount of data restored to the new table so far, in megabytes (MB).
--
-- /Note:/ Consider using 'progressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsProgressInMegaBytes :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Integer)
trsProgressInMegaBytes = Lens.field @"progressInMegaBytes"
{-# INLINEABLE trsProgressInMegaBytes #-}
{-# DEPRECATED progressInMegaBytes "Use generic-lens or generic-optics with 'progressInMegaBytes' instead"  #-}

-- | The time that the table restore request was made, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'requestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsRequestTime :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.UTCTime)
trsRequestTime = Lens.field @"requestTime"
{-# INLINEABLE trsRequestTime #-}
{-# DEPRECATED requestTime "Use generic-lens or generic-optics with 'requestTime' instead"  #-}

-- | The identifier of the snapshot that the table is being restored from.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsSnapshotIdentifier :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE trsSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | The name of the source database that contains the table being restored.
--
-- /Note:/ Consider using 'sourceDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsSourceDatabaseName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsSourceDatabaseName = Lens.field @"sourceDatabaseName"
{-# INLINEABLE trsSourceDatabaseName #-}
{-# DEPRECATED sourceDatabaseName "Use generic-lens or generic-optics with 'sourceDatabaseName' instead"  #-}

-- | The name of the source schema that contains the table being restored.
--
-- /Note:/ Consider using 'sourceSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsSourceSchemaName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsSourceSchemaName = Lens.field @"sourceSchemaName"
{-# INLINEABLE trsSourceSchemaName #-}
{-# DEPRECATED sourceSchemaName "Use generic-lens or generic-optics with 'sourceSchemaName' instead"  #-}

-- | The name of the source table being restored.
--
-- /Note:/ Consider using 'sourceTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsSourceTableName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsSourceTableName = Lens.field @"sourceTableName"
{-# INLINEABLE trsSourceTableName #-}
{-# DEPRECATED sourceTableName "Use generic-lens or generic-optics with 'sourceTableName' instead"  #-}

-- | A value that describes the current state of the table restore request.
--
-- Valid Values: @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsStatus :: Lens.Lens' TableRestoreStatus (Core.Maybe Types.TableRestoreStatusType)
trsStatus = Lens.field @"status"
{-# INLINEABLE trsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The unique identifier for the table restore request.
--
-- /Note:/ Consider using 'tableRestoreRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTableRestoreRequestId :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsTableRestoreRequestId = Lens.field @"tableRestoreRequestId"
{-# INLINEABLE trsTableRestoreRequestId #-}
{-# DEPRECATED tableRestoreRequestId "Use generic-lens or generic-optics with 'tableRestoreRequestId' instead"  #-}

-- | The name of the database to restore the table to.
--
-- /Note:/ Consider using 'targetDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTargetDatabaseName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsTargetDatabaseName = Lens.field @"targetDatabaseName"
{-# INLINEABLE trsTargetDatabaseName #-}
{-# DEPRECATED targetDatabaseName "Use generic-lens or generic-optics with 'targetDatabaseName' instead"  #-}

-- | The name of the schema to restore the table to.
--
-- /Note:/ Consider using 'targetSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTargetSchemaName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
trsTargetSchemaName = Lens.field @"targetSchemaName"
{-# INLINEABLE trsTargetSchemaName #-}
{-# DEPRECATED targetSchemaName "Use generic-lens or generic-optics with 'targetSchemaName' instead"  #-}

-- | The total amount of data to restore to the new table, in megabytes (MB).
--
-- /Note:/ Consider using 'totalDataInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTotalDataInMegaBytes :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Integer)
trsTotalDataInMegaBytes = Lens.field @"totalDataInMegaBytes"
{-# INLINEABLE trsTotalDataInMegaBytes #-}
{-# DEPRECATED totalDataInMegaBytes "Use generic-lens or generic-optics with 'totalDataInMegaBytes' instead"  #-}

instance Core.FromXML TableRestoreStatus where
        parseXML x
          = TableRestoreStatus' Core.<$>
              (x Core..@? "ClusterIdentifier") Core.<*> x Core..@? "Message"
                Core.<*> x Core..@? "NewTableName"
                Core.<*> x Core..@? "ProgressInMegaBytes"
                Core.<*> x Core..@? "RequestTime"
                Core.<*> x Core..@? "SnapshotIdentifier"
                Core.<*> x Core..@? "SourceDatabaseName"
                Core.<*> x Core..@? "SourceSchemaName"
                Core.<*> x Core..@? "SourceTableName"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "TableRestoreRequestId"
                Core.<*> x Core..@? "TargetDatabaseName"
                Core.<*> x Core..@? "TargetSchemaName"
                Core.<*> x Core..@? "TotalDataInMegaBytes"
