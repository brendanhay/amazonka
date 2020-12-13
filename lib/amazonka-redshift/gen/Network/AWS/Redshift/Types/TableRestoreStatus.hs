{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.TableRestoreStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.TableRestoreStatus
  ( TableRestoreStatus (..),

    -- * Smart constructor
    mkTableRestoreStatus,

    -- * Lenses
    trsStatus,
    trsTargetSchemaName,
    trsSnapshotIdentifier,
    trsSourceDatabaseName,
    trsTableRestoreRequestId,
    trsNewTableName,
    trsTargetDatabaseName,
    trsSourceSchemaName,
    trsClusterIdentifier,
    trsRequestTime,
    trsSourceTableName,
    trsTotalDataInMegaBytes,
    trsProgressInMegaBytes,
    trsMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.TableRestoreStatusType

-- | Describes the status of a 'RestoreTableFromClusterSnapshot' operation.
--
-- /See:/ 'mkTableRestoreStatus' smart constructor.
data TableRestoreStatus = TableRestoreStatus'
  { -- | A value that describes the current state of the table restore request.
    --
    -- Valid Values: @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@
    status :: Lude.Maybe TableRestoreStatusType,
    -- | The name of the schema to restore the table to.
    targetSchemaName :: Lude.Maybe Lude.Text,
    -- | The identifier of the snapshot that the table is being restored from.
    snapshotIdentifier :: Lude.Maybe Lude.Text,
    -- | The name of the source database that contains the table being restored.
    sourceDatabaseName :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the table restore request.
    tableRestoreRequestId :: Lude.Maybe Lude.Text,
    -- | The name of the table to create as a result of the table restore request.
    newTableName :: Lude.Maybe Lude.Text,
    -- | The name of the database to restore the table to.
    targetDatabaseName :: Lude.Maybe Lude.Text,
    -- | The name of the source schema that contains the table being restored.
    sourceSchemaName :: Lude.Maybe Lude.Text,
    -- | The identifier of the Amazon Redshift cluster that the table is being restored to.
    clusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The time that the table restore request was made, in Universal Coordinated Time (UTC).
    requestTime :: Lude.Maybe Lude.DateTime,
    -- | The name of the source table being restored.
    sourceTableName :: Lude.Maybe Lude.Text,
    -- | The total amount of data to restore to the new table, in megabytes (MB).
    totalDataInMegaBytes :: Lude.Maybe Lude.Integer,
    -- | The amount of data restored to the new table so far, in megabytes (MB).
    progressInMegaBytes :: Lude.Maybe Lude.Integer,
    -- | A description of the status of the table restore request. Status values include @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ .
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableRestoreStatus' with the minimum fields required to make a request.
--
-- * 'status' - A value that describes the current state of the table restore request.
--
-- Valid Values: @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@
-- * 'targetSchemaName' - The name of the schema to restore the table to.
-- * 'snapshotIdentifier' - The identifier of the snapshot that the table is being restored from.
-- * 'sourceDatabaseName' - The name of the source database that contains the table being restored.
-- * 'tableRestoreRequestId' - The unique identifier for the table restore request.
-- * 'newTableName' - The name of the table to create as a result of the table restore request.
-- * 'targetDatabaseName' - The name of the database to restore the table to.
-- * 'sourceSchemaName' - The name of the source schema that contains the table being restored.
-- * 'clusterIdentifier' - The identifier of the Amazon Redshift cluster that the table is being restored to.
-- * 'requestTime' - The time that the table restore request was made, in Universal Coordinated Time (UTC).
-- * 'sourceTableName' - The name of the source table being restored.
-- * 'totalDataInMegaBytes' - The total amount of data to restore to the new table, in megabytes (MB).
-- * 'progressInMegaBytes' - The amount of data restored to the new table so far, in megabytes (MB).
-- * 'message' - A description of the status of the table restore request. Status values include @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ .
mkTableRestoreStatus ::
  TableRestoreStatus
mkTableRestoreStatus =
  TableRestoreStatus'
    { status = Lude.Nothing,
      targetSchemaName = Lude.Nothing,
      snapshotIdentifier = Lude.Nothing,
      sourceDatabaseName = Lude.Nothing,
      tableRestoreRequestId = Lude.Nothing,
      newTableName = Lude.Nothing,
      targetDatabaseName = Lude.Nothing,
      sourceSchemaName = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      requestTime = Lude.Nothing,
      sourceTableName = Lude.Nothing,
      totalDataInMegaBytes = Lude.Nothing,
      progressInMegaBytes = Lude.Nothing,
      message = Lude.Nothing
    }

-- | A value that describes the current state of the table restore request.
--
-- Valid Values: @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsStatus :: Lens.Lens' TableRestoreStatus (Lude.Maybe TableRestoreStatusType)
trsStatus = Lens.lens (status :: TableRestoreStatus -> Lude.Maybe TableRestoreStatusType) (\s a -> s {status = a} :: TableRestoreStatus)
{-# DEPRECATED trsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the schema to restore the table to.
--
-- /Note:/ Consider using 'targetSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTargetSchemaName :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsTargetSchemaName = Lens.lens (targetSchemaName :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {targetSchemaName = a} :: TableRestoreStatus)
{-# DEPRECATED trsTargetSchemaName "Use generic-lens or generic-optics with 'targetSchemaName' instead." #-}

-- | The identifier of the snapshot that the table is being restored from.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsSnapshotIdentifier :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsSnapshotIdentifier = Lens.lens (snapshotIdentifier :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {snapshotIdentifier = a} :: TableRestoreStatus)
{-# DEPRECATED trsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The name of the source database that contains the table being restored.
--
-- /Note:/ Consider using 'sourceDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsSourceDatabaseName :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsSourceDatabaseName = Lens.lens (sourceDatabaseName :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {sourceDatabaseName = a} :: TableRestoreStatus)
{-# DEPRECATED trsSourceDatabaseName "Use generic-lens or generic-optics with 'sourceDatabaseName' instead." #-}

-- | The unique identifier for the table restore request.
--
-- /Note:/ Consider using 'tableRestoreRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTableRestoreRequestId :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsTableRestoreRequestId = Lens.lens (tableRestoreRequestId :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {tableRestoreRequestId = a} :: TableRestoreStatus)
{-# DEPRECATED trsTableRestoreRequestId "Use generic-lens or generic-optics with 'tableRestoreRequestId' instead." #-}

-- | The name of the table to create as a result of the table restore request.
--
-- /Note:/ Consider using 'newTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsNewTableName :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsNewTableName = Lens.lens (newTableName :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {newTableName = a} :: TableRestoreStatus)
{-# DEPRECATED trsNewTableName "Use generic-lens or generic-optics with 'newTableName' instead." #-}

-- | The name of the database to restore the table to.
--
-- /Note:/ Consider using 'targetDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTargetDatabaseName :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsTargetDatabaseName = Lens.lens (targetDatabaseName :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {targetDatabaseName = a} :: TableRestoreStatus)
{-# DEPRECATED trsTargetDatabaseName "Use generic-lens or generic-optics with 'targetDatabaseName' instead." #-}

-- | The name of the source schema that contains the table being restored.
--
-- /Note:/ Consider using 'sourceSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsSourceSchemaName :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsSourceSchemaName = Lens.lens (sourceSchemaName :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {sourceSchemaName = a} :: TableRestoreStatus)
{-# DEPRECATED trsSourceSchemaName "Use generic-lens or generic-optics with 'sourceSchemaName' instead." #-}

-- | The identifier of the Amazon Redshift cluster that the table is being restored to.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsClusterIdentifier :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsClusterIdentifier = Lens.lens (clusterIdentifier :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: TableRestoreStatus)
{-# DEPRECATED trsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The time that the table restore request was made, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'requestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsRequestTime :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.DateTime)
trsRequestTime = Lens.lens (requestTime :: TableRestoreStatus -> Lude.Maybe Lude.DateTime) (\s a -> s {requestTime = a} :: TableRestoreStatus)
{-# DEPRECATED trsRequestTime "Use generic-lens or generic-optics with 'requestTime' instead." #-}

-- | The name of the source table being restored.
--
-- /Note:/ Consider using 'sourceTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsSourceTableName :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsSourceTableName = Lens.lens (sourceTableName :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {sourceTableName = a} :: TableRestoreStatus)
{-# DEPRECATED trsSourceTableName "Use generic-lens or generic-optics with 'sourceTableName' instead." #-}

-- | The total amount of data to restore to the new table, in megabytes (MB).
--
-- /Note:/ Consider using 'totalDataInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTotalDataInMegaBytes :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Integer)
trsTotalDataInMegaBytes = Lens.lens (totalDataInMegaBytes :: TableRestoreStatus -> Lude.Maybe Lude.Integer) (\s a -> s {totalDataInMegaBytes = a} :: TableRestoreStatus)
{-# DEPRECATED trsTotalDataInMegaBytes "Use generic-lens or generic-optics with 'totalDataInMegaBytes' instead." #-}

-- | The amount of data restored to the new table so far, in megabytes (MB).
--
-- /Note:/ Consider using 'progressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsProgressInMegaBytes :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Integer)
trsProgressInMegaBytes = Lens.lens (progressInMegaBytes :: TableRestoreStatus -> Lude.Maybe Lude.Integer) (\s a -> s {progressInMegaBytes = a} :: TableRestoreStatus)
{-# DEPRECATED trsProgressInMegaBytes "Use generic-lens or generic-optics with 'progressInMegaBytes' instead." #-}

-- | A description of the status of the table restore request. Status values include @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsMessage :: Lens.Lens' TableRestoreStatus (Lude.Maybe Lude.Text)
trsMessage = Lens.lens (message :: TableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: TableRestoreStatus)
{-# DEPRECATED trsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML TableRestoreStatus where
  parseXML x =
    TableRestoreStatus'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "TargetSchemaName")
      Lude.<*> (x Lude..@? "SnapshotIdentifier")
      Lude.<*> (x Lude..@? "SourceDatabaseName")
      Lude.<*> (x Lude..@? "TableRestoreRequestId")
      Lude.<*> (x Lude..@? "NewTableName")
      Lude.<*> (x Lude..@? "TargetDatabaseName")
      Lude.<*> (x Lude..@? "SourceSchemaName")
      Lude.<*> (x Lude..@? "ClusterIdentifier")
      Lude.<*> (x Lude..@? "RequestTime")
      Lude.<*> (x Lude..@? "SourceTableName")
      Lude.<*> (x Lude..@? "TotalDataInMegaBytes")
      Lude.<*> (x Lude..@? "ProgressInMegaBytes")
      Lude.<*> (x Lude..@? "Message")
