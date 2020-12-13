{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RestoreTableFromClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table from a table in an Amazon Redshift cluster snapshot. You must create the new table within the Amazon Redshift cluster that the snapshot was taken from.
--
-- You cannot use @RestoreTableFromClusterSnapshot@ to restore a table with the same name as an existing table in an Amazon Redshift cluster. That is, you cannot overwrite an existing table in a cluster with a restored table. If you want to replace your original table with a new, restored table, then rename or drop your original table before you call @RestoreTableFromClusterSnapshot@ . When you have renamed your original table, then you can pass the original name of the table as the @NewTableName@ parameter value in the call to @RestoreTableFromClusterSnapshot@ . This way, you can replace the original table with the table created from the snapshot.
module Network.AWS.Redshift.RestoreTableFromClusterSnapshot
  ( -- * Creating a request
    RestoreTableFromClusterSnapshot (..),
    mkRestoreTableFromClusterSnapshot,

    -- ** Request lenses
    rtfcsTargetSchemaName,
    rtfcsSnapshotIdentifier,
    rtfcsSourceDatabaseName,
    rtfcsNewTableName,
    rtfcsTargetDatabaseName,
    rtfcsSourceSchemaName,
    rtfcsClusterIdentifier,
    rtfcsSourceTableName,

    -- * Destructuring the response
    RestoreTableFromClusterSnapshotResponse (..),
    mkRestoreTableFromClusterSnapshotResponse,

    -- ** Response lenses
    rtfcsrsTableRestoreStatus,
    rtfcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRestoreTableFromClusterSnapshot' smart constructor.
data RestoreTableFromClusterSnapshot = RestoreTableFromClusterSnapshot'
  { -- | The name of the schema to restore the table to.
    targetSchemaName :: Lude.Maybe Lude.Text,
    -- | The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
    snapshotIdentifier :: Lude.Text,
    -- | The name of the source database that contains the table to restore from.
    sourceDatabaseName :: Lude.Text,
    -- | The name of the table to create as a result of the current request.
    newTableName :: Lude.Text,
    -- | The name of the database to restore the table to.
    targetDatabaseName :: Lude.Maybe Lude.Text,
    -- | The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
    sourceSchemaName :: Lude.Maybe Lude.Text,
    -- | The identifier of the Amazon Redshift cluster to restore the table to.
    clusterIdentifier :: Lude.Text,
    -- | The name of the source table to restore from.
    sourceTableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreTableFromClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'targetSchemaName' - The name of the schema to restore the table to.
-- * 'snapshotIdentifier' - The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
-- * 'sourceDatabaseName' - The name of the source database that contains the table to restore from.
-- * 'newTableName' - The name of the table to create as a result of the current request.
-- * 'targetDatabaseName' - The name of the database to restore the table to.
-- * 'sourceSchemaName' - The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
-- * 'clusterIdentifier' - The identifier of the Amazon Redshift cluster to restore the table to.
-- * 'sourceTableName' - The name of the source table to restore from.
mkRestoreTableFromClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Lude.Text ->
  -- | 'sourceDatabaseName'
  Lude.Text ->
  -- | 'newTableName'
  Lude.Text ->
  -- | 'clusterIdentifier'
  Lude.Text ->
  -- | 'sourceTableName'
  Lude.Text ->
  RestoreTableFromClusterSnapshot
mkRestoreTableFromClusterSnapshot
  pSnapshotIdentifier_
  pSourceDatabaseName_
  pNewTableName_
  pClusterIdentifier_
  pSourceTableName_ =
    RestoreTableFromClusterSnapshot'
      { targetSchemaName = Lude.Nothing,
        snapshotIdentifier = pSnapshotIdentifier_,
        sourceDatabaseName = pSourceDatabaseName_,
        newTableName = pNewTableName_,
        targetDatabaseName = Lude.Nothing,
        sourceSchemaName = Lude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        sourceTableName = pSourceTableName_
      }

-- | The name of the schema to restore the table to.
--
-- /Note:/ Consider using 'targetSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsTargetSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Lude.Maybe Lude.Text)
rtfcsTargetSchemaName = Lens.lens (targetSchemaName :: RestoreTableFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {targetSchemaName = a} :: RestoreTableFromClusterSnapshot)
{-# DEPRECATED rtfcsTargetSchemaName "Use generic-lens or generic-optics with 'targetSchemaName' instead." #-}

-- | The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSnapshotIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Lude.Text
rtfcsSnapshotIdentifier = Lens.lens (snapshotIdentifier :: RestoreTableFromClusterSnapshot -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: RestoreTableFromClusterSnapshot)
{-# DEPRECATED rtfcsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The name of the source database that contains the table to restore from.
--
-- /Note:/ Consider using 'sourceDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot Lude.Text
rtfcsSourceDatabaseName = Lens.lens (sourceDatabaseName :: RestoreTableFromClusterSnapshot -> Lude.Text) (\s a -> s {sourceDatabaseName = a} :: RestoreTableFromClusterSnapshot)
{-# DEPRECATED rtfcsSourceDatabaseName "Use generic-lens or generic-optics with 'sourceDatabaseName' instead." #-}

-- | The name of the table to create as a result of the current request.
--
-- /Note:/ Consider using 'newTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsNewTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Lude.Text
rtfcsNewTableName = Lens.lens (newTableName :: RestoreTableFromClusterSnapshot -> Lude.Text) (\s a -> s {newTableName = a} :: RestoreTableFromClusterSnapshot)
{-# DEPRECATED rtfcsNewTableName "Use generic-lens or generic-optics with 'newTableName' instead." #-}

-- | The name of the database to restore the table to.
--
-- /Note:/ Consider using 'targetDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsTargetDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot (Lude.Maybe Lude.Text)
rtfcsTargetDatabaseName = Lens.lens (targetDatabaseName :: RestoreTableFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {targetDatabaseName = a} :: RestoreTableFromClusterSnapshot)
{-# DEPRECATED rtfcsTargetDatabaseName "Use generic-lens or generic-optics with 'targetDatabaseName' instead." #-}

-- | The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
--
-- /Note:/ Consider using 'sourceSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Lude.Maybe Lude.Text)
rtfcsSourceSchemaName = Lens.lens (sourceSchemaName :: RestoreTableFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceSchemaName = a} :: RestoreTableFromClusterSnapshot)
{-# DEPRECATED rtfcsSourceSchemaName "Use generic-lens or generic-optics with 'sourceSchemaName' instead." #-}

-- | The identifier of the Amazon Redshift cluster to restore the table to.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsClusterIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Lude.Text
rtfcsClusterIdentifier = Lens.lens (clusterIdentifier :: RestoreTableFromClusterSnapshot -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: RestoreTableFromClusterSnapshot)
{-# DEPRECATED rtfcsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The name of the source table to restore from.
--
-- /Note:/ Consider using 'sourceTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Lude.Text
rtfcsSourceTableName = Lens.lens (sourceTableName :: RestoreTableFromClusterSnapshot -> Lude.Text) (\s a -> s {sourceTableName = a} :: RestoreTableFromClusterSnapshot)
{-# DEPRECATED rtfcsSourceTableName "Use generic-lens or generic-optics with 'sourceTableName' instead." #-}

instance Lude.AWSRequest RestoreTableFromClusterSnapshot where
  type
    Rs RestoreTableFromClusterSnapshot =
      RestoreTableFromClusterSnapshotResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "RestoreTableFromClusterSnapshotResult"
      ( \s h x ->
          RestoreTableFromClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "TableRestoreStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreTableFromClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreTableFromClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreTableFromClusterSnapshot where
  toQuery RestoreTableFromClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RestoreTableFromClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TargetSchemaName" Lude.=: targetSchemaName,
        "SnapshotIdentifier" Lude.=: snapshotIdentifier,
        "SourceDatabaseName" Lude.=: sourceDatabaseName,
        "NewTableName" Lude.=: newTableName,
        "TargetDatabaseName" Lude.=: targetDatabaseName,
        "SourceSchemaName" Lude.=: sourceSchemaName,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "SourceTableName" Lude.=: sourceTableName
      ]

-- | /See:/ 'mkRestoreTableFromClusterSnapshotResponse' smart constructor.
data RestoreTableFromClusterSnapshotResponse = RestoreTableFromClusterSnapshotResponse'
  { tableRestoreStatus :: Lude.Maybe TableRestoreStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreTableFromClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'tableRestoreStatus' -
-- * 'responseStatus' - The response status code.
mkRestoreTableFromClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreTableFromClusterSnapshotResponse
mkRestoreTableFromClusterSnapshotResponse pResponseStatus_ =
  RestoreTableFromClusterSnapshotResponse'
    { tableRestoreStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'tableRestoreStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsrsTableRestoreStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse (Lude.Maybe TableRestoreStatus)
rtfcsrsTableRestoreStatus = Lens.lens (tableRestoreStatus :: RestoreTableFromClusterSnapshotResponse -> Lude.Maybe TableRestoreStatus) (\s a -> s {tableRestoreStatus = a} :: RestoreTableFromClusterSnapshotResponse)
{-# DEPRECATED rtfcsrsTableRestoreStatus "Use generic-lens or generic-optics with 'tableRestoreStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsrsResponseStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse Lude.Int
rtfcsrsResponseStatus = Lens.lens (responseStatus :: RestoreTableFromClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreTableFromClusterSnapshotResponse)
{-# DEPRECATED rtfcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
