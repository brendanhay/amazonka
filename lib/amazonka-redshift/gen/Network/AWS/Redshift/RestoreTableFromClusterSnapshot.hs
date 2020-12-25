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
    rtfcsClusterIdentifier,
    rtfcsSnapshotIdentifier,
    rtfcsSourceDatabaseName,
    rtfcsSourceTableName,
    rtfcsNewTableName,
    rtfcsSourceSchemaName,
    rtfcsTargetDatabaseName,
    rtfcsTargetSchemaName,

    -- * Destructuring the response
    RestoreTableFromClusterSnapshotResponse (..),
    mkRestoreTableFromClusterSnapshotResponse,

    -- ** Response lenses
    rtfcsrrsTableRestoreStatus,
    rtfcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRestoreTableFromClusterSnapshot' smart constructor.
data RestoreTableFromClusterSnapshot = RestoreTableFromClusterSnapshot'
  { -- | The identifier of the Amazon Redshift cluster to restore the table to.
    clusterIdentifier :: Types.ClusterIdentifier,
    -- | The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
    snapshotIdentifier :: Types.SnapshotIdentifier,
    -- | The name of the source database that contains the table to restore from.
    sourceDatabaseName :: Types.SourceDatabaseName,
    -- | The name of the source table to restore from.
    sourceTableName :: Types.SourceTableName,
    -- | The name of the table to create as a result of the current request.
    newTableName :: Types.NewTableName,
    -- | The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
    sourceSchemaName :: Core.Maybe Types.SourceSchemaName,
    -- | The name of the database to restore the table to.
    targetDatabaseName :: Core.Maybe Types.TargetDatabaseName,
    -- | The name of the schema to restore the table to.
    targetSchemaName :: Core.Maybe Types.TargetSchemaName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreTableFromClusterSnapshot' value with any optional fields omitted.
mkRestoreTableFromClusterSnapshot ::
  -- | 'clusterIdentifier'
  Types.ClusterIdentifier ->
  -- | 'snapshotIdentifier'
  Types.SnapshotIdentifier ->
  -- | 'sourceDatabaseName'
  Types.SourceDatabaseName ->
  -- | 'sourceTableName'
  Types.SourceTableName ->
  -- | 'newTableName'
  Types.NewTableName ->
  RestoreTableFromClusterSnapshot
mkRestoreTableFromClusterSnapshot
  clusterIdentifier
  snapshotIdentifier
  sourceDatabaseName
  sourceTableName
  newTableName =
    RestoreTableFromClusterSnapshot'
      { clusterIdentifier,
        snapshotIdentifier,
        sourceDatabaseName,
        sourceTableName,
        newTableName,
        sourceSchemaName = Core.Nothing,
        targetDatabaseName = Core.Nothing,
        targetSchemaName = Core.Nothing
      }

-- | The identifier of the Amazon Redshift cluster to restore the table to.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsClusterIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Types.ClusterIdentifier
rtfcsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED rtfcsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSnapshotIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Types.SnapshotIdentifier
rtfcsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED rtfcsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The name of the source database that contains the table to restore from.
--
-- /Note:/ Consider using 'sourceDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot Types.SourceDatabaseName
rtfcsSourceDatabaseName = Lens.field @"sourceDatabaseName"
{-# DEPRECATED rtfcsSourceDatabaseName "Use generic-lens or generic-optics with 'sourceDatabaseName' instead." #-}

-- | The name of the source table to restore from.
--
-- /Note:/ Consider using 'sourceTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Types.SourceTableName
rtfcsSourceTableName = Lens.field @"sourceTableName"
{-# DEPRECATED rtfcsSourceTableName "Use generic-lens or generic-optics with 'sourceTableName' instead." #-}

-- | The name of the table to create as a result of the current request.
--
-- /Note:/ Consider using 'newTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsNewTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Types.NewTableName
rtfcsNewTableName = Lens.field @"newTableName"
{-# DEPRECATED rtfcsNewTableName "Use generic-lens or generic-optics with 'newTableName' instead." #-}

-- | The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
--
-- /Note:/ Consider using 'sourceSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Types.SourceSchemaName)
rtfcsSourceSchemaName = Lens.field @"sourceSchemaName"
{-# DEPRECATED rtfcsSourceSchemaName "Use generic-lens or generic-optics with 'sourceSchemaName' instead." #-}

-- | The name of the database to restore the table to.
--
-- /Note:/ Consider using 'targetDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsTargetDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Types.TargetDatabaseName)
rtfcsTargetDatabaseName = Lens.field @"targetDatabaseName"
{-# DEPRECATED rtfcsTargetDatabaseName "Use generic-lens or generic-optics with 'targetDatabaseName' instead." #-}

-- | The name of the schema to restore the table to.
--
-- /Note:/ Consider using 'targetSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsTargetSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Types.TargetSchemaName)
rtfcsTargetSchemaName = Lens.field @"targetSchemaName"
{-# DEPRECATED rtfcsTargetSchemaName "Use generic-lens or generic-optics with 'targetSchemaName' instead." #-}

instance Core.AWSRequest RestoreTableFromClusterSnapshot where
  type
    Rs RestoreTableFromClusterSnapshot =
      RestoreTableFromClusterSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RestoreTableFromClusterSnapshot")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> (Core.toQueryValue "SnapshotIdentifier" snapshotIdentifier)
                Core.<> (Core.toQueryValue "SourceDatabaseName" sourceDatabaseName)
                Core.<> (Core.toQueryValue "SourceTableName" sourceTableName)
                Core.<> (Core.toQueryValue "NewTableName" newTableName)
                Core.<> (Core.toQueryValue "SourceSchemaName" Core.<$> sourceSchemaName)
                Core.<> ( Core.toQueryValue "TargetDatabaseName"
                            Core.<$> targetDatabaseName
                        )
                Core.<> (Core.toQueryValue "TargetSchemaName" Core.<$> targetSchemaName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "RestoreTableFromClusterSnapshotResult"
      ( \s h x ->
          RestoreTableFromClusterSnapshotResponse'
            Core.<$> (x Core..@? "TableRestoreStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreTableFromClusterSnapshotResponse' smart constructor.
data RestoreTableFromClusterSnapshotResponse = RestoreTableFromClusterSnapshotResponse'
  { tableRestoreStatus :: Core.Maybe Types.TableRestoreStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreTableFromClusterSnapshotResponse' value with any optional fields omitted.
mkRestoreTableFromClusterSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreTableFromClusterSnapshotResponse
mkRestoreTableFromClusterSnapshotResponse responseStatus =
  RestoreTableFromClusterSnapshotResponse'
    { tableRestoreStatus =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'tableRestoreStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsrrsTableRestoreStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse (Core.Maybe Types.TableRestoreStatus)
rtfcsrrsTableRestoreStatus = Lens.field @"tableRestoreStatus"
{-# DEPRECATED rtfcsrrsTableRestoreStatus "Use generic-lens or generic-optics with 'tableRestoreStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsrrsResponseStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse Core.Int
rtfcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtfcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
