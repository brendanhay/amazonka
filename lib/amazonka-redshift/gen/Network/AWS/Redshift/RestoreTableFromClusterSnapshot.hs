{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RestoreTableFromClusterSnapshot (..)
    , mkRestoreTableFromClusterSnapshot
    -- ** Request lenses
    , rtfcsClusterIdentifier
    , rtfcsSnapshotIdentifier
    , rtfcsSourceDatabaseName
    , rtfcsSourceTableName
    , rtfcsNewTableName
    , rtfcsSourceSchemaName
    , rtfcsTargetDatabaseName
    , rtfcsTargetSchemaName

    -- * Destructuring the response
    , RestoreTableFromClusterSnapshotResponse (..)
    , mkRestoreTableFromClusterSnapshotResponse
    -- ** Response lenses
    , rtfcsrrsTableRestoreStatus
    , rtfcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRestoreTableFromClusterSnapshot' smart constructor.
data RestoreTableFromClusterSnapshot = RestoreTableFromClusterSnapshot'
  { clusterIdentifier :: Core.Text
    -- ^ The identifier of the Amazon Redshift cluster to restore the table to.
  , snapshotIdentifier :: Core.Text
    -- ^ The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
  , sourceDatabaseName :: Core.Text
    -- ^ The name of the source database that contains the table to restore from.
  , sourceTableName :: Core.Text
    -- ^ The name of the source table to restore from.
  , newTableName :: Core.Text
    -- ^ The name of the table to create as a result of the current request.
  , sourceSchemaName :: Core.Maybe Core.Text
    -- ^ The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
  , targetDatabaseName :: Core.Maybe Core.Text
    -- ^ The name of the database to restore the table to.
  , targetSchemaName :: Core.Maybe Core.Text
    -- ^ The name of the schema to restore the table to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreTableFromClusterSnapshot' value with any optional fields omitted.
mkRestoreTableFromClusterSnapshot
    :: Core.Text -- ^ 'clusterIdentifier'
    -> Core.Text -- ^ 'snapshotIdentifier'
    -> Core.Text -- ^ 'sourceDatabaseName'
    -> Core.Text -- ^ 'sourceTableName'
    -> Core.Text -- ^ 'newTableName'
    -> RestoreTableFromClusterSnapshot
mkRestoreTableFromClusterSnapshot clusterIdentifier
  snapshotIdentifier sourceDatabaseName sourceTableName newTableName
  = RestoreTableFromClusterSnapshot'{clusterIdentifier,
                                     snapshotIdentifier, sourceDatabaseName, sourceTableName,
                                     newTableName, sourceSchemaName = Core.Nothing,
                                     targetDatabaseName = Core.Nothing,
                                     targetSchemaName = Core.Nothing}

-- | The identifier of the Amazon Redshift cluster to restore the table to.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsClusterIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
rtfcsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE rtfcsClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSnapshotIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
rtfcsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE rtfcsSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | The name of the source database that contains the table to restore from.
--
-- /Note:/ Consider using 'sourceDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
rtfcsSourceDatabaseName = Lens.field @"sourceDatabaseName"
{-# INLINEABLE rtfcsSourceDatabaseName #-}
{-# DEPRECATED sourceDatabaseName "Use generic-lens or generic-optics with 'sourceDatabaseName' instead"  #-}

-- | The name of the source table to restore from.
--
-- /Note:/ Consider using 'sourceTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
rtfcsSourceTableName = Lens.field @"sourceTableName"
{-# INLINEABLE rtfcsSourceTableName #-}
{-# DEPRECATED sourceTableName "Use generic-lens or generic-optics with 'sourceTableName' instead"  #-}

-- | The name of the table to create as a result of the current request.
--
-- /Note:/ Consider using 'newTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsNewTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
rtfcsNewTableName = Lens.field @"newTableName"
{-# INLINEABLE rtfcsNewTableName #-}
{-# DEPRECATED newTableName "Use generic-lens or generic-optics with 'newTableName' instead"  #-}

-- | The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
--
-- /Note:/ Consider using 'sourceSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsSourceSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Core.Text)
rtfcsSourceSchemaName = Lens.field @"sourceSchemaName"
{-# INLINEABLE rtfcsSourceSchemaName #-}
{-# DEPRECATED sourceSchemaName "Use generic-lens or generic-optics with 'sourceSchemaName' instead"  #-}

-- | The name of the database to restore the table to.
--
-- /Note:/ Consider using 'targetDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsTargetDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Core.Text)
rtfcsTargetDatabaseName = Lens.field @"targetDatabaseName"
{-# INLINEABLE rtfcsTargetDatabaseName #-}
{-# DEPRECATED targetDatabaseName "Use generic-lens or generic-optics with 'targetDatabaseName' instead"  #-}

-- | The name of the schema to restore the table to.
--
-- /Note:/ Consider using 'targetSchemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsTargetSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Core.Text)
rtfcsTargetSchemaName = Lens.field @"targetSchemaName"
{-# INLINEABLE rtfcsTargetSchemaName #-}
{-# DEPRECATED targetSchemaName "Use generic-lens or generic-optics with 'targetSchemaName' instead"  #-}

instance Core.ToQuery RestoreTableFromClusterSnapshot where
        toQuery RestoreTableFromClusterSnapshot{..}
          = Core.toQueryPair "Action"
              ("RestoreTableFromClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<> Core.toQueryPair "SnapshotIdentifier" snapshotIdentifier
              Core.<> Core.toQueryPair "SourceDatabaseName" sourceDatabaseName
              Core.<> Core.toQueryPair "SourceTableName" sourceTableName
              Core.<> Core.toQueryPair "NewTableName" newTableName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceSchemaName")
                sourceSchemaName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetDatabaseName")
                targetDatabaseName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetSchemaName")
                targetSchemaName

instance Core.ToHeaders RestoreTableFromClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreTableFromClusterSnapshot where
        type Rs RestoreTableFromClusterSnapshot =
             RestoreTableFromClusterSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "RestoreTableFromClusterSnapshotResult"
              (\ s h x ->
                 RestoreTableFromClusterSnapshotResponse' Core.<$>
                   (x Core..@? "TableRestoreStatus") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreTableFromClusterSnapshotResponse' smart constructor.
data RestoreTableFromClusterSnapshotResponse = RestoreTableFromClusterSnapshotResponse'
  { tableRestoreStatus :: Core.Maybe Types.TableRestoreStatus
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreTableFromClusterSnapshotResponse' value with any optional fields omitted.
mkRestoreTableFromClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreTableFromClusterSnapshotResponse
mkRestoreTableFromClusterSnapshotResponse responseStatus
  = RestoreTableFromClusterSnapshotResponse'{tableRestoreStatus =
                                               Core.Nothing,
                                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tableRestoreStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsrrsTableRestoreStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse (Core.Maybe Types.TableRestoreStatus)
rtfcsrrsTableRestoreStatus = Lens.field @"tableRestoreStatus"
{-# INLINEABLE rtfcsrrsTableRestoreStatus #-}
{-# DEPRECATED tableRestoreStatus "Use generic-lens or generic-optics with 'tableRestoreStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcsrrsResponseStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse Core.Int
rtfcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtfcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
