{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBInstance action deletes a previously provisioned DB instance. When you delete a DB instance, all automated backups for that instance are deleted and can't be recovered. Manual DB snapshots of the DB instance to be deleted by @DeleteDBInstance@ are not deleted.
--
-- If you request a final DB snapshot the status of the Amazon RDS DB instance is @deleting@ until the DB snapshot is created. The API action @DescribeDBInstance@ is used to monitor the status of this operation. The action can't be canceled or reverted once submitted. 
-- When a DB instance is in a failure state and has a status of @failed@ , @incompatible-restore@ , or @incompatible-network@ , you can only delete it when you skip creation of the final snapshot with the @SkipFinalSnapshot@ parameter.
-- If the specified DB instance is part of an Amazon Aurora DB cluster, you can't delete the DB instance if both of the following conditions are true:
--
--     * The DB cluster is a read replica of another Amazon Aurora DB cluster.
--
--
--     * The DB instance is the only instance in the DB cluster.
--
--
-- To delete a DB instance in this case, first call the @PromoteReadReplicaDBCluster@ API action to promote the DB cluster so it's no longer a read replica. After the promotion completes, then call the @DeleteDBInstance@ API action to delete the final instance in the DB cluster.
module Network.AWS.RDS.DeleteDBInstance
    (
    -- * Creating a request
      DeleteDBInstance (..)
    , mkDeleteDBInstance
    -- ** Request lenses
    , ddbiDBInstanceIdentifier
    , ddbiDeleteAutomatedBackups
    , ddbiFinalDBSnapshotIdentifier
    , ddbiSkipFinalSnapshot

    -- * Destructuring the response
    , DeleteDBInstanceResponse (..)
    , mkDeleteDBInstanceResponse
    -- ** Response lenses
    , ddbirrsDBInstance
    , ddbirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteDBInstance' smart constructor.
data DeleteDBInstance = DeleteDBInstance'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The DB instance identifier for the DB instance to be deleted. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the name of an existing DB instance.
--
--
  , deleteAutomatedBackups :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to remove automated backups immediately after the DB instance is deleted. This parameter isn't case-sensitive. The default is to remove automated backups immediately after the DB instance is deleted.
  , finalDBSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the @SkipFinalSnapshot@ parameter is disabled. 
--
-- Constraints:
--
--     * Must be 1 to 255 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
--     * Can't be specified when deleting a read replica.
--
--
  , skipFinalSnapshot :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to skip the creation of a final DB snapshot before the DB instance is deleted. If skip is specified, no DB snapshot is created. If skip isn't specified, a DB snapshot is created before the DB instance is deleted. By default, skip isn't specified, and the DB snapshot is created.
--
-- When a DB instance is in a failure state and has a status of 'failed', 'incompatible-restore', or 'incompatible-network', it can only be deleted when skip is specified.
-- Specify skip when deleting a read replica.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBInstance' value with any optional fields omitted.
mkDeleteDBInstance
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> DeleteDBInstance
mkDeleteDBInstance dBInstanceIdentifier
  = DeleteDBInstance'{dBInstanceIdentifier,
                      deleteAutomatedBackups = Core.Nothing,
                      finalDBSnapshotIdentifier = Core.Nothing,
                      skipFinalSnapshot = Core.Nothing}

-- | The DB instance identifier for the DB instance to be deleted. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the name of an existing DB instance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiDBInstanceIdentifier :: Lens.Lens' DeleteDBInstance Core.Text
ddbiDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE ddbiDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | A value that indicates whether to remove automated backups immediately after the DB instance is deleted. This parameter isn't case-sensitive. The default is to remove automated backups immediately after the DB instance is deleted.
--
-- /Note:/ Consider using 'deleteAutomatedBackups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiDeleteAutomatedBackups :: Lens.Lens' DeleteDBInstance (Core.Maybe Core.Bool)
ddbiDeleteAutomatedBackups = Lens.field @"deleteAutomatedBackups"
{-# INLINEABLE ddbiDeleteAutomatedBackups #-}
{-# DEPRECATED deleteAutomatedBackups "Use generic-lens or generic-optics with 'deleteAutomatedBackups' instead"  #-}

-- | The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the @SkipFinalSnapshot@ parameter is disabled. 
--
-- Constraints:
--
--     * Must be 1 to 255 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
--     * Can't be specified when deleting a read replica.
--
--
--
-- /Note:/ Consider using 'finalDBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiFinalDBSnapshotIdentifier :: Lens.Lens' DeleteDBInstance (Core.Maybe Core.Text)
ddbiFinalDBSnapshotIdentifier = Lens.field @"finalDBSnapshotIdentifier"
{-# INLINEABLE ddbiFinalDBSnapshotIdentifier #-}
{-# DEPRECATED finalDBSnapshotIdentifier "Use generic-lens or generic-optics with 'finalDBSnapshotIdentifier' instead"  #-}

-- | A value that indicates whether to skip the creation of a final DB snapshot before the DB instance is deleted. If skip is specified, no DB snapshot is created. If skip isn't specified, a DB snapshot is created before the DB instance is deleted. By default, skip isn't specified, and the DB snapshot is created.
--
-- When a DB instance is in a failure state and has a status of 'failed', 'incompatible-restore', or 'incompatible-network', it can only be deleted when skip is specified.
-- Specify skip when deleting a read replica.
--
-- /Note:/ Consider using 'skipFinalSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiSkipFinalSnapshot :: Lens.Lens' DeleteDBInstance (Core.Maybe Core.Bool)
ddbiSkipFinalSnapshot = Lens.field @"skipFinalSnapshot"
{-# INLINEABLE ddbiSkipFinalSnapshot #-}
{-# DEPRECATED skipFinalSnapshot "Use generic-lens or generic-optics with 'skipFinalSnapshot' instead"  #-}

instance Core.ToQuery DeleteDBInstance where
        toQuery DeleteDBInstance{..}
          = Core.toQueryPair "Action" ("DeleteDBInstance" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeleteAutomatedBackups")
                deleteAutomatedBackups
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "FinalDBSnapshotIdentifier")
                finalDBSnapshotIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SkipFinalSnapshot")
                skipFinalSnapshot

instance Core.ToHeaders DeleteDBInstance where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBInstance where
        type Rs DeleteDBInstance = DeleteDBInstanceResponse
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
          = Response.receiveXMLWrapper "DeleteDBInstanceResult"
              (\ s h x ->
                 DeleteDBInstanceResponse' Core.<$>
                   (x Core..@? "DBInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDBInstanceResponse' smart constructor.
data DeleteDBInstanceResponse = DeleteDBInstanceResponse'
  { dBInstance :: Core.Maybe Types.DBInstance
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDBInstanceResponse' value with any optional fields omitted.
mkDeleteDBInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDBInstanceResponse
mkDeleteDBInstanceResponse responseStatus
  = DeleteDBInstanceResponse'{dBInstance = Core.Nothing,
                              responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirrsDBInstance :: Lens.Lens' DeleteDBInstanceResponse (Core.Maybe Types.DBInstance)
ddbirrsDBInstance = Lens.field @"dBInstance"
{-# INLINEABLE ddbirrsDBInstance #-}
{-# DEPRECATED dBInstance "Use generic-lens or generic-optics with 'dBInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirrsResponseStatus :: Lens.Lens' DeleteDBInstanceResponse Core.Int
ddbirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
