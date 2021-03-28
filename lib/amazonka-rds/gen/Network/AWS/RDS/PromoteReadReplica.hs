{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PromoteReadReplica
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Promotes a read replica DB instance to a standalone DB instance.
module Network.AWS.RDS.PromoteReadReplica
    (
    -- * Creating a request
      PromoteReadReplica (..)
    , mkPromoteReadReplica
    -- ** Request lenses
    , prrDBInstanceIdentifier
    , prrBackupRetentionPeriod
    , prrPreferredBackupWindow

    -- * Destructuring the response
    , PromoteReadReplicaResponse (..)
    , mkPromoteReadReplicaResponse
    -- ** Response lenses
    , prrrrsDBInstance
    , prrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkPromoteReadReplica' smart constructor.
data PromoteReadReplica = PromoteReadReplica'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing read replica DB instance.
--
--
-- Example: @mydbinstance@ 
  , backupRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 0 to 35.
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas.
--
--
  , preferredBackupWindow :: Core.Maybe Core.Text
    -- ^ The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. 
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./ 
-- Constraints:
--
--     * Must be in the format @hh24:mi-hh24:mi@ .
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred maintenance window.
--
--
--     * Must be at least 30 minutes.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PromoteReadReplica' value with any optional fields omitted.
mkPromoteReadReplica
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> PromoteReadReplica
mkPromoteReadReplica dBInstanceIdentifier
  = PromoteReadReplica'{dBInstanceIdentifier,
                        backupRetentionPeriod = Core.Nothing,
                        preferredBackupWindow = Core.Nothing}

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing read replica DB instance.
--
--
-- Example: @mydbinstance@ 
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrDBInstanceIdentifier :: Lens.Lens' PromoteReadReplica Core.Text
prrDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE prrDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 0 to 35.
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas.
--
--
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrBackupRetentionPeriod :: Lens.Lens' PromoteReadReplica (Core.Maybe Core.Int)
prrBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# INLINEABLE prrBackupRetentionPeriod #-}
{-# DEPRECATED backupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead"  #-}

-- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. 
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./ 
-- Constraints:
--
--     * Must be in the format @hh24:mi-hh24:mi@ .
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred maintenance window.
--
--
--     * Must be at least 30 minutes.
--
--
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrPreferredBackupWindow :: Lens.Lens' PromoteReadReplica (Core.Maybe Core.Text)
prrPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# INLINEABLE prrPreferredBackupWindow #-}
{-# DEPRECATED preferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead"  #-}

instance Core.ToQuery PromoteReadReplica where
        toQuery PromoteReadReplica{..}
          = Core.toQueryPair "Action" ("PromoteReadReplica" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BackupRetentionPeriod")
                backupRetentionPeriod
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PreferredBackupWindow")
                preferredBackupWindow

instance Core.ToHeaders PromoteReadReplica where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PromoteReadReplica where
        type Rs PromoteReadReplica = PromoteReadReplicaResponse
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
          = Response.receiveXMLWrapper "PromoteReadReplicaResult"
              (\ s h x ->
                 PromoteReadReplicaResponse' Core.<$>
                   (x Core..@? "DBInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPromoteReadReplicaResponse' smart constructor.
data PromoteReadReplicaResponse = PromoteReadReplicaResponse'
  { dBInstance :: Core.Maybe Types.DBInstance
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PromoteReadReplicaResponse' value with any optional fields omitted.
mkPromoteReadReplicaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PromoteReadReplicaResponse
mkPromoteReadReplicaResponse responseStatus
  = PromoteReadReplicaResponse'{dBInstance = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrrsDBInstance :: Lens.Lens' PromoteReadReplicaResponse (Core.Maybe Types.DBInstance)
prrrrsDBInstance = Lens.field @"dBInstance"
{-# INLINEABLE prrrrsDBInstance #-}
{-# DEPRECATED dBInstance "Use generic-lens or generic-optics with 'dBInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrrsResponseStatus :: Lens.Lens' PromoteReadReplicaResponse Core.Int
prrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
