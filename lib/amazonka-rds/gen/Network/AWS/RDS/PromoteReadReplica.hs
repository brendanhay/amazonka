{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PromoteReadReplica (..),
    mkPromoteReadReplica,

    -- ** Request lenses
    prrDBInstanceIdentifier,
    prrBackupRetentionPeriod,
    prrPreferredBackupWindow,

    -- * Destructuring the response
    PromoteReadReplicaResponse (..),
    mkPromoteReadReplicaResponse,

    -- ** Response lenses
    prrrrsDBInstance,
    prrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkPromoteReadReplica' smart constructor.
data PromoteReadReplica = PromoteReadReplica'
  { -- | The DB instance identifier. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing read replica DB instance.
    --
    --
    -- Example: @mydbinstance@
    dBInstanceIdentifier :: Types.String,
    -- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
    --
    -- Default: 1
    -- Constraints:
    --
    --     * Must be a value from 0 to 35.
    --
    --
    --     * Can't be set to 0 if the DB instance is a source to read replicas.
    backupRetentionPeriod :: Core.Maybe Core.Int,
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
    preferredBackupWindow :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PromoteReadReplica' value with any optional fields omitted.
mkPromoteReadReplica ::
  -- | 'dBInstanceIdentifier'
  Types.String ->
  PromoteReadReplica
mkPromoteReadReplica dBInstanceIdentifier =
  PromoteReadReplica'
    { dBInstanceIdentifier,
      backupRetentionPeriod = Core.Nothing,
      preferredBackupWindow = Core.Nothing
    }

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
prrDBInstanceIdentifier :: Lens.Lens' PromoteReadReplica Types.String
prrDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED prrDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

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
{-# DEPRECATED prrBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

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
prrPreferredBackupWindow :: Lens.Lens' PromoteReadReplica (Core.Maybe Types.String)
prrPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED prrPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

instance Core.AWSRequest PromoteReadReplica where
  type Rs PromoteReadReplica = PromoteReadReplicaResponse
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
            ( Core.pure ("Action", "PromoteReadReplica")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceIdentifier" dBInstanceIdentifier)
                Core.<> ( Core.toQueryValue "BackupRetentionPeriod"
                            Core.<$> backupRetentionPeriod
                        )
                Core.<> ( Core.toQueryValue "PreferredBackupWindow"
                            Core.<$> preferredBackupWindow
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "PromoteReadReplicaResult"
      ( \s h x ->
          PromoteReadReplicaResponse'
            Core.<$> (x Core..@? "DBInstance") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPromoteReadReplicaResponse' smart constructor.
data PromoteReadReplicaResponse = PromoteReadReplicaResponse'
  { dBInstance :: Core.Maybe Types.DBInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PromoteReadReplicaResponse' value with any optional fields omitted.
mkPromoteReadReplicaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PromoteReadReplicaResponse
mkPromoteReadReplicaResponse responseStatus =
  PromoteReadReplicaResponse'
    { dBInstance = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrrsDBInstance :: Lens.Lens' PromoteReadReplicaResponse (Core.Maybe Types.DBInstance)
prrrrsDBInstance = Lens.field @"dBInstance"
{-# DEPRECATED prrrrsDBInstance "Use generic-lens or generic-optics with 'dBInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrrsResponseStatus :: Lens.Lens' PromoteReadReplicaResponse Core.Int
prrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
