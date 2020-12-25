{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StopDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon RDS DB instance. When you stop a DB instance, Amazon RDS retains the DB instance's metadata, including its endpoint, DB parameter group, and option group membership. Amazon RDS also retains the transaction logs so you can do a point-in-time restore if necessary.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_StopInstance.html Stopping an Amazon RDS DB Instance Temporarily> in the /Amazon RDS User Guide./
module Network.AWS.RDS.StopDBInstance
  ( -- * Creating a request
    StopDBInstance (..),
    mkStopDBInstance,

    -- ** Request lenses
    sdbiDBInstanceIdentifier,
    sdbiDBSnapshotIdentifier,

    -- * Destructuring the response
    StopDBInstanceResponse (..),
    mkStopDBInstanceResponse,

    -- ** Response lenses
    sdbirrsDBInstance,
    sdbirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopDBInstance' smart constructor.
data StopDBInstance = StopDBInstance'
  { -- | The user-supplied instance identifier.
    dBInstanceIdentifier :: Types.DBInstanceIdentifier,
    -- | The user-supplied instance identifier of the DB Snapshot created immediately before the DB instance is stopped.
    dBSnapshotIdentifier :: Core.Maybe Types.DBSnapshotIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopDBInstance' value with any optional fields omitted.
mkStopDBInstance ::
  -- | 'dBInstanceIdentifier'
  Types.DBInstanceIdentifier ->
  StopDBInstance
mkStopDBInstance dBInstanceIdentifier =
  StopDBInstance'
    { dBInstanceIdentifier,
      dBSnapshotIdentifier = Core.Nothing
    }

-- | The user-supplied instance identifier.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbiDBInstanceIdentifier :: Lens.Lens' StopDBInstance Types.DBInstanceIdentifier
sdbiDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED sdbiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | The user-supplied instance identifier of the DB Snapshot created immediately before the DB instance is stopped.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbiDBSnapshotIdentifier :: Lens.Lens' StopDBInstance (Core.Maybe Types.DBSnapshotIdentifier)
sdbiDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# DEPRECATED sdbiDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead." #-}

instance Core.AWSRequest StopDBInstance where
  type Rs StopDBInstance = StopDBInstanceResponse
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
            ( Core.pure ("Action", "StopDBInstance")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceIdentifier" dBInstanceIdentifier)
                Core.<> ( Core.toQueryValue "DBSnapshotIdentifier"
                            Core.<$> dBSnapshotIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "StopDBInstanceResult"
      ( \s h x ->
          StopDBInstanceResponse'
            Core.<$> (x Core..@? "DBInstance") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopDBInstanceResponse' smart constructor.
data StopDBInstanceResponse = StopDBInstanceResponse'
  { dBInstance :: Core.Maybe Types.DBInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StopDBInstanceResponse' value with any optional fields omitted.
mkStopDBInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopDBInstanceResponse
mkStopDBInstanceResponse responseStatus =
  StopDBInstanceResponse'
    { dBInstance = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbirrsDBInstance :: Lens.Lens' StopDBInstanceResponse (Core.Maybe Types.DBInstance)
sdbirrsDBInstance = Lens.field @"dBInstance"
{-# DEPRECATED sdbirrsDBInstance "Use generic-lens or generic-optics with 'dBInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbirrsResponseStatus :: Lens.Lens' StopDBInstanceResponse Core.Int
sdbirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sdbirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
