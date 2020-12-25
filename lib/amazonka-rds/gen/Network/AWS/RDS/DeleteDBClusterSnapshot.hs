{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB cluster snapshot. If the snapshot is being copied, the copy operation is terminated.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.DeleteDBClusterSnapshot
  ( -- * Creating a request
    DeleteDBClusterSnapshot (..),
    mkDeleteDBClusterSnapshot,

    -- ** Request lenses
    ddbcsDBClusterSnapshotIdentifier,

    -- * Destructuring the response
    DeleteDBClusterSnapshotResponse (..),
    mkDeleteDBClusterSnapshotResponse,

    -- ** Response lenses
    ddbcsrrsDBClusterSnapshot,
    ddbcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteDBClusterSnapshot' smart constructor.
newtype DeleteDBClusterSnapshot = DeleteDBClusterSnapshot'
  { -- | The identifier of the DB cluster snapshot to delete.
    --
    -- Constraints: Must be the name of an existing DB cluster snapshot in the @available@ state.
    dBClusterSnapshotIdentifier :: Types.DBClusterSnapshotIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBClusterSnapshot' value with any optional fields omitted.
mkDeleteDBClusterSnapshot ::
  -- | 'dBClusterSnapshotIdentifier'
  Types.DBClusterSnapshotIdentifier ->
  DeleteDBClusterSnapshot
mkDeleteDBClusterSnapshot dBClusterSnapshotIdentifier =
  DeleteDBClusterSnapshot' {dBClusterSnapshotIdentifier}

-- | The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the @available@ state.
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsDBClusterSnapshotIdentifier :: Lens.Lens' DeleteDBClusterSnapshot Types.DBClusterSnapshotIdentifier
ddbcsDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# DEPRECATED ddbcsDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead." #-}

instance Core.AWSRequest DeleteDBClusterSnapshot where
  type Rs DeleteDBClusterSnapshot = DeleteDBClusterSnapshotResponse
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
            ( Core.pure ("Action", "DeleteDBClusterSnapshot")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "DBClusterSnapshotIdentifier"
                            dBClusterSnapshotIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteDBClusterSnapshotResult"
      ( \s h x ->
          DeleteDBClusterSnapshotResponse'
            Core.<$> (x Core..@? "DBClusterSnapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDBClusterSnapshotResponse' smart constructor.
data DeleteDBClusterSnapshotResponse = DeleteDBClusterSnapshotResponse'
  { dBClusterSnapshot :: Core.Maybe Types.DBClusterSnapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteDBClusterSnapshotResponse' value with any optional fields omitted.
mkDeleteDBClusterSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDBClusterSnapshotResponse
mkDeleteDBClusterSnapshotResponse responseStatus =
  DeleteDBClusterSnapshotResponse'
    { dBClusterSnapshot =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrrsDBClusterSnapshot :: Lens.Lens' DeleteDBClusterSnapshotResponse (Core.Maybe Types.DBClusterSnapshot)
ddbcsrrsDBClusterSnapshot = Lens.field @"dBClusterSnapshot"
{-# DEPRECATED ddbcsrrsDBClusterSnapshot "Use generic-lens or generic-optics with 'dBClusterSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrrsResponseStatus :: Lens.Lens' DeleteDBClusterSnapshotResponse Core.Int
ddbcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
