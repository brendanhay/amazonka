{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB snapshot. If the snapshot is being copied, the copy operation is terminated.
module Network.AWS.RDS.DeleteDBSnapshot
  ( -- * Creating a request
    DeleteDBSnapshot (..),
    mkDeleteDBSnapshot,

    -- ** Request lenses
    dDBSnapshotIdentifier,

    -- * Destructuring the response
    DeleteDBSnapshotResponse (..),
    mkDeleteDBSnapshotResponse,

    -- ** Response lenses
    ddbsrfrsDBSnapshot,
    ddbsrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteDBSnapshot' smart constructor.
newtype DeleteDBSnapshot = DeleteDBSnapshot'
  { -- | The DB snapshot identifier.
    --
    -- Constraints: Must be the name of an existing DB snapshot in the @available@ state.
    dBSnapshotIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSnapshot' value with any optional fields omitted.
mkDeleteDBSnapshot ::
  -- | 'dBSnapshotIdentifier'
  Types.String ->
  DeleteDBSnapshot
mkDeleteDBSnapshot dBSnapshotIdentifier =
  DeleteDBSnapshot' {dBSnapshotIdentifier}

-- | The DB snapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the @available@ state.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBSnapshotIdentifier :: Lens.Lens' DeleteDBSnapshot Types.String
dDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# DEPRECATED dDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead." #-}

instance Core.AWSRequest DeleteDBSnapshot where
  type Rs DeleteDBSnapshot = DeleteDBSnapshotResponse
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
            ( Core.pure ("Action", "DeleteDBSnapshot")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSnapshotIdentifier" dBSnapshotIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteDBSnapshotResult"
      ( \s h x ->
          DeleteDBSnapshotResponse'
            Core.<$> (x Core..@? "DBSnapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDBSnapshotResponse' smart constructor.
data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse'
  { dBSnapshot :: Core.Maybe Types.DBSnapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteDBSnapshotResponse' value with any optional fields omitted.
mkDeleteDBSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDBSnapshotResponse
mkDeleteDBSnapshotResponse responseStatus =
  DeleteDBSnapshotResponse'
    { dBSnapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrfrsDBSnapshot :: Lens.Lens' DeleteDBSnapshotResponse (Core.Maybe Types.DBSnapshot)
ddbsrfrsDBSnapshot = Lens.field @"dBSnapshot"
{-# DEPRECATED ddbsrfrsDBSnapshot "Use generic-lens or generic-optics with 'dBSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrfrsResponseStatus :: Lens.Lens' DeleteDBSnapshotResponse Core.Int
ddbsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
