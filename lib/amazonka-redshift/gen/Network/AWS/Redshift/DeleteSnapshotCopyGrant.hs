{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteSnapshotCopyGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot copy grant.
module Network.AWS.Redshift.DeleteSnapshotCopyGrant
  ( -- * Creating a request
    DeleteSnapshotCopyGrant (..),
    mkDeleteSnapshotCopyGrant,

    -- ** Request lenses
    dscgSnapshotCopyGrantName,

    -- * Destructuring the response
    DeleteSnapshotCopyGrantResponse (..),
    mkDeleteSnapshotCopyGrantResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result of the @DeleteSnapshotCopyGrant@ action.
--
-- /See:/ 'mkDeleteSnapshotCopyGrant' smart constructor.
newtype DeleteSnapshotCopyGrant = DeleteSnapshotCopyGrant'
  { -- | The name of the snapshot copy grant to delete.
    snapshotCopyGrantName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotCopyGrant' value with any optional fields omitted.
mkDeleteSnapshotCopyGrant ::
  -- | 'snapshotCopyGrantName'
  Types.String ->
  DeleteSnapshotCopyGrant
mkDeleteSnapshotCopyGrant snapshotCopyGrantName =
  DeleteSnapshotCopyGrant' {snapshotCopyGrantName}

-- | The name of the snapshot copy grant to delete.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgSnapshotCopyGrantName :: Lens.Lens' DeleteSnapshotCopyGrant Types.String
dscgSnapshotCopyGrantName = Lens.field @"snapshotCopyGrantName"
{-# DEPRECATED dscgSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

instance Core.AWSRequest DeleteSnapshotCopyGrant where
  type Rs DeleteSnapshotCopyGrant = DeleteSnapshotCopyGrantResponse
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
            ( Core.pure ("Action", "DeleteSnapshotCopyGrant")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "SnapshotCopyGrantName" snapshotCopyGrantName)
            )
      }
  response = Response.receiveNull DeleteSnapshotCopyGrantResponse'

-- | /See:/ 'mkDeleteSnapshotCopyGrantResponse' smart constructor.
data DeleteSnapshotCopyGrantResponse = DeleteSnapshotCopyGrantResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotCopyGrantResponse' value with any optional fields omitted.
mkDeleteSnapshotCopyGrantResponse ::
  DeleteSnapshotCopyGrantResponse
mkDeleteSnapshotCopyGrantResponse =
  DeleteSnapshotCopyGrantResponse'
