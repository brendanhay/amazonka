{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.BatchDeleteClusterSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of cluster snapshots.
module Network.AWS.Redshift.BatchDeleteClusterSnapshots
  ( -- * Creating a request
    BatchDeleteClusterSnapshots (..),
    mkBatchDeleteClusterSnapshots,

    -- ** Request lenses
    bdcsIdentifiers,

    -- * Destructuring the response
    BatchDeleteClusterSnapshotsResponse (..),
    mkBatchDeleteClusterSnapshotsResponse,

    -- ** Response lenses
    bdcsrrsErrors,
    bdcsrrsResources,
    bdcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteClusterSnapshots' smart constructor.
newtype BatchDeleteClusterSnapshots = BatchDeleteClusterSnapshots'
  { -- | A list of identifiers for the snapshots that you want to delete.
    identifiers :: [Types.DeleteClusterSnapshotMessage]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteClusterSnapshots' value with any optional fields omitted.
mkBatchDeleteClusterSnapshots ::
  BatchDeleteClusterSnapshots
mkBatchDeleteClusterSnapshots =
  BatchDeleteClusterSnapshots' {identifiers = Core.mempty}

-- | A list of identifiers for the snapshots that you want to delete.
--
-- /Note:/ Consider using 'identifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcsIdentifiers :: Lens.Lens' BatchDeleteClusterSnapshots [Types.DeleteClusterSnapshotMessage]
bdcsIdentifiers = Lens.field @"identifiers"
{-# DEPRECATED bdcsIdentifiers "Use generic-lens or generic-optics with 'identifiers' instead." #-}

instance Core.AWSRequest BatchDeleteClusterSnapshots where
  type
    Rs BatchDeleteClusterSnapshots =
      BatchDeleteClusterSnapshotsResponse
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
            ( Core.pure ("Action", "BatchDeleteClusterSnapshots")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue
                            "Identifiers"
                            (Core.toQueryList "DeleteClusterSnapshotMessage" identifiers)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "BatchDeleteClusterSnapshotsResult"
      ( \s h x ->
          BatchDeleteClusterSnapshotsResponse'
            Core.<$> ( x Core..@? "Errors"
                         Core..<@> Core.parseXMLList "SnapshotErrorMessage"
                     )
            Core.<*> (x Core..@? "Resources" Core..<@> Core.parseXMLList "String")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDeleteClusterSnapshotsResponse' smart constructor.
data BatchDeleteClusterSnapshotsResponse = BatchDeleteClusterSnapshotsResponse'
  { -- | A list of any errors returned.
    errors :: Core.Maybe [Types.SnapshotErrorMessage],
    -- | A list of the snapshot identifiers that were deleted.
    resources :: Core.Maybe [Types.String],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteClusterSnapshotsResponse' value with any optional fields omitted.
mkBatchDeleteClusterSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchDeleteClusterSnapshotsResponse
mkBatchDeleteClusterSnapshotsResponse responseStatus =
  BatchDeleteClusterSnapshotsResponse'
    { errors = Core.Nothing,
      resources = Core.Nothing,
      responseStatus
    }

-- | A list of any errors returned.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcsrrsErrors :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Core.Maybe [Types.SnapshotErrorMessage])
bdcsrrsErrors = Lens.field @"errors"
{-# DEPRECATED bdcsrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | A list of the snapshot identifiers that were deleted.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcsrrsResources :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Core.Maybe [Types.String])
bdcsrrsResources = Lens.field @"resources"
{-# DEPRECATED bdcsrrsResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcsrrsResponseStatus :: Lens.Lens' BatchDeleteClusterSnapshotsResponse Core.Int
bdcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bdcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
