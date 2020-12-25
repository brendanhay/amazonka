{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteAutoSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an automatic snapshot of an instance or disk. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteAutoSnapshot
  ( -- * Creating a request
    DeleteAutoSnapshot (..),
    mkDeleteAutoSnapshot,

    -- ** Request lenses
    dasResourceName,
    dasDate,

    -- * Destructuring the response
    DeleteAutoSnapshotResponse (..),
    mkDeleteAutoSnapshotResponse,

    -- ** Response lenses
    dasrrsOperations,
    dasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAutoSnapshot' smart constructor.
data DeleteAutoSnapshot = DeleteAutoSnapshot'
  { -- | The name of the source instance or disk from which to delete the automatic snapshot.
    resourceName :: Types.ResourceName,
    -- | The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use the @get auto snapshots@ operation to get the available automatic snapshots for a resource.
    date :: Types.Date
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAutoSnapshot' value with any optional fields omitted.
mkDeleteAutoSnapshot ::
  -- | 'resourceName'
  Types.ResourceName ->
  -- | 'date'
  Types.Date ->
  DeleteAutoSnapshot
mkDeleteAutoSnapshot resourceName date =
  DeleteAutoSnapshot' {resourceName, date}

-- | The name of the source instance or disk from which to delete the automatic snapshot.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasResourceName :: Lens.Lens' DeleteAutoSnapshot Types.ResourceName
dasResourceName = Lens.field @"resourceName"
{-# DEPRECATED dasResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use the @get auto snapshots@ operation to get the available automatic snapshots for a resource.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasDate :: Lens.Lens' DeleteAutoSnapshot Types.Date
dasDate = Lens.field @"date"
{-# DEPRECATED dasDate "Use generic-lens or generic-optics with 'date' instead." #-}

instance Core.FromJSON DeleteAutoSnapshot where
  toJSON DeleteAutoSnapshot {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceName" Core..= resourceName),
            Core.Just ("date" Core..= date)
          ]
      )

instance Core.AWSRequest DeleteAutoSnapshot where
  type Rs DeleteAutoSnapshot = DeleteAutoSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteAutoSnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAutoSnapshotResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAutoSnapshotResponse' smart constructor.
data DeleteAutoSnapshotResponse = DeleteAutoSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteAutoSnapshotResponse' value with any optional fields omitted.
mkDeleteAutoSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAutoSnapshotResponse
mkDeleteAutoSnapshotResponse responseStatus =
  DeleteAutoSnapshotResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsOperations :: Lens.Lens' DeleteAutoSnapshotResponse (Core.Maybe [Types.Operation])
dasrrsOperations = Lens.field @"operations"
{-# DEPRECATED dasrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsResponseStatus :: Lens.Lens' DeleteAutoSnapshotResponse Core.Int
dasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
