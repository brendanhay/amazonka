{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database snapshot in Amazon Lightsail.
--
-- The @delete relational database snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
  ( -- * Creating a request
    DeleteRelationalDatabaseSnapshot (..),
    mkDeleteRelationalDatabaseSnapshot,

    -- ** Request lenses
    drdsRelationalDatabaseSnapshotName,

    -- * Destructuring the response
    DeleteRelationalDatabaseSnapshotResponse (..),
    mkDeleteRelationalDatabaseSnapshotResponse,

    -- ** Response lenses
    drdsrrsOperations,
    drdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRelationalDatabaseSnapshot' smart constructor.
newtype DeleteRelationalDatabaseSnapshot = DeleteRelationalDatabaseSnapshot'
  { -- | The name of the database snapshot that you are deleting.
    relationalDatabaseSnapshotName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRelationalDatabaseSnapshot' value with any optional fields omitted.
mkDeleteRelationalDatabaseSnapshot ::
  -- | 'relationalDatabaseSnapshotName'
  Types.ResourceName ->
  DeleteRelationalDatabaseSnapshot
mkDeleteRelationalDatabaseSnapshot relationalDatabaseSnapshotName =
  DeleteRelationalDatabaseSnapshot' {relationalDatabaseSnapshotName}

-- | The name of the database snapshot that you are deleting.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsRelationalDatabaseSnapshotName :: Lens.Lens' DeleteRelationalDatabaseSnapshot Types.ResourceName
drdsRelationalDatabaseSnapshotName = Lens.field @"relationalDatabaseSnapshotName"
{-# DEPRECATED drdsRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead." #-}

instance Core.FromJSON DeleteRelationalDatabaseSnapshot where
  toJSON DeleteRelationalDatabaseSnapshot {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "relationalDatabaseSnapshotName"
                  Core..= relationalDatabaseSnapshotName
              )
          ]
      )

instance Core.AWSRequest DeleteRelationalDatabaseSnapshot where
  type
    Rs DeleteRelationalDatabaseSnapshot =
      DeleteRelationalDatabaseSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.DeleteRelationalDatabaseSnapshot"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRelationalDatabaseSnapshotResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRelationalDatabaseSnapshotResponse' smart constructor.
data DeleteRelationalDatabaseSnapshotResponse = DeleteRelationalDatabaseSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteRelationalDatabaseSnapshotResponse' value with any optional fields omitted.
mkDeleteRelationalDatabaseSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRelationalDatabaseSnapshotResponse
mkDeleteRelationalDatabaseSnapshotResponse responseStatus =
  DeleteRelationalDatabaseSnapshotResponse'
    { operations =
        Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrrsOperations :: Lens.Lens' DeleteRelationalDatabaseSnapshotResponse (Core.Maybe [Types.Operation])
drdsrrsOperations = Lens.field @"operations"
{-# DEPRECATED drdsrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrrsResponseStatus :: Lens.Lens' DeleteRelationalDatabaseSnapshotResponse Core.Int
drdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
