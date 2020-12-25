{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database in Amazon Lightsail.
--
-- The @delete relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteRelationalDatabase
  ( -- * Creating a request
    DeleteRelationalDatabase (..),
    mkDeleteRelationalDatabase,

    -- ** Request lenses
    drdRelationalDatabaseName,
    drdFinalRelationalDatabaseSnapshotName,
    drdSkipFinalSnapshot,

    -- * Destructuring the response
    DeleteRelationalDatabaseResponse (..),
    mkDeleteRelationalDatabaseResponse,

    -- ** Response lenses
    drdrrsOperations,
    drdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRelationalDatabase' smart constructor.
data DeleteRelationalDatabase = DeleteRelationalDatabase'
  { -- | The name of the database that you are deleting.
    relationalDatabaseName :: Types.ResourceName,
    -- | The name of the database snapshot created if @skip final snapshot@ is @false@ , which is the default value for that parameter.
    --
    -- Constraints:
    --
    --     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    --
    --     * The first and last character must be a letter or number.
    finalRelationalDatabaseSnapshotName :: Core.Maybe Types.ResourceName,
    -- | Determines whether a final database snapshot is created before your database is deleted. If @true@ is specified, no database snapshot is created. If @false@ is specified, a database snapshot is created before your database is deleted.
    --
    -- You must specify the @final relational database snapshot name@ parameter if the @skip final snapshot@ parameter is @false@ .
    -- Default: @false@
    skipFinalSnapshot :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRelationalDatabase' value with any optional fields omitted.
mkDeleteRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Types.ResourceName ->
  DeleteRelationalDatabase
mkDeleteRelationalDatabase relationalDatabaseName =
  DeleteRelationalDatabase'
    { relationalDatabaseName,
      finalRelationalDatabaseSnapshotName = Core.Nothing,
      skipFinalSnapshot = Core.Nothing
    }

-- | The name of the database that you are deleting.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdRelationalDatabaseName :: Lens.Lens' DeleteRelationalDatabase Types.ResourceName
drdRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# DEPRECATED drdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The name of the database snapshot created if @skip final snapshot@ is @false@ , which is the default value for that parameter.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
--
-- /Note:/ Consider using 'finalRelationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdFinalRelationalDatabaseSnapshotName :: Lens.Lens' DeleteRelationalDatabase (Core.Maybe Types.ResourceName)
drdFinalRelationalDatabaseSnapshotName = Lens.field @"finalRelationalDatabaseSnapshotName"
{-# DEPRECATED drdFinalRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'finalRelationalDatabaseSnapshotName' instead." #-}

-- | Determines whether a final database snapshot is created before your database is deleted. If @true@ is specified, no database snapshot is created. If @false@ is specified, a database snapshot is created before your database is deleted.
--
-- You must specify the @final relational database snapshot name@ parameter if the @skip final snapshot@ parameter is @false@ .
-- Default: @false@
--
-- /Note:/ Consider using 'skipFinalSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdSkipFinalSnapshot :: Lens.Lens' DeleteRelationalDatabase (Core.Maybe Core.Bool)
drdSkipFinalSnapshot = Lens.field @"skipFinalSnapshot"
{-# DEPRECATED drdSkipFinalSnapshot "Use generic-lens or generic-optics with 'skipFinalSnapshot' instead." #-}

instance Core.FromJSON DeleteRelationalDatabase where
  toJSON DeleteRelationalDatabase {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("relationalDatabaseName" Core..= relationalDatabaseName),
            ("finalRelationalDatabaseSnapshotName" Core..=)
              Core.<$> finalRelationalDatabaseSnapshotName,
            ("skipFinalSnapshot" Core..=) Core.<$> skipFinalSnapshot
          ]
      )

instance Core.AWSRequest DeleteRelationalDatabase where
  type Rs DeleteRelationalDatabase = DeleteRelationalDatabaseResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.DeleteRelationalDatabase")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRelationalDatabaseResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRelationalDatabaseResponse' smart constructor.
data DeleteRelationalDatabaseResponse = DeleteRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteRelationalDatabaseResponse' value with any optional fields omitted.
mkDeleteRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRelationalDatabaseResponse
mkDeleteRelationalDatabaseResponse responseStatus =
  DeleteRelationalDatabaseResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrrsOperations :: Lens.Lens' DeleteRelationalDatabaseResponse (Core.Maybe [Types.Operation])
drdrrsOperations = Lens.field @"operations"
{-# DEPRECATED drdrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrrsResponseStatus :: Lens.Lens' DeleteRelationalDatabaseResponse Core.Int
drdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
