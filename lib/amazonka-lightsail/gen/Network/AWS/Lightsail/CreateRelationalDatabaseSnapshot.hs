{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of your database in Amazon Lightsail. You can use snapshots for backups, to make copies of a database, and to save data before deleting a database.
--
-- The @create relational database snapshot@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
  ( -- * Creating a request
    CreateRelationalDatabaseSnapshot (..),
    mkCreateRelationalDatabaseSnapshot,

    -- ** Request lenses
    crdsRelationalDatabaseName,
    crdsRelationalDatabaseSnapshotName,
    crdsTags,

    -- * Destructuring the response
    CreateRelationalDatabaseSnapshotResponse (..),
    mkCreateRelationalDatabaseSnapshotResponse,

    -- ** Response lenses
    crdsrrsOperations,
    crdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRelationalDatabaseSnapshot' smart constructor.
data CreateRelationalDatabaseSnapshot = CreateRelationalDatabaseSnapshot'
  { -- | The name of the database on which to base your new snapshot.
    relationalDatabaseName :: Types.RelationalDatabaseName,
    -- | The name for your new database snapshot.
    --
    -- Constraints:
    --
    --     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    --
    --     * The first and last character must be a letter or number.
    relationalDatabaseSnapshotName :: Types.RelationalDatabaseSnapshotName,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRelationalDatabaseSnapshot' value with any optional fields omitted.
mkCreateRelationalDatabaseSnapshot ::
  -- | 'relationalDatabaseName'
  Types.RelationalDatabaseName ->
  -- | 'relationalDatabaseSnapshotName'
  Types.RelationalDatabaseSnapshotName ->
  CreateRelationalDatabaseSnapshot
mkCreateRelationalDatabaseSnapshot
  relationalDatabaseName
  relationalDatabaseSnapshotName =
    CreateRelationalDatabaseSnapshot'
      { relationalDatabaseName,
        relationalDatabaseSnapshotName,
        tags = Core.Nothing
      }

-- | The name of the database on which to base your new snapshot.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsRelationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseSnapshot Types.RelationalDatabaseName
crdsRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# DEPRECATED crdsRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The name for your new database snapshot.
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
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsRelationalDatabaseSnapshotName :: Lens.Lens' CreateRelationalDatabaseSnapshot Types.RelationalDatabaseSnapshotName
crdsRelationalDatabaseSnapshotName = Lens.field @"relationalDatabaseSnapshotName"
{-# DEPRECATED crdsRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsTags :: Lens.Lens' CreateRelationalDatabaseSnapshot (Core.Maybe [Types.Tag])
crdsTags = Lens.field @"tags"
{-# DEPRECATED crdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateRelationalDatabaseSnapshot where
  toJSON CreateRelationalDatabaseSnapshot {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("relationalDatabaseName" Core..= relationalDatabaseName),
            Core.Just
              ( "relationalDatabaseSnapshotName"
                  Core..= relationalDatabaseSnapshotName
              ),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateRelationalDatabaseSnapshot where
  type
    Rs CreateRelationalDatabaseSnapshot =
      CreateRelationalDatabaseSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.CreateRelationalDatabaseSnapshot"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseSnapshotResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRelationalDatabaseSnapshotResponse' smart constructor.
data CreateRelationalDatabaseSnapshotResponse = CreateRelationalDatabaseSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateRelationalDatabaseSnapshotResponse' value with any optional fields omitted.
mkCreateRelationalDatabaseSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRelationalDatabaseSnapshotResponse
mkCreateRelationalDatabaseSnapshotResponse responseStatus =
  CreateRelationalDatabaseSnapshotResponse'
    { operations =
        Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrrsOperations :: Lens.Lens' CreateRelationalDatabaseSnapshotResponse (Core.Maybe [Types.Operation])
crdsrrsOperations = Lens.field @"operations"
{-# DEPRECATED crdsrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrrsResponseStatus :: Lens.Lens' CreateRelationalDatabaseSnapshotResponse Core.Int
crdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
