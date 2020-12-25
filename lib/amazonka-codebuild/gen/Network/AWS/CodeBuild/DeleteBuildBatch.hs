{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch build.
module Network.AWS.CodeBuild.DeleteBuildBatch
  ( -- * Creating a request
    DeleteBuildBatch (..),
    mkDeleteBuildBatch,

    -- ** Request lenses
    dbbId,

    -- * Destructuring the response
    DeleteBuildBatchResponse (..),
    mkDeleteBuildBatchResponse,

    -- ** Response lenses
    dbbrrsBuildsDeleted,
    dbbrrsBuildsNotDeleted,
    dbbrrsStatusCode,
    dbbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBuildBatch' smart constructor.
newtype DeleteBuildBatch = DeleteBuildBatch'
  { -- | The identifier of the batch build to delete.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBuildBatch' value with any optional fields omitted.
mkDeleteBuildBatch ::
  -- | 'id'
  Types.Id ->
  DeleteBuildBatch
mkDeleteBuildBatch id = DeleteBuildBatch' {id}

-- | The identifier of the batch build to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbId :: Lens.Lens' DeleteBuildBatch Types.Id
dbbId = Lens.field @"id"
{-# DEPRECATED dbbId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON DeleteBuildBatch where
  toJSON DeleteBuildBatch {..} =
    Core.object (Core.catMaybes [Core.Just ("id" Core..= id)])

instance Core.AWSRequest DeleteBuildBatch where
  type Rs DeleteBuildBatch = DeleteBuildBatchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.DeleteBuildBatch")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBuildBatchResponse'
            Core.<$> (x Core..:? "buildsDeleted")
            Core.<*> (x Core..:? "buildsNotDeleted")
            Core.<*> (x Core..:? "statusCode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteBuildBatchResponse' smart constructor.
data DeleteBuildBatchResponse = DeleteBuildBatchResponse'
  { -- | An array of strings that contain the identifiers of the builds that were deleted.
    buildsDeleted :: Core.Maybe (Core.NonEmpty Types.NonEmptyString),
    -- | An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
    buildsNotDeleted :: Core.Maybe [Types.BuildNotDeleted],
    -- | The status code.
    statusCode :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBuildBatchResponse' value with any optional fields omitted.
mkDeleteBuildBatchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteBuildBatchResponse
mkDeleteBuildBatchResponse responseStatus =
  DeleteBuildBatchResponse'
    { buildsDeleted = Core.Nothing,
      buildsNotDeleted = Core.Nothing,
      statusCode = Core.Nothing,
      responseStatus
    }

-- | An array of strings that contain the identifiers of the builds that were deleted.
--
-- /Note:/ Consider using 'buildsDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrrsBuildsDeleted :: Lens.Lens' DeleteBuildBatchResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
dbbrrsBuildsDeleted = Lens.field @"buildsDeleted"
{-# DEPRECATED dbbrrsBuildsDeleted "Use generic-lens or generic-optics with 'buildsDeleted' instead." #-}

-- | An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
--
-- /Note:/ Consider using 'buildsNotDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrrsBuildsNotDeleted :: Lens.Lens' DeleteBuildBatchResponse (Core.Maybe [Types.BuildNotDeleted])
dbbrrsBuildsNotDeleted = Lens.field @"buildsNotDeleted"
{-# DEPRECATED dbbrrsBuildsNotDeleted "Use generic-lens or generic-optics with 'buildsNotDeleted' instead." #-}

-- | The status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrrsStatusCode :: Lens.Lens' DeleteBuildBatchResponse (Core.Maybe Types.String)
dbbrrsStatusCode = Lens.field @"statusCode"
{-# DEPRECATED dbbrrsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrrsResponseStatus :: Lens.Lens' DeleteBuildBatchResponse Core.Int
dbbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
