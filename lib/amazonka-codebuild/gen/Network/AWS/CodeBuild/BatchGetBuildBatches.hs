{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetBuildBatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about one or more batch builds.
module Network.AWS.CodeBuild.BatchGetBuildBatches
  ( -- * Creating a request
    BatchGetBuildBatches (..),
    mkBatchGetBuildBatches,

    -- ** Request lenses
    bgbbIds,

    -- * Destructuring the response
    BatchGetBuildBatchesResponse (..),
    mkBatchGetBuildBatchesResponse,

    -- ** Response lenses
    bgbbrrsBuildBatches,
    bgbbrrsBuildBatchesNotFound,
    bgbbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetBuildBatches' smart constructor.
newtype BatchGetBuildBatches = BatchGetBuildBatches'
  { -- | An array that contains the batch build identifiers to retrieve.
    ids :: [Types.NonEmptyString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetBuildBatches' value with any optional fields omitted.
mkBatchGetBuildBatches ::
  BatchGetBuildBatches
mkBatchGetBuildBatches = BatchGetBuildBatches' {ids = Core.mempty}

-- | An array that contains the batch build identifiers to retrieve.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbIds :: Lens.Lens' BatchGetBuildBatches [Types.NonEmptyString]
bgbbIds = Lens.field @"ids"
{-# DEPRECATED bgbbIds "Use generic-lens or generic-optics with 'ids' instead." #-}

instance Core.FromJSON BatchGetBuildBatches where
  toJSON BatchGetBuildBatches {..} =
    Core.object (Core.catMaybes [Core.Just ("ids" Core..= ids)])

instance Core.AWSRequest BatchGetBuildBatches where
  type Rs BatchGetBuildBatches = BatchGetBuildBatchesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.BatchGetBuildBatches")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetBuildBatchesResponse'
            Core.<$> (x Core..:? "buildBatches")
            Core.<*> (x Core..:? "buildBatchesNotFound")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchGetBuildBatchesResponse' smart constructor.
data BatchGetBuildBatchesResponse = BatchGetBuildBatchesResponse'
  { -- | An array of @BuildBatch@ objects that represent the retrieved batch builds.
    buildBatches :: Core.Maybe [Types.BuildBatch],
    -- | An array that contains the identifiers of any batch builds that are not found.
    buildBatchesNotFound :: Core.Maybe [Types.NonEmptyString],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetBuildBatchesResponse' value with any optional fields omitted.
mkBatchGetBuildBatchesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetBuildBatchesResponse
mkBatchGetBuildBatchesResponse responseStatus =
  BatchGetBuildBatchesResponse'
    { buildBatches = Core.Nothing,
      buildBatchesNotFound = Core.Nothing,
      responseStatus
    }

-- | An array of @BuildBatch@ objects that represent the retrieved batch builds.
--
-- /Note:/ Consider using 'buildBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrrsBuildBatches :: Lens.Lens' BatchGetBuildBatchesResponse (Core.Maybe [Types.BuildBatch])
bgbbrrsBuildBatches = Lens.field @"buildBatches"
{-# DEPRECATED bgbbrrsBuildBatches "Use generic-lens or generic-optics with 'buildBatches' instead." #-}

-- | An array that contains the identifiers of any batch builds that are not found.
--
-- /Note:/ Consider using 'buildBatchesNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrrsBuildBatchesNotFound :: Lens.Lens' BatchGetBuildBatchesResponse (Core.Maybe [Types.NonEmptyString])
bgbbrrsBuildBatchesNotFound = Lens.field @"buildBatchesNotFound"
{-# DEPRECATED bgbbrrsBuildBatchesNotFound "Use generic-lens or generic-optics with 'buildBatchesNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrrsResponseStatus :: Lens.Lens' BatchGetBuildBatchesResponse Core.Int
bgbbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgbbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
