{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.RetryBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a failed batch build. Only batch builds that have failed can be retried.
module Network.AWS.CodeBuild.RetryBuildBatch
  ( -- * Creating a request
    RetryBuildBatch (..),
    mkRetryBuildBatch,

    -- ** Request lenses
    rbbId,
    rbbIdempotencyToken,
    rbbRetryType,

    -- * Destructuring the response
    RetryBuildBatchResponse (..),
    mkRetryBuildBatchResponse,

    -- ** Response lenses
    rbbrrsBuildBatch,
    rbbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRetryBuildBatch' smart constructor.
data RetryBuildBatch = RetryBuildBatch'
  { -- | Specifies the identifier of the batch build to restart.
    id :: Core.Maybe Types.Id,
    -- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuildBatch@ request. The token is included in the @RetryBuildBatch@ request and is valid for five minutes. If you repeat the @RetryBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Core.Maybe Types.IdempotencyToken,
    -- | Specifies the type of retry to perform.
    retryType :: Core.Maybe Types.RetryBuildBatchType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryBuildBatch' value with any optional fields omitted.
mkRetryBuildBatch ::
  RetryBuildBatch
mkRetryBuildBatch =
  RetryBuildBatch'
    { id = Core.Nothing,
      idempotencyToken = Core.Nothing,
      retryType = Core.Nothing
    }

-- | Specifies the identifier of the batch build to restart.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbId :: Lens.Lens' RetryBuildBatch (Core.Maybe Types.Id)
rbbId = Lens.field @"id"
{-# DEPRECATED rbbId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuildBatch@ request. The token is included in the @RetryBuildBatch@ request and is valid for five minutes. If you repeat the @RetryBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbIdempotencyToken :: Lens.Lens' RetryBuildBatch (Core.Maybe Types.IdempotencyToken)
rbbIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED rbbIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | Specifies the type of retry to perform.
--
-- /Note:/ Consider using 'retryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbRetryType :: Lens.Lens' RetryBuildBatch (Core.Maybe Types.RetryBuildBatchType)
rbbRetryType = Lens.field @"retryType"
{-# DEPRECATED rbbRetryType "Use generic-lens or generic-optics with 'retryType' instead." #-}

instance Core.FromJSON RetryBuildBatch where
  toJSON RetryBuildBatch {..} =
    Core.object
      ( Core.catMaybes
          [ ("id" Core..=) Core.<$> id,
            ("idempotencyToken" Core..=) Core.<$> idempotencyToken,
            ("retryType" Core..=) Core.<$> retryType
          ]
      )

instance Core.AWSRequest RetryBuildBatch where
  type Rs RetryBuildBatch = RetryBuildBatchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.RetryBuildBatch")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RetryBuildBatchResponse'
            Core.<$> (x Core..:? "buildBatch") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRetryBuildBatchResponse' smart constructor.
data RetryBuildBatchResponse = RetryBuildBatchResponse'
  { buildBatch :: Core.Maybe Types.BuildBatch,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RetryBuildBatchResponse' value with any optional fields omitted.
mkRetryBuildBatchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RetryBuildBatchResponse
mkRetryBuildBatchResponse responseStatus =
  RetryBuildBatchResponse'
    { buildBatch = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbrrsBuildBatch :: Lens.Lens' RetryBuildBatchResponse (Core.Maybe Types.BuildBatch)
rbbrrsBuildBatch = Lens.field @"buildBatch"
{-# DEPRECATED rbbrrsBuildBatch "Use generic-lens or generic-optics with 'buildBatch' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbrrsResponseStatus :: Lens.Lens' RetryBuildBatchResponse Core.Int
rbbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rbbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
