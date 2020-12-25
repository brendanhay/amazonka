{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.BatchWrite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs all the write operations in a batch. Either all the operations succeed or none.
module Network.AWS.CloudDirectory.BatchWrite
  ( -- * Creating a request
    BatchWrite (..),
    mkBatchWrite,

    -- ** Request lenses
    bwDirectoryArn,
    bwOperations,

    -- * Destructuring the response
    BatchWriteResponse (..),
    mkBatchWriteResponse,

    -- ** Response lenses
    bwrrsResponses,
    bwrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchWrite' smart constructor.
data BatchWrite = BatchWrite'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | A list of operations that are part of the batch.
    operations :: [Types.BatchWriteOperation]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchWrite' value with any optional fields omitted.
mkBatchWrite ::
  -- | 'directoryArn'
  Types.Arn ->
  BatchWrite
mkBatchWrite directoryArn =
  BatchWrite' {directoryArn, operations = Core.mempty}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwDirectoryArn :: Lens.Lens' BatchWrite Types.Arn
bwDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED bwDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | A list of operations that are part of the batch.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwOperations :: Lens.Lens' BatchWrite [Types.BatchWriteOperation]
bwOperations = Lens.field @"operations"
{-# DEPRECATED bwOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

instance Core.FromJSON BatchWrite where
  toJSON BatchWrite {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Operations" Core..= operations)])

instance Core.AWSRequest BatchWrite where
  type Rs BatchWrite = BatchWriteResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/batchwrite",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchWriteResponse'
            Core.<$> (x Core..:? "Responses") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchWriteResponse' smart constructor.
data BatchWriteResponse = BatchWriteResponse'
  { -- | A list of all the responses for each batch write.
    responses :: Core.Maybe [Types.BatchWriteOperationResponse],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchWriteResponse' value with any optional fields omitted.
mkBatchWriteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchWriteResponse
mkBatchWriteResponse responseStatus =
  BatchWriteResponse' {responses = Core.Nothing, responseStatus}

-- | A list of all the responses for each batch write.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwrrsResponses :: Lens.Lens' BatchWriteResponse (Core.Maybe [Types.BatchWriteOperationResponse])
bwrrsResponses = Lens.field @"responses"
{-# DEPRECATED bwrrsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwrrsResponseStatus :: Lens.Lens' BatchWriteResponse Core.Int
bwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
