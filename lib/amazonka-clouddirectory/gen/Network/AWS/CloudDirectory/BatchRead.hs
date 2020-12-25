{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.BatchRead
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs all the read operations in a batch.
module Network.AWS.CloudDirectory.BatchRead
  ( -- * Creating a request
    BatchRead (..),
    mkBatchRead,

    -- ** Request lenses
    brDirectoryArn,
    brOperations,
    brConsistencyLevel,

    -- * Destructuring the response
    BatchReadResponse (..),
    mkBatchReadResponse,

    -- ** Response lenses
    brrrsResponses,
    brrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchRead' smart constructor.
data BatchRead = BatchRead'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | A list of operations that are part of the batch.
    operations :: [Types.BatchReadOperation],
    -- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchRead' value with any optional fields omitted.
mkBatchRead ::
  -- | 'directoryArn'
  Types.Arn ->
  BatchRead
mkBatchRead directoryArn =
  BatchRead'
    { directoryArn,
      operations = Core.mempty,
      consistencyLevel = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDirectoryArn :: Lens.Lens' BatchRead Types.Arn
brDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED brDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | A list of operations that are part of the batch.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brOperations :: Lens.Lens' BatchRead [Types.BatchReadOperation]
brOperations = Lens.field @"operations"
{-# DEPRECATED brOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brConsistencyLevel :: Lens.Lens' BatchRead (Core.Maybe Types.ConsistencyLevel)
brConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED brConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

instance Core.FromJSON BatchRead where
  toJSON BatchRead {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Operations" Core..= operations)])

instance Core.AWSRequest BatchRead where
  type Rs BatchRead = BatchReadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/batchread",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn
            Core.<> (Core.toHeaders "x-amz-consistency-level" consistencyLevel),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchReadResponse'
            Core.<$> (x Core..:? "Responses") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchReadResponse' smart constructor.
data BatchReadResponse = BatchReadResponse'
  { -- | A list of all the responses for each batch read.
    responses :: Core.Maybe [Types.BatchReadOperationResponse],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchReadResponse' value with any optional fields omitted.
mkBatchReadResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchReadResponse
mkBatchReadResponse responseStatus =
  BatchReadResponse' {responses = Core.Nothing, responseStatus}

-- | A list of all the responses for each batch read.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brrrsResponses :: Lens.Lens' BatchReadResponse (Core.Maybe [Types.BatchReadOperationResponse])
brrrsResponses = Lens.field @"responses"
{-# DEPRECATED brrrsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brrrsResponseStatus :: Lens.Lens' BatchReadResponse Core.Int
brrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED brrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
