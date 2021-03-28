{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      BatchWrite (..)
    , mkBatchWrite
    -- ** Request lenses
    , bwDirectoryArn
    , bwOperations

    -- * Destructuring the response
    , BatchWriteResponse (..)
    , mkBatchWriteResponse
    -- ** Response lenses
    , bwrrsResponses
    , bwrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchWrite' smart constructor.
data BatchWrite = BatchWrite'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
  , operations :: [Types.BatchWriteOperation]
    -- ^ A list of operations that are part of the batch.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchWrite' value with any optional fields omitted.
mkBatchWrite
    :: Types.Arn -- ^ 'directoryArn'
    -> BatchWrite
mkBatchWrite directoryArn
  = BatchWrite'{directoryArn, operations = Core.mempty}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwDirectoryArn :: Lens.Lens' BatchWrite Types.Arn
bwDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE bwDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | A list of operations that are part of the batch.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwOperations :: Lens.Lens' BatchWrite [Types.BatchWriteOperation]
bwOperations = Lens.field @"operations"
{-# INLINEABLE bwOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

instance Core.ToQuery BatchWrite where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchWrite where
        toHeaders BatchWrite{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON BatchWrite where
        toJSON BatchWrite{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Operations" Core..= operations)])

instance Core.AWSRequest BatchWrite where
        type Rs BatchWrite = BatchWriteResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/batchwrite",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchWriteResponse' Core.<$>
                   (x Core..:? "Responses") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchWriteResponse' smart constructor.
data BatchWriteResponse = BatchWriteResponse'
  { responses :: Core.Maybe [Types.BatchWriteOperationResponse]
    -- ^ A list of all the responses for each batch write.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchWriteResponse' value with any optional fields omitted.
mkBatchWriteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchWriteResponse
mkBatchWriteResponse responseStatus
  = BatchWriteResponse'{responses = Core.Nothing, responseStatus}

-- | A list of all the responses for each batch write.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwrrsResponses :: Lens.Lens' BatchWriteResponse (Core.Maybe [Types.BatchWriteOperationResponse])
bwrrsResponses = Lens.field @"responses"
{-# INLINEABLE bwrrsResponses #-}
{-# DEPRECATED responses "Use generic-lens or generic-optics with 'responses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwrrsResponseStatus :: Lens.Lens' BatchWriteResponse Core.Int
bwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
