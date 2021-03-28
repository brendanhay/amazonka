{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      BatchRead (..)
    , mkBatchRead
    -- ** Request lenses
    , brDirectoryArn
    , brOperations
    , brConsistencyLevel

    -- * Destructuring the response
    , BatchReadResponse (..)
    , mkBatchReadResponse
    -- ** Response lenses
    , brrrsResponses
    , brrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchRead' smart constructor.
data BatchRead = BatchRead'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
  , operations :: [Types.BatchReadOperation]
    -- ^ A list of operations that are part of the batch.
  , consistencyLevel :: Core.Maybe Types.ConsistencyLevel
    -- ^ Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchRead' value with any optional fields omitted.
mkBatchRead
    :: Types.Arn -- ^ 'directoryArn'
    -> BatchRead
mkBatchRead directoryArn
  = BatchRead'{directoryArn, operations = Core.mempty,
               consistencyLevel = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDirectoryArn :: Lens.Lens' BatchRead Types.Arn
brDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE brDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | A list of operations that are part of the batch.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brOperations :: Lens.Lens' BatchRead [Types.BatchReadOperation]
brOperations = Lens.field @"operations"
{-# INLINEABLE brOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brConsistencyLevel :: Lens.Lens' BatchRead (Core.Maybe Types.ConsistencyLevel)
brConsistencyLevel = Lens.field @"consistencyLevel"
{-# INLINEABLE brConsistencyLevel #-}
{-# DEPRECATED consistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead"  #-}

instance Core.ToQuery BatchRead where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchRead where
        toHeaders BatchRead{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn Core.<>
              Core.toHeaders "x-amz-consistency-level" consistencyLevel

instance Core.FromJSON BatchRead where
        toJSON BatchRead{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Operations" Core..= operations)])

instance Core.AWSRequest BatchRead where
        type Rs BatchRead = BatchReadResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/batchread",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchReadResponse' Core.<$>
                   (x Core..:? "Responses") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchReadResponse' smart constructor.
data BatchReadResponse = BatchReadResponse'
  { responses :: Core.Maybe [Types.BatchReadOperationResponse]
    -- ^ A list of all the responses for each batch read.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchReadResponse' value with any optional fields omitted.
mkBatchReadResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchReadResponse
mkBatchReadResponse responseStatus
  = BatchReadResponse'{responses = Core.Nothing, responseStatus}

-- | A list of all the responses for each batch read.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brrrsResponses :: Lens.Lens' BatchReadResponse (Core.Maybe [Types.BatchReadOperationResponse])
brrrsResponses = Lens.field @"responses"
{-# INLINEABLE brrrsResponses #-}
{-# DEPRECATED responses "Use generic-lens or generic-optics with 'responses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brrrsResponseStatus :: Lens.Lens' BatchReadResponse Core.Int
brrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE brrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
