{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DeleteComputeEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Batch compute environment.
--
-- Before you can delete a compute environment, you must set its state to @DISABLED@ with the 'UpdateComputeEnvironment' API operation and disassociate it from any job queues with the 'UpdateJobQueue' API operation.
module Network.AWS.Batch.DeleteComputeEnvironment
    (
    -- * Creating a request
      DeleteComputeEnvironment (..)
    , mkDeleteComputeEnvironment
    -- ** Request lenses
    , dceComputeEnvironment

    -- * Destructuring the response
    , DeleteComputeEnvironmentResponse (..)
    , mkDeleteComputeEnvironmentResponse
    -- ** Response lenses
    , dcerrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteComputeEnvironment' smart constructor.
newtype DeleteComputeEnvironment = DeleteComputeEnvironment'
  { computeEnvironment :: Core.Text
    -- ^ The name or Amazon Resource Name (ARN) of the compute environment to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteComputeEnvironment' value with any optional fields omitted.
mkDeleteComputeEnvironment
    :: Core.Text -- ^ 'computeEnvironment'
    -> DeleteComputeEnvironment
mkDeleteComputeEnvironment computeEnvironment
  = DeleteComputeEnvironment'{computeEnvironment}

-- | The name or Amazon Resource Name (ARN) of the compute environment to delete.
--
-- /Note:/ Consider using 'computeEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceComputeEnvironment :: Lens.Lens' DeleteComputeEnvironment Core.Text
dceComputeEnvironment = Lens.field @"computeEnvironment"
{-# INLINEABLE dceComputeEnvironment #-}
{-# DEPRECATED computeEnvironment "Use generic-lens or generic-optics with 'computeEnvironment' instead"  #-}

instance Core.ToQuery DeleteComputeEnvironment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteComputeEnvironment where
        toHeaders DeleteComputeEnvironment{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteComputeEnvironment where
        toJSON DeleteComputeEnvironment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("computeEnvironment" Core..= computeEnvironment)])

instance Core.AWSRequest DeleteComputeEnvironment where
        type Rs DeleteComputeEnvironment = DeleteComputeEnvironmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/v1/deletecomputeenvironment",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteComputeEnvironmentResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteComputeEnvironmentResponse' smart constructor.
newtype DeleteComputeEnvironmentResponse = DeleteComputeEnvironmentResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteComputeEnvironmentResponse' value with any optional fields omitted.
mkDeleteComputeEnvironmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteComputeEnvironmentResponse
mkDeleteComputeEnvironmentResponse responseStatus
  = DeleteComputeEnvironmentResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcerrsResponseStatus :: Lens.Lens' DeleteComputeEnvironmentResponse Core.Int
dcerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
