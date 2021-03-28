{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.DeletePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeletePipeline operation removes a pipeline.
--
-- You can only delete a pipeline that has never been used or that is not currently in use (doesn't contain any active jobs). If the pipeline is currently in use, @DeletePipeline@ returns an error. 
module Network.AWS.ElasticTranscoder.DeletePipeline
    (
    -- * Creating a request
      DeletePipeline (..)
    , mkDeletePipeline
    -- ** Request lenses
    , dId

    -- * Destructuring the response
    , DeletePipelineResponse (..)
    , mkDeletePipelineResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @DeletePipelineRequest@ structure.
--
-- /See:/ 'mkDeletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { id :: Types.Id
    -- ^ The identifier of the pipeline that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePipeline' value with any optional fields omitted.
mkDeletePipeline
    :: Types.Id -- ^ 'id'
    -> DeletePipeline
mkDeletePipeline id = DeletePipeline'{id}

-- | The identifier of the pipeline that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DeletePipeline Types.Id
dId = Lens.field @"id"
{-# INLINEABLE dId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeletePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePipeline where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeletePipeline where
        type Rs DeletePipeline = DeletePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/2012-09-25/pipelines/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeletePipelineResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The @DeletePipelineResponse@ structure.
--
-- /See:/ 'mkDeletePipelineResponse' smart constructor.
newtype DeletePipelineResponse = DeletePipelineResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePipelineResponse' value with any optional fields omitted.
mkDeletePipelineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeletePipelineResponse
mkDeletePipelineResponse responseStatus
  = DeletePipelineResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeletePipelineResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
