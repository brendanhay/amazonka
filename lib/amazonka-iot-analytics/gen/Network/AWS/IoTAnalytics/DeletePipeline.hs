{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeletePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
module Network.AWS.IoTAnalytics.DeletePipeline
    (
    -- * Creating a request
      DeletePipeline (..)
    , mkDeletePipeline
    -- ** Request lenses
    , dPipelineName

    -- * Destructuring the response
    , DeletePipelineResponse (..)
    , mkDeletePipelineResponse
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { pipelineName :: Types.PipelineName
    -- ^ The name of the pipeline to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePipeline' value with any optional fields omitted.
mkDeletePipeline
    :: Types.PipelineName -- ^ 'pipelineName'
    -> DeletePipeline
mkDeletePipeline pipelineName = DeletePipeline'{pipelineName}

-- | The name of the pipeline to delete.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPipelineName :: Lens.Lens' DeletePipeline Types.PipelineName
dPipelineName = Lens.field @"pipelineName"
{-# INLINEABLE dPipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

instance Core.ToQuery DeletePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePipeline where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeletePipeline where
        type Rs DeletePipeline = DeletePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/pipelines/" Core.<> Core.toText pipelineName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeletePipelineResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePipelineResponse' value with any optional fields omitted.
mkDeletePipelineResponse
    :: DeletePipelineResponse
mkDeletePipelineResponse = DeletePipelineResponse'
