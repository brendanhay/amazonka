{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeletePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
module Network.AWS.CodePipeline.DeletePipeline
    (
    -- * Creating a request
      DeletePipeline (..)
    , mkDeletePipeline
    -- ** Request lenses
    , dpName

    -- * Destructuring the response
    , DeletePipelineResponse (..)
    , mkDeletePipelineResponse
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeletePipeline@ action.
--
-- /See:/ 'mkDeletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { name :: Types.PipelineName
    -- ^ The name of the pipeline to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePipeline' value with any optional fields omitted.
mkDeletePipeline
    :: Types.PipelineName -- ^ 'name'
    -> DeletePipeline
mkDeletePipeline name = DeletePipeline'{name}

-- | The name of the pipeline to be deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DeletePipeline Types.PipelineName
dpName = Lens.field @"name"
{-# INLINEABLE dpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeletePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePipeline where
        toHeaders DeletePipeline{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.DeletePipeline")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeletePipeline where
        toJSON DeletePipeline{..}
          = Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.AWSRequest DeletePipeline where
        type Rs DeletePipeline = DeletePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
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
