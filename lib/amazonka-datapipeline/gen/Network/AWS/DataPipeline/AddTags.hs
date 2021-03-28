{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or modifies tags for the specified pipeline.
module Network.AWS.DataPipeline.AddTags
    (
    -- * Creating a request
      AddTags (..)
    , mkAddTags
    -- ** Request lenses
    , atPipelineId
    , atTags

    -- * Destructuring the response
    , AddTagsResponse (..)
    , mkAddTagsResponse
    -- ** Response lenses
    , atrrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AddTags.
--
-- /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { pipelineId :: Types.Id
    -- ^ The ID of the pipeline.
  , tags :: [Types.Tag]
    -- ^ The tags to add, as key/value pairs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTags' value with any optional fields omitted.
mkAddTags
    :: Types.Id -- ^ 'pipelineId'
    -> AddTags
mkAddTags pipelineId = AddTags'{pipelineId, tags = Core.mempty}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atPipelineId :: Lens.Lens' AddTags Types.Id
atPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE atPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | The tags to add, as key/value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Types.Tag]
atTags = Lens.field @"tags"
{-# INLINEABLE atTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery AddTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddTags where
        toHeaders AddTags{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.AddTags") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddTags where
        toJSON AddTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineId" Core..= pipelineId),
                  Core.Just ("tags" Core..= tags)])

instance Core.AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddTagsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of AddTags.
--
-- /See:/ 'mkAddTagsResponse' smart constructor.
newtype AddTagsResponse = AddTagsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsResponse' value with any optional fields omitted.
mkAddTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddTagsResponse
mkAddTagsResponse responseStatus = AddTagsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResponseStatus :: Lens.Lens' AddTagsResponse Core.Int
atrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
