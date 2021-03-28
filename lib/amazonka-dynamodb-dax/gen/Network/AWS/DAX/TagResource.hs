{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of tags with a DAX resource. You can call @TagResource@ up to 5 times per second, per account. 
module Network.AWS.DAX.TagResource
    (
    -- * Creating a request
      TagResource (..)
    , mkTagResource
    -- ** Request lenses
    , trResourceName
    , trTags

    -- * Destructuring the response
    , TagResourceResponse (..)
    , mkTagResourceResponse
    -- ** Response lenses
    , trrrsTags
    , trrrsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { resourceName :: Core.Text
    -- ^ The name of the DAX resource to which tags should be added.
  , tags :: [Types.Tag]
    -- ^ The tags to be assigned to the DAX resource. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResource' value with any optional fields omitted.
mkTagResource
    :: Core.Text -- ^ 'resourceName'
    -> TagResource
mkTagResource resourceName
  = TagResource'{resourceName, tags = Core.mempty}

-- | The name of the DAX resource to which tags should be added.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceName :: Lens.Lens' TagResource Core.Text
trResourceName = Lens.field @"resourceName"
{-# INLINEABLE trResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | The tags to be assigned to the DAX resource. 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource [Types.Tag]
trTags = Lens.field @"tags"
{-# INLINEABLE trTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery TagResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TagResource where
        toHeaders TagResource{..}
          = Core.pure ("X-Amz-Target", "AmazonDAXV3.TagResource") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TagResource where
        toJSON TagResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceName" Core..= resourceName),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TagResourceResponse' Core.<$>
                   (x Core..:? "Tags") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  { tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags that are associated with the DAX resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResourceResponse' value with any optional fields omitted.
mkTagResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TagResourceResponse
mkTagResourceResponse responseStatus
  = TagResourceResponse'{tags = Core.Nothing, responseStatus}

-- | The list of tags that are associated with the DAX resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrrsTags :: Lens.Lens' TagResourceResponse (Core.Maybe [Types.Tag])
trrrsTags = Lens.field @"tags"
{-# INLINEABLE trrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrrsResponseStatus :: Lens.Lens' TagResourceResponse Core.Int
trrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE trrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
