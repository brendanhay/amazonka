{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.SetTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets tags (key and value pairs) to the assessment template that is specified by the ARN of the assessment template.
module Network.AWS.Inspector.SetTagsForResource
    (
    -- * Creating a request
      SetTagsForResource (..)
    , mkSetTagsForResource
    -- ** Request lenses
    , stfrResourceArn
    , stfrTags

    -- * Destructuring the response
    , SetTagsForResourceResponse (..)
    , mkSetTagsForResourceResponse
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetTagsForResource' smart constructor.
data SetTagsForResource = SetTagsForResource'
  { resourceArn :: Types.Arn
    -- ^ The ARN of the assessment template that you want to set tags to.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A collection of key and value pairs that you want to set to the assessment template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTagsForResource' value with any optional fields omitted.
mkSetTagsForResource
    :: Types.Arn -- ^ 'resourceArn'
    -> SetTagsForResource
mkSetTagsForResource resourceArn
  = SetTagsForResource'{resourceArn, tags = Core.Nothing}

-- | The ARN of the assessment template that you want to set tags to.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfrResourceArn :: Lens.Lens' SetTagsForResource Types.Arn
stfrResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE stfrResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | A collection of key and value pairs that you want to set to the assessment template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfrTags :: Lens.Lens' SetTagsForResource (Core.Maybe [Types.Tag])
stfrTags = Lens.field @"tags"
{-# INLINEABLE stfrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery SetTagsForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetTagsForResource where
        toHeaders SetTagsForResource{..}
          = Core.pure ("X-Amz-Target", "InspectorService.SetTagsForResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetTagsForResource where
        toJSON SetTagsForResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("resourceArn" Core..= resourceArn),
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest SetTagsForResource where
        type Rs SetTagsForResource = SetTagsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SetTagsForResourceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetTagsForResourceResponse' smart constructor.
data SetTagsForResourceResponse = SetTagsForResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTagsForResourceResponse' value with any optional fields omitted.
mkSetTagsForResourceResponse
    :: SetTagsForResourceResponse
mkSetTagsForResourceResponse = SetTagsForResourceResponse'
