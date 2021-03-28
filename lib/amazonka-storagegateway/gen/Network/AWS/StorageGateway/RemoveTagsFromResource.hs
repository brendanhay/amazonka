{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified resource. This operation is supported in storage gateways of all types.
module Network.AWS.StorageGateway.RemoveTagsFromResource
    (
    -- * Creating a request
      RemoveTagsFromResource (..)
    , mkRemoveTagsFromResource
    -- ** Request lenses
    , rtfrResourceARN
    , rtfrTagKeys

    -- * Destructuring the response
    , RemoveTagsFromResourceResponse (..)
    , mkRemoveTagsFromResourceResponse
    -- ** Response lenses
    , rtfrrrsResourceARN
    , rtfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | RemoveTagsFromResourceInput
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceARN :: Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the resource you want to remove the tags from.
  , tagKeys :: [Types.TagKey]
    -- ^ The keys of the tags you want to remove from the specified resource. A tag is composed of a key-value pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> RemoveTagsFromResource
mkRemoveTagsFromResource resourceARN
  = RemoveTagsFromResource'{resourceARN, tagKeys = Core.mempty}

-- | The Amazon Resource Name (ARN) of the resource you want to remove the tags from.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceARN :: Lens.Lens' RemoveTagsFromResource Types.ResourceARN
rtfrResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE rtfrResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | The keys of the tags you want to remove from the specified resource. A tag is composed of a key-value pair.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Types.TagKey]
rtfrTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE rtfrTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery RemoveTagsFromResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveTagsFromResource where
        toHeaders RemoveTagsFromResource{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.RemoveTagsFromResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveTagsFromResource where
        toJSON RemoveTagsFromResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  Core.Just ("TagKeys" Core..= tagKeys)])

instance Core.AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RemoveTagsFromResourceResponse' Core.<$>
                   (x Core..:? "ResourceARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | RemoveTagsFromResourceOutput
--
-- /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the resource that the tags were removed from.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResourceResponse' value with any optional fields omitted.
mkRemoveTagsFromResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse responseStatus
  = RemoveTagsFromResourceResponse'{resourceARN = Core.Nothing,
                                    responseStatus}

-- | The Amazon Resource Name (ARN) of the resource that the tags were removed from.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsResourceARN :: Lens.Lens' RemoveTagsFromResourceResponse (Core.Maybe Types.ResourceARN)
rtfrrrsResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE rtfrrrsResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Core.Int
rtfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
