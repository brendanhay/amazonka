{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified resource. You use tags to add metadata to resources, which you can use to categorize these resources. For example, you can categorize resources by purpose, owner, environment, or team. Each tag consists of a key and a value, which you define. You can add tags to the following AWS Storage Gateway resources:
--
--
--     * Storage gateways of all types
--
--
--     * Storage volumes
--
--
--     * Virtual tapes
--
--
--     * NFS and SMB file shares
--
--
-- You can create a maximum of 50 tags for each resource. Virtual tapes and storage volumes that are recovered to a new gateway maintain their tags.
module Network.AWS.StorageGateway.AddTagsToResource
    (
    -- * Creating a request
      AddTagsToResource (..)
    , mkAddTagsToResource
    -- ** Request lenses
    , attrResourceARN
    , attrTags

    -- * Destructuring the response
    , AddTagsToResourceResponse (..)
    , mkAddTagsToResourceResponse
    -- ** Response lenses
    , attrrrsResourceARN
    , attrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | AddTagsToResourceInput
--
-- /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceARN :: Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the resource you want to add tags to.
  , tags :: [Types.Tag]
    -- ^ The key-value pair that represents the tag you want to add to the resource. The value can be an empty string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResource' value with any optional fields omitted.
mkAddTagsToResource
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> AddTagsToResource
mkAddTagsToResource resourceARN
  = AddTagsToResource'{resourceARN, tags = Core.mempty}

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceARN :: Lens.Lens' AddTagsToResource Types.ResourceARN
attrResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE attrResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | The key-value pair that represents the tag you want to add to the resource. The value can be an empty string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Types.Tag]
attrTags = Lens.field @"tags"
{-# INLINEABLE attrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery AddTagsToResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddTagsToResource where
        toHeaders AddTagsToResource{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.AddTagsToResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddTagsToResource where
        toJSON AddTagsToResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddTagsToResourceResponse' Core.<$>
                   (x Core..:? "ResourceARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | AddTagsToResourceOutput
--
-- /See:/ 'mkAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the resource you want to add tags to.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResourceResponse' value with any optional fields omitted.
mkAddTagsToResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddTagsToResourceResponse
mkAddTagsToResourceResponse responseStatus
  = AddTagsToResourceResponse'{resourceARN = Core.Nothing,
                               responseStatus}

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrrsResourceARN :: Lens.Lens' AddTagsToResourceResponse (Core.Maybe Types.ResourceARN)
attrrrsResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE attrrrsResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Core.Int
attrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE attrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
