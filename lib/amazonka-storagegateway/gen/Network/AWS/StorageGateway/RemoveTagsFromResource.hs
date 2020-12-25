{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceARN,
    rtfrTagKeys,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,

    -- ** Response lenses
    rtfrrrsResourceARN,
    rtfrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | RemoveTagsFromResourceInput
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The Amazon Resource Name (ARN) of the resource you want to remove the tags from.
    resourceARN :: Types.ResourceARN,
    -- | The keys of the tags you want to remove from the specified resource. A tag is composed of a key-value pair.
    tagKeys :: [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource ::
  -- | 'resourceARN'
  Types.ResourceARN ->
  RemoveTagsFromResource
mkRemoveTagsFromResource resourceARN =
  RemoveTagsFromResource' {resourceARN, tagKeys = Core.mempty}

-- | The Amazon Resource Name (ARN) of the resource you want to remove the tags from.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceARN :: Lens.Lens' RemoveTagsFromResource Types.ResourceARN
rtfrResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED rtfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The keys of the tags you want to remove from the specified resource. A tag is composed of a key-value pair.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Types.TagKey]
rtfrTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED rtfrTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARN" Core..= resourceARN),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.RemoveTagsFromResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Core.<$> (x Core..:? "ResourceARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | RemoveTagsFromResourceOutput
--
-- /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The Amazon Resource Name (ARN) of the resource that the tags were removed from.
    resourceARN :: Core.Maybe Types.ResourceARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResourceResponse' value with any optional fields omitted.
mkRemoveTagsFromResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse responseStatus =
  RemoveTagsFromResourceResponse'
    { resourceARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the resource that the tags were removed from.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsResourceARN :: Lens.Lens' RemoveTagsFromResourceResponse (Core.Maybe Types.ResourceARN)
rtfrrrsResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED rtfrrrsResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Core.Int
rtfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
