{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an object, up to a limit of 10. Each tag consists of a key and an optional value. If you add a tag using a key that is already associated with the ML object, @AddTags@ updates the tag's value.
module Network.AWS.MachineLearning.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atTags,
    atResourceId,
    atResourceType,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,

    -- ** Response lenses
    atrrsResourceId,
    atrrsResourceType,
    atrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The key-value pairs to use to create tags. If you specify a key without specifying a value, Amazon ML creates a tag with the specified key and a value of null.
    tags :: [Types.Tag],
    -- | The ID of the ML object to tag. For example, @exampleModelId@ .
    resourceId :: Types.EntityId,
    -- | The type of the ML object to tag.
    resourceType :: Types.TaggableResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTags' value with any optional fields omitted.
mkAddTags ::
  -- | 'resourceId'
  Types.EntityId ->
  -- | 'resourceType'
  Types.TaggableResourceType ->
  AddTags
mkAddTags resourceId resourceType =
  AddTags' {tags = Core.mempty, resourceId, resourceType}

-- | The key-value pairs to use to create tags. If you specify a key without specifying a value, Amazon ML creates a tag with the specified key and a value of null.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Types.Tag]
atTags = Lens.field @"tags"
{-# DEPRECATED atTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the ML object to tag. For example, @exampleModelId@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceId :: Lens.Lens' AddTags Types.EntityId
atResourceId = Lens.field @"resourceId"
{-# DEPRECATED atResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the ML object to tag.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceType :: Lens.Lens' AddTags Types.TaggableResourceType
atResourceType = Lens.field @"resourceType"
{-# DEPRECATED atResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON AddTags where
  toJSON AddTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Tags" Core..= tags),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance Core.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.AddTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AddTagsResponse'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "ResourceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Amazon ML returns the following elements.
--
-- /See:/ 'mkAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | The ID of the ML object that was tagged.
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The type of the ML object that was tagged.
    resourceType :: Core.Maybe Types.TaggableResourceType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsResponse' value with any optional fields omitted.
mkAddTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddTagsResponse
mkAddTagsResponse responseStatus =
  AddTagsResponse'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      responseStatus
    }

-- | The ID of the ML object that was tagged.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResourceId :: Lens.Lens' AddTagsResponse (Core.Maybe Types.ResourceId)
atrrsResourceId = Lens.field @"resourceId"
{-# DEPRECATED atrrsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the ML object that was tagged.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResourceType :: Lens.Lens' AddTagsResponse (Core.Maybe Types.TaggableResourceType)
atrrsResourceType = Lens.field @"resourceType"
{-# DEPRECATED atrrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResponseStatus :: Lens.Lens' AddTagsResponse Core.Int
atrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
