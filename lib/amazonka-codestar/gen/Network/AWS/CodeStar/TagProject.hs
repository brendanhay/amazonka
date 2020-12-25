{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.TagProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a project.
module Network.AWS.CodeStar.TagProject
  ( -- * Creating a request
    TagProject (..),
    mkTagProject,

    -- ** Request lenses
    tpId,
    tpTags,

    -- * Destructuring the response
    TagProjectResponse (..),
    mkTagProjectResponse,

    -- ** Response lenses
    tprrsTags,
    tprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagProject' smart constructor.
data TagProject = TagProject'
  { -- | The ID of the project you want to add a tag to.
    id :: Types.ProjectId,
    -- | The tags you want to add to the project.
    tags :: Core.HashMap Types.TagKey Types.TagValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagProject' value with any optional fields omitted.
mkTagProject ::
  -- | 'id'
  Types.ProjectId ->
  TagProject
mkTagProject id = TagProject' {id, tags = Core.mempty}

-- | The ID of the project you want to add a tag to.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpId :: Lens.Lens' TagProject Types.ProjectId
tpId = Lens.field @"id"
{-# DEPRECATED tpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The tags you want to add to the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpTags :: Lens.Lens' TagProject (Core.HashMap Types.TagKey Types.TagValue)
tpTags = Lens.field @"tags"
{-# DEPRECATED tpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON TagProject where
  toJSON TagProject {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("id" Core..= id), Core.Just ("tags" Core..= tags)]
      )

instance Core.AWSRequest TagProject where
  type Rs TagProject = TagProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.TagProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TagProjectResponse'
            Core.<$> (x Core..:? "tags") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTagProjectResponse' smart constructor.
data TagProjectResponse = TagProjectResponse'
  { -- | The tags for the project.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagProjectResponse' value with any optional fields omitted.
mkTagProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TagProjectResponse
mkTagProjectResponse responseStatus =
  TagProjectResponse' {tags = Core.Nothing, responseStatus}

-- | The tags for the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tprrsTags :: Lens.Lens' TagProjectResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
tprrsTags = Lens.field @"tags"
{-# DEPRECATED tprrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tprrsResponseStatus :: Lens.Lens' TagProjectResponse Core.Int
tprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
