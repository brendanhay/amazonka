{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between configuration items and one or more tags. This API accepts a list of multiple configuration items.
module Network.AWS.Discovery.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dtConfigurationIds,
    dtTags,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | A list of configuration items with tags that you want to delete.
    configurationIds :: [Types.ConfigurationId],
    -- | Tags that you want to delete from one or more configuration items. Specify the tags that you want to delete in a /key/ -/value/ format. For example:
    --
    -- @{"key": "serverType", "value": "webServer"}@
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTags' value with any optional fields omitted.
mkDeleteTags ::
  DeleteTags
mkDeleteTags =
  DeleteTags' {configurationIds = Core.mempty, tags = Core.Nothing}

-- | A list of configuration items with tags that you want to delete.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtConfigurationIds :: Lens.Lens' DeleteTags [Types.ConfigurationId]
dtConfigurationIds = Lens.field @"configurationIds"
{-# DEPRECATED dtConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

-- | Tags that you want to delete from one or more configuration items. Specify the tags that you want to delete in a /key/ -/value/ format. For example:
--
-- @{"key": "serverType", "value": "webServer"}@
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTags :: Lens.Lens' DeleteTags (Core.Maybe [Types.Tag])
dtTags = Lens.field @"tags"
{-# DEPRECATED dtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON DeleteTags where
  toJSON DeleteTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("configurationIds" Core..= configurationIds),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSPoseidonService_V2015_11_01.DeleteTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
newtype DeleteTagsResponse = DeleteTagsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagsResponse' value with any optional fields omitted.
mkDeleteTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTagsResponse
mkDeleteTagsResponse responseStatus =
  DeleteTagsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteTagsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
