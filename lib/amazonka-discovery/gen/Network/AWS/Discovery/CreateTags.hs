{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more tags for configuration items. Tags are metadata that help you categorize IT assets. This API accepts a list of multiple configuration items.
module Network.AWS.Discovery.CreateTags
  ( -- * Creating a request
    CreateTags (..),
    mkCreateTags,

    -- ** Request lenses
    ctConfigurationIds,
    ctTags,

    -- * Destructuring the response
    CreateTagsResponse (..),
    mkCreateTagsResponse,

    -- ** Response lenses
    ctrrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | A list of configuration items that you want to tag.
    configurationIds :: [Types.ConfigurationId],
    -- | Tags that you want to associate with one or more configuration items. Specify the tags that you want to create in a /key/ -/value/ format. For example:
    --
    -- @{"key": "serverType", "value": "webServer"}@
    tags :: [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTags' value with any optional fields omitted.
mkCreateTags ::
  CreateTags
mkCreateTags =
  CreateTags' {configurationIds = Core.mempty, tags = Core.mempty}

-- | A list of configuration items that you want to tag.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConfigurationIds :: Lens.Lens' CreateTags [Types.ConfigurationId]
ctConfigurationIds = Lens.field @"configurationIds"
{-# DEPRECATED ctConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

-- | Tags that you want to associate with one or more configuration items. Specify the tags that you want to create in a /key/ -/value/ format. For example:
--
-- @{"key": "serverType", "value": "webServer"}@
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags [Types.Tag]
ctTags = Lens.field @"tags"
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateTags where
  toJSON CreateTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("configurationIds" Core..= configurationIds),
            Core.Just ("tags" Core..= tags)
          ]
      )

instance Core.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSPoseidonService_V2015_11_01.CreateTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateTagsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
newtype CreateTagsResponse = CreateTagsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagsResponse' value with any optional fields omitted.
mkCreateTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTagsResponse
mkCreateTagsResponse responseStatus =
  CreateTagsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTagsResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
