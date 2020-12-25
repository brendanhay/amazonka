{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SetTagsForResource (..),
    mkSetTagsForResource,

    -- ** Request lenses
    stfrResourceArn,
    stfrTags,

    -- * Destructuring the response
    SetTagsForResourceResponse (..),
    mkSetTagsForResourceResponse,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetTagsForResource' smart constructor.
data SetTagsForResource = SetTagsForResource'
  { -- | The ARN of the assessment template that you want to set tags to.
    resourceArn :: Types.Arn,
    -- | A collection of key and value pairs that you want to set to the assessment template.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTagsForResource' value with any optional fields omitted.
mkSetTagsForResource ::
  -- | 'resourceArn'
  Types.Arn ->
  SetTagsForResource
mkSetTagsForResource resourceArn =
  SetTagsForResource' {resourceArn, tags = Core.Nothing}

-- | The ARN of the assessment template that you want to set tags to.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfrResourceArn :: Lens.Lens' SetTagsForResource Types.Arn
stfrResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED stfrResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | A collection of key and value pairs that you want to set to the assessment template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfrTags :: Lens.Lens' SetTagsForResource (Core.Maybe [Types.Tag])
stfrTags = Lens.field @"tags"
{-# DEPRECATED stfrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON SetTagsForResource where
  toJSON SetTagsForResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceArn" Core..= resourceArn),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest SetTagsForResource where
  type Rs SetTagsForResource = SetTagsForResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "InspectorService.SetTagsForResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull SetTagsForResourceResponse'

-- | /See:/ 'mkSetTagsForResourceResponse' smart constructor.
data SetTagsForResourceResponse = SetTagsForResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTagsForResourceResponse' value with any optional fields omitted.
mkSetTagsForResourceResponse ::
  SetTagsForResourceResponse
mkSetTagsForResourceResponse = SetTagsForResourceResponse'
