{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are associated with a resource group, specified by an ARN.
module Network.AWS.ResourceGroups.GetTags
  ( -- * Creating a request
    GetTags (..),
    mkGetTags,

    -- ** Request lenses
    gtArn,

    -- * Destructuring the response
    GetTagsResponse (..),
    mkGetTagsResponse,

    -- ** Response lenses
    gtrrsArn,
    gtrrsTags,
    gtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTags' smart constructor.
newtype GetTags = GetTags'
  { -- | The ARN of the resource group whose tags you want to retrieve.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTags' value with any optional fields omitted.
mkGetTags ::
  -- | 'arn'
  Types.Arn ->
  GetTags
mkGetTags arn = GetTags' {arn}

-- | The ARN of the resource group whose tags you want to retrieve.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtArn :: Lens.Lens' GetTags Types.Arn
gtArn = Lens.field @"arn"
{-# DEPRECATED gtArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/resources/" Core.<> (Core.toText arn) Core.<> ("/tags")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The ARN of the tagged resource group.
    arn :: Core.Maybe Types.GroupArn,
    -- | The tags associated with the specified resource group.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagsResponse' value with any optional fields omitted.
mkGetTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTagsResponse
mkGetTagsResponse responseStatus =
  GetTagsResponse'
    { arn = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The ARN of the tagged resource group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsArn :: Lens.Lens' GetTagsResponse (Core.Maybe Types.GroupArn)
gtrrsArn = Lens.field @"arn"
{-# DEPRECATED gtrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags associated with the specified resource group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTags :: Lens.Lens' GetTagsResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gtrrsTags = Lens.field @"tags"
{-# DEPRECATED gtrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTagsResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
