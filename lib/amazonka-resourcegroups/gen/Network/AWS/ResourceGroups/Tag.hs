{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a resource group with the specified ARN. Existing tags on a resource group are not changed if they are not specified in the request parameters.
--
-- /Important:/ Do not store personally identifiable information (PII) or other confidential or sensitive information in tags. We use tags to provide you with billing and administration services. Tags are not intended to be used for private or sensitive data.
module Network.AWS.ResourceGroups.Tag
  ( -- * Creating a request
    Tag (..),
    mkTag,

    -- ** Request lenses
    tArn,
    tTags,

    -- * Destructuring the response
    TagResponse (..),
    mkTagResponse,

    -- ** Response lenses
    trrsArn,
    trrsTags,
    trrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | The ARN of the resource group to which to add tags.
    arn :: Types.Arn,
    -- | The tags to add to the specified resource group. A tag is a string-to-string map of key-value pairs.
    tags :: Core.HashMap Types.TagKey Types.TagValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag ::
  -- | 'arn'
  Types.Arn ->
  Tag
mkTag arn = Tag' {arn, tags = Core.mempty}

-- | The ARN of the resource group to which to add tags.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tArn :: Lens.Lens' Tag Types.Arn
tArn = Lens.field @"arn"
{-# DEPRECATED tArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags to add to the specified resource group. A tag is a string-to-string map of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTags :: Lens.Lens' Tag (Core.HashMap Types.TagKey Types.TagValue)
tTags = Lens.field @"tags"
{-# DEPRECATED tTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Tag where
  toJSON Tag {..} =
    Core.object (Core.catMaybes [Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest Tag where
  type Rs Tag = TagResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/resources/" Core.<> (Core.toText arn) Core.<> ("/tags")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TagResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTagResponse' smart constructor.
data TagResponse = TagResponse'
  { -- | The ARN of the tagged resource.
    arn :: Core.Maybe Types.GroupArn,
    -- | The tags that have been added to the specified resource group.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResponse' value with any optional fields omitted.
mkTagResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TagResponse
mkTagResponse responseStatus =
  TagResponse'
    { arn = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The ARN of the tagged resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrsArn :: Lens.Lens' TagResponse (Core.Maybe Types.GroupArn)
trrsArn = Lens.field @"arn"
{-# DEPRECATED trrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags that have been added to the specified resource group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrsTags :: Lens.Lens' TagResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
trrsTags = Lens.field @"tags"
{-# DEPRECATED trrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrsResponseStatus :: Lens.Lens' TagResponse Core.Int
trrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED trrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
