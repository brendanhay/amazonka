{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified Amazon SageMaker resource. You can add tags to notebook instances, training jobs, hyperparameter tuning jobs, batch transform jobs, models, labeling jobs, work teams, endpoint configurations, and endpoints.
--
-- Each tag consists of a key and an optional value. Tag keys must be unique per resource. For more information about tags, see For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> .
module Network.AWS.SageMaker.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atResourceArn,
    atTags,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,

    -- ** Response lenses
    atrrsTags,
    atrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The Amazon Resource Name (ARN) of the resource that you want to tag.
    resourceArn :: Types.ResourceArn,
    -- | An array of @Tag@ objects. Each tag is a key-value pair. Only the @key@ parameter is required. If you don't specify a value, Amazon SageMaker sets the value to an empty string.
    tags :: [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTags' value with any optional fields omitted.
mkAddTags ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  AddTags
mkAddTags resourceArn = AddTags' {resourceArn, tags = Core.mempty}

-- | The Amazon Resource Name (ARN) of the resource that you want to tag.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceArn :: Lens.Lens' AddTags Types.ResourceArn
atResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED atResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | An array of @Tag@ objects. Each tag is a key-value pair. Only the @key@ parameter is required. If you don't specify a value, Amazon SageMaker sets the value to an empty string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Types.Tag]
atTags = Lens.field @"tags"
{-# DEPRECATED atTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON AddTags where
  toJSON AddTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceArn" Core..= resourceArn),
            Core.Just ("Tags" Core..= tags)
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
          Core.pure ("X-Amz-Target", "SageMaker.AddTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AddTagsResponse'
            Core.<$> (x Core..:? "Tags") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | A list of tags associated with the Amazon SageMaker resource.
    tags :: Core.Maybe [Types.Tag],
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
  AddTagsResponse' {tags = Core.Nothing, responseStatus}

-- | A list of tags associated with the Amazon SageMaker resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsTags :: Lens.Lens' AddTagsResponse (Core.Maybe [Types.Tag])
atrrsTags = Lens.field @"tags"
{-# DEPRECATED atrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResponseStatus :: Lens.Lens' AddTagsResponse Core.Int
atrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
