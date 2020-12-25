{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified resource.
--
-- Currently, you can attach tags to the following resources in AWS Organizations.
--
--     * AWS account
--
--
--     * Organization root
--
--
--     * Organizational unit (OU)
--
--
--     * Policy (any type)
--
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trResourceId,
    trTags,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The ID of the resource to add a tag to.
    resourceId :: Types.ResourceId,
    -- | A list of tags to add to the specified resource.
    --
    -- You can specify any of the following taggable resources.
    --
    --     * AWS account – specify the account ID number.
    --
    --
    --     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @
    --
    --
    --     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @
    --
    --
    --     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @
    --
    --
    -- For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ .
    tags :: [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResource' value with any optional fields omitted.
mkTagResource ::
  -- | 'resourceId'
  Types.ResourceId ->
  TagResource
mkTagResource resourceId =
  TagResource' {resourceId, tags = Core.mempty}

-- | The ID of the resource to add a tag to.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceId :: Lens.Lens' TagResource Types.ResourceId
trResourceId = Lens.field @"resourceId"
{-# DEPRECATED trResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | A list of tags to add to the specified resource.
--
-- You can specify any of the following taggable resources.
--
--     * AWS account – specify the account ID number.
--
--
--     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @
--
--
--     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @
--
--
--     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @
--
--
-- For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource [Types.Tag]
trTags = Lens.field @"tags"
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON TagResource where
  toJSON TagResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSOrganizationsV20161128.TagResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull TagResourceResponse'

-- | /See:/ 'mkTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResourceResponse' value with any optional fields omitted.
mkTagResourceResponse ::
  TagResourceResponse
mkTagResourceResponse = TagResourceResponse'
