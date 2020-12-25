{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ChangeTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, edits, or deletes tags for a health check or a hosted zone.
--
-- For information about using tags for cost allocation, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.Route53.ChangeTagsForResource
  ( -- * Creating a request
    ChangeTagsForResource (..),
    mkChangeTagsForResource,

    -- ** Request lenses
    ctfrResourceType,
    ctfrResourceId,
    ctfrAddTags,
    ctfrRemoveTagKeys,

    -- * Destructuring the response
    ChangeTagsForResourceResponse (..),
    mkChangeTagsForResourceResponse,

    -- ** Response lenses
    ctfrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the tags that you want to add, edit, or delete.
--
-- /See:/ 'mkChangeTagsForResource' smart constructor.
data ChangeTagsForResource = ChangeTagsForResource'
  { -- | The type of the resource.
    --
    --
    --     * The resource type for health checks is @healthcheck@ .
    --
    --
    --     * The resource type for hosted zones is @hostedzone@ .
    resourceType :: Types.TagResourceType,
    -- | The ID of the resource for which you want to add, change, or delete tags.
    resourceId :: Types.TagResourceId,
    -- | A complex type that contains a list of the tags that you want to add to the specified health check or hosted zone and/or the tags that you want to edit @Value@ for.
    --
    -- You can add a maximum of 10 tags to a health check or a hosted zone.
    addTags :: Core.Maybe (Core.NonEmpty Types.Tag),
    -- | A complex type that contains a list of the tags that you want to delete from the specified health check or hosted zone. You can specify up to 10 keys.
    removeTagKeys :: Core.Maybe (Core.NonEmpty Types.TagKey)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeTagsForResource' value with any optional fields omitted.
mkChangeTagsForResource ::
  -- | 'resourceType'
  Types.TagResourceType ->
  -- | 'resourceId'
  Types.TagResourceId ->
  ChangeTagsForResource
mkChangeTagsForResource resourceType resourceId =
  ChangeTagsForResource'
    { resourceType,
      resourceId,
      addTags = Core.Nothing,
      removeTagKeys = Core.Nothing
    }

-- | The type of the resource.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
--
--
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrResourceType :: Lens.Lens' ChangeTagsForResource Types.TagResourceType
ctfrResourceType = Lens.field @"resourceType"
{-# DEPRECATED ctfrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the resource for which you want to add, change, or delete tags.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrResourceId :: Lens.Lens' ChangeTagsForResource Types.TagResourceId
ctfrResourceId = Lens.field @"resourceId"
{-# DEPRECATED ctfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | A complex type that contains a list of the tags that you want to add to the specified health check or hosted zone and/or the tags that you want to edit @Value@ for.
--
-- You can add a maximum of 10 tags to a health check or a hosted zone.
--
-- /Note:/ Consider using 'addTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrAddTags :: Lens.Lens' ChangeTagsForResource (Core.Maybe (Core.NonEmpty Types.Tag))
ctfrAddTags = Lens.field @"addTags"
{-# DEPRECATED ctfrAddTags "Use generic-lens or generic-optics with 'addTags' instead." #-}

-- | A complex type that contains a list of the tags that you want to delete from the specified health check or hosted zone. You can specify up to 10 keys.
--
-- /Note:/ Consider using 'removeTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrRemoveTagKeys :: Lens.Lens' ChangeTagsForResource (Core.Maybe (Core.NonEmpty Types.TagKey))
ctfrRemoveTagKeys = Lens.field @"removeTagKeys"
{-# DEPRECATED ctfrRemoveTagKeys "Use generic-lens or generic-optics with 'removeTagKeys' instead." #-}

instance Core.ToXML ChangeTagsForResource where
  toXML ChangeTagsForResource {..} =
    Core.toXMLNode "AddTags" (Core.toXMLList "Tag" Core.<$> addTags)
      Core.<> Core.toXMLNode
        "RemoveTagKeys"
        (Core.toXMLList "Key" Core.<$> removeTagKeys)
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeTagsForResourceRequest"

instance Core.AWSRequest ChangeTagsForResource where
  type Rs ChangeTagsForResource = ChangeTagsForResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/tags/" Core.<> (Core.toText resourceType)
                Core.<> ("/")
                Core.<> (Core.toText resourceId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ChangeTagsForResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Empty response for the request.
--
-- /See:/ 'mkChangeTagsForResourceResponse' smart constructor.
newtype ChangeTagsForResourceResponse = ChangeTagsForResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeTagsForResourceResponse' value with any optional fields omitted.
mkChangeTagsForResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ChangeTagsForResourceResponse
mkChangeTagsForResourceResponse responseStatus =
  ChangeTagsForResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrrrsResponseStatus :: Lens.Lens' ChangeTagsForResourceResponse Core.Int
ctfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
