{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return the tags applied to an AWS Elastic Beanstalk resource. The response contains a list of tag key-value pairs.
--
-- Elastic Beanstalk supports tagging of all of its resources. For details about resource tagging, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/applications-tagging-resources.html Tagging Application Resources> .
module Network.AWS.ElasticBeanstalk.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrResourceArn,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrrsResourceArn,
    ltfrrrsResourceTags,
    ltfrrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
  { -- | The Amazon Resource Name (ARN) of the resouce for which a tag list is requested.
    --
    -- Must be the ARN of an Elastic Beanstalk resource.
    resourceArn :: Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  ListTagsForResource
mkListTagsForResource resourceArn =
  ListTagsForResource' {resourceArn}

-- | The Amazon Resource Name (ARN) of the resouce for which a tag list is requested.
--
-- Must be the ARN of an Elastic Beanstalk resource.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceArn :: Lens.Lens' ListTagsForResource Types.ResourceArn
ltfrResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED ltfrResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListTagsForResource")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ResourceArn" resourceArn)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListTagsForResourceResult"
      ( \s h x ->
          ListTagsForResourceResponse'
            Core.<$> (x Core..@? "ResourceArn")
            Core.<*> (x Core..@? "ResourceTags" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The Amazon Resource Name (ARN) of the resource for which a tag list was requested.
    resourceArn :: Core.Maybe Types.ResourceArn,
    -- | A list of tag key-value pairs.
    resourceTags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResourceResponse' value with any optional fields omitted.
mkListTagsForResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse responseStatus =
  ListTagsForResourceResponse'
    { resourceArn = Core.Nothing,
      resourceTags = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the resource for which a tag list was requested.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResourceArn :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.ResourceArn)
ltfrrrsResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED ltfrrrsResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | A list of tag key-value pairs.
--
-- /Note:/ Consider using 'resourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResourceTags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Types.Tag])
ltfrrrsResourceTags = Lens.field @"resourceTags"
{-# DEPRECATED ltfrrrsResourceTags "Use generic-lens or generic-optics with 'resourceTags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
ltfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
