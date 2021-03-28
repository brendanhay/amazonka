{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListTagsForResource (..)
    , mkListTagsForResource
    -- ** Request lenses
    , ltfrResourceArn

    -- * Destructuring the response
    , ListTagsForResourceResponse (..)
    , mkListTagsForResourceResponse
    -- ** Response lenses
    , ltfrrrsResourceArn
    , ltfrrrsResourceTags
    , ltfrrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
  { resourceArn :: Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the resouce for which a tag list is requested.
--
-- Must be the ARN of an Elastic Beanstalk resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> ListTagsForResource
mkListTagsForResource resourceArn
  = ListTagsForResource'{resourceArn}

-- | The Amazon Resource Name (ARN) of the resouce for which a tag list is requested.
--
-- Must be the ARN of an Elastic Beanstalk resource.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceArn :: Lens.Lens' ListTagsForResource Types.ResourceArn
ltfrResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE ltfrResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery ListTagsForResource where
        toQuery ListTagsForResource{..}
          = Core.toQueryPair "Action" ("ListTagsForResource" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ResourceArn" resourceArn

instance Core.ToHeaders ListTagsForResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTagsForResource where
        type Rs ListTagsForResource = ListTagsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListTagsForResourceResult"
              (\ s h x ->
                 ListTagsForResourceResponse' Core.<$>
                   (x Core..@? "ResourceArn") Core.<*>
                     x Core..@? "ResourceTags" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { resourceArn :: Core.Maybe Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the resource for which a tag list was requested.
  , resourceTags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag key-value pairs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResourceResponse' value with any optional fields omitted.
mkListTagsForResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForResourceResponse
mkListTagsForResourceResponse responseStatus
  = ListTagsForResourceResponse'{resourceArn = Core.Nothing,
                                 resourceTags = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the resource for which a tag list was requested.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResourceArn :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.ResourceArn)
ltfrrrsResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE ltfrrrsResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | A list of tag key-value pairs.
--
-- /Note:/ Consider using 'resourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResourceTags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Types.Tag])
ltfrrrsResourceTags = Lens.field @"resourceTags"
{-# INLINEABLE ltfrrrsResourceTags #-}
{-# DEPRECATED resourceTags "Use generic-lens or generic-optics with 'resourceTags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
ltfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
