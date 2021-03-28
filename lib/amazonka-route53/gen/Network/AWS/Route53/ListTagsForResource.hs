{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for one health check or hosted zone. 
--
-- For information about using tags for cost allocation, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.Route53.ListTagsForResource
    (
    -- * Creating a request
      ListTagsForResource (..)
    , mkListTagsForResource
    -- ** Request lenses
    , ltfrResourceType
    , ltfrResourceId

    -- * Destructuring the response
    , ListTagsForResourceResponse (..)
    , mkListTagsForResourceResponse
    -- ** Response lenses
    , ltfrrrsResourceTagSet
    , ltfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type containing information about a request for a list of the tags that are associated with an individual resource.
--
-- /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { resourceType :: Types.TagResourceType
    -- ^ The type of the resource.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
--
--
  , resourceId :: Types.TagResourceId
    -- ^ The ID of the resource for which you want to retrieve tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource
    :: Types.TagResourceType -- ^ 'resourceType'
    -> Types.TagResourceId -- ^ 'resourceId'
    -> ListTagsForResource
mkListTagsForResource resourceType resourceId
  = ListTagsForResource'{resourceType, resourceId}

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
ltfrResourceType :: Lens.Lens' ListTagsForResource Types.TagResourceType
ltfrResourceType = Lens.field @"resourceType"
{-# INLINEABLE ltfrResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The ID of the resource for which you want to retrieve tags.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceId :: Lens.Lens' ListTagsForResource Types.TagResourceId
ltfrResourceId = Lens.field @"resourceId"
{-# INLINEABLE ltfrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.ToQuery ListTagsForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTagsForResource where
        type Rs ListTagsForResource = ListTagsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/tags/" Core.<> Core.toText resourceType Core.<> "/"
                             Core.<> Core.toText resourceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListTagsForResourceResponse' Core.<$>
                   (x Core..@ "ResourceTagSet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains information about the health checks or hosted zones for which you want to list tags.
--
-- /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { resourceTagSet :: Types.ResourceTagSet
    -- ^ A @ResourceTagSet@ containing tags associated with the specified resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResourceResponse' value with any optional fields omitted.
mkListTagsForResourceResponse
    :: Types.ResourceTagSet -- ^ 'resourceTagSet'
    -> Core.Int -- ^ 'responseStatus'
    -> ListTagsForResourceResponse
mkListTagsForResourceResponse resourceTagSet responseStatus
  = ListTagsForResourceResponse'{resourceTagSet, responseStatus}

-- | A @ResourceTagSet@ containing tags associated with the specified resource.
--
-- /Note:/ Consider using 'resourceTagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResourceTagSet :: Lens.Lens' ListTagsForResourceResponse Types.ResourceTagSet
ltfrrrsResourceTagSet = Lens.field @"resourceTagSet"
{-# INLINEABLE ltfrrrsResourceTagSet #-}
{-# DEPRECATED resourceTagSet "Use generic-lens or generic-optics with 'resourceTagSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
ltfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
