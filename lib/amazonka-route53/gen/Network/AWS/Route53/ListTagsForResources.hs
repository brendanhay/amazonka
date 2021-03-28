{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTagsForResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for up to 10 health checks or hosted zones.
--
-- For information about using tags for cost allocation, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.Route53.ListTagsForResources
    (
    -- * Creating a request
      ListTagsForResources (..)
    , mkListTagsForResources
    -- ** Request lenses
    , lResourceType
    , lResourceIds

    -- * Destructuring the response
    , ListTagsForResourcesResponse (..)
    , mkListTagsForResourcesResponse
    -- ** Response lenses
    , lrsResourceTagSets
    , lrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the health checks or hosted zones for which you want to list tags.
--
-- /See:/ 'mkListTagsForResources' smart constructor.
data ListTagsForResources = ListTagsForResources'
  { resourceType :: Types.TagResourceType
    -- ^ The type of the resources.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
--
--
  , resourceIds :: Core.NonEmpty Types.TagResourceId
    -- ^ A complex type that contains the ResourceId element for each resource for which you want to get a list of tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResources' value with any optional fields omitted.
mkListTagsForResources
    :: Types.TagResourceType -- ^ 'resourceType'
    -> Core.NonEmpty Types.TagResourceId -- ^ 'resourceIds'
    -> ListTagsForResources
mkListTagsForResources resourceType resourceIds
  = ListTagsForResources'{resourceType, resourceIds}

-- | The type of the resources.
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
lResourceType :: Lens.Lens' ListTagsForResources Types.TagResourceType
lResourceType = Lens.field @"resourceType"
{-# INLINEABLE lResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | A complex type that contains the ResourceId element for each resource for which you want to get a list of tags.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceIds :: Lens.Lens' ListTagsForResources (Core.NonEmpty Types.TagResourceId)
lResourceIds = Lens.field @"resourceIds"
{-# INLINEABLE lResourceIds #-}
{-# DEPRECATED resourceIds "Use generic-lens or generic-optics with 'resourceIds' instead"  #-}

instance Core.ToQuery ListTagsForResources where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForResources where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML ListTagsForResources where
        toXML ListTagsForResources{..}
          = Core.toXMLElement "ResourceIds"
              (Core.toXMLList "ResourceId" resourceIds)
        toXMLDocument
          = Core.newXMLDocument
              "{https://route53.amazonaws.com/doc/2013-04-01/}ListTagsForResourcesRequest"

instance Core.AWSRequest ListTagsForResources where
        type Rs ListTagsForResources = ListTagsForResourcesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2013-04-01/tags/" Core.<> Core.toText resourceType,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListTagsForResourcesResponse' Core.<$>
                   (x Core..@ "ResourceTagSets" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "ResourceTagSet")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type containing tags for the specified resources.
--
-- /See:/ 'mkListTagsForResourcesResponse' smart constructor.
data ListTagsForResourcesResponse = ListTagsForResourcesResponse'
  { resourceTagSets :: [Types.ResourceTagSet]
    -- ^ A list of @ResourceTagSet@ s containing tags associated with the specified resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResourcesResponse' value with any optional fields omitted.
mkListTagsForResourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForResourcesResponse
mkListTagsForResourcesResponse responseStatus
  = ListTagsForResourcesResponse'{resourceTagSets = Core.mempty,
                                  responseStatus}

-- | A list of @ResourceTagSet@ s containing tags associated with the specified resources.
--
-- /Note:/ Consider using 'resourceTagSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResourceTagSets :: Lens.Lens' ListTagsForResourcesResponse [Types.ResourceTagSet]
lrsResourceTagSets = Lens.field @"resourceTagSets"
{-# INLINEABLE lrsResourceTagSets #-}
{-# DEPRECATED resourceTagSets "Use generic-lens or generic-optics with 'resourceTagSets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListTagsForResourcesResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
