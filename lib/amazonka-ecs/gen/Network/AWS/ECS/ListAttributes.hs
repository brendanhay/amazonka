{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attributes for Amazon ECS resources within a specified target type and cluster. When you specify a target type and cluster, @ListAttributes@ returns a list of attribute objects, one for each attribute on each resource. You can filter the list of results to a single attribute name to only return results that have that name. You can also filter the results by attribute name and value, for example, to see which container instances in a cluster are running a Linux AMI (@ecs.os-type=linux@ ). 
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListAttributes
    (
    -- * Creating a request
      ListAttributes (..)
    , mkListAttributes
    -- ** Request lenses
    , laTargetType
    , laAttributeName
    , laAttributeValue
    , laCluster
    , laMaxResults
    , laNextToken

    -- * Destructuring the response
    , ListAttributesResponse (..)
    , mkListAttributesResponse
    -- ** Response lenses
    , larrsAttributes
    , larrsNextToken
    , larrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAttributes' smart constructor.
data ListAttributes = ListAttributes'
  { targetType :: Types.TargetType
    -- ^ The type of the target with which to list attributes.
  , attributeName :: Core.Maybe Core.Text
    -- ^ The name of the attribute with which to filter the results. 
  , attributeValue :: Core.Maybe Core.Text
    -- ^ The value of the attribute with which to filter results. You must also specify an attribute name to use this parameter.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster to list attributes. If you do not specify a cluster, the default cluster is assumed.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of cluster results returned by @ListAttributes@ in paginated output. When this parameter is used, @ListAttributes@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAttributes@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListAttributes@ returns up to 100 results and a @nextToken@ value if applicable.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value returned from a @ListAttributes@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAttributes' value with any optional fields omitted.
mkListAttributes
    :: Types.TargetType -- ^ 'targetType'
    -> ListAttributes
mkListAttributes targetType
  = ListAttributes'{targetType, attributeName = Core.Nothing,
                    attributeValue = Core.Nothing, cluster = Core.Nothing,
                    maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The type of the target with which to list attributes.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laTargetType :: Lens.Lens' ListAttributes Types.TargetType
laTargetType = Lens.field @"targetType"
{-# INLINEABLE laTargetType #-}
{-# DEPRECATED targetType "Use generic-lens or generic-optics with 'targetType' instead"  #-}

-- | The name of the attribute with which to filter the results. 
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAttributeName :: Lens.Lens' ListAttributes (Core.Maybe Core.Text)
laAttributeName = Lens.field @"attributeName"
{-# INLINEABLE laAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The value of the attribute with which to filter results. You must also specify an attribute name to use this parameter.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAttributeValue :: Lens.Lens' ListAttributes (Core.Maybe Core.Text)
laAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE laAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster to list attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laCluster :: Lens.Lens' ListAttributes (Core.Maybe Core.Text)
laCluster = Lens.field @"cluster"
{-# INLINEABLE laCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The maximum number of cluster results returned by @ListAttributes@ in paginated output. When this parameter is used, @ListAttributes@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAttributes@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListAttributes@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListAttributes (Core.Maybe Core.Int)
laMaxResults = Lens.field @"maxResults"
{-# INLINEABLE laMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a @ListAttributes@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAttributes (Core.Maybe Core.Text)
laNextToken = Lens.field @"nextToken"
{-# INLINEABLE laNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAttributes where
        toHeaders ListAttributes{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.ListAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAttributes where
        toJSON ListAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("targetType" Core..= targetType),
                  ("attributeName" Core..=) Core.<$> attributeName,
                  ("attributeValue" Core..=) Core.<$> attributeValue,
                  ("cluster" Core..=) Core.<$> cluster,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAttributes where
        type Rs ListAttributes = ListAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAttributesResponse' Core.<$>
                   (x Core..:? "attributes") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAttributes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"attributes" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAttributesResponse' smart constructor.
data ListAttributesResponse = ListAttributesResponse'
  { attributes :: Core.Maybe [Types.Attribute]
    -- ^ A list of attribute objects that meet the criteria of the request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value to include in a future @ListAttributes@ request. When the results of a @ListAttributes@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAttributesResponse' value with any optional fields omitted.
mkListAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAttributesResponse
mkListAttributesResponse responseStatus
  = ListAttributesResponse'{attributes = Core.Nothing,
                            nextToken = Core.Nothing, responseStatus}

-- | A list of attribute objects that meet the criteria of the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsAttributes :: Lens.Lens' ListAttributesResponse (Core.Maybe [Types.Attribute])
larrsAttributes = Lens.field @"attributes"
{-# INLINEABLE larrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The @nextToken@ value to include in a future @ListAttributes@ request. When the results of a @ListAttributes@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListAttributesResponse (Core.Maybe Core.Text)
larrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE larrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAttributesResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
