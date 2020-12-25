{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.ListDiscoveredResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a resource type and returns a list of resource identifiers for the resources of that type. A resource identifier includes the resource type, ID, and (if available) the custom resource name. The results consist of resources that AWS Config has discovered, including those that AWS Config is not currently recording. You can narrow the results to include only resources that have specific resource IDs or a resource name.
--
-- The response is paginated. By default, AWS Config lists 100 resource identifiers on each page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.Config.ListDiscoveredResources
  ( -- * Creating a request
    ListDiscoveredResources (..),
    mkListDiscoveredResources,

    -- ** Request lenses
    ldrResourceType,
    ldrIncludeDeletedResources,
    ldrLimit,
    ldrNextToken,
    ldrResourceIds,
    ldrResourceName,

    -- * Destructuring the response
    ListDiscoveredResourcesResponse (..),
    mkListDiscoveredResourcesResponse,

    -- ** Response lenses
    ldrrrsNextToken,
    ldrrrsResourceIdentifiers,
    ldrrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { -- | The type of resources that you want AWS Config to list in the response.
    resourceType :: Types.ResourceType,
    -- | Specifies whether AWS Config includes deleted resources in the results. By default, deleted resources are not included.
    includeDeletedResources :: Core.Maybe Core.Bool,
    -- | The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The IDs of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
    resourceIds :: Core.Maybe [Types.ResourceId],
    -- | The custom name of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
    resourceName :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDiscoveredResources' value with any optional fields omitted.
mkListDiscoveredResources ::
  -- | 'resourceType'
  Types.ResourceType ->
  ListDiscoveredResources
mkListDiscoveredResources resourceType =
  ListDiscoveredResources'
    { resourceType,
      includeDeletedResources = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing,
      resourceIds = Core.Nothing,
      resourceName = Core.Nothing
    }

-- | The type of resources that you want AWS Config to list in the response.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrResourceType :: Lens.Lens' ListDiscoveredResources Types.ResourceType
ldrResourceType = Lens.field @"resourceType"
{-# DEPRECATED ldrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Specifies whether AWS Config includes deleted resources in the results. By default, deleted resources are not included.
--
-- /Note:/ Consider using 'includeDeletedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrIncludeDeletedResources :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Bool)
ldrIncludeDeletedResources = Lens.field @"includeDeletedResources"
{-# DEPRECATED ldrIncludeDeletedResources "Use generic-lens or generic-optics with 'includeDeletedResources' instead." #-}

-- | The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrLimit :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Natural)
ldrLimit = Lens.field @"limit"
{-# DEPRECATED ldrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrNextToken :: Lens.Lens' ListDiscoveredResources (Core.Maybe Types.NextToken)
ldrNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrResourceIds :: Lens.Lens' ListDiscoveredResources (Core.Maybe [Types.ResourceId])
ldrResourceIds = Lens.field @"resourceIds"
{-# DEPRECATED ldrResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

-- | The custom name of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrResourceName :: Lens.Lens' ListDiscoveredResources (Core.Maybe Types.ResourceName)
ldrResourceName = Lens.field @"resourceName"
{-# DEPRECATED ldrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Core.FromJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceType" Core..= resourceType),
            ("includeDeletedResources" Core..=)
              Core.<$> includeDeletedResources,
            ("limit" Core..=) Core.<$> limit,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("resourceIds" Core..=) Core.<$> resourceIds,
            ("resourceName" Core..=) Core.<$> resourceName
          ]
      )

instance Core.AWSRequest ListDiscoveredResources where
  type Rs ListDiscoveredResources = ListDiscoveredResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.ListDiscoveredResources")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "resourceIdentifiers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDiscoveredResources where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"resourceIdentifiers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- |
--
-- /See:/ 'mkListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
    resourceIdentifiers :: Core.Maybe [Types.ResourceIdentifier],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDiscoveredResourcesResponse' value with any optional fields omitted.
mkListDiscoveredResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDiscoveredResourcesResponse
mkListDiscoveredResourcesResponse responseStatus =
  ListDiscoveredResourcesResponse'
    { nextToken = Core.Nothing,
      resourceIdentifiers = Core.Nothing,
      responseStatus
    }

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrrsNextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Core.Maybe Types.NextToken)
ldrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrrsResourceIdentifiers :: Lens.Lens' ListDiscoveredResourcesResponse (Core.Maybe [Types.ResourceIdentifier])
ldrrrsResourceIdentifiers = Lens.field @"resourceIdentifiers"
{-# DEPRECATED ldrrrsResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrrsResponseStatus :: Lens.Lens' ListDiscoveredResourcesResponse Core.Int
ldrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
