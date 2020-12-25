{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByCachePolicyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that’s associated with the specified cache policy.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByCachePolicyId
  ( -- * Creating a request
    ListDistributionsByCachePolicyId (..),
    mkListDistributionsByCachePolicyId,

    -- ** Request lenses
    ldbcpiCachePolicyId,
    ldbcpiMarker,
    ldbcpiMaxItems,

    -- * Destructuring the response
    ListDistributionsByCachePolicyIdResponse (..),
    mkListDistributionsByCachePolicyIdResponse,

    -- ** Response lenses
    ldbcpirrsDistributionIdList,
    ldbcpirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDistributionsByCachePolicyId' smart constructor.
data ListDistributionsByCachePolicyId = ListDistributionsByCachePolicyId'
  { -- | The ID of the cache policy whose associated distribution IDs you want to list.
    cachePolicyId :: Types.String,
    -- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of distribution IDs that you want in the response.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByCachePolicyId' value with any optional fields omitted.
mkListDistributionsByCachePolicyId ::
  -- | 'cachePolicyId'
  Types.String ->
  ListDistributionsByCachePolicyId
mkListDistributionsByCachePolicyId cachePolicyId =
  ListDistributionsByCachePolicyId'
    { cachePolicyId,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The ID of the cache policy whose associated distribution IDs you want to list.
--
-- /Note:/ Consider using 'cachePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiCachePolicyId :: Lens.Lens' ListDistributionsByCachePolicyId Types.String
ldbcpiCachePolicyId = Lens.field @"cachePolicyId"
{-# DEPRECATED ldbcpiCachePolicyId "Use generic-lens or generic-optics with 'cachePolicyId' instead." #-}

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiMarker :: Lens.Lens' ListDistributionsByCachePolicyId (Core.Maybe Types.String)
ldbcpiMarker = Lens.field @"marker"
{-# DEPRECATED ldbcpiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distribution IDs that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiMaxItems :: Lens.Lens' ListDistributionsByCachePolicyId (Core.Maybe Types.String)
ldbcpiMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ldbcpiMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListDistributionsByCachePolicyId where
  type
    Rs ListDistributionsByCachePolicyId =
      ListDistributionsByCachePolicyIdResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/distributionsByCachePolicyId/"
                Core.<> (Core.toText cachePolicyId)
            ),
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByCachePolicyIdResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListDistributionsByCachePolicyIdResponse' smart constructor.
data ListDistributionsByCachePolicyIdResponse = ListDistributionsByCachePolicyIdResponse'
  { -- | A list of distribution IDs.
    distributionIdList :: Core.Maybe Types.DistributionIdList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByCachePolicyIdResponse' value with any optional fields omitted.
mkListDistributionsByCachePolicyIdResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDistributionsByCachePolicyIdResponse
mkListDistributionsByCachePolicyIdResponse responseStatus =
  ListDistributionsByCachePolicyIdResponse'
    { distributionIdList =
        Core.Nothing,
      responseStatus
    }

-- | A list of distribution IDs.
--
-- /Note:/ Consider using 'distributionIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpirrsDistributionIdList :: Lens.Lens' ListDistributionsByCachePolicyIdResponse (Core.Maybe Types.DistributionIdList)
ldbcpirrsDistributionIdList = Lens.field @"distributionIdList"
{-# DEPRECATED ldbcpirrsDistributionIdList "Use generic-lens or generic-optics with 'distributionIdList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpirrsResponseStatus :: Lens.Lens' ListDistributionsByCachePolicyIdResponse Core.Int
ldbcpirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldbcpirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
