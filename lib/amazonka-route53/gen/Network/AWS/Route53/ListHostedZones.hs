{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListHostedZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the public and private hosted zones that are associated with the current AWS account. The response includes a @HostedZones@ child element for each hosted zone.
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of hosted zones, you can use the @maxitems@ parameter to list them in groups of up to 100.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListHostedZones
  ( -- * Creating a request
    ListHostedZones (..),
    mkListHostedZones,

    -- ** Request lenses
    lhzDelegationSetId,
    lhzMarker,
    lhzMaxItems,

    -- * Destructuring the response
    ListHostedZonesResponse (..),
    mkListHostedZonesResponse,

    -- ** Response lenses
    lhzrrsHostedZones,
    lhzrrsMarker,
    lhzrrsIsTruncated,
    lhzrrsMaxItems,
    lhzrrsNextMarker,
    lhzrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to retrieve a list of the public and private hosted zones that are associated with the current AWS account.
--
-- /See:/ 'mkListHostedZones' smart constructor.
data ListHostedZones = ListHostedZones'
  { -- | If you're using reusable delegation sets and you want to list all of the hosted zones that are associated with a reusable delegation set, specify the ID of that reusable delegation set.
    delegationSetId :: Core.Maybe Types.DelegationSetId,
    -- | If the value of @IsTruncated@ in the previous response was @true@ , you have more hosted zones. To get more hosted zones, submit another @ListHostedZones@ request.
    --
    -- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first hosted zone that Amazon Route 53 will return if you submit another request.
    -- If the value of @IsTruncated@ in the previous response was @false@ , there are no more hosted zones to get.
    marker :: Core.Maybe Types.Marker,
    -- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If you have more than @maxitems@ hosted zones, the value of @IsTruncated@ in the response is @true@ , and the value of @NextMarker@ is the hosted zone ID of the first hosted zone that Route 53 will return if you submit another request.
    maxItems :: Core.Maybe Types.MaxItems
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHostedZones' value with any optional fields omitted.
mkListHostedZones ::
  ListHostedZones
mkListHostedZones =
  ListHostedZones'
    { delegationSetId = Core.Nothing,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | If you're using reusable delegation sets and you want to list all of the hosted zones that are associated with a reusable delegation set, specify the ID of that reusable delegation set.
--
-- /Note:/ Consider using 'delegationSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzDelegationSetId :: Lens.Lens' ListHostedZones (Core.Maybe Types.DelegationSetId)
lhzDelegationSetId = Lens.field @"delegationSetId"
{-# DEPRECATED lhzDelegationSetId "Use generic-lens or generic-optics with 'delegationSetId' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more hosted zones. To get more hosted zones, submit another @ListHostedZones@ request.
--
-- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first hosted zone that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more hosted zones to get.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzMarker :: Lens.Lens' ListHostedZones (Core.Maybe Types.Marker)
lhzMarker = Lens.field @"marker"
{-# DEPRECATED lhzMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If you have more than @maxitems@ hosted zones, the value of @IsTruncated@ in the response is @true@ , and the value of @NextMarker@ is the hosted zone ID of the first hosted zone that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzMaxItems :: Lens.Lens' ListHostedZones (Core.Maybe Types.MaxItems)
lhzMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lhzMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListHostedZones where
  type Rs ListHostedZones = ListHostedZonesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/hostedzone",
        Core._rqQuery =
          Core.toQueryValue "delegationsetid" Core.<$> delegationSetId
            Core.<> (Core.toQueryValue "marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "maxitems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListHostedZonesResponse'
            Core.<$> ( x Core..@? "HostedZones" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "HostedZone"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "MaxItems")
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListHostedZones where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"nextMarker") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListHostedZonesResponse' smart constructor.
data ListHostedZonesResponse = ListHostedZonesResponse'
  { -- | A complex type that contains general information about the hosted zone.
    hostedZones :: [Types.HostedZone],
    -- | For the second and subsequent calls to @ListHostedZones@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
    marker :: Core.Maybe Types.Marker,
    -- | A flag indicating whether there are more hosted zones to be listed. If the response was truncated, you can get more hosted zones by submitting another @ListHostedZones@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
    isTruncated :: Core.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZones@ that produced the current response.
    maxItems :: Types.MaxItems,
    -- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first hosted zone in the next group of hosted zones. Submit another @ListHostedZones@ request, and specify the value of @NextMarker@ from the response in the @marker@ parameter.
    --
    -- This element is present only if @IsTruncated@ is @true@ .
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHostedZonesResponse' value with any optional fields omitted.
mkListHostedZonesResponse ::
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'maxItems'
  Types.MaxItems ->
  -- | 'responseStatus'
  Core.Int ->
  ListHostedZonesResponse
mkListHostedZonesResponse isTruncated maxItems responseStatus =
  ListHostedZonesResponse'
    { hostedZones = Core.mempty,
      marker = Core.Nothing,
      isTruncated,
      maxItems,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | A complex type that contains general information about the hosted zone.
--
-- /Note:/ Consider using 'hostedZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrrsHostedZones :: Lens.Lens' ListHostedZonesResponse [Types.HostedZone]
lhzrrsHostedZones = Lens.field @"hostedZones"
{-# DEPRECATED lhzrrsHostedZones "Use generic-lens or generic-optics with 'hostedZones' instead." #-}

-- | For the second and subsequent calls to @ListHostedZones@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrrsMarker :: Lens.Lens' ListHostedZonesResponse (Core.Maybe Types.Marker)
lhzrrsMarker = Lens.field @"marker"
{-# DEPRECATED lhzrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag indicating whether there are more hosted zones to be listed. If the response was truncated, you can get more hosted zones by submitting another @ListHostedZones@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrrsIsTruncated :: Lens.Lens' ListHostedZonesResponse Core.Bool
lhzrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lhzrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZones@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrrsMaxItems :: Lens.Lens' ListHostedZonesResponse Types.MaxItems
lhzrrsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lhzrrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first hosted zone in the next group of hosted zones. Submit another @ListHostedZones@ request, and specify the value of @NextMarker@ from the response in the @marker@ parameter.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrrsNextMarker :: Lens.Lens' ListHostedZonesResponse (Core.Maybe Types.NextMarker)
lhzrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lhzrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrrsResponseStatus :: Lens.Lens' ListHostedZonesResponse Core.Int
lhzrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lhzrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
