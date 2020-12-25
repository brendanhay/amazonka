{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListReusableDelegationSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the reusable delegation sets that are associated with the current AWS account.
module Network.AWS.Route53.ListReusableDelegationSets
  ( -- * Creating a request
    ListReusableDelegationSets (..),
    mkListReusableDelegationSets,

    -- ** Request lenses
    lrdsMarker,
    lrdsMaxItems,

    -- * Destructuring the response
    ListReusableDelegationSetsResponse (..),
    mkListReusableDelegationSetsResponse,

    -- ** Response lenses
    lrdsrrsDelegationSets,
    lrdsrrsMarker,
    lrdsrrsIsTruncated,
    lrdsrrsMaxItems,
    lrdsrrsNextMarker,
    lrdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to get a list of the reusable delegation sets that are associated with the current AWS account.
--
-- /See:/ 'mkListReusableDelegationSets' smart constructor.
data ListReusableDelegationSets = ListReusableDelegationSets'
  { -- | If the value of @IsTruncated@ in the previous response was @true@ , you have more reusable delegation sets. To get another group, submit another @ListReusableDelegationSets@ request.
    --
    -- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first reusable delegation set that Amazon Route 53 will return if you submit another request.
    -- If the value of @IsTruncated@ in the previous response was @false@ , there are no more reusable delegation sets to get.
    marker :: Core.Maybe Types.Marker,
    -- | The number of reusable delegation sets that you want Amazon Route 53 to return in the response to this request. If you specify a value greater than 100, Route 53 returns only the first 100 reusable delegation sets.
    maxItems :: Core.Maybe Types.MaxItems
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReusableDelegationSets' value with any optional fields omitted.
mkListReusableDelegationSets ::
  ListReusableDelegationSets
mkListReusableDelegationSets =
  ListReusableDelegationSets'
    { marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more reusable delegation sets. To get another group, submit another @ListReusableDelegationSets@ request.
--
-- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first reusable delegation set that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more reusable delegation sets to get.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsMarker :: Lens.Lens' ListReusableDelegationSets (Core.Maybe Types.Marker)
lrdsMarker = Lens.field @"marker"
{-# DEPRECATED lrdsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The number of reusable delegation sets that you want Amazon Route 53 to return in the response to this request. If you specify a value greater than 100, Route 53 returns only the first 100 reusable delegation sets.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsMaxItems :: Lens.Lens' ListReusableDelegationSets (Core.Maybe Types.MaxItems)
lrdsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lrdsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListReusableDelegationSets where
  type
    Rs ListReusableDelegationSets =
      ListReusableDelegationSetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/delegationset",
        Core._rqQuery =
          Core.toQueryValue "marker" Core.<$> marker
            Core.<> (Core.toQueryValue "maxitems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListReusableDelegationSetsResponse'
            Core.<$> ( x Core..@? "DelegationSets" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "DelegationSet"
                     )
            Core.<*> (x Core..@ "Marker")
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "MaxItems")
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains information about the reusable delegation sets that are associated with the current AWS account.
--
-- /See:/ 'mkListReusableDelegationSetsResponse' smart constructor.
data ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse'
  { -- | A complex type that contains one @DelegationSet@ element for each reusable delegation set that was created by the current AWS account.
    delegationSets :: [Types.DelegationSet],
    -- | For the second and subsequent calls to @ListReusableDelegationSets@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
    marker :: Types.Marker,
    -- | A flag that indicates whether there are more reusable delegation sets to be listed.
    isTruncated :: Core.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to @ListReusableDelegationSets@ that produced the current response.
    maxItems :: Types.MaxItems,
    -- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the next reusable delegation set that Amazon Route 53 will return if you submit another @ListReusableDelegationSets@ request and specify the value of @NextMarker@ in the @marker@ parameter.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReusableDelegationSetsResponse' value with any optional fields omitted.
mkListReusableDelegationSetsResponse ::
  -- | 'marker'
  Types.Marker ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'maxItems'
  Types.MaxItems ->
  -- | 'responseStatus'
  Core.Int ->
  ListReusableDelegationSetsResponse
mkListReusableDelegationSetsResponse
  marker
  isTruncated
  maxItems
  responseStatus =
    ListReusableDelegationSetsResponse'
      { delegationSets = Core.mempty,
        marker,
        isTruncated,
        maxItems,
        nextMarker = Core.Nothing,
        responseStatus
      }

-- | A complex type that contains one @DelegationSet@ element for each reusable delegation set that was created by the current AWS account.
--
-- /Note:/ Consider using 'delegationSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsDelegationSets :: Lens.Lens' ListReusableDelegationSetsResponse [Types.DelegationSet]
lrdsrrsDelegationSets = Lens.field @"delegationSets"
{-# DEPRECATED lrdsrrsDelegationSets "Use generic-lens or generic-optics with 'delegationSets' instead." #-}

-- | For the second and subsequent calls to @ListReusableDelegationSets@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsMarker :: Lens.Lens' ListReusableDelegationSetsResponse Types.Marker
lrdsrrsMarker = Lens.field @"marker"
{-# DEPRECATED lrdsrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more reusable delegation sets to be listed.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsIsTruncated :: Lens.Lens' ListReusableDelegationSetsResponse Core.Bool
lrdsrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lrdsrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the call to @ListReusableDelegationSets@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsMaxItems :: Lens.Lens' ListReusableDelegationSetsResponse Types.MaxItems
lrdsrrsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lrdsrrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the next reusable delegation set that Amazon Route 53 will return if you submit another @ListReusableDelegationSets@ request and specify the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsNextMarker :: Lens.Lens' ListReusableDelegationSetsResponse (Core.Maybe Types.NextMarker)
lrdsrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lrdsrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsResponseStatus :: Lens.Lens' ListReusableDelegationSetsResponse Core.Int
lrdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
