{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List CloudFront distributions.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListDistributions
  ( -- * Creating a request
    ListDistributions (..),
    mkListDistributions,

    -- ** Request lenses
    ldMarker,
    ldMaxItems,

    -- * Destructuring the response
    ListDistributionsResponse (..),
    mkListDistributionsResponse,

    -- ** Response lenses
    ldrrsDistributionList,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list your distributions.
--
-- /See:/ 'mkListDistributions' smart constructor.
data ListDistributions = ListDistributions'
  { -- | Use this when paginating results to indicate where to begin in your list of distributions. The results include distributions in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last distribution on that page).
    marker :: Core.Maybe Types.String,
    -- | The maximum number of distributions you want in the response body.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributions' value with any optional fields omitted.
mkListDistributions ::
  ListDistributions
mkListDistributions =
  ListDistributions'
    { marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of distributions. The results include distributions in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last distribution on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMarker :: Lens.Lens' ListDistributions (Core.Maybe Types.String)
ldMarker = Lens.field @"marker"
{-# DEPRECATED ldMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distributions you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxItems :: Lens.Lens' ListDistributions (Core.Maybe Types.String)
ldMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ldMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListDistributions where
  type Rs ListDistributions = ListDistributionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2020-05-31/distribution",
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDistributions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^. Lens.field @"distributionList" Core.. Lens.field @"isTruncated"
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^. Lens.field @"distributionList" Core.. Lens.field @"nextMarker"
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs
            Lens.^. Lens.field @"distributionList" Core.. Lens.field @"nextMarker"
        )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkListDistributionsResponse' smart constructor.
data ListDistributionsResponse = ListDistributionsResponse'
  { -- | The @DistributionList@ type.
    distributionList :: Types.DistributionList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDistributionsResponse' value with any optional fields omitted.
mkListDistributionsResponse ::
  -- | 'distributionList'
  Types.DistributionList ->
  -- | 'responseStatus'
  Core.Int ->
  ListDistributionsResponse
mkListDistributionsResponse distributionList responseStatus =
  ListDistributionsResponse' {distributionList, responseStatus}

-- | The @DistributionList@ type.
--
-- /Note:/ Consider using 'distributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDistributionList :: Lens.Lens' ListDistributionsResponse Types.DistributionList
ldrrsDistributionList = Lens.field @"distributionList"
{-# DEPRECATED ldrrsDistributionList "Use generic-lens or generic-optics with 'distributionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDistributionsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
