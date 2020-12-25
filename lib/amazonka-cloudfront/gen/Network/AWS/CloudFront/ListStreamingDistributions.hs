{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListStreamingDistributions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List streaming distributions.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListStreamingDistributions
  ( -- * Creating a request
    ListStreamingDistributions (..),
    mkListStreamingDistributions,

    -- ** Request lenses
    lsdMarker,
    lsdMaxItems,

    -- * Destructuring the response
    ListStreamingDistributionsResponse (..),
    mkListStreamingDistributionsResponse,

    -- ** Response lenses
    lsdrrsStreamingDistributionList,
    lsdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list your streaming distributions.
--
-- /See:/ 'mkListStreamingDistributions' smart constructor.
data ListStreamingDistributions = ListStreamingDistributions'
  { -- | The value that you provided for the @Marker@ request parameter.
    marker :: Core.Maybe Types.String,
    -- | The value that you provided for the @MaxItems@ request parameter.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreamingDistributions' value with any optional fields omitted.
mkListStreamingDistributions ::
  ListStreamingDistributions
mkListStreamingDistributions =
  ListStreamingDistributions'
    { marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The value that you provided for the @Marker@ request parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdMarker :: Lens.Lens' ListStreamingDistributions (Core.Maybe Types.String)
lsdMarker = Lens.field @"marker"
{-# DEPRECATED lsdMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The value that you provided for the @MaxItems@ request parameter.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdMaxItems :: Lens.Lens' ListStreamingDistributions (Core.Maybe Types.String)
lsdMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lsdMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListStreamingDistributions where
  type
    Rs ListStreamingDistributions =
      ListStreamingDistributionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2020-05-31/streaming-distribution",
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListStreamingDistributionsResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListStreamingDistributions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^. Lens.field @"streamingDistributionList"
              Core.. Lens.field @"isTruncated"
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^. Lens.field @"streamingDistributionList"
              Core.. Lens.field @"nextMarker"
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs
            Lens.^. Lens.field @"streamingDistributionList"
              Core.. Lens.field @"nextMarker"
        )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkListStreamingDistributionsResponse' smart constructor.
data ListStreamingDistributionsResponse = ListStreamingDistributionsResponse'
  { -- | The @StreamingDistributionList@ type.
    streamingDistributionList :: Types.StreamingDistributionList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStreamingDistributionsResponse' value with any optional fields omitted.
mkListStreamingDistributionsResponse ::
  -- | 'streamingDistributionList'
  Types.StreamingDistributionList ->
  -- | 'responseStatus'
  Core.Int ->
  ListStreamingDistributionsResponse
mkListStreamingDistributionsResponse
  streamingDistributionList
  responseStatus =
    ListStreamingDistributionsResponse'
      { streamingDistributionList,
        responseStatus
      }

-- | The @StreamingDistributionList@ type.
--
-- /Note:/ Consider using 'streamingDistributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrrsStreamingDistributionList :: Lens.Lens' ListStreamingDistributionsResponse Types.StreamingDistributionList
lsdrrsStreamingDistributionList = Lens.field @"streamingDistributionList"
{-# DEPRECATED lsdrrsStreamingDistributionList "Use generic-lens or generic-optics with 'streamingDistributionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrrsResponseStatus :: Lens.Lens' ListStreamingDistributionsResponse Core.Int
lsdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
