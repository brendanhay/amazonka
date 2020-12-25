{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists origin access identities.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
  ( -- * Creating a request
    ListCloudFrontOriginAccessIdentities (..),
    mkListCloudFrontOriginAccessIdentities,

    -- ** Request lenses
    lcfoaiMarker,
    lcfoaiMaxItems,

    -- * Destructuring the response
    ListCloudFrontOriginAccessIdentitiesResponse (..),
    mkListCloudFrontOriginAccessIdentitiesResponse,

    -- ** Response lenses
    lcfoairrsCloudFrontOriginAccessIdentityList,
    lcfoairrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list origin access identities.
--
-- /See:/ 'mkListCloudFrontOriginAccessIdentities' smart constructor.
data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities'
  { -- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
    marker :: Core.Maybe Types.String,
    -- | The maximum number of origin access identities you want in the response body.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCloudFrontOriginAccessIdentities' value with any optional fields omitted.
mkListCloudFrontOriginAccessIdentities ::
  ListCloudFrontOriginAccessIdentities
mkListCloudFrontOriginAccessIdentities =
  ListCloudFrontOriginAccessIdentities'
    { marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfoaiMarker :: Lens.Lens' ListCloudFrontOriginAccessIdentities (Core.Maybe Types.String)
lcfoaiMarker = Lens.field @"marker"
{-# DEPRECATED lcfoaiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of origin access identities you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfoaiMaxItems :: Lens.Lens' ListCloudFrontOriginAccessIdentities (Core.Maybe Types.String)
lcfoaiMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lcfoaiMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListCloudFrontOriginAccessIdentities where
  type
    Rs ListCloudFrontOriginAccessIdentities =
      ListCloudFrontOriginAccessIdentitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath "/2020-05-31/origin-access-identity/cloudfront",
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListCloudFrontOriginAccessIdentitiesResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCloudFrontOriginAccessIdentities where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^. Lens.field @"cloudFrontOriginAccessIdentityList"
              Core.. Lens.field @"isTruncated"
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^. Lens.field @"cloudFrontOriginAccessIdentityList"
              Core.. Lens.field @"nextMarker"
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs
            Lens.^. Lens.field @"cloudFrontOriginAccessIdentityList"
              Core.. Lens.field @"nextMarker"
        )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkListCloudFrontOriginAccessIdentitiesResponse' smart constructor.
data ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse'
  { -- | The @CloudFrontOriginAccessIdentityList@ type.
    cloudFrontOriginAccessIdentityList :: Types.CloudFrontOriginAccessIdentityList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCloudFrontOriginAccessIdentitiesResponse' value with any optional fields omitted.
mkListCloudFrontOriginAccessIdentitiesResponse ::
  -- | 'cloudFrontOriginAccessIdentityList'
  Types.CloudFrontOriginAccessIdentityList ->
  -- | 'responseStatus'
  Core.Int ->
  ListCloudFrontOriginAccessIdentitiesResponse
mkListCloudFrontOriginAccessIdentitiesResponse
  cloudFrontOriginAccessIdentityList
  responseStatus =
    ListCloudFrontOriginAccessIdentitiesResponse'
      { cloudFrontOriginAccessIdentityList,
        responseStatus
      }

-- | The @CloudFrontOriginAccessIdentityList@ type.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfoairrsCloudFrontOriginAccessIdentityList :: Lens.Lens' ListCloudFrontOriginAccessIdentitiesResponse Types.CloudFrontOriginAccessIdentityList
lcfoairrsCloudFrontOriginAccessIdentityList = Lens.field @"cloudFrontOriginAccessIdentityList"
{-# DEPRECATED lcfoairrsCloudFrontOriginAccessIdentityList "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfoairrsResponseStatus :: Lens.Lens' ListCloudFrontOriginAccessIdentitiesResponse Core.Int
lcfoairrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcfoairrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
