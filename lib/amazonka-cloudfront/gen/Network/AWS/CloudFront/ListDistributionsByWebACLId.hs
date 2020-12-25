{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByWebACLId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the distributions that are associated with a specified AWS WAF web ACL.
module Network.AWS.CloudFront.ListDistributionsByWebACLId
  ( -- * Creating a request
    ListDistributionsByWebACLId (..),
    mkListDistributionsByWebACLId,

    -- ** Request lenses
    ldbwacliWebACLId,
    ldbwacliMarker,
    ldbwacliMaxItems,

    -- * Destructuring the response
    ListDistributionsByWebACLIdResponse (..),
    mkListDistributionsByWebACLIdResponse,

    -- ** Response lenses
    ldbwaclirrsDistributionList,
    ldbwaclirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list distributions that are associated with a specified AWS WAF web ACL.
--
-- /See:/ 'mkListDistributionsByWebACLId' smart constructor.
data ListDistributionsByWebACLId = ListDistributionsByWebACLId'
  { -- | The ID of the AWS WAF web ACL that you want to list the associated distributions. If you specify "null" for the ID, the request returns a list of the distributions that aren't associated with a web ACL.
    webACLId :: Types.String,
    -- | Use @Marker@ and @MaxItems@ to control pagination of results. If you have more than @MaxItems@ distributions that satisfy the request, the response includes a @NextMarker@ element. To get the next page of results, submit another request. For the value of @Marker@ , specify the value of @NextMarker@ from the last response. (For the first request, omit @Marker@ .)
    marker :: Core.Maybe Types.String,
    -- | The maximum number of distributions that you want CloudFront to return in the response body. The maximum and default values are both 100.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByWebACLId' value with any optional fields omitted.
mkListDistributionsByWebACLId ::
  -- | 'webACLId'
  Types.String ->
  ListDistributionsByWebACLId
mkListDistributionsByWebACLId webACLId =
  ListDistributionsByWebACLId'
    { webACLId,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The ID of the AWS WAF web ACL that you want to list the associated distributions. If you specify "null" for the ID, the request returns a list of the distributions that aren't associated with a web ACL.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwacliWebACLId :: Lens.Lens' ListDistributionsByWebACLId Types.String
ldbwacliWebACLId = Lens.field @"webACLId"
{-# DEPRECATED ldbwacliWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | Use @Marker@ and @MaxItems@ to control pagination of results. If you have more than @MaxItems@ distributions that satisfy the request, the response includes a @NextMarker@ element. To get the next page of results, submit another request. For the value of @Marker@ , specify the value of @NextMarker@ from the last response. (For the first request, omit @Marker@ .)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwacliMarker :: Lens.Lens' ListDistributionsByWebACLId (Core.Maybe Types.String)
ldbwacliMarker = Lens.field @"marker"
{-# DEPRECATED ldbwacliMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distributions that you want CloudFront to return in the response body. The maximum and default values are both 100.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwacliMaxItems :: Lens.Lens' ListDistributionsByWebACLId (Core.Maybe Types.String)
ldbwacliMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ldbwacliMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListDistributionsByWebACLId where
  type
    Rs ListDistributionsByWebACLId =
      ListDistributionsByWebACLIdResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/distributionsByWebACLId/"
                Core.<> (Core.toText webACLId)
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
          ListDistributionsByWebACLIdResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response to a request to list the distributions that are associated with a specified AWS WAF web ACL.
--
-- /See:/ 'mkListDistributionsByWebACLIdResponse' smart constructor.
data ListDistributionsByWebACLIdResponse = ListDistributionsByWebACLIdResponse'
  { -- | The @DistributionList@ type.
    distributionList :: Core.Maybe Types.DistributionList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDistributionsByWebACLIdResponse' value with any optional fields omitted.
mkListDistributionsByWebACLIdResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDistributionsByWebACLIdResponse
mkListDistributionsByWebACLIdResponse responseStatus =
  ListDistributionsByWebACLIdResponse'
    { distributionList =
        Core.Nothing,
      responseStatus
    }

-- | The @DistributionList@ type.
--
-- /Note:/ Consider using 'distributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwaclirrsDistributionList :: Lens.Lens' ListDistributionsByWebACLIdResponse (Core.Maybe Types.DistributionList)
ldbwaclirrsDistributionList = Lens.field @"distributionList"
{-# DEPRECATED ldbwaclirrsDistributionList "Use generic-lens or generic-optics with 'distributionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwaclirrsResponseStatus :: Lens.Lens' ListDistributionsByWebACLIdResponse Core.Int
ldbwaclirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldbwaclirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
