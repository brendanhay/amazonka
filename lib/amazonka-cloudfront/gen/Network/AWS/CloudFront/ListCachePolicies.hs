{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListCachePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of cache policies.
--
-- You can optionally apply a filter to return only the managed policies created by AWS, or only the custom policies created in your AWS account.
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListCachePolicies
  ( -- * Creating a request
    ListCachePolicies (..),
    mkListCachePolicies,

    -- ** Request lenses
    lcpMarker,
    lcpMaxItems,
    lcpType,

    -- * Destructuring the response
    ListCachePoliciesResponse (..),
    mkListCachePoliciesResponse,

    -- ** Response lenses
    lcprrsCachePolicyList,
    lcprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCachePolicies' smart constructor.
data ListCachePolicies = ListCachePolicies'
  { -- | Use this field when paginating results to indicate where to begin in your list of cache policies. The response includes cache policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of cache policies that you want in the response.
    maxItems :: Core.Maybe Types.String,
    -- | A filter to return only the specified kinds of cache policies. Valid values are:
    --
    --
    --     * @managed@ – Returns only the managed policies created by AWS.
    --
    --
    --     * @custom@ – Returns only the custom policies created in your AWS account.
    type' :: Core.Maybe Types.CachePolicyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCachePolicies' value with any optional fields omitted.
mkListCachePolicies ::
  ListCachePolicies
mkListCachePolicies =
  ListCachePolicies'
    { marker = Core.Nothing,
      maxItems = Core.Nothing,
      type' = Core.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of cache policies. The response includes cache policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpMarker :: Lens.Lens' ListCachePolicies (Core.Maybe Types.String)
lcpMarker = Lens.field @"marker"
{-# DEPRECATED lcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of cache policies that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpMaxItems :: Lens.Lens' ListCachePolicies (Core.Maybe Types.String)
lcpMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lcpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A filter to return only the specified kinds of cache policies. Valid values are:
--
--
--     * @managed@ – Returns only the managed policies created by AWS.
--
--
--     * @custom@ – Returns only the custom policies created in your AWS account.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpType :: Lens.Lens' ListCachePolicies (Core.Maybe Types.CachePolicyType)
lcpType = Lens.field @"type'"
{-# DEPRECATED lcpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.AWSRequest ListCachePolicies where
  type Rs ListCachePolicies = ListCachePoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2020-05-31/cache-policy",
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
            Core.<> (Core.toQueryValue "Type" Core.<$> type'),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListCachePoliciesResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListCachePoliciesResponse' smart constructor.
data ListCachePoliciesResponse = ListCachePoliciesResponse'
  { -- | A list of cache policies.
    cachePolicyList :: Core.Maybe Types.CachePolicyList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCachePoliciesResponse' value with any optional fields omitted.
mkListCachePoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCachePoliciesResponse
mkListCachePoliciesResponse responseStatus =
  ListCachePoliciesResponse'
    { cachePolicyList = Core.Nothing,
      responseStatus
    }

-- | A list of cache policies.
--
-- /Note:/ Consider using 'cachePolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprrsCachePolicyList :: Lens.Lens' ListCachePoliciesResponse (Core.Maybe Types.CachePolicyList)
lcprrsCachePolicyList = Lens.field @"cachePolicyList"
{-# DEPRECATED lcprrsCachePolicyList "Use generic-lens or generic-optics with 'cachePolicyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprrsResponseStatus :: Lens.Lens' ListCachePoliciesResponse Core.Int
lcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
