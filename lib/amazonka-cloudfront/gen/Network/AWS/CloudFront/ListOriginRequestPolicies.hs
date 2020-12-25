{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListOriginRequestPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of origin request policies.
--
-- You can optionally apply a filter to return only the managed policies created by AWS, or only the custom policies created in your AWS account.
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListOriginRequestPolicies
  ( -- * Creating a request
    ListOriginRequestPolicies (..),
    mkListOriginRequestPolicies,

    -- ** Request lenses
    lorpMarker,
    lorpMaxItems,
    lorpType,

    -- * Destructuring the response
    ListOriginRequestPoliciesResponse (..),
    mkListOriginRequestPoliciesResponse,

    -- ** Response lenses
    lorprrsOriginRequestPolicyList,
    lorprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOriginRequestPolicies' smart constructor.
data ListOriginRequestPolicies = ListOriginRequestPolicies'
  { -- | Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of origin request policies that you want in the response.
    maxItems :: Core.Maybe Types.String,
    -- | A filter to return only the specified kinds of origin request policies. Valid values are:
    --
    --
    --     * @managed@ – Returns only the managed policies created by AWS.
    --
    --
    --     * @custom@ – Returns only the custom policies created in your AWS account.
    type' :: Core.Maybe Types.OriginRequestPolicyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOriginRequestPolicies' value with any optional fields omitted.
mkListOriginRequestPolicies ::
  ListOriginRequestPolicies
mkListOriginRequestPolicies =
  ListOriginRequestPolicies'
    { marker = Core.Nothing,
      maxItems = Core.Nothing,
      type' = Core.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorpMarker :: Lens.Lens' ListOriginRequestPolicies (Core.Maybe Types.String)
lorpMarker = Lens.field @"marker"
{-# DEPRECATED lorpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of origin request policies that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorpMaxItems :: Lens.Lens' ListOriginRequestPolicies (Core.Maybe Types.String)
lorpMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lorpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A filter to return only the specified kinds of origin request policies. Valid values are:
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
lorpType :: Lens.Lens' ListOriginRequestPolicies (Core.Maybe Types.OriginRequestPolicyType)
lorpType = Lens.field @"type'"
{-# DEPRECATED lorpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.AWSRequest ListOriginRequestPolicies where
  type
    Rs ListOriginRequestPolicies =
      ListOriginRequestPoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2020-05-31/origin-request-policy",
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
          ListOriginRequestPoliciesResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListOriginRequestPoliciesResponse' smart constructor.
data ListOriginRequestPoliciesResponse = ListOriginRequestPoliciesResponse'
  { -- | A list of origin request policies.
    originRequestPolicyList :: Core.Maybe Types.OriginRequestPolicyList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListOriginRequestPoliciesResponse' value with any optional fields omitted.
mkListOriginRequestPoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListOriginRequestPoliciesResponse
mkListOriginRequestPoliciesResponse responseStatus =
  ListOriginRequestPoliciesResponse'
    { originRequestPolicyList =
        Core.Nothing,
      responseStatus
    }

-- | A list of origin request policies.
--
-- /Note:/ Consider using 'originRequestPolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorprrsOriginRequestPolicyList :: Lens.Lens' ListOriginRequestPoliciesResponse (Core.Maybe Types.OriginRequestPolicyList)
lorprrsOriginRequestPolicyList = Lens.field @"originRequestPolicyList"
{-# DEPRECATED lorprrsOriginRequestPolicyList "Use generic-lens or generic-optics with 'originRequestPolicyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorprrsResponseStatus :: Lens.Lens' ListOriginRequestPoliciesResponse Core.Int
lorprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lorprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
