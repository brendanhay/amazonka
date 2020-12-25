{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAttachedPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies attached to the specified thing group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAttachedPolicies
  ( -- * Creating a request
    ListAttachedPolicies (..),
    mkListAttachedPolicies,

    -- ** Request lenses
    lapTarget,
    lapMarker,
    lapPageSize,
    lapRecursive,

    -- * Destructuring the response
    ListAttachedPoliciesResponse (..),
    mkListAttachedPoliciesResponse,

    -- ** Response lenses
    laprrsNextMarker,
    laprrsPolicies,
    laprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAttachedPolicies' smart constructor.
data ListAttachedPolicies = ListAttachedPolicies'
  { -- | The group or principal for which the policies will be listed. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
    target :: Types.Target,
    -- | The token to retrieve the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of results to be returned per request.
    pageSize :: Core.Maybe Core.Natural,
    -- | When true, recursively list attached policies.
    recursive :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAttachedPolicies' value with any optional fields omitted.
mkListAttachedPolicies ::
  -- | 'target'
  Types.Target ->
  ListAttachedPolicies
mkListAttachedPolicies target =
  ListAttachedPolicies'
    { target,
      marker = Core.Nothing,
      pageSize = Core.Nothing,
      recursive = Core.Nothing
    }

-- | The group or principal for which the policies will be listed. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapTarget :: Lens.Lens' ListAttachedPolicies Types.Target
lapTarget = Lens.field @"target"
{-# DEPRECATED lapTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapMarker :: Lens.Lens' ListAttachedPolicies (Core.Maybe Types.Marker)
lapMarker = Lens.field @"marker"
{-# DEPRECATED lapMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapPageSize :: Lens.Lens' ListAttachedPolicies (Core.Maybe Core.Natural)
lapPageSize = Lens.field @"pageSize"
{-# DEPRECATED lapPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | When true, recursively list attached policies.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapRecursive :: Lens.Lens' ListAttachedPolicies (Core.Maybe Core.Bool)
lapRecursive = Lens.field @"recursive"
{-# DEPRECATED lapRecursive "Use generic-lens or generic-optics with 'recursive' instead." #-}

instance Core.FromJSON ListAttachedPolicies where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ListAttachedPolicies where
  type Rs ListAttachedPolicies = ListAttachedPoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/attached-policies/" Core.<> (Core.toText target)),
        Core._rqQuery =
          Core.toQueryValue "marker" Core.<$> marker
            Core.<> (Core.toQueryValue "pageSize" Core.<$> pageSize)
            Core.<> (Core.toQueryValue "recursive" Core.<$> recursive),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttachedPoliciesResponse'
            Core.<$> (x Core..:? "nextMarker")
            Core.<*> (x Core..:? "policies")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAttachedPolicies where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"policies" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListAttachedPoliciesResponse' smart constructor.
data ListAttachedPoliciesResponse = ListAttachedPoliciesResponse'
  { -- | The token to retrieve the next set of results, or ``null`` if there are no more results.
    nextMarker :: Core.Maybe Types.Marker,
    -- | The policies.
    policies :: Core.Maybe [Types.Policy],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAttachedPoliciesResponse' value with any optional fields omitted.
mkListAttachedPoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAttachedPoliciesResponse
mkListAttachedPoliciesResponse responseStatus =
  ListAttachedPoliciesResponse'
    { nextMarker = Core.Nothing,
      policies = Core.Nothing,
      responseStatus
    }

-- | The token to retrieve the next set of results, or ``null`` if there are no more results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laprrsNextMarker :: Lens.Lens' ListAttachedPoliciesResponse (Core.Maybe Types.Marker)
laprrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED laprrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laprrsPolicies :: Lens.Lens' ListAttachedPoliciesResponse (Core.Maybe [Types.Policy])
laprrsPolicies = Lens.field @"policies"
{-# DEPRECATED laprrsPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laprrsResponseStatus :: Lens.Lens' ListAttachedPoliciesResponse Core.Int
laprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED laprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
