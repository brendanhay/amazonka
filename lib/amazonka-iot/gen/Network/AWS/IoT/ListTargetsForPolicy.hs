{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListTargetsForPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List targets for the specified policy.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTargetsForPolicy
  ( -- * Creating a request
    ListTargetsForPolicy (..),
    mkListTargetsForPolicy,

    -- ** Request lenses
    ltfpPolicyName,
    ltfpMarker,
    ltfpPageSize,

    -- * Destructuring the response
    ListTargetsForPolicyResponse (..),
    mkListTargetsForPolicyResponse,

    -- ** Response lenses
    ltfprrsNextMarker,
    ltfprrsTargets,
    ltfprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { -- | The policy name.
    policyName :: Types.PolicyName,
    -- | A marker used to get the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of results to return at one time.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTargetsForPolicy' value with any optional fields omitted.
mkListTargetsForPolicy ::
  -- | 'policyName'
  Types.PolicyName ->
  ListTargetsForPolicy
mkListTargetsForPolicy policyName =
  ListTargetsForPolicy'
    { policyName,
      marker = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpPolicyName :: Lens.Lens' ListTargetsForPolicy Types.PolicyName
ltfpPolicyName = Lens.field @"policyName"
{-# DEPRECATED ltfpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpMarker :: Lens.Lens' ListTargetsForPolicy (Core.Maybe Types.Marker)
ltfpMarker = Lens.field @"marker"
{-# DEPRECATED ltfpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpPageSize :: Lens.Lens' ListTargetsForPolicy (Core.Maybe Core.Natural)
ltfpPageSize = Lens.field @"pageSize"
{-# DEPRECATED ltfpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.FromJSON ListTargetsForPolicy where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ListTargetsForPolicy where
  type Rs ListTargetsForPolicy = ListTargetsForPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/policy-targets/" Core.<> (Core.toText policyName)),
        Core._rqQuery =
          Core.toQueryValue "marker" Core.<$> marker
            Core.<> (Core.toQueryValue "pageSize" Core.<$> pageSize),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsForPolicyResponse'
            Core.<$> (x Core..:? "nextMarker")
            Core.<*> (x Core..:? "targets")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTargetsForPolicy where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"targets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { -- | A marker used to get the next set of results.
    nextMarker :: Core.Maybe Types.Marker,
    -- | The policy targets.
    targets :: Core.Maybe [Types.PolicyTarget],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTargetsForPolicyResponse' value with any optional fields omitted.
mkListTargetsForPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTargetsForPolicyResponse
mkListTargetsForPolicyResponse responseStatus =
  ListTargetsForPolicyResponse'
    { nextMarker = Core.Nothing,
      targets = Core.Nothing,
      responseStatus
    }

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsNextMarker :: Lens.Lens' ListTargetsForPolicyResponse (Core.Maybe Types.Marker)
ltfprrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED ltfprrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The policy targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsTargets :: Lens.Lens' ListTargetsForPolicyResponse (Core.Maybe [Types.PolicyTarget])
ltfprrsTargets = Lens.field @"targets"
{-# DEPRECATED ltfprrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsResponseStatus :: Lens.Lens' ListTargetsForPolicyResponse Core.Int
ltfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
