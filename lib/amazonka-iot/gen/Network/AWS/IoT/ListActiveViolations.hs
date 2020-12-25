{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListActiveViolations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active violations for a given Device Defender security profile.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListActiveViolations
  ( -- * Creating a request
    ListActiveViolations (..),
    mkListActiveViolations,

    -- ** Request lenses
    lavMaxResults,
    lavNextToken,
    lavSecurityProfileName,
    lavThingName,

    -- * Destructuring the response
    ListActiveViolationsResponse (..),
    mkListActiveViolationsResponse,

    -- ** Response lenses
    lavrrsActiveViolations,
    lavrrsNextToken,
    lavrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListActiveViolations' smart constructor.
data ListActiveViolations = ListActiveViolations'
  { -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The name of the Device Defender security profile for which violations are listed.
    securityProfileName :: Core.Maybe Types.SecurityProfileName,
    -- | The name of the thing whose active violations are listed.
    thingName :: Core.Maybe Types.DeviceDefenderThingName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListActiveViolations' value with any optional fields omitted.
mkListActiveViolations ::
  ListActiveViolations
mkListActiveViolations =
  ListActiveViolations'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      securityProfileName = Core.Nothing,
      thingName = Core.Nothing
    }

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavMaxResults :: Lens.Lens' ListActiveViolations (Core.Maybe Core.Natural)
lavMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lavMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavNextToken :: Lens.Lens' ListActiveViolations (Core.Maybe Types.NextToken)
lavNextToken = Lens.field @"nextToken"
{-# DEPRECATED lavNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the Device Defender security profile for which violations are listed.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavSecurityProfileName :: Lens.Lens' ListActiveViolations (Core.Maybe Types.SecurityProfileName)
lavSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED lavSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The name of the thing whose active violations are listed.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavThingName :: Lens.Lens' ListActiveViolations (Core.Maybe Types.DeviceDefenderThingName)
lavThingName = Lens.field @"thingName"
{-# DEPRECATED lavThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.AWSRequest ListActiveViolations where
  type Rs ListActiveViolations = ListActiveViolationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/active-violations",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> ( Core.toQueryValue "securityProfileName"
                        Core.<$> securityProfileName
                    )
            Core.<> (Core.toQueryValue "thingName" Core.<$> thingName),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActiveViolationsResponse'
            Core.<$> (x Core..:? "activeViolations")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListActiveViolations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"activeViolations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListActiveViolationsResponse' smart constructor.
data ListActiveViolationsResponse = ListActiveViolationsResponse'
  { -- | The list of active violations.
    activeViolations :: Core.Maybe [Types.ActiveViolation],
    -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListActiveViolationsResponse' value with any optional fields omitted.
mkListActiveViolationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListActiveViolationsResponse
mkListActiveViolationsResponse responseStatus =
  ListActiveViolationsResponse'
    { activeViolations = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of active violations.
--
-- /Note:/ Consider using 'activeViolations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsActiveViolations :: Lens.Lens' ListActiveViolationsResponse (Core.Maybe [Types.ActiveViolation])
lavrrsActiveViolations = Lens.field @"activeViolations"
{-# DEPRECATED lavrrsActiveViolations "Use generic-lens or generic-optics with 'activeViolations' instead." #-}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsNextToken :: Lens.Lens' ListActiveViolationsResponse (Core.Maybe Types.NextToken)
lavrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lavrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsResponseStatus :: Lens.Lens' ListActiveViolationsResponse Core.Int
lavrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lavrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
