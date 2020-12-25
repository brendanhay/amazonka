{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListTargetsForSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets (thing groups) associated with a given Device Defender security profile.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTargetsForSecurityProfile
  ( -- * Creating a request
    ListTargetsForSecurityProfile (..),
    mkListTargetsForSecurityProfile,

    -- ** Request lenses
    ltfspSecurityProfileName,
    ltfspMaxResults,
    ltfspNextToken,

    -- * Destructuring the response
    ListTargetsForSecurityProfileResponse (..),
    mkListTargetsForSecurityProfileResponse,

    -- ** Response lenses
    ltfsprrsNextToken,
    ltfsprrsSecurityProfileTargets,
    ltfsprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTargetsForSecurityProfile' smart constructor.
data ListTargetsForSecurityProfile = ListTargetsForSecurityProfile'
  { -- | The security profile.
    securityProfileName :: Types.SecurityProfileName,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTargetsForSecurityProfile' value with any optional fields omitted.
mkListTargetsForSecurityProfile ::
  -- | 'securityProfileName'
  Types.SecurityProfileName ->
  ListTargetsForSecurityProfile
mkListTargetsForSecurityProfile securityProfileName =
  ListTargetsForSecurityProfile'
    { securityProfileName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfspSecurityProfileName :: Lens.Lens' ListTargetsForSecurityProfile Types.SecurityProfileName
ltfspSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED ltfspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfspMaxResults :: Lens.Lens' ListTargetsForSecurityProfile (Core.Maybe Core.Natural)
ltfspMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltfspMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfspNextToken :: Lens.Lens' ListTargetsForSecurityProfile (Core.Maybe Types.NextToken)
ltfspNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltfspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListTargetsForSecurityProfile where
  type
    Rs ListTargetsForSecurityProfile =
      ListTargetsForSecurityProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/security-profiles/" Core.<> (Core.toText securityProfileName)
                Core.<> ("/targets")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsForSecurityProfileResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "securityProfileTargets")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTargetsForSecurityProfile where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"securityProfileTargets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTargetsForSecurityProfileResponse' smart constructor.
data ListTargetsForSecurityProfileResponse = ListTargetsForSecurityProfileResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The thing groups to which the security profile is attached.
    securityProfileTargets :: Core.Maybe [Types.SecurityProfileTarget],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTargetsForSecurityProfileResponse' value with any optional fields omitted.
mkListTargetsForSecurityProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTargetsForSecurityProfileResponse
mkListTargetsForSecurityProfileResponse responseStatus =
  ListTargetsForSecurityProfileResponse'
    { nextToken = Core.Nothing,
      securityProfileTargets = Core.Nothing,
      responseStatus
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsprrsNextToken :: Lens.Lens' ListTargetsForSecurityProfileResponse (Core.Maybe Types.NextToken)
ltfsprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltfsprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The thing groups to which the security profile is attached.
--
-- /Note:/ Consider using 'securityProfileTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsprrsSecurityProfileTargets :: Lens.Lens' ListTargetsForSecurityProfileResponse (Core.Maybe [Types.SecurityProfileTarget])
ltfsprrsSecurityProfileTargets = Lens.field @"securityProfileTargets"
{-# DEPRECATED ltfsprrsSecurityProfileTargets "Use generic-lens or generic-optics with 'securityProfileTargets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsprrsResponseStatus :: Lens.Lens' ListTargetsForSecurityProfileResponse Core.Int
ltfsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltfsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
