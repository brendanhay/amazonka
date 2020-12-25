{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListSecurityProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the security profiles for the specified Amazon Connect instance.
--
-- For more information about security profiles, see <https://docs.aws.amazon.com/connect/latest/adminguide/connect-security-profiles.html Security Profiles> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListSecurityProfiles
  ( -- * Creating a request
    ListSecurityProfiles (..),
    mkListSecurityProfiles,

    -- ** Request lenses
    lspInstanceId,
    lspMaxResults,
    lspNextToken,

    -- * Destructuring the response
    ListSecurityProfilesResponse (..),
    mkListSecurityProfilesResponse,

    -- ** Response lenses
    lsprrsNextToken,
    lsprrsSecurityProfileSummaryList,
    lsprrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSecurityProfiles' value with any optional fields omitted.
mkListSecurityProfiles ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListSecurityProfiles
mkListSecurityProfiles instanceId =
  ListSecurityProfiles'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspInstanceId :: Lens.Lens' ListSecurityProfiles Types.InstanceId
lspInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lspInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspMaxResults :: Lens.Lens' ListSecurityProfiles (Core.Maybe Core.Natural)
lspMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lspMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspNextToken :: Lens.Lens' ListSecurityProfiles (Core.Maybe Types.NextToken)
lspNextToken = Lens.field @"nextToken"
{-# DEPRECATED lspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListSecurityProfiles where
  type Rs ListSecurityProfiles = ListSecurityProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/security-profiles-summary/" Core.<> (Core.toText instanceId)),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityProfilesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SecurityProfileSummaryList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSecurityProfiles where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"securityProfileSummaryList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the security profiles.
    securityProfileSummaryList :: Core.Maybe [Types.SecurityProfileSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSecurityProfilesResponse' value with any optional fields omitted.
mkListSecurityProfilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSecurityProfilesResponse
mkListSecurityProfilesResponse responseStatus =
  ListSecurityProfilesResponse'
    { nextToken = Core.Nothing,
      securityProfileSummaryList = Core.Nothing,
      responseStatus
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsNextToken :: Lens.Lens' ListSecurityProfilesResponse (Core.Maybe Types.NextToken)
lsprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the security profiles.
--
-- /Note:/ Consider using 'securityProfileSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsSecurityProfileSummaryList :: Lens.Lens' ListSecurityProfilesResponse (Core.Maybe [Types.SecurityProfileSummary])
lsprrsSecurityProfileSummaryList = Lens.field @"securityProfileSummaryList"
{-# DEPRECATED lsprrsSecurityProfileSummaryList "Use generic-lens or generic-optics with 'securityProfileSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsResponseStatus :: Lens.Lens' ListSecurityProfilesResponse Core.Int
lsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
