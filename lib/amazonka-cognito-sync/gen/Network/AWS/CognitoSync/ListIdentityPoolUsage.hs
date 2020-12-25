{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListIdentityPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of identity pools registered with Cognito.
--
-- ListIdentityPoolUsage can only be called with developer credentials. You cannot make this API call with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.ListIdentityPoolUsage
  ( -- * Creating a request
    ListIdentityPoolUsage (..),
    mkListIdentityPoolUsage,

    -- ** Request lenses
    lipuMaxResults,
    lipuNextToken,

    -- * Destructuring the response
    ListIdentityPoolUsageResponse (..),
    mkListIdentityPoolUsageResponse,

    -- ** Response lenses
    lipurrsCount,
    lipurrsIdentityPoolUsages,
    lipurrsMaxResults,
    lipurrsNextToken,
    lipurrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for usage information on an identity pool.
--
-- /See:/ 'mkListIdentityPoolUsage' smart constructor.
data ListIdentityPoolUsage = ListIdentityPoolUsage'
  { -- | The maximum number of results to be returned.
    maxResults :: Core.Maybe Core.Int,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentityPoolUsage' value with any optional fields omitted.
mkListIdentityPoolUsage ::
  ListIdentityPoolUsage
mkListIdentityPoolUsage =
  ListIdentityPoolUsage'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipuMaxResults :: Lens.Lens' ListIdentityPoolUsage (Core.Maybe Core.Int)
lipuMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lipuMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipuNextToken :: Lens.Lens' ListIdentityPoolUsage (Core.Maybe Types.NextToken)
lipuNextToken = Lens.field @"nextToken"
{-# DEPRECATED lipuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListIdentityPoolUsage where
  type Rs ListIdentityPoolUsage = ListIdentityPoolUsageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/identitypools",
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
          ListIdentityPoolUsageResponse'
            Core.<$> (x Core..:? "Count")
            Core.<*> (x Core..:? "IdentityPoolUsages")
            Core.<*> (x Core..:? "MaxResults")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returned for a successful ListIdentityPoolUsage request.
--
-- /See:/ 'mkListIdentityPoolUsageResponse' smart constructor.
data ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse'
  { -- | Total number of identities for the identity pool.
    count :: Core.Maybe Core.Int,
    -- | Usage information for the identity pools.
    identityPoolUsages :: Core.Maybe [Types.IdentityPoolUsage],
    -- | The maximum number of results to be returned.
    maxResults :: Core.Maybe Core.Int,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListIdentityPoolUsageResponse' value with any optional fields omitted.
mkListIdentityPoolUsageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListIdentityPoolUsageResponse
mkListIdentityPoolUsageResponse responseStatus =
  ListIdentityPoolUsageResponse'
    { count = Core.Nothing,
      identityPoolUsages = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Total number of identities for the identity pool.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipurrsCount :: Lens.Lens' ListIdentityPoolUsageResponse (Core.Maybe Core.Int)
lipurrsCount = Lens.field @"count"
{-# DEPRECATED lipurrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Usage information for the identity pools.
--
-- /Note:/ Consider using 'identityPoolUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipurrsIdentityPoolUsages :: Lens.Lens' ListIdentityPoolUsageResponse (Core.Maybe [Types.IdentityPoolUsage])
lipurrsIdentityPoolUsages = Lens.field @"identityPoolUsages"
{-# DEPRECATED lipurrsIdentityPoolUsages "Use generic-lens or generic-optics with 'identityPoolUsages' instead." #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipurrsMaxResults :: Lens.Lens' ListIdentityPoolUsageResponse (Core.Maybe Core.Int)
lipurrsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lipurrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipurrsNextToken :: Lens.Lens' ListIdentityPoolUsageResponse (Core.Maybe Types.String)
lipurrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lipurrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipurrsResponseStatus :: Lens.Lens' ListIdentityPoolUsageResponse Core.Int
lipurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lipurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
