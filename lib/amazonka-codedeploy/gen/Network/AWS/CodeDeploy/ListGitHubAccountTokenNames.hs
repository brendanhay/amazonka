{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of stored connections to GitHub accounts.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
  ( -- * Creating a request
    ListGitHubAccountTokenNames (..),
    mkListGitHubAccountTokenNames,

    -- ** Request lenses
    lghatnNextToken,

    -- * Destructuring the response
    ListGitHubAccountTokenNamesResponse (..),
    mkListGitHubAccountTokenNamesResponse,

    -- ** Response lenses
    lghatnrrsNextToken,
    lghatnrrsTokenNameList,
    lghatnrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListGitHubAccountTokenNames@ operation.
--
-- /See:/ 'mkListGitHubAccountTokenNames' smart constructor.
newtype ListGitHubAccountTokenNames = ListGitHubAccountTokenNames'
  { -- | An identifier returned from the previous @ListGitHubAccountTokenNames@ call. It can be used to return the next set of names in the list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListGitHubAccountTokenNames' value with any optional fields omitted.
mkListGitHubAccountTokenNames ::
  ListGitHubAccountTokenNames
mkListGitHubAccountTokenNames =
  ListGitHubAccountTokenNames' {nextToken = Core.Nothing}

-- | An identifier returned from the previous @ListGitHubAccountTokenNames@ call. It can be used to return the next set of names in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lghatnNextToken :: Lens.Lens' ListGitHubAccountTokenNames (Core.Maybe Types.NextToken)
lghatnNextToken = Lens.field @"nextToken"
{-# DEPRECATED lghatnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListGitHubAccountTokenNames where
  toJSON ListGitHubAccountTokenNames {..} =
    Core.object
      (Core.catMaybes [("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListGitHubAccountTokenNames where
  type
    Rs ListGitHubAccountTokenNames =
      ListGitHubAccountTokenNamesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.ListGitHubAccountTokenNames")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGitHubAccountTokenNamesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "tokenNameList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListGitHubAccountTokenNames where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"tokenNameList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the output of a @ListGitHubAccountTokenNames@ operation.
--
-- /See:/ 'mkListGitHubAccountTokenNamesResponse' smart constructor.
data ListGitHubAccountTokenNamesResponse = ListGitHubAccountTokenNamesResponse'
  { -- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent @ListGitHubAccountTokenNames@ call to return the next set of names in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of names of connections to GitHub accounts.
    tokenNameList :: Core.Maybe [Types.GitHubAccountTokenName],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGitHubAccountTokenNamesResponse' value with any optional fields omitted.
mkListGitHubAccountTokenNamesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGitHubAccountTokenNamesResponse
mkListGitHubAccountTokenNamesResponse responseStatus =
  ListGitHubAccountTokenNamesResponse'
    { nextToken = Core.Nothing,
      tokenNameList = Core.Nothing,
      responseStatus
    }

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent @ListGitHubAccountTokenNames@ call to return the next set of names in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lghatnrrsNextToken :: Lens.Lens' ListGitHubAccountTokenNamesResponse (Core.Maybe Types.NextToken)
lghatnrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lghatnrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of names of connections to GitHub accounts.
--
-- /Note:/ Consider using 'tokenNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lghatnrrsTokenNameList :: Lens.Lens' ListGitHubAccountTokenNamesResponse (Core.Maybe [Types.GitHubAccountTokenName])
lghatnrrsTokenNameList = Lens.field @"tokenNameList"
{-# DEPRECATED lghatnrrsTokenNameList "Use generic-lens or generic-optics with 'tokenNameList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lghatnrrsResponseStatus :: Lens.Lens' ListGitHubAccountTokenNamesResponse Core.Int
lghatnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lghatnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
