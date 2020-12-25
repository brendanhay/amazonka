{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListLambdaFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the Lambda functions that show up in the drop-down options in the relevant contact flow blocks.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLambdaFunctions
  ( -- * Creating a request
    ListLambdaFunctions (..),
    mkListLambdaFunctions,

    -- ** Request lenses
    llfInstanceId,
    llfMaxResults,
    llfNextToken,

    -- * Destructuring the response
    ListLambdaFunctionsResponse (..),
    mkListLambdaFunctionsResponse,

    -- ** Response lenses
    llfrrsLambdaFunctions,
    llfrrsNextToken,
    llfrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLambdaFunctions' smart constructor.
data ListLambdaFunctions = ListLambdaFunctions'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLambdaFunctions' value with any optional fields omitted.
mkListLambdaFunctions ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListLambdaFunctions
mkListLambdaFunctions instanceId =
  ListLambdaFunctions'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfInstanceId :: Lens.Lens' ListLambdaFunctions Types.InstanceId
llfInstanceId = Lens.field @"instanceId"
{-# DEPRECATED llfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfMaxResults :: Lens.Lens' ListLambdaFunctions (Core.Maybe Core.Natural)
llfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED llfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfNextToken :: Lens.Lens' ListLambdaFunctions (Core.Maybe Types.NextToken)
llfNextToken = Lens.field @"nextToken"
{-# DEPRECATED llfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListLambdaFunctions where
  type Rs ListLambdaFunctions = ListLambdaFunctionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/lambda-functions")
            ),
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
          ListLambdaFunctionsResponse'
            Core.<$> (x Core..:? "LambdaFunctions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLambdaFunctions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"lambdaFunctions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListLambdaFunctionsResponse' smart constructor.
data ListLambdaFunctionsResponse = ListLambdaFunctionsResponse'
  { -- | The Lambdafunction ARNs associated with the specified instance.
    lambdaFunctions :: Core.Maybe [Types.FunctionArn],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLambdaFunctionsResponse' value with any optional fields omitted.
mkListLambdaFunctionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLambdaFunctionsResponse
mkListLambdaFunctionsResponse responseStatus =
  ListLambdaFunctionsResponse'
    { lambdaFunctions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The Lambdafunction ARNs associated with the specified instance.
--
-- /Note:/ Consider using 'lambdaFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfrrsLambdaFunctions :: Lens.Lens' ListLambdaFunctionsResponse (Core.Maybe [Types.FunctionArn])
llfrrsLambdaFunctions = Lens.field @"lambdaFunctions"
{-# DEPRECATED llfrrsLambdaFunctions "Use generic-lens or generic-optics with 'lambdaFunctions' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfrrsNextToken :: Lens.Lens' ListLambdaFunctionsResponse (Core.Maybe Types.NextToken)
llfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED llfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfrrsResponseStatus :: Lens.Lens' ListLambdaFunctionsResponse Core.Int
llfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED llfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
