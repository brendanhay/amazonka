{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of all resources of the specified stack.
--
-- For deleted stacks, ListStackResources returns resource information for up to 90 days after the stack has been deleted.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackResources
  ( -- * Creating a request
    ListStackResources (..),
    mkListStackResources,

    -- ** Request lenses
    lsrStackName,
    lsrNextToken,

    -- * Destructuring the response
    ListStackResourcesResponse (..),
    mkListStackResourcesResponse,

    -- ** Response lenses
    lsrrrsNextToken,
    lsrrrsStackResourceSummaries,
    lsrrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'ListStackResource' action.
--
-- /See:/ 'mkListStackResources' smart constructor.
data ListStackResources = ListStackResources'
  { -- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
    --
    --
    --     * Running stacks: You can specify either the stack's name or its unique stack ID.
    --
    --
    --     * Deleted stacks: You must specify the unique stack ID.
    --
    --
    -- Default: There is no default value.
    stackName :: Types.StackName,
    -- | A string that identifies the next page of stack resources that you want to retrieve.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStackResources' value with any optional fields omitted.
mkListStackResources ::
  -- | 'stackName'
  Types.StackName ->
  ListStackResources
mkListStackResources stackName =
  ListStackResources' {stackName, nextToken = Core.Nothing}

-- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrStackName :: Lens.Lens' ListStackResources Types.StackName
lsrStackName = Lens.field @"stackName"
{-# DEPRECATED lsrStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | A string that identifies the next page of stack resources that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrNextToken :: Lens.Lens' ListStackResources (Core.Maybe Types.NextToken)
lsrNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListStackResources where
  type Rs ListStackResources = ListStackResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListStackResources")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" stackName)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListStackResourcesResult"
      ( \s h x ->
          ListStackResourcesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "StackResourceSummaries"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListStackResources where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"stackResourceSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The output for a 'ListStackResources' action.
--
-- /See:/ 'mkListStackResourcesResponse' smart constructor.
data ListStackResourcesResponse = ListStackResourcesResponse'
  { -- | If the output exceeds 1 MB, a string that identifies the next page of stack resources. If no additional page exists, this value is null.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @StackResourceSummary@ structures.
    stackResourceSummaries :: Core.Maybe [Types.StackResourceSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStackResourcesResponse' value with any optional fields omitted.
mkListStackResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListStackResourcesResponse
mkListStackResourcesResponse responseStatus =
  ListStackResourcesResponse'
    { nextToken = Core.Nothing,
      stackResourceSummaries = Core.Nothing,
      responseStatus
    }

-- | If the output exceeds 1 MB, a string that identifies the next page of stack resources. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrrsNextToken :: Lens.Lens' ListStackResourcesResponse (Core.Maybe Types.NextToken)
lsrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackResourceSummary@ structures.
--
-- /Note:/ Consider using 'stackResourceSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrrsStackResourceSummaries :: Lens.Lens' ListStackResourcesResponse (Core.Maybe [Types.StackResourceSummary])
lsrrrsStackResourceSummaries = Lens.field @"stackResourceSummaries"
{-# DEPRECATED lsrrrsStackResourceSummaries "Use generic-lens or generic-optics with 'stackResourceSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrrsResponseStatus :: Lens.Lens' ListStackResourcesResponse Core.Int
lsrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
