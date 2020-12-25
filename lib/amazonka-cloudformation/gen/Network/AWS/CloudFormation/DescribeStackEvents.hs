{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all stack related events for a specified stack in reverse chronological order. For more information about a stack's event history, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html Stacks> in the AWS CloudFormation User Guide.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStackEvents
  ( -- * Creating a request
    DescribeStackEvents (..),
    mkDescribeStackEvents,

    -- ** Request lenses
    dseNextToken,
    dseStackName,

    -- * Destructuring the response
    DescribeStackEventsResponse (..),
    mkDescribeStackEventsResponse,

    -- ** Response lenses
    dserrsNextToken,
    dserrsStackEvents,
    dserrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for 'DescribeStackEvents' action.
--
-- /See:/ 'mkDescribeStackEvents' smart constructor.
data DescribeStackEvents = DescribeStackEvents'
  { -- | A string that identifies the next page of events that you want to retrieve.
    nextToken :: Core.Maybe Types.NextToken,
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
    stackName :: Core.Maybe Types.StackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackEvents' value with any optional fields omitted.
mkDescribeStackEvents ::
  DescribeStackEvents
mkDescribeStackEvents =
  DescribeStackEvents'
    { nextToken = Core.Nothing,
      stackName = Core.Nothing
    }

-- | A string that identifies the next page of events that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseNextToken :: Lens.Lens' DescribeStackEvents (Core.Maybe Types.NextToken)
dseNextToken = Lens.field @"nextToken"
{-# DEPRECATED dseNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
dseStackName :: Lens.Lens' DescribeStackEvents (Core.Maybe Types.StackName)
dseStackName = Lens.field @"stackName"
{-# DEPRECATED dseStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.AWSRequest DescribeStackEvents where
  type Rs DescribeStackEvents = DescribeStackEventsResponse
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
            ( Core.pure ("Action", "DescribeStackEvents")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryValue "StackName" Core.<$> stackName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeStackEventsResult"
      ( \s h x ->
          DescribeStackEventsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (x Core..@? "StackEvents" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeStackEvents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"stackEvents" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The output for a 'DescribeStackEvents' action.
--
-- /See:/ 'mkDescribeStackEventsResponse' smart constructor.
data DescribeStackEventsResponse = DescribeStackEventsResponse'
  { -- | If the output exceeds 1 MB in size, a string that identifies the next page of events. If no additional page exists, this value is null.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @StackEvents@ structures.
    stackEvents :: Core.Maybe [Types.StackEvent],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStackEventsResponse' value with any optional fields omitted.
mkDescribeStackEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStackEventsResponse
mkDescribeStackEventsResponse responseStatus =
  DescribeStackEventsResponse'
    { nextToken = Core.Nothing,
      stackEvents = Core.Nothing,
      responseStatus
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next page of events. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dserrsNextToken :: Lens.Lens' DescribeStackEventsResponse (Core.Maybe Types.NextToken)
dserrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dserrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackEvents@ structures.
--
-- /Note:/ Consider using 'stackEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dserrsStackEvents :: Lens.Lens' DescribeStackEventsResponse (Core.Maybe [Types.StackEvent])
dserrsStackEvents = Lens.field @"stackEvents"
{-# DEPRECATED dserrsStackEvents "Use generic-lens or generic-optics with 'stackEvents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dserrsResponseStatus :: Lens.Lens' DescribeStackEventsResponse Core.Int
dserrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dserrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
