{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListCommandInvocations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user runs SendCommand against three instances, then a command invocation is created for each requested instance ID. ListCommandInvocations provide status about command execution.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListCommandInvocations
  ( -- * Creating a request
    ListCommandInvocations (..),
    mkListCommandInvocations,

    -- ** Request lenses
    lciCommandId,
    lciDetails,
    lciFilters,
    lciInstanceId,
    lciMaxResults,
    lciNextToken,

    -- * Destructuring the response
    ListCommandInvocationsResponse (..),
    mkListCommandInvocationsResponse,

    -- ** Response lenses
    lcirrsCommandInvocations,
    lcirrsNextToken,
    lcirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListCommandInvocations' smart constructor.
data ListCommandInvocations = ListCommandInvocations'
  { -- | (Optional) The invocations for a specific command ID.
    commandId :: Core.Maybe Types.CommandId,
    -- | (Optional) If set this returns the response of the command executions and any command output. By default this is set to False.
    details :: Core.Maybe Core.Bool,
    -- | (Optional) One or more filters. Use a filter to return a more specific list of results.
    filters :: Core.Maybe (Core.NonEmpty Types.CommandFilter),
    -- | (Optional) The command execution details for a specific instance ID.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCommandInvocations' value with any optional fields omitted.
mkListCommandInvocations ::
  ListCommandInvocations
mkListCommandInvocations =
  ListCommandInvocations'
    { commandId = Core.Nothing,
      details = Core.Nothing,
      filters = Core.Nothing,
      instanceId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | (Optional) The invocations for a specific command ID.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciCommandId :: Lens.Lens' ListCommandInvocations (Core.Maybe Types.CommandId)
lciCommandId = Lens.field @"commandId"
{-# DEPRECATED lciCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | (Optional) If set this returns the response of the command executions and any command output. By default this is set to False.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciDetails :: Lens.Lens' ListCommandInvocations (Core.Maybe Core.Bool)
lciDetails = Lens.field @"details"
{-# DEPRECATED lciDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | (Optional) One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciFilters :: Lens.Lens' ListCommandInvocations (Core.Maybe (Core.NonEmpty Types.CommandFilter))
lciFilters = Lens.field @"filters"
{-# DEPRECATED lciFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | (Optional) The command execution details for a specific instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciInstanceId :: Lens.Lens' ListCommandInvocations (Core.Maybe Types.InstanceId)
lciInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lciInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListCommandInvocations (Core.Maybe Core.Natural)
lciMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lciMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListCommandInvocations (Core.Maybe Types.NextToken)
lciNextToken = Lens.field @"nextToken"
{-# DEPRECATED lciNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListCommandInvocations where
  toJSON ListCommandInvocations {..} =
    Core.object
      ( Core.catMaybes
          [ ("CommandId" Core..=) Core.<$> commandId,
            ("Details" Core..=) Core.<$> details,
            ("Filters" Core..=) Core.<$> filters,
            ("InstanceId" Core..=) Core.<$> instanceId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListCommandInvocations where
  type Rs ListCommandInvocations = ListCommandInvocationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.ListCommandInvocations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCommandInvocationsResponse'
            Core.<$> (x Core..:? "CommandInvocations")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCommandInvocations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"commandInvocations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListCommandInvocationsResponse' smart constructor.
data ListCommandInvocationsResponse = ListCommandInvocationsResponse'
  { -- | (Optional) A list of all invocations.
    commandInvocations :: Core.Maybe [Types.CommandInvocation],
    -- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCommandInvocationsResponse' value with any optional fields omitted.
mkListCommandInvocationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCommandInvocationsResponse
mkListCommandInvocationsResponse responseStatus =
  ListCommandInvocationsResponse'
    { commandInvocations =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | (Optional) A list of all invocations.
--
-- /Note:/ Consider using 'commandInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsCommandInvocations :: Lens.Lens' ListCommandInvocationsResponse (Core.Maybe [Types.CommandInvocation])
lcirrsCommandInvocations = Lens.field @"commandInvocations"
{-# DEPRECATED lcirrsCommandInvocations "Use generic-lens or generic-optics with 'commandInvocations' instead." #-}

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsNextToken :: Lens.Lens' ListCommandInvocationsResponse (Core.Maybe Types.NextToken)
lcirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsResponseStatus :: Lens.Lens' ListCommandInvocationsResponse Core.Int
lcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
