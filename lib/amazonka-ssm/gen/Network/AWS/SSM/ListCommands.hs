{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListCommands
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the commands requested by users of the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListCommands
  ( -- * Creating a request
    ListCommands (..),
    mkListCommands,

    -- ** Request lenses
    lcCommandId,
    lcFilters,
    lcInstanceId,
    lcMaxResults,
    lcNextToken,

    -- * Destructuring the response
    ListCommandsResponse (..),
    mkListCommandsResponse,

    -- ** Response lenses
    lcrrsCommands,
    lcrrsNextToken,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListCommands' smart constructor.
data ListCommands = ListCommands'
  { -- | (Optional) If provided, lists only the specified command.
    commandId :: Core.Maybe Types.CommandId,
    -- | (Optional) One or more filters. Use a filter to return a more specific list of results.
    filters :: Core.Maybe (Core.NonEmpty Types.CommandFilter),
    -- | (Optional) Lists commands issued against this instance ID.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCommands' value with any optional fields omitted.
mkListCommands ::
  ListCommands
mkListCommands =
  ListCommands'
    { commandId = Core.Nothing,
      filters = Core.Nothing,
      instanceId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | (Optional) If provided, lists only the specified command.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCommandId :: Lens.Lens' ListCommands (Core.Maybe Types.CommandId)
lcCommandId = Lens.field @"commandId"
{-# DEPRECATED lcCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | (Optional) One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcFilters :: Lens.Lens' ListCommands (Core.Maybe (Core.NonEmpty Types.CommandFilter))
lcFilters = Lens.field @"filters"
{-# DEPRECATED lcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | (Optional) Lists commands issued against this instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcInstanceId :: Lens.Lens' ListCommands (Core.Maybe Types.InstanceId)
lcInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListCommands (Core.Maybe Core.Natural)
lcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCommands (Core.Maybe Types.NextToken)
lcNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListCommands where
  toJSON ListCommands {..} =
    Core.object
      ( Core.catMaybes
          [ ("CommandId" Core..=) Core.<$> commandId,
            ("Filters" Core..=) Core.<$> filters,
            ("InstanceId" Core..=) Core.<$> instanceId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListCommands where
  type Rs ListCommands = ListCommandsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.ListCommands")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCommandsResponse'
            Core.<$> (x Core..:? "Commands")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCommands where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"commands" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListCommandsResponse' smart constructor.
data ListCommandsResponse = ListCommandsResponse'
  { -- | (Optional) The list of commands requested by the user.
    commands :: Core.Maybe [Types.Command],
    -- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCommandsResponse' value with any optional fields omitted.
mkListCommandsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCommandsResponse
mkListCommandsResponse responseStatus =
  ListCommandsResponse'
    { commands = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | (Optional) The list of commands requested by the user.
--
-- /Note:/ Consider using 'commands' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsCommands :: Lens.Lens' ListCommandsResponse (Core.Maybe [Types.Command])
lcrrsCommands = Lens.field @"commands"
{-# DEPRECATED lcrrsCommands "Use generic-lens or generic-optics with 'commands' instead." #-}

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListCommandsResponse (Core.Maybe Types.NextToken)
lcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListCommandsResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
