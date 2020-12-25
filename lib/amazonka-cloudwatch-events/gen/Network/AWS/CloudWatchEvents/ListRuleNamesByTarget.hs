{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListRuleNamesByTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the rules for the specified target. You can see which of the rules in Amazon EventBridge can invoke a specific target in your account.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchEvents.ListRuleNamesByTarget
  ( -- * Creating a request
    ListRuleNamesByTarget (..),
    mkListRuleNamesByTarget,

    -- ** Request lenses
    lrnbtTargetArn,
    lrnbtEventBusName,
    lrnbtLimit,
    lrnbtNextToken,

    -- * Destructuring the response
    ListRuleNamesByTargetResponse (..),
    mkListRuleNamesByTargetResponse,

    -- ** Response lenses
    lrnbtrrsNextToken,
    lrnbtrrsRuleNames,
    lrnbtrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRuleNamesByTarget' smart constructor.
data ListRuleNamesByTarget = ListRuleNamesByTarget'
  { -- | The Amazon Resource Name (ARN) of the target resource.
    targetArn :: Types.TargetArn,
    -- | The name or ARN of the event bus to list rules for. If you omit this, the default event bus is used.
    eventBusName :: Core.Maybe Types.EventBusNameOrArn,
    -- | The maximum number of results to return.
    limit :: Core.Maybe Core.Natural,
    -- | The token returned by a previous call to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRuleNamesByTarget' value with any optional fields omitted.
mkListRuleNamesByTarget ::
  -- | 'targetArn'
  Types.TargetArn ->
  ListRuleNamesByTarget
mkListRuleNamesByTarget targetArn =
  ListRuleNamesByTarget'
    { targetArn,
      eventBusName = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target resource.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtTargetArn :: Lens.Lens' ListRuleNamesByTarget Types.TargetArn
lrnbtTargetArn = Lens.field @"targetArn"
{-# DEPRECATED lrnbtTargetArn "Use generic-lens or generic-optics with 'targetArn' instead." #-}

-- | The name or ARN of the event bus to list rules for. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtEventBusName :: Lens.Lens' ListRuleNamesByTarget (Core.Maybe Types.EventBusNameOrArn)
lrnbtEventBusName = Lens.field @"eventBusName"
{-# DEPRECATED lrnbtEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtLimit :: Lens.Lens' ListRuleNamesByTarget (Core.Maybe Core.Natural)
lrnbtLimit = Lens.field @"limit"
{-# DEPRECATED lrnbtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtNextToken :: Lens.Lens' ListRuleNamesByTarget (Core.Maybe Types.NextToken)
lrnbtNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrnbtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListRuleNamesByTarget where
  toJSON ListRuleNamesByTarget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TargetArn" Core..= targetArn),
            ("EventBusName" Core..=) Core.<$> eventBusName,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListRuleNamesByTarget where
  type Rs ListRuleNamesByTarget = ListRuleNamesByTargetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.ListRuleNamesByTarget")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRuleNamesByTargetResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "RuleNames")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListRuleNamesByTarget where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"ruleNames" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListRuleNamesByTargetResponse' smart constructor.
data ListRuleNamesByTargetResponse = ListRuleNamesByTargetResponse'
  { -- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The names of the rules that can invoke the given target.
    ruleNames :: Core.Maybe [Types.RuleName],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRuleNamesByTargetResponse' value with any optional fields omitted.
mkListRuleNamesByTargetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRuleNamesByTargetResponse
mkListRuleNamesByTargetResponse responseStatus =
  ListRuleNamesByTargetResponse'
    { nextToken = Core.Nothing,
      ruleNames = Core.Nothing,
      responseStatus
    }

-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtrrsNextToken :: Lens.Lens' ListRuleNamesByTargetResponse (Core.Maybe Types.NextToken)
lrnbtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrnbtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of the rules that can invoke the given target.
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtrrsRuleNames :: Lens.Lens' ListRuleNamesByTargetResponse (Core.Maybe [Types.RuleName])
lrnbtrrsRuleNames = Lens.field @"ruleNames"
{-# DEPRECATED lrnbtrrsRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtrrsResponseStatus :: Lens.Lens' ListRuleNamesByTargetResponse Core.Int
lrnbtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrnbtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
