{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListTopicRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the rules for the specific topic.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTopicRules
  ( -- * Creating a request
    ListTopicRules (..),
    mkListTopicRules,

    -- ** Request lenses
    ltrMaxResults,
    ltrNextToken,
    ltrRuleDisabled,
    ltrTopic,

    -- * Destructuring the response
    ListTopicRulesResponse (..),
    mkListTopicRulesResponse,

    -- ** Response lenses
    ltrrrsNextToken,
    ltrrrsRules,
    ltrrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListTopicRules operation.
--
-- /See:/ 'mkListTopicRules' smart constructor.
data ListTopicRules = ListTopicRules'
  { -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Specifies whether the rule is disabled.
    ruleDisabled :: Core.Maybe Core.Bool,
    -- | The topic.
    topic :: Core.Maybe Types.Topic
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTopicRules' value with any optional fields omitted.
mkListTopicRules ::
  ListTopicRules
mkListTopicRules =
  ListTopicRules'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      ruleDisabled = Core.Nothing,
      topic = Core.Nothing
    }

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrMaxResults :: Lens.Lens' ListTopicRules (Core.Maybe Core.Natural)
ltrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrNextToken :: Lens.Lens' ListTopicRules (Core.Maybe Types.NextToken)
ltrNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies whether the rule is disabled.
--
-- /Note:/ Consider using 'ruleDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrRuleDisabled :: Lens.Lens' ListTopicRules (Core.Maybe Core.Bool)
ltrRuleDisabled = Lens.field @"ruleDisabled"
{-# DEPRECATED ltrRuleDisabled "Use generic-lens or generic-optics with 'ruleDisabled' instead." #-}

-- | The topic.
--
-- /Note:/ Consider using 'topic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrTopic :: Lens.Lens' ListTopicRules (Core.Maybe Types.Topic)
ltrTopic = Lens.field @"topic"
{-# DEPRECATED ltrTopic "Use generic-lens or generic-optics with 'topic' instead." #-}

instance Core.AWSRequest ListTopicRules where
  type Rs ListTopicRules = ListTopicRulesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/rules",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "ruleDisabled" Core.<$> ruleDisabled)
            Core.<> (Core.toQueryValue "topic" Core.<$> topic),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicRulesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "rules")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTopicRules where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"rules" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The output from the ListTopicRules operation.
--
-- /See:/ 'mkListTopicRulesResponse' smart constructor.
data ListTopicRulesResponse = ListTopicRulesResponse'
  { -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The rules.
    rules :: Core.Maybe [Types.TopicRuleListItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTopicRulesResponse' value with any optional fields omitted.
mkListTopicRulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTopicRulesResponse
mkListTopicRulesResponse responseStatus =
  ListTopicRulesResponse'
    { nextToken = Core.Nothing,
      rules = Core.Nothing,
      responseStatus
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrrsNextToken :: Lens.Lens' ListTopicRulesResponse (Core.Maybe Types.NextToken)
ltrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrrsRules :: Lens.Lens' ListTopicRulesResponse (Core.Maybe [Types.TopicRuleListItem])
ltrrrsRules = Lens.field @"rules"
{-# DEPRECATED ltrrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrrsResponseStatus :: Lens.Lens' ListTopicRulesResponse Core.Int
ltrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
