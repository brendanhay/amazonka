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
    ltrRuleDisabled,
    ltrTopic,
    ltrNextToken,
    ltrMaxResults,

    -- * Destructuring the response
    ListTopicRulesResponse (..),
    mkListTopicRulesResponse,

    -- ** Response lenses
    ltrrsRules,
    ltrrsNextToken,
    ltrrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ListTopicRules operation.
--
-- /See:/ 'mkListTopicRules' smart constructor.
data ListTopicRules = ListTopicRules'
  { -- | Specifies whether the rule is disabled.
    ruleDisabled :: Lude.Maybe Lude.Bool,
    -- | The topic.
    topic :: Lude.Maybe Lude.Text,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTopicRules' with the minimum fields required to make a request.
--
-- * 'ruleDisabled' - Specifies whether the rule is disabled.
-- * 'topic' - The topic.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'maxResults' - The maximum number of results to return.
mkListTopicRules ::
  ListTopicRules
mkListTopicRules =
  ListTopicRules'
    { ruleDisabled = Lude.Nothing,
      topic = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies whether the rule is disabled.
--
-- /Note:/ Consider using 'ruleDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrRuleDisabled :: Lens.Lens' ListTopicRules (Lude.Maybe Lude.Bool)
ltrRuleDisabled = Lens.lens (ruleDisabled :: ListTopicRules -> Lude.Maybe Lude.Bool) (\s a -> s {ruleDisabled = a} :: ListTopicRules)
{-# DEPRECATED ltrRuleDisabled "Use generic-lens or generic-optics with 'ruleDisabled' instead." #-}

-- | The topic.
--
-- /Note:/ Consider using 'topic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrTopic :: Lens.Lens' ListTopicRules (Lude.Maybe Lude.Text)
ltrTopic = Lens.lens (topic :: ListTopicRules -> Lude.Maybe Lude.Text) (\s a -> s {topic = a} :: ListTopicRules)
{-# DEPRECATED ltrTopic "Use generic-lens or generic-optics with 'topic' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrNextToken :: Lens.Lens' ListTopicRules (Lude.Maybe Lude.Text)
ltrNextToken = Lens.lens (nextToken :: ListTopicRules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTopicRules)
{-# DEPRECATED ltrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrMaxResults :: Lens.Lens' ListTopicRules (Lude.Maybe Lude.Natural)
ltrMaxResults = Lens.lens (maxResults :: ListTopicRules -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTopicRules)
{-# DEPRECATED ltrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTopicRules where
  page rq rs
    | Page.stop (rs Lens.^. ltrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrrsRules) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltrNextToken Lens..~ rs Lens.^. ltrrsNextToken

instance Lude.AWSRequest ListTopicRules where
  type Rs ListTopicRules = ListTopicRulesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTopicRulesResponse'
            Lude.<$> (x Lude..?> "rules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTopicRules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTopicRules where
  toPath = Lude.const "/rules"

instance Lude.ToQuery ListTopicRules where
  toQuery ListTopicRules' {..} =
    Lude.mconcat
      [ "ruleDisabled" Lude.=: ruleDisabled,
        "topic" Lude.=: topic,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | The output from the ListTopicRules operation.
--
-- /See:/ 'mkListTopicRulesResponse' smart constructor.
data ListTopicRulesResponse = ListTopicRulesResponse'
  { -- | The rules.
    rules :: Lude.Maybe [TopicRuleListItem],
    -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTopicRulesResponse' with the minimum fields required to make a request.
--
-- * 'rules' - The rules.
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListTopicRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTopicRulesResponse
mkListTopicRulesResponse pResponseStatus_ =
  ListTopicRulesResponse'
    { rules = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsRules :: Lens.Lens' ListTopicRulesResponse (Lude.Maybe [TopicRuleListItem])
ltrrsRules = Lens.lens (rules :: ListTopicRulesResponse -> Lude.Maybe [TopicRuleListItem]) (\s a -> s {rules = a} :: ListTopicRulesResponse)
{-# DEPRECATED ltrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTopicRulesResponse (Lude.Maybe Lude.Text)
ltrrsNextToken = Lens.lens (nextToken :: ListTopicRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTopicRulesResponse)
{-# DEPRECATED ltrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTopicRulesResponse Lude.Int
ltrrsResponseStatus = Lens.lens (responseStatus :: ListTopicRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTopicRulesResponse)
{-# DEPRECATED ltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
