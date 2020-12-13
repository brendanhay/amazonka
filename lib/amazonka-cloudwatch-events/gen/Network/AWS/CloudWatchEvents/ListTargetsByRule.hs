{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListTargetsByRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets assigned to the specified rule.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchEvents.ListTargetsByRule
  ( -- * Creating a request
    ListTargetsByRule (..),
    mkListTargetsByRule,

    -- ** Request lenses
    ltbrRule,
    ltbrNextToken,
    ltbrEventBusName,
    ltbrLimit,

    -- * Destructuring the response
    ListTargetsByRuleResponse (..),
    mkListTargetsByRuleResponse,

    -- ** Response lenses
    ltbrrsNextToken,
    ltbrrsTargets,
    ltbrrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTargetsByRule' smart constructor.
data ListTargetsByRule = ListTargetsByRule'
  { -- | The name of the rule.
    rule :: Lude.Text,
    -- | The token returned by a previous call to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTargetsByRule' with the minimum fields required to make a request.
--
-- * 'rule' - The name of the rule.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
-- * 'eventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'limit' - The maximum number of results to return.
mkListTargetsByRule ::
  -- | 'rule'
  Lude.Text ->
  ListTargetsByRule
mkListTargetsByRule pRule_ =
  ListTargetsByRule'
    { rule = pRule_,
      nextToken = Lude.Nothing,
      eventBusName = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The name of the rule.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrRule :: Lens.Lens' ListTargetsByRule Lude.Text
ltbrRule = Lens.lens (rule :: ListTargetsByRule -> Lude.Text) (\s a -> s {rule = a} :: ListTargetsByRule)
{-# DEPRECATED ltbrRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrNextToken :: Lens.Lens' ListTargetsByRule (Lude.Maybe Lude.Text)
ltbrNextToken = Lens.lens (nextToken :: ListTargetsByRule -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTargetsByRule)
{-# DEPRECATED ltbrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrEventBusName :: Lens.Lens' ListTargetsByRule (Lude.Maybe Lude.Text)
ltbrEventBusName = Lens.lens (eventBusName :: ListTargetsByRule -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: ListTargetsByRule)
{-# DEPRECATED ltbrEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrLimit :: Lens.Lens' ListTargetsByRule (Lude.Maybe Lude.Natural)
ltbrLimit = Lens.lens (limit :: ListTargetsByRule -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListTargetsByRule)
{-# DEPRECATED ltbrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListTargetsByRule where
  page rq rs
    | Page.stop (rs Lens.^. ltbrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltbrrsTargets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltbrNextToken Lens..~ rs Lens.^. ltbrrsNextToken

instance Lude.AWSRequest ListTargetsByRule where
  type Rs ListTargetsByRule = ListTargetsByRuleResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTargetsByRuleResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Targets")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTargetsByRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ListTargetsByRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTargetsByRule where
  toJSON ListTargetsByRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Rule" Lude..= rule),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EventBusName" Lude..=) Lude.<$> eventBusName,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListTargetsByRule where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTargetsByRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTargetsByRuleResponse' smart constructor.
data ListTargetsByRuleResponse = ListTargetsByRuleResponse'
  { -- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The targets assigned to the rule.
    targets :: Lude.Maybe (Lude.NonEmpty Target),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTargetsByRuleResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
-- * 'targets' - The targets assigned to the rule.
-- * 'responseStatus' - The response status code.
mkListTargetsByRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTargetsByRuleResponse
mkListTargetsByRuleResponse pResponseStatus_ =
  ListTargetsByRuleResponse'
    { nextToken = Lude.Nothing,
      targets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrrsNextToken :: Lens.Lens' ListTargetsByRuleResponse (Lude.Maybe Lude.Text)
ltbrrsNextToken = Lens.lens (nextToken :: ListTargetsByRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTargetsByRuleResponse)
{-# DEPRECATED ltbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The targets assigned to the rule.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrrsTargets :: Lens.Lens' ListTargetsByRuleResponse (Lude.Maybe (Lude.NonEmpty Target))
ltbrrsTargets = Lens.lens (targets :: ListTargetsByRuleResponse -> Lude.Maybe (Lude.NonEmpty Target)) (\s a -> s {targets = a} :: ListTargetsByRuleResponse)
{-# DEPRECATED ltbrrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrrsResponseStatus :: Lens.Lens' ListTargetsByRuleResponse Lude.Int
ltbrrsResponseStatus = Lens.lens (responseStatus :: ListTargetsByRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTargetsByRuleResponse)
{-# DEPRECATED ltbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
