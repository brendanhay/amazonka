{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    lrnbtNextToken,
    lrnbtEventBusName,
    lrnbtLimit,
    lrnbtTargetARN,

    -- * Destructuring the response
    ListRuleNamesByTargetResponse (..),
    mkListRuleNamesByTargetResponse,

    -- ** Response lenses
    lrnbtrsRuleNames,
    lrnbtrsNextToken,
    lrnbtrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRuleNamesByTarget' smart constructor.
data ListRuleNamesByTarget = ListRuleNamesByTarget'
  { nextToken ::
      Lude.Maybe Lude.Text,
    eventBusName :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    targetARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRuleNamesByTarget' with the minimum fields required to make a request.
--
-- * 'eventBusName' - The name or ARN of the event bus to list rules for. If you omit this, the default event bus is used.
-- * 'limit' - The maximum number of results to return.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the target resource.
mkListRuleNamesByTarget ::
  -- | 'targetARN'
  Lude.Text ->
  ListRuleNamesByTarget
mkListRuleNamesByTarget pTargetARN_ =
  ListRuleNamesByTarget'
    { nextToken = Lude.Nothing,
      eventBusName = Lude.Nothing,
      limit = Lude.Nothing,
      targetARN = pTargetARN_
    }

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtNextToken :: Lens.Lens' ListRuleNamesByTarget (Lude.Maybe Lude.Text)
lrnbtNextToken = Lens.lens (nextToken :: ListRuleNamesByTarget -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRuleNamesByTarget)
{-# DEPRECATED lrnbtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name or ARN of the event bus to list rules for. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtEventBusName :: Lens.Lens' ListRuleNamesByTarget (Lude.Maybe Lude.Text)
lrnbtEventBusName = Lens.lens (eventBusName :: ListRuleNamesByTarget -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: ListRuleNamesByTarget)
{-# DEPRECATED lrnbtEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtLimit :: Lens.Lens' ListRuleNamesByTarget (Lude.Maybe Lude.Natural)
lrnbtLimit = Lens.lens (limit :: ListRuleNamesByTarget -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListRuleNamesByTarget)
{-# DEPRECATED lrnbtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The Amazon Resource Name (ARN) of the target resource.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtTargetARN :: Lens.Lens' ListRuleNamesByTarget Lude.Text
lrnbtTargetARN = Lens.lens (targetARN :: ListRuleNamesByTarget -> Lude.Text) (\s a -> s {targetARN = a} :: ListRuleNamesByTarget)
{-# DEPRECATED lrnbtTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

instance Page.AWSPager ListRuleNamesByTarget where
  page rq rs
    | Page.stop (rs Lens.^. lrnbtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrnbtrsRuleNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrnbtNextToken Lens..~ rs Lens.^. lrnbtrsNextToken

instance Lude.AWSRequest ListRuleNamesByTarget where
  type Rs ListRuleNamesByTarget = ListRuleNamesByTargetResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRuleNamesByTargetResponse'
            Lude.<$> (x Lude..?> "RuleNames" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRuleNamesByTarget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ListRuleNamesByTarget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRuleNamesByTarget where
  toJSON ListRuleNamesByTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EventBusName" Lude..=) Lude.<$> eventBusName,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("TargetArn" Lude..= targetARN)
          ]
      )

instance Lude.ToPath ListRuleNamesByTarget where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRuleNamesByTarget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRuleNamesByTargetResponse' smart constructor.
data ListRuleNamesByTargetResponse = ListRuleNamesByTargetResponse'
  { ruleNames ::
      Lude.Maybe [Lude.Text],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRuleNamesByTargetResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
-- * 'responseStatus' - The response status code.
-- * 'ruleNames' - The names of the rules that can invoke the given target.
mkListRuleNamesByTargetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRuleNamesByTargetResponse
mkListRuleNamesByTargetResponse pResponseStatus_ =
  ListRuleNamesByTargetResponse'
    { ruleNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of the rules that can invoke the given target.
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtrsRuleNames :: Lens.Lens' ListRuleNamesByTargetResponse (Lude.Maybe [Lude.Text])
lrnbtrsRuleNames = Lens.lens (ruleNames :: ListRuleNamesByTargetResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {ruleNames = a} :: ListRuleNamesByTargetResponse)
{-# DEPRECATED lrnbtrsRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtrsNextToken :: Lens.Lens' ListRuleNamesByTargetResponse (Lude.Maybe Lude.Text)
lrnbtrsNextToken = Lens.lens (nextToken :: ListRuleNamesByTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRuleNamesByTargetResponse)
{-# DEPRECATED lrnbtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrnbtrsResponseStatus :: Lens.Lens' ListRuleNamesByTargetResponse Lude.Int
lrnbtrsResponseStatus = Lens.lens (responseStatus :: ListRuleNamesByTargetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRuleNamesByTargetResponse)
{-# DEPRECATED lrnbtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
