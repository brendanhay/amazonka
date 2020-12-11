{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Amazon EventBridge rules. You can either list all the rules or you can provide a prefix to match to the rule names.
--
-- ListRules does not list the targets of a rule. To see the targets associated with a rule, use 'ListTargetsByRule' .
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchEvents.ListRules
  ( -- * Creating a request
    ListRules (..),
    mkListRules,

    -- ** Request lenses
    lrNextToken,
    lrEventBusName,
    lrNamePrefix,
    lrLimit,

    -- * Destructuring the response
    ListRulesResponse (..),
    mkListRulesResponse,

    -- ** Response lenses
    lrrsRules,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRules' smart constructor.
data ListRules = ListRules'
  { nextToken :: Lude.Maybe Lude.Text,
    eventBusName :: Lude.Maybe Lude.Text,
    namePrefix :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRules' with the minimum fields required to make a request.
--
-- * 'eventBusName' - The name or ARN of the event bus to list the rules for. If you omit this, the default event bus is used.
-- * 'limit' - The maximum number of results to return.
-- * 'namePrefix' - The prefix matching the rule name.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
mkListRules ::
  ListRules
mkListRules =
  ListRules'
    { nextToken = Lude.Nothing,
      eventBusName = Lude.Nothing,
      namePrefix = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRules (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListRules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRules)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name or ARN of the event bus to list the rules for. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrEventBusName :: Lens.Lens' ListRules (Lude.Maybe Lude.Text)
lrEventBusName = Lens.lens (eventBusName :: ListRules -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: ListRules)
{-# DEPRECATED lrEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The prefix matching the rule name.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNamePrefix :: Lens.Lens' ListRules (Lude.Maybe Lude.Text)
lrNamePrefix = Lens.lens (namePrefix :: ListRules -> Lude.Maybe Lude.Text) (\s a -> s {namePrefix = a} :: ListRules)
{-# DEPRECATED lrNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLimit :: Lens.Lens' ListRules (Lude.Maybe Lude.Natural)
lrLimit = Lens.lens (limit :: ListRules -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListRules)
{-# DEPRECATED lrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListRules where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsRules) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListRules where
  type Rs ListRules = ListRulesResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Lude.<$> (x Lude..?> "Rules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSEvents.ListRules" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRules where
  toJSON ListRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EventBusName" Lude..=) Lude.<$> eventBusName,
            ("NamePrefix" Lude..=) Lude.<$> namePrefix,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListRules where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { rules ::
      Lude.Maybe [Rule],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRulesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
-- * 'responseStatus' - The response status code.
-- * 'rules' - The rules that match the specified criteria.
mkListRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRulesResponse
mkListRulesResponse pResponseStatus_ =
  ListRulesResponse'
    { rules = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The rules that match the specified criteria.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsRules :: Lens.Lens' ListRulesResponse (Lude.Maybe [Rule])
lrrsRules = Lens.lens (rules :: ListRulesResponse -> Lude.Maybe [Rule]) (\s a -> s {rules = a} :: ListRulesResponse)
{-# DEPRECATED lrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListRulesResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRulesResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListRulesResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRulesResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
