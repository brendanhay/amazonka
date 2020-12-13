{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListSubscribedRuleGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleGroup' objects that you are subscribed to.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListSubscribedRuleGroups
  ( -- * Creating a request
    ListSubscribedRuleGroups (..),
    mkListSubscribedRuleGroups,

    -- ** Request lenses
    lsrgNextMarker,
    lsrgLimit,

    -- * Destructuring the response
    ListSubscribedRuleGroupsResponse (..),
    mkListSubscribedRuleGroupsResponse,

    -- ** Response lenses
    lsrgrsRuleGroups,
    lsrgrsNextMarker,
    lsrgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkListSubscribedRuleGroups' smart constructor.
data ListSubscribedRuleGroups = ListSubscribedRuleGroups'
  { -- | If you specify a value for @Limit@ and you have more @ByteMatchSets@ subscribed rule groups than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of subscribed rule groups. For the second and subsequent @ListSubscribedRuleGroupsRequest@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of subscribed rule groups.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Specifies the number of subscribed rule groups that you want AWS WAF to return for this request. If you have more objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of objects.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscribedRuleGroups' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @ByteMatchSets@ subscribed rule groups than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of subscribed rule groups. For the second and subsequent @ListSubscribedRuleGroupsRequest@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of subscribed rule groups.
-- * 'limit' - Specifies the number of subscribed rule groups that you want AWS WAF to return for this request. If you have more objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of objects.
mkListSubscribedRuleGroups ::
  ListSubscribedRuleGroups
mkListSubscribedRuleGroups =
  ListSubscribedRuleGroups'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @ByteMatchSets@ subscribed rule groups than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of subscribed rule groups. For the second and subsequent @ListSubscribedRuleGroupsRequest@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of subscribed rule groups.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgNextMarker :: Lens.Lens' ListSubscribedRuleGroups (Lude.Maybe Lude.Text)
lsrgNextMarker = Lens.lens (nextMarker :: ListSubscribedRuleGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListSubscribedRuleGroups)
{-# DEPRECATED lsrgNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of subscribed rule groups that you want AWS WAF to return for this request. If you have more objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgLimit :: Lens.Lens' ListSubscribedRuleGroups (Lude.Maybe Lude.Natural)
lsrgLimit = Lens.lens (limit :: ListSubscribedRuleGroups -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListSubscribedRuleGroups)
{-# DEPRECATED lsrgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListSubscribedRuleGroups where
  page rq rs
    | Page.stop (rs Lens.^. lsrgrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrgrsRuleGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsrgNextMarker Lens..~ rs Lens.^. lsrgrsNextMarker

instance Lude.AWSRequest ListSubscribedRuleGroups where
  type Rs ListSubscribedRuleGroups = ListSubscribedRuleGroupsResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSubscribedRuleGroupsResponse'
            Lude.<$> (x Lude..?> "RuleGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSubscribedRuleGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.ListSubscribedRuleGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSubscribedRuleGroups where
  toJSON ListSubscribedRuleGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListSubscribedRuleGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSubscribedRuleGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSubscribedRuleGroupsResponse' smart constructor.
data ListSubscribedRuleGroupsResponse = ListSubscribedRuleGroupsResponse'
  { -- | An array of 'RuleGroup' objects.
    ruleGroups :: Lude.Maybe [SubscribedRuleGroupSummary],
    -- | If you have more objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more objects, submit another @ListSubscribedRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscribedRuleGroupsResponse' with the minimum fields required to make a request.
--
-- * 'ruleGroups' - An array of 'RuleGroup' objects.
-- * 'nextMarker' - If you have more objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more objects, submit another @ListSubscribedRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListSubscribedRuleGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSubscribedRuleGroupsResponse
mkListSubscribedRuleGroupsResponse pResponseStatus_ =
  ListSubscribedRuleGroupsResponse'
    { ruleGroups = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'RuleGroup' objects.
--
-- /Note:/ Consider using 'ruleGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrsRuleGroups :: Lens.Lens' ListSubscribedRuleGroupsResponse (Lude.Maybe [SubscribedRuleGroupSummary])
lsrgrsRuleGroups = Lens.lens (ruleGroups :: ListSubscribedRuleGroupsResponse -> Lude.Maybe [SubscribedRuleGroupSummary]) (\s a -> s {ruleGroups = a} :: ListSubscribedRuleGroupsResponse)
{-# DEPRECATED lsrgrsRuleGroups "Use generic-lens or generic-optics with 'ruleGroups' instead." #-}

-- | If you have more objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more objects, submit another @ListSubscribedRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrsNextMarker :: Lens.Lens' ListSubscribedRuleGroupsResponse (Lude.Maybe Lude.Text)
lsrgrsNextMarker = Lens.lens (nextMarker :: ListSubscribedRuleGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListSubscribedRuleGroupsResponse)
{-# DEPRECATED lsrgrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrsResponseStatus :: Lens.Lens' ListSubscribedRuleGroupsResponse Lude.Int
lsrgrsResponseStatus = Lens.lens (responseStatus :: ListSubscribedRuleGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSubscribedRuleGroupsResponse)
{-# DEPRECATED lsrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
