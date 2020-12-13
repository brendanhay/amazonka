{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListRuleGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleGroup' objects.
module Network.AWS.WAFRegional.ListRuleGroups
  ( -- * Creating a request
    ListRuleGroups (..),
    mkListRuleGroups,

    -- ** Request lenses
    lrgNextMarker,
    lrgLimit,

    -- * Destructuring the response
    ListRuleGroupsResponse (..),
    mkListRuleGroupsResponse,

    -- ** Response lenses
    lrgrsRuleGroups,
    lrgrsNextMarker,
    lrgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListRuleGroups' smart constructor.
data ListRuleGroups = ListRuleGroups'
  { -- | If you specify a value for @Limit@ and you have more @RuleGroups@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RuleGroups@ . For the second and subsequent @ListRuleGroups@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RuleGroups@ .
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Specifies the number of @RuleGroups@ that you want AWS WAF to return for this request. If you have more @RuleGroups@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RuleGroups@ .
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRuleGroups' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @RuleGroups@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RuleGroups@ . For the second and subsequent @ListRuleGroups@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RuleGroups@ .
-- * 'limit' - Specifies the number of @RuleGroups@ that you want AWS WAF to return for this request. If you have more @RuleGroups@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RuleGroups@ .
mkListRuleGroups ::
  ListRuleGroups
mkListRuleGroups =
  ListRuleGroups' {nextMarker = Lude.Nothing, limit = Lude.Nothing}

-- | If you specify a value for @Limit@ and you have more @RuleGroups@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RuleGroups@ . For the second and subsequent @ListRuleGroups@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RuleGroups@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgNextMarker :: Lens.Lens' ListRuleGroups (Lude.Maybe Lude.Text)
lrgNextMarker = Lens.lens (nextMarker :: ListRuleGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRuleGroups)
{-# DEPRECATED lrgNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @RuleGroups@ that you want AWS WAF to return for this request. If you have more @RuleGroups@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RuleGroups@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgLimit :: Lens.Lens' ListRuleGroups (Lude.Maybe Lude.Natural)
lrgLimit = Lens.lens (limit :: ListRuleGroups -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListRuleGroups)
{-# DEPRECATED lrgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListRuleGroups where
  type Rs ListRuleGroups = ListRuleGroupsResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRuleGroupsResponse'
            Lude.<$> (x Lude..?> "RuleGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRuleGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.ListRuleGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRuleGroups where
  toJSON ListRuleGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListRuleGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRuleGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRuleGroupsResponse' smart constructor.
data ListRuleGroupsResponse = ListRuleGroupsResponse'
  { -- | An array of 'RuleGroup' objects.
    ruleGroups :: Lude.Maybe [RuleGroupSummary],
    -- | If you have more @RuleGroups@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RuleGroups@ , submit another @ListRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRuleGroupsResponse' with the minimum fields required to make a request.
--
-- * 'ruleGroups' - An array of 'RuleGroup' objects.
-- * 'nextMarker' - If you have more @RuleGroups@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RuleGroups@ , submit another @ListRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListRuleGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRuleGroupsResponse
mkListRuleGroupsResponse pResponseStatus_ =
  ListRuleGroupsResponse'
    { ruleGroups = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'RuleGroup' objects.
--
-- /Note:/ Consider using 'ruleGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrsRuleGroups :: Lens.Lens' ListRuleGroupsResponse (Lude.Maybe [RuleGroupSummary])
lrgrsRuleGroups = Lens.lens (ruleGroups :: ListRuleGroupsResponse -> Lude.Maybe [RuleGroupSummary]) (\s a -> s {ruleGroups = a} :: ListRuleGroupsResponse)
{-# DEPRECATED lrgrsRuleGroups "Use generic-lens or generic-optics with 'ruleGroups' instead." #-}

-- | If you have more @RuleGroups@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RuleGroups@ , submit another @ListRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrsNextMarker :: Lens.Lens' ListRuleGroupsResponse (Lude.Maybe Lude.Text)
lrgrsNextMarker = Lens.lens (nextMarker :: ListRuleGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRuleGroupsResponse)
{-# DEPRECATED lrgrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrsResponseStatus :: Lens.Lens' ListRuleGroupsResponse Lude.Int
lrgrsResponseStatus = Lens.lens (responseStatus :: ListRuleGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRuleGroupsResponse)
{-# DEPRECATED lrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
