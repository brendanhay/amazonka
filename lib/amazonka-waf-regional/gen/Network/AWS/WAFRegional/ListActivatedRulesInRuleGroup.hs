{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListActivatedRulesInRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'ActivatedRule' objects.
module Network.AWS.WAFRegional.ListActivatedRulesInRuleGroup
  ( -- * Creating a request
    ListActivatedRulesInRuleGroup (..),
    mkListActivatedRulesInRuleGroup,

    -- ** Request lenses
    larirgRuleGroupId,
    larirgNextMarker,
    larirgLimit,

    -- * Destructuring the response
    ListActivatedRulesInRuleGroupResponse (..),
    mkListActivatedRulesInRuleGroupResponse,

    -- ** Response lenses
    larirgrsNextMarker,
    larirgrsActivatedRules,
    larirgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListActivatedRulesInRuleGroup' smart constructor.
data ListActivatedRulesInRuleGroup = ListActivatedRulesInRuleGroup'
  { ruleGroupId ::
      Lude.Maybe Lude.Text,
    nextMarker ::
      Lude.Maybe Lude.Text,
    limit ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActivatedRulesInRuleGroup' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies the number of @ActivatedRules@ that you want AWS WAF to return for this request. If you have more @ActivatedRules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ActivatedRules@ .
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @ActivatedRules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ActivatedRules@ . For the second and subsequent @ListActivatedRulesInRuleGroup@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ActivatedRules@ .
-- * 'ruleGroupId' - The @RuleGroupId@ of the 'RuleGroup' for which you want to get a list of 'ActivatedRule' objects.
mkListActivatedRulesInRuleGroup ::
  ListActivatedRulesInRuleGroup
mkListActivatedRulesInRuleGroup =
  ListActivatedRulesInRuleGroup'
    { ruleGroupId = Lude.Nothing,
      nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The @RuleGroupId@ of the 'RuleGroup' for which you want to get a list of 'ActivatedRule' objects.
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgRuleGroupId :: Lens.Lens' ListActivatedRulesInRuleGroup (Lude.Maybe Lude.Text)
larirgRuleGroupId = Lens.lens (ruleGroupId :: ListActivatedRulesInRuleGroup -> Lude.Maybe Lude.Text) (\s a -> s {ruleGroupId = a} :: ListActivatedRulesInRuleGroup)
{-# DEPRECATED larirgRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

-- | If you specify a value for @Limit@ and you have more @ActivatedRules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ActivatedRules@ . For the second and subsequent @ListActivatedRulesInRuleGroup@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ActivatedRules@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgNextMarker :: Lens.Lens' ListActivatedRulesInRuleGroup (Lude.Maybe Lude.Text)
larirgNextMarker = Lens.lens (nextMarker :: ListActivatedRulesInRuleGroup -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListActivatedRulesInRuleGroup)
{-# DEPRECATED larirgNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @ActivatedRules@ that you want AWS WAF to return for this request. If you have more @ActivatedRules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ActivatedRules@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgLimit :: Lens.Lens' ListActivatedRulesInRuleGroup (Lude.Maybe Lude.Natural)
larirgLimit = Lens.lens (limit :: ListActivatedRulesInRuleGroup -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListActivatedRulesInRuleGroup)
{-# DEPRECATED larirgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListActivatedRulesInRuleGroup where
  type
    Rs ListActivatedRulesInRuleGroup =
      ListActivatedRulesInRuleGroupResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListActivatedRulesInRuleGroupResponse'
            Lude.<$> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "ActivatedRules" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListActivatedRulesInRuleGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.ListActivatedRulesInRuleGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListActivatedRulesInRuleGroup where
  toJSON ListActivatedRulesInRuleGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RuleGroupId" Lude..=) Lude.<$> ruleGroupId,
            ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListActivatedRulesInRuleGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ListActivatedRulesInRuleGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListActivatedRulesInRuleGroupResponse' smart constructor.
data ListActivatedRulesInRuleGroupResponse = ListActivatedRulesInRuleGroupResponse'
  { nextMarker ::
      Lude.Maybe
        Lude.Text,
    activatedRules ::
      Lude.Maybe
        [ActivatedRule],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActivatedRulesInRuleGroupResponse' with the minimum fields required to make a request.
--
-- * 'activatedRules' - An array of @ActivatedRules@ objects.
-- * 'nextMarker' - If you have more @ActivatedRules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ActivatedRules@ , submit another @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListActivatedRulesInRuleGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListActivatedRulesInRuleGroupResponse
mkListActivatedRulesInRuleGroupResponse pResponseStatus_ =
  ListActivatedRulesInRuleGroupResponse'
    { nextMarker = Lude.Nothing,
      activatedRules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If you have more @ActivatedRules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ActivatedRules@ , submit another @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgrsNextMarker :: Lens.Lens' ListActivatedRulesInRuleGroupResponse (Lude.Maybe Lude.Text)
larirgrsNextMarker = Lens.lens (nextMarker :: ListActivatedRulesInRuleGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListActivatedRulesInRuleGroupResponse)
{-# DEPRECATED larirgrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | An array of @ActivatedRules@ objects.
--
-- /Note:/ Consider using 'activatedRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgrsActivatedRules :: Lens.Lens' ListActivatedRulesInRuleGroupResponse (Lude.Maybe [ActivatedRule])
larirgrsActivatedRules = Lens.lens (activatedRules :: ListActivatedRulesInRuleGroupResponse -> Lude.Maybe [ActivatedRule]) (\s a -> s {activatedRules = a} :: ListActivatedRulesInRuleGroupResponse)
{-# DEPRECATED larirgrsActivatedRules "Use generic-lens or generic-optics with 'activatedRules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgrsResponseStatus :: Lens.Lens' ListActivatedRulesInRuleGroupResponse Lude.Int
larirgrsResponseStatus = Lens.lens (responseStatus :: ListActivatedRulesInRuleGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListActivatedRulesInRuleGroupResponse)
{-# DEPRECATED larirgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
