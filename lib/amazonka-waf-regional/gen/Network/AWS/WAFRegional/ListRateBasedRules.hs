{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListRateBasedRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleSummary' objects.
module Network.AWS.WAFRegional.ListRateBasedRules
  ( -- * Creating a request
    ListRateBasedRules (..),
    mkListRateBasedRules,

    -- ** Request lenses
    lrbrNextMarker,
    lrbrLimit,

    -- * Destructuring the response
    ListRateBasedRulesResponse (..),
    mkListRateBasedRulesResponse,

    -- ** Response lenses
    lrbrrsRules,
    lrbrrsNextMarker,
    lrbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListRateBasedRules' smart constructor.
data ListRateBasedRules = ListRateBasedRules'
  { nextMarker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRateBasedRules' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies the number of @Rules@ that you want AWS WAF to return for this request. If you have more @Rules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @Rules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @Rules@ . For the second and subsequent @ListRateBasedRules@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @Rules@ .
mkListRateBasedRules ::
  ListRateBasedRules
mkListRateBasedRules =
  ListRateBasedRules'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @Rules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @Rules@ . For the second and subsequent @ListRateBasedRules@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @Rules@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrNextMarker :: Lens.Lens' ListRateBasedRules (Lude.Maybe Lude.Text)
lrbrNextMarker = Lens.lens (nextMarker :: ListRateBasedRules -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRateBasedRules)
{-# DEPRECATED lrbrNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @Rules@ that you want AWS WAF to return for this request. If you have more @Rules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrLimit :: Lens.Lens' ListRateBasedRules (Lude.Maybe Lude.Natural)
lrbrLimit = Lens.lens (limit :: ListRateBasedRules -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListRateBasedRules)
{-# DEPRECATED lrbrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListRateBasedRules where
  type Rs ListRateBasedRules = ListRateBasedRulesResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRateBasedRulesResponse'
            Lude.<$> (x Lude..?> "Rules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRateBasedRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.ListRateBasedRules" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRateBasedRules where
  toJSON ListRateBasedRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListRateBasedRules where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRateBasedRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRateBasedRulesResponse' smart constructor.
data ListRateBasedRulesResponse = ListRateBasedRulesResponse'
  { rules ::
      Lude.Maybe [RuleSummary],
    nextMarker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRateBasedRulesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you have more @Rules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @Rules@ , submit another @ListRateBasedRules@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
-- * 'rules' - An array of 'RuleSummary' objects.
mkListRateBasedRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRateBasedRulesResponse
mkListRateBasedRulesResponse pResponseStatus_ =
  ListRateBasedRulesResponse'
    { rules = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'RuleSummary' objects.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrrsRules :: Lens.Lens' ListRateBasedRulesResponse (Lude.Maybe [RuleSummary])
lrbrrsRules = Lens.lens (rules :: ListRateBasedRulesResponse -> Lude.Maybe [RuleSummary]) (\s a -> s {rules = a} :: ListRateBasedRulesResponse)
{-# DEPRECATED lrbrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | If you have more @Rules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @Rules@ , submit another @ListRateBasedRules@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrrsNextMarker :: Lens.Lens' ListRateBasedRulesResponse (Lude.Maybe Lude.Text)
lrbrrsNextMarker = Lens.lens (nextMarker :: ListRateBasedRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRateBasedRulesResponse)
{-# DEPRECATED lrbrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrrsResponseStatus :: Lens.Lens' ListRateBasedRulesResponse Lude.Int
lrbrrsResponseStatus = Lens.lens (responseStatus :: ListRateBasedRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRateBasedRulesResponse)
{-# DEPRECATED lrbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
