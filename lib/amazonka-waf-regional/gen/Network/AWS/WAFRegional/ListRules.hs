{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleSummary' objects.
module Network.AWS.WAFRegional.ListRules
  ( -- * Creating a request
    ListRules (..),
    mkListRules,

    -- ** Request lenses
    lrNextMarker,
    lrLimit,

    -- * Destructuring the response
    ListRulesResponse (..),
    mkListRulesResponse,

    -- ** Response lenses
    lrrsRules,
    lrrsNextMarker,
    lrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListRules' smart constructor.
data ListRules = ListRules'
  { nextMarker :: Lude.Maybe Lude.Text,
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
-- * 'limit' - Specifies the number of @Rules@ that you want AWS WAF to return for this request. If you have more @Rules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @Rules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @Rules@ . For the second and subsequent @ListRules@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @Rules@ .
mkListRules ::
  ListRules
mkListRules =
  ListRules' {nextMarker = Lude.Nothing, limit = Lude.Nothing}

-- | If you specify a value for @Limit@ and you have more @Rules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @Rules@ . For the second and subsequent @ListRules@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @Rules@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextMarker :: Lens.Lens' ListRules (Lude.Maybe Lude.Text)
lrNextMarker = Lens.lens (nextMarker :: ListRules -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRules)
{-# DEPRECATED lrNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @Rules@ that you want AWS WAF to return for this request. If you have more @Rules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLimit :: Lens.Lens' ListRules (Lude.Maybe Lude.Natural)
lrLimit = Lens.lens (limit :: ListRules -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListRules)
{-# DEPRECATED lrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListRules where
  type Rs ListRules = ListRulesResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Lude.<$> (x Lude..?> "Rules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.ListRules" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRules where
  toJSON ListRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
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

-- | Creates a value of 'ListRulesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you have more @Rules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @Rules@ , submit another @ListRules@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
-- * 'rules' - An array of 'RuleSummary' objects.
mkListRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRulesResponse
mkListRulesResponse pResponseStatus_ =
  ListRulesResponse'
    { rules = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'RuleSummary' objects.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsRules :: Lens.Lens' ListRulesResponse (Lude.Maybe [RuleSummary])
lrrsRules = Lens.lens (rules :: ListRulesResponse -> Lude.Maybe [RuleSummary]) (\s a -> s {rules = a} :: ListRulesResponse)
{-# DEPRECATED lrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | If you have more @Rules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @Rules@ , submit another @ListRules@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextMarker :: Lens.Lens' ListRulesResponse (Lude.Maybe Lude.Text)
lrrsNextMarker = Lens.lens (nextMarker :: ListRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRulesResponse)
{-# DEPRECATED lrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListRulesResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRulesResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
