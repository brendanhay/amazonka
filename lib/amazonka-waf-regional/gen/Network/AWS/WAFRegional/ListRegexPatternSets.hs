{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListRegexPatternSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RegexPatternSetSummary' objects.
module Network.AWS.WAFRegional.ListRegexPatternSets
  ( -- * Creating a request
    ListRegexPatternSets (..),
    mkListRegexPatternSets,

    -- ** Request lenses
    lrpsNextMarker,
    lrpsLimit,

    -- * Destructuring the response
    ListRegexPatternSetsResponse (..),
    mkListRegexPatternSetsResponse,

    -- ** Response lenses
    lrpsrsRegexPatternSets,
    lrpsrsNextMarker,
    lrpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListRegexPatternSets' smart constructor.
data ListRegexPatternSets = ListRegexPatternSets'
  { -- | If you specify a value for @Limit@ and you have more @RegexPatternSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RegexPatternSet@ objects. For the second and subsequent @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexPatternSet@ objects.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Specifies the number of @RegexPatternSet@ objects that you want AWS WAF to return for this request. If you have more @RegexPatternSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexPatternSet@ objects.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRegexPatternSets' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @RegexPatternSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RegexPatternSet@ objects. For the second and subsequent @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexPatternSet@ objects.
-- * 'limit' - Specifies the number of @RegexPatternSet@ objects that you want AWS WAF to return for this request. If you have more @RegexPatternSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexPatternSet@ objects.
mkListRegexPatternSets ::
  ListRegexPatternSets
mkListRegexPatternSets =
  ListRegexPatternSets'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @RegexPatternSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RegexPatternSet@ objects. For the second and subsequent @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexPatternSet@ objects.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsNextMarker :: Lens.Lens' ListRegexPatternSets (Lude.Maybe Lude.Text)
lrpsNextMarker = Lens.lens (nextMarker :: ListRegexPatternSets -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRegexPatternSets)
{-# DEPRECATED lrpsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @RegexPatternSet@ objects that you want AWS WAF to return for this request. If you have more @RegexPatternSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexPatternSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsLimit :: Lens.Lens' ListRegexPatternSets (Lude.Maybe Lude.Natural)
lrpsLimit = Lens.lens (limit :: ListRegexPatternSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListRegexPatternSets)
{-# DEPRECATED lrpsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListRegexPatternSets where
  type Rs ListRegexPatternSets = ListRegexPatternSetsResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRegexPatternSetsResponse'
            Lude.<$> (x Lude..?> "RegexPatternSets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRegexPatternSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.ListRegexPatternSets" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRegexPatternSets where
  toJSON ListRegexPatternSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListRegexPatternSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRegexPatternSets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRegexPatternSetsResponse' smart constructor.
data ListRegexPatternSetsResponse = ListRegexPatternSetsResponse'
  { -- | An array of 'RegexPatternSetSummary' objects.
    regexPatternSets :: Lude.Maybe [RegexPatternSetSummary],
    -- | If you have more @RegexPatternSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexPatternSet@ objects, submit another @ListRegexPatternSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRegexPatternSetsResponse' with the minimum fields required to make a request.
--
-- * 'regexPatternSets' - An array of 'RegexPatternSetSummary' objects.
-- * 'nextMarker' - If you have more @RegexPatternSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexPatternSet@ objects, submit another @ListRegexPatternSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListRegexPatternSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRegexPatternSetsResponse
mkListRegexPatternSetsResponse pResponseStatus_ =
  ListRegexPatternSetsResponse'
    { regexPatternSets = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'RegexPatternSetSummary' objects.
--
-- /Note:/ Consider using 'regexPatternSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsrsRegexPatternSets :: Lens.Lens' ListRegexPatternSetsResponse (Lude.Maybe [RegexPatternSetSummary])
lrpsrsRegexPatternSets = Lens.lens (regexPatternSets :: ListRegexPatternSetsResponse -> Lude.Maybe [RegexPatternSetSummary]) (\s a -> s {regexPatternSets = a} :: ListRegexPatternSetsResponse)
{-# DEPRECATED lrpsrsRegexPatternSets "Use generic-lens or generic-optics with 'regexPatternSets' instead." #-}

-- | If you have more @RegexPatternSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexPatternSet@ objects, submit another @ListRegexPatternSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsrsNextMarker :: Lens.Lens' ListRegexPatternSetsResponse (Lude.Maybe Lude.Text)
lrpsrsNextMarker = Lens.lens (nextMarker :: ListRegexPatternSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRegexPatternSetsResponse)
{-# DEPRECATED lrpsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsrsResponseStatus :: Lens.Lens' ListRegexPatternSetsResponse Lude.Int
lrpsrsResponseStatus = Lens.lens (responseStatus :: ListRegexPatternSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRegexPatternSetsResponse)
{-# DEPRECATED lrpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
