{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListRegexMatchSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RegexMatchSetSummary' objects.
module Network.AWS.WAFRegional.ListRegexMatchSets
  ( -- * Creating a request
    ListRegexMatchSets (..),
    mkListRegexMatchSets,

    -- ** Request lenses
    lrmsNextMarker,
    lrmsLimit,

    -- * Destructuring the response
    ListRegexMatchSetsResponse (..),
    mkListRegexMatchSetsResponse,

    -- ** Response lenses
    lrmsrsRegexMatchSets,
    lrmsrsNextMarker,
    lrmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListRegexMatchSets' smart constructor.
data ListRegexMatchSets = ListRegexMatchSets'
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

-- | Creates a value of 'ListRegexMatchSets' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies the number of @RegexMatchSet@ objects that you want AWS WAF to return for this request. If you have more @RegexMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexMatchSet@ objects.
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @RegexMatchSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListRegexMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexMatchSet@ objects.
mkListRegexMatchSets ::
  ListRegexMatchSets
mkListRegexMatchSets =
  ListRegexMatchSets'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @RegexMatchSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListRegexMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexMatchSet@ objects.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrmsNextMarker :: Lens.Lens' ListRegexMatchSets (Lude.Maybe Lude.Text)
lrmsNextMarker = Lens.lens (nextMarker :: ListRegexMatchSets -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRegexMatchSets)
{-# DEPRECATED lrmsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @RegexMatchSet@ objects that you want AWS WAF to return for this request. If you have more @RegexMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexMatchSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrmsLimit :: Lens.Lens' ListRegexMatchSets (Lude.Maybe Lude.Natural)
lrmsLimit = Lens.lens (limit :: ListRegexMatchSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListRegexMatchSets)
{-# DEPRECATED lrmsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListRegexMatchSets where
  type Rs ListRegexMatchSets = ListRegexMatchSetsResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRegexMatchSetsResponse'
            Lude.<$> (x Lude..?> "RegexMatchSets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRegexMatchSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.ListRegexMatchSets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRegexMatchSets where
  toJSON ListRegexMatchSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListRegexMatchSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRegexMatchSets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRegexMatchSetsResponse' smart constructor.
data ListRegexMatchSetsResponse = ListRegexMatchSetsResponse'
  { regexMatchSets ::
      Lude.Maybe [RegexMatchSetSummary],
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

-- | Creates a value of 'ListRegexMatchSetsResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you have more @RegexMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexMatchSet@ objects, submit another @ListRegexMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'regexMatchSets' - An array of 'RegexMatchSetSummary' objects.
-- * 'responseStatus' - The response status code.
mkListRegexMatchSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRegexMatchSetsResponse
mkListRegexMatchSetsResponse pResponseStatus_ =
  ListRegexMatchSetsResponse'
    { regexMatchSets = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'RegexMatchSetSummary' objects.
--
-- /Note:/ Consider using 'regexMatchSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrmsrsRegexMatchSets :: Lens.Lens' ListRegexMatchSetsResponse (Lude.Maybe [RegexMatchSetSummary])
lrmsrsRegexMatchSets = Lens.lens (regexMatchSets :: ListRegexMatchSetsResponse -> Lude.Maybe [RegexMatchSetSummary]) (\s a -> s {regexMatchSets = a} :: ListRegexMatchSetsResponse)
{-# DEPRECATED lrmsrsRegexMatchSets "Use generic-lens or generic-optics with 'regexMatchSets' instead." #-}

-- | If you have more @RegexMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexMatchSet@ objects, submit another @ListRegexMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrmsrsNextMarker :: Lens.Lens' ListRegexMatchSetsResponse (Lude.Maybe Lude.Text)
lrmsrsNextMarker = Lens.lens (nextMarker :: ListRegexMatchSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRegexMatchSetsResponse)
{-# DEPRECATED lrmsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrmsrsResponseStatus :: Lens.Lens' ListRegexMatchSetsResponse Lude.Int
lrmsrsResponseStatus = Lens.lens (responseStatus :: ListRegexMatchSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRegexMatchSetsResponse)
{-# DEPRECATED lrmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
