{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListXSSMatchSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'XssMatchSet' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListXSSMatchSets
  ( -- * Creating a request
    ListXSSMatchSets (..),
    mkListXSSMatchSets,

    -- ** Request lenses
    lxmsNextMarker,
    lxmsLimit,

    -- * Destructuring the response
    ListXSSMatchSetsResponse (..),
    mkListXSSMatchSetsResponse,

    -- ** Response lenses
    lxmsrsXSSMatchSets,
    lxmsrsNextMarker,
    lxmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | A request to list the 'XssMatchSet' objects created by the current AWS account.
--
-- /See:/ 'mkListXSSMatchSets' smart constructor.
data ListXSSMatchSets = ListXSSMatchSets'
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

-- | Creates a value of 'ListXSSMatchSets' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies the number of 'XssMatchSet' objects that you want AWS WAF to return for this request. If you have more @XssMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more 'XssMatchSet' objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @XssMatchSets@ . For the second and subsequent @ListXssMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @XssMatchSets@ .
mkListXSSMatchSets ::
  ListXSSMatchSets
mkListXSSMatchSets =
  ListXSSMatchSets'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more 'XssMatchSet' objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @XssMatchSets@ . For the second and subsequent @ListXssMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @XssMatchSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsNextMarker :: Lens.Lens' ListXSSMatchSets (Lude.Maybe Lude.Text)
lxmsNextMarker = Lens.lens (nextMarker :: ListXSSMatchSets -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListXSSMatchSets)
{-# DEPRECATED lxmsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of 'XssMatchSet' objects that you want AWS WAF to return for this request. If you have more @XssMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsLimit :: Lens.Lens' ListXSSMatchSets (Lude.Maybe Lude.Natural)
lxmsLimit = Lens.lens (limit :: ListXSSMatchSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListXSSMatchSets)
{-# DEPRECATED lxmsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListXSSMatchSets where
  page rq rs
    | Page.stop (rs Lens.^. lxmsrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lxmsrsXSSMatchSets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lxmsNextMarker Lens..~ rs Lens.^. lxmsrsNextMarker

instance Lude.AWSRequest ListXSSMatchSets where
  type Rs ListXSSMatchSets = ListXSSMatchSetsResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListXSSMatchSetsResponse'
            Lude.<$> (x Lude..?> "XssMatchSets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListXSSMatchSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.ListXssMatchSets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListXSSMatchSets where
  toJSON ListXSSMatchSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListXSSMatchSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListXSSMatchSets where
  toQuery = Lude.const Lude.mempty

-- | The response to a 'ListXssMatchSets' request.
--
-- /See:/ 'mkListXSSMatchSetsResponse' smart constructor.
data ListXSSMatchSetsResponse = ListXSSMatchSetsResponse'
  { xssMatchSets ::
      Lude.Maybe [XSSMatchSetSummary],
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

-- | Creates a value of 'ListXSSMatchSetsResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you have more 'XssMatchSet' objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
-- * 'xssMatchSets' - An array of 'XssMatchSetSummary' objects.
mkListXSSMatchSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListXSSMatchSetsResponse
mkListXSSMatchSetsResponse pResponseStatus_ =
  ListXSSMatchSetsResponse'
    { xssMatchSets = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'XssMatchSetSummary' objects.
--
-- /Note:/ Consider using 'xssMatchSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsrsXSSMatchSets :: Lens.Lens' ListXSSMatchSetsResponse (Lude.Maybe [XSSMatchSetSummary])
lxmsrsXSSMatchSets = Lens.lens (xssMatchSets :: ListXSSMatchSetsResponse -> Lude.Maybe [XSSMatchSetSummary]) (\s a -> s {xssMatchSets = a} :: ListXSSMatchSetsResponse)
{-# DEPRECATED lxmsrsXSSMatchSets "Use generic-lens or generic-optics with 'xssMatchSets' instead." #-}

-- | If you have more 'XssMatchSet' objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsrsNextMarker :: Lens.Lens' ListXSSMatchSetsResponse (Lude.Maybe Lude.Text)
lxmsrsNextMarker = Lens.lens (nextMarker :: ListXSSMatchSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListXSSMatchSetsResponse)
{-# DEPRECATED lxmsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsrsResponseStatus :: Lens.Lens' ListXSSMatchSetsResponse Lude.Int
lxmsrsResponseStatus = Lens.lens (responseStatus :: ListXSSMatchSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListXSSMatchSetsResponse)
{-# DEPRECATED lxmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
