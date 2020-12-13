{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListSizeConstraintSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'SizeConstraintSetSummary' objects.
module Network.AWS.WAFRegional.ListSizeConstraintSets
  ( -- * Creating a request
    ListSizeConstraintSets (..),
    mkListSizeConstraintSets,

    -- ** Request lenses
    lscsNextMarker,
    lscsLimit,

    -- * Destructuring the response
    ListSizeConstraintSetsResponse (..),
    mkListSizeConstraintSetsResponse,

    -- ** Response lenses
    lscsrsSizeConstraintSets,
    lscsrsNextMarker,
    lscsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListSizeConstraintSets' smart constructor.
data ListSizeConstraintSets = ListSizeConstraintSets'
  { -- | If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSizeConstraintSets' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
-- * 'limit' - Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
mkListSizeConstraintSets ::
  ListSizeConstraintSets
mkListSizeConstraintSets =
  ListSizeConstraintSets'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsNextMarker :: Lens.Lens' ListSizeConstraintSets (Lude.Maybe Lude.Text)
lscsNextMarker = Lens.lens (nextMarker :: ListSizeConstraintSets -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListSizeConstraintSets)
{-# DEPRECATED lscsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsLimit :: Lens.Lens' ListSizeConstraintSets (Lude.Maybe Lude.Natural)
lscsLimit = Lens.lens (limit :: ListSizeConstraintSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListSizeConstraintSets)
{-# DEPRECATED lscsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListSizeConstraintSets where
  type Rs ListSizeConstraintSets = ListSizeConstraintSetsResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSizeConstraintSetsResponse'
            Lude.<$> (x Lude..?> "SizeConstraintSets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSizeConstraintSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.ListSizeConstraintSets" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSizeConstraintSets where
  toJSON ListSizeConstraintSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListSizeConstraintSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSizeConstraintSets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSizeConstraintSetsResponse' smart constructor.
data ListSizeConstraintSetsResponse = ListSizeConstraintSetsResponse'
  { -- | An array of 'SizeConstraintSetSummary' objects.
    sizeConstraintSets :: Lude.Maybe [SizeConstraintSetSummary],
    -- | If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSizeConstraintSetsResponse' with the minimum fields required to make a request.
--
-- * 'sizeConstraintSets' - An array of 'SizeConstraintSetSummary' objects.
-- * 'nextMarker' - If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListSizeConstraintSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSizeConstraintSetsResponse
mkListSizeConstraintSetsResponse pResponseStatus_ =
  ListSizeConstraintSetsResponse'
    { sizeConstraintSets =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'SizeConstraintSetSummary' objects.
--
-- /Note:/ Consider using 'sizeConstraintSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrsSizeConstraintSets :: Lens.Lens' ListSizeConstraintSetsResponse (Lude.Maybe [SizeConstraintSetSummary])
lscsrsSizeConstraintSets = Lens.lens (sizeConstraintSets :: ListSizeConstraintSetsResponse -> Lude.Maybe [SizeConstraintSetSummary]) (\s a -> s {sizeConstraintSets = a} :: ListSizeConstraintSetsResponse)
{-# DEPRECATED lscsrsSizeConstraintSets "Use generic-lens or generic-optics with 'sizeConstraintSets' instead." #-}

-- | If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrsNextMarker :: Lens.Lens' ListSizeConstraintSetsResponse (Lude.Maybe Lude.Text)
lscsrsNextMarker = Lens.lens (nextMarker :: ListSizeConstraintSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListSizeConstraintSetsResponse)
{-# DEPRECATED lscsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrsResponseStatus :: Lens.Lens' ListSizeConstraintSetsResponse Lude.Int
lscsrsResponseStatus = Lens.lens (responseStatus :: ListSizeConstraintSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSizeConstraintSetsResponse)
{-# DEPRECATED lscsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
