{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListByteMatchSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'ByteMatchSetSummary' objects.
module Network.AWS.WAFRegional.ListByteMatchSets
  ( -- * Creating a request
    ListByteMatchSets (..),
    mkListByteMatchSets,

    -- ** Request lenses
    lbmsNextMarker,
    lbmsLimit,

    -- * Destructuring the response
    ListByteMatchSetsResponse (..),
    mkListByteMatchSetsResponse,

    -- ** Response lenses
    lbmsrsByteMatchSets,
    lbmsrsNextMarker,
    lbmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListByteMatchSets' smart constructor.
data ListByteMatchSets = ListByteMatchSets'
  { -- | If you specify a value for @Limit@ and you have more @ByteMatchSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListByteMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ByteMatchSets@ .
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to return for this request. If you have more @ByteMatchSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ByteMatchSet@ objects.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListByteMatchSets' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @ByteMatchSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListByteMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ByteMatchSets@ .
-- * 'limit' - Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to return for this request. If you have more @ByteMatchSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ByteMatchSet@ objects.
mkListByteMatchSets ::
  ListByteMatchSets
mkListByteMatchSets =
  ListByteMatchSets'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @ByteMatchSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListByteMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ByteMatchSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsNextMarker :: Lens.Lens' ListByteMatchSets (Lude.Maybe Lude.Text)
lbmsNextMarker = Lens.lens (nextMarker :: ListByteMatchSets -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListByteMatchSets)
{-# DEPRECATED lbmsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to return for this request. If you have more @ByteMatchSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ByteMatchSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsLimit :: Lens.Lens' ListByteMatchSets (Lude.Maybe Lude.Natural)
lbmsLimit = Lens.lens (limit :: ListByteMatchSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListByteMatchSets)
{-# DEPRECATED lbmsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListByteMatchSets where
  type Rs ListByteMatchSets = ListByteMatchSetsResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListByteMatchSetsResponse'
            Lude.<$> (x Lude..?> "ByteMatchSets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListByteMatchSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.ListByteMatchSets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListByteMatchSets where
  toJSON ListByteMatchSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListByteMatchSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListByteMatchSets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListByteMatchSetsResponse' smart constructor.
data ListByteMatchSetsResponse = ListByteMatchSetsResponse'
  { -- | An array of 'ByteMatchSetSummary' objects.
    byteMatchSets :: Lude.Maybe [ByteMatchSetSummary],
    -- | If you have more @ByteMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another @ListByteMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListByteMatchSetsResponse' with the minimum fields required to make a request.
--
-- * 'byteMatchSets' - An array of 'ByteMatchSetSummary' objects.
-- * 'nextMarker' - If you have more @ByteMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another @ListByteMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListByteMatchSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListByteMatchSetsResponse
mkListByteMatchSetsResponse pResponseStatus_ =
  ListByteMatchSetsResponse'
    { byteMatchSets = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'ByteMatchSetSummary' objects.
--
-- /Note:/ Consider using 'byteMatchSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsrsByteMatchSets :: Lens.Lens' ListByteMatchSetsResponse (Lude.Maybe [ByteMatchSetSummary])
lbmsrsByteMatchSets = Lens.lens (byteMatchSets :: ListByteMatchSetsResponse -> Lude.Maybe [ByteMatchSetSummary]) (\s a -> s {byteMatchSets = a} :: ListByteMatchSetsResponse)
{-# DEPRECATED lbmsrsByteMatchSets "Use generic-lens or generic-optics with 'byteMatchSets' instead." #-}

-- | If you have more @ByteMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another @ListByteMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsrsNextMarker :: Lens.Lens' ListByteMatchSetsResponse (Lude.Maybe Lude.Text)
lbmsrsNextMarker = Lens.lens (nextMarker :: ListByteMatchSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListByteMatchSetsResponse)
{-# DEPRECATED lbmsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsrsResponseStatus :: Lens.Lens' ListByteMatchSetsResponse Lude.Int
lbmsrsResponseStatus = Lens.lens (responseStatus :: ListByteMatchSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListByteMatchSetsResponse)
{-# DEPRECATED lbmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
