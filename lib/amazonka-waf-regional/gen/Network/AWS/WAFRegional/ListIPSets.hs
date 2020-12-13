{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListIPSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'IPSetSummary' objects in the response.
module Network.AWS.WAFRegional.ListIPSets
  ( -- * Creating a request
    ListIPSets (..),
    mkListIPSets,

    -- ** Request lenses
    lisNextMarker,
    lisLimit,

    -- * Destructuring the response
    ListIPSetsResponse (..),
    mkListIPSetsResponse,

    -- ** Response lenses
    lisrsNextMarker,
    lisrsIPSets,
    lisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListIPSets' smart constructor.
data ListIPSets = ListIPSets'
  { -- | AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @IPSets@ . For the second and subsequent @ListIPSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @IPSets@ .
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Specifies the number of @IPSet@ objects that you want AWS WAF to return for this request. If you have more @IPSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @IPSet@ objects.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIPSets' with the minimum fields required to make a request.
--
-- * 'nextMarker' - AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @IPSets@ . For the second and subsequent @ListIPSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @IPSets@ .
-- * 'limit' - Specifies the number of @IPSet@ objects that you want AWS WAF to return for this request. If you have more @IPSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @IPSet@ objects.
mkListIPSets ::
  ListIPSets
mkListIPSets =
  ListIPSets' {nextMarker = Lude.Nothing, limit = Lude.Nothing}

-- | AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @IPSets@ . For the second and subsequent @ListIPSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @IPSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisNextMarker :: Lens.Lens' ListIPSets (Lude.Maybe Lude.Text)
lisNextMarker = Lens.lens (nextMarker :: ListIPSets -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListIPSets)
{-# DEPRECATED lisNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @IPSet@ objects that you want AWS WAF to return for this request. If you have more @IPSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @IPSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisLimit :: Lens.Lens' ListIPSets (Lude.Maybe Lude.Natural)
lisLimit = Lens.lens (limit :: ListIPSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListIPSets)
{-# DEPRECATED lisLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListIPSets where
  type Rs ListIPSets = ListIPSetsResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIPSetsResponse'
            Lude.<$> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "IPSets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIPSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.ListIPSets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListIPSets where
  toJSON ListIPSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListIPSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListIPSets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { -- | To list more @IPSet@ objects, submit another @ListIPSets@ request, and in the next request use the @NextMarker@ response value as the @NextMarker@ value.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | An array of 'IPSetSummary' objects.
    ipSets :: Lude.Maybe [IPSetSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIPSetsResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - To list more @IPSet@ objects, submit another @ListIPSets@ request, and in the next request use the @NextMarker@ response value as the @NextMarker@ value.
-- * 'ipSets' - An array of 'IPSetSummary' objects.
-- * 'responseStatus' - The response status code.
mkListIPSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIPSetsResponse
mkListIPSetsResponse pResponseStatus_ =
  ListIPSetsResponse'
    { nextMarker = Lude.Nothing,
      ipSets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | To list more @IPSet@ objects, submit another @ListIPSets@ request, and in the next request use the @NextMarker@ response value as the @NextMarker@ value.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisrsNextMarker :: Lens.Lens' ListIPSetsResponse (Lude.Maybe Lude.Text)
lisrsNextMarker = Lens.lens (nextMarker :: ListIPSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListIPSetsResponse)
{-# DEPRECATED lisrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | An array of 'IPSetSummary' objects.
--
-- /Note:/ Consider using 'ipSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisrsIPSets :: Lens.Lens' ListIPSetsResponse (Lude.Maybe [IPSetSummary])
lisrsIPSets = Lens.lens (ipSets :: ListIPSetsResponse -> Lude.Maybe [IPSetSummary]) (\s a -> s {ipSets = a} :: ListIPSetsResponse)
{-# DEPRECATED lisrsIPSets "Use generic-lens or generic-optics with 'ipSets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisrsResponseStatus :: Lens.Lens' ListIPSetsResponse Lude.Int
lisrsResponseStatus = Lens.lens (responseStatus :: ListIPSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIPSetsResponse)
{-# DEPRECATED lisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
