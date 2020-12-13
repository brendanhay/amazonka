{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListWebACLs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'WebACLSummary' objects in the response.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListWebACLs
  ( -- * Creating a request
    ListWebACLs (..),
    mkListWebACLs,

    -- ** Request lenses
    lwaNextMarker,
    lwaLimit,

    -- * Destructuring the response
    ListWebACLsResponse (..),
    mkListWebACLsResponse,

    -- ** Response lenses
    lwarsWebACLs,
    lwarsNextMarker,
    lwarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkListWebACLs' smart constructor.
data ListWebACLs = ListWebACLs'
  { -- | If you specify a value for @Limit@ and you have more @WebACL@ objects than the number that you specify for @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @WebACL@ objects. For the second and subsequent @ListWebACLs@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @WebACL@ objects.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Specifies the number of @WebACL@ objects that you want AWS WAF to return for this request. If you have more @WebACL@ objects than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @WebACL@ objects.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWebACLs' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @WebACL@ objects than the number that you specify for @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @WebACL@ objects. For the second and subsequent @ListWebACLs@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @WebACL@ objects.
-- * 'limit' - Specifies the number of @WebACL@ objects that you want AWS WAF to return for this request. If you have more @WebACL@ objects than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @WebACL@ objects.
mkListWebACLs ::
  ListWebACLs
mkListWebACLs =
  ListWebACLs' {nextMarker = Lude.Nothing, limit = Lude.Nothing}

-- | If you specify a value for @Limit@ and you have more @WebACL@ objects than the number that you specify for @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @WebACL@ objects. For the second and subsequent @ListWebACLs@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @WebACL@ objects.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwaNextMarker :: Lens.Lens' ListWebACLs (Lude.Maybe Lude.Text)
lwaNextMarker = Lens.lens (nextMarker :: ListWebACLs -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListWebACLs)
{-# DEPRECATED lwaNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @WebACL@ objects that you want AWS WAF to return for this request. If you have more @WebACL@ objects than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @WebACL@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwaLimit :: Lens.Lens' ListWebACLs (Lude.Maybe Lude.Natural)
lwaLimit = Lens.lens (limit :: ListWebACLs -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListWebACLs)
{-# DEPRECATED lwaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListWebACLs where
  page rq rs
    | Page.stop (rs Lens.^. lwarsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lwarsWebACLs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lwaNextMarker Lens..~ rs Lens.^. lwarsNextMarker

instance Lude.AWSRequest ListWebACLs where
  type Rs ListWebACLs = ListWebACLsResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWebACLsResponse'
            Lude.<$> (x Lude..?> "WebACLs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListWebACLs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.ListWebACLs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListWebACLs where
  toJSON ListWebACLs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListWebACLs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListWebACLs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListWebACLsResponse' smart constructor.
data ListWebACLsResponse = ListWebACLsResponse'
  { -- | An array of 'WebACLSummary' objects.
    webACLs :: Lude.Maybe [WebACLSummary],
    -- | If you have more @WebACL@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @WebACL@ objects, submit another @ListWebACLs@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWebACLsResponse' with the minimum fields required to make a request.
--
-- * 'webACLs' - An array of 'WebACLSummary' objects.
-- * 'nextMarker' - If you have more @WebACL@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @WebACL@ objects, submit another @ListWebACLs@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListWebACLsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWebACLsResponse
mkListWebACLsResponse pResponseStatus_ =
  ListWebACLsResponse'
    { webACLs = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'WebACLSummary' objects.
--
-- /Note:/ Consider using 'webACLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwarsWebACLs :: Lens.Lens' ListWebACLsResponse (Lude.Maybe [WebACLSummary])
lwarsWebACLs = Lens.lens (webACLs :: ListWebACLsResponse -> Lude.Maybe [WebACLSummary]) (\s a -> s {webACLs = a} :: ListWebACLsResponse)
{-# DEPRECATED lwarsWebACLs "Use generic-lens or generic-optics with 'webACLs' instead." #-}

-- | If you have more @WebACL@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @WebACL@ objects, submit another @ListWebACLs@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwarsNextMarker :: Lens.Lens' ListWebACLsResponse (Lude.Maybe Lude.Text)
lwarsNextMarker = Lens.lens (nextMarker :: ListWebACLsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListWebACLsResponse)
{-# DEPRECATED lwarsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwarsResponseStatus :: Lens.Lens' ListWebACLsResponse Lude.Int
lwarsResponseStatus = Lens.lens (responseStatus :: ListWebACLsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWebACLsResponse)
{-# DEPRECATED lwarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
