{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListGeoMatchSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'GeoMatchSetSummary' objects in the response.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListGeoMatchSets
  ( -- * Creating a request
    ListGeoMatchSets (..),
    mkListGeoMatchSets,

    -- ** Request lenses
    lgmsNextMarker,
    lgmsLimit,

    -- * Destructuring the response
    ListGeoMatchSetsResponse (..),
    mkListGeoMatchSetsResponse,

    -- ** Response lenses
    lgmsrsGeoMatchSets,
    lgmsrsNextMarker,
    lgmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkListGeoMatchSets' smart constructor.
data ListGeoMatchSets = ListGeoMatchSets'
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

-- | Creates a value of 'ListGeoMatchSets' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to return for this request. If you have more @GeoMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @GeoMatchSet@ objects.
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @GeoMatchSet@ s than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @GeoMatchSet@ objects. For the second and subsequent @ListGeoMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @GeoMatchSet@ objects.
mkListGeoMatchSets ::
  ListGeoMatchSets
mkListGeoMatchSets =
  ListGeoMatchSets'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @GeoMatchSet@ s than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @GeoMatchSet@ objects. For the second and subsequent @ListGeoMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @GeoMatchSet@ objects.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmsNextMarker :: Lens.Lens' ListGeoMatchSets (Lude.Maybe Lude.Text)
lgmsNextMarker = Lens.lens (nextMarker :: ListGeoMatchSets -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListGeoMatchSets)
{-# DEPRECATED lgmsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to return for this request. If you have more @GeoMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @GeoMatchSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmsLimit :: Lens.Lens' ListGeoMatchSets (Lude.Maybe Lude.Natural)
lgmsLimit = Lens.lens (limit :: ListGeoMatchSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListGeoMatchSets)
{-# DEPRECATED lgmsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListGeoMatchSets where
  page rq rs
    | Page.stop (rs Lens.^. lgmsrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lgmsrsGeoMatchSets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgmsNextMarker Lens..~ rs Lens.^. lgmsrsNextMarker

instance Lude.AWSRequest ListGeoMatchSets where
  type Rs ListGeoMatchSets = ListGeoMatchSetsResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGeoMatchSetsResponse'
            Lude.<$> (x Lude..?> "GeoMatchSets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGeoMatchSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.ListGeoMatchSets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGeoMatchSets where
  toJSON ListGeoMatchSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListGeoMatchSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGeoMatchSets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGeoMatchSetsResponse' smart constructor.
data ListGeoMatchSetsResponse = ListGeoMatchSetsResponse'
  { geoMatchSets ::
      Lude.Maybe [GeoMatchSetSummary],
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

-- | Creates a value of 'ListGeoMatchSetsResponse' with the minimum fields required to make a request.
--
-- * 'geoMatchSets' - An array of 'GeoMatchSetSummary' objects.
-- * 'nextMarker' - If you have more @GeoMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListGeoMatchSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGeoMatchSetsResponse
mkListGeoMatchSetsResponse pResponseStatus_ =
  ListGeoMatchSetsResponse'
    { geoMatchSets = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'GeoMatchSetSummary' objects.
--
-- /Note:/ Consider using 'geoMatchSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmsrsGeoMatchSets :: Lens.Lens' ListGeoMatchSetsResponse (Lude.Maybe [GeoMatchSetSummary])
lgmsrsGeoMatchSets = Lens.lens (geoMatchSets :: ListGeoMatchSetsResponse -> Lude.Maybe [GeoMatchSetSummary]) (\s a -> s {geoMatchSets = a} :: ListGeoMatchSetsResponse)
{-# DEPRECATED lgmsrsGeoMatchSets "Use generic-lens or generic-optics with 'geoMatchSets' instead." #-}

-- | If you have more @GeoMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmsrsNextMarker :: Lens.Lens' ListGeoMatchSetsResponse (Lude.Maybe Lude.Text)
lgmsrsNextMarker = Lens.lens (nextMarker :: ListGeoMatchSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListGeoMatchSetsResponse)
{-# DEPRECATED lgmsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmsrsResponseStatus :: Lens.Lens' ListGeoMatchSetsResponse Lude.Int
lgmsrsResponseStatus = Lens.lens (responseStatus :: ListGeoMatchSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGeoMatchSetsResponse)
{-# DEPRECATED lgmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
