{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your policies.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPolicies
  ( -- * Creating a request
    ListPolicies (..),
    mkListPolicies,

    -- ** Request lenses
    lpMarker,
    lpAscendingOrder,
    lpPageSize,

    -- * Destructuring the response
    ListPoliciesResponse (..),
    mkListPoliciesResponse,

    -- ** Response lenses
    lprsNextMarker,
    lprsPolicies,
    lprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ListPolicies operation.
--
-- /See:/ 'mkListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { marker :: Lude.Maybe Lude.Text,
    ascendingOrder :: Lude.Maybe Lude.Bool,
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicies' with the minimum fields required to make a request.
--
-- * 'ascendingOrder' - Specifies the order for results. If true, the results are returned in ascending creation order.
-- * 'marker' - The marker for the next set of results.
-- * 'pageSize' - The result page size.
mkListPolicies ::
  ListPolicies
mkListPolicies =
  ListPolicies'
    { marker = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMarker :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Text)
lpMarker = Lens.lens (marker :: ListPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPolicies)
{-# DEPRECATED lpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies the order for results. If true, the results are returned in ascending creation order.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpAscendingOrder :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Bool)
lpAscendingOrder = Lens.lens (ascendingOrder :: ListPolicies -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListPolicies)
{-# DEPRECATED lpAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageSize :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Natural)
lpPageSize = Lens.lens (pageSize :: ListPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListPolicies)
{-# DEPRECATED lpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListPolicies where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpMarker Lens..~ rs Lens.^. lprsNextMarker

instance Lude.AWSRequest ListPolicies where
  type Rs ListPolicies = ListPoliciesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Lude.<$> (x Lude..?> "nextMarker")
            Lude.<*> (x Lude..?> "policies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPolicies where
  toPath = Lude.const "/policies"

instance Lude.ToQuery ListPolicies where
  toQuery ListPolicies' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "isAscendingOrder" Lude.=: ascendingOrder,
        "pageSize" Lude.=: pageSize
      ]

-- | The output from the ListPolicies operation.
--
-- /See:/ 'mkListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { nextMarker ::
      Lude.Maybe Lude.Text,
    policies :: Lude.Maybe [Policy],
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

-- | Creates a value of 'ListPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - The marker for the next set of results, or null if there are no additional results.
-- * 'policies' - The descriptions of the policies.
-- * 'responseStatus' - The response status code.
mkListPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPoliciesResponse
mkListPoliciesResponse pResponseStatus_ =
  ListPoliciesResponse'
    { nextMarker = Lude.Nothing,
      policies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The marker for the next set of results, or null if there are no additional results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextMarker :: Lens.Lens' ListPoliciesResponse (Lude.Maybe Lude.Text)
lprsNextMarker = Lens.lens (nextMarker :: ListPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The descriptions of the policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPolicies :: Lens.Lens' ListPoliciesResponse (Lude.Maybe [Policy])
lprsPolicies = Lens.lens (policies :: ListPoliciesResponse -> Lude.Maybe [Policy]) (\s a -> s {policies = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPoliciesResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
