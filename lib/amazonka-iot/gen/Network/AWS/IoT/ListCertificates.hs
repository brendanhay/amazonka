{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the certificates registered in your AWS account.
--
-- The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCertificates
  ( -- * Creating a request
    ListCertificates (..),
    mkListCertificates,

    -- ** Request lenses
    lcMarker,
    lcAscendingOrder,
    lcPageSize,

    -- * Destructuring the response
    ListCertificatesResponse (..),
    mkListCertificatesResponse,

    -- ** Response lenses
    lcrsCertificates,
    lcrsNextMarker,
    lcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ListCertificates operation.
--
-- /See:/ 'mkListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListCertificates' with the minimum fields required to make a request.
--
-- * 'ascendingOrder' - Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
-- * 'marker' - The marker for the next set of results.
-- * 'pageSize' - The result page size.
mkListCertificates ::
  ListCertificates
mkListCertificates =
  ListCertificates'
    { marker = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMarker :: Lens.Lens' ListCertificates (Lude.Maybe Lude.Text)
lcMarker = Lens.lens (marker :: ListCertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListCertificates)
{-# DEPRECATED lcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcAscendingOrder :: Lens.Lens' ListCertificates (Lude.Maybe Lude.Bool)
lcAscendingOrder = Lens.lens (ascendingOrder :: ListCertificates -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListCertificates)
{-# DEPRECATED lcAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcPageSize :: Lens.Lens' ListCertificates (Lude.Maybe Lude.Natural)
lcPageSize = Lens.lens (pageSize :: ListCertificates -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListCertificates)
{-# DEPRECATED lcPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListCertificates where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsCertificates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcMarker Lens..~ rs Lens.^. lcrsNextMarker

instance Lude.AWSRequest ListCertificates where
  type Rs ListCertificates = ListCertificatesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Lude.<$> (x Lude..?> "certificates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListCertificates where
  toPath = Lude.const "/certificates"

instance Lude.ToQuery ListCertificates where
  toQuery ListCertificates' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "isAscendingOrder" Lude.=: ascendingOrder,
        "pageSize" Lude.=: pageSize
      ]

-- | The output of the ListCertificates operation.
--
-- /See:/ 'mkListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { certificates ::
      Lude.Maybe [Certificate],
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

-- | Creates a value of 'ListCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - The descriptions of the certificates.
-- * 'nextMarker' - The marker for the next set of results, or null if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCertificatesResponse
mkListCertificatesResponse pResponseStatus_ =
  ListCertificatesResponse'
    { certificates = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The descriptions of the certificates.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsCertificates :: Lens.Lens' ListCertificatesResponse (Lude.Maybe [Certificate])
lcrsCertificates = Lens.lens (certificates :: ListCertificatesResponse -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The marker for the next set of results, or null if there are no additional results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextMarker :: Lens.Lens' ListCertificatesResponse (Lude.Maybe Lude.Text)
lcrsNextMarker = Lens.lens (nextMarker :: ListCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListCertificatesResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
