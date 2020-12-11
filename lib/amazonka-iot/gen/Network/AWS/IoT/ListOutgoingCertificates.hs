{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListOutgoingCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists certificates that are being transferred but not yet accepted.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListOutgoingCertificates
  ( -- * Creating a request
    ListOutgoingCertificates (..),
    mkListOutgoingCertificates,

    -- ** Request lenses
    locMarker,
    locAscendingOrder,
    locPageSize,

    -- * Destructuring the response
    ListOutgoingCertificatesResponse (..),
    mkListOutgoingCertificatesResponse,

    -- ** Response lenses
    locrsNextMarker,
    locrsOutgoingCertificates,
    locrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input to the ListOutgoingCertificates operation.
--
-- /See:/ 'mkListOutgoingCertificates' smart constructor.
data ListOutgoingCertificates = ListOutgoingCertificates'
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

-- | Creates a value of 'ListOutgoingCertificates' with the minimum fields required to make a request.
--
-- * 'ascendingOrder' - Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
-- * 'marker' - The marker for the next set of results.
-- * 'pageSize' - The result page size.
mkListOutgoingCertificates ::
  ListOutgoingCertificates
mkListOutgoingCertificates =
  ListOutgoingCertificates'
    { marker = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locMarker :: Lens.Lens' ListOutgoingCertificates (Lude.Maybe Lude.Text)
locMarker = Lens.lens (marker :: ListOutgoingCertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListOutgoingCertificates)
{-# DEPRECATED locMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locAscendingOrder :: Lens.Lens' ListOutgoingCertificates (Lude.Maybe Lude.Bool)
locAscendingOrder = Lens.lens (ascendingOrder :: ListOutgoingCertificates -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListOutgoingCertificates)
{-# DEPRECATED locAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locPageSize :: Lens.Lens' ListOutgoingCertificates (Lude.Maybe Lude.Natural)
locPageSize = Lens.lens (pageSize :: ListOutgoingCertificates -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListOutgoingCertificates)
{-# DEPRECATED locPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListOutgoingCertificates where
  page rq rs
    | Page.stop (rs Lens.^. locrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. locrsOutgoingCertificates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& locMarker Lens..~ rs Lens.^. locrsNextMarker

instance Lude.AWSRequest ListOutgoingCertificates where
  type Rs ListOutgoingCertificates = ListOutgoingCertificatesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOutgoingCertificatesResponse'
            Lude.<$> (x Lude..?> "nextMarker")
            Lude.<*> (x Lude..?> "outgoingCertificates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOutgoingCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListOutgoingCertificates where
  toPath = Lude.const "/certificates-out-going"

instance Lude.ToQuery ListOutgoingCertificates where
  toQuery ListOutgoingCertificates' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "isAscendingOrder" Lude.=: ascendingOrder,
        "pageSize" Lude.=: pageSize
      ]

-- | The output from the ListOutgoingCertificates operation.
--
-- /See:/ 'mkListOutgoingCertificatesResponse' smart constructor.
data ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse'
  { nextMarker ::
      Lude.Maybe Lude.Text,
    outgoingCertificates ::
      Lude.Maybe
        [OutgoingCertificate],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOutgoingCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - The marker for the next set of results.
-- * 'outgoingCertificates' - The certificates that are being transferred but not yet accepted.
-- * 'responseStatus' - The response status code.
mkListOutgoingCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOutgoingCertificatesResponse
mkListOutgoingCertificatesResponse pResponseStatus_ =
  ListOutgoingCertificatesResponse'
    { nextMarker = Lude.Nothing,
      outgoingCertificates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrsNextMarker :: Lens.Lens' ListOutgoingCertificatesResponse (Lude.Maybe Lude.Text)
locrsNextMarker = Lens.lens (nextMarker :: ListOutgoingCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListOutgoingCertificatesResponse)
{-# DEPRECATED locrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The certificates that are being transferred but not yet accepted.
--
-- /Note:/ Consider using 'outgoingCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrsOutgoingCertificates :: Lens.Lens' ListOutgoingCertificatesResponse (Lude.Maybe [OutgoingCertificate])
locrsOutgoingCertificates = Lens.lens (outgoingCertificates :: ListOutgoingCertificatesResponse -> Lude.Maybe [OutgoingCertificate]) (\s a -> s {outgoingCertificates = a} :: ListOutgoingCertificatesResponse)
{-# DEPRECATED locrsOutgoingCertificates "Use generic-lens or generic-optics with 'outgoingCertificates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrsResponseStatus :: Lens.Lens' ListOutgoingCertificatesResponse Lude.Int
locrsResponseStatus = Lens.lens (responseStatus :: ListOutgoingCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOutgoingCertificatesResponse)
{-# DEPRECATED locrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
