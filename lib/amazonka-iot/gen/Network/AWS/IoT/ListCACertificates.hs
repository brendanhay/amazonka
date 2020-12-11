{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListCACertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the CA certificates registered for your AWS account.
--
-- The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCACertificates
  ( -- * Creating a request
    ListCACertificates (..),
    mkListCACertificates,

    -- ** Request lenses
    lcacMarker,
    lcacAscendingOrder,
    lcacPageSize,

    -- * Destructuring the response
    ListCACertificatesResponse (..),
    mkListCACertificatesResponse,

    -- ** Response lenses
    lcacrsCertificates,
    lcacrsNextMarker,
    lcacrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input for the ListCACertificates operation.
--
-- /See:/ 'mkListCACertificates' smart constructor.
data ListCACertificates = ListCACertificates'
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

-- | Creates a value of 'ListCACertificates' with the minimum fields required to make a request.
--
-- * 'ascendingOrder' - Determines the order of the results.
-- * 'marker' - The marker for the next set of results.
-- * 'pageSize' - The result page size.
mkListCACertificates ::
  ListCACertificates
mkListCACertificates =
  ListCACertificates'
    { marker = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacMarker :: Lens.Lens' ListCACertificates (Lude.Maybe Lude.Text)
lcacMarker = Lens.lens (marker :: ListCACertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListCACertificates)
{-# DEPRECATED lcacMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Determines the order of the results.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacAscendingOrder :: Lens.Lens' ListCACertificates (Lude.Maybe Lude.Bool)
lcacAscendingOrder = Lens.lens (ascendingOrder :: ListCACertificates -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListCACertificates)
{-# DEPRECATED lcacAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacPageSize :: Lens.Lens' ListCACertificates (Lude.Maybe Lude.Natural)
lcacPageSize = Lens.lens (pageSize :: ListCACertificates -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListCACertificates)
{-# DEPRECATED lcacPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListCACertificates where
  page rq rs
    | Page.stop (rs Lens.^. lcacrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lcacrsCertificates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcacMarker Lens..~ rs Lens.^. lcacrsNextMarker

instance Lude.AWSRequest ListCACertificates where
  type Rs ListCACertificates = ListCACertificatesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCACertificatesResponse'
            Lude.<$> (x Lude..?> "certificates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCACertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListCACertificates where
  toPath = Lude.const "/cacertificates"

instance Lude.ToQuery ListCACertificates where
  toQuery ListCACertificates' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "isAscendingOrder" Lude.=: ascendingOrder,
        "pageSize" Lude.=: pageSize
      ]

-- | The output from the ListCACertificates operation.
--
-- /See:/ 'mkListCACertificatesResponse' smart constructor.
data ListCACertificatesResponse = ListCACertificatesResponse'
  { certificates ::
      Lude.Maybe [CACertificate],
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

-- | Creates a value of 'ListCACertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - The CA certificates registered in your AWS account.
-- * 'nextMarker' - The current position within the list of CA certificates.
-- * 'responseStatus' - The response status code.
mkListCACertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCACertificatesResponse
mkListCACertificatesResponse pResponseStatus_ =
  ListCACertificatesResponse'
    { certificates = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The CA certificates registered in your AWS account.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacrsCertificates :: Lens.Lens' ListCACertificatesResponse (Lude.Maybe [CACertificate])
lcacrsCertificates = Lens.lens (certificates :: ListCACertificatesResponse -> Lude.Maybe [CACertificate]) (\s a -> s {certificates = a} :: ListCACertificatesResponse)
{-# DEPRECATED lcacrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The current position within the list of CA certificates.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacrsNextMarker :: Lens.Lens' ListCACertificatesResponse (Lude.Maybe Lude.Text)
lcacrsNextMarker = Lens.lens (nextMarker :: ListCACertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListCACertificatesResponse)
{-# DEPRECATED lcacrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacrsResponseStatus :: Lens.Lens' ListCACertificatesResponse Lude.Int
lcacrsResponseStatus = Lens.lens (responseStatus :: ListCACertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCACertificatesResponse)
{-# DEPRECATED lcacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
