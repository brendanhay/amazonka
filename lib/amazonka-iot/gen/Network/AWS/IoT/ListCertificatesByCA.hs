{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListCertificatesByCA
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the device certificates signed by the specified CA certificate.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCertificatesByCA
  ( -- * Creating a request
    ListCertificatesByCA (..),
    mkListCertificatesByCA,

    -- ** Request lenses
    lcbcaCaCertificateId,
    lcbcaMarker,
    lcbcaAscendingOrder,
    lcbcaPageSize,

    -- * Destructuring the response
    ListCertificatesByCAResponse (..),
    mkListCertificatesByCAResponse,

    -- ** Response lenses
    lcbcarsCertificates,
    lcbcarsNextMarker,
    lcbcarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input to the ListCertificatesByCA operation.
--
-- /See:/ 'mkListCertificatesByCA' smart constructor.
data ListCertificatesByCA = ListCertificatesByCA'
  { -- | The ID of the CA certificate. This operation will list all registered device certificate that were signed by this CA certificate.
    caCertificateId :: Lude.Text,
    -- | The marker for the next set of results.
    marker :: Lude.Maybe Lude.Text,
    -- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
    ascendingOrder :: Lude.Maybe Lude.Bool,
    -- | The result page size.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCertificatesByCA' with the minimum fields required to make a request.
--
-- * 'caCertificateId' - The ID of the CA certificate. This operation will list all registered device certificate that were signed by this CA certificate.
-- * 'marker' - The marker for the next set of results.
-- * 'ascendingOrder' - Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
-- * 'pageSize' - The result page size.
mkListCertificatesByCA ::
  -- | 'caCertificateId'
  Lude.Text ->
  ListCertificatesByCA
mkListCertificatesByCA pCaCertificateId_ =
  ListCertificatesByCA'
    { caCertificateId = pCaCertificateId_,
      marker = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The ID of the CA certificate. This operation will list all registered device certificate that were signed by this CA certificate.
--
-- /Note:/ Consider using 'caCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcaCaCertificateId :: Lens.Lens' ListCertificatesByCA Lude.Text
lcbcaCaCertificateId = Lens.lens (caCertificateId :: ListCertificatesByCA -> Lude.Text) (\s a -> s {caCertificateId = a} :: ListCertificatesByCA)
{-# DEPRECATED lcbcaCaCertificateId "Use generic-lens or generic-optics with 'caCertificateId' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcaMarker :: Lens.Lens' ListCertificatesByCA (Lude.Maybe Lude.Text)
lcbcaMarker = Lens.lens (marker :: ListCertificatesByCA -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListCertificatesByCA)
{-# DEPRECATED lcbcaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcaAscendingOrder :: Lens.Lens' ListCertificatesByCA (Lude.Maybe Lude.Bool)
lcbcaAscendingOrder = Lens.lens (ascendingOrder :: ListCertificatesByCA -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListCertificatesByCA)
{-# DEPRECATED lcbcaAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcaPageSize :: Lens.Lens' ListCertificatesByCA (Lude.Maybe Lude.Natural)
lcbcaPageSize = Lens.lens (pageSize :: ListCertificatesByCA -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListCertificatesByCA)
{-# DEPRECATED lcbcaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListCertificatesByCA where
  page rq rs
    | Page.stop (rs Lens.^. lcbcarsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lcbcarsCertificates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcbcaMarker Lens..~ rs Lens.^. lcbcarsNextMarker

instance Lude.AWSRequest ListCertificatesByCA where
  type Rs ListCertificatesByCA = ListCertificatesByCAResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCertificatesByCAResponse'
            Lude.<$> (x Lude..?> "certificates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCertificatesByCA where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListCertificatesByCA where
  toPath ListCertificatesByCA' {..} =
    Lude.mconcat ["/certificates-by-ca/", Lude.toBS caCertificateId]

instance Lude.ToQuery ListCertificatesByCA where
  toQuery ListCertificatesByCA' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "isAscendingOrder" Lude.=: ascendingOrder,
        "pageSize" Lude.=: pageSize
      ]

-- | The output of the ListCertificatesByCA operation.
--
-- /See:/ 'mkListCertificatesByCAResponse' smart constructor.
data ListCertificatesByCAResponse = ListCertificatesByCAResponse'
  { -- | The device certificates signed by the specified CA certificate.
    certificates :: Lude.Maybe [Certificate],
    -- | The marker for the next set of results, or null if there are no additional results.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCertificatesByCAResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - The device certificates signed by the specified CA certificate.
-- * 'nextMarker' - The marker for the next set of results, or null if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListCertificatesByCAResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCertificatesByCAResponse
mkListCertificatesByCAResponse pResponseStatus_ =
  ListCertificatesByCAResponse'
    { certificates = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The device certificates signed by the specified CA certificate.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcarsCertificates :: Lens.Lens' ListCertificatesByCAResponse (Lude.Maybe [Certificate])
lcbcarsCertificates = Lens.lens (certificates :: ListCertificatesByCAResponse -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: ListCertificatesByCAResponse)
{-# DEPRECATED lcbcarsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The marker for the next set of results, or null if there are no additional results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcarsNextMarker :: Lens.Lens' ListCertificatesByCAResponse (Lude.Maybe Lude.Text)
lcbcarsNextMarker = Lens.lens (nextMarker :: ListCertificatesByCAResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListCertificatesByCAResponse)
{-# DEPRECATED lcbcarsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcarsResponseStatus :: Lens.Lens' ListCertificatesByCAResponse Lude.Int
lcbcarsResponseStatus = Lens.lens (responseStatus :: ListCertificatesByCAResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCertificatesByCAResponse)
{-# DEPRECATED lcbcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
