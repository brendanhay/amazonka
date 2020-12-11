{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeListenerCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default certificate and the certificate list for the specified HTTPS or TLS listener.
--
-- If the default certificate is also in the certificate list, it appears twice in the results (once with @IsDefault@ set to true and once with @IsDefault@ set to false).
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#https-listener-certificates SSL certificates> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#tls-listener-certificate Server certificates> in the /Network Load Balancers Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeListenerCertificates
  ( -- * Creating a request
    DescribeListenerCertificates (..),
    mkDescribeListenerCertificates,

    -- ** Request lenses
    dlcMarker,
    dlcPageSize,
    dlcListenerARN,

    -- * Destructuring the response
    DescribeListenerCertificatesResponse (..),
    mkDescribeListenerCertificatesResponse,

    -- ** Response lenses
    dlcrsCertificates,
    dlcrsNextMarker,
    dlcrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeListenerCertificates' smart constructor.
data DescribeListenerCertificates = DescribeListenerCertificates'
  { marker ::
      Lude.Maybe Lude.Text,
    pageSize ::
      Lude.Maybe Lude.Natural,
    listenerARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeListenerCertificates' with the minimum fields required to make a request.
--
-- * 'listenerARN' - The Amazon Resource Names (ARN) of the listener.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'pageSize' - The maximum number of results to return with this call.
mkDescribeListenerCertificates ::
  -- | 'listenerARN'
  Lude.Text ->
  DescribeListenerCertificates
mkDescribeListenerCertificates pListenerARN_ =
  DescribeListenerCertificates'
    { marker = Lude.Nothing,
      pageSize = Lude.Nothing,
      listenerARN = pListenerARN_
    }

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcMarker :: Lens.Lens' DescribeListenerCertificates (Lude.Maybe Lude.Text)
dlcMarker = Lens.lens (marker :: DescribeListenerCertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeListenerCertificates)
{-# DEPRECATED dlcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcPageSize :: Lens.Lens' DescribeListenerCertificates (Lude.Maybe Lude.Natural)
dlcPageSize = Lens.lens (pageSize :: DescribeListenerCertificates -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeListenerCertificates)
{-# DEPRECATED dlcPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The Amazon Resource Names (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcListenerARN :: Lens.Lens' DescribeListenerCertificates Lude.Text
dlcListenerARN = Lens.lens (listenerARN :: DescribeListenerCertificates -> Lude.Text) (\s a -> s {listenerARN = a} :: DescribeListenerCertificates)
{-# DEPRECATED dlcListenerARN "Use generic-lens or generic-optics with 'listenerARN' instead." #-}

instance Page.AWSPager DescribeListenerCertificates where
  page rq rs
    | Page.stop (rs Lens.^. dlcrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dlcrsCertificates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlcMarker Lens..~ rs Lens.^. dlcrsNextMarker

instance Lude.AWSRequest DescribeListenerCertificates where
  type
    Rs DescribeListenerCertificates =
      DescribeListenerCertificatesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeListenerCertificatesResult"
      ( \s h x ->
          DescribeListenerCertificatesResponse'
            Lude.<$> ( x Lude..@? "Certificates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeListenerCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeListenerCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeListenerCertificates where
  toQuery DescribeListenerCertificates' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeListenerCertificates" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "PageSize" Lude.=: pageSize,
        "ListenerArn" Lude.=: listenerARN
      ]

-- | /See:/ 'mkDescribeListenerCertificatesResponse' smart constructor.
data DescribeListenerCertificatesResponse = DescribeListenerCertificatesResponse'
  { certificates ::
      Lude.Maybe
        [Certificate],
    nextMarker ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'DescribeListenerCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - Information about the certificates.
-- * 'nextMarker' - If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
-- * 'responseStatus' - The response status code.
mkDescribeListenerCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeListenerCertificatesResponse
mkDescribeListenerCertificatesResponse pResponseStatus_ =
  DescribeListenerCertificatesResponse'
    { certificates =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the certificates.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsCertificates :: Lens.Lens' DescribeListenerCertificatesResponse (Lude.Maybe [Certificate])
dlcrsCertificates = Lens.lens (certificates :: DescribeListenerCertificatesResponse -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: DescribeListenerCertificatesResponse)
{-# DEPRECATED dlcrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsNextMarker :: Lens.Lens' DescribeListenerCertificatesResponse (Lude.Maybe Lude.Text)
dlcrsNextMarker = Lens.lens (nextMarker :: DescribeListenerCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeListenerCertificatesResponse)
{-# DEPRECATED dlcrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsResponseStatus :: Lens.Lens' DescribeListenerCertificatesResponse Lude.Int
dlcrsResponseStatus = Lens.lens (responseStatus :: DescribeListenerCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeListenerCertificatesResponse)
{-# DEPRECATED dlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
