{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the set of CA certificates provided by Amazon RDS for this AWS account.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeCertificates
  ( -- * Creating a request
    DescribeCertificates (..),
    mkDescribeCertificates,

    -- ** Request lenses
    dcFilters,
    dcCertificateIdentifier,
    dcMarker,
    dcMaxRecords,

    -- * Destructuring the response
    DescribeCertificatesResponse (..),
    mkDescribeCertificatesResponse,

    -- ** Response lenses
    dcrsCertificates,
    dcrsMarker,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { -- | This parameter isn't currently supported.
    filters :: Lude.Maybe [Filter],
    -- | The user-supplied certificate identifier. If this parameter is specified, information for only the identified certificate is returned. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match an existing CertificateIdentifier.
    certificateIdentifier :: Lude.Maybe Lude.Text,
    -- | An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificates' with the minimum fields required to make a request.
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'certificateIdentifier' - The user-supplied certificate identifier. If this parameter is specified, information for only the identified certificate is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match an existing CertificateIdentifier.
--
--
-- * 'marker' - An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeCertificates ::
  DescribeCertificates
mkDescribeCertificates =
  DescribeCertificates'
    { filters = Lude.Nothing,
      certificateIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcFilters :: Lens.Lens' DescribeCertificates (Lude.Maybe [Filter])
dcFilters = Lens.lens (filters :: DescribeCertificates -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeCertificates)
{-# DEPRECATED dcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The user-supplied certificate identifier. If this parameter is specified, information for only the identified certificate is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match an existing CertificateIdentifier.
--
--
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateIdentifier :: Lens.Lens' DescribeCertificates (Lude.Maybe Lude.Text)
dcCertificateIdentifier = Lens.lens (certificateIdentifier :: DescribeCertificates -> Lude.Maybe Lude.Text) (\s a -> s {certificateIdentifier = a} :: DescribeCertificates)
{-# DEPRECATED dcCertificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead." #-}

-- | An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMarker :: Lens.Lens' DescribeCertificates (Lude.Maybe Lude.Text)
dcMarker = Lens.lens (marker :: DescribeCertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCertificates)
{-# DEPRECATED dcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxRecords :: Lens.Lens' DescribeCertificates (Lude.Maybe Lude.Int)
dcMaxRecords = Lens.lens (maxRecords :: DescribeCertificates -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeCertificates)
{-# DEPRECATED dcMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeCertificates where
  page rq rs
    | Page.stop (rs Lens.^. dcrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcrsCertificates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dcMarker Lens..~ rs Lens.^. dcrsMarker

instance Lude.AWSRequest DescribeCertificates where
  type Rs DescribeCertificates = DescribeCertificatesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeCertificatesResult"
      ( \s h x ->
          DescribeCertificatesResponse'
            Lude.<$> ( x Lude..@? "Certificates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Certificate")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCertificates where
  toQuery DescribeCertificates' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeCertificates" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "CertificateIdentifier" Lude.=: certificateIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Data returned by the __DescribeCertificates__ action.
--
-- /See:/ 'mkDescribeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { -- | The list of @Certificate@ objects for the AWS account.
    certificates :: Lude.Maybe [Certificate],
    -- | An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - The list of @Certificate@ objects for the AWS account.
-- * 'marker' - An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCertificatesResponse
mkDescribeCertificatesResponse pResponseStatus_ =
  DescribeCertificatesResponse'
    { certificates = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of @Certificate@ objects for the AWS account.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCertificates :: Lens.Lens' DescribeCertificatesResponse (Lude.Maybe [Certificate])
dcrsCertificates = Lens.lens (certificates :: DescribeCertificatesResponse -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: DescribeCertificatesResponse)
{-# DEPRECATED dcrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsMarker :: Lens.Lens' DescribeCertificatesResponse (Lude.Maybe Lude.Text)
dcrsMarker = Lens.lens (marker :: DescribeCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCertificatesResponse)
{-# DEPRECATED dcrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeCertificatesResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCertificatesResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
