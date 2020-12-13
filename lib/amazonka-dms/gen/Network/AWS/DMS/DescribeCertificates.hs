{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a description of the certificate.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeCertificates
  ( -- * Creating a request
    DescribeCertificates (..),
    mkDescribeCertificates,

    -- ** Request lenses
    dcFilters,
    dcMarker,
    dcMaxRecords,

    -- * Destructuring the response
    DescribeCertificatesResponse (..),
    mkDescribeCertificatesResponse,

    -- ** Response lenses
    drsCertificates,
    drsMarker,
    drsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { -- | Filters applied to the certificates described in the form of key-value pairs.
    filters :: Lude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 10
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificates' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to the certificates described in the form of key-value pairs.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 10
mkDescribeCertificates ::
  DescribeCertificates
mkDescribeCertificates =
  DescribeCertificates'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filters applied to the certificates described in the form of key-value pairs.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcFilters :: Lens.Lens' DescribeCertificates (Lude.Maybe [Filter])
dcFilters = Lens.lens (filters :: DescribeCertificates -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeCertificates)
{-# DEPRECATED dcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMarker :: Lens.Lens' DescribeCertificates (Lude.Maybe Lude.Text)
dcMarker = Lens.lens (marker :: DescribeCertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCertificates)
{-# DEPRECATED dcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 10
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxRecords :: Lens.Lens' DescribeCertificates (Lude.Maybe Lude.Int)
dcMaxRecords = Lens.lens (maxRecords :: DescribeCertificates -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeCertificates)
{-# DEPRECATED dcMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeCertificates where
  page rq rs
    | Page.stop (rs Lens.^. drsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drsCertificates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dcMarker Lens..~ rs Lens.^. drsMarker

instance Lude.AWSRequest DescribeCertificates where
  type Rs DescribeCertificates = DescribeCertificatesResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCertificatesResponse'
            Lude.<$> (x Lude..?> "Certificates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCertificates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeCertificates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCertificates where
  toJSON DescribeCertificates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCertificates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { -- | The Secure Sockets Layer (SSL) certificates associated with the replication instance.
    certificates :: Lude.Maybe [Certificate],
    -- | The pagination token.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - The Secure Sockets Layer (SSL) certificates associated with the replication instance.
-- * 'marker' - The pagination token.
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

-- | The Secure Sockets Layer (SSL) certificates associated with the replication instance.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCertificates :: Lens.Lens' DescribeCertificatesResponse (Lude.Maybe [Certificate])
drsCertificates = Lens.lens (certificates :: DescribeCertificatesResponse -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: DescribeCertificatesResponse)
{-# DEPRECATED drsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsMarker :: Lens.Lens' DescribeCertificatesResponse (Lude.Maybe Lude.Text)
drsMarker = Lens.lens (marker :: DescribeCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCertificatesResponse)
{-# DEPRECATED drsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCertificatesResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCertificatesResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
