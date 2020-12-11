{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeInstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available installation media for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeInstallationMedia
  ( -- * Creating a request
    DescribeInstallationMedia (..),
    mkDescribeInstallationMedia,

    -- ** Request lenses
    dimInstallationMediaId,
    dimFilters,
    dimMarker,
    dimMaxRecords,

    -- * Destructuring the response
    DescribeInstallationMediaResponse (..),
    mkDescribeInstallationMediaResponse,

    -- ** Response lenses
    dimrsMarker,
    dimrsInstallationMedia,
    dimrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstallationMedia' smart constructor.
data DescribeInstallationMedia = DescribeInstallationMedia'
  { installationMediaId ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstallationMedia' with the minimum fields required to make a request.
--
-- * 'filters' - A filter that specifies one or more installation media to describe. Supported filters include the following:
--
--
--     * @custom-availability-zone-id@ - Accepts custom Availability Zone (AZ) identifiers. The results list includes information about only the custom AZs identified by these identifiers.
--
--
--     * @engine@ - Accepts database engines. The results list includes information about only the database engines identified by these identifiers.
-- For more information about the valid engines for installation media, see 'ImportInstallationMedia' .
--
--
-- * 'installationMediaId' - The installation medium ID.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - An optional pagination token provided by a previous DescribeInstallationMedia request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
mkDescribeInstallationMedia ::
  DescribeInstallationMedia
mkDescribeInstallationMedia =
  DescribeInstallationMedia'
    { installationMediaId = Lude.Nothing,
      filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The installation medium ID.
--
-- /Note:/ Consider using 'installationMediaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimInstallationMediaId :: Lens.Lens' DescribeInstallationMedia (Lude.Maybe Lude.Text)
dimInstallationMediaId = Lens.lens (installationMediaId :: DescribeInstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {installationMediaId = a} :: DescribeInstallationMedia)
{-# DEPRECATED dimInstallationMediaId "Use generic-lens or generic-optics with 'installationMediaId' instead." #-}

-- | A filter that specifies one or more installation media to describe. Supported filters include the following:
--
--
--     * @custom-availability-zone-id@ - Accepts custom Availability Zone (AZ) identifiers. The results list includes information about only the custom AZs identified by these identifiers.
--
--
--     * @engine@ - Accepts database engines. The results list includes information about only the database engines identified by these identifiers.
-- For more information about the valid engines for installation media, see 'ImportInstallationMedia' .
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimFilters :: Lens.Lens' DescribeInstallationMedia (Lude.Maybe [Filter])
dimFilters = Lens.lens (filters :: DescribeInstallationMedia -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeInstallationMedia)
{-# DEPRECATED dimFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimMarker :: Lens.Lens' DescribeInstallationMedia (Lude.Maybe Lude.Text)
dimMarker = Lens.lens (marker :: DescribeInstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeInstallationMedia)
{-# DEPRECATED dimMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An optional pagination token provided by a previous DescribeInstallationMedia request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimMaxRecords :: Lens.Lens' DescribeInstallationMedia (Lude.Maybe Lude.Int)
dimMaxRecords = Lens.lens (maxRecords :: DescribeInstallationMedia -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeInstallationMedia)
{-# DEPRECATED dimMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeInstallationMedia where
  page rq rs
    | Page.stop (rs Lens.^. dimrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dimrsInstallationMedia) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dimMarker Lens..~ rs Lens.^. dimrsMarker

instance Lude.AWSRequest DescribeInstallationMedia where
  type
    Rs DescribeInstallationMedia =
      DescribeInstallationMediaResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeInstallationMediaResult"
      ( \s h x ->
          DescribeInstallationMediaResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "InstallationMedia" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "InstallationMedia")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstallationMedia where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstallationMedia where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstallationMedia where
  toQuery DescribeInstallationMedia' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInstallationMedia" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "InstallationMediaId" Lude.=: installationMediaId,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeInstallationMediaResponse' smart constructor.
data DescribeInstallationMediaResponse = DescribeInstallationMediaResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    installationMedia ::
      Lude.Maybe
        [InstallationMedia],
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

-- | Creates a value of 'DescribeInstallationMediaResponse' with the minimum fields required to make a request.
--
-- * 'installationMedia' - The list of 'InstallationMedia' objects for the AWS account.
-- * 'marker' - An optional pagination token provided by a previous 'DescribeInstallationMedia' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeInstallationMediaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstallationMediaResponse
mkDescribeInstallationMediaResponse pResponseStatus_ =
  DescribeInstallationMediaResponse'
    { marker = Lude.Nothing,
      installationMedia = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous 'DescribeInstallationMedia' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimrsMarker :: Lens.Lens' DescribeInstallationMediaResponse (Lude.Maybe Lude.Text)
dimrsMarker = Lens.lens (marker :: DescribeInstallationMediaResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeInstallationMediaResponse)
{-# DEPRECATED dimrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of 'InstallationMedia' objects for the AWS account.
--
-- /Note:/ Consider using 'installationMedia' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimrsInstallationMedia :: Lens.Lens' DescribeInstallationMediaResponse (Lude.Maybe [InstallationMedia])
dimrsInstallationMedia = Lens.lens (installationMedia :: DescribeInstallationMediaResponse -> Lude.Maybe [InstallationMedia]) (\s a -> s {installationMedia = a} :: DescribeInstallationMediaResponse)
{-# DEPRECATED dimrsInstallationMedia "Use generic-lens or generic-optics with 'installationMedia' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimrsResponseStatus :: Lens.Lens' DescribeInstallationMediaResponse Lude.Int
dimrsResponseStatus = Lens.lens (responseStatus :: DescribeInstallationMediaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstallationMediaResponse)
{-# DEPRECATED dimrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
