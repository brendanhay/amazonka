{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeTableRestoreStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the status of one or more table restore requests made using the 'RestoreTableFromClusterSnapshot' API action. If you don't specify a value for the @TableRestoreRequestId@ parameter, then @DescribeTableRestoreStatus@ returns the status of all table restore requests ordered by the date and time of the request in ascending order. Otherwise @DescribeTableRestoreStatus@ returns the status of the table specified by @TableRestoreRequestId@ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeTableRestoreStatus
  ( -- * Creating a request
    DescribeTableRestoreStatus (..),
    mkDescribeTableRestoreStatus,

    -- ** Request lenses
    dtrsTableRestoreRequestId,
    dtrsClusterIdentifier,
    dtrsMarker,
    dtrsMaxRecords,

    -- * Destructuring the response
    DescribeTableRestoreStatusResponse (..),
    mkDescribeTableRestoreStatusResponse,

    -- ** Response lenses
    dtrsrsMarker,
    dtrsrsTableRestoreStatusDetails,
    dtrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeTableRestoreStatus' smart constructor.
data DescribeTableRestoreStatus = DescribeTableRestoreStatus'
  { -- | The identifier of the table restore request to return status for. If you don't specify a @TableRestoreRequestId@ value, then @DescribeTableRestoreStatus@ returns the status of all in-progress table restore requests.
    tableRestoreRequestId :: Lude.Maybe Lude.Text,
    -- | The Amazon Redshift cluster that the table is being restored to.
    clusterIdentifier :: Lude.Maybe Lude.Text,
    -- | An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTableRestoreStatus' with the minimum fields required to make a request.
--
-- * 'tableRestoreRequestId' - The identifier of the table restore request to return status for. If you don't specify a @TableRestoreRequestId@ value, then @DescribeTableRestoreStatus@ returns the status of all in-progress table restore requests.
-- * 'clusterIdentifier' - The Amazon Redshift cluster that the table is being restored to.
-- * 'marker' - An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
mkDescribeTableRestoreStatus ::
  DescribeTableRestoreStatus
mkDescribeTableRestoreStatus =
  DescribeTableRestoreStatus'
    { tableRestoreRequestId = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The identifier of the table restore request to return status for. If you don't specify a @TableRestoreRequestId@ value, then @DescribeTableRestoreStatus@ returns the status of all in-progress table restore requests.
--
-- /Note:/ Consider using 'tableRestoreRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTableRestoreRequestId :: Lens.Lens' DescribeTableRestoreStatus (Lude.Maybe Lude.Text)
dtrsTableRestoreRequestId = Lens.lens (tableRestoreRequestId :: DescribeTableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {tableRestoreRequestId = a} :: DescribeTableRestoreStatus)
{-# DEPRECATED dtrsTableRestoreRequestId "Use generic-lens or generic-optics with 'tableRestoreRequestId' instead." #-}

-- | The Amazon Redshift cluster that the table is being restored to.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsClusterIdentifier :: Lens.Lens' DescribeTableRestoreStatus (Lude.Maybe Lude.Text)
dtrsClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeTableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeTableRestoreStatus)
{-# DEPRECATED dtrsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsMarker :: Lens.Lens' DescribeTableRestoreStatus (Lude.Maybe Lude.Text)
dtrsMarker = Lens.lens (marker :: DescribeTableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTableRestoreStatus)
{-# DEPRECATED dtrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsMaxRecords :: Lens.Lens' DescribeTableRestoreStatus (Lude.Maybe Lude.Int)
dtrsMaxRecords = Lens.lens (maxRecords :: DescribeTableRestoreStatus -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeTableRestoreStatus)
{-# DEPRECATED dtrsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeTableRestoreStatus where
  page rq rs
    | Page.stop (rs Lens.^. dtrsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dtrsrsTableRestoreStatusDetails) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtrsMarker Lens..~ rs Lens.^. dtrsrsMarker

instance Lude.AWSRequest DescribeTableRestoreStatus where
  type
    Rs DescribeTableRestoreStatus =
      DescribeTableRestoreStatusResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeTableRestoreStatusResult"
      ( \s h x ->
          DescribeTableRestoreStatusResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "TableRestoreStatusDetails" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "TableRestoreStatus")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTableRestoreStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTableRestoreStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTableRestoreStatus where
  toQuery DescribeTableRestoreStatus' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTableRestoreStatus" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TableRestoreRequestId" Lude.=: tableRestoreRequestId,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- |
--
-- /See:/ 'mkDescribeTableRestoreStatusResponse' smart constructor.
data DescribeTableRestoreStatusResponse = DescribeTableRestoreStatusResponse'
  { -- | A pagination token that can be used in a subsequent 'DescribeTableRestoreStatus' request.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of status details for one or more table restore requests.
    tableRestoreStatusDetails :: Lude.Maybe [TableRestoreStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTableRestoreStatusResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A pagination token that can be used in a subsequent 'DescribeTableRestoreStatus' request.
-- * 'tableRestoreStatusDetails' - A list of status details for one or more table restore requests.
-- * 'responseStatus' - The response status code.
mkDescribeTableRestoreStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTableRestoreStatusResponse
mkDescribeTableRestoreStatusResponse pResponseStatus_ =
  DescribeTableRestoreStatusResponse'
    { marker = Lude.Nothing,
      tableRestoreStatusDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token that can be used in a subsequent 'DescribeTableRestoreStatus' request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsrsMarker :: Lens.Lens' DescribeTableRestoreStatusResponse (Lude.Maybe Lude.Text)
dtrsrsMarker = Lens.lens (marker :: DescribeTableRestoreStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTableRestoreStatusResponse)
{-# DEPRECATED dtrsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of status details for one or more table restore requests.
--
-- /Note:/ Consider using 'tableRestoreStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsrsTableRestoreStatusDetails :: Lens.Lens' DescribeTableRestoreStatusResponse (Lude.Maybe [TableRestoreStatus])
dtrsrsTableRestoreStatusDetails = Lens.lens (tableRestoreStatusDetails :: DescribeTableRestoreStatusResponse -> Lude.Maybe [TableRestoreStatus]) (\s a -> s {tableRestoreStatusDetails = a} :: DescribeTableRestoreStatusResponse)
{-# DEPRECATED dtrsrsTableRestoreStatusDetails "Use generic-lens or generic-optics with 'tableRestoreStatusDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsrsResponseStatus :: Lens.Lens' DescribeTableRestoreStatusResponse Lude.Int
dtrsrsResponseStatus = Lens.lens (responseStatus :: DescribeTableRestoreStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTableRestoreStatusResponse)
{-# DEPRECATED dtrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
