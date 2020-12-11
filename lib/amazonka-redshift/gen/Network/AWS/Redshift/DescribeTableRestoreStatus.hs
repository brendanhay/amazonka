{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dtrssTableRestoreRequestId,
    dtrssClusterIdentifier,
    dtrssMarker,
    dtrssMaxRecords,

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
  { tableRestoreRequestId ::
      Lude.Maybe Lude.Text,
    clusterIdentifier ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeTableRestoreStatus' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The Amazon Redshift cluster that the table is being restored to.
-- * 'marker' - An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
-- * 'tableRestoreRequestId' - The identifier of the table restore request to return status for. If you don't specify a @TableRestoreRequestId@ value, then @DescribeTableRestoreStatus@ returns the status of all in-progress table restore requests.
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
dtrssTableRestoreRequestId :: Lens.Lens' DescribeTableRestoreStatus (Lude.Maybe Lude.Text)
dtrssTableRestoreRequestId = Lens.lens (tableRestoreRequestId :: DescribeTableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {tableRestoreRequestId = a} :: DescribeTableRestoreStatus)
{-# DEPRECATED dtrssTableRestoreRequestId "Use generic-lens or generic-optics with 'tableRestoreRequestId' instead." #-}

-- | The Amazon Redshift cluster that the table is being restored to.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrssClusterIdentifier :: Lens.Lens' DescribeTableRestoreStatus (Lude.Maybe Lude.Text)
dtrssClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeTableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeTableRestoreStatus)
{-# DEPRECATED dtrssClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrssMarker :: Lens.Lens' DescribeTableRestoreStatus (Lude.Maybe Lude.Text)
dtrssMarker = Lens.lens (marker :: DescribeTableRestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTableRestoreStatus)
{-# DEPRECATED dtrssMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrssMaxRecords :: Lens.Lens' DescribeTableRestoreStatus (Lude.Maybe Lude.Int)
dtrssMaxRecords = Lens.lens (maxRecords :: DescribeTableRestoreStatus -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeTableRestoreStatus)
{-# DEPRECATED dtrssMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeTableRestoreStatus where
  page rq rs
    | Page.stop (rs Lens.^. dtrsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dtrsrsTableRestoreStatusDetails) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtrssMarker Lens..~ rs Lens.^. dtrsrsMarker

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
  { marker ::
      Lude.Maybe Lude.Text,
    tableRestoreStatusDetails ::
      Lude.Maybe
        [TableRestoreStatus],
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

-- | Creates a value of 'DescribeTableRestoreStatusResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A pagination token that can be used in a subsequent 'DescribeTableRestoreStatus' request.
-- * 'responseStatus' - The response status code.
-- * 'tableRestoreStatusDetails' - A list of status details for one or more table restore requests.
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
