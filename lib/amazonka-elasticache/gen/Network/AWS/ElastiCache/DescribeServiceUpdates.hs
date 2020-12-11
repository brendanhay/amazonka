{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeServiceUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the service updates
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeServiceUpdates
  ( -- * Creating a request
    DescribeServiceUpdates (..),
    mkDescribeServiceUpdates,

    -- ** Request lenses
    dsuServiceUpdateName,
    dsuMarker,
    dsuMaxRecords,
    dsuServiceUpdateStatus,

    -- * Destructuring the response
    DescribeServiceUpdatesResponse (..),
    mkDescribeServiceUpdatesResponse,

    -- ** Response lenses
    dsursServiceUpdates,
    dsursMarker,
    dsursResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeServiceUpdates' smart constructor.
data DescribeServiceUpdates = DescribeServiceUpdates'
  { serviceUpdateName ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    serviceUpdateStatus ::
      Lude.Maybe [ServiceUpdateStatus]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServiceUpdates' with the minimum fields required to make a request.
--
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response
-- * 'serviceUpdateName' - The unique ID of the service update
-- * 'serviceUpdateStatus' - The status of the service update
mkDescribeServiceUpdates ::
  DescribeServiceUpdates
mkDescribeServiceUpdates =
  DescribeServiceUpdates'
    { serviceUpdateName = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      serviceUpdateStatus = Lude.Nothing
    }

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuServiceUpdateName :: Lens.Lens' DescribeServiceUpdates (Lude.Maybe Lude.Text)
dsuServiceUpdateName = Lens.lens (serviceUpdateName :: DescribeServiceUpdates -> Lude.Maybe Lude.Text) (\s a -> s {serviceUpdateName = a} :: DescribeServiceUpdates)
{-# DEPRECATED dsuServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuMarker :: Lens.Lens' DescribeServiceUpdates (Lude.Maybe Lude.Text)
dsuMarker = Lens.lens (marker :: DescribeServiceUpdates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeServiceUpdates)
{-# DEPRECATED dsuMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuMaxRecords :: Lens.Lens' DescribeServiceUpdates (Lude.Maybe Lude.Int)
dsuMaxRecords = Lens.lens (maxRecords :: DescribeServiceUpdates -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeServiceUpdates)
{-# DEPRECATED dsuMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuServiceUpdateStatus :: Lens.Lens' DescribeServiceUpdates (Lude.Maybe [ServiceUpdateStatus])
dsuServiceUpdateStatus = Lens.lens (serviceUpdateStatus :: DescribeServiceUpdates -> Lude.Maybe [ServiceUpdateStatus]) (\s a -> s {serviceUpdateStatus = a} :: DescribeServiceUpdates)
{-# DEPRECATED dsuServiceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead." #-}

instance Page.AWSPager DescribeServiceUpdates where
  page rq rs
    | Page.stop (rs Lens.^. dsursMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dsursServiceUpdates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dsuMarker Lens..~ rs Lens.^. dsursMarker

instance Lude.AWSRequest DescribeServiceUpdates where
  type Rs DescribeServiceUpdates = DescribeServiceUpdatesResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeServiceUpdatesResult"
      ( \s h x ->
          DescribeServiceUpdatesResponse'
            Lude.<$> ( x Lude..@? "ServiceUpdates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ServiceUpdate")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeServiceUpdates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeServiceUpdates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeServiceUpdates where
  toQuery DescribeServiceUpdates' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeServiceUpdates" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "ServiceUpdateName" Lude.=: serviceUpdateName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ServiceUpdateStatus"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> serviceUpdateStatus)
      ]

-- | /See:/ 'mkDescribeServiceUpdatesResponse' smart constructor.
data DescribeServiceUpdatesResponse = DescribeServiceUpdatesResponse'
  { serviceUpdates ::
      Lude.Maybe [ServiceUpdate],
    marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeServiceUpdatesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
-- * 'serviceUpdates' - A list of service updates
mkDescribeServiceUpdatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeServiceUpdatesResponse
mkDescribeServiceUpdatesResponse pResponseStatus_ =
  DescribeServiceUpdatesResponse'
    { serviceUpdates = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of service updates
--
-- /Note:/ Consider using 'serviceUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsursServiceUpdates :: Lens.Lens' DescribeServiceUpdatesResponse (Lude.Maybe [ServiceUpdate])
dsursServiceUpdates = Lens.lens (serviceUpdates :: DescribeServiceUpdatesResponse -> Lude.Maybe [ServiceUpdate]) (\s a -> s {serviceUpdates = a} :: DescribeServiceUpdatesResponse)
{-# DEPRECATED dsursServiceUpdates "Use generic-lens or generic-optics with 'serviceUpdates' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsursMarker :: Lens.Lens' DescribeServiceUpdatesResponse (Lude.Maybe Lude.Text)
dsursMarker = Lens.lens (marker :: DescribeServiceUpdatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeServiceUpdatesResponse)
{-# DEPRECATED dsursMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsursResponseStatus :: Lens.Lens' DescribeServiceUpdatesResponse Lude.Int
dsursResponseStatus = Lens.lens (responseStatus :: DescribeServiceUpdatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeServiceUpdatesResponse)
{-# DEPRECATED dsursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
