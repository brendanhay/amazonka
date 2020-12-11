{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For internal use only
module Network.AWS.DMS.DescribePendingMaintenanceActions
  ( -- * Creating a request
    DescribePendingMaintenanceActions (..),
    mkDescribePendingMaintenanceActions,

    -- ** Request lenses
    dpmaFilters,
    dpmaMarker,
    dpmaMaxRecords,
    dpmaReplicationInstanceARN,

    -- * Destructuring the response
    DescribePendingMaintenanceActionsResponse (..),
    mkDescribePendingMaintenanceActionsResponse,

    -- ** Response lenses
    dpmarsPendingMaintenanceActions,
    dpmarsMarker,
    dpmarsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { filters ::
      Lude.Maybe [Filter],
    marker ::
      Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    replicationInstanceARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- * 'filters' -
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
mkDescribePendingMaintenanceActions ::
  DescribePendingMaintenanceActions
mkDescribePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      replicationInstanceARN = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaFilters :: Lens.Lens' DescribePendingMaintenanceActions (Lude.Maybe [Filter])
dpmaFilters = Lens.lens (filters :: DescribePendingMaintenanceActions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribePendingMaintenanceActions)
{-# DEPRECATED dpmaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaMarker :: Lens.Lens' DescribePendingMaintenanceActions (Lude.Maybe Lude.Text)
dpmaMarker = Lens.lens (marker :: DescribePendingMaintenanceActions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribePendingMaintenanceActions)
{-# DEPRECATED dpmaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaMaxRecords :: Lens.Lens' DescribePendingMaintenanceActions (Lude.Maybe Lude.Int)
dpmaMaxRecords = Lens.lens (maxRecords :: DescribePendingMaintenanceActions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribePendingMaintenanceActions)
{-# DEPRECATED dpmaMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaReplicationInstanceARN :: Lens.Lens' DescribePendingMaintenanceActions (Lude.Maybe Lude.Text)
dpmaReplicationInstanceARN = Lens.lens (replicationInstanceARN :: DescribePendingMaintenanceActions -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceARN = a} :: DescribePendingMaintenanceActions)
{-# DEPRECATED dpmaReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

instance Lude.AWSRequest DescribePendingMaintenanceActions where
  type
    Rs DescribePendingMaintenanceActions =
      DescribePendingMaintenanceActionsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePendingMaintenanceActionsResponse'
            Lude.<$> (x Lude..?> "PendingMaintenanceActions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePendingMaintenanceActions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribePendingMaintenanceActions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePendingMaintenanceActions where
  toJSON DescribePendingMaintenanceActions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords,
            ("ReplicationInstanceArn" Lude..=)
              Lude.<$> replicationInstanceARN
          ]
      )

instance Lude.ToPath DescribePendingMaintenanceActions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePendingMaintenanceActions where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { pendingMaintenanceActions ::
      Lude.Maybe
        [ResourcePendingMaintenanceActions],
    marker ::
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

-- | Creates a value of 'DescribePendingMaintenanceActionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'pendingMaintenanceActions' - The pending maintenance action.
-- * 'responseStatus' - The response status code.
mkDescribePendingMaintenanceActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePendingMaintenanceActionsResponse
mkDescribePendingMaintenanceActionsResponse pResponseStatus_ =
  DescribePendingMaintenanceActionsResponse'
    { pendingMaintenanceActions =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pending maintenance action.
--
-- /Note:/ Consider using 'pendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarsPendingMaintenanceActions :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Lude.Maybe [ResourcePendingMaintenanceActions])
dpmarsPendingMaintenanceActions = Lens.lens (pendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> Lude.Maybe [ResourcePendingMaintenanceActions]) (\s a -> s {pendingMaintenanceActions = a} :: DescribePendingMaintenanceActionsResponse)
{-# DEPRECATED dpmarsPendingMaintenanceActions "Use generic-lens or generic-optics with 'pendingMaintenanceActions' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarsMarker :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Lude.Maybe Lude.Text)
dpmarsMarker = Lens.lens (marker :: DescribePendingMaintenanceActionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribePendingMaintenanceActionsResponse)
{-# DEPRECATED dpmarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarsResponseStatus :: Lens.Lens' DescribePendingMaintenanceActionsResponse Lude.Int
dpmarsResponseStatus = Lens.lens (responseStatus :: DescribePendingMaintenanceActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePendingMaintenanceActionsResponse)
{-# DEPRECATED dpmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
