{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resources (for example, DB instances) that have at least one pending maintenance action.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribePendingMaintenanceActions
  ( -- * Creating a request
    DescribePendingMaintenanceActions (..),
    mkDescribePendingMaintenanceActions,

    -- ** Request lenses
    dpmaFilters,
    dpmaMarker,
    dpmaMaxRecords,
    dpmaResourceIdentifier,

    -- * Destructuring the response
    DescribePendingMaintenanceActionsResponse (..),
    mkDescribePendingMaintenanceActionsResponse,

    -- ** Response lenses
    dpmarsPendingMaintenanceActions,
    dpmarsMarker,
    dpmarsResponseStatus,
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
-- /See:/ 'mkDescribePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { -- | A filter that specifies one or more resources to return pending maintenance actions for.
    --
    -- Supported filters:
    --
    --     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include pending maintenance actions for the DB clusters identified by these ARNs.
    --
    --
    --     * @db-instance-id@ - Accepts DB instance identifiers and DB instance ARNs. The results list will only include pending maintenance actions for the DB instances identified by these ARNs.
    filters :: Lude.Maybe [Filter],
    -- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The ARN of a resource to return pending maintenance actions for.
    resourceIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- * 'filters' - A filter that specifies one or more resources to return pending maintenance actions for.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include pending maintenance actions for the DB clusters identified by these ARNs.
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance ARNs. The results list will only include pending maintenance actions for the DB instances identified by these ARNs.
--
--
-- * 'marker' - An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'resourceIdentifier' - The ARN of a resource to return pending maintenance actions for.
mkDescribePendingMaintenanceActions ::
  DescribePendingMaintenanceActions
mkDescribePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      resourceIdentifier = Lude.Nothing
    }

-- | A filter that specifies one or more resources to return pending maintenance actions for.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include pending maintenance actions for the DB clusters identified by these ARNs.
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance ARNs. The results list will only include pending maintenance actions for the DB instances identified by these ARNs.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaFilters :: Lens.Lens' DescribePendingMaintenanceActions (Lude.Maybe [Filter])
dpmaFilters = Lens.lens (filters :: DescribePendingMaintenanceActions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribePendingMaintenanceActions)
{-# DEPRECATED dpmaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaMarker :: Lens.Lens' DescribePendingMaintenanceActions (Lude.Maybe Lude.Text)
dpmaMarker = Lens.lens (marker :: DescribePendingMaintenanceActions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribePendingMaintenanceActions)
{-# DEPRECATED dpmaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaMaxRecords :: Lens.Lens' DescribePendingMaintenanceActions (Lude.Maybe Lude.Int)
dpmaMaxRecords = Lens.lens (maxRecords :: DescribePendingMaintenanceActions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribePendingMaintenanceActions)
{-# DEPRECATED dpmaMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The ARN of a resource to return pending maintenance actions for.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaResourceIdentifier :: Lens.Lens' DescribePendingMaintenanceActions (Lude.Maybe Lude.Text)
dpmaResourceIdentifier = Lens.lens (resourceIdentifier :: DescribePendingMaintenanceActions -> Lude.Maybe Lude.Text) (\s a -> s {resourceIdentifier = a} :: DescribePendingMaintenanceActions)
{-# DEPRECATED dpmaResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Page.AWSPager DescribePendingMaintenanceActions where
  page rq rs
    | Page.stop (rs Lens.^. dpmarsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dpmarsPendingMaintenanceActions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpmaMarker Lens..~ rs Lens.^. dpmarsMarker

instance Lude.AWSRequest DescribePendingMaintenanceActions where
  type
    Rs DescribePendingMaintenanceActions =
      DescribePendingMaintenanceActionsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribePendingMaintenanceActionsResult"
      ( \s h x ->
          DescribePendingMaintenanceActionsResponse'
            Lude.<$> ( x Lude..@? "PendingMaintenanceActions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ResourcePendingMaintenanceActions")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePendingMaintenanceActions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribePendingMaintenanceActions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePendingMaintenanceActions where
  toQuery DescribePendingMaintenanceActions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribePendingMaintenanceActions" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ResourceIdentifier" Lude.=: resourceIdentifier
      ]

-- | Data returned from the __DescribePendingMaintenanceActions__ action.
--
-- /See:/ 'mkDescribePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { -- | A list of the pending maintenance actions for the resource.
    pendingMaintenanceActions :: Lude.Maybe [ResourcePendingMaintenanceActions],
    -- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePendingMaintenanceActionsResponse' with the minimum fields required to make a request.
--
-- * 'pendingMaintenanceActions' - A list of the pending maintenance actions for the resource.
-- * 'marker' - An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
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

-- | A list of the pending maintenance actions for the resource.
--
-- /Note:/ Consider using 'pendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarsPendingMaintenanceActions :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Lude.Maybe [ResourcePendingMaintenanceActions])
dpmarsPendingMaintenanceActions = Lens.lens (pendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> Lude.Maybe [ResourcePendingMaintenanceActions]) (\s a -> s {pendingMaintenanceActions = a} :: DescribePendingMaintenanceActionsResponse)
{-# DEPRECATED dpmarsPendingMaintenanceActions "Use generic-lens or generic-optics with 'pendingMaintenanceActions' instead." #-}

-- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
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
