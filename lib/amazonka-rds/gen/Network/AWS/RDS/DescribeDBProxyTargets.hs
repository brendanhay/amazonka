{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about @DBProxyTarget@ objects. This API supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyTargets
  ( -- * Creating a request
    DescribeDBProxyTargets (..),
    mkDescribeDBProxyTargets,

    -- ** Request lenses
    ddptFilters,
    ddptMarker,
    ddptMaxRecords,
    ddptDBProxyName,
    ddptTargetGroupName,

    -- * Destructuring the response
    DescribeDBProxyTargetsResponse (..),
    mkDescribeDBProxyTargetsResponse,

    -- ** Response lenses
    ddbptrsTargets,
    ddbptrsMarker,
    ddbptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDBProxyTargets' smart constructor.
data DescribeDBProxyTargets = DescribeDBProxyTargets'
  { -- | This parameter is not currently supported.
    filters :: Lude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Natural,
    -- | The identifier of the @DBProxyTarget@ to describe.
    dbProxyName :: Lude.Text,
    -- | The identifier of the @DBProxyTargetGroup@ to describe.
    targetGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBProxyTargets' with the minimum fields required to make a request.
--
-- * 'filters' - This parameter is not currently supported.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'dbProxyName' - The identifier of the @DBProxyTarget@ to describe.
-- * 'targetGroupName' - The identifier of the @DBProxyTargetGroup@ to describe.
mkDescribeDBProxyTargets ::
  -- | 'dbProxyName'
  Lude.Text ->
  DescribeDBProxyTargets
mkDescribeDBProxyTargets pDBProxyName_ =
  DescribeDBProxyTargets'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      dbProxyName = pDBProxyName_,
      targetGroupName = Lude.Nothing
    }

-- | This parameter is not currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptFilters :: Lens.Lens' DescribeDBProxyTargets (Lude.Maybe [Filter])
ddptFilters = Lens.lens (filters :: DescribeDBProxyTargets -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBProxyTargets)
{-# DEPRECATED ddptFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptMarker :: Lens.Lens' DescribeDBProxyTargets (Lude.Maybe Lude.Text)
ddptMarker = Lens.lens (marker :: DescribeDBProxyTargets -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBProxyTargets)
{-# DEPRECATED ddptMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptMaxRecords :: Lens.Lens' DescribeDBProxyTargets (Lude.Maybe Lude.Natural)
ddptMaxRecords = Lens.lens (maxRecords :: DescribeDBProxyTargets -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeDBProxyTargets)
{-# DEPRECATED ddptMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The identifier of the @DBProxyTarget@ to describe.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptDBProxyName :: Lens.Lens' DescribeDBProxyTargets Lude.Text
ddptDBProxyName = Lens.lens (dbProxyName :: DescribeDBProxyTargets -> Lude.Text) (\s a -> s {dbProxyName = a} :: DescribeDBProxyTargets)
{-# DEPRECATED ddptDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

-- | The identifier of the @DBProxyTargetGroup@ to describe.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptTargetGroupName :: Lens.Lens' DescribeDBProxyTargets (Lude.Maybe Lude.Text)
ddptTargetGroupName = Lens.lens (targetGroupName :: DescribeDBProxyTargets -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupName = a} :: DescribeDBProxyTargets)
{-# DEPRECATED ddptTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

instance Page.AWSPager DescribeDBProxyTargets where
  page rq rs
    | Page.stop (rs Lens.^. ddbptrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddbptrsTargets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddptMarker Lens..~ rs Lens.^. ddbptrsMarker

instance Lude.AWSRequest DescribeDBProxyTargets where
  type Rs DescribeDBProxyTargets = DescribeDBProxyTargetsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBProxyTargetsResult"
      ( \s h x ->
          DescribeDBProxyTargetsResponse'
            Lude.<$> ( x Lude..@? "Targets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBProxyTargets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBProxyTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBProxyTargets where
  toQuery DescribeDBProxyTargets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBProxyTargets" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "DBProxyName" Lude.=: dbProxyName,
        "TargetGroupName" Lude.=: targetGroupName
      ]

-- | /See:/ 'mkDescribeDBProxyTargetsResponse' smart constructor.
data DescribeDBProxyTargetsResponse = DescribeDBProxyTargetsResponse'
  { -- | An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
    targets :: Lude.Maybe [DBProxyTarget],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBProxyTargetsResponse' with the minimum fields required to make a request.
--
-- * 'targets' - An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBProxyTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBProxyTargetsResponse
mkDescribeDBProxyTargetsResponse pResponseStatus_ =
  DescribeDBProxyTargetsResponse'
    { targets = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrsTargets :: Lens.Lens' DescribeDBProxyTargetsResponse (Lude.Maybe [DBProxyTarget])
ddbptrsTargets = Lens.lens (targets :: DescribeDBProxyTargetsResponse -> Lude.Maybe [DBProxyTarget]) (\s a -> s {targets = a} :: DescribeDBProxyTargetsResponse)
{-# DEPRECATED ddbptrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrsMarker :: Lens.Lens' DescribeDBProxyTargetsResponse (Lude.Maybe Lude.Text)
ddbptrsMarker = Lens.lens (marker :: DescribeDBProxyTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBProxyTargetsResponse)
{-# DEPRECATED ddbptrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrsResponseStatus :: Lens.Lens' DescribeDBProxyTargetsResponse Lude.Int
ddbptrsResponseStatus = Lens.lens (responseStatus :: DescribeDBProxyTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBProxyTargetsResponse)
{-# DEPRECATED ddbptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
