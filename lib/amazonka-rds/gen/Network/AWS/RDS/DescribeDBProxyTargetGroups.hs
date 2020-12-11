{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBProxyTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxy target groups, represented by @DBProxyTargetGroup@ data structures.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyTargetGroups
  ( -- * Creating a request
    DescribeDBProxyTargetGroups (..),
    mkDescribeDBProxyTargetGroups,

    -- ** Request lenses
    ddptgFilters,
    ddptgMarker,
    ddptgMaxRecords,
    ddptgTargetGroupName,
    ddptgDBProxyName,

    -- * Destructuring the response
    DescribeDBProxyTargetGroupsResponse (..),
    mkDescribeDBProxyTargetGroupsResponse,

    -- ** Response lenses
    ddptgrsMarker,
    ddptgrsTargetGroups,
    ddptgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDBProxyTargetGroups' smart constructor.
data DescribeDBProxyTargetGroups = DescribeDBProxyTargetGroups'
  { filters ::
      Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Natural,
    targetGroupName ::
      Lude.Maybe Lude.Text,
    dbProxyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBProxyTargetGroups' with the minimum fields required to make a request.
--
-- * 'dbProxyName' - The identifier of the @DBProxy@ associated with the target group.
-- * 'filters' - This parameter is not currently supported.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'targetGroupName' - The identifier of the @DBProxyTargetGroup@ to describe.
mkDescribeDBProxyTargetGroups ::
  -- | 'dbProxyName'
  Lude.Text ->
  DescribeDBProxyTargetGroups
mkDescribeDBProxyTargetGroups pDBProxyName_ =
  DescribeDBProxyTargetGroups'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      targetGroupName = Lude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | This parameter is not currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptgFilters :: Lens.Lens' DescribeDBProxyTargetGroups (Lude.Maybe [Filter])
ddptgFilters = Lens.lens (filters :: DescribeDBProxyTargetGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBProxyTargetGroups)
{-# DEPRECATED ddptgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptgMarker :: Lens.Lens' DescribeDBProxyTargetGroups (Lude.Maybe Lude.Text)
ddptgMarker = Lens.lens (marker :: DescribeDBProxyTargetGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBProxyTargetGroups)
{-# DEPRECATED ddptgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptgMaxRecords :: Lens.Lens' DescribeDBProxyTargetGroups (Lude.Maybe Lude.Natural)
ddptgMaxRecords = Lens.lens (maxRecords :: DescribeDBProxyTargetGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeDBProxyTargetGroups)
{-# DEPRECATED ddptgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The identifier of the @DBProxyTargetGroup@ to describe.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptgTargetGroupName :: Lens.Lens' DescribeDBProxyTargetGroups (Lude.Maybe Lude.Text)
ddptgTargetGroupName = Lens.lens (targetGroupName :: DescribeDBProxyTargetGroups -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupName = a} :: DescribeDBProxyTargetGroups)
{-# DEPRECATED ddptgTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

-- | The identifier of the @DBProxy@ associated with the target group.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptgDBProxyName :: Lens.Lens' DescribeDBProxyTargetGroups Lude.Text
ddptgDBProxyName = Lens.lens (dbProxyName :: DescribeDBProxyTargetGroups -> Lude.Text) (\s a -> s {dbProxyName = a} :: DescribeDBProxyTargetGroups)
{-# DEPRECATED ddptgDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

instance Page.AWSPager DescribeDBProxyTargetGroups where
  page rq rs
    | Page.stop (rs Lens.^. ddptgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddptgrsTargetGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddptgMarker Lens..~ rs Lens.^. ddptgrsMarker

instance Lude.AWSRequest DescribeDBProxyTargetGroups where
  type
    Rs DescribeDBProxyTargetGroups =
      DescribeDBProxyTargetGroupsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBProxyTargetGroupsResult"
      ( \s h x ->
          DescribeDBProxyTargetGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "TargetGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBProxyTargetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBProxyTargetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBProxyTargetGroups where
  toQuery DescribeDBProxyTargetGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBProxyTargetGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "TargetGroupName" Lude.=: targetGroupName,
        "DBProxyName" Lude.=: dbProxyName
      ]

-- | /See:/ 'mkDescribeDBProxyTargetGroupsResponse' smart constructor.
data DescribeDBProxyTargetGroupsResponse = DescribeDBProxyTargetGroupsResponse'
  { marker ::
      Lude.Maybe
        Lude.Text,
    targetGroups ::
      Lude.Maybe
        [DBProxyTargetGroup],
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

-- | Creates a value of 'DescribeDBProxyTargetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
-- * 'targetGroups' - An arbitrary number of @DBProxyTargetGroup@ objects, containing details of the corresponding target groups.
mkDescribeDBProxyTargetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBProxyTargetGroupsResponse
mkDescribeDBProxyTargetGroupsResponse pResponseStatus_ =
  DescribeDBProxyTargetGroupsResponse'
    { marker = Lude.Nothing,
      targetGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptgrsMarker :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Lude.Maybe Lude.Text)
ddptgrsMarker = Lens.lens (marker :: DescribeDBProxyTargetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBProxyTargetGroupsResponse)
{-# DEPRECATED ddptgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details of the corresponding target groups.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptgrsTargetGroups :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Lude.Maybe [DBProxyTargetGroup])
ddptgrsTargetGroups = Lens.lens (targetGroups :: DescribeDBProxyTargetGroupsResponse -> Lude.Maybe [DBProxyTargetGroup]) (\s a -> s {targetGroups = a} :: DescribeDBProxyTargetGroupsResponse)
{-# DEPRECATED ddptgrsTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptgrsResponseStatus :: Lens.Lens' DescribeDBProxyTargetGroupsResponse Lude.Int
ddptgrsResponseStatus = Lens.lens (responseStatus :: DescribeDBProxyTargetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBProxyTargetGroupsResponse)
{-# DEPRECATED ddptgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
