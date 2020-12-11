{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBClusterParameterGroup@ descriptions. If a @DBClusterParameterGroupName@ parameter is specified, the list will contain only the description of the specified DB cluster parameter group.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterParameterGroups
  ( -- * Creating a request
    DescribeDBClusterParameterGroups (..),
    mkDescribeDBClusterParameterGroups,

    -- ** Request lenses
    ddcpgFilters,
    ddcpgMarker,
    ddcpgMaxRecords,
    ddcpgDBClusterParameterGroupName,

    -- * Destructuring the response
    DescribeDBClusterParameterGroupsResponse (..),
    mkDescribeDBClusterParameterGroupsResponse,

    -- ** Response lenses
    ddcpgrsMarker,
    ddcpgrsDBClusterParameterGroups,
    ddcpgrsResponseStatus,
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
-- /See:/ 'mkDescribeDBClusterParameterGroups' smart constructor.
data DescribeDBClusterParameterGroups = DescribeDBClusterParameterGroups'
  { filters ::
      Lude.Maybe [Filter],
    marker ::
      Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    dbClusterParameterGroupName ::
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

-- | Creates a value of 'DescribeDBClusterParameterGroups' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroupName' - The name of a specific DB cluster parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBClusterParameterGroups ::
  DescribeDBClusterParameterGroups
mkDescribeDBClusterParameterGroups =
  DescribeDBClusterParameterGroups'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      dbClusterParameterGroupName = Lude.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpgFilters :: Lens.Lens' DescribeDBClusterParameterGroups (Lude.Maybe [Filter])
ddcpgFilters = Lens.lens (filters :: DescribeDBClusterParameterGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBClusterParameterGroups)
{-# DEPRECATED ddcpgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpgMarker :: Lens.Lens' DescribeDBClusterParameterGroups (Lude.Maybe Lude.Text)
ddcpgMarker = Lens.lens (marker :: DescribeDBClusterParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterParameterGroups)
{-# DEPRECATED ddcpgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpgMaxRecords :: Lens.Lens' DescribeDBClusterParameterGroups (Lude.Maybe Lude.Int)
ddcpgMaxRecords = Lens.lens (maxRecords :: DescribeDBClusterParameterGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBClusterParameterGroups)
{-# DEPRECATED ddcpgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of a specific DB cluster parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpgDBClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameterGroups (Lude.Maybe Lude.Text)
ddcpgDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: DescribeDBClusterParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: DescribeDBClusterParameterGroups)
{-# DEPRECATED ddcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

instance Page.AWSPager DescribeDBClusterParameterGroups where
  page rq rs
    | Page.stop (rs Lens.^. ddcpgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcpgrsDBClusterParameterGroups) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcpgMarker Lens..~ rs Lens.^. ddcpgrsMarker

instance Lude.AWSRequest DescribeDBClusterParameterGroups where
  type
    Rs DescribeDBClusterParameterGroups =
      DescribeDBClusterParameterGroupsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBClusterParameterGroupsResult"
      ( \s h x ->
          DescribeDBClusterParameterGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "DBClusterParameterGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBClusterParameterGroup")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBClusterParameterGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBClusterParameterGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBClusterParameterGroups where
  toQuery DescribeDBClusterParameterGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBClusterParameterGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName
      ]

-- |
--
-- /See:/ 'mkDescribeDBClusterParameterGroupsResponse' smart constructor.
data DescribeDBClusterParameterGroupsResponse = DescribeDBClusterParameterGroupsResponse'
  { marker ::
      Lude.Maybe
        Lude.Text,
    dbClusterParameterGroups ::
      Lude.Maybe
        [DBClusterParameterGroup],
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

-- | Creates a value of 'DescribeDBClusterParameterGroupsResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroups' - A list of DB cluster parameter groups.
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBClusterParameterGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBClusterParameterGroupsResponse
mkDescribeDBClusterParameterGroupsResponse pResponseStatus_ =
  DescribeDBClusterParameterGroupsResponse'
    { marker = Lude.Nothing,
      dbClusterParameterGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpgrsMarker :: Lens.Lens' DescribeDBClusterParameterGroupsResponse (Lude.Maybe Lude.Text)
ddcpgrsMarker = Lens.lens (marker :: DescribeDBClusterParameterGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterParameterGroupsResponse)
{-# DEPRECATED ddcpgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of DB cluster parameter groups.
--
-- /Note:/ Consider using 'dbClusterParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpgrsDBClusterParameterGroups :: Lens.Lens' DescribeDBClusterParameterGroupsResponse (Lude.Maybe [DBClusterParameterGroup])
ddcpgrsDBClusterParameterGroups = Lens.lens (dbClusterParameterGroups :: DescribeDBClusterParameterGroupsResponse -> Lude.Maybe [DBClusterParameterGroup]) (\s a -> s {dbClusterParameterGroups = a} :: DescribeDBClusterParameterGroupsResponse)
{-# DEPRECATED ddcpgrsDBClusterParameterGroups "Use generic-lens or generic-optics with 'dbClusterParameterGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpgrsResponseStatus :: Lens.Lens' DescribeDBClusterParameterGroupsResponse Lude.Int
ddcpgrsResponseStatus = Lens.lens (responseStatus :: DescribeDBClusterParameterGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBClusterParameterGroupsResponse)
{-# DEPRECATED ddcpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
