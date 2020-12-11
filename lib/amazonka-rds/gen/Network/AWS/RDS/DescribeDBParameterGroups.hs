{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBParameterGroup@ descriptions. If a @DBParameterGroupName@ is specified, the list will contain only the description of the specified DB parameter group.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBParameterGroups
  ( -- * Creating a request
    DescribeDBParameterGroups (..),
    mkDescribeDBParameterGroups,

    -- ** Request lenses
    ddpgFilters,
    ddpgDBParameterGroupName,
    ddpgMarker,
    ddpgMaxRecords,

    -- * Destructuring the response
    DescribeDBParameterGroupsResponse (..),
    mkDescribeDBParameterGroupsResponse,

    -- ** Response lenses
    ddpgrsMarker,
    ddpgrsDBParameterGroups,
    ddpgrsResponseStatus,
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
-- /See:/ 'mkDescribeDBParameterGroups' smart constructor.
data DescribeDBParameterGroups = DescribeDBParameterGroups'
  { filters ::
      Lude.Maybe [Filter],
    dbParameterGroupName ::
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

-- | Creates a value of 'DescribeDBParameterGroups' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupName' - The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBParameterGroups ::
  DescribeDBParameterGroups
mkDescribeDBParameterGroups =
  DescribeDBParameterGroups'
    { filters = Lude.Nothing,
      dbParameterGroupName = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpgFilters :: Lens.Lens' DescribeDBParameterGroups (Lude.Maybe [Filter])
ddpgFilters = Lens.lens (filters :: DescribeDBParameterGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBParameterGroups)
{-# DEPRECATED ddpgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpgDBParameterGroupName :: Lens.Lens' DescribeDBParameterGroups (Lude.Maybe Lude.Text)
ddpgDBParameterGroupName = Lens.lens (dbParameterGroupName :: DescribeDBParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: DescribeDBParameterGroups)
{-# DEPRECATED ddpgDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpgMarker :: Lens.Lens' DescribeDBParameterGroups (Lude.Maybe Lude.Text)
ddpgMarker = Lens.lens (marker :: DescribeDBParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBParameterGroups)
{-# DEPRECATED ddpgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpgMaxRecords :: Lens.Lens' DescribeDBParameterGroups (Lude.Maybe Lude.Int)
ddpgMaxRecords = Lens.lens (maxRecords :: DescribeDBParameterGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBParameterGroups)
{-# DEPRECATED ddpgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeDBParameterGroups where
  page rq rs
    | Page.stop (rs Lens.^. ddpgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddpgrsDBParameterGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddpgMarker Lens..~ rs Lens.^. ddpgrsMarker

instance Lude.AWSRequest DescribeDBParameterGroups where
  type
    Rs DescribeDBParameterGroups =
      DescribeDBParameterGroupsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBParameterGroupsResult"
      ( \s h x ->
          DescribeDBParameterGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "DBParameterGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBParameterGroup")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBParameterGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBParameterGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBParameterGroups where
  toQuery DescribeDBParameterGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBParameterGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the @DescribeDBParameterGroups@ action.
--
-- /See:/ 'mkDescribeDBParameterGroupsResponse' smart constructor.
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    dbParameterGroups ::
      Lude.Maybe
        [DBParameterGroup],
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

-- | Creates a value of 'DescribeDBParameterGroupsResponse' with the minimum fields required to make a request.
--
-- * 'dbParameterGroups' - A list of @DBParameterGroup@ instances.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBParameterGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBParameterGroupsResponse
mkDescribeDBParameterGroupsResponse pResponseStatus_ =
  DescribeDBParameterGroupsResponse'
    { marker = Lude.Nothing,
      dbParameterGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpgrsMarker :: Lens.Lens' DescribeDBParameterGroupsResponse (Lude.Maybe Lude.Text)
ddpgrsMarker = Lens.lens (marker :: DescribeDBParameterGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBParameterGroupsResponse)
{-# DEPRECATED ddpgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of @DBParameterGroup@ instances.
--
-- /Note:/ Consider using 'dbParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpgrsDBParameterGroups :: Lens.Lens' DescribeDBParameterGroupsResponse (Lude.Maybe [DBParameterGroup])
ddpgrsDBParameterGroups = Lens.lens (dbParameterGroups :: DescribeDBParameterGroupsResponse -> Lude.Maybe [DBParameterGroup]) (\s a -> s {dbParameterGroups = a} :: DescribeDBParameterGroupsResponse)
{-# DEPRECATED ddpgrsDBParameterGroups "Use generic-lens or generic-optics with 'dbParameterGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpgrsResponseStatus :: Lens.Lens' DescribeDBParameterGroupsResponse Lude.Int
ddpgrsResponseStatus = Lens.lens (responseStatus :: DescribeDBParameterGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBParameterGroupsResponse)
{-# DEPRECATED ddpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
