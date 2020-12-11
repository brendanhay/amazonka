{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is specified, the list will contain only the descriptions of the specified DBSubnetGroup.
--
-- For an overview of CIDR ranges, go to the <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial> .
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSubnetGroups
  ( -- * Creating a request
    DescribeDBSubnetGroups (..),
    mkDescribeDBSubnetGroups,

    -- ** Request lenses
    ddsgDBSubnetGroupName,
    ddsgFilters,
    ddsgMarker,
    ddsgMaxRecords,

    -- * Destructuring the response
    DescribeDBSubnetGroupsResponse (..),
    mkDescribeDBSubnetGroupsResponse,

    -- ** Response lenses
    ddsgrsDBSubnetGroups,
    ddsgrsMarker,
    ddsgrsResponseStatus,
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
-- /See:/ 'mkDescribeDBSubnetGroups' smart constructor.
data DescribeDBSubnetGroups = DescribeDBSubnetGroups'
  { dbSubnetGroupName ::
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

-- | Creates a value of 'DescribeDBSubnetGroups' with the minimum fields required to make a request.
--
-- * 'dbSubnetGroupName' - The name of the DB subnet group to return details for.
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous DescribeDBSubnetGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBSubnetGroups ::
  DescribeDBSubnetGroups
mkDescribeDBSubnetGroups =
  DescribeDBSubnetGroups'
    { dbSubnetGroupName = Lude.Nothing,
      filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the DB subnet group to return details for.
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsgDBSubnetGroupName :: Lens.Lens' DescribeDBSubnetGroups (Lude.Maybe Lude.Text)
ddsgDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: DescribeDBSubnetGroups -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: DescribeDBSubnetGroups)
{-# DEPRECATED ddsgDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsgFilters :: Lens.Lens' DescribeDBSubnetGroups (Lude.Maybe [Filter])
ddsgFilters = Lens.lens (filters :: DescribeDBSubnetGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBSubnetGroups)
{-# DEPRECATED ddsgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous DescribeDBSubnetGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsgMarker :: Lens.Lens' DescribeDBSubnetGroups (Lude.Maybe Lude.Text)
ddsgMarker = Lens.lens (marker :: DescribeDBSubnetGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBSubnetGroups)
{-# DEPRECATED ddsgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsgMaxRecords :: Lens.Lens' DescribeDBSubnetGroups (Lude.Maybe Lude.Int)
ddsgMaxRecords = Lens.lens (maxRecords :: DescribeDBSubnetGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBSubnetGroups)
{-# DEPRECATED ddsgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeDBSubnetGroups where
  page rq rs
    | Page.stop (rs Lens.^. ddsgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddsgrsDBSubnetGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddsgMarker Lens..~ rs Lens.^. ddsgrsMarker

instance Lude.AWSRequest DescribeDBSubnetGroups where
  type Rs DescribeDBSubnetGroups = DescribeDBSubnetGroupsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBSubnetGroupsResult"
      ( \s h x ->
          DescribeDBSubnetGroupsResponse'
            Lude.<$> ( x Lude..@? "DBSubnetGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBSubnetGroup")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBSubnetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBSubnetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBSubnetGroups where
  toQuery DescribeDBSubnetGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBSubnetGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the @DescribeDBSubnetGroups@ action.
--
-- /See:/ 'mkDescribeDBSubnetGroupsResponse' smart constructor.
data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse'
  { dbSubnetGroups ::
      Lude.Maybe [DBSubnetGroup],
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

-- | Creates a value of 'DescribeDBSubnetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'dbSubnetGroups' - A list of @DBSubnetGroup@ instances.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBSubnetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBSubnetGroupsResponse
mkDescribeDBSubnetGroupsResponse pResponseStatus_ =
  DescribeDBSubnetGroupsResponse'
    { dbSubnetGroups = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @DBSubnetGroup@ instances.
--
-- /Note:/ Consider using 'dbSubnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsgrsDBSubnetGroups :: Lens.Lens' DescribeDBSubnetGroupsResponse (Lude.Maybe [DBSubnetGroup])
ddsgrsDBSubnetGroups = Lens.lens (dbSubnetGroups :: DescribeDBSubnetGroupsResponse -> Lude.Maybe [DBSubnetGroup]) (\s a -> s {dbSubnetGroups = a} :: DescribeDBSubnetGroupsResponse)
{-# DEPRECATED ddsgrsDBSubnetGroups "Use generic-lens or generic-optics with 'dbSubnetGroups' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsgrsMarker :: Lens.Lens' DescribeDBSubnetGroupsResponse (Lude.Maybe Lude.Text)
ddsgrsMarker = Lens.lens (marker :: DescribeDBSubnetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBSubnetGroupsResponse)
{-# DEPRECATED ddsgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsgrsResponseStatus :: Lens.Lens' DescribeDBSubnetGroupsResponse Lude.Int
ddsgrsResponseStatus = Lens.lens (responseStatus :: DescribeDBSubnetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBSubnetGroupsResponse)
{-# DEPRECATED ddsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
