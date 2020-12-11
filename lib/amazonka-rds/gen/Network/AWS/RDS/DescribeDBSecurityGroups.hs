{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBSecurityGroup@ descriptions. If a @DBSecurityGroupName@ is specified, the list will contain only the descriptions of the specified DB security group.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSecurityGroups
  ( -- * Creating a request
    DescribeDBSecurityGroups (..),
    mkDescribeDBSecurityGroups,

    -- ** Request lenses
    ddbsgFilters,
    ddbsgMarker,
    ddbsgMaxRecords,
    ddbsgDBSecurityGroupName,

    -- * Destructuring the response
    DescribeDBSecurityGroupsResponse (..),
    mkDescribeDBSecurityGroupsResponse,

    -- ** Response lenses
    ddbsgrsDBSecurityGroups,
    ddbsgrsMarker,
    ddbsgrsResponseStatus,
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
-- /See:/ 'mkDescribeDBSecurityGroups' smart constructor.
data DescribeDBSecurityGroups = DescribeDBSecurityGroups'
  { filters ::
      Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    dbSecurityGroupName ::
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

-- | Creates a value of 'DescribeDBSecurityGroups' with the minimum fields required to make a request.
--
-- * 'dbSecurityGroupName' - The name of the DB security group to return details for.
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBSecurityGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBSecurityGroups ::
  DescribeDBSecurityGroups
mkDescribeDBSecurityGroups =
  DescribeDBSecurityGroups'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      dbSecurityGroupName = Lude.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgFilters :: Lens.Lens' DescribeDBSecurityGroups (Lude.Maybe [Filter])
ddbsgFilters = Lens.lens (filters :: DescribeDBSecurityGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBSecurityGroups)
{-# DEPRECATED ddbsgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBSecurityGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgMarker :: Lens.Lens' DescribeDBSecurityGroups (Lude.Maybe Lude.Text)
ddbsgMarker = Lens.lens (marker :: DescribeDBSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBSecurityGroups)
{-# DEPRECATED ddbsgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgMaxRecords :: Lens.Lens' DescribeDBSecurityGroups (Lude.Maybe Lude.Int)
ddbsgMaxRecords = Lens.lens (maxRecords :: DescribeDBSecurityGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBSecurityGroups)
{-# DEPRECATED ddbsgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the DB security group to return details for.
--
-- /Note:/ Consider using 'dbSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgDBSecurityGroupName :: Lens.Lens' DescribeDBSecurityGroups (Lude.Maybe Lude.Text)
ddbsgDBSecurityGroupName = Lens.lens (dbSecurityGroupName :: DescribeDBSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {dbSecurityGroupName = a} :: DescribeDBSecurityGroups)
{-# DEPRECATED ddbsgDBSecurityGroupName "Use generic-lens or generic-optics with 'dbSecurityGroupName' instead." #-}

instance Page.AWSPager DescribeDBSecurityGroups where
  page rq rs
    | Page.stop (rs Lens.^. ddbsgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddbsgrsDBSecurityGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddbsgMarker Lens..~ rs Lens.^. ddbsgrsMarker

instance Lude.AWSRequest DescribeDBSecurityGroups where
  type Rs DescribeDBSecurityGroups = DescribeDBSecurityGroupsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBSecurityGroupsResult"
      ( \s h x ->
          DescribeDBSecurityGroupsResponse'
            Lude.<$> ( x Lude..@? "DBSecurityGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBSecurityGroup")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBSecurityGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBSecurityGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBSecurityGroups where
  toQuery DescribeDBSecurityGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBSecurityGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "DBSecurityGroupName" Lude.=: dbSecurityGroupName
      ]

-- | Contains the result of a successful invocation of the @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'mkDescribeDBSecurityGroupsResponse' smart constructor.
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'
  { dbSecurityGroups ::
      Lude.Maybe
        [DBSecurityGroup],
    marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeDBSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- * 'dbSecurityGroups' - A list of @DBSecurityGroup@ instances.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBSecurityGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBSecurityGroupsResponse
mkDescribeDBSecurityGroupsResponse pResponseStatus_ =
  DescribeDBSecurityGroupsResponse'
    { dbSecurityGroups =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @DBSecurityGroup@ instances.
--
-- /Note:/ Consider using 'dbSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrsDBSecurityGroups :: Lens.Lens' DescribeDBSecurityGroupsResponse (Lude.Maybe [DBSecurityGroup])
ddbsgrsDBSecurityGroups = Lens.lens (dbSecurityGroups :: DescribeDBSecurityGroupsResponse -> Lude.Maybe [DBSecurityGroup]) (\s a -> s {dbSecurityGroups = a} :: DescribeDBSecurityGroupsResponse)
{-# DEPRECATED ddbsgrsDBSecurityGroups "Use generic-lens or generic-optics with 'dbSecurityGroups' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrsMarker :: Lens.Lens' DescribeDBSecurityGroupsResponse (Lude.Maybe Lude.Text)
ddbsgrsMarker = Lens.lens (marker :: DescribeDBSecurityGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBSecurityGroupsResponse)
{-# DEPRECATED ddbsgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrsResponseStatus :: Lens.Lens' DescribeDBSecurityGroupsResponse Lude.Int
ddbsgrsResponseStatus = Lens.lens (responseStatus :: DescribeDBSecurityGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBSecurityGroupsResponse)
{-# DEPRECATED ddbsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
