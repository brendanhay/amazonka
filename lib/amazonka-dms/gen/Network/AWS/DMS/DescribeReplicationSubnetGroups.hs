{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationSubnetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication subnet groups.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationSubnetGroups
  ( -- * Creating a request
    DescribeReplicationSubnetGroups (..),
    mkDescribeReplicationSubnetGroups,

    -- ** Request lenses
    drsgFilters,
    drsgMarker,
    drsgMaxRecords,

    -- * Destructuring the response
    DescribeReplicationSubnetGroupsResponse (..),
    mkDescribeReplicationSubnetGroupsResponse,

    -- ** Response lenses
    drsgsrsMarker,
    drsgsrsReplicationSubnetGroups,
    drsgsrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeReplicationSubnetGroups' smart constructor.
data DescribeReplicationSubnetGroups = DescribeReplicationSubnetGroups'
  { -- | Filters applied to replication subnet groups.
    --
    -- Valid filter names: replication-subnet-group-id
    filters :: Lude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplicationSubnetGroups' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to replication subnet groups.
--
-- Valid filter names: replication-subnet-group-id
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeReplicationSubnetGroups ::
  DescribeReplicationSubnetGroups
mkDescribeReplicationSubnetGroups =
  DescribeReplicationSubnetGroups'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filters applied to replication subnet groups.
--
-- Valid filter names: replication-subnet-group-id
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgFilters :: Lens.Lens' DescribeReplicationSubnetGroups (Lude.Maybe [Filter])
drsgFilters = Lens.lens (filters :: DescribeReplicationSubnetGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReplicationSubnetGroups)
{-# DEPRECATED drsgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgMarker :: Lens.Lens' DescribeReplicationSubnetGroups (Lude.Maybe Lude.Text)
drsgMarker = Lens.lens (marker :: DescribeReplicationSubnetGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationSubnetGroups)
{-# DEPRECATED drsgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgMaxRecords :: Lens.Lens' DescribeReplicationSubnetGroups (Lude.Maybe Lude.Int)
drsgMaxRecords = Lens.lens (maxRecords :: DescribeReplicationSubnetGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationSubnetGroups)
{-# DEPRECATED drsgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeReplicationSubnetGroups where
  page rq rs
    | Page.stop (rs Lens.^. drsgsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drsgsrsReplicationSubnetGroups) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drsgMarker Lens..~ rs Lens.^. drsgsrsMarker

instance Lude.AWSRequest DescribeReplicationSubnetGroups where
  type
    Rs DescribeReplicationSubnetGroups =
      DescribeReplicationSubnetGroupsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReplicationSubnetGroupsResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "ReplicationSubnetGroups" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReplicationSubnetGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeReplicationSubnetGroups" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReplicationSubnetGroups where
  toJSON DescribeReplicationSubnetGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeReplicationSubnetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplicationSubnetGroups where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeReplicationSubnetGroupsResponse' smart constructor.
data DescribeReplicationSubnetGroupsResponse = DescribeReplicationSubnetGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | A description of the replication subnet groups.
    replicationSubnetGroups :: Lude.Maybe [ReplicationSubnetGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplicationSubnetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'replicationSubnetGroups' - A description of the replication subnet groups.
-- * 'responseStatus' - The response status code.
mkDescribeReplicationSubnetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplicationSubnetGroupsResponse
mkDescribeReplicationSubnetGroupsResponse pResponseStatus_ =
  DescribeReplicationSubnetGroupsResponse'
    { marker = Lude.Nothing,
      replicationSubnetGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgsrsMarker :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Lude.Maybe Lude.Text)
drsgsrsMarker = Lens.lens (marker :: DescribeReplicationSubnetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationSubnetGroupsResponse)
{-# DEPRECATED drsgsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A description of the replication subnet groups.
--
-- /Note:/ Consider using 'replicationSubnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgsrsReplicationSubnetGroups :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Lude.Maybe [ReplicationSubnetGroup])
drsgsrsReplicationSubnetGroups = Lens.lens (replicationSubnetGroups :: DescribeReplicationSubnetGroupsResponse -> Lude.Maybe [ReplicationSubnetGroup]) (\s a -> s {replicationSubnetGroups = a} :: DescribeReplicationSubnetGroupsResponse)
{-# DEPRECATED drsgsrsReplicationSubnetGroups "Use generic-lens or generic-optics with 'replicationSubnetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgsrsResponseStatus :: Lens.Lens' DescribeReplicationSubnetGroupsResponse Lude.Int
drsgsrsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationSubnetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationSubnetGroupsResponse)
{-# DEPRECATED drsgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
