{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a particular global replication group. If no identifier is specified, returns information about all Global Datastores.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
  ( -- * Creating a request
    DescribeGlobalReplicationGroups (..),
    mkDescribeGlobalReplicationGroups,

    -- ** Request lenses
    dgrgShowMemberInfo,
    dgrgMarker,
    dgrgMaxRecords,
    dgrgGlobalReplicationGroupId,

    -- * Destructuring the response
    DescribeGlobalReplicationGroupsResponse (..),
    mkDescribeGlobalReplicationGroupsResponse,

    -- ** Response lenses
    dgrgsrsMarker,
    dgrgsrsGlobalReplicationGroups,
    dgrgsrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeGlobalReplicationGroups' smart constructor.
data DescribeGlobalReplicationGroups = DescribeGlobalReplicationGroups'
  { -- | Returns the list of members that comprise the Global Datastore.
    showMemberInfo :: Lude.Maybe Lude.Bool,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGlobalReplicationGroups' with the minimum fields required to make a request.
--
-- * 'showMemberInfo' - Returns the list of members that comprise the Global Datastore.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
-- * 'globalReplicationGroupId' - The name of the Global Datastore
mkDescribeGlobalReplicationGroups ::
  DescribeGlobalReplicationGroups
mkDescribeGlobalReplicationGroups =
  DescribeGlobalReplicationGroups'
    { showMemberInfo = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      globalReplicationGroupId = Lude.Nothing
    }

-- | Returns the list of members that comprise the Global Datastore.
--
-- /Note:/ Consider using 'showMemberInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgShowMemberInfo :: Lens.Lens' DescribeGlobalReplicationGroups (Lude.Maybe Lude.Bool)
dgrgShowMemberInfo = Lens.lens (showMemberInfo :: DescribeGlobalReplicationGroups -> Lude.Maybe Lude.Bool) (\s a -> s {showMemberInfo = a} :: DescribeGlobalReplicationGroups)
{-# DEPRECATED dgrgShowMemberInfo "Use generic-lens or generic-optics with 'showMemberInfo' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgMarker :: Lens.Lens' DescribeGlobalReplicationGroups (Lude.Maybe Lude.Text)
dgrgMarker = Lens.lens (marker :: DescribeGlobalReplicationGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeGlobalReplicationGroups)
{-# DEPRECATED dgrgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgMaxRecords :: Lens.Lens' DescribeGlobalReplicationGroups (Lude.Maybe Lude.Int)
dgrgMaxRecords = Lens.lens (maxRecords :: DescribeGlobalReplicationGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeGlobalReplicationGroups)
{-# DEPRECATED dgrgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgGlobalReplicationGroupId :: Lens.Lens' DescribeGlobalReplicationGroups (Lude.Maybe Lude.Text)
dgrgGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: DescribeGlobalReplicationGroups -> Lude.Maybe Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: DescribeGlobalReplicationGroups)
{-# DEPRECATED dgrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

instance Page.AWSPager DescribeGlobalReplicationGroups where
  page rq rs
    | Page.stop (rs Lens.^. dgrgsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dgrgsrsGlobalReplicationGroups) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dgrgMarker Lens..~ rs Lens.^. dgrgsrsMarker

instance Lude.AWSRequest DescribeGlobalReplicationGroups where
  type
    Rs DescribeGlobalReplicationGroups =
      DescribeGlobalReplicationGroupsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeGlobalReplicationGroupsResult"
      ( \s h x ->
          DescribeGlobalReplicationGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "GlobalReplicationGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "GlobalReplicationGroup")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGlobalReplicationGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeGlobalReplicationGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGlobalReplicationGroups where
  toQuery DescribeGlobalReplicationGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeGlobalReplicationGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "ShowMemberInfo" Lude.=: showMemberInfo,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId
      ]

-- | /See:/ 'mkDescribeGlobalReplicationGroupsResponse' smart constructor.
data DescribeGlobalReplicationGroupsResponse = DescribeGlobalReplicationGroupsResponse'
  { -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
    marker :: Lude.Maybe Lude.Text,
    -- | Indicates the slot configuration and global identifier for each slice group.
    globalReplicationGroups :: Lude.Maybe [GlobalReplicationGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGlobalReplicationGroupsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
-- * 'globalReplicationGroups' - Indicates the slot configuration and global identifier for each slice group.
-- * 'responseStatus' - The response status code.
mkDescribeGlobalReplicationGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGlobalReplicationGroupsResponse
mkDescribeGlobalReplicationGroupsResponse pResponseStatus_ =
  DescribeGlobalReplicationGroupsResponse'
    { marker = Lude.Nothing,
      globalReplicationGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsrsMarker :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Lude.Maybe Lude.Text)
dgrgsrsMarker = Lens.lens (marker :: DescribeGlobalReplicationGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeGlobalReplicationGroupsResponse)
{-# DEPRECATED dgrgsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Indicates the slot configuration and global identifier for each slice group.
--
-- /Note:/ Consider using 'globalReplicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsrsGlobalReplicationGroups :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Lude.Maybe [GlobalReplicationGroup])
dgrgsrsGlobalReplicationGroups = Lens.lens (globalReplicationGroups :: DescribeGlobalReplicationGroupsResponse -> Lude.Maybe [GlobalReplicationGroup]) (\s a -> s {globalReplicationGroups = a} :: DescribeGlobalReplicationGroupsResponse)
{-# DEPRECATED dgrgsrsGlobalReplicationGroups "Use generic-lens or generic-optics with 'globalReplicationGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsrsResponseStatus :: Lens.Lens' DescribeGlobalReplicationGroupsResponse Lude.Int
dgrgsrsResponseStatus = Lens.lens (responseStatus :: DescribeGlobalReplicationGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGlobalReplicationGroupsResponse)
{-# DEPRECATED dgrgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
