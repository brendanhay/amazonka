{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterSubnetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more cluster subnet group objects, which contain metadata about your cluster subnet groups. By default, this operation returns information about all cluster subnet groups that are defined in you AWS account.
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all subnet groups that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all subnet groups that have any combination of those values are returned.
-- If both tag keys and values are omitted from the request, subnet groups are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterSubnetGroups
  ( -- * Creating a request
    DescribeClusterSubnetGroups (..),
    mkDescribeClusterSubnetGroups,

    -- ** Request lenses
    dcsgsTagValues,
    dcsgsTagKeys,
    dcsgsClusterSubnetGroupName,
    dcsgsMarker,
    dcsgsMaxRecords,

    -- * Destructuring the response
    DescribeClusterSubnetGroupsResponse (..),
    mkDescribeClusterSubnetGroupsResponse,

    -- ** Response lenses
    dcsgrsClusterSubnetGroups,
    dcsgrsMarker,
    dcsgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeClusterSubnetGroups' smart constructor.
data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups'
  { tagValues ::
      Lude.Maybe [Lude.Text],
    tagKeys :: Lude.Maybe [Lude.Text],
    clusterSubnetGroupName ::
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

-- | Creates a value of 'DescribeClusterSubnetGroups' with the minimum fields required to make a request.
--
-- * 'clusterSubnetGroupName' - The name of the cluster subnet group for which information is requested.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSubnetGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching cluster subnet groups that are associated with the specified key or keys. For example, suppose that you have subnet groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the subnet groups that have either or both of these tag keys associated with them.
-- * 'tagValues' - A tag value or values for which you want to return all matching cluster subnet groups that are associated with the specified tag value or values. For example, suppose that you have subnet groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the subnet groups that have either or both of these tag values associated with them.
mkDescribeClusterSubnetGroups ::
  DescribeClusterSubnetGroups
mkDescribeClusterSubnetGroups =
  DescribeClusterSubnetGroups'
    { tagValues = Lude.Nothing,
      tagKeys = Lude.Nothing,
      clusterSubnetGroupName = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching cluster subnet groups that are associated with the specified tag value or values. For example, suppose that you have subnet groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the subnet groups that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsTagValues :: Lens.Lens' DescribeClusterSubnetGroups (Lude.Maybe [Lude.Text])
dcsgsTagValues = Lens.lens (tagValues :: DescribeClusterSubnetGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeClusterSubnetGroups)
{-# DEPRECATED dcsgsTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | A tag key or keys for which you want to return all matching cluster subnet groups that are associated with the specified key or keys. For example, suppose that you have subnet groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the subnet groups that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsTagKeys :: Lens.Lens' DescribeClusterSubnetGroups (Lude.Maybe [Lude.Text])
dcsgsTagKeys = Lens.lens (tagKeys :: DescribeClusterSubnetGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeClusterSubnetGroups)
{-# DEPRECATED dcsgsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The name of the cluster subnet group for which information is requested.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsClusterSubnetGroupName :: Lens.Lens' DescribeClusterSubnetGroups (Lude.Maybe Lude.Text)
dcsgsClusterSubnetGroupName = Lens.lens (clusterSubnetGroupName :: DescribeClusterSubnetGroups -> Lude.Maybe Lude.Text) (\s a -> s {clusterSubnetGroupName = a} :: DescribeClusterSubnetGroups)
{-# DEPRECATED dcsgsClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSubnetGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsMarker :: Lens.Lens' DescribeClusterSubnetGroups (Lude.Maybe Lude.Text)
dcsgsMarker = Lens.lens (marker :: DescribeClusterSubnetGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterSubnetGroups)
{-# DEPRECATED dcsgsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsMaxRecords :: Lens.Lens' DescribeClusterSubnetGroups (Lude.Maybe Lude.Int)
dcsgsMaxRecords = Lens.lens (maxRecords :: DescribeClusterSubnetGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusterSubnetGroups)
{-# DEPRECATED dcsgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeClusterSubnetGroups where
  page rq rs
    | Page.stop (rs Lens.^. dcsgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcsgrsClusterSubnetGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcsgsMarker Lens..~ rs Lens.^. dcsgrsMarker

instance Lude.AWSRequest DescribeClusterSubnetGroups where
  type
    Rs DescribeClusterSubnetGroups =
      DescribeClusterSubnetGroupsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClusterSubnetGroupsResult"
      ( \s h x ->
          DescribeClusterSubnetGroupsResponse'
            Lude.<$> ( x Lude..@? "ClusterSubnetGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ClusterSubnetGroup")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusterSubnetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusterSubnetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusterSubnetGroups where
  toQuery DescribeClusterSubnetGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClusterSubnetGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "ClusterSubnetGroupName" Lude.=: clusterSubnetGroupName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Contains the output from the 'DescribeClusterSubnetGroups' action.
--
-- /See:/ 'mkDescribeClusterSubnetGroupsResponse' smart constructor.
data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse'
  { clusterSubnetGroups ::
      Lude.Maybe
        [ClusterSubnetGroup],
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

-- | Creates a value of 'DescribeClusterSubnetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'clusterSubnetGroups' - A list of 'ClusterSubnetGroup' instances.
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'responseStatus' - The response status code.
mkDescribeClusterSubnetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterSubnetGroupsResponse
mkDescribeClusterSubnetGroupsResponse pResponseStatus_ =
  DescribeClusterSubnetGroupsResponse'
    { clusterSubnetGroups =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'ClusterSubnetGroup' instances.
--
-- /Note:/ Consider using 'clusterSubnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrsClusterSubnetGroups :: Lens.Lens' DescribeClusterSubnetGroupsResponse (Lude.Maybe [ClusterSubnetGroup])
dcsgrsClusterSubnetGroups = Lens.lens (clusterSubnetGroups :: DescribeClusterSubnetGroupsResponse -> Lude.Maybe [ClusterSubnetGroup]) (\s a -> s {clusterSubnetGroups = a} :: DescribeClusterSubnetGroupsResponse)
{-# DEPRECATED dcsgrsClusterSubnetGroups "Use generic-lens or generic-optics with 'clusterSubnetGroups' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrsMarker :: Lens.Lens' DescribeClusterSubnetGroupsResponse (Lude.Maybe Lude.Text)
dcsgrsMarker = Lens.lens (marker :: DescribeClusterSubnetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterSubnetGroupsResponse)
{-# DEPRECATED dcsgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrsResponseStatus :: Lens.Lens' DescribeClusterSubnetGroupsResponse Lude.Int
dcsgrsResponseStatus = Lens.lens (responseStatus :: DescribeClusterSubnetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterSubnetGroupsResponse)
{-# DEPRECATED dcsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
