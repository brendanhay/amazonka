{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Redshift security groups. If the name of a security group is specified, the response will contain only information about only that security group.
--
-- For information about managing security groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all security groups that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all security groups that have any combination of those values are returned.
-- If both tag keys and values are omitted from the request, security groups are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterSecurityGroups
  ( -- * Creating a request
    DescribeClusterSecurityGroups (..),
    mkDescribeClusterSecurityGroups,

    -- ** Request lenses
    dcsgfTagValues,
    dcsgfTagKeys,
    dcsgfClusterSecurityGroupName,
    dcsgfMarker,
    dcsgfMaxRecords,

    -- * Destructuring the response
    DescribeClusterSecurityGroupsResponse (..),
    mkDescribeClusterSecurityGroupsResponse,

    -- ** Response lenses
    drsClusterSecurityGroups,
    drsMarker,
    drsResponseStatus,
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
-- /See:/ 'mkDescribeClusterSecurityGroups' smart constructor.
data DescribeClusterSecurityGroups = DescribeClusterSecurityGroups'
  { -- | A tag value or values for which you want to return all matching cluster security groups that are associated with the specified tag value or values. For example, suppose that you have security groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag values associated with them.
    tagValues :: Lude.Maybe [Lude.Text],
    -- | A tag key or keys for which you want to return all matching cluster security groups that are associated with the specified key or keys. For example, suppose that you have security groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag keys associated with them.
    tagKeys :: Lude.Maybe [Lude.Text],
    -- | The name of a cluster security group for which you are requesting details. You can specify either the __Marker__ parameter or a __ClusterSecurityGroupName__ parameter, but not both.
    --
    -- Example: @securitygroup1@
    clusterSecurityGroupName :: Lude.Maybe Lude.Text,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSecurityGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __ClusterSecurityGroupName__ parameter or the __Marker__ parameter, but not both.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterSecurityGroups' with the minimum fields required to make a request.
--
-- * 'tagValues' - A tag value or values for which you want to return all matching cluster security groups that are associated with the specified tag value or values. For example, suppose that you have security groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag values associated with them.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching cluster security groups that are associated with the specified key or keys. For example, suppose that you have security groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag keys associated with them.
-- * 'clusterSecurityGroupName' - The name of a cluster security group for which you are requesting details. You can specify either the __Marker__ parameter or a __ClusterSecurityGroupName__ parameter, but not both.
--
-- Example: @securitygroup1@
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSecurityGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterSecurityGroupName__ parameter or the __Marker__ parameter, but not both.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
mkDescribeClusterSecurityGroups ::
  DescribeClusterSecurityGroups
mkDescribeClusterSecurityGroups =
  DescribeClusterSecurityGroups'
    { tagValues = Lude.Nothing,
      tagKeys = Lude.Nothing,
      clusterSecurityGroupName = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching cluster security groups that are associated with the specified tag value or values. For example, suppose that you have security groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgfTagValues :: Lens.Lens' DescribeClusterSecurityGroups (Lude.Maybe [Lude.Text])
dcsgfTagValues = Lens.lens (tagValues :: DescribeClusterSecurityGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeClusterSecurityGroups)
{-# DEPRECATED dcsgfTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | A tag key or keys for which you want to return all matching cluster security groups that are associated with the specified key or keys. For example, suppose that you have security groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgfTagKeys :: Lens.Lens' DescribeClusterSecurityGroups (Lude.Maybe [Lude.Text])
dcsgfTagKeys = Lens.lens (tagKeys :: DescribeClusterSecurityGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeClusterSecurityGroups)
{-# DEPRECATED dcsgfTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The name of a cluster security group for which you are requesting details. You can specify either the __Marker__ parameter or a __ClusterSecurityGroupName__ parameter, but not both.
--
-- Example: @securitygroup1@
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgfClusterSecurityGroupName :: Lens.Lens' DescribeClusterSecurityGroups (Lude.Maybe Lude.Text)
dcsgfClusterSecurityGroupName = Lens.lens (clusterSecurityGroupName :: DescribeClusterSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {clusterSecurityGroupName = a} :: DescribeClusterSecurityGroups)
{-# DEPRECATED dcsgfClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSecurityGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterSecurityGroupName__ parameter or the __Marker__ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgfMarker :: Lens.Lens' DescribeClusterSecurityGroups (Lude.Maybe Lude.Text)
dcsgfMarker = Lens.lens (marker :: DescribeClusterSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterSecurityGroups)
{-# DEPRECATED dcsgfMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgfMaxRecords :: Lens.Lens' DescribeClusterSecurityGroups (Lude.Maybe Lude.Int)
dcsgfMaxRecords = Lens.lens (maxRecords :: DescribeClusterSecurityGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusterSecurityGroups)
{-# DEPRECATED dcsgfMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeClusterSecurityGroups where
  page rq rs
    | Page.stop (rs Lens.^. drsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drsClusterSecurityGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dcsgfMarker Lens..~ rs Lens.^. drsMarker

instance Lude.AWSRequest DescribeClusterSecurityGroups where
  type
    Rs DescribeClusterSecurityGroups =
      DescribeClusterSecurityGroupsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClusterSecurityGroupsResult"
      ( \s h x ->
          DescribeClusterSecurityGroupsResponse'
            Lude.<$> ( x Lude..@? "ClusterSecurityGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ClusterSecurityGroup")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusterSecurityGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusterSecurityGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusterSecurityGroups where
  toQuery DescribeClusterSecurityGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClusterSecurityGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "ClusterSecurityGroupName" Lude.=: clusterSecurityGroupName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- |
--
-- /See:/ 'mkDescribeClusterSecurityGroupsResponse' smart constructor.
data DescribeClusterSecurityGroupsResponse = DescribeClusterSecurityGroupsResponse'
  { -- | A list of 'ClusterSecurityGroup' instances.
    clusterSecurityGroups :: Lude.Maybe [ClusterSecurityGroup],
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- * 'clusterSecurityGroups' - A list of 'ClusterSecurityGroup' instances.
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'responseStatus' - The response status code.
mkDescribeClusterSecurityGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterSecurityGroupsResponse
mkDescribeClusterSecurityGroupsResponse pResponseStatus_ =
  DescribeClusterSecurityGroupsResponse'
    { clusterSecurityGroups =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'ClusterSecurityGroup' instances.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsClusterSecurityGroups :: Lens.Lens' DescribeClusterSecurityGroupsResponse (Lude.Maybe [ClusterSecurityGroup])
drsClusterSecurityGroups = Lens.lens (clusterSecurityGroups :: DescribeClusterSecurityGroupsResponse -> Lude.Maybe [ClusterSecurityGroup]) (\s a -> s {clusterSecurityGroups = a} :: DescribeClusterSecurityGroupsResponse)
{-# DEPRECATED drsClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsMarker :: Lens.Lens' DescribeClusterSecurityGroupsResponse (Lude.Maybe Lude.Text)
drsMarker = Lens.lens (marker :: DescribeClusterSecurityGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterSecurityGroupsResponse)
{-# DEPRECATED drsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeClusterSecurityGroupsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeClusterSecurityGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterSecurityGroupsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
