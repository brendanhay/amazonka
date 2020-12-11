{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Amazon Redshift parameter groups, including parameter groups you created and the default parameter group. For each parameter group, the response includes the parameter group name, description, and parameter group family name. You can optionally specify a name to retrieve the description of a specific parameter group.
--
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all parameter groups that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all parameter groups that have any combination of those values are returned.
-- If both tag keys and values are omitted from the request, parameter groups are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterParameterGroups
  ( -- * Creating a request
    DescribeClusterParameterGroups (..),
    mkDescribeClusterParameterGroups,

    -- ** Request lenses
    dcpgTagValues,
    dcpgTagKeys,
    dcpgMarker,
    dcpgMaxRecords,
    dcpgParameterGroupName,

    -- * Destructuring the response
    DescribeClusterParameterGroupsResponse (..),
    mkDescribeClusterParameterGroupsResponse,

    -- ** Response lenses
    dcpgrsMarker,
    dcpgrsParameterGroups,
    dcpgrsResponseStatus,
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
-- /See:/ 'mkDescribeClusterParameterGroups' smart constructor.
data DescribeClusterParameterGroups = DescribeClusterParameterGroups'
  { tagValues ::
      Lude.Maybe [Lude.Text],
    tagKeys ::
      Lude.Maybe [Lude.Text],
    marker ::
      Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    parameterGroupName ::
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

-- | Creates a value of 'DescribeClusterParameterGroups' with the minimum fields required to make a request.
--
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameterGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'parameterGroupName' - The name of a specific parameter group for which to return details. By default, details about all parameter groups and the default parameter group are returned.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching cluster parameter groups that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the parameter groups that have either or both of these tag keys associated with them.
-- * 'tagValues' - A tag value or values for which you want to return all matching cluster parameter groups that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the parameter groups that have either or both of these tag values associated with them.
mkDescribeClusterParameterGroups ::
  DescribeClusterParameterGroups
mkDescribeClusterParameterGroups =
  DescribeClusterParameterGroups'
    { tagValues = Lude.Nothing,
      tagKeys = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      parameterGroupName = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching cluster parameter groups that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the parameter groups that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgTagValues :: Lens.Lens' DescribeClusterParameterGroups (Lude.Maybe [Lude.Text])
dcpgTagValues = Lens.lens (tagValues :: DescribeClusterParameterGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeClusterParameterGroups)
{-# DEPRECATED dcpgTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | A tag key or keys for which you want to return all matching cluster parameter groups that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the parameter groups that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgTagKeys :: Lens.Lens' DescribeClusterParameterGroups (Lude.Maybe [Lude.Text])
dcpgTagKeys = Lens.lens (tagKeys :: DescribeClusterParameterGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeClusterParameterGroups)
{-# DEPRECATED dcpgTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameterGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMarker :: Lens.Lens' DescribeClusterParameterGroups (Lude.Maybe Lude.Text)
dcpgMarker = Lens.lens (marker :: DescribeClusterParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterParameterGroups)
{-# DEPRECATED dcpgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMaxRecords :: Lens.Lens' DescribeClusterParameterGroups (Lude.Maybe Lude.Int)
dcpgMaxRecords = Lens.lens (maxRecords :: DescribeClusterParameterGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusterParameterGroups)
{-# DEPRECATED dcpgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of a specific parameter group for which to return details. By default, details about all parameter groups and the default parameter group are returned.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgParameterGroupName :: Lens.Lens' DescribeClusterParameterGroups (Lude.Maybe Lude.Text)
dcpgParameterGroupName = Lens.lens (parameterGroupName :: DescribeClusterParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupName = a} :: DescribeClusterParameterGroups)
{-# DEPRECATED dcpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Page.AWSPager DescribeClusterParameterGroups where
  page rq rs
    | Page.stop (rs Lens.^. dcpgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcpgrsParameterGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcpgMarker Lens..~ rs Lens.^. dcpgrsMarker

instance Lude.AWSRequest DescribeClusterParameterGroups where
  type
    Rs DescribeClusterParameterGroups =
      DescribeClusterParameterGroupsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClusterParameterGroupsResult"
      ( \s h x ->
          DescribeClusterParameterGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "ParameterGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ClusterParameterGroup")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusterParameterGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusterParameterGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusterParameterGroups where
  toQuery DescribeClusterParameterGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClusterParameterGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ParameterGroupName" Lude.=: parameterGroupName
      ]

-- | Contains the output from the 'DescribeClusterParameterGroups' action.
--
-- /See:/ 'mkDescribeClusterParameterGroupsResponse' smart constructor.
data DescribeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse'
  { marker ::
      Lude.Maybe
        Lude.Text,
    parameterGroups ::
      Lude.Maybe
        [ClusterParameterGroup],
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

-- | Creates a value of 'DescribeClusterParameterGroupsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'parameterGroups' - A list of 'ClusterParameterGroup' instances. Each instance describes one cluster parameter group.
-- * 'responseStatus' - The response status code.
mkDescribeClusterParameterGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterParameterGroupsResponse
mkDescribeClusterParameterGroupsResponse pResponseStatus_ =
  DescribeClusterParameterGroupsResponse'
    { marker = Lude.Nothing,
      parameterGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrsMarker :: Lens.Lens' DescribeClusterParameterGroupsResponse (Lude.Maybe Lude.Text)
dcpgrsMarker = Lens.lens (marker :: DescribeClusterParameterGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterParameterGroupsResponse)
{-# DEPRECATED dcpgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of 'ClusterParameterGroup' instances. Each instance describes one cluster parameter group.
--
-- /Note:/ Consider using 'parameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrsParameterGroups :: Lens.Lens' DescribeClusterParameterGroupsResponse (Lude.Maybe [ClusterParameterGroup])
dcpgrsParameterGroups = Lens.lens (parameterGroups :: DescribeClusterParameterGroupsResponse -> Lude.Maybe [ClusterParameterGroup]) (\s a -> s {parameterGroups = a} :: DescribeClusterParameterGroupsResponse)
{-# DEPRECATED dcpgrsParameterGroups "Use generic-lens or generic-optics with 'parameterGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrsResponseStatus :: Lens.Lens' DescribeClusterParameterGroupsResponse Lude.Int
dcpgrsResponseStatus = Lens.lens (responseStatus :: DescribeClusterParameterGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterParameterGroupsResponse)
{-# DEPRECATED dcpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
