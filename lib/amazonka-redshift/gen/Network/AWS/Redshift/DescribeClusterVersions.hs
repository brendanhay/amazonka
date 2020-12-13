{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of the available Amazon Redshift cluster versions. You can call this operation even before creating any clusters to learn more about the Amazon Redshift versions. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterVersions
  ( -- * Creating a request
    DescribeClusterVersions (..),
    mkDescribeClusterVersions,

    -- ** Request lenses
    dcvClusterParameterGroupFamily,
    dcvMarker,
    dcvMaxRecords,
    dcvClusterVersion,

    -- * Destructuring the response
    DescribeClusterVersionsResponse (..),
    mkDescribeClusterVersionsResponse,

    -- ** Response lenses
    dcvrsClusterVersions,
    dcvrsMarker,
    dcvrsResponseStatus,
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
-- /See:/ 'mkDescribeClusterVersions' smart constructor.
data DescribeClusterVersions = DescribeClusterVersions'
  { -- | The name of a specific cluster parameter group family to return details for.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 alphanumeric characters
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens
    clusterParameterGroupFamily :: Lude.Maybe Lude.Text,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The specific cluster version to return.
    --
    -- Example: @1.0@
    clusterVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterVersions' with the minimum fields required to make a request.
--
-- * 'clusterParameterGroupFamily' - The name of a specific cluster parameter group family to return details for.
--
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'clusterVersion' - The specific cluster version to return.
--
-- Example: @1.0@
mkDescribeClusterVersions ::
  DescribeClusterVersions
mkDescribeClusterVersions =
  DescribeClusterVersions'
    { clusterParameterGroupFamily =
        Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      clusterVersion = Lude.Nothing
    }

-- | The name of a specific cluster parameter group family to return details for.
--
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'clusterParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvClusterParameterGroupFamily :: Lens.Lens' DescribeClusterVersions (Lude.Maybe Lude.Text)
dcvClusterParameterGroupFamily = Lens.lens (clusterParameterGroupFamily :: DescribeClusterVersions -> Lude.Maybe Lude.Text) (\s a -> s {clusterParameterGroupFamily = a} :: DescribeClusterVersions)
{-# DEPRECATED dcvClusterParameterGroupFamily "Use generic-lens or generic-optics with 'clusterParameterGroupFamily' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvMarker :: Lens.Lens' DescribeClusterVersions (Lude.Maybe Lude.Text)
dcvMarker = Lens.lens (marker :: DescribeClusterVersions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterVersions)
{-# DEPRECATED dcvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvMaxRecords :: Lens.Lens' DescribeClusterVersions (Lude.Maybe Lude.Int)
dcvMaxRecords = Lens.lens (maxRecords :: DescribeClusterVersions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusterVersions)
{-# DEPRECATED dcvMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The specific cluster version to return.
--
-- Example: @1.0@
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvClusterVersion :: Lens.Lens' DescribeClusterVersions (Lude.Maybe Lude.Text)
dcvClusterVersion = Lens.lens (clusterVersion :: DescribeClusterVersions -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: DescribeClusterVersions)
{-# DEPRECATED dcvClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

instance Page.AWSPager DescribeClusterVersions where
  page rq rs
    | Page.stop (rs Lens.^. dcvrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcvrsClusterVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dcvMarker Lens..~ rs Lens.^. dcvrsMarker

instance Lude.AWSRequest DescribeClusterVersions where
  type Rs DescribeClusterVersions = DescribeClusterVersionsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClusterVersionsResult"
      ( \s h x ->
          DescribeClusterVersionsResponse'
            Lude.<$> ( x Lude..@? "ClusterVersions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ClusterVersion")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusterVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusterVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusterVersions where
  toQuery DescribeClusterVersions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeClusterVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterParameterGroupFamily" Lude.=: clusterParameterGroupFamily,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ClusterVersion" Lude.=: clusterVersion
      ]

-- | Contains the output from the 'DescribeClusterVersions' action.
--
-- /See:/ 'mkDescribeClusterVersionsResponse' smart constructor.
data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse'
  { -- | A list of @Version@ elements.
    clusterVersions :: Lude.Maybe [ClusterVersion],
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterVersionsResponse' with the minimum fields required to make a request.
--
-- * 'clusterVersions' - A list of @Version@ elements.
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'responseStatus' - The response status code.
mkDescribeClusterVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterVersionsResponse
mkDescribeClusterVersionsResponse pResponseStatus_ =
  DescribeClusterVersionsResponse'
    { clusterVersions = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @Version@ elements.
--
-- /Note:/ Consider using 'clusterVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrsClusterVersions :: Lens.Lens' DescribeClusterVersionsResponse (Lude.Maybe [ClusterVersion])
dcvrsClusterVersions = Lens.lens (clusterVersions :: DescribeClusterVersionsResponse -> Lude.Maybe [ClusterVersion]) (\s a -> s {clusterVersions = a} :: DescribeClusterVersionsResponse)
{-# DEPRECATED dcvrsClusterVersions "Use generic-lens or generic-optics with 'clusterVersions' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrsMarker :: Lens.Lens' DescribeClusterVersionsResponse (Lude.Maybe Lude.Text)
dcvrsMarker = Lens.lens (marker :: DescribeClusterVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterVersionsResponse)
{-# DEPRECATED dcvrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrsResponseStatus :: Lens.Lens' DescribeClusterVersionsResponse Lude.Int
dcvrsResponseStatus = Lens.lens (responseStatus :: DescribeClusterVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterVersionsResponse)
{-# DEPRECATED dcvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
