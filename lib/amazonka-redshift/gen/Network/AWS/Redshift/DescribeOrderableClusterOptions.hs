{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeOrderableClusterOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable cluster options. Before you create a new cluster you can use this operation to find what options are available, such as the EC2 Availability Zones (AZ) in the specific AWS Region that you can specify, and the node types you can request. The node types differ by available storage, memory, CPU and price. With the cost involved you might want to obtain a list of cluster options in the specific region and specify values when creating a cluster. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeOrderableClusterOptions
  ( -- * Creating a request
    DescribeOrderableClusterOptions (..),
    mkDescribeOrderableClusterOptions,

    -- ** Request lenses
    docoMarker,
    docoMaxRecords,
    docoClusterVersion,
    docoNodeType,

    -- * Destructuring the response
    DescribeOrderableClusterOptionsResponse (..),
    mkDescribeOrderableClusterOptionsResponse,

    -- ** Response lenses
    docorsMarker,
    docorsOrderableClusterOptions,
    docorsResponseStatus,
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
-- /See:/ 'mkDescribeOrderableClusterOptions' smart constructor.
data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions'
  { -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeOrderableClusterOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The version filter value. Specify this parameter to show only the available offerings matching the specified version.
    --
    -- Default: All versions.
    -- Constraints: Must be one of the version returned from 'DescribeClusterVersions' .
    clusterVersion :: Lude.Maybe Lude.Text,
    -- | The node type filter value. Specify this parameter to show only the available offerings matching the specified node type.
    nodeType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrderableClusterOptions' with the minimum fields required to make a request.
--
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeOrderableClusterOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'clusterVersion' - The version filter value. Specify this parameter to show only the available offerings matching the specified version.
--
-- Default: All versions.
-- Constraints: Must be one of the version returned from 'DescribeClusterVersions' .
-- * 'nodeType' - The node type filter value. Specify this parameter to show only the available offerings matching the specified node type.
mkDescribeOrderableClusterOptions ::
  DescribeOrderableClusterOptions
mkDescribeOrderableClusterOptions =
  DescribeOrderableClusterOptions'
    { marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      clusterVersion = Lude.Nothing,
      nodeType = Lude.Nothing
    }

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeOrderableClusterOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docoMarker :: Lens.Lens' DescribeOrderableClusterOptions (Lude.Maybe Lude.Text)
docoMarker = Lens.lens (marker :: DescribeOrderableClusterOptions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOrderableClusterOptions)
{-# DEPRECATED docoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docoMaxRecords :: Lens.Lens' DescribeOrderableClusterOptions (Lude.Maybe Lude.Int)
docoMaxRecords = Lens.lens (maxRecords :: DescribeOrderableClusterOptions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeOrderableClusterOptions)
{-# DEPRECATED docoMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The version filter value. Specify this parameter to show only the available offerings matching the specified version.
--
-- Default: All versions.
-- Constraints: Must be one of the version returned from 'DescribeClusterVersions' .
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docoClusterVersion :: Lens.Lens' DescribeOrderableClusterOptions (Lude.Maybe Lude.Text)
docoClusterVersion = Lens.lens (clusterVersion :: DescribeOrderableClusterOptions -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: DescribeOrderableClusterOptions)
{-# DEPRECATED docoClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The node type filter value. Specify this parameter to show only the available offerings matching the specified node type.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docoNodeType :: Lens.Lens' DescribeOrderableClusterOptions (Lude.Maybe Lude.Text)
docoNodeType = Lens.lens (nodeType :: DescribeOrderableClusterOptions -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: DescribeOrderableClusterOptions)
{-# DEPRECATED docoNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

instance Page.AWSPager DescribeOrderableClusterOptions where
  page rq rs
    | Page.stop (rs Lens.^. docorsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. docorsOrderableClusterOptions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& docoMarker Lens..~ rs Lens.^. docorsMarker

instance Lude.AWSRequest DescribeOrderableClusterOptions where
  type
    Rs DescribeOrderableClusterOptions =
      DescribeOrderableClusterOptionsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeOrderableClusterOptionsResult"
      ( \s h x ->
          DescribeOrderableClusterOptionsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "OrderableClusterOptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "OrderableClusterOption")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrderableClusterOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeOrderableClusterOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrderableClusterOptions where
  toQuery DescribeOrderableClusterOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeOrderableClusterOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ClusterVersion" Lude.=: clusterVersion,
        "NodeType" Lude.=: nodeType
      ]

-- | Contains the output from the 'DescribeOrderableClusterOptions' action.
--
-- /See:/ 'mkDescribeOrderableClusterOptionsResponse' smart constructor.
data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | An @OrderableClusterOption@ structure containing information about orderable options for the cluster.
    orderableClusterOptions :: Lude.Maybe [OrderableClusterOption],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrderableClusterOptionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'orderableClusterOptions' - An @OrderableClusterOption@ structure containing information about orderable options for the cluster.
-- * 'responseStatus' - The response status code.
mkDescribeOrderableClusterOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrderableClusterOptionsResponse
mkDescribeOrderableClusterOptionsResponse pResponseStatus_ =
  DescribeOrderableClusterOptionsResponse'
    { marker = Lude.Nothing,
      orderableClusterOptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docorsMarker :: Lens.Lens' DescribeOrderableClusterOptionsResponse (Lude.Maybe Lude.Text)
docorsMarker = Lens.lens (marker :: DescribeOrderableClusterOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOrderableClusterOptionsResponse)
{-# DEPRECATED docorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An @OrderableClusterOption@ structure containing information about orderable options for the cluster.
--
-- /Note:/ Consider using 'orderableClusterOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docorsOrderableClusterOptions :: Lens.Lens' DescribeOrderableClusterOptionsResponse (Lude.Maybe [OrderableClusterOption])
docorsOrderableClusterOptions = Lens.lens (orderableClusterOptions :: DescribeOrderableClusterOptionsResponse -> Lude.Maybe [OrderableClusterOption]) (\s a -> s {orderableClusterOptions = a} :: DescribeOrderableClusterOptionsResponse)
{-# DEPRECATED docorsOrderableClusterOptions "Use generic-lens or generic-optics with 'orderableClusterOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docorsResponseStatus :: Lens.Lens' DescribeOrderableClusterOptionsResponse Lude.Int
docorsResponseStatus = Lens.lens (responseStatus :: DescribeOrderableClusterOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrderableClusterOptionsResponse)
{-# DEPRECATED docorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
