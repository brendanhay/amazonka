{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeUsageLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shows usage limits on a cluster. Results are filtered based on the combination of input usage limit identifier, cluster identifier, and feature type parameters:
--
--
--     * If usage limit identifier, cluster identifier, and feature type are not provided, then all usage limit objects for the current account in the current region are returned.
--
--
--     * If usage limit identifier is provided, then the corresponding usage limit object is returned.
--
--
--     * If cluster identifier is provided, then all usage limit objects for the specified cluster are returned.
--
--
--     * If cluster identifier and feature type are provided, then all usage limit objects for the combination of cluster and feature are returned.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeUsageLimits
  ( -- * Creating a request
    DescribeUsageLimits (..),
    mkDescribeUsageLimits,

    -- ** Request lenses
    dulsTagValues,
    dulsUsageLimitId,
    dulsTagKeys,
    dulsClusterIdentifier,
    dulsFeatureType,
    dulsMarker,
    dulsMaxRecords,

    -- * Destructuring the response
    DescribeUsageLimitsResponse (..),
    mkDescribeUsageLimitsResponse,

    -- ** Response lenses
    dulrsUsageLimits,
    dulrsMarker,
    dulrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUsageLimits' smart constructor.
data DescribeUsageLimits = DescribeUsageLimits'
  { tagValues ::
      Lude.Maybe [Lude.Text],
    usageLimitId :: Lude.Maybe Lude.Text,
    tagKeys :: Lude.Maybe [Lude.Text],
    clusterIdentifier :: Lude.Maybe Lude.Text,
    featureType :: Lude.Maybe UsageLimitFeatureType,
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

-- | Creates a value of 'DescribeUsageLimits' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The identifier of the cluster for which you want to describe usage limits.
-- * 'featureType' - The feature type for which you want to describe usage limits.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeUsageLimits' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching usage limit objects that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the usage limit objects have either or both of these tag keys associated with them.
-- * 'tagValues' - A tag value or values for which you want to return all matching usage limit objects that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the usage limit objects that have either or both of these tag values associated with them.
-- * 'usageLimitId' - The identifier of the usage limit to describe.
mkDescribeUsageLimits ::
  DescribeUsageLimits
mkDescribeUsageLimits =
  DescribeUsageLimits'
    { tagValues = Lude.Nothing,
      usageLimitId = Lude.Nothing,
      tagKeys = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      featureType = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching usage limit objects that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the usage limit objects that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsTagValues :: Lens.Lens' DescribeUsageLimits (Lude.Maybe [Lude.Text])
dulsTagValues = Lens.lens (tagValues :: DescribeUsageLimits -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeUsageLimits)
{-# DEPRECATED dulsTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | The identifier of the usage limit to describe.
--
-- /Note:/ Consider using 'usageLimitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsUsageLimitId :: Lens.Lens' DescribeUsageLimits (Lude.Maybe Lude.Text)
dulsUsageLimitId = Lens.lens (usageLimitId :: DescribeUsageLimits -> Lude.Maybe Lude.Text) (\s a -> s {usageLimitId = a} :: DescribeUsageLimits)
{-# DEPRECATED dulsUsageLimitId "Use generic-lens or generic-optics with 'usageLimitId' instead." #-}

-- | A tag key or keys for which you want to return all matching usage limit objects that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the usage limit objects have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsTagKeys :: Lens.Lens' DescribeUsageLimits (Lude.Maybe [Lude.Text])
dulsTagKeys = Lens.lens (tagKeys :: DescribeUsageLimits -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeUsageLimits)
{-# DEPRECATED dulsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The identifier of the cluster for which you want to describe usage limits.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsClusterIdentifier :: Lens.Lens' DescribeUsageLimits (Lude.Maybe Lude.Text)
dulsClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeUsageLimits -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeUsageLimits)
{-# DEPRECATED dulsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The feature type for which you want to describe usage limits.
--
-- /Note:/ Consider using 'featureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsFeatureType :: Lens.Lens' DescribeUsageLimits (Lude.Maybe UsageLimitFeatureType)
dulsFeatureType = Lens.lens (featureType :: DescribeUsageLimits -> Lude.Maybe UsageLimitFeatureType) (\s a -> s {featureType = a} :: DescribeUsageLimits)
{-# DEPRECATED dulsFeatureType "Use generic-lens or generic-optics with 'featureType' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeUsageLimits' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsMarker :: Lens.Lens' DescribeUsageLimits (Lude.Maybe Lude.Text)
dulsMarker = Lens.lens (marker :: DescribeUsageLimits -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUsageLimits)
{-# DEPRECATED dulsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsMaxRecords :: Lens.Lens' DescribeUsageLimits (Lude.Maybe Lude.Int)
dulsMaxRecords = Lens.lens (maxRecords :: DescribeUsageLimits -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeUsageLimits)
{-# DEPRECATED dulsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeUsageLimits where
  page rq rs
    | Page.stop (rs Lens.^. dulrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dulrsUsageLimits) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dulsMarker Lens..~ rs Lens.^. dulrsMarker

instance Lude.AWSRequest DescribeUsageLimits where
  type Rs DescribeUsageLimits = DescribeUsageLimitsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeUsageLimitsResult"
      ( \s h x ->
          DescribeUsageLimitsResponse'
            Lude.<$> ( x Lude..@? "UsageLimits" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUsageLimits where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeUsageLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUsageLimits where
  toQuery DescribeUsageLimits' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeUsageLimits" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "UsageLimitId" Lude.=: usageLimitId,
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "FeatureType" Lude.=: featureType,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeUsageLimitsResponse' smart constructor.
data DescribeUsageLimitsResponse = DescribeUsageLimitsResponse'
  { usageLimits ::
      Lude.Maybe [UsageLimit],
    marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeUsageLimitsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'responseStatus' - The response status code.
-- * 'usageLimits' - Contains the output from the 'DescribeUsageLimits' action.
mkDescribeUsageLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUsageLimitsResponse
mkDescribeUsageLimitsResponse pResponseStatus_ =
  DescribeUsageLimitsResponse'
    { usageLimits = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the output from the 'DescribeUsageLimits' action.
--
-- /Note:/ Consider using 'usageLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulrsUsageLimits :: Lens.Lens' DescribeUsageLimitsResponse (Lude.Maybe [UsageLimit])
dulrsUsageLimits = Lens.lens (usageLimits :: DescribeUsageLimitsResponse -> Lude.Maybe [UsageLimit]) (\s a -> s {usageLimits = a} :: DescribeUsageLimitsResponse)
{-# DEPRECATED dulrsUsageLimits "Use generic-lens or generic-optics with 'usageLimits' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulrsMarker :: Lens.Lens' DescribeUsageLimitsResponse (Lude.Maybe Lude.Text)
dulrsMarker = Lens.lens (marker :: DescribeUsageLimitsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUsageLimitsResponse)
{-# DEPRECATED dulrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulrsResponseStatus :: Lens.Lens' DescribeUsageLimitsResponse Lude.Int
dulrsResponseStatus = Lens.lens (responseStatus :: DescribeUsageLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUsageLimitsResponse)
{-# DEPRECATED dulrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
