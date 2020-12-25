{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dulsClusterIdentifier,
    dulsFeatureType,
    dulsMarker,
    dulsMaxRecords,
    dulsTagKeys,
    dulsTagValues,
    dulsUsageLimitId,

    -- * Destructuring the response
    DescribeUsageLimitsResponse (..),
    mkDescribeUsageLimitsResponse,

    -- ** Response lenses
    dulrrsMarker,
    dulrrsUsageLimits,
    dulrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUsageLimits' smart constructor.
data DescribeUsageLimits = DescribeUsageLimits'
  { -- | The identifier of the cluster for which you want to describe usage limits.
    clusterIdentifier :: Core.Maybe Types.String,
    -- | The feature type for which you want to describe usage limits.
    featureType :: Core.Maybe Types.UsageLimitFeatureType,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeUsageLimits' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | A tag key or keys for which you want to return all matching usage limit objects that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the usage limit objects have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Types.String],
    -- | A tag value or values for which you want to return all matching usage limit objects that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the usage limit objects that have either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Types.String],
    -- | The identifier of the usage limit to describe.
    usageLimitId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsageLimits' value with any optional fields omitted.
mkDescribeUsageLimits ::
  DescribeUsageLimits
mkDescribeUsageLimits =
  DescribeUsageLimits'
    { clusterIdentifier = Core.Nothing,
      featureType = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing,
      usageLimitId = Core.Nothing
    }

-- | The identifier of the cluster for which you want to describe usage limits.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsClusterIdentifier :: Lens.Lens' DescribeUsageLimits (Core.Maybe Types.String)
dulsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED dulsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The feature type for which you want to describe usage limits.
--
-- /Note:/ Consider using 'featureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsFeatureType :: Lens.Lens' DescribeUsageLimits (Core.Maybe Types.UsageLimitFeatureType)
dulsFeatureType = Lens.field @"featureType"
{-# DEPRECATED dulsFeatureType "Use generic-lens or generic-optics with 'featureType' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeUsageLimits' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsMarker :: Lens.Lens' DescribeUsageLimits (Core.Maybe Types.String)
dulsMarker = Lens.field @"marker"
{-# DEPRECATED dulsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsMaxRecords :: Lens.Lens' DescribeUsageLimits (Core.Maybe Core.Int)
dulsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dulsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A tag key or keys for which you want to return all matching usage limit objects that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the usage limit objects have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsTagKeys :: Lens.Lens' DescribeUsageLimits (Core.Maybe [Types.String])
dulsTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dulsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | A tag value or values for which you want to return all matching usage limit objects that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the usage limit objects that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsTagValues :: Lens.Lens' DescribeUsageLimits (Core.Maybe [Types.String])
dulsTagValues = Lens.field @"tagValues"
{-# DEPRECATED dulsTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | The identifier of the usage limit to describe.
--
-- /Note:/ Consider using 'usageLimitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulsUsageLimitId :: Lens.Lens' DescribeUsageLimits (Core.Maybe Types.String)
dulsUsageLimitId = Lens.field @"usageLimitId"
{-# DEPRECATED dulsUsageLimitId "Use generic-lens or generic-optics with 'usageLimitId' instead." #-}

instance Core.AWSRequest DescribeUsageLimits where
  type Rs DescribeUsageLimits = DescribeUsageLimitsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeUsageLimits")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" Core.<$> clusterIdentifier)
                Core.<> (Core.toQueryValue "FeatureType" Core.<$> featureType)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue
                            "TagKeys"
                            (Core.toQueryList "TagKey" Core.<$> tagKeys)
                        )
                Core.<> ( Core.toQueryValue
                            "TagValues"
                            (Core.toQueryList "TagValue" Core.<$> tagValues)
                        )
                Core.<> (Core.toQueryValue "UsageLimitId" Core.<$> usageLimitId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeUsageLimitsResult"
      ( \s h x ->
          DescribeUsageLimitsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "UsageLimits" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeUsageLimits where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"usageLimits" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeUsageLimitsResponse' smart constructor.
data DescribeUsageLimitsResponse = DescribeUsageLimitsResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | Contains the output from the 'DescribeUsageLimits' action.
    usageLimits :: Core.Maybe [Types.UsageLimit],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsageLimitsResponse' value with any optional fields omitted.
mkDescribeUsageLimitsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUsageLimitsResponse
mkDescribeUsageLimitsResponse responseStatus =
  DescribeUsageLimitsResponse'
    { marker = Core.Nothing,
      usageLimits = Core.Nothing,
      responseStatus
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulrrsMarker :: Lens.Lens' DescribeUsageLimitsResponse (Core.Maybe Types.String)
dulrrsMarker = Lens.field @"marker"
{-# DEPRECATED dulrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Contains the output from the 'DescribeUsageLimits' action.
--
-- /Note:/ Consider using 'usageLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulrrsUsageLimits :: Lens.Lens' DescribeUsageLimitsResponse (Core.Maybe [Types.UsageLimit])
dulrrsUsageLimits = Lens.field @"usageLimits"
{-# DEPRECATED dulrrsUsageLimits "Use generic-lens or generic-optics with 'usageLimits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulrrsResponseStatus :: Lens.Lens' DescribeUsageLimitsResponse Core.Int
dulrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dulrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
