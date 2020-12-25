{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dcsgsClusterSubnetGroupName,
    dcsgsMarker,
    dcsgsMaxRecords,
    dcsgsTagKeys,
    dcsgsTagValues,

    -- * Destructuring the response
    DescribeClusterSubnetGroupsResponse (..),
    mkDescribeClusterSubnetGroupsResponse,

    -- ** Response lenses
    dcsgrfrsClusterSubnetGroups,
    dcsgrfrsMarker,
    dcsgrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeClusterSubnetGroups' smart constructor.
data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups'
  { -- | The name of the cluster subnet group for which information is requested.
    clusterSubnetGroupName :: Core.Maybe Types.String,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSubnetGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | A tag key or keys for which you want to return all matching cluster subnet groups that are associated with the specified key or keys. For example, suppose that you have subnet groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the subnet groups that have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Types.String],
    -- | A tag value or values for which you want to return all matching cluster subnet groups that are associated with the specified tag value or values. For example, suppose that you have subnet groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the subnet groups that have either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterSubnetGroups' value with any optional fields omitted.
mkDescribeClusterSubnetGroups ::
  DescribeClusterSubnetGroups
mkDescribeClusterSubnetGroups =
  DescribeClusterSubnetGroups'
    { clusterSubnetGroupName =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing
    }

-- | The name of the cluster subnet group for which information is requested.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsClusterSubnetGroupName :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe Types.String)
dcsgsClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# DEPRECATED dcsgsClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSubnetGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsMarker :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe Types.String)
dcsgsMarker = Lens.field @"marker"
{-# DEPRECATED dcsgsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsMaxRecords :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe Core.Int)
dcsgsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcsgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A tag key or keys for which you want to return all matching cluster subnet groups that are associated with the specified key or keys. For example, suppose that you have subnet groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the subnet groups that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsTagKeys :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe [Types.String])
dcsgsTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dcsgsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | A tag value or values for which you want to return all matching cluster subnet groups that are associated with the specified tag value or values. For example, suppose that you have subnet groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the subnet groups that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsTagValues :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe [Types.String])
dcsgsTagValues = Lens.field @"tagValues"
{-# DEPRECATED dcsgsTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

instance Core.AWSRequest DescribeClusterSubnetGroups where
  type
    Rs DescribeClusterSubnetGroups =
      DescribeClusterSubnetGroupsResponse
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
            ( Core.pure ("Action", "DescribeClusterSubnetGroups")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue "ClusterSubnetGroupName"
                            Core.<$> clusterSubnetGroupName
                        )
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
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeClusterSubnetGroupsResult"
      ( \s h x ->
          DescribeClusterSubnetGroupsResponse'
            Core.<$> ( x Core..@? "ClusterSubnetGroups"
                         Core..<@> Core.parseXMLList "ClusterSubnetGroup"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClusterSubnetGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"clusterSubnetGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the output from the 'DescribeClusterSubnetGroups' action.
--
-- /See:/ 'mkDescribeClusterSubnetGroupsResponse' smart constructor.
data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse'
  { -- | A list of 'ClusterSubnetGroup' instances.
    clusterSubnetGroups :: Core.Maybe [Types.ClusterSubnetGroup],
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterSubnetGroupsResponse' value with any optional fields omitted.
mkDescribeClusterSubnetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClusterSubnetGroupsResponse
mkDescribeClusterSubnetGroupsResponse responseStatus =
  DescribeClusterSubnetGroupsResponse'
    { clusterSubnetGroups =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of 'ClusterSubnetGroup' instances.
--
-- /Note:/ Consider using 'clusterSubnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrfrsClusterSubnetGroups :: Lens.Lens' DescribeClusterSubnetGroupsResponse (Core.Maybe [Types.ClusterSubnetGroup])
dcsgrfrsClusterSubnetGroups = Lens.field @"clusterSubnetGroups"
{-# DEPRECATED dcsgrfrsClusterSubnetGroups "Use generic-lens or generic-optics with 'clusterSubnetGroups' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrfrsMarker :: Lens.Lens' DescribeClusterSubnetGroupsResponse (Core.Maybe Types.Marker)
dcsgrfrsMarker = Lens.field @"marker"
{-# DEPRECATED dcsgrfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrfrsResponseStatus :: Lens.Lens' DescribeClusterSubnetGroupsResponse Core.Int
dcsgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
