{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dcpgMarker,
    dcpgMaxRecords,
    dcpgParameterGroupName,
    dcpgTagKeys,
    dcpgTagValues,

    -- * Destructuring the response
    DescribeClusterParameterGroupsResponse (..),
    mkDescribeClusterParameterGroupsResponse,

    -- ** Response lenses
    dcpgrrsMarker,
    dcpgrrsParameterGroups,
    dcpgrrsResponseStatus,
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
-- /See:/ 'mkDescribeClusterParameterGroups' smart constructor.
data DescribeClusterParameterGroups = DescribeClusterParameterGroups'
  { -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameterGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of a specific parameter group for which to return details. By default, details about all parameter groups and the default parameter group are returned.
    parameterGroupName :: Core.Maybe Types.String,
    -- | A tag key or keys for which you want to return all matching cluster parameter groups that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the parameter groups that have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Types.String],
    -- | A tag value or values for which you want to return all matching cluster parameter groups that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the parameter groups that have either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterParameterGroups' value with any optional fields omitted.
mkDescribeClusterParameterGroups ::
  DescribeClusterParameterGroups
mkDescribeClusterParameterGroups =
  DescribeClusterParameterGroups'
    { marker = Core.Nothing,
      maxRecords = Core.Nothing,
      parameterGroupName = Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing
    }

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameterGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMarker :: Lens.Lens' DescribeClusterParameterGroups (Core.Maybe Types.String)
dcpgMarker = Lens.field @"marker"
{-# DEPRECATED dcpgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMaxRecords :: Lens.Lens' DescribeClusterParameterGroups (Core.Maybe Core.Int)
dcpgMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcpgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of a specific parameter group for which to return details. By default, details about all parameter groups and the default parameter group are returned.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgParameterGroupName :: Lens.Lens' DescribeClusterParameterGroups (Core.Maybe Types.String)
dcpgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED dcpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | A tag key or keys for which you want to return all matching cluster parameter groups that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the parameter groups that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgTagKeys :: Lens.Lens' DescribeClusterParameterGroups (Core.Maybe [Types.String])
dcpgTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dcpgTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | A tag value or values for which you want to return all matching cluster parameter groups that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the parameter groups that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgTagValues :: Lens.Lens' DescribeClusterParameterGroups (Core.Maybe [Types.String])
dcpgTagValues = Lens.field @"tagValues"
{-# DEPRECATED dcpgTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

instance Core.AWSRequest DescribeClusterParameterGroups where
  type
    Rs DescribeClusterParameterGroups =
      DescribeClusterParameterGroupsResponse
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
            ( Core.pure ("Action", "DescribeClusterParameterGroups")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue "ParameterGroupName"
                            Core.<$> parameterGroupName
                        )
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
      "DescribeClusterParameterGroupsResult"
      ( \s h x ->
          DescribeClusterParameterGroupsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "ParameterGroups"
                         Core..<@> Core.parseXMLList "ClusterParameterGroup"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClusterParameterGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"parameterGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the output from the 'DescribeClusterParameterGroups' action.
--
-- /See:/ 'mkDescribeClusterParameterGroupsResponse' smart constructor.
data DescribeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | A list of 'ClusterParameterGroup' instances. Each instance describes one cluster parameter group.
    parameterGroups :: Core.Maybe [Types.ClusterParameterGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterParameterGroupsResponse' value with any optional fields omitted.
mkDescribeClusterParameterGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClusterParameterGroupsResponse
mkDescribeClusterParameterGroupsResponse responseStatus =
  DescribeClusterParameterGroupsResponse'
    { marker = Core.Nothing,
      parameterGroups = Core.Nothing,
      responseStatus
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsMarker :: Lens.Lens' DescribeClusterParameterGroupsResponse (Core.Maybe Types.String)
dcpgrrsMarker = Lens.field @"marker"
{-# DEPRECATED dcpgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of 'ClusterParameterGroup' instances. Each instance describes one cluster parameter group.
--
-- /Note:/ Consider using 'parameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsParameterGroups :: Lens.Lens' DescribeClusterParameterGroupsResponse (Core.Maybe [Types.ClusterParameterGroup])
dcpgrrsParameterGroups = Lens.field @"parameterGroups"
{-# DEPRECATED dcpgrrsParameterGroups "Use generic-lens or generic-optics with 'parameterGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsResponseStatus :: Lens.Lens' DescribeClusterParameterGroupsResponse Core.Int
dcpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
