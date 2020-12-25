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
    dcsgClusterSecurityGroupName,
    dcsgMarker,
    dcsgMaxRecords,
    dcsgTagKeys,
    dcsgTagValues,

    -- * Destructuring the response
    DescribeClusterSecurityGroupsResponse (..),
    mkDescribeClusterSecurityGroupsResponse,

    -- ** Response lenses
    dcsgrrsClusterSecurityGroups,
    dcsgrrsMarker,
    dcsgrrsResponseStatus,
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
-- /See:/ 'mkDescribeClusterSecurityGroups' smart constructor.
data DescribeClusterSecurityGroups = DescribeClusterSecurityGroups'
  { -- | The name of a cluster security group for which you are requesting details. You can specify either the __Marker__ parameter or a __ClusterSecurityGroupName__ parameter, but not both.
    --
    -- Example: @securitygroup1@
    clusterSecurityGroupName :: Core.Maybe Types.String,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSecurityGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __ClusterSecurityGroupName__ parameter or the __Marker__ parameter, but not both.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | A tag key or keys for which you want to return all matching cluster security groups that are associated with the specified key or keys. For example, suppose that you have security groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Types.String],
    -- | A tag value or values for which you want to return all matching cluster security groups that are associated with the specified tag value or values. For example, suppose that you have security groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterSecurityGroups' value with any optional fields omitted.
mkDescribeClusterSecurityGroups ::
  DescribeClusterSecurityGroups
mkDescribeClusterSecurityGroups =
  DescribeClusterSecurityGroups'
    { clusterSecurityGroupName =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing
    }

-- | The name of a cluster security group for which you are requesting details. You can specify either the __Marker__ parameter or a __ClusterSecurityGroupName__ parameter, but not both.
--
-- Example: @securitygroup1@
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgClusterSecurityGroupName :: Lens.Lens' DescribeClusterSecurityGroups (Core.Maybe Types.String)
dcsgClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# DEPRECATED dcsgClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSecurityGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterSecurityGroupName__ parameter or the __Marker__ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgMarker :: Lens.Lens' DescribeClusterSecurityGroups (Core.Maybe Types.String)
dcsgMarker = Lens.field @"marker"
{-# DEPRECATED dcsgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgMaxRecords :: Lens.Lens' DescribeClusterSecurityGroups (Core.Maybe Core.Int)
dcsgMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcsgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A tag key or keys for which you want to return all matching cluster security groups that are associated with the specified key or keys. For example, suppose that you have security groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgTagKeys :: Lens.Lens' DescribeClusterSecurityGroups (Core.Maybe [Types.String])
dcsgTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dcsgTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | A tag value or values for which you want to return all matching cluster security groups that are associated with the specified tag value or values. For example, suppose that you have security groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgTagValues :: Lens.Lens' DescribeClusterSecurityGroups (Core.Maybe [Types.String])
dcsgTagValues = Lens.field @"tagValues"
{-# DEPRECATED dcsgTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

instance Core.AWSRequest DescribeClusterSecurityGroups where
  type
    Rs DescribeClusterSecurityGroups =
      DescribeClusterSecurityGroupsResponse
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
            ( Core.pure ("Action", "DescribeClusterSecurityGroups")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue "ClusterSecurityGroupName"
                            Core.<$> clusterSecurityGroupName
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
      "DescribeClusterSecurityGroupsResult"
      ( \s h x ->
          DescribeClusterSecurityGroupsResponse'
            Core.<$> ( x Core..@? "ClusterSecurityGroups"
                         Core..<@> Core.parseXMLList "ClusterSecurityGroup"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClusterSecurityGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"clusterSecurityGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- |
--
-- /See:/ 'mkDescribeClusterSecurityGroupsResponse' smart constructor.
data DescribeClusterSecurityGroupsResponse = DescribeClusterSecurityGroupsResponse'
  { -- | A list of 'ClusterSecurityGroup' instances.
    clusterSecurityGroups :: Core.Maybe [Types.ClusterSecurityGroup],
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterSecurityGroupsResponse' value with any optional fields omitted.
mkDescribeClusterSecurityGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClusterSecurityGroupsResponse
mkDescribeClusterSecurityGroupsResponse responseStatus =
  DescribeClusterSecurityGroupsResponse'
    { clusterSecurityGroups =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of 'ClusterSecurityGroup' instances.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrrsClusterSecurityGroups :: Lens.Lens' DescribeClusterSecurityGroupsResponse (Core.Maybe [Types.ClusterSecurityGroup])
dcsgrrsClusterSecurityGroups = Lens.field @"clusterSecurityGroups"
{-# DEPRECATED dcsgrrsClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrrsMarker :: Lens.Lens' DescribeClusterSecurityGroupsResponse (Core.Maybe Types.String)
dcsgrrsMarker = Lens.field @"marker"
{-# DEPRECATED dcsgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrrsResponseStatus :: Lens.Lens' DescribeClusterSecurityGroupsResponse Core.Int
dcsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
