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
    dcvClusterVersion,
    dcvMarker,
    dcvMaxRecords,

    -- * Destructuring the response
    DescribeClusterVersionsResponse (..),
    mkDescribeClusterVersionsResponse,

    -- ** Response lenses
    dcvrrsClusterVersions,
    dcvrrsMarker,
    dcvrrsResponseStatus,
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
    clusterParameterGroupFamily :: Core.Maybe Types.String,
    -- | The specific cluster version to return.
    --
    -- Example: @1.0@
    clusterVersion :: Core.Maybe Types.String,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterVersions' value with any optional fields omitted.
mkDescribeClusterVersions ::
  DescribeClusterVersions
mkDescribeClusterVersions =
  DescribeClusterVersions'
    { clusterParameterGroupFamily =
        Core.Nothing,
      clusterVersion = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
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
dcvClusterParameterGroupFamily :: Lens.Lens' DescribeClusterVersions (Core.Maybe Types.String)
dcvClusterParameterGroupFamily = Lens.field @"clusterParameterGroupFamily"
{-# DEPRECATED dcvClusterParameterGroupFamily "Use generic-lens or generic-optics with 'clusterParameterGroupFamily' instead." #-}

-- | The specific cluster version to return.
--
-- Example: @1.0@
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvClusterVersion :: Lens.Lens' DescribeClusterVersions (Core.Maybe Types.String)
dcvClusterVersion = Lens.field @"clusterVersion"
{-# DEPRECATED dcvClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvMarker :: Lens.Lens' DescribeClusterVersions (Core.Maybe Types.String)
dcvMarker = Lens.field @"marker"
{-# DEPRECATED dcvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvMaxRecords :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Int)
dcvMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcvMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeClusterVersions where
  type Rs DescribeClusterVersions = DescribeClusterVersionsResponse
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
            ( Core.pure ("Action", "DescribeClusterVersions")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue "ClusterParameterGroupFamily"
                            Core.<$> clusterParameterGroupFamily
                        )
                Core.<> (Core.toQueryValue "ClusterVersion" Core.<$> clusterVersion)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeClusterVersionsResult"
      ( \s h x ->
          DescribeClusterVersionsResponse'
            Core.<$> ( x Core..@? "ClusterVersions"
                         Core..<@> Core.parseXMLList "ClusterVersion"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClusterVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"clusterVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the output from the 'DescribeClusterVersions' action.
--
-- /See:/ 'mkDescribeClusterVersionsResponse' smart constructor.
data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse'
  { -- | A list of @Version@ elements.
    clusterVersions :: Core.Maybe [Types.ClusterVersion],
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterVersionsResponse' value with any optional fields omitted.
mkDescribeClusterVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClusterVersionsResponse
mkDescribeClusterVersionsResponse responseStatus =
  DescribeClusterVersionsResponse'
    { clusterVersions = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of @Version@ elements.
--
-- /Note:/ Consider using 'clusterVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsClusterVersions :: Lens.Lens' DescribeClusterVersionsResponse (Core.Maybe [Types.ClusterVersion])
dcvrrsClusterVersions = Lens.field @"clusterVersions"
{-# DEPRECATED dcvrrsClusterVersions "Use generic-lens or generic-optics with 'clusterVersions' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsMarker :: Lens.Lens' DescribeClusterVersionsResponse (Core.Maybe Types.String)
dcvrrsMarker = Lens.field @"marker"
{-# DEPRECATED dcvrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsResponseStatus :: Lens.Lens' DescribeClusterVersionsResponse Core.Int
dcvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
