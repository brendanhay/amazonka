{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeClusterVersions (..)
    , mkDescribeClusterVersions
    -- ** Request lenses
    , dcvClusterParameterGroupFamily
    , dcvClusterVersion
    , dcvMarker
    , dcvMaxRecords

    -- * Destructuring the response
    , DescribeClusterVersionsResponse (..)
    , mkDescribeClusterVersionsResponse
    -- ** Response lenses
    , dcvrrsClusterVersions
    , dcvrrsMarker
    , dcvrrsResponseStatus
    ) where

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
  { clusterParameterGroupFamily :: Core.Maybe Core.Text
    -- ^ The name of a specific cluster parameter group family to return details for.
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
  , clusterVersion :: Core.Maybe Core.Text
    -- ^ The specific cluster version to return.
--
-- Example: @1.0@ 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterVersions' value with any optional fields omitted.
mkDescribeClusterVersions
    :: DescribeClusterVersions
mkDescribeClusterVersions
  = DescribeClusterVersions'{clusterParameterGroupFamily =
                               Core.Nothing,
                             clusterVersion = Core.Nothing, marker = Core.Nothing,
                             maxRecords = Core.Nothing}

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
dcvClusterParameterGroupFamily :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Text)
dcvClusterParameterGroupFamily = Lens.field @"clusterParameterGroupFamily"
{-# INLINEABLE dcvClusterParameterGroupFamily #-}
{-# DEPRECATED clusterParameterGroupFamily "Use generic-lens or generic-optics with 'clusterParameterGroupFamily' instead"  #-}

-- | The specific cluster version to return.
--
-- Example: @1.0@ 
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvClusterVersion :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Text)
dcvClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE dcvClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvMarker :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Text)
dcvMarker = Lens.field @"marker"
{-# INLINEABLE dcvMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvMaxRecords :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Int)
dcvMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dcvMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeClusterVersions where
        toQuery DescribeClusterVersions{..}
          = Core.toQueryPair "Action"
              ("DescribeClusterVersions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ClusterParameterGroupFamily")
                clusterParameterGroupFamily
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterVersion")
                clusterVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeClusterVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeClusterVersions where
        type Rs DescribeClusterVersions = DescribeClusterVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeClusterVersionsResult"
              (\ s h x ->
                 DescribeClusterVersionsResponse' Core.<$>
                   (x Core..@? "ClusterVersions" Core..<@>
                      Core.parseXMLList "ClusterVersion")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeClusterVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"clusterVersions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the output from the 'DescribeClusterVersions' action. 
--
-- /See:/ 'mkDescribeClusterVersionsResponse' smart constructor.
data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse'
  { clusterVersions :: Core.Maybe [Types.ClusterVersion]
    -- ^ A list of @Version@ elements. 
  , marker :: Core.Maybe Core.Text
    -- ^ A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterVersionsResponse' value with any optional fields omitted.
mkDescribeClusterVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClusterVersionsResponse
mkDescribeClusterVersionsResponse responseStatus
  = DescribeClusterVersionsResponse'{clusterVersions = Core.Nothing,
                                     marker = Core.Nothing, responseStatus}

-- | A list of @Version@ elements. 
--
-- /Note:/ Consider using 'clusterVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsClusterVersions :: Lens.Lens' DescribeClusterVersionsResponse (Core.Maybe [Types.ClusterVersion])
dcvrrsClusterVersions = Lens.field @"clusterVersions"
{-# INLINEABLE dcvrrsClusterVersions #-}
{-# DEPRECATED clusterVersions "Use generic-lens or generic-optics with 'clusterVersions' instead"  #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsMarker :: Lens.Lens' DescribeClusterVersionsResponse (Core.Maybe Core.Text)
dcvrrsMarker = Lens.field @"marker"
{-# INLINEABLE dcvrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsResponseStatus :: Lens.Lens' DescribeClusterVersionsResponse Core.Int
dcvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
