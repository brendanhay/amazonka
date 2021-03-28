{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeOrderableClusterOptions (..)
    , mkDescribeOrderableClusterOptions
    -- ** Request lenses
    , docoClusterVersion
    , docoMarker
    , docoMaxRecords
    , docoNodeType

    -- * Destructuring the response
    , DescribeOrderableClusterOptionsResponse (..)
    , mkDescribeOrderableClusterOptionsResponse
    -- ** Response lenses
    , docorrsMarker
    , docorrsOrderableClusterOptions
    , docorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeOrderableClusterOptions' smart constructor.
data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions'
  { clusterVersion :: Core.Maybe Core.Text
    -- ^ The version filter value. Specify this parameter to show only the available offerings matching the specified version.
--
-- Default: All versions.
-- Constraints: Must be one of the version returned from 'DescribeClusterVersions' .
  , marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeOrderableClusterOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The node type filter value. Specify this parameter to show only the available offerings matching the specified node type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrderableClusterOptions' value with any optional fields omitted.
mkDescribeOrderableClusterOptions
    :: DescribeOrderableClusterOptions
mkDescribeOrderableClusterOptions
  = DescribeOrderableClusterOptions'{clusterVersion = Core.Nothing,
                                     marker = Core.Nothing, maxRecords = Core.Nothing,
                                     nodeType = Core.Nothing}

-- | The version filter value. Specify this parameter to show only the available offerings matching the specified version.
--
-- Default: All versions.
-- Constraints: Must be one of the version returned from 'DescribeClusterVersions' .
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docoClusterVersion :: Lens.Lens' DescribeOrderableClusterOptions (Core.Maybe Core.Text)
docoClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE docoClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeOrderableClusterOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docoMarker :: Lens.Lens' DescribeOrderableClusterOptions (Core.Maybe Core.Text)
docoMarker = Lens.field @"marker"
{-# INLINEABLE docoMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docoMaxRecords :: Lens.Lens' DescribeOrderableClusterOptions (Core.Maybe Core.Int)
docoMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE docoMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The node type filter value. Specify this parameter to show only the available offerings matching the specified node type.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docoNodeType :: Lens.Lens' DescribeOrderableClusterOptions (Core.Maybe Core.Text)
docoNodeType = Lens.field @"nodeType"
{-# INLINEABLE docoNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

instance Core.ToQuery DescribeOrderableClusterOptions where
        toQuery DescribeOrderableClusterOptions{..}
          = Core.toQueryPair "Action"
              ("DescribeOrderableClusterOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterVersion")
                clusterVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NodeType") nodeType

instance Core.ToHeaders DescribeOrderableClusterOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeOrderableClusterOptions where
        type Rs DescribeOrderableClusterOptions =
             DescribeOrderableClusterOptionsResponse
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
          = Response.receiveXMLWrapper
              "DescribeOrderableClusterOptionsResult"
              (\ s h x ->
                 DescribeOrderableClusterOptionsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "OrderableClusterOptions" Core..<@>
                       Core.parseXMLList "OrderableClusterOption"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeOrderableClusterOptions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"orderableClusterOptions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the output from the 'DescribeOrderableClusterOptions' action. 
--
-- /See:/ 'mkDescribeOrderableClusterOptionsResponse' smart constructor.
data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
  , orderableClusterOptions :: Core.Maybe [Types.OrderableClusterOption]
    -- ^ An @OrderableClusterOption@ structure containing information about orderable options for the cluster.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrderableClusterOptionsResponse' value with any optional fields omitted.
mkDescribeOrderableClusterOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOrderableClusterOptionsResponse
mkDescribeOrderableClusterOptionsResponse responseStatus
  = DescribeOrderableClusterOptionsResponse'{marker = Core.Nothing,
                                             orderableClusterOptions = Core.Nothing, responseStatus}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docorrsMarker :: Lens.Lens' DescribeOrderableClusterOptionsResponse (Core.Maybe Core.Text)
docorrsMarker = Lens.field @"marker"
{-# INLINEABLE docorrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | An @OrderableClusterOption@ structure containing information about orderable options for the cluster.
--
-- /Note:/ Consider using 'orderableClusterOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docorrsOrderableClusterOptions :: Lens.Lens' DescribeOrderableClusterOptionsResponse (Core.Maybe [Types.OrderableClusterOption])
docorrsOrderableClusterOptions = Lens.field @"orderableClusterOptions"
{-# INLINEABLE docorrsOrderableClusterOptions #-}
{-# DEPRECATED orderableClusterOptions "Use generic-lens or generic-optics with 'orderableClusterOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docorrsResponseStatus :: Lens.Lens' DescribeOrderableClusterOptionsResponse Core.Int
docorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE docorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
