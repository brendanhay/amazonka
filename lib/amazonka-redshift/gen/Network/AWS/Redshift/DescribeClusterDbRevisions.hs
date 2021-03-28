{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterDbRevisions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterDbRevision@ objects.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterDbRevisions
    (
    -- * Creating a request
      DescribeClusterDbRevisions (..)
    , mkDescribeClusterDbRevisions
    -- ** Request lenses
    , dcdrClusterIdentifier
    , dcdrMarker
    , dcdrMaxRecords

    -- * Destructuring the response
    , DescribeClusterDbRevisionsResponse (..)
    , mkDescribeClusterDbRevisionsResponse
    -- ** Response lenses
    , dcdrrrsClusterDbRevisions
    , dcdrrrsMarker
    , dcdrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClusterDbRevisions' smart constructor.
data DescribeClusterDbRevisions = DescribeClusterDbRevisions'
  { clusterIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for a cluster whose @ClusterDbRevisions@ you are requesting. This parameter is case sensitive. All clusters defined for an account are returned by default.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point for returning a set of response records. When the results of a @DescribeClusterDbRevisions@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request. 
--
-- Constraints: You can specify either the @ClusterIdentifier@ parameter, or the @marker@ parameter, but not both.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified MaxRecords value, a value is returned in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request. 
--
-- Default: 100
-- Constraints: minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterDbRevisions' value with any optional fields omitted.
mkDescribeClusterDbRevisions
    :: DescribeClusterDbRevisions
mkDescribeClusterDbRevisions
  = DescribeClusterDbRevisions'{clusterIdentifier = Core.Nothing,
                                marker = Core.Nothing, maxRecords = Core.Nothing}

-- | A unique identifier for a cluster whose @ClusterDbRevisions@ you are requesting. This parameter is case sensitive. All clusters defined for an account are returned by default.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrClusterIdentifier :: Lens.Lens' DescribeClusterDbRevisions (Core.Maybe Core.Text)
dcdrClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE dcdrClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | An optional parameter that specifies the starting point for returning a set of response records. When the results of a @DescribeClusterDbRevisions@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request. 
--
-- Constraints: You can specify either the @ClusterIdentifier@ parameter, or the @marker@ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrMarker :: Lens.Lens' DescribeClusterDbRevisions (Core.Maybe Core.Text)
dcdrMarker = Lens.field @"marker"
{-# INLINEABLE dcdrMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified MaxRecords value, a value is returned in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request. 
--
-- Default: 100
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrMaxRecords :: Lens.Lens' DescribeClusterDbRevisions (Core.Maybe Core.Int)
dcdrMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dcdrMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeClusterDbRevisions where
        toQuery DescribeClusterDbRevisions{..}
          = Core.toQueryPair "Action"
              ("DescribeClusterDbRevisions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterIdentifier")
                clusterIdentifier
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeClusterDbRevisions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeClusterDbRevisions where
        type Rs DescribeClusterDbRevisions =
             DescribeClusterDbRevisionsResponse
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
          = Response.receiveXMLWrapper "DescribeClusterDbRevisionsResult"
              (\ s h x ->
                 DescribeClusterDbRevisionsResponse' Core.<$>
                   (x Core..@? "ClusterDbRevisions" Core..<@>
                      Core.parseXMLList "ClusterDbRevision")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeClusterDbRevisions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"clusterDbRevisions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeClusterDbRevisionsResponse' smart constructor.
data DescribeClusterDbRevisionsResponse = DescribeClusterDbRevisionsResponse'
  { clusterDbRevisions :: Core.Maybe [Types.ClusterDbRevision]
    -- ^ A list of revisions.
  , marker :: Core.Maybe Core.Text
    -- ^ A string representing the starting point for the next set of revisions. If a value is returned in a response, you can retrieve the next set of revisions by providing the value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all revisions have already been returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeClusterDbRevisionsResponse' value with any optional fields omitted.
mkDescribeClusterDbRevisionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClusterDbRevisionsResponse
mkDescribeClusterDbRevisionsResponse responseStatus
  = DescribeClusterDbRevisionsResponse'{clusterDbRevisions =
                                          Core.Nothing,
                                        marker = Core.Nothing, responseStatus}

-- | A list of revisions.
--
-- /Note:/ Consider using 'clusterDbRevisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrrrsClusterDbRevisions :: Lens.Lens' DescribeClusterDbRevisionsResponse (Core.Maybe [Types.ClusterDbRevision])
dcdrrrsClusterDbRevisions = Lens.field @"clusterDbRevisions"
{-# INLINEABLE dcdrrrsClusterDbRevisions #-}
{-# DEPRECATED clusterDbRevisions "Use generic-lens or generic-optics with 'clusterDbRevisions' instead"  #-}

-- | A string representing the starting point for the next set of revisions. If a value is returned in a response, you can retrieve the next set of revisions by providing the value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all revisions have already been returned.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrrrsMarker :: Lens.Lens' DescribeClusterDbRevisionsResponse (Core.Maybe Core.Text)
dcdrrrsMarker = Lens.field @"marker"
{-# INLINEABLE dcdrrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrrrsResponseStatus :: Lens.Lens' DescribeClusterDbRevisionsResponse Core.Int
dcdrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcdrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
