{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeSnapshotCopyGrants
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot copy grants owned by the AWS account in the destination region.
--
-- For more information about managing snapshot copy grants, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ . 
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeSnapshotCopyGrants
    (
    -- * Creating a request
      DescribeSnapshotCopyGrants (..)
    , mkDescribeSnapshotCopyGrants
    -- ** Request lenses
    , dscgsMarker
    , dscgsMaxRecords
    , dscgsSnapshotCopyGrantName
    , dscgsTagKeys
    , dscgsTagValues

    -- * Destructuring the response
    , DescribeSnapshotCopyGrantsResponse (..)
    , mkDescribeSnapshotCopyGrantsResponse
    -- ** Response lenses
    , dscgrrsMarker
    , dscgrrsSnapshotCopyGrants
    , dscgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result of the @DescribeSnapshotCopyGrants@ action.
--
-- /See:/ 'mkDescribeSnapshotCopyGrants' smart constructor.
data DescribeSnapshotCopyGrants = DescribeSnapshotCopyGrants'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both. 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
  , snapshotCopyGrantName :: Core.Maybe Core.Text
    -- ^ The name of the snapshot copy grant.
  , tagKeys :: Core.Maybe [Core.Text]
    -- ^ A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
  , tagValues :: Core.Maybe [Core.Text]
    -- ^ A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotCopyGrants' value with any optional fields omitted.
mkDescribeSnapshotCopyGrants
    :: DescribeSnapshotCopyGrants
mkDescribeSnapshotCopyGrants
  = DescribeSnapshotCopyGrants'{marker = Core.Nothing,
                                maxRecords = Core.Nothing, snapshotCopyGrantName = Core.Nothing,
                                tagKeys = Core.Nothing, tagValues = Core.Nothing}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsMarker :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Core.Text)
dscgsMarker = Lens.field @"marker"
{-# INLINEABLE dscgsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsMaxRecords :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Core.Int)
dscgsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dscgsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The name of the snapshot copy grant.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsSnapshotCopyGrantName :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Core.Text)
dscgsSnapshotCopyGrantName = Lens.field @"snapshotCopyGrantName"
{-# INLINEABLE dscgsSnapshotCopyGrantName #-}
{-# DEPRECATED snapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead"  #-}

-- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsTagKeys :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe [Core.Text])
dscgsTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE dscgsTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

-- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsTagValues :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe [Core.Text])
dscgsTagValues = Lens.field @"tagValues"
{-# INLINEABLE dscgsTagValues #-}
{-# DEPRECATED tagValues "Use generic-lens or generic-optics with 'tagValues' instead"  #-}

instance Core.ToQuery DescribeSnapshotCopyGrants where
        toQuery DescribeSnapshotCopyGrants{..}
          = Core.toQueryPair "Action"
              ("DescribeSnapshotCopyGrants" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotCopyGrantName")
                snapshotCopyGrantName
              Core.<>
              Core.toQueryPair "TagKeys"
                (Core.maybe Core.mempty (Core.toQueryList "TagKey") tagKeys)
              Core.<>
              Core.toQueryPair "TagValues"
                (Core.maybe Core.mempty (Core.toQueryList "TagValue") tagValues)

instance Core.ToHeaders DescribeSnapshotCopyGrants where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSnapshotCopyGrants where
        type Rs DescribeSnapshotCopyGrants =
             DescribeSnapshotCopyGrantsResponse
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
          = Response.receiveXMLWrapper "DescribeSnapshotCopyGrantsResult"
              (\ s h x ->
                 DescribeSnapshotCopyGrantsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "SnapshotCopyGrants" Core..<@>
                       Core.parseXMLList "SnapshotCopyGrant"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSnapshotCopyGrants where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"snapshotCopyGrants" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeSnapshotCopyGrantsResponse' smart constructor.
data DescribeSnapshotCopyGrantsResponse = DescribeSnapshotCopyGrantsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both. 
  , snapshotCopyGrants :: Core.Maybe [Types.SnapshotCopyGrant]
    -- ^ The list of @SnapshotCopyGrant@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotCopyGrantsResponse' value with any optional fields omitted.
mkDescribeSnapshotCopyGrantsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSnapshotCopyGrantsResponse
mkDescribeSnapshotCopyGrantsResponse responseStatus
  = DescribeSnapshotCopyGrantsResponse'{marker = Core.Nothing,
                                        snapshotCopyGrants = Core.Nothing, responseStatus}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrrsMarker :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Core.Maybe Core.Text)
dscgrrsMarker = Lens.field @"marker"
{-# INLINEABLE dscgrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The list of @SnapshotCopyGrant@ objects.
--
-- /Note:/ Consider using 'snapshotCopyGrants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrrsSnapshotCopyGrants :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Core.Maybe [Types.SnapshotCopyGrant])
dscgrrsSnapshotCopyGrants = Lens.field @"snapshotCopyGrants"
{-# INLINEABLE dscgrrsSnapshotCopyGrants #-}
{-# DEPRECATED snapshotCopyGrants "Use generic-lens or generic-optics with 'snapshotCopyGrants' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrrsResponseStatus :: Lens.Lens' DescribeSnapshotCopyGrantsResponse Core.Int
dscgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dscgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
