{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeSnapshotCopyGrants (..),
    mkDescribeSnapshotCopyGrants,

    -- ** Request lenses
    dscgsMarker,
    dscgsMaxRecords,
    dscgsSnapshotCopyGrantName,
    dscgsTagKeys,
    dscgsTagValues,

    -- * Destructuring the response
    DescribeSnapshotCopyGrantsResponse (..),
    mkDescribeSnapshotCopyGrantsResponse,

    -- ** Response lenses
    dscgrrsMarker,
    dscgrrsSnapshotCopyGrants,
    dscgrrsResponseStatus,
  )
where

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
  { -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Core.Maybe Types.String,
    -- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Types.String],
    -- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotCopyGrants' value with any optional fields omitted.
mkDescribeSnapshotCopyGrants ::
  DescribeSnapshotCopyGrants
mkDescribeSnapshotCopyGrants =
  DescribeSnapshotCopyGrants'
    { marker = Core.Nothing,
      maxRecords = Core.Nothing,
      snapshotCopyGrantName = Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing
    }

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsMarker :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Types.String)
dscgsMarker = Lens.field @"marker"
{-# DEPRECATED dscgsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsMaxRecords :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Core.Int)
dscgsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dscgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the snapshot copy grant.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsSnapshotCopyGrantName :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Types.String)
dscgsSnapshotCopyGrantName = Lens.field @"snapshotCopyGrantName"
{-# DEPRECATED dscgsSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

-- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsTagKeys :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe [Types.String])
dscgsTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dscgsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgsTagValues :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe [Types.String])
dscgsTagValues = Lens.field @"tagValues"
{-# DEPRECATED dscgsTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

instance Core.AWSRequest DescribeSnapshotCopyGrants where
  type
    Rs DescribeSnapshotCopyGrants =
      DescribeSnapshotCopyGrantsResponse
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
            ( Core.pure ("Action", "DescribeSnapshotCopyGrants")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue "SnapshotCopyGrantName"
                            Core.<$> snapshotCopyGrantName
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
      "DescribeSnapshotCopyGrantsResult"
      ( \s h x ->
          DescribeSnapshotCopyGrantsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "SnapshotCopyGrants"
                         Core..<@> Core.parseXMLList "SnapshotCopyGrant"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSnapshotCopyGrants where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"snapshotCopyGrants" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- |
--
-- /See:/ 'mkDescribeSnapshotCopyGrantsResponse' smart constructor.
data DescribeSnapshotCopyGrantsResponse = DescribeSnapshotCopyGrantsResponse'
  { -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
    marker :: Core.Maybe Types.Marker,
    -- | The list of @SnapshotCopyGrant@ objects.
    snapshotCopyGrants :: Core.Maybe [Types.SnapshotCopyGrant],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotCopyGrantsResponse' value with any optional fields omitted.
mkDescribeSnapshotCopyGrantsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSnapshotCopyGrantsResponse
mkDescribeSnapshotCopyGrantsResponse responseStatus =
  DescribeSnapshotCopyGrantsResponse'
    { marker = Core.Nothing,
      snapshotCopyGrants = Core.Nothing,
      responseStatus
    }

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrrsMarker :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Core.Maybe Types.Marker)
dscgrrsMarker = Lens.field @"marker"
{-# DEPRECATED dscgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of @SnapshotCopyGrant@ objects.
--
-- /Note:/ Consider using 'snapshotCopyGrants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrrsSnapshotCopyGrants :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Core.Maybe [Types.SnapshotCopyGrant])
dscgrrsSnapshotCopyGrants = Lens.field @"snapshotCopyGrants"
{-# DEPRECATED dscgrrsSnapshotCopyGrants "Use generic-lens or generic-optics with 'snapshotCopyGrants' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrrsResponseStatus :: Lens.Lens' DescribeSnapshotCopyGrantsResponse Core.Int
dscgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dscgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
