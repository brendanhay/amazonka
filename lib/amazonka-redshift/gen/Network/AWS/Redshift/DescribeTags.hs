{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags. You can return tags from a specific resource by specifying an ARN, or you can return all tags for a given type of resource, such as clusters, snapshots, and so on.
--
-- The following are limitations for @DescribeTags@ :
--
--     * You cannot specify an ARN and a resource-type value together in the same request.
--
--
--     * You cannot use the @MaxRecords@ and @Marker@ parameters together with the ARN parameter.
--
--
--     * The @MaxRecords@ parameter can be a range from 10 to 50 results to return in a request.
--
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all resources that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all resources that have any combination of those values are returned.
-- If both tag keys and values are omitted from the request, resources are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtMarker,
    dtMaxRecords,
    dtResourceName,
    dtResourceType,
    dtTagKeys,
    dtTagValues,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtrrsMarker,
    dtrrsTaggedResources,
    dtrrsResponseStatus,
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
-- /See:/ 'mkDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
    maxRecords :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) for which you want to describe the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
    resourceName :: Core.Maybe Types.String,
    -- | The type of resource with which you want to view tags. Valid resource types are:
    --
    --
    --     * Cluster
    --
    --
    --     * CIDR/IP
    --
    --
    --     * EC2 security group
    --
    --
    --     * Snapshot
    --
    --
    --     * Cluster security group
    --
    --
    --     * Subnet group
    --
    --
    --     * HSM connection
    --
    --
    --     * HSM certificate
    --
    --
    --     * Parameter group
    --
    --
    --     * Snapshot copy grant
    --
    --
    -- For more information about Amazon Redshift resource types and constructing ARNs, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Specifying Policy Elements: Actions, Effects, Resources, and Principals> in the Amazon Redshift Cluster Management Guide.
    resourceType :: Core.Maybe Types.String,
    -- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Types.String],
    -- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTags' value with any optional fields omitted.
mkDescribeTags ::
  DescribeTags
mkDescribeTags =
  DescribeTags'
    { marker = Core.Nothing,
      maxRecords = Core.Nothing,
      resourceName = Core.Nothing,
      resourceType = Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMarker :: Lens.Lens' DescribeTags (Core.Maybe Types.String)
dtMarker = Lens.field @"marker"
{-# DEPRECATED dtMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMaxRecords :: Lens.Lens' DescribeTags (Core.Maybe Core.Int)
dtMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dtMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The Amazon Resource Name (ARN) for which you want to describe the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceName :: Lens.Lens' DescribeTags (Core.Maybe Types.String)
dtResourceName = Lens.field @"resourceName"
{-# DEPRECATED dtResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The type of resource with which you want to view tags. Valid resource types are:
--
--
--     * Cluster
--
--
--     * CIDR/IP
--
--
--     * EC2 security group
--
--
--     * Snapshot
--
--
--     * Cluster security group
--
--
--     * Subnet group
--
--
--     * HSM connection
--
--
--     * HSM certificate
--
--
--     * Parameter group
--
--
--     * Snapshot copy grant
--
--
-- For more information about Amazon Redshift resource types and constructing ARNs, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Specifying Policy Elements: Actions, Effects, Resources, and Principals> in the Amazon Redshift Cluster Management Guide.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceType :: Lens.Lens' DescribeTags (Core.Maybe Types.String)
dtResourceType = Lens.field @"resourceType"
{-# DEPRECATED dtResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagKeys :: Lens.Lens' DescribeTags (Core.Maybe [Types.String])
dtTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagValues :: Lens.Lens' DescribeTags (Core.Maybe [Types.String])
dtTagValues = Lens.field @"tagValues"
{-# DEPRECATED dtTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

instance Core.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
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
            ( Core.pure ("Action", "DescribeTags")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "ResourceName" Core.<$> resourceName)
                Core.<> (Core.toQueryValue "ResourceType" Core.<$> resourceType)
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
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "TaggedResources"
                         Core..<@> Core.parseXMLList "TaggedResource"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTags where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"taggedResources" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- |
--
-- /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | A list of tags with their associated resources.
    taggedResources :: Core.Maybe [Types.TaggedResource],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTagsResponse' value with any optional fields omitted.
mkDescribeTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTagsResponse
mkDescribeTagsResponse responseStatus =
  DescribeTagsResponse'
    { marker = Core.Nothing,
      taggedResources = Core.Nothing,
      responseStatus
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsMarker :: Lens.Lens' DescribeTagsResponse (Core.Maybe Types.String)
dtrrsMarker = Lens.field @"marker"
{-# DEPRECATED dtrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of tags with their associated resources.
--
-- /Note:/ Consider using 'taggedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTaggedResources :: Lens.Lens' DescribeTagsResponse (Core.Maybe [Types.TaggedResource])
dtrrsTaggedResources = Lens.field @"taggedResources"
{-# DEPRECATED dtrrsTaggedResources "Use generic-lens or generic-optics with 'taggedResources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTagsResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
