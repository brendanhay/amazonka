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
    dtTagValues,
    dtResourceType,
    dtResourceName,
    dtTagKeys,
    dtMarker,
    dtMaxRecords,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtsrsMarker,
    dtsrsTaggedResources,
    dtsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
    tagValues :: Lude.Maybe [Lude.Text],
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
    resourceType :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for which you want to describe the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
    resourceName :: Lude.Maybe Lude.Text,
    -- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
    tagKeys :: Lude.Maybe [Lude.Text],
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'tagValues' - A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
-- * 'resourceType' - The type of resource with which you want to view tags. Valid resource types are:
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
-- * 'resourceName' - The Amazon Resource Name (ARN) for which you want to describe the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
-- * 'tagKeys' - A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
-- * 'maxRecords' - The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
mkDescribeTags ::
  DescribeTags
mkDescribeTags =
  DescribeTags'
    { tagValues = Lude.Nothing,
      resourceType = Lude.Nothing,
      resourceName = Lude.Nothing,
      tagKeys = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagValues :: Lens.Lens' DescribeTags (Lude.Maybe [Lude.Text])
dtTagValues = Lens.lens (tagValues :: DescribeTags -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeTags)
{-# DEPRECATED dtTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

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
dtResourceType :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Text)
dtResourceType = Lens.lens (resourceType :: DescribeTags -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: DescribeTags)
{-# DEPRECATED dtResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) for which you want to describe the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceName :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Text)
dtResourceName = Lens.lens (resourceName :: DescribeTags -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: DescribeTags)
{-# DEPRECATED dtResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagKeys :: Lens.Lens' DescribeTags (Lude.Maybe [Lude.Text])
dtTagKeys = Lens.lens (tagKeys :: DescribeTags -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeTags)
{-# DEPRECATED dtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMarker :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Text)
dtMarker = Lens.lens (marker :: DescribeTags -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTags)
{-# DEPRECATED dtMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMaxRecords :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Int)
dtMaxRecords = Lens.lens (maxRecords :: DescribeTags -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeTags)
{-# DEPRECATED dtMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeTags where
  page rq rs
    | Page.stop (rs Lens.^. dtsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dtsrsTaggedResources) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dtMarker Lens..~ rs Lens.^. dtsrsMarker

instance Lude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "TaggedResources" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "TaggedResource")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTags where
  toQuery DescribeTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeTags" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "ResourceType" Lude.=: resourceType,
        "ResourceName" Lude.=: resourceName,
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- |
--
-- /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of tags with their associated resources.
    taggedResources :: Lude.Maybe [TaggedResource],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'taggedResources' - A list of tags with their associated resources.
-- * 'responseStatus' - The response status code.
mkDescribeTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTagsResponse
mkDescribeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { marker = Lude.Nothing,
      taggedResources = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsMarker :: Lens.Lens' DescribeTagsResponse (Lude.Maybe Lude.Text)
dtsrsMarker = Lens.lens (marker :: DescribeTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTagsResponse)
{-# DEPRECATED dtsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of tags with their associated resources.
--
-- /Note:/ Consider using 'taggedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsTaggedResources :: Lens.Lens' DescribeTagsResponse (Lude.Maybe [TaggedResource])
dtsrsTaggedResources = Lens.lens (taggedResources :: DescribeTagsResponse -> Lude.Maybe [TaggedResource]) (\s a -> s {taggedResources = a} :: DescribeTagsResponse)
{-# DEPRECATED dtsrsTaggedResources "Use generic-lens or generic-optics with 'taggedResources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsResponseStatus :: Lens.Lens' DescribeTagsResponse Lude.Int
dtsrsResponseStatus = Lens.lens (responseStatus :: DescribeTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagsResponse)
{-# DEPRECATED dtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
