{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags.
--
-- You can use filters to limit the results. For example, you can query for the tags for a specific Auto Scaling group. You can specify multiple values for a filter. A tag must match at least one of the specified values for it to be included in the results.
-- You can also specify multiple filters. The result includes information for a particular tag only if it matches all the filters. If there's no match, no special message is returned.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtFilters,
    dtNextToken,
    dtMaxRecords,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtrsNextToken,
    dtrsTags,
    dtrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | One or more filters to scope the tags to return. The maximum number of filters per filter type (for example, @auto-scaling-group@ ) is 1000.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters to scope the tags to return. The maximum number of filters per filter type (for example, @auto-scaling-group@ ) is 1000.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
mkDescribeTags ::
  DescribeTags
mkDescribeTags =
  DescribeTags'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | One or more filters to scope the tags to return. The maximum number of filters per filter type (for example, @auto-scaling-group@ ) is 1000.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtFilters :: Lens.Lens' DescribeTags (Lude.Maybe [Filter])
dtFilters = Lens.lens (filters :: DescribeTags -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTags)
{-# DEPRECATED dtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtNextToken :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Text)
dtNextToken = Lens.lens (nextToken :: DescribeTags -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTags)
{-# DEPRECATED dtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMaxRecords :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Int)
dtMaxRecords = Lens.lens (maxRecords :: DescribeTags -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeTags)
{-# DEPRECATED dtMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeTags where
  page rq rs
    | Page.stop (rs Lens.^. dtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtrsTags) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtNextToken Lens..~ rs Lens.^. dtrsNextToken

instance Lude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
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
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more tags.
    tags :: Lude.Maybe [TagDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'tags' - One or more tags.
-- * 'responseStatus' - The response status code.
mkDescribeTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTagsResponse
mkDescribeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsNextToken :: Lens.Lens' DescribeTagsResponse (Lude.Maybe Lude.Text)
dtrsNextToken = Lens.lens (nextToken :: DescribeTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTags :: Lens.Lens' DescribeTagsResponse (Lude.Maybe [TagDescription])
dtrsTags = Lens.lens (tags :: DescribeTagsResponse -> Lude.Maybe [TagDescription]) (\s a -> s {tags = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTagsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
