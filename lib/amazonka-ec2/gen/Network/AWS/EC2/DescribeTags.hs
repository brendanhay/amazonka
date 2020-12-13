{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags for your EC2 resources.
--
-- For more information about tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtFilters,
    dtNextToken,
    dtDryRun,
    dtMaxResults,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtrsNextToken,
    dtrsTags,
    dtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The filters.
    --
    --
    --     * @key@ - The tag key.
    --
    --
    --     * @resource-id@ - The ID of the resource.
    --
    --
    --     * @resource-type@ - The resource type (@customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ | @fpga-image@ | @host-reservation@ | @image@ | @instance@ | @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ | @network-acl@ | @network-interface@ | @placement-group@ | @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@ | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ | @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of the tag. For example, specify "tag:Owner" for the filter name and "TeamA" for the filter value to find resources with the tag "Owner=TeamA".
    --
    --
    --     * @value@ - The tag value.
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call. This value can be between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'filters' - The filters.
--
--
--     * @key@ - The tag key.
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The resource type (@customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ | @fpga-image@ | @host-reservation@ | @image@ | @instance@ | @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ | @network-acl@ | @network-interface@ | @placement-group@ | @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@ | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ | @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ ).
--
--
--     * @tag@ :<key> - The key/value combination of the tag. For example, specify "tag:Owner" for the filter name and "TeamA" for the filter value to find resources with the tag "Owner=TeamA".
--
--
--     * @value@ - The tag value.
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. This value can be between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
mkDescribeTags ::
  DescribeTags
mkDescribeTags =
  DescribeTags'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
--
--     * @key@ - The tag key.
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The resource type (@customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ | @fpga-image@ | @host-reservation@ | @image@ | @instance@ | @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ | @network-acl@ | @network-interface@ | @placement-group@ | @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@ | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ | @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ ).
--
--
--     * @tag@ :<key> - The key/value combination of the tag. For example, specify "tag:Owner" for the filter name and "TeamA" for the filter value to find resources with the tag "Owner=TeamA".
--
--
--     * @value@ - The tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtFilters :: Lens.Lens' DescribeTags (Lude.Maybe [Filter])
dtFilters = Lens.lens (filters :: DescribeTags -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTags)
{-# DEPRECATED dtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtNextToken :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Text)
dtNextToken = Lens.lens (nextToken :: DescribeTags -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTags)
{-# DEPRECATED dtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDryRun :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Bool)
dtDryRun = Lens.lens (dryRun :: DescribeTags -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTags)
{-# DEPRECATED dtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. This value can be between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMaxResults :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Int)
dtMaxResults = Lens.lens (maxResults :: DescribeTags -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeTags)
{-# DEPRECATED dtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

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
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTagsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
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
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The tags.
    tags :: Lude.Maybe [TagDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'tags' - The tags.
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

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsNextToken :: Lens.Lens' DescribeTagsResponse (Lude.Maybe Lude.Text)
dtrsNextToken = Lens.lens (nextToken :: DescribeTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The tags.
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
