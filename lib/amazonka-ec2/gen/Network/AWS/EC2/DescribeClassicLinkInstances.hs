{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClassicLinkInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your linked EC2-Classic instances. This request only returns information about EC2-Classic instances linked to a VPC through ClassicLink. You cannot use this request to return information about other instances.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClassicLinkInstances
  ( -- * Creating a request
    DescribeClassicLinkInstances (..),
    mkDescribeClassicLinkInstances,

    -- ** Request lenses
    dcliFilters,
    dcliNextToken,
    dcliInstanceIds,
    dcliDryRun,
    dcliMaxResults,

    -- * Destructuring the response
    DescribeClassicLinkInstancesResponse (..),
    mkDescribeClassicLinkInstancesResponse,

    -- ** Response lenses
    dclirsNextToken,
    dclirsInstances,
    dclirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClassicLinkInstances' smart constructor.
data DescribeClassicLinkInstances = DescribeClassicLinkInstances'
  { -- | One or more filters.
    --
    --
    --     * @group-id@ - The ID of a VPC security group that's associated with the instance.
    --
    --
    --     * @instance-id@ - The ID of the instance.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC to which the instance is linked.
    -- @vpc-id@ - The ID of the VPC that the instance is linked to.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more instance IDs. Must be instances linked to a VPC through ClassicLink.
    instanceIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    --
    -- Constraint: If the value is greater than 1000, we return only 1000 items.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClassicLinkInstances' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @group-id@ - The ID of a VPC security group that's associated with the instance.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC to which the instance is linked.
-- @vpc-id@ - The ID of the VPC that the instance is linked to.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'instanceIds' - One or more instance IDs. Must be instances linked to a VPC through ClassicLink.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- Constraint: If the value is greater than 1000, we return only 1000 items.
mkDescribeClassicLinkInstances ::
  DescribeClassicLinkInstances
mkDescribeClassicLinkInstances =
  DescribeClassicLinkInstances'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      instanceIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @group-id@ - The ID of a VPC security group that's associated with the instance.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC to which the instance is linked.
-- @vpc-id@ - The ID of the VPC that the instance is linked to.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliFilters :: Lens.Lens' DescribeClassicLinkInstances (Lude.Maybe [Filter])
dcliFilters = Lens.lens (filters :: DescribeClassicLinkInstances -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeClassicLinkInstances)
{-# DEPRECATED dcliFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliNextToken :: Lens.Lens' DescribeClassicLinkInstances (Lude.Maybe Lude.Text)
dcliNextToken = Lens.lens (nextToken :: DescribeClassicLinkInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClassicLinkInstances)
{-# DEPRECATED dcliNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more instance IDs. Must be instances linked to a VPC through ClassicLink.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliInstanceIds :: Lens.Lens' DescribeClassicLinkInstances (Lude.Maybe [Lude.Text])
dcliInstanceIds = Lens.lens (instanceIds :: DescribeClassicLinkInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: DescribeClassicLinkInstances)
{-# DEPRECATED dcliInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliDryRun :: Lens.Lens' DescribeClassicLinkInstances (Lude.Maybe Lude.Bool)
dcliDryRun = Lens.lens (dryRun :: DescribeClassicLinkInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeClassicLinkInstances)
{-# DEPRECATED dcliDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- Constraint: If the value is greater than 1000, we return only 1000 items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliMaxResults :: Lens.Lens' DescribeClassicLinkInstances (Lude.Maybe Lude.Natural)
dcliMaxResults = Lens.lens (maxResults :: DescribeClassicLinkInstances -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeClassicLinkInstances)
{-# DEPRECATED dcliMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeClassicLinkInstances where
  page rq rs
    | Page.stop (rs Lens.^. dclirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dclirsInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcliNextToken Lens..~ rs Lens.^. dclirsNextToken

instance Lude.AWSRequest DescribeClassicLinkInstances where
  type
    Rs DescribeClassicLinkInstances =
      DescribeClassicLinkInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeClassicLinkInstancesResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "instancesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClassicLinkInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClassicLinkInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClassicLinkInstances where
  toQuery DescribeClassicLinkInstances' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClassicLinkInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "InstanceId" Lude.<$> instanceIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeClassicLinkInstancesResponse' smart constructor.
data DescribeClassicLinkInstancesResponse = DescribeClassicLinkInstancesResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about one or more linked EC2-Classic instances.
    instances :: Lude.Maybe [ClassicLinkInstance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClassicLinkInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'instances' - Information about one or more linked EC2-Classic instances.
-- * 'responseStatus' - The response status code.
mkDescribeClassicLinkInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClassicLinkInstancesResponse
mkDescribeClassicLinkInstancesResponse pResponseStatus_ =
  DescribeClassicLinkInstancesResponse'
    { nextToken = Lude.Nothing,
      instances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclirsNextToken :: Lens.Lens' DescribeClassicLinkInstancesResponse (Lude.Maybe Lude.Text)
dclirsNextToken = Lens.lens (nextToken :: DescribeClassicLinkInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClassicLinkInstancesResponse)
{-# DEPRECATED dclirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about one or more linked EC2-Classic instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclirsInstances :: Lens.Lens' DescribeClassicLinkInstancesResponse (Lude.Maybe [ClassicLinkInstance])
dclirsInstances = Lens.lens (instances :: DescribeClassicLinkInstancesResponse -> Lude.Maybe [ClassicLinkInstance]) (\s a -> s {instances = a} :: DescribeClassicLinkInstancesResponse)
{-# DEPRECATED dclirsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclirsResponseStatus :: Lens.Lens' DescribeClassicLinkInstancesResponse Lude.Int
dclirsResponseStatus = Lens.lens (responseStatus :: DescribeClassicLinkInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClassicLinkInstancesResponse)
{-# DEPRECATED dclirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
