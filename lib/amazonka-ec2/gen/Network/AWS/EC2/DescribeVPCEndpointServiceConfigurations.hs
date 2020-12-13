{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCEndpointServiceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint service configurations in your account (your services).
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCEndpointServiceConfigurations
  ( -- * Creating a request
    DescribeVPCEndpointServiceConfigurations (..),
    mkDescribeVPCEndpointServiceConfigurations,

    -- ** Request lenses
    dvescFilters,
    dvescServiceIds,
    dvescNextToken,
    dvescDryRun,
    dvescMaxResults,

    -- * Destructuring the response
    DescribeVPCEndpointServiceConfigurationsResponse (..),
    mkDescribeVPCEndpointServiceConfigurationsResponse,

    -- ** Response lenses
    dvescrsNextToken,
    dvescrsServiceConfigurations,
    dvescrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCEndpointServiceConfigurations' smart constructor.
data DescribeVPCEndpointServiceConfigurations = DescribeVPCEndpointServiceConfigurations'
  { -- | One or more filters.
    --
    --
    --     * @service-name@ - The name of the service.
    --
    --
    --     * @service-id@ - The ID of the service.
    --
    --
    --     * @service-state@ - The state of the service (@Pending@ | @Available@ | @Deleting@ | @Deleted@ | @Failed@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filters :: Lude.Maybe [Filter],
    -- | The IDs of one or more services.
    serviceIds :: Lude.Maybe [Lude.Text],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointServiceConfigurations' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @service-id@ - The ID of the service.
--
--
--     * @service-state@ - The state of the service (@Pending@ | @Available@ | @Deleting@ | @Deleted@ | @Failed@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'serviceIds' - The IDs of one or more services.
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
mkDescribeVPCEndpointServiceConfigurations ::
  DescribeVPCEndpointServiceConfigurations
mkDescribeVPCEndpointServiceConfigurations =
  DescribeVPCEndpointServiceConfigurations'
    { filters = Lude.Nothing,
      serviceIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @service-id@ - The ID of the service.
--
--
--     * @service-state@ - The state of the service (@Pending@ | @Available@ | @Deleting@ | @Deleted@ | @Failed@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescFilters :: Lens.Lens' DescribeVPCEndpointServiceConfigurations (Lude.Maybe [Filter])
dvescFilters = Lens.lens (filters :: DescribeVPCEndpointServiceConfigurations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCEndpointServiceConfigurations)
{-# DEPRECATED dvescFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of one or more services.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescServiceIds :: Lens.Lens' DescribeVPCEndpointServiceConfigurations (Lude.Maybe [Lude.Text])
dvescServiceIds = Lens.lens (serviceIds :: DescribeVPCEndpointServiceConfigurations -> Lude.Maybe [Lude.Text]) (\s a -> s {serviceIds = a} :: DescribeVPCEndpointServiceConfigurations)
{-# DEPRECATED dvescServiceIds "Use generic-lens or generic-optics with 'serviceIds' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescNextToken :: Lens.Lens' DescribeVPCEndpointServiceConfigurations (Lude.Maybe Lude.Text)
dvescNextToken = Lens.lens (nextToken :: DescribeVPCEndpointServiceConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointServiceConfigurations)
{-# DEPRECATED dvescNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescDryRun :: Lens.Lens' DescribeVPCEndpointServiceConfigurations (Lude.Maybe Lude.Bool)
dvescDryRun = Lens.lens (dryRun :: DescribeVPCEndpointServiceConfigurations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCEndpointServiceConfigurations)
{-# DEPRECATED dvescDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescMaxResults :: Lens.Lens' DescribeVPCEndpointServiceConfigurations (Lude.Maybe Lude.Int)
dvescMaxResults = Lens.lens (maxResults :: DescribeVPCEndpointServiceConfigurations -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVPCEndpointServiceConfigurations)
{-# DEPRECATED dvescMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCEndpointServiceConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. dvescrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvescrsServiceConfigurations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvescNextToken Lens..~ rs Lens.^. dvescrsNextToken

instance Lude.AWSRequest DescribeVPCEndpointServiceConfigurations where
  type
    Rs DescribeVPCEndpointServiceConfigurations =
      DescribeVPCEndpointServiceConfigurationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCEndpointServiceConfigurationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "serviceConfigurationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCEndpointServiceConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCEndpointServiceConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCEndpointServiceConfigurations where
  toQuery DescribeVPCEndpointServiceConfigurations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeVpcEndpointServiceConfigurations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "ServiceId" Lude.<$> serviceIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVPCEndpointServiceConfigurationsResponse' smart constructor.
data DescribeVPCEndpointServiceConfigurationsResponse = DescribeVPCEndpointServiceConfigurationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about one or more services.
    serviceConfigurations :: Lude.Maybe [ServiceConfiguration],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointServiceConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'serviceConfigurations' - Information about one or more services.
-- * 'responseStatus' - The response status code.
mkDescribeVPCEndpointServiceConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCEndpointServiceConfigurationsResponse
mkDescribeVPCEndpointServiceConfigurationsResponse pResponseStatus_ =
  DescribeVPCEndpointServiceConfigurationsResponse'
    { nextToken =
        Lude.Nothing,
      serviceConfigurations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescrsNextToken :: Lens.Lens' DescribeVPCEndpointServiceConfigurationsResponse (Lude.Maybe Lude.Text)
dvescrsNextToken = Lens.lens (nextToken :: DescribeVPCEndpointServiceConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointServiceConfigurationsResponse)
{-# DEPRECATED dvescrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about one or more services.
--
-- /Note:/ Consider using 'serviceConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescrsServiceConfigurations :: Lens.Lens' DescribeVPCEndpointServiceConfigurationsResponse (Lude.Maybe [ServiceConfiguration])
dvescrsServiceConfigurations = Lens.lens (serviceConfigurations :: DescribeVPCEndpointServiceConfigurationsResponse -> Lude.Maybe [ServiceConfiguration]) (\s a -> s {serviceConfigurations = a} :: DescribeVPCEndpointServiceConfigurationsResponse)
{-# DEPRECATED dvescrsServiceConfigurations "Use generic-lens or generic-optics with 'serviceConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescrsResponseStatus :: Lens.Lens' DescribeVPCEndpointServiceConfigurationsResponse Lude.Int
dvescrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCEndpointServiceConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCEndpointServiceConfigurationsResponse)
{-# DEPRECATED dvescrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
