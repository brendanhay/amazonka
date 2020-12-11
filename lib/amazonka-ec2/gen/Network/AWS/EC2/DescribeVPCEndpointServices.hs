{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCEndpointServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes available services to which you can create a VPC endpoint.
--
-- When the service provider and the consumer have different accounts multiple Availability Zones, and the consumer views the VPC endpoint service information, the response only includes the common Availability Zones. For example, when the service provider account uses @us-east-1a@ and @us-east-1c@ and the consumer uses @us-east-1a@ and us-east-1a and us-east-1b, the response includes the VPC endpoint services in the common Availability Zone, @us-east-1a@ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCEndpointServices
  ( -- * Creating a request
    DescribeVPCEndpointServices (..),
    mkDescribeVPCEndpointServices,

    -- ** Request lenses
    dvesFilters,
    dvesServiceNames,
    dvesNextToken,
    dvesDryRun,
    dvesMaxResults,

    -- * Destructuring the response
    DescribeVPCEndpointServicesResponse (..),
    mkDescribeVPCEndpointServicesResponse,

    -- ** Response lenses
    dvesrsServiceDetails,
    dvesrsServiceNames,
    dvesrsNextToken,
    dvesrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeVpcEndpointServices.
--
-- /See:/ 'mkDescribeVPCEndpointServices' smart constructor.
data DescribeVPCEndpointServices = DescribeVPCEndpointServices'
  { filters ::
      Lude.Maybe [Filter],
    serviceNames ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointServices' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'maxResults' - The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000 items.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a prior call.)
-- * 'serviceNames' - One or more service names.
mkDescribeVPCEndpointServices ::
  DescribeVPCEndpointServices
mkDescribeVPCEndpointServices =
  DescribeVPCEndpointServices'
    { filters = Lude.Nothing,
      serviceNames = Lude.Nothing,
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
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesFilters :: Lens.Lens' DescribeVPCEndpointServices (Lude.Maybe [Filter])
dvesFilters = Lens.lens (filters :: DescribeVPCEndpointServices -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCEndpointServices)
{-# DEPRECATED dvesFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more service names.
--
-- /Note:/ Consider using 'serviceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesServiceNames :: Lens.Lens' DescribeVPCEndpointServices (Lude.Maybe [Lude.Text])
dvesServiceNames = Lens.lens (serviceNames :: DescribeVPCEndpointServices -> Lude.Maybe [Lude.Text]) (\s a -> s {serviceNames = a} :: DescribeVPCEndpointServices)
{-# DEPRECATED dvesServiceNames "Use generic-lens or generic-optics with 'serviceNames' instead." #-}

-- | The token for the next set of items to return. (You received this token from a prior call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesNextToken :: Lens.Lens' DescribeVPCEndpointServices (Lude.Maybe Lude.Text)
dvesNextToken = Lens.lens (nextToken :: DescribeVPCEndpointServices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointServices)
{-# DEPRECATED dvesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesDryRun :: Lens.Lens' DescribeVPCEndpointServices (Lude.Maybe Lude.Bool)
dvesDryRun = Lens.lens (dryRun :: DescribeVPCEndpointServices -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCEndpointServices)
{-# DEPRECATED dvesDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000 items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesMaxResults :: Lens.Lens' DescribeVPCEndpointServices (Lude.Maybe Lude.Int)
dvesMaxResults = Lens.lens (maxResults :: DescribeVPCEndpointServices -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVPCEndpointServices)
{-# DEPRECATED dvesMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCEndpointServices where
  page rq rs
    | Page.stop (rs Lens.^. dvesrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvesrsServiceDetails) = Lude.Nothing
    | Page.stop (rs Lens.^. dvesrsServiceNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvesNextToken Lens..~ rs Lens.^. dvesrsNextToken

instance Lude.AWSRequest DescribeVPCEndpointServices where
  type
    Rs DescribeVPCEndpointServices =
      DescribeVPCEndpointServicesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCEndpointServicesResponse'
            Lude.<$> ( x Lude..@? "serviceDetailSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "serviceNameSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCEndpointServices where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCEndpointServices where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCEndpointServices where
  toQuery DescribeVPCEndpointServices' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeVpcEndpointServices" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "ServiceName" Lude.<$> serviceNames),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeVpcEndpointServices.
--
-- /See:/ 'mkDescribeVPCEndpointServicesResponse' smart constructor.
data DescribeVPCEndpointServicesResponse = DescribeVPCEndpointServicesResponse'
  { serviceDetails ::
      Lude.Maybe
        [ServiceDetail],
    serviceNames ::
      Lude.Maybe
        [Lude.Text],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointServicesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
-- * 'serviceDetails' - Information about the service.
-- * 'serviceNames' - A list of supported services.
mkDescribeVPCEndpointServicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCEndpointServicesResponse
mkDescribeVPCEndpointServicesResponse pResponseStatus_ =
  DescribeVPCEndpointServicesResponse'
    { serviceDetails =
        Lude.Nothing,
      serviceNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the service.
--
-- /Note:/ Consider using 'serviceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesrsServiceDetails :: Lens.Lens' DescribeVPCEndpointServicesResponse (Lude.Maybe [ServiceDetail])
dvesrsServiceDetails = Lens.lens (serviceDetails :: DescribeVPCEndpointServicesResponse -> Lude.Maybe [ServiceDetail]) (\s a -> s {serviceDetails = a} :: DescribeVPCEndpointServicesResponse)
{-# DEPRECATED dvesrsServiceDetails "Use generic-lens or generic-optics with 'serviceDetails' instead." #-}

-- | A list of supported services.
--
-- /Note:/ Consider using 'serviceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesrsServiceNames :: Lens.Lens' DescribeVPCEndpointServicesResponse (Lude.Maybe [Lude.Text])
dvesrsServiceNames = Lens.lens (serviceNames :: DescribeVPCEndpointServicesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {serviceNames = a} :: DescribeVPCEndpointServicesResponse)
{-# DEPRECATED dvesrsServiceNames "Use generic-lens or generic-optics with 'serviceNames' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesrsNextToken :: Lens.Lens' DescribeVPCEndpointServicesResponse (Lude.Maybe Lude.Text)
dvesrsNextToken = Lens.lens (nextToken :: DescribeVPCEndpointServicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointServicesResponse)
{-# DEPRECATED dvesrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesrsResponseStatus :: Lens.Lens' DescribeVPCEndpointServicesResponse Lude.Int
dvesrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCEndpointServicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCEndpointServicesResponse)
{-# DEPRECATED dvesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
