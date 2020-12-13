{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCEndpointServicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the principals (service consumers) that are permitted to discover your VPC endpoint service.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCEndpointServicePermissions
  ( -- * Creating a request
    DescribeVPCEndpointServicePermissions (..),
    mkDescribeVPCEndpointServicePermissions,

    -- ** Request lenses
    dvespFilters,
    dvespNextToken,
    dvespServiceId,
    dvespDryRun,
    dvespMaxResults,

    -- * Destructuring the response
    DescribeVPCEndpointServicePermissionsResponse (..),
    mkDescribeVPCEndpointServicePermissionsResponse,

    -- ** Response lenses
    dvesprsNextToken,
    dvesprsAllowedPrincipals,
    dvesprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCEndpointServicePermissions' smart constructor.
data DescribeVPCEndpointServicePermissions = DescribeVPCEndpointServicePermissions'
  { -- | One or more filters.
    --
    --
    --     * @principal@ - The ARN of the principal.
    --
    --
    --     * @principal-type@ - The principal type (@All@ | @Service@ | @OrganizationUnit@ | @Account@ | @User@ | @Role@ ).
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the service.
    serviceId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointServicePermissions' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @principal@ - The ARN of the principal.
--
--
--     * @principal-type@ - The principal type (@All@ | @Service@ | @OrganizationUnit@ | @Account@ | @User@ | @Role@ ).
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'serviceId' - The ID of the service.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
mkDescribeVPCEndpointServicePermissions ::
  -- | 'serviceId'
  Lude.Text ->
  DescribeVPCEndpointServicePermissions
mkDescribeVPCEndpointServicePermissions pServiceId_ =
  DescribeVPCEndpointServicePermissions'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      serviceId = pServiceId_,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @principal@ - The ARN of the principal.
--
--
--     * @principal-type@ - The principal type (@All@ | @Service@ | @OrganizationUnit@ | @Account@ | @User@ | @Role@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespFilters :: Lens.Lens' DescribeVPCEndpointServicePermissions (Lude.Maybe [Filter])
dvespFilters = Lens.lens (filters :: DescribeVPCEndpointServicePermissions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCEndpointServicePermissions)
{-# DEPRECATED dvespFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespNextToken :: Lens.Lens' DescribeVPCEndpointServicePermissions (Lude.Maybe Lude.Text)
dvespNextToken = Lens.lens (nextToken :: DescribeVPCEndpointServicePermissions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointServicePermissions)
{-# DEPRECATED dvespNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespServiceId :: Lens.Lens' DescribeVPCEndpointServicePermissions Lude.Text
dvespServiceId = Lens.lens (serviceId :: DescribeVPCEndpointServicePermissions -> Lude.Text) (\s a -> s {serviceId = a} :: DescribeVPCEndpointServicePermissions)
{-# DEPRECATED dvespServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespDryRun :: Lens.Lens' DescribeVPCEndpointServicePermissions (Lude.Maybe Lude.Bool)
dvespDryRun = Lens.lens (dryRun :: DescribeVPCEndpointServicePermissions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCEndpointServicePermissions)
{-# DEPRECATED dvespDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespMaxResults :: Lens.Lens' DescribeVPCEndpointServicePermissions (Lude.Maybe Lude.Int)
dvespMaxResults = Lens.lens (maxResults :: DescribeVPCEndpointServicePermissions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVPCEndpointServicePermissions)
{-# DEPRECATED dvespMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCEndpointServicePermissions where
  page rq rs
    | Page.stop (rs Lens.^. dvesprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvesprsAllowedPrincipals) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvespNextToken Lens..~ rs Lens.^. dvesprsNextToken

instance Lude.AWSRequest DescribeVPCEndpointServicePermissions where
  type
    Rs DescribeVPCEndpointServicePermissions =
      DescribeVPCEndpointServicePermissionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCEndpointServicePermissionsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "allowedPrincipals" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCEndpointServicePermissions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCEndpointServicePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCEndpointServicePermissions where
  toQuery DescribeVPCEndpointServicePermissions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeVpcEndpointServicePermissions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "ServiceId" Lude.=: serviceId,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVPCEndpointServicePermissionsResponse' smart constructor.
data DescribeVPCEndpointServicePermissionsResponse = DescribeVPCEndpointServicePermissionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about one or more allowed principals.
    allowedPrincipals :: Lude.Maybe [AllowedPrincipal],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointServicePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'allowedPrincipals' - Information about one or more allowed principals.
-- * 'responseStatus' - The response status code.
mkDescribeVPCEndpointServicePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCEndpointServicePermissionsResponse
mkDescribeVPCEndpointServicePermissionsResponse pResponseStatus_ =
  DescribeVPCEndpointServicePermissionsResponse'
    { nextToken =
        Lude.Nothing,
      allowedPrincipals = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesprsNextToken :: Lens.Lens' DescribeVPCEndpointServicePermissionsResponse (Lude.Maybe Lude.Text)
dvesprsNextToken = Lens.lens (nextToken :: DescribeVPCEndpointServicePermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointServicePermissionsResponse)
{-# DEPRECATED dvesprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about one or more allowed principals.
--
-- /Note:/ Consider using 'allowedPrincipals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesprsAllowedPrincipals :: Lens.Lens' DescribeVPCEndpointServicePermissionsResponse (Lude.Maybe [AllowedPrincipal])
dvesprsAllowedPrincipals = Lens.lens (allowedPrincipals :: DescribeVPCEndpointServicePermissionsResponse -> Lude.Maybe [AllowedPrincipal]) (\s a -> s {allowedPrincipals = a} :: DescribeVPCEndpointServicePermissionsResponse)
{-# DEPRECATED dvesprsAllowedPrincipals "Use generic-lens or generic-optics with 'allowedPrincipals' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesprsResponseStatus :: Lens.Lens' DescribeVPCEndpointServicePermissionsResponse Lude.Int
dvesprsResponseStatus = Lens.lens (responseStatus :: DescribeVPCEndpointServicePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCEndpointServicePermissionsResponse)
{-# DEPRECATED dvesprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
