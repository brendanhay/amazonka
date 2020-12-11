{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified local gateway virtual interfaces.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
  ( -- * Creating a request
    DescribeLocalGatewayVirtualInterfaces (..),
    mkDescribeLocalGatewayVirtualInterfaces,

    -- ** Request lenses
    dlgviFilters,
    dlgviNextToken,
    dlgviLocalGatewayVirtualInterfaceIds,
    dlgviDryRun,
    dlgviMaxResults,

    -- * Destructuring the response
    DescribeLocalGatewayVirtualInterfacesResponse (..),
    mkDescribeLocalGatewayVirtualInterfacesResponse,

    -- ** Response lenses
    dlgvirsNextToken,
    dlgvirsLocalGatewayVirtualInterfaces,
    dlgvirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLocalGatewayVirtualInterfaces' smart constructor.
data DescribeLocalGatewayVirtualInterfaces = DescribeLocalGatewayVirtualInterfaces'
  { filters ::
      Lude.Maybe
        [Filter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    localGatewayVirtualInterfaceIds ::
      Lude.Maybe
        [Lude.Text],
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    maxResults ::
      Lude.Maybe
        Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLocalGatewayVirtualInterfaces' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
-- * 'localGatewayVirtualInterfaceIds' - The IDs of the virtual interfaces.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
mkDescribeLocalGatewayVirtualInterfaces ::
  DescribeLocalGatewayVirtualInterfaces
mkDescribeLocalGatewayVirtualInterfaces =
  DescribeLocalGatewayVirtualInterfaces'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      localGatewayVirtualInterfaceIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviFilters :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Lude.Maybe [Filter])
dlgviFilters = Lens.lens (filters :: DescribeLocalGatewayVirtualInterfaces -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeLocalGatewayVirtualInterfaces)
{-# DEPRECATED dlgviFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviNextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Lude.Maybe Lude.Text)
dlgviNextToken = Lens.lens (nextToken :: DescribeLocalGatewayVirtualInterfaces -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaces)
{-# DEPRECATED dlgviNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the virtual interfaces.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviLocalGatewayVirtualInterfaceIds :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Lude.Maybe [Lude.Text])
dlgviLocalGatewayVirtualInterfaceIds = Lens.lens (localGatewayVirtualInterfaceIds :: DescribeLocalGatewayVirtualInterfaces -> Lude.Maybe [Lude.Text]) (\s a -> s {localGatewayVirtualInterfaceIds = a} :: DescribeLocalGatewayVirtualInterfaces)
{-# DEPRECATED dlgviLocalGatewayVirtualInterfaceIds "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviDryRun :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Lude.Maybe Lude.Bool)
dlgviDryRun = Lens.lens (dryRun :: DescribeLocalGatewayVirtualInterfaces -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeLocalGatewayVirtualInterfaces)
{-# DEPRECATED dlgviDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviMaxResults :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Lude.Maybe Lude.Natural)
dlgviMaxResults = Lens.lens (maxResults :: DescribeLocalGatewayVirtualInterfaces -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeLocalGatewayVirtualInterfaces)
{-# DEPRECATED dlgviMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeLocalGatewayVirtualInterfaces where
  page rq rs
    | Page.stop (rs Lens.^. dlgvirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlgvirsLocalGatewayVirtualInterfaces) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlgviNextToken Lens..~ rs Lens.^. dlgvirsNextToken

instance Lude.AWSRequest DescribeLocalGatewayVirtualInterfaces where
  type
    Rs DescribeLocalGatewayVirtualInterfaces =
      DescribeLocalGatewayVirtualInterfacesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeLocalGatewayVirtualInterfacesResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "localGatewayVirtualInterfaceSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLocalGatewayVirtualInterfaces where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLocalGatewayVirtualInterfaces where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLocalGatewayVirtualInterfaces where
  toQuery DescribeLocalGatewayVirtualInterfaces' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLocalGatewayVirtualInterfaces" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "LocalGatewayVirtualInterfaceId"
              Lude.<$> localGatewayVirtualInterfaceIds
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeLocalGatewayVirtualInterfacesResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfacesResponse = DescribeLocalGatewayVirtualInterfacesResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    localGatewayVirtualInterfaces ::
      Lude.Maybe
        [LocalGatewayVirtualInterface],
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeLocalGatewayVirtualInterfacesResponse' with the minimum fields required to make a request.
--
-- * 'localGatewayVirtualInterfaces' - Information about the virtual interfaces.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeLocalGatewayVirtualInterfacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLocalGatewayVirtualInterfacesResponse
mkDescribeLocalGatewayVirtualInterfacesResponse pResponseStatus_ =
  DescribeLocalGatewayVirtualInterfacesResponse'
    { nextToken =
        Lude.Nothing,
      localGatewayVirtualInterfaces = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvirsNextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse (Lude.Maybe Lude.Text)
dlgvirsNextToken = Lens.lens (nextToken :: DescribeLocalGatewayVirtualInterfacesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfacesResponse)
{-# DEPRECATED dlgvirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the virtual interfaces.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvirsLocalGatewayVirtualInterfaces :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse (Lude.Maybe [LocalGatewayVirtualInterface])
dlgvirsLocalGatewayVirtualInterfaces = Lens.lens (localGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfacesResponse -> Lude.Maybe [LocalGatewayVirtualInterface]) (\s a -> s {localGatewayVirtualInterfaces = a} :: DescribeLocalGatewayVirtualInterfacesResponse)
{-# DEPRECATED dlgvirsLocalGatewayVirtualInterfaces "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvirsResponseStatus :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse Lude.Int
dlgvirsResponseStatus = Lens.lens (responseStatus :: DescribeLocalGatewayVirtualInterfacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLocalGatewayVirtualInterfacesResponse)
{-# DEPRECATED dlgvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
