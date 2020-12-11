{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNetworkInterfacePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for your network interfaces.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkInterfacePermissions
  ( -- * Creating a request
    DescribeNetworkInterfacePermissions (..),
    mkDescribeNetworkInterfacePermissions,

    -- ** Request lenses
    dnipFilters,
    dnipNextToken,
    dnipNetworkInterfacePermissionIds,
    dnipMaxResults,

    -- * Destructuring the response
    DescribeNetworkInterfacePermissionsResponse (..),
    mkDescribeNetworkInterfacePermissionsResponse,

    -- ** Response lenses
    dnipsrsNetworkInterfacePermissions,
    dnipsrsNextToken,
    dnipsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeNetworkInterfacePermissions.
--
-- /See:/ 'mkDescribeNetworkInterfacePermissions' smart constructor.
data DescribeNetworkInterfacePermissions = DescribeNetworkInterfacePermissions'
  { filters ::
      Lude.Maybe [Filter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    networkInterfacePermissionIds ::
      Lude.Maybe
        [Lude.Text],
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

-- | Creates a value of 'DescribeNetworkInterfacePermissions' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @network-interface-permission.network-interface-permission-id@ - The ID of the permission.
--
--
--     * @network-interface-permission.network-interface-id@ - The ID of the network interface.
--
--
--     * @network-interface-permission.aws-account-id@ - The AWS account ID.
--
--
--     * @network-interface-permission.aws-service@ - The AWS service.
--
--
--     * @network-interface-permission.permission@ - The type of permission (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@ ).
--
--
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. If this parameter is not specified, up to 50 results are returned by default.
-- * 'networkInterfacePermissionIds' - One or more network interface permission IDs.
-- * 'nextToken' - The token to request the next page of results.
mkDescribeNetworkInterfacePermissions ::
  DescribeNetworkInterfacePermissions
mkDescribeNetworkInterfacePermissions =
  DescribeNetworkInterfacePermissions'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      networkInterfacePermissionIds = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @network-interface-permission.network-interface-permission-id@ - The ID of the permission.
--
--
--     * @network-interface-permission.network-interface-id@ - The ID of the network interface.
--
--
--     * @network-interface-permission.aws-account-id@ - The AWS account ID.
--
--
--     * @network-interface-permission.aws-service@ - The AWS service.
--
--
--     * @network-interface-permission.permission@ - The type of permission (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipFilters :: Lens.Lens' DescribeNetworkInterfacePermissions (Lude.Maybe [Filter])
dnipFilters = Lens.lens (filters :: DescribeNetworkInterfacePermissions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeNetworkInterfacePermissions)
{-# DEPRECATED dnipFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipNextToken :: Lens.Lens' DescribeNetworkInterfacePermissions (Lude.Maybe Lude.Text)
dnipNextToken = Lens.lens (nextToken :: DescribeNetworkInterfacePermissions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNetworkInterfacePermissions)
{-# DEPRECATED dnipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more network interface permission IDs.
--
-- /Note:/ Consider using 'networkInterfacePermissionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipNetworkInterfacePermissionIds :: Lens.Lens' DescribeNetworkInterfacePermissions (Lude.Maybe [Lude.Text])
dnipNetworkInterfacePermissionIds = Lens.lens (networkInterfacePermissionIds :: DescribeNetworkInterfacePermissions -> Lude.Maybe [Lude.Text]) (\s a -> s {networkInterfacePermissionIds = a} :: DescribeNetworkInterfacePermissions)
{-# DEPRECATED dnipNetworkInterfacePermissionIds "Use generic-lens or generic-optics with 'networkInterfacePermissionIds' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. If this parameter is not specified, up to 50 results are returned by default.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipMaxResults :: Lens.Lens' DescribeNetworkInterfacePermissions (Lude.Maybe Lude.Natural)
dnipMaxResults = Lens.lens (maxResults :: DescribeNetworkInterfacePermissions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeNetworkInterfacePermissions)
{-# DEPRECATED dnipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeNetworkInterfacePermissions where
  page rq rs
    | Page.stop (rs Lens.^. dnipsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dnipsrsNetworkInterfacePermissions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dnipNextToken Lens..~ rs Lens.^. dnipsrsNextToken

instance Lude.AWSRequest DescribeNetworkInterfacePermissions where
  type
    Rs DescribeNetworkInterfacePermissions =
      DescribeNetworkInterfacePermissionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeNetworkInterfacePermissionsResponse'
            Lude.<$> ( x Lude..@? "networkInterfacePermissions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNetworkInterfacePermissions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeNetworkInterfacePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNetworkInterfacePermissions where
  toQuery DescribeNetworkInterfacePermissions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeNetworkInterfacePermissions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "NetworkInterfacePermissionId"
              Lude.<$> networkInterfacePermissionIds
          ),
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output for DescribeNetworkInterfacePermissions.
--
-- /See:/ 'mkDescribeNetworkInterfacePermissionsResponse' smart constructor.
data DescribeNetworkInterfacePermissionsResponse = DescribeNetworkInterfacePermissionsResponse'
  { networkInterfacePermissions ::
      Lude.Maybe
        [NetworkInterfacePermission],
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

-- | Creates a value of 'DescribeNetworkInterfacePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'networkInterfacePermissions' - The network interface permissions.
-- * 'nextToken' - The token to use to retrieve the next page of results.
-- * 'responseStatus' - The response status code.
mkDescribeNetworkInterfacePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNetworkInterfacePermissionsResponse
mkDescribeNetworkInterfacePermissionsResponse pResponseStatus_ =
  DescribeNetworkInterfacePermissionsResponse'
    { networkInterfacePermissions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The network interface permissions.
--
-- /Note:/ Consider using 'networkInterfacePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipsrsNetworkInterfacePermissions :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse (Lude.Maybe [NetworkInterfacePermission])
dnipsrsNetworkInterfacePermissions = Lens.lens (networkInterfacePermissions :: DescribeNetworkInterfacePermissionsResponse -> Lude.Maybe [NetworkInterfacePermission]) (\s a -> s {networkInterfacePermissions = a} :: DescribeNetworkInterfacePermissionsResponse)
{-# DEPRECATED dnipsrsNetworkInterfacePermissions "Use generic-lens or generic-optics with 'networkInterfacePermissions' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipsrsNextToken :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse (Lude.Maybe Lude.Text)
dnipsrsNextToken = Lens.lens (nextToken :: DescribeNetworkInterfacePermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNetworkInterfacePermissionsResponse)
{-# DEPRECATED dnipsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipsrsResponseStatus :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse Lude.Int
dnipsrsResponseStatus = Lens.lens (responseStatus :: DescribeNetworkInterfacePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNetworkInterfacePermissionsResponse)
{-# DEPRECATED dnipsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
