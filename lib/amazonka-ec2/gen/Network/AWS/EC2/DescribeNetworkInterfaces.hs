{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNetworkInterfaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network interfaces.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkInterfaces
  ( -- * Creating a request
    DescribeNetworkInterfaces (..),
    mkDescribeNetworkInterfaces,

    -- ** Request lenses
    dniNetworkInterfaceIds,
    dniFilters,
    dniNextToken,
    dniDryRun,
    dniMaxResults,

    -- * Destructuring the response
    DescribeNetworkInterfacesResponse (..),
    mkDescribeNetworkInterfacesResponse,

    -- ** Response lenses
    dnirsNetworkInterfaces,
    dnirsNextToken,
    dnirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeNetworkInterfaces.
--
-- /See:/ 'mkDescribeNetworkInterfaces' smart constructor.
data DescribeNetworkInterfaces = DescribeNetworkInterfaces'
  { -- | One or more network interface IDs.
    --
    -- Default: Describes all your network interfaces.
    networkInterfaceIds :: Lude.Maybe [Lude.Text],
    -- | One or more filters.
    --
    --
    --     * @addresses.private-ip-address@ - The private IPv4 addresses associated with the network interface.
    --
    --
    --     * @addresses.primary@ - Whether the private IPv4 address is the primary IP address associated with the network interface.
    --
    --
    --     * @addresses.association.public-ip@ - The association ID returned when the network interface was associated with the Elastic IP address (IPv4).
    --
    --
    --     * @addresses.association.owner-id@ - The owner ID of the addresses associated with the network interface.
    --
    --
    --     * @association.association-id@ - The association ID returned when the network interface was associated with an IPv4 address.
    --
    --
    --     * @association.allocation-id@ - The allocation ID returned when you allocated the Elastic IP address (IPv4) for your network interface.
    --
    --
    --     * @association.ip-owner-id@ - The owner of the Elastic IP address (IPv4) associated with the network interface.
    --
    --
    --     * @association.public-ip@ - The address of the Elastic IP address (IPv4) bound to the network interface.
    --
    --
    --     * @association.public-dns-name@ - The public DNS name for the network interface (IPv4).
    --
    --
    --     * @attachment.attachment-id@ - The ID of the interface attachment.
    --
    --
    --     * @attachment.attach-time@ - The time that the network interface was attached to an instance.
    --
    --
    --     * @attachment.delete-on-termination@ - Indicates whether the attachment is deleted when an instance is terminated.
    --
    --
    --     * @attachment.device-index@ - The device index to which the network interface is attached.
    --
    --
    --     * @attachment.instance-id@ - The ID of the instance to which the network interface is attached.
    --
    --
    --     * @attachment.instance-owner-id@ - The owner ID of the instance to which the network interface is attached.
    --
    --
    --     * @attachment.status@ - The status of the attachment (@attaching@ | @attached@ | @detaching@ | @detached@ ).
    --
    --
    --     * @availability-zone@ - The Availability Zone of the network interface.
    --
    --
    --     * @description@ - The description of the network interface.
    --
    --
    --     * @group-id@ - The ID of a security group associated with the network interface.
    --
    --
    --     * @group-name@ - The name of a security group associated with the network interface.
    --
    --
    --     * @ipv6-addresses.ipv6-address@ - An IPv6 address associated with the network interface.
    --
    --
    --     * @mac-address@ - The MAC address of the network interface.
    --
    --
    --     * @network-interface-id@ - The ID of the network interface.
    --
    --
    --     * @owner-id@ - The AWS account ID of the network interface owner.
    --
    --
    --     * @private-ip-address@ - The private IPv4 address or addresses of the network interface.
    --
    --
    --     * @private-dns-name@ - The private DNS name of the network interface (IPv4).
    --
    --
    --     * @requester-id@ - The ID of the entity that launched the instance on your behalf (for example, AWS Management Console, Auto Scaling, and so on).
    --
    --
    --     * @requester-managed@ - Indicates whether the network interface is being managed by an AWS service (for example, AWS Management Console, Auto Scaling, and so on).
    --
    --
    --     * @source-dest-check@ - Indicates whether the network interface performs source/destination checking. A value of @true@ means checking is enabled, and @false@ means checking is disabled. The value must be @false@ for the network interface to perform network address translation (NAT) in your VPC.
    --
    --
    --     * @status@ - The status of the network interface. If the network interface is not attached to an instance, the status is @available@ ; if a network interface is attached to an instance the status is @in-use@ .
    --
    --
    --     * @subnet-id@ - The ID of the subnet for the network interface.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC for the network interface.
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. You cannot specify this parameter and the network interface IDs parameter in the same request.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNetworkInterfaces' with the minimum fields required to make a request.
--
-- * 'networkInterfaceIds' - One or more network interface IDs.
--
-- Default: Describes all your network interfaces.
-- * 'filters' - One or more filters.
--
--
--     * @addresses.private-ip-address@ - The private IPv4 addresses associated with the network interface.
--
--
--     * @addresses.primary@ - Whether the private IPv4 address is the primary IP address associated with the network interface.
--
--
--     * @addresses.association.public-ip@ - The association ID returned when the network interface was associated with the Elastic IP address (IPv4).
--
--
--     * @addresses.association.owner-id@ - The owner ID of the addresses associated with the network interface.
--
--
--     * @association.association-id@ - The association ID returned when the network interface was associated with an IPv4 address.
--
--
--     * @association.allocation-id@ - The allocation ID returned when you allocated the Elastic IP address (IPv4) for your network interface.
--
--
--     * @association.ip-owner-id@ - The owner of the Elastic IP address (IPv4) associated with the network interface.
--
--
--     * @association.public-ip@ - The address of the Elastic IP address (IPv4) bound to the network interface.
--
--
--     * @association.public-dns-name@ - The public DNS name for the network interface (IPv4).
--
--
--     * @attachment.attachment-id@ - The ID of the interface attachment.
--
--
--     * @attachment.attach-time@ - The time that the network interface was attached to an instance.
--
--
--     * @attachment.delete-on-termination@ - Indicates whether the attachment is deleted when an instance is terminated.
--
--
--     * @attachment.device-index@ - The device index to which the network interface is attached.
--
--
--     * @attachment.instance-id@ - The ID of the instance to which the network interface is attached.
--
--
--     * @attachment.instance-owner-id@ - The owner ID of the instance to which the network interface is attached.
--
--
--     * @attachment.status@ - The status of the attachment (@attaching@ | @attached@ | @detaching@ | @detached@ ).
--
--
--     * @availability-zone@ - The Availability Zone of the network interface.
--
--
--     * @description@ - The description of the network interface.
--
--
--     * @group-id@ - The ID of a security group associated with the network interface.
--
--
--     * @group-name@ - The name of a security group associated with the network interface.
--
--
--     * @ipv6-addresses.ipv6-address@ - An IPv6 address associated with the network interface.
--
--
--     * @mac-address@ - The MAC address of the network interface.
--
--
--     * @network-interface-id@ - The ID of the network interface.
--
--
--     * @owner-id@ - The AWS account ID of the network interface owner.
--
--
--     * @private-ip-address@ - The private IPv4 address or addresses of the network interface.
--
--
--     * @private-dns-name@ - The private DNS name of the network interface (IPv4).
--
--
--     * @requester-id@ - The ID of the entity that launched the instance on your behalf (for example, AWS Management Console, Auto Scaling, and so on).
--
--
--     * @requester-managed@ - Indicates whether the network interface is being managed by an AWS service (for example, AWS Management Console, Auto Scaling, and so on).
--
--
--     * @source-dest-check@ - Indicates whether the network interface performs source/destination checking. A value of @true@ means checking is enabled, and @false@ means checking is disabled. The value must be @false@ for the network interface to perform network address translation (NAT) in your VPC.
--
--
--     * @status@ - The status of the network interface. If the network interface is not attached to an instance, the status is @available@ ; if a network interface is attached to an instance the status is @in-use@ .
--
--
--     * @subnet-id@ - The ID of the subnet for the network interface.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC for the network interface.
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. You cannot specify this parameter and the network interface IDs parameter in the same request.
mkDescribeNetworkInterfaces ::
  DescribeNetworkInterfaces
mkDescribeNetworkInterfaces =
  DescribeNetworkInterfaces'
    { networkInterfaceIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more network interface IDs.
--
-- Default: Describes all your network interfaces.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniNetworkInterfaceIds :: Lens.Lens' DescribeNetworkInterfaces (Lude.Maybe [Lude.Text])
dniNetworkInterfaceIds = Lens.lens (networkInterfaceIds :: DescribeNetworkInterfaces -> Lude.Maybe [Lude.Text]) (\s a -> s {networkInterfaceIds = a} :: DescribeNetworkInterfaces)
{-# DEPRECATED dniNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | One or more filters.
--
--
--     * @addresses.private-ip-address@ - The private IPv4 addresses associated with the network interface.
--
--
--     * @addresses.primary@ - Whether the private IPv4 address is the primary IP address associated with the network interface.
--
--
--     * @addresses.association.public-ip@ - The association ID returned when the network interface was associated with the Elastic IP address (IPv4).
--
--
--     * @addresses.association.owner-id@ - The owner ID of the addresses associated with the network interface.
--
--
--     * @association.association-id@ - The association ID returned when the network interface was associated with an IPv4 address.
--
--
--     * @association.allocation-id@ - The allocation ID returned when you allocated the Elastic IP address (IPv4) for your network interface.
--
--
--     * @association.ip-owner-id@ - The owner of the Elastic IP address (IPv4) associated with the network interface.
--
--
--     * @association.public-ip@ - The address of the Elastic IP address (IPv4) bound to the network interface.
--
--
--     * @association.public-dns-name@ - The public DNS name for the network interface (IPv4).
--
--
--     * @attachment.attachment-id@ - The ID of the interface attachment.
--
--
--     * @attachment.attach-time@ - The time that the network interface was attached to an instance.
--
--
--     * @attachment.delete-on-termination@ - Indicates whether the attachment is deleted when an instance is terminated.
--
--
--     * @attachment.device-index@ - The device index to which the network interface is attached.
--
--
--     * @attachment.instance-id@ - The ID of the instance to which the network interface is attached.
--
--
--     * @attachment.instance-owner-id@ - The owner ID of the instance to which the network interface is attached.
--
--
--     * @attachment.status@ - The status of the attachment (@attaching@ | @attached@ | @detaching@ | @detached@ ).
--
--
--     * @availability-zone@ - The Availability Zone of the network interface.
--
--
--     * @description@ - The description of the network interface.
--
--
--     * @group-id@ - The ID of a security group associated with the network interface.
--
--
--     * @group-name@ - The name of a security group associated with the network interface.
--
--
--     * @ipv6-addresses.ipv6-address@ - An IPv6 address associated with the network interface.
--
--
--     * @mac-address@ - The MAC address of the network interface.
--
--
--     * @network-interface-id@ - The ID of the network interface.
--
--
--     * @owner-id@ - The AWS account ID of the network interface owner.
--
--
--     * @private-ip-address@ - The private IPv4 address or addresses of the network interface.
--
--
--     * @private-dns-name@ - The private DNS name of the network interface (IPv4).
--
--
--     * @requester-id@ - The ID of the entity that launched the instance on your behalf (for example, AWS Management Console, Auto Scaling, and so on).
--
--
--     * @requester-managed@ - Indicates whether the network interface is being managed by an AWS service (for example, AWS Management Console, Auto Scaling, and so on).
--
--
--     * @source-dest-check@ - Indicates whether the network interface performs source/destination checking. A value of @true@ means checking is enabled, and @false@ means checking is disabled. The value must be @false@ for the network interface to perform network address translation (NAT) in your VPC.
--
--
--     * @status@ - The status of the network interface. If the network interface is not attached to an instance, the status is @available@ ; if a network interface is attached to an instance the status is @in-use@ .
--
--
--     * @subnet-id@ - The ID of the subnet for the network interface.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC for the network interface.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniFilters :: Lens.Lens' DescribeNetworkInterfaces (Lude.Maybe [Filter])
dniFilters = Lens.lens (filters :: DescribeNetworkInterfaces -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeNetworkInterfaces)
{-# DEPRECATED dniFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniNextToken :: Lens.Lens' DescribeNetworkInterfaces (Lude.Maybe Lude.Text)
dniNextToken = Lens.lens (nextToken :: DescribeNetworkInterfaces -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNetworkInterfaces)
{-# DEPRECATED dniNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniDryRun :: Lens.Lens' DescribeNetworkInterfaces (Lude.Maybe Lude.Bool)
dniDryRun = Lens.lens (dryRun :: DescribeNetworkInterfaces -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeNetworkInterfaces)
{-# DEPRECATED dniDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. You cannot specify this parameter and the network interface IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniMaxResults :: Lens.Lens' DescribeNetworkInterfaces (Lude.Maybe Lude.Natural)
dniMaxResults = Lens.lens (maxResults :: DescribeNetworkInterfaces -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeNetworkInterfaces)
{-# DEPRECATED dniMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeNetworkInterfaces where
  page rq rs
    | Page.stop (rs Lens.^. dnirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dnirsNetworkInterfaces) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dniNextToken Lens..~ rs Lens.^. dnirsNextToken

instance Lude.AWSRequest DescribeNetworkInterfaces where
  type
    Rs DescribeNetworkInterfaces =
      DescribeNetworkInterfacesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeNetworkInterfacesResponse'
            Lude.<$> ( x Lude..@? "networkInterfaceSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNetworkInterfaces where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeNetworkInterfaces where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNetworkInterfaces where
  toQuery DescribeNetworkInterfaces' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeNetworkInterfaces" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "NetworkInterfaceId"
              Lude.<$> networkInterfaceIds
          ),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeNetworkInterfaces.
--
-- /See:/ 'mkDescribeNetworkInterfacesResponse' smart constructor.
data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse'
  { -- | Information about one or more network interfaces.
    networkInterfaces :: Lude.Maybe [NetworkInterface],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNetworkInterfacesResponse' with the minimum fields required to make a request.
--
-- * 'networkInterfaces' - Information about one or more network interfaces.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeNetworkInterfacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNetworkInterfacesResponse
mkDescribeNetworkInterfacesResponse pResponseStatus_ =
  DescribeNetworkInterfacesResponse'
    { networkInterfaces =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more network interfaces.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsNetworkInterfaces :: Lens.Lens' DescribeNetworkInterfacesResponse (Lude.Maybe [NetworkInterface])
dnirsNetworkInterfaces = Lens.lens (networkInterfaces :: DescribeNetworkInterfacesResponse -> Lude.Maybe [NetworkInterface]) (\s a -> s {networkInterfaces = a} :: DescribeNetworkInterfacesResponse)
{-# DEPRECATED dnirsNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsNextToken :: Lens.Lens' DescribeNetworkInterfacesResponse (Lude.Maybe Lude.Text)
dnirsNextToken = Lens.lens (nextToken :: DescribeNetworkInterfacesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNetworkInterfacesResponse)
{-# DEPRECATED dnirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsResponseStatus :: Lens.Lens' DescribeNetworkInterfacesResponse Lude.Int
dnirsResponseStatus = Lens.lens (responseStatus :: DescribeNetworkInterfacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNetworkInterfacesResponse)
{-# DEPRECATED dnirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
