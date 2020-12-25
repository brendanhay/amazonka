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
    dnisDryRun,
    dnisFilters,
    dnisMaxResults,
    dnisNetworkInterfaceIds,
    dnisNextToken,

    -- * Destructuring the response
    DescribeNetworkInterfacesResponse (..),
    mkDescribeNetworkInterfacesResponse,

    -- ** Response lenses
    dnirrsNetworkInterfaces,
    dnirrsNextToken,
    dnirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeNetworkInterfaces.
--
-- /See:/ 'mkDescribeNetworkInterfaces' smart constructor.
data DescribeNetworkInterfaces = DescribeNetworkInterfaces'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. You cannot specify this parameter and the network interface IDs parameter in the same request.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more network interface IDs.
    --
    -- Default: Describes all your network interfaces.
    networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId],
    -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNetworkInterfaces' value with any optional fields omitted.
mkDescribeNetworkInterfaces ::
  DescribeNetworkInterfaces
mkDescribeNetworkInterfaces =
  DescribeNetworkInterfaces'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      networkInterfaceIds = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnisDryRun :: Lens.Lens' DescribeNetworkInterfaces (Core.Maybe Core.Bool)
dnisDryRun = Lens.field @"dryRun"
{-# DEPRECATED dnisDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
dnisFilters :: Lens.Lens' DescribeNetworkInterfaces (Core.Maybe [Types.Filter])
dnisFilters = Lens.field @"filters"
{-# DEPRECATED dnisFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. You cannot specify this parameter and the network interface IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnisMaxResults :: Lens.Lens' DescribeNetworkInterfaces (Core.Maybe Core.Natural)
dnisMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dnisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | One or more network interface IDs.
--
-- Default: Describes all your network interfaces.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnisNetworkInterfaceIds :: Lens.Lens' DescribeNetworkInterfaces (Core.Maybe [Types.NetworkInterfaceId])
dnisNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# DEPRECATED dnisNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnisNextToken :: Lens.Lens' DescribeNetworkInterfaces (Core.Maybe Types.NextToken)
dnisNextToken = Lens.field @"nextToken"
{-# DEPRECATED dnisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeNetworkInterfaces where
  type
    Rs DescribeNetworkInterfaces =
      DescribeNetworkInterfacesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeNetworkInterfaces")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> ( Core.toQueryList "NetworkInterfaceId"
                            Core.<$> networkInterfaceIds
                        )
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInterfacesResponse'
            Core.<$> ( x Core..@? "networkInterfaceSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeNetworkInterfaces where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"networkInterfaces" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Contains the output of DescribeNetworkInterfaces.
--
-- /See:/ 'mkDescribeNetworkInterfacesResponse' smart constructor.
data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse'
  { -- | Information about one or more network interfaces.
    networkInterfaces :: Core.Maybe [Types.NetworkInterface],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeNetworkInterfacesResponse' value with any optional fields omitted.
mkDescribeNetworkInterfacesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeNetworkInterfacesResponse
mkDescribeNetworkInterfacesResponse responseStatus =
  DescribeNetworkInterfacesResponse'
    { networkInterfaces =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about one or more network interfaces.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsNetworkInterfaces :: Lens.Lens' DescribeNetworkInterfacesResponse (Core.Maybe [Types.NetworkInterface])
dnirrsNetworkInterfaces = Lens.field @"networkInterfaces"
{-# DEPRECATED dnirrsNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsNextToken :: Lens.Lens' DescribeNetworkInterfacesResponse (Core.Maybe Types.NextToken)
dnirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dnirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsResponseStatus :: Lens.Lens' DescribeNetworkInterfacesResponse Core.Int
dnirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dnirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
