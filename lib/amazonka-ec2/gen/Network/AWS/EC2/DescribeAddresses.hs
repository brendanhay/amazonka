{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Elastic IP addresses or all of your Elastic IP addresses.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeAddresses
  ( -- * Creating a request
    DescribeAddresses (..),
    mkDescribeAddresses,

    -- ** Request lenses
    daAllocationIds,
    daDryRun,
    daFilters,
    daPublicIps,

    -- * Destructuring the response
    DescribeAddressesResponse (..),
    mkDescribeAddressesResponse,

    -- ** Response lenses
    darrsAddresses,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { -- | [EC2-VPC] Information about the allocation IDs.
    allocationIds :: Core.Maybe [Types.AllocationId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    --
    --     * @allocation-id@ - [EC2-VPC] The allocation ID for the address.
    --
    --
    --     * @association-id@ - [EC2-VPC] The association ID for the address.
    --
    --
    --     * @domain@ - Indicates whether the address is for use in EC2-Classic (@standard@ ) or in a VPC (@vpc@ ).
    --
    --
    --     * @instance-id@ - The ID of the instance the address is associated with, if any.
    --
    --
    --     * @network-border-group@ - A unique set of Availability Zones, Local Zones, or Wavelength Zones from where AWS advertises IP addresses.
    --
    --
    --     * @network-interface-id@ - [EC2-VPC] The ID of the network interface that the address is associated with, if any.
    --
    --
    --     * @network-interface-owner-id@ - The AWS account ID of the owner.
    --
    --
    --     * @private-ip-address@ - [EC2-VPC] The private IP address associated with the Elastic IP address.
    --
    --
    --     * @public-ip@ - The Elastic IP address, or the carrier IP address.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filters :: Core.Maybe [Types.Filter],
    -- | One or more Elastic IP addresses.
    --
    -- Default: Describes all your Elastic IP addresses.
    publicIps :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAddresses' value with any optional fields omitted.
mkDescribeAddresses ::
  DescribeAddresses
mkDescribeAddresses =
  DescribeAddresses'
    { allocationIds = Core.Nothing,
      dryRun = Core.Nothing,
      filters = Core.Nothing,
      publicIps = Core.Nothing
    }

-- | [EC2-VPC] Information about the allocation IDs.
--
-- /Note:/ Consider using 'allocationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAllocationIds :: Lens.Lens' DescribeAddresses (Core.Maybe [Types.AllocationId])
daAllocationIds = Lens.field @"allocationIds"
{-# DEPRECATED daAllocationIds "Use generic-lens or generic-optics with 'allocationIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDryRun :: Lens.Lens' DescribeAddresses (Core.Maybe Core.Bool)
daDryRun = Lens.field @"dryRun"
{-# DEPRECATED daDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @allocation-id@ - [EC2-VPC] The allocation ID for the address.
--
--
--     * @association-id@ - [EC2-VPC] The association ID for the address.
--
--
--     * @domain@ - Indicates whether the address is for use in EC2-Classic (@standard@ ) or in a VPC (@vpc@ ).
--
--
--     * @instance-id@ - The ID of the instance the address is associated with, if any.
--
--
--     * @network-border-group@ - A unique set of Availability Zones, Local Zones, or Wavelength Zones from where AWS advertises IP addresses.
--
--
--     * @network-interface-id@ - [EC2-VPC] The ID of the network interface that the address is associated with, if any.
--
--
--     * @network-interface-owner-id@ - The AWS account ID of the owner.
--
--
--     * @private-ip-address@ - [EC2-VPC] The private IP address associated with the Elastic IP address.
--
--
--     * @public-ip@ - The Elastic IP address, or the carrier IP address.
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
daFilters :: Lens.Lens' DescribeAddresses (Core.Maybe [Types.Filter])
daFilters = Lens.field @"filters"
{-# DEPRECATED daFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more Elastic IP addresses.
--
-- Default: Describes all your Elastic IP addresses.
--
-- /Note:/ Consider using 'publicIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daPublicIps :: Lens.Lens' DescribeAddresses (Core.Maybe [Types.String])
daPublicIps = Lens.field @"publicIps"
{-# DEPRECATED daPublicIps "Use generic-lens or generic-optics with 'publicIps' instead." #-}

instance Core.AWSRequest DescribeAddresses where
  type Rs DescribeAddresses = DescribeAddressesResponse
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
            ( Core.pure ("Action", "DescribeAddresses")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "AllocationId" Core.<$> allocationIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "PublicIp" Core.<$> publicIps)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAddressesResponse'
            Core.<$> (x Core..@? "addressesSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { -- | Information about the Elastic IP addresses.
    addresses :: Core.Maybe [Types.Address],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAddressesResponse' value with any optional fields omitted.
mkDescribeAddressesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAddressesResponse
mkDescribeAddressesResponse responseStatus =
  DescribeAddressesResponse'
    { addresses = Core.Nothing,
      responseStatus
    }

-- | Information about the Elastic IP addresses.
--
-- /Note:/ Consider using 'addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAddresses :: Lens.Lens' DescribeAddressesResponse (Core.Maybe [Types.Address])
darrsAddresses = Lens.field @"addresses"
{-# DEPRECATED darrsAddresses "Use generic-lens or generic-optics with 'addresses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAddressesResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
