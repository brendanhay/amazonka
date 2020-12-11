{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    daFilters,
    daPublicIPs,
    daAllocationIds,
    daDryRun,

    -- * Destructuring the response
    DescribeAddressesResponse (..),
    mkDescribeAddressesResponse,

    -- ** Response lenses
    darsAddresses,
    darsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { filters ::
      Lude.Maybe [Filter],
    publicIPs :: Lude.Maybe [Lude.Text],
    allocationIds :: Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAddresses' with the minimum fields required to make a request.
--
-- * 'allocationIds' - [EC2-VPC] Information about the allocation IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
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
-- * 'publicIPs' - One or more Elastic IP addresses.
--
-- Default: Describes all your Elastic IP addresses.
mkDescribeAddresses ::
  DescribeAddresses
mkDescribeAddresses =
  DescribeAddresses'
    { filters = Lude.Nothing,
      publicIPs = Lude.Nothing,
      allocationIds = Lude.Nothing,
      dryRun = Lude.Nothing
    }

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
daFilters :: Lens.Lens' DescribeAddresses (Lude.Maybe [Filter])
daFilters = Lens.lens (filters :: DescribeAddresses -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeAddresses)
{-# DEPRECATED daFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more Elastic IP addresses.
--
-- Default: Describes all your Elastic IP addresses.
--
-- /Note:/ Consider using 'publicIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daPublicIPs :: Lens.Lens' DescribeAddresses (Lude.Maybe [Lude.Text])
daPublicIPs = Lens.lens (publicIPs :: DescribeAddresses -> Lude.Maybe [Lude.Text]) (\s a -> s {publicIPs = a} :: DescribeAddresses)
{-# DEPRECATED daPublicIPs "Use generic-lens or generic-optics with 'publicIPs' instead." #-}

-- | [EC2-VPC] Information about the allocation IDs.
--
-- /Note:/ Consider using 'allocationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAllocationIds :: Lens.Lens' DescribeAddresses (Lude.Maybe [Lude.Text])
daAllocationIds = Lens.lens (allocationIds :: DescribeAddresses -> Lude.Maybe [Lude.Text]) (\s a -> s {allocationIds = a} :: DescribeAddresses)
{-# DEPRECATED daAllocationIds "Use generic-lens or generic-optics with 'allocationIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDryRun :: Lens.Lens' DescribeAddresses (Lude.Maybe Lude.Bool)
daDryRun = Lens.lens (dryRun :: DescribeAddresses -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeAddresses)
{-# DEPRECATED daDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeAddresses where
  type Rs DescribeAddresses = DescribeAddressesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeAddressesResponse'
            Lude.<$> ( x Lude..@? "addressesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAddresses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAddresses where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAddresses where
  toQuery DescribeAddresses' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAddresses" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "PublicIp" Lude.<$> publicIPs),
        Lude.toQuery
          (Lude.toQueryList "AllocationId" Lude.<$> allocationIds),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { addresses ::
      Lude.Maybe [Address],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAddressesResponse' with the minimum fields required to make a request.
--
-- * 'addresses' - Information about the Elastic IP addresses.
-- * 'responseStatus' - The response status code.
mkDescribeAddressesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAddressesResponse
mkDescribeAddressesResponse pResponseStatus_ =
  DescribeAddressesResponse'
    { addresses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Elastic IP addresses.
--
-- /Note:/ Consider using 'addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAddresses :: Lens.Lens' DescribeAddressesResponse (Lude.Maybe [Address])
darsAddresses = Lens.lens (addresses :: DescribeAddressesResponse -> Lude.Maybe [Address]) (\s a -> s {addresses = a} :: DescribeAddressesResponse)
{-# DEPRECATED darsAddresses "Use generic-lens or generic-optics with 'addresses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAddressesResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAddressesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAddressesResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
