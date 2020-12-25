{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Elastic IP address, or carrier IP address (for instances that are in subnets in Wavelength Zones) with an instance or a network interface. Before you can use an Elastic IP address, you must allocate it to your account.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
-- [EC2-Classic, VPC in an EC2-VPC-only account] If the Elastic IP address is already associated with a different instance, it is disassociated from that instance and associated with the specified instance. If you associate an Elastic IP address with an instance that has an existing Elastic IP address, the existing address is disassociated from the instance, but remains allocated to your account.
-- [VPC in an EC2-Classic account] If you don't specify a private IP address, the Elastic IP address is associated with the primary IP address. If the Elastic IP address is already associated with a different instance or a network interface, you get an error unless you allow reassociation. You cannot associate an Elastic IP address with an instance or network interface that has an existing Elastic IP address.
-- [Subnets in Wavelength Zones] You can associate an IP address from the telecommunication carrier to the instance or network interface.
-- You cannot associate an Elastic IP address with an interface in a different network border group.
-- /Important:/ This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error, and you may be charged for each time the Elastic IP address is remapped to the same instance. For more information, see the /Elastic IP Addresses/ section of <http://aws.amazon.com/ec2/pricing/ Amazon EC2 Pricing> .
module Network.AWS.EC2.AssociateAddress
  ( -- * Creating a request
    AssociateAddress (..),
    mkAssociateAddress,

    -- ** Request lenses
    aasAllocationId,
    aasAllowReassociation,
    aasDryRun,
    aasInstanceId,
    aasNetworkInterfaceId,
    aasPrivateIpAddress,
    aasPublicIp,

    -- * Destructuring the response
    AssociateAddressResponse (..),
    mkAssociateAddressResponse,

    -- ** Response lenses
    arsAssociationId,
    arsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateAddress' smart constructor.
data AssociateAddress = AssociateAddress'
  { -- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
    allocationId :: Core.Maybe Types.AllocationId,
    -- | [EC2-VPC] For a VPC in an EC2-Classic account, specify true to allow an Elastic IP address that is already associated with an instance or network interface to be reassociated with the specified instance or network interface. Otherwise, the operation fails. In a VPC in an EC2-VPC-only account, reassociation is automatic, therefore you can specify false to ensure the operation fails if the Elastic IP address is already associated with another resource.
    allowReassociation :: Core.Maybe Core.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the instance. This is required for EC2-Classic. For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both. The operation fails if you specify an instance ID unless exactly one network interface is attached.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | [EC2-VPC] The ID of the network interface. If the instance has more than one network interface, you must specify a network interface ID.
    --
    -- For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both.
    networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId,
    -- | [EC2-VPC] The primary or secondary private IP address to associate with the Elastic IP address. If no private IP address is specified, the Elastic IP address is associated with the primary private IP address.
    privateIpAddress :: Core.Maybe Types.String,
    -- | The Elastic IP address to associate with the instance. This is required for EC2-Classic.
    publicIp :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateAddress' value with any optional fields omitted.
mkAssociateAddress ::
  AssociateAddress
mkAssociateAddress =
  AssociateAddress'
    { allocationId = Core.Nothing,
      allowReassociation = Core.Nothing,
      dryRun = Core.Nothing,
      instanceId = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      privateIpAddress = Core.Nothing,
      publicIp = Core.Nothing
    }

-- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAllocationId :: Lens.Lens' AssociateAddress (Core.Maybe Types.AllocationId)
aasAllocationId = Lens.field @"allocationId"
{-# DEPRECATED aasAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | [EC2-VPC] For a VPC in an EC2-Classic account, specify true to allow an Elastic IP address that is already associated with an instance or network interface to be reassociated with the specified instance or network interface. Otherwise, the operation fails. In a VPC in an EC2-VPC-only account, reassociation is automatic, therefore you can specify false to ensure the operation fails if the Elastic IP address is already associated with another resource.
--
-- /Note:/ Consider using 'allowReassociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAllowReassociation :: Lens.Lens' AssociateAddress (Core.Maybe Core.Bool)
aasAllowReassociation = Lens.field @"allowReassociation"
{-# DEPRECATED aasAllowReassociation "Use generic-lens or generic-optics with 'allowReassociation' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasDryRun :: Lens.Lens' AssociateAddress (Core.Maybe Core.Bool)
aasDryRun = Lens.field @"dryRun"
{-# DEPRECATED aasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the instance. This is required for EC2-Classic. For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both. The operation fails if you specify an instance ID unless exactly one network interface is attached.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasInstanceId :: Lens.Lens' AssociateAddress (Core.Maybe Types.InstanceId)
aasInstanceId = Lens.field @"instanceId"
{-# DEPRECATED aasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | [EC2-VPC] The ID of the network interface. If the instance has more than one network interface, you must specify a network interface ID.
--
-- For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasNetworkInterfaceId :: Lens.Lens' AssociateAddress (Core.Maybe Types.NetworkInterfaceId)
aasNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED aasNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | [EC2-VPC] The primary or secondary private IP address to associate with the Elastic IP address. If no private IP address is specified, the Elastic IP address is associated with the primary private IP address.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasPrivateIpAddress :: Lens.Lens' AssociateAddress (Core.Maybe Types.String)
aasPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED aasPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | The Elastic IP address to associate with the instance. This is required for EC2-Classic.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasPublicIp :: Lens.Lens' AssociateAddress (Core.Maybe Types.String)
aasPublicIp = Lens.field @"publicIp"
{-# DEPRECATED aasPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

instance Core.AWSRequest AssociateAddress where
  type Rs AssociateAddress = AssociateAddressResponse
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
            ( Core.pure ("Action", "AssociateAddress")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AllocationId" Core.<$> allocationId)
                Core.<> ( Core.toQueryValue "AllowReassociation"
                            Core.<$> allowReassociation
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "InstanceId" Core.<$> instanceId)
                Core.<> ( Core.toQueryValue "NetworkInterfaceId"
                            Core.<$> networkInterfaceId
                        )
                Core.<> (Core.toQueryValue "PrivateIpAddress" Core.<$> privateIpAddress)
                Core.<> (Core.toQueryValue "PublicIp" Core.<$> publicIp)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateAddressResponse'
            Core.<$> (x Core..@? "associationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateAddressResponse' smart constructor.
data AssociateAddressResponse = AssociateAddressResponse'
  { -- | [EC2-VPC] The ID that represents the association of the Elastic IP address with an instance.
    associationId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateAddressResponse' value with any optional fields omitted.
mkAssociateAddressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateAddressResponse
mkAssociateAddressResponse responseStatus =
  AssociateAddressResponse'
    { associationId = Core.Nothing,
      responseStatus
    }

-- | [EC2-VPC] The ID that represents the association of the Elastic IP address with an instance.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arsAssociationId :: Lens.Lens' AssociateAddressResponse (Core.Maybe Types.String)
arsAssociationId = Lens.field @"associationId"
{-# DEPRECATED arsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arsResponseStatus :: Lens.Lens' AssociateAddressResponse Core.Int
arsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED arsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
