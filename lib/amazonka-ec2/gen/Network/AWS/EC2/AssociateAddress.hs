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
    aasInstanceId,
    aasAllocationId,
    aasNetworkInterfaceId,
    aasAllowReassociation,
    aasPrivateIPAddress,
    aasPublicIP,
    aasDryRun,

    -- * Destructuring the response
    AssociateAddressResponse (..),
    mkAssociateAddressResponse,

    -- ** Response lenses
    arsAssociationId,
    arsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateAddress' smart constructor.
data AssociateAddress = AssociateAddress'
  { -- | The ID of the instance. This is required for EC2-Classic. For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both. The operation fails if you specify an instance ID unless exactly one network interface is attached.
    instanceId :: Lude.Maybe Lude.Text,
    -- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
    allocationId :: Lude.Maybe Lude.Text,
    -- | [EC2-VPC] The ID of the network interface. If the instance has more than one network interface, you must specify a network interface ID.
    --
    -- For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | [EC2-VPC] For a VPC in an EC2-Classic account, specify true to allow an Elastic IP address that is already associated with an instance or network interface to be reassociated with the specified instance or network interface. Otherwise, the operation fails. In a VPC in an EC2-VPC-only account, reassociation is automatic, therefore you can specify false to ensure the operation fails if the Elastic IP address is already associated with another resource.
    allowReassociation :: Lude.Maybe Lude.Bool,
    -- | [EC2-VPC] The primary or secondary private IP address to associate with the Elastic IP address. If no private IP address is specified, the Elastic IP address is associated with the primary private IP address.
    privateIPAddress :: Lude.Maybe Lude.Text,
    -- | The Elastic IP address to associate with the instance. This is required for EC2-Classic.
    publicIP :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateAddress' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance. This is required for EC2-Classic. For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both. The operation fails if you specify an instance ID unless exactly one network interface is attached.
-- * 'allocationId' - [EC2-VPC] The allocation ID. This is required for EC2-VPC.
-- * 'networkInterfaceId' - [EC2-VPC] The ID of the network interface. If the instance has more than one network interface, you must specify a network interface ID.
--
-- For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both.
-- * 'allowReassociation' - [EC2-VPC] For a VPC in an EC2-Classic account, specify true to allow an Elastic IP address that is already associated with an instance or network interface to be reassociated with the specified instance or network interface. Otherwise, the operation fails. In a VPC in an EC2-VPC-only account, reassociation is automatic, therefore you can specify false to ensure the operation fails if the Elastic IP address is already associated with another resource.
-- * 'privateIPAddress' - [EC2-VPC] The primary or secondary private IP address to associate with the Elastic IP address. If no private IP address is specified, the Elastic IP address is associated with the primary private IP address.
-- * 'publicIP' - The Elastic IP address to associate with the instance. This is required for EC2-Classic.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAssociateAddress ::
  AssociateAddress
mkAssociateAddress =
  AssociateAddress'
    { instanceId = Lude.Nothing,
      allocationId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      allowReassociation = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      publicIP = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance. This is required for EC2-Classic. For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both. The operation fails if you specify an instance ID unless exactly one network interface is attached.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasInstanceId :: Lens.Lens' AssociateAddress (Lude.Maybe Lude.Text)
aasInstanceId = Lens.lens (instanceId :: AssociateAddress -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: AssociateAddress)
{-# DEPRECATED aasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAllocationId :: Lens.Lens' AssociateAddress (Lude.Maybe Lude.Text)
aasAllocationId = Lens.lens (allocationId :: AssociateAddress -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: AssociateAddress)
{-# DEPRECATED aasAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | [EC2-VPC] The ID of the network interface. If the instance has more than one network interface, you must specify a network interface ID.
--
-- For EC2-VPC, you can specify either the instance ID or the network interface ID, but not both.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasNetworkInterfaceId :: Lens.Lens' AssociateAddress (Lude.Maybe Lude.Text)
aasNetworkInterfaceId = Lens.lens (networkInterfaceId :: AssociateAddress -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: AssociateAddress)
{-# DEPRECATED aasNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | [EC2-VPC] For a VPC in an EC2-Classic account, specify true to allow an Elastic IP address that is already associated with an instance or network interface to be reassociated with the specified instance or network interface. Otherwise, the operation fails. In a VPC in an EC2-VPC-only account, reassociation is automatic, therefore you can specify false to ensure the operation fails if the Elastic IP address is already associated with another resource.
--
-- /Note:/ Consider using 'allowReassociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAllowReassociation :: Lens.Lens' AssociateAddress (Lude.Maybe Lude.Bool)
aasAllowReassociation = Lens.lens (allowReassociation :: AssociateAddress -> Lude.Maybe Lude.Bool) (\s a -> s {allowReassociation = a} :: AssociateAddress)
{-# DEPRECATED aasAllowReassociation "Use generic-lens or generic-optics with 'allowReassociation' instead." #-}

-- | [EC2-VPC] The primary or secondary private IP address to associate with the Elastic IP address. If no private IP address is specified, the Elastic IP address is associated with the primary private IP address.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasPrivateIPAddress :: Lens.Lens' AssociateAddress (Lude.Maybe Lude.Text)
aasPrivateIPAddress = Lens.lens (privateIPAddress :: AssociateAddress -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: AssociateAddress)
{-# DEPRECATED aasPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The Elastic IP address to associate with the instance. This is required for EC2-Classic.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasPublicIP :: Lens.Lens' AssociateAddress (Lude.Maybe Lude.Text)
aasPublicIP = Lens.lens (publicIP :: AssociateAddress -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: AssociateAddress)
{-# DEPRECATED aasPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasDryRun :: Lens.Lens' AssociateAddress (Lude.Maybe Lude.Bool)
aasDryRun = Lens.lens (dryRun :: AssociateAddress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateAddress)
{-# DEPRECATED aasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AssociateAddress where
  type Rs AssociateAddress = AssociateAddressResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateAddressResponse'
            Lude.<$> (x Lude..@? "associationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateAddress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateAddress where
  toQuery AssociateAddress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssociateAddress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "AllocationId" Lude.=: allocationId,
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "AllowReassociation" Lude.=: allowReassociation,
        "PrivateIpAddress" Lude.=: privateIPAddress,
        "PublicIp" Lude.=: publicIP,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAssociateAddressResponse' smart constructor.
data AssociateAddressResponse = AssociateAddressResponse'
  { -- | [EC2-VPC] The ID that represents the association of the Elastic IP address with an instance.
    associationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateAddressResponse' with the minimum fields required to make a request.
--
-- * 'associationId' - [EC2-VPC] The ID that represents the association of the Elastic IP address with an instance.
-- * 'responseStatus' - The response status code.
mkAssociateAddressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateAddressResponse
mkAssociateAddressResponse pResponseStatus_ =
  AssociateAddressResponse'
    { associationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | [EC2-VPC] The ID that represents the association of the Elastic IP address with an instance.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arsAssociationId :: Lens.Lens' AssociateAddressResponse (Lude.Maybe Lude.Text)
arsAssociationId = Lens.lens (associationId :: AssociateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociateAddressResponse)
{-# DEPRECATED arsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arsResponseStatus :: Lens.Lens' AssociateAddressResponse Lude.Int
arsResponseStatus = Lens.lens (responseStatus :: AssociateAddressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateAddressResponse)
{-# DEPRECATED arsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
