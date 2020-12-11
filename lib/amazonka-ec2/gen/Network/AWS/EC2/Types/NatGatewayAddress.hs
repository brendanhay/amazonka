-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NatGatewayAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NatGatewayAddress
  ( NatGatewayAddress (..),

    -- * Smart constructor
    mkNatGatewayAddress,

    -- * Lenses
    ngaPrivateIP,
    ngaAllocationId,
    ngaNetworkInterfaceId,
    ngaPublicIP,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the IP addresses and network interface associated with a NAT gateway.
--
-- /See:/ 'mkNatGatewayAddress' smart constructor.
data NatGatewayAddress = NatGatewayAddress'
  { privateIP ::
      Lude.Maybe Lude.Text,
    allocationId :: Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    publicIP :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NatGatewayAddress' with the minimum fields required to make a request.
--
-- * 'allocationId' - The allocation ID of the Elastic IP address that's associated with the NAT gateway.
-- * 'networkInterfaceId' - The ID of the network interface associated with the NAT gateway.
-- * 'privateIP' - The private IP address associated with the Elastic IP address.
-- * 'publicIP' - The Elastic IP address associated with the NAT gateway.
mkNatGatewayAddress ::
  NatGatewayAddress
mkNatGatewayAddress =
  NatGatewayAddress'
    { privateIP = Lude.Nothing,
      allocationId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      publicIP = Lude.Nothing
    }

-- | The private IP address associated with the Elastic IP address.
--
-- /Note:/ Consider using 'privateIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngaPrivateIP :: Lens.Lens' NatGatewayAddress (Lude.Maybe Lude.Text)
ngaPrivateIP = Lens.lens (privateIP :: NatGatewayAddress -> Lude.Maybe Lude.Text) (\s a -> s {privateIP = a} :: NatGatewayAddress)
{-# DEPRECATED ngaPrivateIP "Use generic-lens or generic-optics with 'privateIP' instead." #-}

-- | The allocation ID of the Elastic IP address that's associated with the NAT gateway.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngaAllocationId :: Lens.Lens' NatGatewayAddress (Lude.Maybe Lude.Text)
ngaAllocationId = Lens.lens (allocationId :: NatGatewayAddress -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: NatGatewayAddress)
{-# DEPRECATED ngaAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The ID of the network interface associated with the NAT gateway.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngaNetworkInterfaceId :: Lens.Lens' NatGatewayAddress (Lude.Maybe Lude.Text)
ngaNetworkInterfaceId = Lens.lens (networkInterfaceId :: NatGatewayAddress -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: NatGatewayAddress)
{-# DEPRECATED ngaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The Elastic IP address associated with the NAT gateway.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngaPublicIP :: Lens.Lens' NatGatewayAddress (Lude.Maybe Lude.Text)
ngaPublicIP = Lens.lens (publicIP :: NatGatewayAddress -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: NatGatewayAddress)
{-# DEPRECATED ngaPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

instance Lude.FromXML NatGatewayAddress where
  parseXML x =
    NatGatewayAddress'
      Lude.<$> (x Lude..@? "privateIp")
      Lude.<*> (x Lude..@? "allocationId")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "publicIp")
