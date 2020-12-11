-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.NetworkInterface
  ( NetworkInterface (..),

    -- * Smart constructor
    mkNetworkInterface,

    -- * Lenses
    niSubnetId,
    niNetworkInterfaceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network interface.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { subnetId ::
      Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'subnetId' - The ID of the subnet.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { subnetId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing
    }

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSubnetId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niSubnetId = Lens.lens (subnetId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: NetworkInterface)
{-# DEPRECATED niSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkInterfaceId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niNetworkInterfaceId = Lens.lens (networkInterfaceId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: NetworkInterface)
{-# DEPRECATED niNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

instance Lude.FromJSON NetworkInterface where
  parseJSON =
    Lude.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Lude.<$> (x Lude..:? "subnetId") Lude.<*> (x Lude..:? "networkInterfaceId")
      )
