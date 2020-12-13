{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceIPv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceIPv6Address
  ( NetworkInterfaceIPv6Address (..),

    -- * Smart constructor
    mkNetworkInterfaceIPv6Address,

    -- * Lenses
    niiaIPv6Address,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 address associated with a network interface.
--
-- /See:/ 'mkNetworkInterfaceIPv6Address' smart constructor.
newtype NetworkInterfaceIPv6Address = NetworkInterfaceIPv6Address'
  { -- | The IPv6 address.
    ipv6Address :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterfaceIPv6Address' with the minimum fields required to make a request.
--
-- * 'ipv6Address' - The IPv6 address.
mkNetworkInterfaceIPv6Address ::
  NetworkInterfaceIPv6Address
mkNetworkInterfaceIPv6Address =
  NetworkInterfaceIPv6Address' {ipv6Address = Lude.Nothing}

-- | The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niiaIPv6Address :: Lens.Lens' NetworkInterfaceIPv6Address (Lude.Maybe Lude.Text)
niiaIPv6Address = Lens.lens (ipv6Address :: NetworkInterfaceIPv6Address -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: NetworkInterfaceIPv6Address)
{-# DEPRECATED niiaIPv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

instance Lude.FromXML NetworkInterfaceIPv6Address where
  parseXML x =
    NetworkInterfaceIPv6Address' Lude.<$> (x Lude..@? "ipv6Address")
