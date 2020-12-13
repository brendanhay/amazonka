{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceIPv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceIPv6Address
  ( InstanceIPv6Address (..),

    -- * Smart constructor
    mkInstanceIPv6Address,

    -- * Lenses
    iiaIPv6Address,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 address.
--
-- /See:/ 'mkInstanceIPv6Address' smart constructor.
newtype InstanceIPv6Address = InstanceIPv6Address'
  { -- | The IPv6 address.
    ipv6Address :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceIPv6Address' with the minimum fields required to make a request.
--
-- * 'ipv6Address' - The IPv6 address.
mkInstanceIPv6Address ::
  InstanceIPv6Address
mkInstanceIPv6Address =
  InstanceIPv6Address' {ipv6Address = Lude.Nothing}

-- | The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiaIPv6Address :: Lens.Lens' InstanceIPv6Address (Lude.Maybe Lude.Text)
iiaIPv6Address = Lens.lens (ipv6Address :: InstanceIPv6Address -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: InstanceIPv6Address)
{-# DEPRECATED iiaIPv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

instance Lude.FromXML InstanceIPv6Address where
  parseXML x =
    InstanceIPv6Address' Lude.<$> (x Lude..@? "ipv6Address")

instance Lude.ToQuery InstanceIPv6Address where
  toQuery InstanceIPv6Address' {..} =
    Lude.mconcat ["Ipv6Address" Lude.=: ipv6Address]
