{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6CidrAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6CidrAssociation
  ( IPv6CidrAssociation (..),

    -- * Smart constructor
    mkIPv6CidrAssociation,

    -- * Lenses
    icaAssociatedResource,
    icaIPv6Cidr,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 CIDR block association.
--
-- /See:/ 'mkIPv6CidrAssociation' smart constructor.
data IPv6CidrAssociation = IPv6CidrAssociation'
  { associatedResource ::
      Lude.Maybe Lude.Text,
    ipv6Cidr :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPv6CidrAssociation' with the minimum fields required to make a request.
--
-- * 'associatedResource' - The resource that's associated with the IPv6 CIDR block.
-- * 'ipv6Cidr' - The IPv6 CIDR block.
mkIPv6CidrAssociation ::
  IPv6CidrAssociation
mkIPv6CidrAssociation =
  IPv6CidrAssociation'
    { associatedResource = Lude.Nothing,
      ipv6Cidr = Lude.Nothing
    }

-- | The resource that's associated with the IPv6 CIDR block.
--
-- /Note:/ Consider using 'associatedResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icaAssociatedResource :: Lens.Lens' IPv6CidrAssociation (Lude.Maybe Lude.Text)
icaAssociatedResource = Lens.lens (associatedResource :: IPv6CidrAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associatedResource = a} :: IPv6CidrAssociation)
{-# DEPRECATED icaAssociatedResource "Use generic-lens or generic-optics with 'associatedResource' instead." #-}

-- | The IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6Cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icaIPv6Cidr :: Lens.Lens' IPv6CidrAssociation (Lude.Maybe Lude.Text)
icaIPv6Cidr = Lens.lens (ipv6Cidr :: IPv6CidrAssociation -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Cidr = a} :: IPv6CidrAssociation)
{-# DEPRECATED icaIPv6Cidr "Use generic-lens or generic-optics with 'ipv6Cidr' instead." #-}

instance Lude.FromXML IPv6CidrAssociation where
  parseXML x =
    IPv6CidrAssociation'
      Lude.<$> (x Lude..@? "associatedResource") Lude.<*> (x Lude..@? "ipv6Cidr")
