-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6Range
  ( IPv6Range (..),

    -- * Smart constructor
    mkIPv6Range,

    -- * Lenses
    irCidrIPv6,
    irDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | [EC2-VPC only] Describes an IPv6 range.
--
-- /See:/ 'mkIPv6Range' smart constructor.
data IPv6Range = IPv6Range'
  { cidrIPv6 :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPv6Range' with the minimum fields required to make a request.
--
-- * 'cidrIPv6' - The IPv6 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv6 address, use the /128 prefix length.
-- * 'description' - A description for the security group rule that references this IPv6 address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
mkIPv6Range ::
  IPv6Range
mkIPv6Range =
  IPv6Range' {cidrIPv6 = Lude.Nothing, description = Lude.Nothing}

-- | The IPv6 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv6 address, use the /128 prefix length.
--
-- /Note:/ Consider using 'cidrIPv6' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irCidrIPv6 :: Lens.Lens' IPv6Range (Lude.Maybe Lude.Text)
irCidrIPv6 = Lens.lens (cidrIPv6 :: IPv6Range -> Lude.Maybe Lude.Text) (\s a -> s {cidrIPv6 = a} :: IPv6Range)
{-# DEPRECATED irCidrIPv6 "Use generic-lens or generic-optics with 'cidrIPv6' instead." #-}

-- | A description for the security group rule that references this IPv6 address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irDescription :: Lens.Lens' IPv6Range (Lude.Maybe Lude.Text)
irDescription = Lens.lens (description :: IPv6Range -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: IPv6Range)
{-# DEPRECATED irDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML IPv6Range where
  parseXML x =
    IPv6Range'
      Lude.<$> (x Lude..@? "cidrIpv6") Lude.<*> (x Lude..@? "description")

instance Lude.ToQuery IPv6Range where
  toQuery IPv6Range' {..} =
    Lude.mconcat
      ["CidrIpv6" Lude.=: cidrIPv6, "Description" Lude.=: description]
