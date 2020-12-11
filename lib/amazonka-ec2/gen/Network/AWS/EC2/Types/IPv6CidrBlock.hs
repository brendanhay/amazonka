-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6CidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6CidrBlock
  ( IPv6CidrBlock (..),

    -- * Smart constructor
    mkIPv6CidrBlock,

    -- * Lenses
    icbIPv6CidrBlock,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 CIDR block.
--
-- /See:/ 'mkIPv6CidrBlock' smart constructor.
newtype IPv6CidrBlock = IPv6CidrBlock'
  { ipv6CidrBlock ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPv6CidrBlock' with the minimum fields required to make a request.
--
-- * 'ipv6CidrBlock' - The IPv6 CIDR block.
mkIPv6CidrBlock ::
  IPv6CidrBlock
mkIPv6CidrBlock = IPv6CidrBlock' {ipv6CidrBlock = Lude.Nothing}

-- | The IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icbIPv6CidrBlock :: Lens.Lens' IPv6CidrBlock (Lude.Maybe Lude.Text)
icbIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: IPv6CidrBlock -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: IPv6CidrBlock)
{-# DEPRECATED icbIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

instance Lude.FromXML IPv6CidrBlock where
  parseXML x = IPv6CidrBlock' Lude.<$> (x Lude..@? "ipv6CidrBlock")
