-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CidrBlock
  ( CidrBlock (..),

    -- * Smart constructor
    mkCidrBlock,

    -- * Lenses
    cbCidrBlock,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv4 CIDR block.
--
-- /See:/ 'mkCidrBlock' smart constructor.
newtype CidrBlock = CidrBlock' {cidrBlock :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CidrBlock' with the minimum fields required to make a request.
--
-- * 'cidrBlock' - The IPv4 CIDR block.
mkCidrBlock ::
  CidrBlock
mkCidrBlock = CidrBlock' {cidrBlock = Lude.Nothing}

-- | The IPv4 CIDR block.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCidrBlock :: Lens.Lens' CidrBlock (Lude.Maybe Lude.Text)
cbCidrBlock = Lens.lens (cidrBlock :: CidrBlock -> Lude.Maybe Lude.Text) (\s a -> s {cidrBlock = a} :: CidrBlock)
{-# DEPRECATED cbCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

instance Lude.FromXML CidrBlock where
  parseXML x = CidrBlock' Lude.<$> (x Lude..@? "cidrBlock")
