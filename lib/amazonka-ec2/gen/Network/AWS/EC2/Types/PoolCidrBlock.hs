-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PoolCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PoolCidrBlock
  ( PoolCidrBlock (..),

    -- * Smart constructor
    mkPoolCidrBlock,

    -- * Lenses
    pcbCidr,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a CIDR block for an address pool.
--
-- /See:/ 'mkPoolCidrBlock' smart constructor.
newtype PoolCidrBlock = PoolCidrBlock'
  { cidr ::
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

-- | Creates a value of 'PoolCidrBlock' with the minimum fields required to make a request.
--
-- * 'cidr' - The CIDR block.
mkPoolCidrBlock ::
  PoolCidrBlock
mkPoolCidrBlock = PoolCidrBlock' {cidr = Lude.Nothing}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcbCidr :: Lens.Lens' PoolCidrBlock (Lude.Maybe Lude.Text)
pcbCidr = Lens.lens (cidr :: PoolCidrBlock -> Lude.Maybe Lude.Text) (\s a -> s {cidr = a} :: PoolCidrBlock)
{-# DEPRECATED pcbCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Lude.FromXML PoolCidrBlock where
  parseXML x = PoolCidrBlock' Lude.<$> (x Lude..@? "poolCidrBlock")
