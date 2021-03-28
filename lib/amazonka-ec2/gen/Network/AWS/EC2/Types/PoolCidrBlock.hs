{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PoolCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PoolCidrBlock
  ( PoolCidrBlock (..)
  -- * Smart constructor
  , mkPoolCidrBlock
  -- * Lenses
  , pcbCidr
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a CIDR block for an address pool.
--
-- /See:/ 'mkPoolCidrBlock' smart constructor.
newtype PoolCidrBlock = PoolCidrBlock'
  { cidr :: Core.Maybe Core.Text
    -- ^ The CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PoolCidrBlock' value with any optional fields omitted.
mkPoolCidrBlock
    :: PoolCidrBlock
mkPoolCidrBlock = PoolCidrBlock'{cidr = Core.Nothing}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcbCidr :: Lens.Lens' PoolCidrBlock (Core.Maybe Core.Text)
pcbCidr = Lens.field @"cidr"
{-# INLINEABLE pcbCidr #-}
{-# DEPRECATED cidr "Use generic-lens or generic-optics with 'cidr' instead"  #-}

instance Core.FromXML PoolCidrBlock where
        parseXML x = PoolCidrBlock' Core.<$> (x Core..@? "poolCidrBlock")
