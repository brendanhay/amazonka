{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PortRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PortRange
  ( PortRange (..)
  -- * Smart constructor
  , mkPortRange
  -- * Lenses
  , prFrom
  , prTo
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a range of ports.
--
-- /See:/ 'mkPortRange' smart constructor.
data PortRange = PortRange'
  { from :: Core.Maybe Core.Int
    -- ^ The first port in the range.
  , to :: Core.Maybe Core.Int
    -- ^ The last port in the range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PortRange' value with any optional fields omitted.
mkPortRange
    :: PortRange
mkPortRange = PortRange'{from = Core.Nothing, to = Core.Nothing}

-- | The first port in the range.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prFrom :: Lens.Lens' PortRange (Core.Maybe Core.Int)
prFrom = Lens.field @"from"
{-# INLINEABLE prFrom #-}
{-# DEPRECATED from "Use generic-lens or generic-optics with 'from' instead"  #-}

-- | The last port in the range.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prTo :: Lens.Lens' PortRange (Core.Maybe Core.Int)
prTo = Lens.field @"to"
{-# INLINEABLE prTo #-}
{-# DEPRECATED to "Use generic-lens or generic-optics with 'to' instead"  #-}

instance Core.ToQuery PortRange where
        toQuery PortRange{..}
          = Core.maybe Core.mempty (Core.toQueryPair "From") from Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "To") to

instance Core.FromXML PortRange where
        parseXML x
          = PortRange' Core.<$> (x Core..@? "from") Core.<*> x Core..@? "to"
