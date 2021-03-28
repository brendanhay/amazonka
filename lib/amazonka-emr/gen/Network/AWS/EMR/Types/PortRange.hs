{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PortRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.PortRange
  ( PortRange (..)
  -- * Smart constructor
  , mkPortRange
  -- * Lenses
  , prMinRange
  , prMaxRange
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of port ranges that are permitted to allow inbound traffic from all public IP addresses. To specify a single port, use the same value for @MinRange@ and @MaxRange@ .
--
-- /See:/ 'mkPortRange' smart constructor.
data PortRange = PortRange'
  { minRange :: Core.Int
    -- ^ The smallest port number in a specified range of port numbers.
  , maxRange :: Core.Maybe Core.Int
    -- ^ The smallest port number in a specified range of port numbers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PortRange' value with any optional fields omitted.
mkPortRange
    :: Core.Int -- ^ 'minRange'
    -> PortRange
mkPortRange minRange
  = PortRange'{minRange, maxRange = Core.Nothing}

-- | The smallest port number in a specified range of port numbers.
--
-- /Note:/ Consider using 'minRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prMinRange :: Lens.Lens' PortRange Core.Int
prMinRange = Lens.field @"minRange"
{-# INLINEABLE prMinRange #-}
{-# DEPRECATED minRange "Use generic-lens or generic-optics with 'minRange' instead"  #-}

-- | The smallest port number in a specified range of port numbers.
--
-- /Note:/ Consider using 'maxRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prMaxRange :: Lens.Lens' PortRange (Core.Maybe Core.Int)
prMaxRange = Lens.field @"maxRange"
{-# INLINEABLE prMaxRange #-}
{-# DEPRECATED maxRange "Use generic-lens or generic-optics with 'maxRange' instead"  #-}

instance Core.FromJSON PortRange where
        toJSON PortRange{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MinRange" Core..= minRange),
                  ("MaxRange" Core..=) Core.<$> maxRange])

instance Core.FromJSON PortRange where
        parseJSON
          = Core.withObject "PortRange" Core.$
              \ x ->
                PortRange' Core.<$>
                  (x Core..: "MinRange") Core.<*> x Core..:? "MaxRange"
