{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Counters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Counters
  ( Counters (..)
  -- * Smart constructor
  , mkCounters
  -- * Lenses
  , cErrored
  , cFailed
  , cPassed
  , cSkipped
  , cStopped
  , cTotal
  , cWarned
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents entity counters.
--
-- /See:/ 'mkCounters' smart constructor.
data Counters = Counters'
  { errored :: Core.Maybe Core.Int
    -- ^ The number of errored entities.
  , failed :: Core.Maybe Core.Int
    -- ^ The number of failed entities.
  , passed :: Core.Maybe Core.Int
    -- ^ The number of passed entities.
  , skipped :: Core.Maybe Core.Int
    -- ^ The number of skipped entities.
  , stopped :: Core.Maybe Core.Int
    -- ^ The number of stopped entities.
  , total :: Core.Maybe Core.Int
    -- ^ The total number of entities.
  , warned :: Core.Maybe Core.Int
    -- ^ The number of warned entities.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Counters' value with any optional fields omitted.
mkCounters
    :: Counters
mkCounters
  = Counters'{errored = Core.Nothing, failed = Core.Nothing,
              passed = Core.Nothing, skipped = Core.Nothing,
              stopped = Core.Nothing, total = Core.Nothing,
              warned = Core.Nothing}

-- | The number of errored entities.
--
-- /Note:/ Consider using 'errored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cErrored :: Lens.Lens' Counters (Core.Maybe Core.Int)
cErrored = Lens.field @"errored"
{-# INLINEABLE cErrored #-}
{-# DEPRECATED errored "Use generic-lens or generic-optics with 'errored' instead"  #-}

-- | The number of failed entities.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cFailed :: Lens.Lens' Counters (Core.Maybe Core.Int)
cFailed = Lens.field @"failed"
{-# INLINEABLE cFailed #-}
{-# DEPRECATED failed "Use generic-lens or generic-optics with 'failed' instead"  #-}

-- | The number of passed entities.
--
-- /Note:/ Consider using 'passed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPassed :: Lens.Lens' Counters (Core.Maybe Core.Int)
cPassed = Lens.field @"passed"
{-# INLINEABLE cPassed #-}
{-# DEPRECATED passed "Use generic-lens or generic-optics with 'passed' instead"  #-}

-- | The number of skipped entities.
--
-- /Note:/ Consider using 'skipped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSkipped :: Lens.Lens' Counters (Core.Maybe Core.Int)
cSkipped = Lens.field @"skipped"
{-# INLINEABLE cSkipped #-}
{-# DEPRECATED skipped "Use generic-lens or generic-optics with 'skipped' instead"  #-}

-- | The number of stopped entities.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStopped :: Lens.Lens' Counters (Core.Maybe Core.Int)
cStopped = Lens.field @"stopped"
{-# INLINEABLE cStopped #-}
{-# DEPRECATED stopped "Use generic-lens or generic-optics with 'stopped' instead"  #-}

-- | The total number of entities.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTotal :: Lens.Lens' Counters (Core.Maybe Core.Int)
cTotal = Lens.field @"total"
{-# INLINEABLE cTotal #-}
{-# DEPRECATED total "Use generic-lens or generic-optics with 'total' instead"  #-}

-- | The number of warned entities.
--
-- /Note:/ Consider using 'warned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWarned :: Lens.Lens' Counters (Core.Maybe Core.Int)
cWarned = Lens.field @"warned"
{-# INLINEABLE cWarned #-}
{-# DEPRECATED warned "Use generic-lens or generic-optics with 'warned' instead"  #-}

instance Core.FromJSON Counters where
        parseJSON
          = Core.withObject "Counters" Core.$
              \ x ->
                Counters' Core.<$>
                  (x Core..:? "errored") Core.<*> x Core..:? "failed" Core.<*>
                    x Core..:? "passed"
                    Core.<*> x Core..:? "skipped"
                    Core.<*> x Core..:? "stopped"
                    Core.<*> x Core..:? "total"
                    Core.<*> x Core..:? "warned"
