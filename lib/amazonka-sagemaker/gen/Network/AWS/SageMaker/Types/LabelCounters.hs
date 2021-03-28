{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.LabelCounters
  ( LabelCounters (..)
  -- * Smart constructor
  , mkLabelCounters
  -- * Lenses
  , lcFailedNonRetryableError
  , lcHumanLabeled
  , lcMachineLabeled
  , lcTotalLabeled
  , lcUnlabeled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a breakdown of the number of objects labeled.
--
-- /See:/ 'mkLabelCounters' smart constructor.
data LabelCounters = LabelCounters'
  { failedNonRetryableError :: Core.Maybe Core.Natural
    -- ^ The total number of objects that could not be labeled due to an error.
  , humanLabeled :: Core.Maybe Core.Natural
    -- ^ The total number of objects labeled by a human worker.
  , machineLabeled :: Core.Maybe Core.Natural
    -- ^ The total number of objects labeled by automated data labeling.
  , totalLabeled :: Core.Maybe Core.Natural
    -- ^ The total number of objects labeled.
  , unlabeled :: Core.Maybe Core.Natural
    -- ^ The total number of objects not yet labeled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelCounters' value with any optional fields omitted.
mkLabelCounters
    :: LabelCounters
mkLabelCounters
  = LabelCounters'{failedNonRetryableError = Core.Nothing,
                   humanLabeled = Core.Nothing, machineLabeled = Core.Nothing,
                   totalLabeled = Core.Nothing, unlabeled = Core.Nothing}

-- | The total number of objects that could not be labeled due to an error.
--
-- /Note:/ Consider using 'failedNonRetryableError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcFailedNonRetryableError :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
lcFailedNonRetryableError = Lens.field @"failedNonRetryableError"
{-# INLINEABLE lcFailedNonRetryableError #-}
{-# DEPRECATED failedNonRetryableError "Use generic-lens or generic-optics with 'failedNonRetryableError' instead"  #-}

-- | The total number of objects labeled by a human worker.
--
-- /Note:/ Consider using 'humanLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcHumanLabeled :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
lcHumanLabeled = Lens.field @"humanLabeled"
{-# INLINEABLE lcHumanLabeled #-}
{-# DEPRECATED humanLabeled "Use generic-lens or generic-optics with 'humanLabeled' instead"  #-}

-- | The total number of objects labeled by automated data labeling.
--
-- /Note:/ Consider using 'machineLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMachineLabeled :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
lcMachineLabeled = Lens.field @"machineLabeled"
{-# INLINEABLE lcMachineLabeled #-}
{-# DEPRECATED machineLabeled "Use generic-lens or generic-optics with 'machineLabeled' instead"  #-}

-- | The total number of objects labeled.
--
-- /Note:/ Consider using 'totalLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcTotalLabeled :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
lcTotalLabeled = Lens.field @"totalLabeled"
{-# INLINEABLE lcTotalLabeled #-}
{-# DEPRECATED totalLabeled "Use generic-lens or generic-optics with 'totalLabeled' instead"  #-}

-- | The total number of objects not yet labeled.
--
-- /Note:/ Consider using 'unlabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcUnlabeled :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
lcUnlabeled = Lens.field @"unlabeled"
{-# INLINEABLE lcUnlabeled #-}
{-# DEPRECATED unlabeled "Use generic-lens or generic-optics with 'unlabeled' instead"  #-}

instance Core.FromJSON LabelCounters where
        parseJSON
          = Core.withObject "LabelCounters" Core.$
              \ x ->
                LabelCounters' Core.<$>
                  (x Core..:? "FailedNonRetryableError") Core.<*>
                    x Core..:? "HumanLabeled"
                    Core.<*> x Core..:? "MachineLabeled"
                    Core.<*> x Core..:? "TotalLabeled"
                    Core.<*> x Core..:? "Unlabeled"
