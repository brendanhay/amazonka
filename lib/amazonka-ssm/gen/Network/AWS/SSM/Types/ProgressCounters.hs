{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ProgressCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ProgressCounters
  ( ProgressCounters (..)
  -- * Smart constructor
  , mkProgressCounters
  -- * Lenses
  , pcCancelledSteps
  , pcFailedSteps
  , pcSuccessSteps
  , pcTimedOutSteps
  , pcTotalSteps
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
--
-- /See:/ 'mkProgressCounters' smart constructor.
data ProgressCounters = ProgressCounters'
  { cancelledSteps :: Core.Maybe Core.Int
    -- ^ The total number of steps that the system cancelled in all specified AWS Regions and accounts for the current Automation execution.
  , failedSteps :: Core.Maybe Core.Int
    -- ^ The total number of steps that failed to run in all specified AWS Regions and accounts for the current Automation execution.
  , successSteps :: Core.Maybe Core.Int
    -- ^ The total number of steps that successfully completed in all specified AWS Regions and accounts for the current Automation execution.
  , timedOutSteps :: Core.Maybe Core.Int
    -- ^ The total number of steps that timed out in all specified AWS Regions and accounts for the current Automation execution.
  , totalSteps :: Core.Maybe Core.Int
    -- ^ The total number of steps run in all specified AWS Regions and accounts for the current Automation execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProgressCounters' value with any optional fields omitted.
mkProgressCounters
    :: ProgressCounters
mkProgressCounters
  = ProgressCounters'{cancelledSteps = Core.Nothing,
                      failedSteps = Core.Nothing, successSteps = Core.Nothing,
                      timedOutSteps = Core.Nothing, totalSteps = Core.Nothing}

-- | The total number of steps that the system cancelled in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'cancelledSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcCancelledSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
pcCancelledSteps = Lens.field @"cancelledSteps"
{-# INLINEABLE pcCancelledSteps #-}
{-# DEPRECATED cancelledSteps "Use generic-lens or generic-optics with 'cancelledSteps' instead"  #-}

-- | The total number of steps that failed to run in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'failedSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcFailedSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
pcFailedSteps = Lens.field @"failedSteps"
{-# INLINEABLE pcFailedSteps #-}
{-# DEPRECATED failedSteps "Use generic-lens or generic-optics with 'failedSteps' instead"  #-}

-- | The total number of steps that successfully completed in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'successSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcSuccessSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
pcSuccessSteps = Lens.field @"successSteps"
{-# INLINEABLE pcSuccessSteps #-}
{-# DEPRECATED successSteps "Use generic-lens or generic-optics with 'successSteps' instead"  #-}

-- | The total number of steps that timed out in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'timedOutSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcTimedOutSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
pcTimedOutSteps = Lens.field @"timedOutSteps"
{-# INLINEABLE pcTimedOutSteps #-}
{-# DEPRECATED timedOutSteps "Use generic-lens or generic-optics with 'timedOutSteps' instead"  #-}

-- | The total number of steps run in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'totalSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcTotalSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
pcTotalSteps = Lens.field @"totalSteps"
{-# INLINEABLE pcTotalSteps #-}
{-# DEPRECATED totalSteps "Use generic-lens or generic-optics with 'totalSteps' instead"  #-}

instance Core.FromJSON ProgressCounters where
        parseJSON
          = Core.withObject "ProgressCounters" Core.$
              \ x ->
                ProgressCounters' Core.<$>
                  (x Core..:? "CancelledSteps") Core.<*> x Core..:? "FailedSteps"
                    Core.<*> x Core..:? "SuccessSteps"
                    Core.<*> x Core..:? "TimedOutSteps"
                    Core.<*> x Core..:? "TotalSteps"
