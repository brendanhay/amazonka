{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.StepTimeline
  ( StepTimeline (..)
  -- * Smart constructor
  , mkStepTimeline
  -- * Lenses
  , stCreationDateTime
  , stEndDateTime
  , stStartDateTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The timeline of the cluster step lifecycle.
--
-- /See:/ 'mkStepTimeline' smart constructor.
data StepTimeline = StepTimeline'
  { creationDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the cluster step was created.
  , endDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the cluster step execution completed or failed.
  , startDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the cluster step execution started.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StepTimeline' value with any optional fields omitted.
mkStepTimeline
    :: StepTimeline
mkStepTimeline
  = StepTimeline'{creationDateTime = Core.Nothing,
                  endDateTime = Core.Nothing, startDateTime = Core.Nothing}

-- | The date and time when the cluster step was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCreationDateTime :: Lens.Lens' StepTimeline (Core.Maybe Core.NominalDiffTime)
stCreationDateTime = Lens.field @"creationDateTime"
{-# INLINEABLE stCreationDateTime #-}
{-# DEPRECATED creationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead"  #-}

-- | The date and time when the cluster step execution completed or failed.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stEndDateTime :: Lens.Lens' StepTimeline (Core.Maybe Core.NominalDiffTime)
stEndDateTime = Lens.field @"endDateTime"
{-# INLINEABLE stEndDateTime #-}
{-# DEPRECATED endDateTime "Use generic-lens or generic-optics with 'endDateTime' instead"  #-}

-- | The date and time when the cluster step execution started.
--
-- /Note:/ Consider using 'startDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStartDateTime :: Lens.Lens' StepTimeline (Core.Maybe Core.NominalDiffTime)
stStartDateTime = Lens.field @"startDateTime"
{-# INLINEABLE stStartDateTime #-}
{-# DEPRECATED startDateTime "Use generic-lens or generic-optics with 'startDateTime' instead"  #-}

instance Core.FromJSON StepTimeline where
        parseJSON
          = Core.withObject "StepTimeline" Core.$
              \ x ->
                StepTimeline' Core.<$>
                  (x Core..:? "CreationDateTime") Core.<*> x Core..:? "EndDateTime"
                    Core.<*> x Core..:? "StartDateTime"
