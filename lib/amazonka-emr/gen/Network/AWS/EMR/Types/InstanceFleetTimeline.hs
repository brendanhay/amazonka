{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceFleetTimeline
  ( InstanceFleetTimeline (..)
  -- * Smart constructor
  , mkInstanceFleetTimeline
  -- * Lenses
  , iftCreationDateTime
  , iftEndDateTime
  , iftReadyDateTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
--
-- /See:/ 'mkInstanceFleetTimeline' smart constructor.
data InstanceFleetTimeline = InstanceFleetTimeline'
  { creationDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time and date the instance fleet was created.
  , endDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time and date the instance fleet terminated.
  , readyDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time and date the instance fleet was ready to run jobs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceFleetTimeline' value with any optional fields omitted.
mkInstanceFleetTimeline
    :: InstanceFleetTimeline
mkInstanceFleetTimeline
  = InstanceFleetTimeline'{creationDateTime = Core.Nothing,
                           endDateTime = Core.Nothing, readyDateTime = Core.Nothing}

-- | The time and date the instance fleet was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iftCreationDateTime :: Lens.Lens' InstanceFleetTimeline (Core.Maybe Core.NominalDiffTime)
iftCreationDateTime = Lens.field @"creationDateTime"
{-# INLINEABLE iftCreationDateTime #-}
{-# DEPRECATED creationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead"  #-}

-- | The time and date the instance fleet terminated.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iftEndDateTime :: Lens.Lens' InstanceFleetTimeline (Core.Maybe Core.NominalDiffTime)
iftEndDateTime = Lens.field @"endDateTime"
{-# INLINEABLE iftEndDateTime #-}
{-# DEPRECATED endDateTime "Use generic-lens or generic-optics with 'endDateTime' instead"  #-}

-- | The time and date the instance fleet was ready to run jobs.
--
-- /Note:/ Consider using 'readyDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iftReadyDateTime :: Lens.Lens' InstanceFleetTimeline (Core.Maybe Core.NominalDiffTime)
iftReadyDateTime = Lens.field @"readyDateTime"
{-# INLINEABLE iftReadyDateTime #-}
{-# DEPRECATED readyDateTime "Use generic-lens or generic-optics with 'readyDateTime' instead"  #-}

instance Core.FromJSON InstanceFleetTimeline where
        parseJSON
          = Core.withObject "InstanceFleetTimeline" Core.$
              \ x ->
                InstanceFleetTimeline' Core.<$>
                  (x Core..:? "CreationDateTime") Core.<*> x Core..:? "EndDateTime"
                    Core.<*> x Core..:? "ReadyDateTime"
