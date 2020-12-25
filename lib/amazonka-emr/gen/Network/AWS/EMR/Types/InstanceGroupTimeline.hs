{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupTimeline
  ( InstanceGroupTimeline (..),

    -- * Smart constructor
    mkInstanceGroupTimeline,

    -- * Lenses
    igtCreationDateTime,
    igtEndDateTime,
    igtReadyDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The timeline of the instance group lifecycle.
--
-- /See:/ 'mkInstanceGroupTimeline' smart constructor.
data InstanceGroupTimeline = InstanceGroupTimeline'
  { -- | The creation date and time of the instance group.
    creationDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time when the instance group terminated.
    endDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time when the instance group became ready to perform tasks.
    readyDateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceGroupTimeline' value with any optional fields omitted.
mkInstanceGroupTimeline ::
  InstanceGroupTimeline
mkInstanceGroupTimeline =
  InstanceGroupTimeline'
    { creationDateTime = Core.Nothing,
      endDateTime = Core.Nothing,
      readyDateTime = Core.Nothing
    }

-- | The creation date and time of the instance group.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igtCreationDateTime :: Lens.Lens' InstanceGroupTimeline (Core.Maybe Core.NominalDiffTime)
igtCreationDateTime = Lens.field @"creationDateTime"
{-# DEPRECATED igtCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The date and time when the instance group terminated.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igtEndDateTime :: Lens.Lens' InstanceGroupTimeline (Core.Maybe Core.NominalDiffTime)
igtEndDateTime = Lens.field @"endDateTime"
{-# DEPRECATED igtEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

-- | The date and time when the instance group became ready to perform tasks.
--
-- /Note:/ Consider using 'readyDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igtReadyDateTime :: Lens.Lens' InstanceGroupTimeline (Core.Maybe Core.NominalDiffTime)
igtReadyDateTime = Lens.field @"readyDateTime"
{-# DEPRECATED igtReadyDateTime "Use generic-lens or generic-optics with 'readyDateTime' instead." #-}

instance Core.FromJSON InstanceGroupTimeline where
  parseJSON =
    Core.withObject "InstanceGroupTimeline" Core.$
      \x ->
        InstanceGroupTimeline'
          Core.<$> (x Core..:? "CreationDateTime")
          Core.<*> (x Core..:? "EndDateTime")
          Core.<*> (x Core..:? "ReadyDateTime")
