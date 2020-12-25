{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTimeline
  ( InstanceTimeline (..),

    -- * Smart constructor
    mkInstanceTimeline,

    -- * Lenses
    itCreationDateTime,
    itEndDateTime,
    itReadyDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The timeline of the instance lifecycle.
--
-- /See:/ 'mkInstanceTimeline' smart constructor.
data InstanceTimeline = InstanceTimeline'
  { -- | The creation date and time of the instance.
    creationDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time when the instance was terminated.
    endDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time when the instance was ready to perform tasks.
    readyDateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceTimeline' value with any optional fields omitted.
mkInstanceTimeline ::
  InstanceTimeline
mkInstanceTimeline =
  InstanceTimeline'
    { creationDateTime = Core.Nothing,
      endDateTime = Core.Nothing,
      readyDateTime = Core.Nothing
    }

-- | The creation date and time of the instance.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itCreationDateTime :: Lens.Lens' InstanceTimeline (Core.Maybe Core.NominalDiffTime)
itCreationDateTime = Lens.field @"creationDateTime"
{-# DEPRECATED itCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The date and time when the instance was terminated.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itEndDateTime :: Lens.Lens' InstanceTimeline (Core.Maybe Core.NominalDiffTime)
itEndDateTime = Lens.field @"endDateTime"
{-# DEPRECATED itEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

-- | The date and time when the instance was ready to perform tasks.
--
-- /Note:/ Consider using 'readyDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itReadyDateTime :: Lens.Lens' InstanceTimeline (Core.Maybe Core.NominalDiffTime)
itReadyDateTime = Lens.field @"readyDateTime"
{-# DEPRECATED itReadyDateTime "Use generic-lens or generic-optics with 'readyDateTime' instead." #-}

instance Core.FromJSON InstanceTimeline where
  parseJSON =
    Core.withObject "InstanceTimeline" Core.$
      \x ->
        InstanceTimeline'
          Core.<$> (x Core..:? "CreationDateTime")
          Core.<*> (x Core..:? "EndDateTime")
          Core.<*> (x Core..:? "ReadyDateTime")
