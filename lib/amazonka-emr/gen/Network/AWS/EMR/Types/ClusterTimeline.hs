{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterTimeline
  ( ClusterTimeline (..),

    -- * Smart constructor
    mkClusterTimeline,

    -- * Lenses
    ctCreationDateTime,
    ctEndDateTime,
    ctReadyDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the timeline of the cluster's lifecycle.
--
-- /See:/ 'mkClusterTimeline' smart constructor.
data ClusterTimeline = ClusterTimeline'
  { -- | The creation date and time of the cluster.
    creationDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time when the cluster was terminated.
    endDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time when the cluster was ready to run steps.
    readyDateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ClusterTimeline' value with any optional fields omitted.
mkClusterTimeline ::
  ClusterTimeline
mkClusterTimeline =
  ClusterTimeline'
    { creationDateTime = Core.Nothing,
      endDateTime = Core.Nothing,
      readyDateTime = Core.Nothing
    }

-- | The creation date and time of the cluster.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctCreationDateTime :: Lens.Lens' ClusterTimeline (Core.Maybe Core.NominalDiffTime)
ctCreationDateTime = Lens.field @"creationDateTime"
{-# DEPRECATED ctCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The date and time when the cluster was terminated.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctEndDateTime :: Lens.Lens' ClusterTimeline (Core.Maybe Core.NominalDiffTime)
ctEndDateTime = Lens.field @"endDateTime"
{-# DEPRECATED ctEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

-- | The date and time when the cluster was ready to run steps.
--
-- /Note:/ Consider using 'readyDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctReadyDateTime :: Lens.Lens' ClusterTimeline (Core.Maybe Core.NominalDiffTime)
ctReadyDateTime = Lens.field @"readyDateTime"
{-# DEPRECATED ctReadyDateTime "Use generic-lens or generic-optics with 'readyDateTime' instead." #-}

instance Core.FromJSON ClusterTimeline where
  parseJSON =
    Core.withObject "ClusterTimeline" Core.$
      \x ->
        ClusterTimeline'
          Core.<$> (x Core..:? "CreationDateTime")
          Core.<*> (x Core..:? "EndDateTime")
          Core.<*> (x Core..:? "ReadyDateTime")
