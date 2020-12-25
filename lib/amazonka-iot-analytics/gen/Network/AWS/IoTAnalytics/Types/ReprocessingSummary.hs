{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ReprocessingSummary
  ( ReprocessingSummary (..),

    -- * Smart constructor
    mkReprocessingSummary,

    -- * Lenses
    rsCreationTime,
    rsId,
    rsStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.ReprocessingId as Types
import qualified Network.AWS.IoTAnalytics.Types.ReprocessingStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about pipeline reprocessing.
--
-- /See:/ 'mkReprocessingSummary' smart constructor.
data ReprocessingSummary = ReprocessingSummary'
  { -- | The time the pipeline reprocessing was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The @reprocessingId@ returned by @StartPipelineReprocessing@ .
    id :: Core.Maybe Types.ReprocessingId,
    -- | The status of the pipeline reprocessing.
    status :: Core.Maybe Types.ReprocessingStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReprocessingSummary' value with any optional fields omitted.
mkReprocessingSummary ::
  ReprocessingSummary
mkReprocessingSummary =
  ReprocessingSummary'
    { creationTime = Core.Nothing,
      id = Core.Nothing,
      status = Core.Nothing
    }

-- | The time the pipeline reprocessing was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsCreationTime :: Lens.Lens' ReprocessingSummary (Core.Maybe Core.NominalDiffTime)
rsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED rsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The @reprocessingId@ returned by @StartPipelineReprocessing@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsId :: Lens.Lens' ReprocessingSummary (Core.Maybe Types.ReprocessingId)
rsId = Lens.field @"id"
{-# DEPRECATED rsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The status of the pipeline reprocessing.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsStatus :: Lens.Lens' ReprocessingSummary (Core.Maybe Types.ReprocessingStatus)
rsStatus = Lens.field @"status"
{-# DEPRECATED rsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ReprocessingSummary where
  parseJSON =
    Core.withObject "ReprocessingSummary" Core.$
      \x ->
        ReprocessingSummary'
          Core.<$> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "status")
