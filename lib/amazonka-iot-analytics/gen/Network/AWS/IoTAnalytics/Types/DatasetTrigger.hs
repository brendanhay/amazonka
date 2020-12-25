{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetTrigger
  ( DatasetTrigger (..),

    -- * Smart constructor
    mkDatasetTrigger,

    -- * Lenses
    dtDataset,
    dtSchedule,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.Schedule as Types
import qualified Network.AWS.IoTAnalytics.Types.TriggeringDataset as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @DatasetTrigger@ that specifies when the data set is automatically updated.
--
-- /See:/ 'mkDatasetTrigger' smart constructor.
data DatasetTrigger = DatasetTrigger'
  { -- | The data set whose content creation triggers the creation of this data set's contents.
    dataset :: Core.Maybe Types.TriggeringDataset,
    -- | The Schedule when the trigger is initiated.
    schedule :: Core.Maybe Types.Schedule
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatasetTrigger' value with any optional fields omitted.
mkDatasetTrigger ::
  DatasetTrigger
mkDatasetTrigger =
  DatasetTrigger' {dataset = Core.Nothing, schedule = Core.Nothing}

-- | The data set whose content creation triggers the creation of this data set's contents.
--
-- /Note:/ Consider using 'dataset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDataset :: Lens.Lens' DatasetTrigger (Core.Maybe Types.TriggeringDataset)
dtDataset = Lens.field @"dataset"
{-# DEPRECATED dtDataset "Use generic-lens or generic-optics with 'dataset' instead." #-}

-- | The Schedule when the trigger is initiated.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtSchedule :: Lens.Lens' DatasetTrigger (Core.Maybe Types.Schedule)
dtSchedule = Lens.field @"schedule"
{-# DEPRECATED dtSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

instance Core.FromJSON DatasetTrigger where
  toJSON DatasetTrigger {..} =
    Core.object
      ( Core.catMaybes
          [ ("dataset" Core..=) Core.<$> dataset,
            ("schedule" Core..=) Core.<$> schedule
          ]
      )

instance Core.FromJSON DatasetTrigger where
  parseJSON =
    Core.withObject "DatasetTrigger" Core.$
      \x ->
        DatasetTrigger'
          Core.<$> (x Core..:? "dataset") Core.<*> (x Core..:? "schedule")
