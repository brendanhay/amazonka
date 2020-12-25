{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentSummary
  ( DatasetContentSummary (..),

    -- * Smart constructor
    mkDatasetContentSummary,

    -- * Lenses
    dcsCompletionTime,
    dcsCreationTime,
    dcsScheduleTime,
    dcsStatus,
    dcsVersion,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.DatasetContentStatus as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetContentVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary information about dataset contents.
--
-- /See:/ 'mkDatasetContentSummary' smart constructor.
data DatasetContentSummary = DatasetContentSummary'
  { -- | The time the dataset content status was updated to SUCCEEDED or FAILED.
    completionTime :: Core.Maybe Core.NominalDiffTime,
    -- | The actual time the creation of the dataset contents was started.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time the creation of the dataset contents was scheduled to start.
    scheduleTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the data set contents.
    status :: Core.Maybe Types.DatasetContentStatus,
    -- | The version of the dataset contents.
    version :: Core.Maybe Types.DatasetContentVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DatasetContentSummary' value with any optional fields omitted.
mkDatasetContentSummary ::
  DatasetContentSummary
mkDatasetContentSummary =
  DatasetContentSummary'
    { completionTime = Core.Nothing,
      creationTime = Core.Nothing,
      scheduleTime = Core.Nothing,
      status = Core.Nothing,
      version = Core.Nothing
    }

-- | The time the dataset content status was updated to SUCCEEDED or FAILED.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCompletionTime :: Lens.Lens' DatasetContentSummary (Core.Maybe Core.NominalDiffTime)
dcsCompletionTime = Lens.field @"completionTime"
{-# DEPRECATED dcsCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | The actual time the creation of the dataset contents was started.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCreationTime :: Lens.Lens' DatasetContentSummary (Core.Maybe Core.NominalDiffTime)
dcsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dcsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The time the creation of the dataset contents was scheduled to start.
--
-- /Note:/ Consider using 'scheduleTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsScheduleTime :: Lens.Lens' DatasetContentSummary (Core.Maybe Core.NominalDiffTime)
dcsScheduleTime = Lens.field @"scheduleTime"
{-# DEPRECATED dcsScheduleTime "Use generic-lens or generic-optics with 'scheduleTime' instead." #-}

-- | The status of the data set contents.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStatus :: Lens.Lens' DatasetContentSummary (Core.Maybe Types.DatasetContentStatus)
dcsStatus = Lens.field @"status"
{-# DEPRECATED dcsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version of the dataset contents.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsVersion :: Lens.Lens' DatasetContentSummary (Core.Maybe Types.DatasetContentVersion)
dcsVersion = Lens.field @"version"
{-# DEPRECATED dcsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON DatasetContentSummary where
  parseJSON =
    Core.withObject "DatasetContentSummary" Core.$
      \x ->
        DatasetContentSummary'
          Core.<$> (x Core..:? "completionTime")
          Core.<*> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "scheduleTime")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "version")
