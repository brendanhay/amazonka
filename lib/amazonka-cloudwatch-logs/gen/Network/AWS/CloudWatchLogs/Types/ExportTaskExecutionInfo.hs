{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
  ( ExportTaskExecutionInfo (..),

    -- * Smart constructor
    mkExportTaskExecutionInfo,

    -- * Lenses
    eteiCompletionTime,
    eteiCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the status of an export task.
--
-- /See:/ 'mkExportTaskExecutionInfo' smart constructor.
data ExportTaskExecutionInfo = ExportTaskExecutionInfo'
  { -- | The completion time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    completionTime :: Core.Maybe Core.Natural,
    -- | The creation time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTaskExecutionInfo' value with any optional fields omitted.
mkExportTaskExecutionInfo ::
  ExportTaskExecutionInfo
mkExportTaskExecutionInfo =
  ExportTaskExecutionInfo'
    { completionTime = Core.Nothing,
      creationTime = Core.Nothing
    }

-- | The completion time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eteiCompletionTime :: Lens.Lens' ExportTaskExecutionInfo (Core.Maybe Core.Natural)
eteiCompletionTime = Lens.field @"completionTime"
{-# DEPRECATED eteiCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | The creation time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eteiCreationTime :: Lens.Lens' ExportTaskExecutionInfo (Core.Maybe Core.Natural)
eteiCreationTime = Lens.field @"creationTime"
{-# DEPRECATED eteiCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Core.FromJSON ExportTaskExecutionInfo where
  parseJSON =
    Core.withObject "ExportTaskExecutionInfo" Core.$
      \x ->
        ExportTaskExecutionInfo'
          Core.<$> (x Core..:? "completionTime") Core.<*> (x Core..:? "creationTime")
