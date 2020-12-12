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
    eteiCreationTime,
    eteiCompletionTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the status of an export task.
--
-- /See:/ 'mkExportTaskExecutionInfo' smart constructor.
data ExportTaskExecutionInfo = ExportTaskExecutionInfo'
  { creationTime ::
      Lude.Maybe Lude.Natural,
    completionTime :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTaskExecutionInfo' with the minimum fields required to make a request.
--
-- * 'completionTime' - The completion time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'creationTime' - The creation time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
mkExportTaskExecutionInfo ::
  ExportTaskExecutionInfo
mkExportTaskExecutionInfo =
  ExportTaskExecutionInfo'
    { creationTime = Lude.Nothing,
      completionTime = Lude.Nothing
    }

-- | The creation time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eteiCreationTime :: Lens.Lens' ExportTaskExecutionInfo (Lude.Maybe Lude.Natural)
eteiCreationTime = Lens.lens (creationTime :: ExportTaskExecutionInfo -> Lude.Maybe Lude.Natural) (\s a -> s {creationTime = a} :: ExportTaskExecutionInfo)
{-# DEPRECATED eteiCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The completion time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eteiCompletionTime :: Lens.Lens' ExportTaskExecutionInfo (Lude.Maybe Lude.Natural)
eteiCompletionTime = Lens.lens (completionTime :: ExportTaskExecutionInfo -> Lude.Maybe Lude.Natural) (\s a -> s {completionTime = a} :: ExportTaskExecutionInfo)
{-# DEPRECATED eteiCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

instance Lude.FromJSON ExportTaskExecutionInfo where
  parseJSON =
    Lude.withObject
      "ExportTaskExecutionInfo"
      ( \x ->
          ExportTaskExecutionInfo'
            Lude.<$> (x Lude..:? "creationTime") Lude.<*> (x Lude..:? "completionTime")
      )
