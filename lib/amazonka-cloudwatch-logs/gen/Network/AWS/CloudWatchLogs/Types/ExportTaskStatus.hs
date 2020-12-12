{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
  ( ExportTaskStatus (..),

    -- * Smart constructor
    mkExportTaskStatus,

    -- * Lenses
    etsCode,
    etsMessage,
  )
where

import Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the status of an export task.
--
-- /See:/ 'mkExportTaskStatus' smart constructor.
data ExportTaskStatus = ExportTaskStatus'
  { code ::
      Lude.Maybe ExportTaskStatusCode,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTaskStatus' with the minimum fields required to make a request.
--
-- * 'code' - The status code of the export task.
-- * 'message' - The status message related to the status code.
mkExportTaskStatus ::
  ExportTaskStatus
mkExportTaskStatus =
  ExportTaskStatus' {code = Lude.Nothing, message = Lude.Nothing}

-- | The status code of the export task.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etsCode :: Lens.Lens' ExportTaskStatus (Lude.Maybe ExportTaskStatusCode)
etsCode = Lens.lens (code :: ExportTaskStatus -> Lude.Maybe ExportTaskStatusCode) (\s a -> s {code = a} :: ExportTaskStatus)
{-# DEPRECATED etsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The status message related to the status code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etsMessage :: Lens.Lens' ExportTaskStatus (Lude.Maybe Lude.Text)
etsMessage = Lens.lens (message :: ExportTaskStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ExportTaskStatus)
{-# DEPRECATED etsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ExportTaskStatus where
  parseJSON =
    Lude.withObject
      "ExportTaskStatus"
      ( \x ->
          ExportTaskStatus'
            Lude.<$> (x Lude..:? "code") Lude.<*> (x Lude..:? "message")
      )
