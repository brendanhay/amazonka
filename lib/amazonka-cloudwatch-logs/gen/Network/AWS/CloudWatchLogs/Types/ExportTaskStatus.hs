{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
  ( ExportTaskStatus (..)
  -- * Smart constructor
  , mkExportTaskStatus
  -- * Lenses
  , etsCode
  , etsMessage
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode as Types
import qualified Network.AWS.CloudWatchLogs.Types.ExportTaskStatusMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the status of an export task.
--
-- /See:/ 'mkExportTaskStatus' smart constructor.
data ExportTaskStatus = ExportTaskStatus'
  { code :: Core.Maybe Types.ExportTaskStatusCode
    -- ^ The status code of the export task.
  , message :: Core.Maybe Types.ExportTaskStatusMessage
    -- ^ The status message related to the status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTaskStatus' value with any optional fields omitted.
mkExportTaskStatus
    :: ExportTaskStatus
mkExportTaskStatus
  = ExportTaskStatus'{code = Core.Nothing, message = Core.Nothing}

-- | The status code of the export task.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etsCode :: Lens.Lens' ExportTaskStatus (Core.Maybe Types.ExportTaskStatusCode)
etsCode = Lens.field @"code"
{-# INLINEABLE etsCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The status message related to the status code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etsMessage :: Lens.Lens' ExportTaskStatus (Core.Maybe Types.ExportTaskStatusMessage)
etsMessage = Lens.field @"message"
{-# INLINEABLE etsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON ExportTaskStatus where
        parseJSON
          = Core.withObject "ExportTaskStatus" Core.$
              \ x ->
                ExportTaskStatus' Core.<$>
                  (x Core..:? "code") Core.<*> x Core..:? "message"
