{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportSummary
  ( ExportSummary (..),

    -- * Smart constructor
    mkExportSummary,

    -- * Lenses
    esExportArn,
    esExportStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types.ExportArn as Types
import qualified Network.AWS.DynamoDB.Types.ExportStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary information about an export task.
--
-- /See:/ 'mkExportSummary' smart constructor.
data ExportSummary = ExportSummary'
  { -- | The Amazon Resource Name (ARN) of the export.
    exportArn :: Core.Maybe Types.ExportArn,
    -- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
    exportStatus :: Core.Maybe Types.ExportStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportSummary' value with any optional fields omitted.
mkExportSummary ::
  ExportSummary
mkExportSummary =
  ExportSummary'
    { exportArn = Core.Nothing,
      exportStatus = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the export.
--
-- /Note:/ Consider using 'exportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExportArn :: Lens.Lens' ExportSummary (Core.Maybe Types.ExportArn)
esExportArn = Lens.field @"exportArn"
{-# DEPRECATED esExportArn "Use generic-lens or generic-optics with 'exportArn' instead." #-}

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
--
-- /Note:/ Consider using 'exportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExportStatus :: Lens.Lens' ExportSummary (Core.Maybe Types.ExportStatus)
esExportStatus = Lens.field @"exportStatus"
{-# DEPRECATED esExportStatus "Use generic-lens or generic-optics with 'exportStatus' instead." #-}

instance Core.FromJSON ExportSummary where
  parseJSON =
    Core.withObject "ExportSummary" Core.$
      \x ->
        ExportSummary'
          Core.<$> (x Core..:? "ExportArn") Core.<*> (x Core..:? "ExportStatus")
