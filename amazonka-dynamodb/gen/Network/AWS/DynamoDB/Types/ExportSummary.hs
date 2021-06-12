{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ExportStatus
import qualified Network.AWS.Lens as Lens

-- | Summary information about an export task.
--
-- /See:/ 'newExportSummary' smart constructor.
data ExportSummary = ExportSummary'
  { -- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
    -- FAILED.
    exportStatus :: Core.Maybe ExportStatus,
    -- | The Amazon Resource Name (ARN) of the export.
    exportArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportStatus', 'exportSummary_exportStatus' - Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
-- FAILED.
--
-- 'exportArn', 'exportSummary_exportArn' - The Amazon Resource Name (ARN) of the export.
newExportSummary ::
  ExportSummary
newExportSummary =
  ExportSummary'
    { exportStatus = Core.Nothing,
      exportArn = Core.Nothing
    }

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
-- FAILED.
exportSummary_exportStatus :: Lens.Lens' ExportSummary (Core.Maybe ExportStatus)
exportSummary_exportStatus = Lens.lens (\ExportSummary' {exportStatus} -> exportStatus) (\s@ExportSummary' {} a -> s {exportStatus = a} :: ExportSummary)

-- | The Amazon Resource Name (ARN) of the export.
exportSummary_exportArn :: Lens.Lens' ExportSummary (Core.Maybe Core.Text)
exportSummary_exportArn = Lens.lens (\ExportSummary' {exportArn} -> exportArn) (\s@ExportSummary' {} a -> s {exportArn = a} :: ExportSummary)

instance Core.FromJSON ExportSummary where
  parseJSON =
    Core.withObject
      "ExportSummary"
      ( \x ->
          ExportSummary'
            Core.<$> (x Core..:? "ExportStatus")
            Core.<*> (x Core..:? "ExportArn")
      )

instance Core.Hashable ExportSummary

instance Core.NFData ExportSummary
