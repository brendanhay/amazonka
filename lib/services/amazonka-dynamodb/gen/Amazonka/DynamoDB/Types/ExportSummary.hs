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
-- Module      : Amazonka.DynamoDB.Types.ExportSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ExportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ExportStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Summary information about an export task.
--
-- /See:/ 'newExportSummary' smart constructor.
data ExportSummary = ExportSummary'
  { -- | The Amazon Resource Name (ARN) of the export.
    exportArn :: Prelude.Maybe Prelude.Text,
    -- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
    -- FAILED.
    exportStatus :: Prelude.Maybe ExportStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportArn', 'exportSummary_exportArn' - The Amazon Resource Name (ARN) of the export.
--
-- 'exportStatus', 'exportSummary_exportStatus' - Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
-- FAILED.
newExportSummary ::
  ExportSummary
newExportSummary =
  ExportSummary'
    { exportArn = Prelude.Nothing,
      exportStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the export.
exportSummary_exportArn :: Lens.Lens' ExportSummary (Prelude.Maybe Prelude.Text)
exportSummary_exportArn = Lens.lens (\ExportSummary' {exportArn} -> exportArn) (\s@ExportSummary' {} a -> s {exportArn = a} :: ExportSummary)

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
-- FAILED.
exportSummary_exportStatus :: Lens.Lens' ExportSummary (Prelude.Maybe ExportStatus)
exportSummary_exportStatus = Lens.lens (\ExportSummary' {exportStatus} -> exportStatus) (\s@ExportSummary' {} a -> s {exportStatus = a} :: ExportSummary)

instance Core.FromJSON ExportSummary where
  parseJSON =
    Core.withObject
      "ExportSummary"
      ( \x ->
          ExportSummary'
            Prelude.<$> (x Core..:? "ExportArn")
            Prelude.<*> (x Core..:? "ExportStatus")
      )

instance Prelude.Hashable ExportSummary where
  hashWithSalt _salt ExportSummary' {..} =
    _salt `Prelude.hashWithSalt` exportArn
      `Prelude.hashWithSalt` exportStatus

instance Prelude.NFData ExportSummary where
  rnf ExportSummary' {..} =
    Prelude.rnf exportArn
      `Prelude.seq` Prelude.rnf exportStatus
