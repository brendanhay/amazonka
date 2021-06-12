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
-- Module      : Network.AWS.CodeBuild.Types.ReportWithRawData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportWithRawData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the unmodified data for the report. For more information, see .
--
-- /See:/ 'newReportWithRawData' smart constructor.
data ReportWithRawData = ReportWithRawData'
  { -- | The ARN of the report.
    reportArn :: Core.Maybe Core.Text,
    -- | The value of the requested data field from the report.
    data' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReportWithRawData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportArn', 'reportWithRawData_reportArn' - The ARN of the report.
--
-- 'data'', 'reportWithRawData_data' - The value of the requested data field from the report.
newReportWithRawData ::
  ReportWithRawData
newReportWithRawData =
  ReportWithRawData'
    { reportArn = Core.Nothing,
      data' = Core.Nothing
    }

-- | The ARN of the report.
reportWithRawData_reportArn :: Lens.Lens' ReportWithRawData (Core.Maybe Core.Text)
reportWithRawData_reportArn = Lens.lens (\ReportWithRawData' {reportArn} -> reportArn) (\s@ReportWithRawData' {} a -> s {reportArn = a} :: ReportWithRawData)

-- | The value of the requested data field from the report.
reportWithRawData_data :: Lens.Lens' ReportWithRawData (Core.Maybe Core.Text)
reportWithRawData_data = Lens.lens (\ReportWithRawData' {data'} -> data') (\s@ReportWithRawData' {} a -> s {data' = a} :: ReportWithRawData)

instance Core.FromJSON ReportWithRawData where
  parseJSON =
    Core.withObject
      "ReportWithRawData"
      ( \x ->
          ReportWithRawData'
            Core.<$> (x Core..:? "reportArn")
            Core.<*> (x Core..:? "data")
      )

instance Core.Hashable ReportWithRawData

instance Core.NFData ReportWithRawData
