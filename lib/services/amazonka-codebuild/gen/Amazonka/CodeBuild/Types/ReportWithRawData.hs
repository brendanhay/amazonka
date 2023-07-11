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
-- Module      : Amazonka.CodeBuild.Types.ReportWithRawData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportWithRawData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the unmodified data for the report. For more information, see .
--
-- /See:/ 'newReportWithRawData' smart constructor.
data ReportWithRawData = ReportWithRawData'
  { -- | The value of the requested data field from the report.
    data' :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the report.
    reportArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportWithRawData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'reportWithRawData_data' - The value of the requested data field from the report.
--
-- 'reportArn', 'reportWithRawData_reportArn' - The ARN of the report.
newReportWithRawData ::
  ReportWithRawData
newReportWithRawData =
  ReportWithRawData'
    { data' = Prelude.Nothing,
      reportArn = Prelude.Nothing
    }

-- | The value of the requested data field from the report.
reportWithRawData_data :: Lens.Lens' ReportWithRawData (Prelude.Maybe Prelude.Text)
reportWithRawData_data = Lens.lens (\ReportWithRawData' {data'} -> data') (\s@ReportWithRawData' {} a -> s {data' = a} :: ReportWithRawData)

-- | The ARN of the report.
reportWithRawData_reportArn :: Lens.Lens' ReportWithRawData (Prelude.Maybe Prelude.Text)
reportWithRawData_reportArn = Lens.lens (\ReportWithRawData' {reportArn} -> reportArn) (\s@ReportWithRawData' {} a -> s {reportArn = a} :: ReportWithRawData)

instance Data.FromJSON ReportWithRawData where
  parseJSON =
    Data.withObject
      "ReportWithRawData"
      ( \x ->
          ReportWithRawData'
            Prelude.<$> (x Data..:? "data")
            Prelude.<*> (x Data..:? "reportArn")
      )

instance Prelude.Hashable ReportWithRawData where
  hashWithSalt _salt ReportWithRawData' {..} =
    _salt
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` reportArn

instance Prelude.NFData ReportWithRawData where
  rnf ReportWithRawData' {..} =
    Prelude.rnf data'
      `Prelude.seq` Prelude.rnf reportArn
