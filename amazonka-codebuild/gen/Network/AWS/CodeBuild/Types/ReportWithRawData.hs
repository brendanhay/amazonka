{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the unmodified data for the report. For more information, see .
--
-- /See:/ 'newReportWithRawData' smart constructor.
data ReportWithRawData = ReportWithRawData'
  { -- | The ARN of the report.
    reportArn :: Prelude.Maybe Prelude.Text,
    -- | The value of the requested data field from the report.
    data' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { reportArn = Prelude.Nothing,
      data' = Prelude.Nothing
    }

-- | The ARN of the report.
reportWithRawData_reportArn :: Lens.Lens' ReportWithRawData (Prelude.Maybe Prelude.Text)
reportWithRawData_reportArn = Lens.lens (\ReportWithRawData' {reportArn} -> reportArn) (\s@ReportWithRawData' {} a -> s {reportArn = a} :: ReportWithRawData)

-- | The value of the requested data field from the report.
reportWithRawData_data :: Lens.Lens' ReportWithRawData (Prelude.Maybe Prelude.Text)
reportWithRawData_data = Lens.lens (\ReportWithRawData' {data'} -> data') (\s@ReportWithRawData' {} a -> s {data' = a} :: ReportWithRawData)

instance Prelude.FromJSON ReportWithRawData where
  parseJSON =
    Prelude.withObject
      "ReportWithRawData"
      ( \x ->
          ReportWithRawData'
            Prelude.<$> (x Prelude..:? "reportArn")
            Prelude.<*> (x Prelude..:? "data")
      )

instance Prelude.Hashable ReportWithRawData

instance Prelude.NFData ReportWithRawData
