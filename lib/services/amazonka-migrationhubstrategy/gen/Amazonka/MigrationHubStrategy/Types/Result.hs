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
-- Module      : Amazonka.MigrationHubStrategy.Types.Result
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.Result where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AnalysisStatusUnion
import Amazonka.MigrationHubStrategy.Types.AnalysisType
import Amazonka.MigrationHubStrategy.Types.AntipatternReportResult
import qualified Amazonka.Prelude as Prelude

-- | The error in server analysis.
--
-- /See:/ 'newResult' smart constructor.
data Result = Result'
  { -- | The error in server analysis.
    analysisStatus :: Prelude.Maybe AnalysisStatusUnion,
    -- | The error in server analysis.
    analysisType :: Prelude.Maybe AnalysisType,
    -- | The error in server analysis.
    antipatternReportResultList :: Prelude.Maybe [AntipatternReportResult],
    -- | The error in server analysis.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Result' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisStatus', 'result_analysisStatus' - The error in server analysis.
--
-- 'analysisType', 'result_analysisType' - The error in server analysis.
--
-- 'antipatternReportResultList', 'result_antipatternReportResultList' - The error in server analysis.
--
-- 'statusMessage', 'result_statusMessage' - The error in server analysis.
newResult ::
  Result
newResult =
  Result'
    { analysisStatus = Prelude.Nothing,
      analysisType = Prelude.Nothing,
      antipatternReportResultList = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The error in server analysis.
result_analysisStatus :: Lens.Lens' Result (Prelude.Maybe AnalysisStatusUnion)
result_analysisStatus = Lens.lens (\Result' {analysisStatus} -> analysisStatus) (\s@Result' {} a -> s {analysisStatus = a} :: Result)

-- | The error in server analysis.
result_analysisType :: Lens.Lens' Result (Prelude.Maybe AnalysisType)
result_analysisType = Lens.lens (\Result' {analysisType} -> analysisType) (\s@Result' {} a -> s {analysisType = a} :: Result)

-- | The error in server analysis.
result_antipatternReportResultList :: Lens.Lens' Result (Prelude.Maybe [AntipatternReportResult])
result_antipatternReportResultList = Lens.lens (\Result' {antipatternReportResultList} -> antipatternReportResultList) (\s@Result' {} a -> s {antipatternReportResultList = a} :: Result) Prelude.. Lens.mapping Lens.coerced

-- | The error in server analysis.
result_statusMessage :: Lens.Lens' Result (Prelude.Maybe Prelude.Text)
result_statusMessage = Lens.lens (\Result' {statusMessage} -> statusMessage) (\s@Result' {} a -> s {statusMessage = a} :: Result)

instance Data.FromJSON Result where
  parseJSON =
    Data.withObject
      "Result"
      ( \x ->
          Result'
            Prelude.<$> (x Data..:? "analysisStatus")
            Prelude.<*> (x Data..:? "analysisType")
            Prelude.<*> ( x
                            Data..:? "antipatternReportResultList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "statusMessage")
      )

instance Prelude.Hashable Result where
  hashWithSalt _salt Result' {..} =
    _salt
      `Prelude.hashWithSalt` analysisStatus
      `Prelude.hashWithSalt` analysisType
      `Prelude.hashWithSalt` antipatternReportResultList
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData Result where
  rnf Result' {..} =
    Prelude.rnf analysisStatus
      `Prelude.seq` Prelude.rnf analysisType
      `Prelude.seq` Prelude.rnf antipatternReportResultList
      `Prelude.seq` Prelude.rnf statusMessage
