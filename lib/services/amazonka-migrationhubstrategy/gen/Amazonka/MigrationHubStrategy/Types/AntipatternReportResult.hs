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
-- Module      : Amazonka.MigrationHubStrategy.Types.AntipatternReportResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AntipatternReportResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AnalyzerNameUnion
import Amazonka.MigrationHubStrategy.Types.AntipatternReportStatus
import Amazonka.MigrationHubStrategy.Types.S3Object
import qualified Amazonka.Prelude as Prelude

-- | The anti-pattern report result.
--
-- /See:/ 'newAntipatternReportResult' smart constructor.
data AntipatternReportResult = AntipatternReportResult'
  { -- | The analyzer name.
    analyzerName :: Prelude.Maybe AnalyzerNameUnion,
    antiPatternReportS3Object :: Prelude.Maybe S3Object,
    -- | The status of the anti-pattern report generation.
    antipatternReportStatus :: Prelude.Maybe AntipatternReportStatus,
    -- | The status message for the anti-pattern.
    antipatternReportStatusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AntipatternReportResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzerName', 'antipatternReportResult_analyzerName' - The analyzer name.
--
-- 'antiPatternReportS3Object', 'antipatternReportResult_antiPatternReportS3Object' - Undocumented member.
--
-- 'antipatternReportStatus', 'antipatternReportResult_antipatternReportStatus' - The status of the anti-pattern report generation.
--
-- 'antipatternReportStatusMessage', 'antipatternReportResult_antipatternReportStatusMessage' - The status message for the anti-pattern.
newAntipatternReportResult ::
  AntipatternReportResult
newAntipatternReportResult =
  AntipatternReportResult'
    { analyzerName =
        Prelude.Nothing,
      antiPatternReportS3Object = Prelude.Nothing,
      antipatternReportStatus = Prelude.Nothing,
      antipatternReportStatusMessage = Prelude.Nothing
    }

-- | The analyzer name.
antipatternReportResult_analyzerName :: Lens.Lens' AntipatternReportResult (Prelude.Maybe AnalyzerNameUnion)
antipatternReportResult_analyzerName = Lens.lens (\AntipatternReportResult' {analyzerName} -> analyzerName) (\s@AntipatternReportResult' {} a -> s {analyzerName = a} :: AntipatternReportResult)

-- | Undocumented member.
antipatternReportResult_antiPatternReportS3Object :: Lens.Lens' AntipatternReportResult (Prelude.Maybe S3Object)
antipatternReportResult_antiPatternReportS3Object = Lens.lens (\AntipatternReportResult' {antiPatternReportS3Object} -> antiPatternReportS3Object) (\s@AntipatternReportResult' {} a -> s {antiPatternReportS3Object = a} :: AntipatternReportResult)

-- | The status of the anti-pattern report generation.
antipatternReportResult_antipatternReportStatus :: Lens.Lens' AntipatternReportResult (Prelude.Maybe AntipatternReportStatus)
antipatternReportResult_antipatternReportStatus = Lens.lens (\AntipatternReportResult' {antipatternReportStatus} -> antipatternReportStatus) (\s@AntipatternReportResult' {} a -> s {antipatternReportStatus = a} :: AntipatternReportResult)

-- | The status message for the anti-pattern.
antipatternReportResult_antipatternReportStatusMessage :: Lens.Lens' AntipatternReportResult (Prelude.Maybe Prelude.Text)
antipatternReportResult_antipatternReportStatusMessage = Lens.lens (\AntipatternReportResult' {antipatternReportStatusMessage} -> antipatternReportStatusMessage) (\s@AntipatternReportResult' {} a -> s {antipatternReportStatusMessage = a} :: AntipatternReportResult)

instance Data.FromJSON AntipatternReportResult where
  parseJSON =
    Data.withObject
      "AntipatternReportResult"
      ( \x ->
          AntipatternReportResult'
            Prelude.<$> (x Data..:? "analyzerName")
            Prelude.<*> (x Data..:? "antiPatternReportS3Object")
            Prelude.<*> (x Data..:? "antipatternReportStatus")
            Prelude.<*> (x Data..:? "antipatternReportStatusMessage")
      )

instance Prelude.Hashable AntipatternReportResult where
  hashWithSalt _salt AntipatternReportResult' {..} =
    _salt
      `Prelude.hashWithSalt` analyzerName
      `Prelude.hashWithSalt` antiPatternReportS3Object
      `Prelude.hashWithSalt` antipatternReportStatus
      `Prelude.hashWithSalt` antipatternReportStatusMessage

instance Prelude.NFData AntipatternReportResult where
  rnf AntipatternReportResult' {..} =
    Prelude.rnf analyzerName
      `Prelude.seq` Prelude.rnf antiPatternReportS3Object
      `Prelude.seq` Prelude.rnf antipatternReportStatus
      `Prelude.seq` Prelude.rnf antipatternReportStatusMessage
