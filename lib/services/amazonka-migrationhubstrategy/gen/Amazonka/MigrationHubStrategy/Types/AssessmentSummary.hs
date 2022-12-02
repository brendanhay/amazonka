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
-- Module      : Amazonka.MigrationHubStrategy.Types.AssessmentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AssessmentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AntipatternReportStatus
import Amazonka.MigrationHubStrategy.Types.AntipatternSeveritySummary
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentSummary
import Amazonka.MigrationHubStrategy.Types.S3Object
import Amazonka.MigrationHubStrategy.Types.ServerSummary
import Amazonka.MigrationHubStrategy.Types.StrategySummary
import qualified Amazonka.Prelude as Prelude

-- | Contains the summary of the assessment results.
--
-- /See:/ 'newAssessmentSummary' smart constructor.
data AssessmentSummary = AssessmentSummary'
  { -- | The status of the anti-pattern report.
    antipatternReportStatus :: Prelude.Maybe AntipatternReportStatus,
    -- | The Amazon S3 object containing the anti-pattern report.
    antipatternReportS3Object :: Prelude.Maybe S3Object,
    -- | List of ApplicationComponentSummary.
    listApplicationComponentSummary :: Prelude.Maybe [ApplicationComponentSummary],
    -- | List of AntipatternSeveritySummary.
    listAntipatternSeveritySummary :: Prelude.Maybe [AntipatternSeveritySummary],
    -- | List of ServerStrategySummary.
    listServerStrategySummary :: Prelude.Maybe [StrategySummary],
    -- | The status message of the anti-pattern report.
    antipatternReportStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | List of ServerSummary.
    listServerSummary :: Prelude.Maybe [ServerSummary],
    -- | List of ApplicationComponentStrategySummary.
    listApplicationComponentStrategySummary :: Prelude.Maybe [StrategySummary],
    -- | The time the assessment was performed.
    lastAnalyzedTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'antipatternReportStatus', 'assessmentSummary_antipatternReportStatus' - The status of the anti-pattern report.
--
-- 'antipatternReportS3Object', 'assessmentSummary_antipatternReportS3Object' - The Amazon S3 object containing the anti-pattern report.
--
-- 'listApplicationComponentSummary', 'assessmentSummary_listApplicationComponentSummary' - List of ApplicationComponentSummary.
--
-- 'listAntipatternSeveritySummary', 'assessmentSummary_listAntipatternSeveritySummary' - List of AntipatternSeveritySummary.
--
-- 'listServerStrategySummary', 'assessmentSummary_listServerStrategySummary' - List of ServerStrategySummary.
--
-- 'antipatternReportStatusMessage', 'assessmentSummary_antipatternReportStatusMessage' - The status message of the anti-pattern report.
--
-- 'listServerSummary', 'assessmentSummary_listServerSummary' - List of ServerSummary.
--
-- 'listApplicationComponentStrategySummary', 'assessmentSummary_listApplicationComponentStrategySummary' - List of ApplicationComponentStrategySummary.
--
-- 'lastAnalyzedTimestamp', 'assessmentSummary_lastAnalyzedTimestamp' - The time the assessment was performed.
newAssessmentSummary ::
  AssessmentSummary
newAssessmentSummary =
  AssessmentSummary'
    { antipatternReportStatus =
        Prelude.Nothing,
      antipatternReportS3Object = Prelude.Nothing,
      listApplicationComponentSummary = Prelude.Nothing,
      listAntipatternSeveritySummary = Prelude.Nothing,
      listServerStrategySummary = Prelude.Nothing,
      antipatternReportStatusMessage = Prelude.Nothing,
      listServerSummary = Prelude.Nothing,
      listApplicationComponentStrategySummary =
        Prelude.Nothing,
      lastAnalyzedTimestamp = Prelude.Nothing
    }

-- | The status of the anti-pattern report.
assessmentSummary_antipatternReportStatus :: Lens.Lens' AssessmentSummary (Prelude.Maybe AntipatternReportStatus)
assessmentSummary_antipatternReportStatus = Lens.lens (\AssessmentSummary' {antipatternReportStatus} -> antipatternReportStatus) (\s@AssessmentSummary' {} a -> s {antipatternReportStatus = a} :: AssessmentSummary)

-- | The Amazon S3 object containing the anti-pattern report.
assessmentSummary_antipatternReportS3Object :: Lens.Lens' AssessmentSummary (Prelude.Maybe S3Object)
assessmentSummary_antipatternReportS3Object = Lens.lens (\AssessmentSummary' {antipatternReportS3Object} -> antipatternReportS3Object) (\s@AssessmentSummary' {} a -> s {antipatternReportS3Object = a} :: AssessmentSummary)

-- | List of ApplicationComponentSummary.
assessmentSummary_listApplicationComponentSummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [ApplicationComponentSummary])
assessmentSummary_listApplicationComponentSummary = Lens.lens (\AssessmentSummary' {listApplicationComponentSummary} -> listApplicationComponentSummary) (\s@AssessmentSummary' {} a -> s {listApplicationComponentSummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of AntipatternSeveritySummary.
assessmentSummary_listAntipatternSeveritySummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [AntipatternSeveritySummary])
assessmentSummary_listAntipatternSeveritySummary = Lens.lens (\AssessmentSummary' {listAntipatternSeveritySummary} -> listAntipatternSeveritySummary) (\s@AssessmentSummary' {} a -> s {listAntipatternSeveritySummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of ServerStrategySummary.
assessmentSummary_listServerStrategySummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [StrategySummary])
assessmentSummary_listServerStrategySummary = Lens.lens (\AssessmentSummary' {listServerStrategySummary} -> listServerStrategySummary) (\s@AssessmentSummary' {} a -> s {listServerStrategySummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | The status message of the anti-pattern report.
assessmentSummary_antipatternReportStatusMessage :: Lens.Lens' AssessmentSummary (Prelude.Maybe Prelude.Text)
assessmentSummary_antipatternReportStatusMessage = Lens.lens (\AssessmentSummary' {antipatternReportStatusMessage} -> antipatternReportStatusMessage) (\s@AssessmentSummary' {} a -> s {antipatternReportStatusMessage = a} :: AssessmentSummary)

-- | List of ServerSummary.
assessmentSummary_listServerSummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [ServerSummary])
assessmentSummary_listServerSummary = Lens.lens (\AssessmentSummary' {listServerSummary} -> listServerSummary) (\s@AssessmentSummary' {} a -> s {listServerSummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of ApplicationComponentStrategySummary.
assessmentSummary_listApplicationComponentStrategySummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [StrategySummary])
assessmentSummary_listApplicationComponentStrategySummary = Lens.lens (\AssessmentSummary' {listApplicationComponentStrategySummary} -> listApplicationComponentStrategySummary) (\s@AssessmentSummary' {} a -> s {listApplicationComponentStrategySummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | The time the assessment was performed.
assessmentSummary_lastAnalyzedTimestamp :: Lens.Lens' AssessmentSummary (Prelude.Maybe Prelude.UTCTime)
assessmentSummary_lastAnalyzedTimestamp = Lens.lens (\AssessmentSummary' {lastAnalyzedTimestamp} -> lastAnalyzedTimestamp) (\s@AssessmentSummary' {} a -> s {lastAnalyzedTimestamp = a} :: AssessmentSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AssessmentSummary where
  parseJSON =
    Data.withObject
      "AssessmentSummary"
      ( \x ->
          AssessmentSummary'
            Prelude.<$> (x Data..:? "antipatternReportStatus")
            Prelude.<*> (x Data..:? "antipatternReportS3Object")
            Prelude.<*> ( x Data..:? "listApplicationComponentSummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listAntipatternSeveritySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listServerStrategySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "antipatternReportStatusMessage")
            Prelude.<*> ( x Data..:? "listServerSummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listApplicationComponentStrategySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "lastAnalyzedTimestamp")
      )

instance Prelude.Hashable AssessmentSummary where
  hashWithSalt _salt AssessmentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` antipatternReportStatus
      `Prelude.hashWithSalt` antipatternReportS3Object
      `Prelude.hashWithSalt` listApplicationComponentSummary
      `Prelude.hashWithSalt` listAntipatternSeveritySummary
      `Prelude.hashWithSalt` listServerStrategySummary
      `Prelude.hashWithSalt` antipatternReportStatusMessage
      `Prelude.hashWithSalt` listServerSummary
      `Prelude.hashWithSalt` listApplicationComponentStrategySummary
      `Prelude.hashWithSalt` lastAnalyzedTimestamp

instance Prelude.NFData AssessmentSummary where
  rnf AssessmentSummary' {..} =
    Prelude.rnf antipatternReportStatus
      `Prelude.seq` Prelude.rnf antipatternReportS3Object
      `Prelude.seq` Prelude.rnf listApplicationComponentSummary
      `Prelude.seq` Prelude.rnf listAntipatternSeveritySummary
      `Prelude.seq` Prelude.rnf listServerStrategySummary
      `Prelude.seq` Prelude.rnf antipatternReportStatusMessage
      `Prelude.seq` Prelude.rnf listServerSummary
      `Prelude.seq` Prelude.rnf listApplicationComponentStrategySummary
      `Prelude.seq` Prelude.rnf lastAnalyzedTimestamp
