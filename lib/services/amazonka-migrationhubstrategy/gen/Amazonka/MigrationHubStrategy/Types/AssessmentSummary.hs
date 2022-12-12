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
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentStatusSummary
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentSummary
import Amazonka.MigrationHubStrategy.Types.S3Object
import Amazonka.MigrationHubStrategy.Types.ServerStatusSummary
import Amazonka.MigrationHubStrategy.Types.ServerSummary
import Amazonka.MigrationHubStrategy.Types.StrategySummary
import qualified Amazonka.Prelude as Prelude

-- | Contains the summary of the assessment results.
--
-- /See:/ 'newAssessmentSummary' smart constructor.
data AssessmentSummary = AssessmentSummary'
  { -- | The Amazon S3 object containing the anti-pattern report.
    antipatternReportS3Object :: Prelude.Maybe S3Object,
    -- | The status of the anti-pattern report.
    antipatternReportStatus :: Prelude.Maybe AntipatternReportStatus,
    -- | The status message of the anti-pattern report.
    antipatternReportStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The time the assessment was performed.
    lastAnalyzedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | List of AntipatternSeveritySummary.
    listAntipatternSeveritySummary :: Prelude.Maybe [AntipatternSeveritySummary],
    -- | List of status summaries of the analyzed application components.
    listApplicationComponentStatusSummary :: Prelude.Maybe [ApplicationComponentStatusSummary],
    -- | List of ApplicationComponentStrategySummary.
    listApplicationComponentStrategySummary :: Prelude.Maybe [StrategySummary],
    -- | List of ApplicationComponentSummary.
    listApplicationComponentSummary :: Prelude.Maybe [ApplicationComponentSummary],
    -- | List of status summaries of the analyzed servers.
    listServerStatusSummary :: Prelude.Maybe [ServerStatusSummary],
    -- | List of ServerStrategySummary.
    listServerStrategySummary :: Prelude.Maybe [StrategySummary],
    -- | List of ServerSummary.
    listServerSummary :: Prelude.Maybe [ServerSummary]
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
-- 'antipatternReportS3Object', 'assessmentSummary_antipatternReportS3Object' - The Amazon S3 object containing the anti-pattern report.
--
-- 'antipatternReportStatus', 'assessmentSummary_antipatternReportStatus' - The status of the anti-pattern report.
--
-- 'antipatternReportStatusMessage', 'assessmentSummary_antipatternReportStatusMessage' - The status message of the anti-pattern report.
--
-- 'lastAnalyzedTimestamp', 'assessmentSummary_lastAnalyzedTimestamp' - The time the assessment was performed.
--
-- 'listAntipatternSeveritySummary', 'assessmentSummary_listAntipatternSeveritySummary' - List of AntipatternSeveritySummary.
--
-- 'listApplicationComponentStatusSummary', 'assessmentSummary_listApplicationComponentStatusSummary' - List of status summaries of the analyzed application components.
--
-- 'listApplicationComponentStrategySummary', 'assessmentSummary_listApplicationComponentStrategySummary' - List of ApplicationComponentStrategySummary.
--
-- 'listApplicationComponentSummary', 'assessmentSummary_listApplicationComponentSummary' - List of ApplicationComponentSummary.
--
-- 'listServerStatusSummary', 'assessmentSummary_listServerStatusSummary' - List of status summaries of the analyzed servers.
--
-- 'listServerStrategySummary', 'assessmentSummary_listServerStrategySummary' - List of ServerStrategySummary.
--
-- 'listServerSummary', 'assessmentSummary_listServerSummary' - List of ServerSummary.
newAssessmentSummary ::
  AssessmentSummary
newAssessmentSummary =
  AssessmentSummary'
    { antipatternReportS3Object =
        Prelude.Nothing,
      antipatternReportStatus = Prelude.Nothing,
      antipatternReportStatusMessage = Prelude.Nothing,
      lastAnalyzedTimestamp = Prelude.Nothing,
      listAntipatternSeveritySummary = Prelude.Nothing,
      listApplicationComponentStatusSummary =
        Prelude.Nothing,
      listApplicationComponentStrategySummary =
        Prelude.Nothing,
      listApplicationComponentSummary = Prelude.Nothing,
      listServerStatusSummary = Prelude.Nothing,
      listServerStrategySummary = Prelude.Nothing,
      listServerSummary = Prelude.Nothing
    }

-- | The Amazon S3 object containing the anti-pattern report.
assessmentSummary_antipatternReportS3Object :: Lens.Lens' AssessmentSummary (Prelude.Maybe S3Object)
assessmentSummary_antipatternReportS3Object = Lens.lens (\AssessmentSummary' {antipatternReportS3Object} -> antipatternReportS3Object) (\s@AssessmentSummary' {} a -> s {antipatternReportS3Object = a} :: AssessmentSummary)

-- | The status of the anti-pattern report.
assessmentSummary_antipatternReportStatus :: Lens.Lens' AssessmentSummary (Prelude.Maybe AntipatternReportStatus)
assessmentSummary_antipatternReportStatus = Lens.lens (\AssessmentSummary' {antipatternReportStatus} -> antipatternReportStatus) (\s@AssessmentSummary' {} a -> s {antipatternReportStatus = a} :: AssessmentSummary)

-- | The status message of the anti-pattern report.
assessmentSummary_antipatternReportStatusMessage :: Lens.Lens' AssessmentSummary (Prelude.Maybe Prelude.Text)
assessmentSummary_antipatternReportStatusMessage = Lens.lens (\AssessmentSummary' {antipatternReportStatusMessage} -> antipatternReportStatusMessage) (\s@AssessmentSummary' {} a -> s {antipatternReportStatusMessage = a} :: AssessmentSummary)

-- | The time the assessment was performed.
assessmentSummary_lastAnalyzedTimestamp :: Lens.Lens' AssessmentSummary (Prelude.Maybe Prelude.UTCTime)
assessmentSummary_lastAnalyzedTimestamp = Lens.lens (\AssessmentSummary' {lastAnalyzedTimestamp} -> lastAnalyzedTimestamp) (\s@AssessmentSummary' {} a -> s {lastAnalyzedTimestamp = a} :: AssessmentSummary) Prelude.. Lens.mapping Data._Time

-- | List of AntipatternSeveritySummary.
assessmentSummary_listAntipatternSeveritySummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [AntipatternSeveritySummary])
assessmentSummary_listAntipatternSeveritySummary = Lens.lens (\AssessmentSummary' {listAntipatternSeveritySummary} -> listAntipatternSeveritySummary) (\s@AssessmentSummary' {} a -> s {listAntipatternSeveritySummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of status summaries of the analyzed application components.
assessmentSummary_listApplicationComponentStatusSummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [ApplicationComponentStatusSummary])
assessmentSummary_listApplicationComponentStatusSummary = Lens.lens (\AssessmentSummary' {listApplicationComponentStatusSummary} -> listApplicationComponentStatusSummary) (\s@AssessmentSummary' {} a -> s {listApplicationComponentStatusSummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of ApplicationComponentStrategySummary.
assessmentSummary_listApplicationComponentStrategySummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [StrategySummary])
assessmentSummary_listApplicationComponentStrategySummary = Lens.lens (\AssessmentSummary' {listApplicationComponentStrategySummary} -> listApplicationComponentStrategySummary) (\s@AssessmentSummary' {} a -> s {listApplicationComponentStrategySummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of ApplicationComponentSummary.
assessmentSummary_listApplicationComponentSummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [ApplicationComponentSummary])
assessmentSummary_listApplicationComponentSummary = Lens.lens (\AssessmentSummary' {listApplicationComponentSummary} -> listApplicationComponentSummary) (\s@AssessmentSummary' {} a -> s {listApplicationComponentSummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of status summaries of the analyzed servers.
assessmentSummary_listServerStatusSummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [ServerStatusSummary])
assessmentSummary_listServerStatusSummary = Lens.lens (\AssessmentSummary' {listServerStatusSummary} -> listServerStatusSummary) (\s@AssessmentSummary' {} a -> s {listServerStatusSummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of ServerStrategySummary.
assessmentSummary_listServerStrategySummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [StrategySummary])
assessmentSummary_listServerStrategySummary = Lens.lens (\AssessmentSummary' {listServerStrategySummary} -> listServerStrategySummary) (\s@AssessmentSummary' {} a -> s {listServerStrategySummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | List of ServerSummary.
assessmentSummary_listServerSummary :: Lens.Lens' AssessmentSummary (Prelude.Maybe [ServerSummary])
assessmentSummary_listServerSummary = Lens.lens (\AssessmentSummary' {listServerSummary} -> listServerSummary) (\s@AssessmentSummary' {} a -> s {listServerSummary = a} :: AssessmentSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AssessmentSummary where
  parseJSON =
    Data.withObject
      "AssessmentSummary"
      ( \x ->
          AssessmentSummary'
            Prelude.<$> (x Data..:? "antipatternReportS3Object")
            Prelude.<*> (x Data..:? "antipatternReportStatus")
            Prelude.<*> (x Data..:? "antipatternReportStatusMessage")
            Prelude.<*> (x Data..:? "lastAnalyzedTimestamp")
            Prelude.<*> ( x Data..:? "listAntipatternSeveritySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listApplicationComponentStatusSummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listApplicationComponentStrategySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listApplicationComponentSummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listServerStatusSummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listServerStrategySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "listServerSummary"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AssessmentSummary where
  hashWithSalt _salt AssessmentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` antipatternReportS3Object
      `Prelude.hashWithSalt` antipatternReportStatus
      `Prelude.hashWithSalt` antipatternReportStatusMessage
      `Prelude.hashWithSalt` lastAnalyzedTimestamp
      `Prelude.hashWithSalt` listAntipatternSeveritySummary
      `Prelude.hashWithSalt` listApplicationComponentStatusSummary
      `Prelude.hashWithSalt` listApplicationComponentStrategySummary
      `Prelude.hashWithSalt` listApplicationComponentSummary
      `Prelude.hashWithSalt` listServerStatusSummary
      `Prelude.hashWithSalt` listServerStrategySummary
      `Prelude.hashWithSalt` listServerSummary

instance Prelude.NFData AssessmentSummary where
  rnf AssessmentSummary' {..} =
    Prelude.rnf antipatternReportS3Object
      `Prelude.seq` Prelude.rnf antipatternReportStatus
      `Prelude.seq` Prelude.rnf antipatternReportStatusMessage
      `Prelude.seq` Prelude.rnf lastAnalyzedTimestamp
      `Prelude.seq` Prelude.rnf listAntipatternSeveritySummary
      `Prelude.seq` Prelude.rnf listApplicationComponentStatusSummary
      `Prelude.seq` Prelude.rnf listApplicationComponentStrategySummary
      `Prelude.seq` Prelude.rnf listApplicationComponentSummary
      `Prelude.seq` Prelude.rnf listServerStatusSummary
      `Prelude.seq` Prelude.rnf listServerStrategySummary
      `Prelude.seq` Prelude.rnf listServerSummary
