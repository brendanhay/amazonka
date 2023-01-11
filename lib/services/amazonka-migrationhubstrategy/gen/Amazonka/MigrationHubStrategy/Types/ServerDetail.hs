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
-- Module      : Amazonka.MigrationHubStrategy.Types.ServerDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AntipatternReportStatus
import Amazonka.MigrationHubStrategy.Types.AntipatternSeveritySummary
import Amazonka.MigrationHubStrategy.Types.RecommendationSet
import Amazonka.MigrationHubStrategy.Types.RunTimeAssessmentStatus
import Amazonka.MigrationHubStrategy.Types.S3Object
import Amazonka.MigrationHubStrategy.Types.ServerError
import Amazonka.MigrationHubStrategy.Types.StrategySummary
import Amazonka.MigrationHubStrategy.Types.SystemInfo
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a server.
--
-- /See:/ 'newServerDetail' smart constructor.
data ServerDetail = ServerDetail'
  { -- | The S3 bucket name and Amazon S3 key name for anti-pattern report.
    antipatternReportS3Object :: Prelude.Maybe S3Object,
    -- | The status of the anti-pattern report generation.
    antipatternReportStatus :: Prelude.Maybe AntipatternReportStatus,
    -- | A message about the status of the anti-pattern report generation.
    antipatternReportStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | A list of strategy summaries.
    applicationComponentStrategySummary :: Prelude.Maybe [StrategySummary],
    -- | The status of assessment for the server.
    dataCollectionStatus :: Prelude.Maybe RunTimeAssessmentStatus,
    -- | The server ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the server was assessed.
    lastAnalyzedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of anti-pattern severity summaries.
    listAntipatternSeveritySummary :: Prelude.Maybe [AntipatternSeveritySummary],
    -- | The name of the server.
    name :: Prelude.Maybe Prelude.Text,
    -- | A set of recommendations.
    recommendationSet :: Prelude.Maybe RecommendationSet,
    -- | The error in server analysis.
    serverError :: Prelude.Maybe ServerError,
    -- | The type of server.
    serverType :: Prelude.Maybe Prelude.Text,
    -- | A message about the status of data collection, which contains detailed
    -- descriptions of any error messages.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | System information about the server.
    systemInfo :: Prelude.Maybe SystemInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'antipatternReportS3Object', 'serverDetail_antipatternReportS3Object' - The S3 bucket name and Amazon S3 key name for anti-pattern report.
--
-- 'antipatternReportStatus', 'serverDetail_antipatternReportStatus' - The status of the anti-pattern report generation.
--
-- 'antipatternReportStatusMessage', 'serverDetail_antipatternReportStatusMessage' - A message about the status of the anti-pattern report generation.
--
-- 'applicationComponentStrategySummary', 'serverDetail_applicationComponentStrategySummary' - A list of strategy summaries.
--
-- 'dataCollectionStatus', 'serverDetail_dataCollectionStatus' - The status of assessment for the server.
--
-- 'id', 'serverDetail_id' - The server ID.
--
-- 'lastAnalyzedTimestamp', 'serverDetail_lastAnalyzedTimestamp' - The timestamp of when the server was assessed.
--
-- 'listAntipatternSeveritySummary', 'serverDetail_listAntipatternSeveritySummary' - A list of anti-pattern severity summaries.
--
-- 'name', 'serverDetail_name' - The name of the server.
--
-- 'recommendationSet', 'serverDetail_recommendationSet' - A set of recommendations.
--
-- 'serverError', 'serverDetail_serverError' - The error in server analysis.
--
-- 'serverType', 'serverDetail_serverType' - The type of server.
--
-- 'statusMessage', 'serverDetail_statusMessage' - A message about the status of data collection, which contains detailed
-- descriptions of any error messages.
--
-- 'systemInfo', 'serverDetail_systemInfo' - System information about the server.
newServerDetail ::
  ServerDetail
newServerDetail =
  ServerDetail'
    { antipatternReportS3Object =
        Prelude.Nothing,
      antipatternReportStatus = Prelude.Nothing,
      antipatternReportStatusMessage = Prelude.Nothing,
      applicationComponentStrategySummary =
        Prelude.Nothing,
      dataCollectionStatus = Prelude.Nothing,
      id = Prelude.Nothing,
      lastAnalyzedTimestamp = Prelude.Nothing,
      listAntipatternSeveritySummary = Prelude.Nothing,
      name = Prelude.Nothing,
      recommendationSet = Prelude.Nothing,
      serverError = Prelude.Nothing,
      serverType = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      systemInfo = Prelude.Nothing
    }

-- | The S3 bucket name and Amazon S3 key name for anti-pattern report.
serverDetail_antipatternReportS3Object :: Lens.Lens' ServerDetail (Prelude.Maybe S3Object)
serverDetail_antipatternReportS3Object = Lens.lens (\ServerDetail' {antipatternReportS3Object} -> antipatternReportS3Object) (\s@ServerDetail' {} a -> s {antipatternReportS3Object = a} :: ServerDetail)

-- | The status of the anti-pattern report generation.
serverDetail_antipatternReportStatus :: Lens.Lens' ServerDetail (Prelude.Maybe AntipatternReportStatus)
serverDetail_antipatternReportStatus = Lens.lens (\ServerDetail' {antipatternReportStatus} -> antipatternReportStatus) (\s@ServerDetail' {} a -> s {antipatternReportStatus = a} :: ServerDetail)

-- | A message about the status of the anti-pattern report generation.
serverDetail_antipatternReportStatusMessage :: Lens.Lens' ServerDetail (Prelude.Maybe Prelude.Text)
serverDetail_antipatternReportStatusMessage = Lens.lens (\ServerDetail' {antipatternReportStatusMessage} -> antipatternReportStatusMessage) (\s@ServerDetail' {} a -> s {antipatternReportStatusMessage = a} :: ServerDetail)

-- | A list of strategy summaries.
serverDetail_applicationComponentStrategySummary :: Lens.Lens' ServerDetail (Prelude.Maybe [StrategySummary])
serverDetail_applicationComponentStrategySummary = Lens.lens (\ServerDetail' {applicationComponentStrategySummary} -> applicationComponentStrategySummary) (\s@ServerDetail' {} a -> s {applicationComponentStrategySummary = a} :: ServerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The status of assessment for the server.
serverDetail_dataCollectionStatus :: Lens.Lens' ServerDetail (Prelude.Maybe RunTimeAssessmentStatus)
serverDetail_dataCollectionStatus = Lens.lens (\ServerDetail' {dataCollectionStatus} -> dataCollectionStatus) (\s@ServerDetail' {} a -> s {dataCollectionStatus = a} :: ServerDetail)

-- | The server ID.
serverDetail_id :: Lens.Lens' ServerDetail (Prelude.Maybe Prelude.Text)
serverDetail_id = Lens.lens (\ServerDetail' {id} -> id) (\s@ServerDetail' {} a -> s {id = a} :: ServerDetail)

-- | The timestamp of when the server was assessed.
serverDetail_lastAnalyzedTimestamp :: Lens.Lens' ServerDetail (Prelude.Maybe Prelude.UTCTime)
serverDetail_lastAnalyzedTimestamp = Lens.lens (\ServerDetail' {lastAnalyzedTimestamp} -> lastAnalyzedTimestamp) (\s@ServerDetail' {} a -> s {lastAnalyzedTimestamp = a} :: ServerDetail) Prelude.. Lens.mapping Data._Time

-- | A list of anti-pattern severity summaries.
serverDetail_listAntipatternSeveritySummary :: Lens.Lens' ServerDetail (Prelude.Maybe [AntipatternSeveritySummary])
serverDetail_listAntipatternSeveritySummary = Lens.lens (\ServerDetail' {listAntipatternSeveritySummary} -> listAntipatternSeveritySummary) (\s@ServerDetail' {} a -> s {listAntipatternSeveritySummary = a} :: ServerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the server.
serverDetail_name :: Lens.Lens' ServerDetail (Prelude.Maybe Prelude.Text)
serverDetail_name = Lens.lens (\ServerDetail' {name} -> name) (\s@ServerDetail' {} a -> s {name = a} :: ServerDetail)

-- | A set of recommendations.
serverDetail_recommendationSet :: Lens.Lens' ServerDetail (Prelude.Maybe RecommendationSet)
serverDetail_recommendationSet = Lens.lens (\ServerDetail' {recommendationSet} -> recommendationSet) (\s@ServerDetail' {} a -> s {recommendationSet = a} :: ServerDetail)

-- | The error in server analysis.
serverDetail_serverError :: Lens.Lens' ServerDetail (Prelude.Maybe ServerError)
serverDetail_serverError = Lens.lens (\ServerDetail' {serverError} -> serverError) (\s@ServerDetail' {} a -> s {serverError = a} :: ServerDetail)

-- | The type of server.
serverDetail_serverType :: Lens.Lens' ServerDetail (Prelude.Maybe Prelude.Text)
serverDetail_serverType = Lens.lens (\ServerDetail' {serverType} -> serverType) (\s@ServerDetail' {} a -> s {serverType = a} :: ServerDetail)

-- | A message about the status of data collection, which contains detailed
-- descriptions of any error messages.
serverDetail_statusMessage :: Lens.Lens' ServerDetail (Prelude.Maybe Prelude.Text)
serverDetail_statusMessage = Lens.lens (\ServerDetail' {statusMessage} -> statusMessage) (\s@ServerDetail' {} a -> s {statusMessage = a} :: ServerDetail)

-- | System information about the server.
serverDetail_systemInfo :: Lens.Lens' ServerDetail (Prelude.Maybe SystemInfo)
serverDetail_systemInfo = Lens.lens (\ServerDetail' {systemInfo} -> systemInfo) (\s@ServerDetail' {} a -> s {systemInfo = a} :: ServerDetail)

instance Data.FromJSON ServerDetail where
  parseJSON =
    Data.withObject
      "ServerDetail"
      ( \x ->
          ServerDetail'
            Prelude.<$> (x Data..:? "antipatternReportS3Object")
            Prelude.<*> (x Data..:? "antipatternReportStatus")
            Prelude.<*> (x Data..:? "antipatternReportStatusMessage")
            Prelude.<*> ( x Data..:? "applicationComponentStrategySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "dataCollectionStatus")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastAnalyzedTimestamp")
            Prelude.<*> ( x Data..:? "listAntipatternSeveritySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "recommendationSet")
            Prelude.<*> (x Data..:? "serverError")
            Prelude.<*> (x Data..:? "serverType")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "systemInfo")
      )

instance Prelude.Hashable ServerDetail where
  hashWithSalt _salt ServerDetail' {..} =
    _salt
      `Prelude.hashWithSalt` antipatternReportS3Object
      `Prelude.hashWithSalt` antipatternReportStatus
      `Prelude.hashWithSalt` antipatternReportStatusMessage
      `Prelude.hashWithSalt` applicationComponentStrategySummary
      `Prelude.hashWithSalt` dataCollectionStatus
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastAnalyzedTimestamp
      `Prelude.hashWithSalt` listAntipatternSeveritySummary
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recommendationSet
      `Prelude.hashWithSalt` serverError
      `Prelude.hashWithSalt` serverType
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` systemInfo

instance Prelude.NFData ServerDetail where
  rnf ServerDetail' {..} =
    Prelude.rnf antipatternReportS3Object
      `Prelude.seq` Prelude.rnf antipatternReportStatus
      `Prelude.seq` Prelude.rnf antipatternReportStatusMessage
      `Prelude.seq` Prelude.rnf applicationComponentStrategySummary
      `Prelude.seq` Prelude.rnf dataCollectionStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastAnalyzedTimestamp
      `Prelude.seq` Prelude.rnf listAntipatternSeveritySummary
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recommendationSet
      `Prelude.seq` Prelude.rnf serverError
      `Prelude.seq` Prelude.rnf serverType
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf systemInfo
