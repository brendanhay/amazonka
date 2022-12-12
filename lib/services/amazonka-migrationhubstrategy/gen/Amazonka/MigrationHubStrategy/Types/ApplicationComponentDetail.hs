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
-- Module      : Amazonka.MigrationHubStrategy.Types.ApplicationComponentDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ApplicationComponentDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AntipatternReportStatus
import Amazonka.MigrationHubStrategy.Types.AntipatternSeveritySummary
import Amazonka.MigrationHubStrategy.Types.AppType
import Amazonka.MigrationHubStrategy.Types.AppUnitError
import Amazonka.MigrationHubStrategy.Types.DatabaseConfigDetail
import Amazonka.MigrationHubStrategy.Types.InclusionStatus
import Amazonka.MigrationHubStrategy.Types.RecommendationSet
import Amazonka.MigrationHubStrategy.Types.ResourceSubType
import Amazonka.MigrationHubStrategy.Types.RuntimeAnalysisStatus
import Amazonka.MigrationHubStrategy.Types.S3Object
import Amazonka.MigrationHubStrategy.Types.SourceCodeRepository
import Amazonka.MigrationHubStrategy.Types.SrcCodeOrDbAnalysisStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about an application component.
--
-- /See:/ 'newApplicationComponentDetail' smart constructor.
data ApplicationComponentDetail = ApplicationComponentDetail'
  { -- | The status of analysis, if the application component has source code or
    -- an associated database.
    analysisStatus :: Prelude.Maybe SrcCodeOrDbAnalysisStatus,
    -- | The S3 bucket name and the Amazon S3 key name for the anti-pattern
    -- report.
    antipatternReportS3Object :: Prelude.Maybe S3Object,
    -- | The status of the anti-pattern report generation.
    antipatternReportStatus :: Prelude.Maybe AntipatternReportStatus,
    -- | The status message for the anti-pattern.
    antipatternReportStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The type of application component.
    appType :: Prelude.Maybe AppType,
    -- | The error in the analysis of the source code or database.
    appUnitError :: Prelude.Maybe AppUnitError,
    -- | The ID of the server that the application component is running on.
    associatedServerId :: Prelude.Maybe Prelude.Text,
    -- | Configuration details for the database associated with the application
    -- component.
    databaseConfigDetail :: Prelude.Maybe DatabaseConfigDetail,
    -- | The ID of the application component.
    id :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the application component has been included for server
    -- recommendation or not.
    inclusionStatus :: Prelude.Maybe InclusionStatus,
    -- | The timestamp of when the application component was assessed.
    lastAnalyzedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of anti-pattern severity summaries.
    listAntipatternSeveritySummary :: Prelude.Maybe [AntipatternSeveritySummary],
    -- | Set to true if the application component is running on multiple servers.
    moreServerAssociationExists :: Prelude.Maybe Prelude.Bool,
    -- | The name of application component.
    name :: Prelude.Maybe Prelude.Text,
    -- | OS driver.
    osDriver :: Prelude.Maybe Prelude.Text,
    -- | OS version.
    osVersion :: Prelude.Maybe Prelude.Text,
    -- | The top recommendation set for the application component.
    recommendationSet :: Prelude.Maybe RecommendationSet,
    -- | The application component subtype.
    resourceSubType :: Prelude.Maybe ResourceSubType,
    -- | The status of the application unit.
    runtimeStatus :: Prelude.Maybe RuntimeAnalysisStatus,
    -- | The status message for the application unit.
    runtimeStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | Details about the source code repository associated with the application
    -- component.
    sourceCodeRepositories :: Prelude.Maybe [SourceCodeRepository],
    -- | A detailed description of the analysis status and any failure message.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationComponentDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisStatus', 'applicationComponentDetail_analysisStatus' - The status of analysis, if the application component has source code or
-- an associated database.
--
-- 'antipatternReportS3Object', 'applicationComponentDetail_antipatternReportS3Object' - The S3 bucket name and the Amazon S3 key name for the anti-pattern
-- report.
--
-- 'antipatternReportStatus', 'applicationComponentDetail_antipatternReportStatus' - The status of the anti-pattern report generation.
--
-- 'antipatternReportStatusMessage', 'applicationComponentDetail_antipatternReportStatusMessage' - The status message for the anti-pattern.
--
-- 'appType', 'applicationComponentDetail_appType' - The type of application component.
--
-- 'appUnitError', 'applicationComponentDetail_appUnitError' - The error in the analysis of the source code or database.
--
-- 'associatedServerId', 'applicationComponentDetail_associatedServerId' - The ID of the server that the application component is running on.
--
-- 'databaseConfigDetail', 'applicationComponentDetail_databaseConfigDetail' - Configuration details for the database associated with the application
-- component.
--
-- 'id', 'applicationComponentDetail_id' - The ID of the application component.
--
-- 'inclusionStatus', 'applicationComponentDetail_inclusionStatus' - Indicates whether the application component has been included for server
-- recommendation or not.
--
-- 'lastAnalyzedTimestamp', 'applicationComponentDetail_lastAnalyzedTimestamp' - The timestamp of when the application component was assessed.
--
-- 'listAntipatternSeveritySummary', 'applicationComponentDetail_listAntipatternSeveritySummary' - A list of anti-pattern severity summaries.
--
-- 'moreServerAssociationExists', 'applicationComponentDetail_moreServerAssociationExists' - Set to true if the application component is running on multiple servers.
--
-- 'name', 'applicationComponentDetail_name' - The name of application component.
--
-- 'osDriver', 'applicationComponentDetail_osDriver' - OS driver.
--
-- 'osVersion', 'applicationComponentDetail_osVersion' - OS version.
--
-- 'recommendationSet', 'applicationComponentDetail_recommendationSet' - The top recommendation set for the application component.
--
-- 'resourceSubType', 'applicationComponentDetail_resourceSubType' - The application component subtype.
--
-- 'runtimeStatus', 'applicationComponentDetail_runtimeStatus' - The status of the application unit.
--
-- 'runtimeStatusMessage', 'applicationComponentDetail_runtimeStatusMessage' - The status message for the application unit.
--
-- 'sourceCodeRepositories', 'applicationComponentDetail_sourceCodeRepositories' - Details about the source code repository associated with the application
-- component.
--
-- 'statusMessage', 'applicationComponentDetail_statusMessage' - A detailed description of the analysis status and any failure message.
newApplicationComponentDetail ::
  ApplicationComponentDetail
newApplicationComponentDetail =
  ApplicationComponentDetail'
    { analysisStatus =
        Prelude.Nothing,
      antipatternReportS3Object = Prelude.Nothing,
      antipatternReportStatus = Prelude.Nothing,
      antipatternReportStatusMessage =
        Prelude.Nothing,
      appType = Prelude.Nothing,
      appUnitError = Prelude.Nothing,
      associatedServerId = Prelude.Nothing,
      databaseConfigDetail = Prelude.Nothing,
      id = Prelude.Nothing,
      inclusionStatus = Prelude.Nothing,
      lastAnalyzedTimestamp = Prelude.Nothing,
      listAntipatternSeveritySummary =
        Prelude.Nothing,
      moreServerAssociationExists = Prelude.Nothing,
      name = Prelude.Nothing,
      osDriver = Prelude.Nothing,
      osVersion = Prelude.Nothing,
      recommendationSet = Prelude.Nothing,
      resourceSubType = Prelude.Nothing,
      runtimeStatus = Prelude.Nothing,
      runtimeStatusMessage = Prelude.Nothing,
      sourceCodeRepositories = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The status of analysis, if the application component has source code or
-- an associated database.
applicationComponentDetail_analysisStatus :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe SrcCodeOrDbAnalysisStatus)
applicationComponentDetail_analysisStatus = Lens.lens (\ApplicationComponentDetail' {analysisStatus} -> analysisStatus) (\s@ApplicationComponentDetail' {} a -> s {analysisStatus = a} :: ApplicationComponentDetail)

-- | The S3 bucket name and the Amazon S3 key name for the anti-pattern
-- report.
applicationComponentDetail_antipatternReportS3Object :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe S3Object)
applicationComponentDetail_antipatternReportS3Object = Lens.lens (\ApplicationComponentDetail' {antipatternReportS3Object} -> antipatternReportS3Object) (\s@ApplicationComponentDetail' {} a -> s {antipatternReportS3Object = a} :: ApplicationComponentDetail)

-- | The status of the anti-pattern report generation.
applicationComponentDetail_antipatternReportStatus :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe AntipatternReportStatus)
applicationComponentDetail_antipatternReportStatus = Lens.lens (\ApplicationComponentDetail' {antipatternReportStatus} -> antipatternReportStatus) (\s@ApplicationComponentDetail' {} a -> s {antipatternReportStatus = a} :: ApplicationComponentDetail)

-- | The status message for the anti-pattern.
applicationComponentDetail_antipatternReportStatusMessage :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_antipatternReportStatusMessage = Lens.lens (\ApplicationComponentDetail' {antipatternReportStatusMessage} -> antipatternReportStatusMessage) (\s@ApplicationComponentDetail' {} a -> s {antipatternReportStatusMessage = a} :: ApplicationComponentDetail)

-- | The type of application component.
applicationComponentDetail_appType :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe AppType)
applicationComponentDetail_appType = Lens.lens (\ApplicationComponentDetail' {appType} -> appType) (\s@ApplicationComponentDetail' {} a -> s {appType = a} :: ApplicationComponentDetail)

-- | The error in the analysis of the source code or database.
applicationComponentDetail_appUnitError :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe AppUnitError)
applicationComponentDetail_appUnitError = Lens.lens (\ApplicationComponentDetail' {appUnitError} -> appUnitError) (\s@ApplicationComponentDetail' {} a -> s {appUnitError = a} :: ApplicationComponentDetail)

-- | The ID of the server that the application component is running on.
applicationComponentDetail_associatedServerId :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_associatedServerId = Lens.lens (\ApplicationComponentDetail' {associatedServerId} -> associatedServerId) (\s@ApplicationComponentDetail' {} a -> s {associatedServerId = a} :: ApplicationComponentDetail)

-- | Configuration details for the database associated with the application
-- component.
applicationComponentDetail_databaseConfigDetail :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe DatabaseConfigDetail)
applicationComponentDetail_databaseConfigDetail = Lens.lens (\ApplicationComponentDetail' {databaseConfigDetail} -> databaseConfigDetail) (\s@ApplicationComponentDetail' {} a -> s {databaseConfigDetail = a} :: ApplicationComponentDetail)

-- | The ID of the application component.
applicationComponentDetail_id :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_id = Lens.lens (\ApplicationComponentDetail' {id} -> id) (\s@ApplicationComponentDetail' {} a -> s {id = a} :: ApplicationComponentDetail)

-- | Indicates whether the application component has been included for server
-- recommendation or not.
applicationComponentDetail_inclusionStatus :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe InclusionStatus)
applicationComponentDetail_inclusionStatus = Lens.lens (\ApplicationComponentDetail' {inclusionStatus} -> inclusionStatus) (\s@ApplicationComponentDetail' {} a -> s {inclusionStatus = a} :: ApplicationComponentDetail)

-- | The timestamp of when the application component was assessed.
applicationComponentDetail_lastAnalyzedTimestamp :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.UTCTime)
applicationComponentDetail_lastAnalyzedTimestamp = Lens.lens (\ApplicationComponentDetail' {lastAnalyzedTimestamp} -> lastAnalyzedTimestamp) (\s@ApplicationComponentDetail' {} a -> s {lastAnalyzedTimestamp = a} :: ApplicationComponentDetail) Prelude.. Lens.mapping Data._Time

-- | A list of anti-pattern severity summaries.
applicationComponentDetail_listAntipatternSeveritySummary :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe [AntipatternSeveritySummary])
applicationComponentDetail_listAntipatternSeveritySummary = Lens.lens (\ApplicationComponentDetail' {listAntipatternSeveritySummary} -> listAntipatternSeveritySummary) (\s@ApplicationComponentDetail' {} a -> s {listAntipatternSeveritySummary = a} :: ApplicationComponentDetail) Prelude.. Lens.mapping Lens.coerced

-- | Set to true if the application component is running on multiple servers.
applicationComponentDetail_moreServerAssociationExists :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Bool)
applicationComponentDetail_moreServerAssociationExists = Lens.lens (\ApplicationComponentDetail' {moreServerAssociationExists} -> moreServerAssociationExists) (\s@ApplicationComponentDetail' {} a -> s {moreServerAssociationExists = a} :: ApplicationComponentDetail)

-- | The name of application component.
applicationComponentDetail_name :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_name = Lens.lens (\ApplicationComponentDetail' {name} -> name) (\s@ApplicationComponentDetail' {} a -> s {name = a} :: ApplicationComponentDetail)

-- | OS driver.
applicationComponentDetail_osDriver :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_osDriver = Lens.lens (\ApplicationComponentDetail' {osDriver} -> osDriver) (\s@ApplicationComponentDetail' {} a -> s {osDriver = a} :: ApplicationComponentDetail)

-- | OS version.
applicationComponentDetail_osVersion :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_osVersion = Lens.lens (\ApplicationComponentDetail' {osVersion} -> osVersion) (\s@ApplicationComponentDetail' {} a -> s {osVersion = a} :: ApplicationComponentDetail)

-- | The top recommendation set for the application component.
applicationComponentDetail_recommendationSet :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe RecommendationSet)
applicationComponentDetail_recommendationSet = Lens.lens (\ApplicationComponentDetail' {recommendationSet} -> recommendationSet) (\s@ApplicationComponentDetail' {} a -> s {recommendationSet = a} :: ApplicationComponentDetail)

-- | The application component subtype.
applicationComponentDetail_resourceSubType :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe ResourceSubType)
applicationComponentDetail_resourceSubType = Lens.lens (\ApplicationComponentDetail' {resourceSubType} -> resourceSubType) (\s@ApplicationComponentDetail' {} a -> s {resourceSubType = a} :: ApplicationComponentDetail)

-- | The status of the application unit.
applicationComponentDetail_runtimeStatus :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe RuntimeAnalysisStatus)
applicationComponentDetail_runtimeStatus = Lens.lens (\ApplicationComponentDetail' {runtimeStatus} -> runtimeStatus) (\s@ApplicationComponentDetail' {} a -> s {runtimeStatus = a} :: ApplicationComponentDetail)

-- | The status message for the application unit.
applicationComponentDetail_runtimeStatusMessage :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_runtimeStatusMessage = Lens.lens (\ApplicationComponentDetail' {runtimeStatusMessage} -> runtimeStatusMessage) (\s@ApplicationComponentDetail' {} a -> s {runtimeStatusMessage = a} :: ApplicationComponentDetail)

-- | Details about the source code repository associated with the application
-- component.
applicationComponentDetail_sourceCodeRepositories :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe [SourceCodeRepository])
applicationComponentDetail_sourceCodeRepositories = Lens.lens (\ApplicationComponentDetail' {sourceCodeRepositories} -> sourceCodeRepositories) (\s@ApplicationComponentDetail' {} a -> s {sourceCodeRepositories = a} :: ApplicationComponentDetail) Prelude.. Lens.mapping Lens.coerced

-- | A detailed description of the analysis status and any failure message.
applicationComponentDetail_statusMessage :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_statusMessage = Lens.lens (\ApplicationComponentDetail' {statusMessage} -> statusMessage) (\s@ApplicationComponentDetail' {} a -> s {statusMessage = a} :: ApplicationComponentDetail)

instance Data.FromJSON ApplicationComponentDetail where
  parseJSON =
    Data.withObject
      "ApplicationComponentDetail"
      ( \x ->
          ApplicationComponentDetail'
            Prelude.<$> (x Data..:? "analysisStatus")
            Prelude.<*> (x Data..:? "antipatternReportS3Object")
            Prelude.<*> (x Data..:? "antipatternReportStatus")
            Prelude.<*> (x Data..:? "antipatternReportStatusMessage")
            Prelude.<*> (x Data..:? "appType")
            Prelude.<*> (x Data..:? "appUnitError")
            Prelude.<*> (x Data..:? "associatedServerId")
            Prelude.<*> (x Data..:? "databaseConfigDetail")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "inclusionStatus")
            Prelude.<*> (x Data..:? "lastAnalyzedTimestamp")
            Prelude.<*> ( x Data..:? "listAntipatternSeveritySummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "moreServerAssociationExists")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "osDriver")
            Prelude.<*> (x Data..:? "osVersion")
            Prelude.<*> (x Data..:? "recommendationSet")
            Prelude.<*> (x Data..:? "resourceSubType")
            Prelude.<*> (x Data..:? "runtimeStatus")
            Prelude.<*> (x Data..:? "runtimeStatusMessage")
            Prelude.<*> ( x Data..:? "sourceCodeRepositories"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "statusMessage")
      )

instance Prelude.Hashable ApplicationComponentDetail where
  hashWithSalt _salt ApplicationComponentDetail' {..} =
    _salt `Prelude.hashWithSalt` analysisStatus
      `Prelude.hashWithSalt` antipatternReportS3Object
      `Prelude.hashWithSalt` antipatternReportStatus
      `Prelude.hashWithSalt` antipatternReportStatusMessage
      `Prelude.hashWithSalt` appType
      `Prelude.hashWithSalt` appUnitError
      `Prelude.hashWithSalt` associatedServerId
      `Prelude.hashWithSalt` databaseConfigDetail
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` inclusionStatus
      `Prelude.hashWithSalt` lastAnalyzedTimestamp
      `Prelude.hashWithSalt` listAntipatternSeveritySummary
      `Prelude.hashWithSalt` moreServerAssociationExists
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` osDriver
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` recommendationSet
      `Prelude.hashWithSalt` resourceSubType
      `Prelude.hashWithSalt` runtimeStatus
      `Prelude.hashWithSalt` runtimeStatusMessage
      `Prelude.hashWithSalt` sourceCodeRepositories
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ApplicationComponentDetail where
  rnf ApplicationComponentDetail' {..} =
    Prelude.rnf analysisStatus
      `Prelude.seq` Prelude.rnf antipatternReportS3Object
      `Prelude.seq` Prelude.rnf antipatternReportStatus
      `Prelude.seq` Prelude.rnf antipatternReportStatusMessage
      `Prelude.seq` Prelude.rnf appType
      `Prelude.seq` Prelude.rnf appUnitError
      `Prelude.seq` Prelude.rnf associatedServerId
      `Prelude.seq` Prelude.rnf databaseConfigDetail
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf inclusionStatus
      `Prelude.seq` Prelude.rnf lastAnalyzedTimestamp
      `Prelude.seq` Prelude.rnf listAntipatternSeveritySummary
      `Prelude.seq` Prelude.rnf moreServerAssociationExists
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf osDriver
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf recommendationSet
      `Prelude.seq` Prelude.rnf resourceSubType
      `Prelude.seq` Prelude.rnf runtimeStatus
      `Prelude.seq` Prelude.rnf
        runtimeStatusMessage
      `Prelude.seq` Prelude.rnf
        sourceCodeRepositories
      `Prelude.seq` Prelude.rnf statusMessage
