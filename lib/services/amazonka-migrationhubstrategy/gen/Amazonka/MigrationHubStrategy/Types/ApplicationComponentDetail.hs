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
import Amazonka.MigrationHubStrategy.Types.AntipatternReportStatus
import Amazonka.MigrationHubStrategy.Types.AntipatternSeveritySummary
import Amazonka.MigrationHubStrategy.Types.AppType
import Amazonka.MigrationHubStrategy.Types.DatabaseConfigDetail
import Amazonka.MigrationHubStrategy.Types.InclusionStatus
import Amazonka.MigrationHubStrategy.Types.RecommendationSet
import Amazonka.MigrationHubStrategy.Types.ResourceSubType
import Amazonka.MigrationHubStrategy.Types.S3Object
import Amazonka.MigrationHubStrategy.Types.SourceCodeRepository
import Amazonka.MigrationHubStrategy.Types.SrcCodeOrDbAnalysisStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about an application component.
--
-- /See:/ 'newApplicationComponentDetail' smart constructor.
data ApplicationComponentDetail = ApplicationComponentDetail'
  { -- | The status of the anti-pattern report generation.
    antipatternReportStatus :: Prelude.Maybe AntipatternReportStatus,
    -- | The S3 bucket name and the Amazon S3 key name for the anti-pattern
    -- report.
    antipatternReportS3Object :: Prelude.Maybe S3Object,
    -- | The name of application component.
    name :: Prelude.Maybe Prelude.Text,
    -- | The top recommendation set for the application component.
    recommendationSet :: Prelude.Maybe RecommendationSet,
    -- | The type of application component.
    appType :: Prelude.Maybe AppType,
    -- | Set to true if the application component is running on multiple servers.
    moreServerAssociationExists :: Prelude.Maybe Prelude.Bool,
    -- | A list of anti-pattern severity summaries.
    listAntipatternSeveritySummary :: Prelude.Maybe [AntipatternSeveritySummary],
    -- | OS version.
    osVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application component.
    id :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the application component has been included for server
    -- recommendation or not.
    inclusionStatus :: Prelude.Maybe InclusionStatus,
    -- | The status of analysis, if the application component has source code or
    -- an associated database.
    analysisStatus :: Prelude.Maybe SrcCodeOrDbAnalysisStatus,
    -- | The status message for the anti-pattern.
    antipatternReportStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The application component subtype.
    resourceSubType :: Prelude.Maybe ResourceSubType,
    -- | Details about the source code repository associated with the application
    -- component.
    sourceCodeRepositories :: Prelude.Maybe [SourceCodeRepository],
    -- | Configuration details for the database associated with the application
    -- component.
    databaseConfigDetail :: Prelude.Maybe DatabaseConfigDetail,
    -- | The ID of the server that the application component is running on.
    associatedServerId :: Prelude.Maybe Prelude.Text,
    -- | OS driver.
    osDriver :: Prelude.Maybe Prelude.Text,
    -- | A detailed description of the analysis status and any failure message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the application component was assessed.
    lastAnalyzedTimestamp :: Prelude.Maybe Core.POSIX
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
-- 'antipatternReportStatus', 'applicationComponentDetail_antipatternReportStatus' - The status of the anti-pattern report generation.
--
-- 'antipatternReportS3Object', 'applicationComponentDetail_antipatternReportS3Object' - The S3 bucket name and the Amazon S3 key name for the anti-pattern
-- report.
--
-- 'name', 'applicationComponentDetail_name' - The name of application component.
--
-- 'recommendationSet', 'applicationComponentDetail_recommendationSet' - The top recommendation set for the application component.
--
-- 'appType', 'applicationComponentDetail_appType' - The type of application component.
--
-- 'moreServerAssociationExists', 'applicationComponentDetail_moreServerAssociationExists' - Set to true if the application component is running on multiple servers.
--
-- 'listAntipatternSeveritySummary', 'applicationComponentDetail_listAntipatternSeveritySummary' - A list of anti-pattern severity summaries.
--
-- 'osVersion', 'applicationComponentDetail_osVersion' - OS version.
--
-- 'id', 'applicationComponentDetail_id' - The ID of the application component.
--
-- 'inclusionStatus', 'applicationComponentDetail_inclusionStatus' - Indicates whether the application component has been included for server
-- recommendation or not.
--
-- 'analysisStatus', 'applicationComponentDetail_analysisStatus' - The status of analysis, if the application component has source code or
-- an associated database.
--
-- 'antipatternReportStatusMessage', 'applicationComponentDetail_antipatternReportStatusMessage' - The status message for the anti-pattern.
--
-- 'resourceSubType', 'applicationComponentDetail_resourceSubType' - The application component subtype.
--
-- 'sourceCodeRepositories', 'applicationComponentDetail_sourceCodeRepositories' - Details about the source code repository associated with the application
-- component.
--
-- 'databaseConfigDetail', 'applicationComponentDetail_databaseConfigDetail' - Configuration details for the database associated with the application
-- component.
--
-- 'associatedServerId', 'applicationComponentDetail_associatedServerId' - The ID of the server that the application component is running on.
--
-- 'osDriver', 'applicationComponentDetail_osDriver' - OS driver.
--
-- 'statusMessage', 'applicationComponentDetail_statusMessage' - A detailed description of the analysis status and any failure message.
--
-- 'lastAnalyzedTimestamp', 'applicationComponentDetail_lastAnalyzedTimestamp' - The timestamp of when the application component was assessed.
newApplicationComponentDetail ::
  ApplicationComponentDetail
newApplicationComponentDetail =
  ApplicationComponentDetail'
    { antipatternReportStatus =
        Prelude.Nothing,
      antipatternReportS3Object = Prelude.Nothing,
      name = Prelude.Nothing,
      recommendationSet = Prelude.Nothing,
      appType = Prelude.Nothing,
      moreServerAssociationExists = Prelude.Nothing,
      listAntipatternSeveritySummary =
        Prelude.Nothing,
      osVersion = Prelude.Nothing,
      id = Prelude.Nothing,
      inclusionStatus = Prelude.Nothing,
      analysisStatus = Prelude.Nothing,
      antipatternReportStatusMessage =
        Prelude.Nothing,
      resourceSubType = Prelude.Nothing,
      sourceCodeRepositories = Prelude.Nothing,
      databaseConfigDetail = Prelude.Nothing,
      associatedServerId = Prelude.Nothing,
      osDriver = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      lastAnalyzedTimestamp = Prelude.Nothing
    }

-- | The status of the anti-pattern report generation.
applicationComponentDetail_antipatternReportStatus :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe AntipatternReportStatus)
applicationComponentDetail_antipatternReportStatus = Lens.lens (\ApplicationComponentDetail' {antipatternReportStatus} -> antipatternReportStatus) (\s@ApplicationComponentDetail' {} a -> s {antipatternReportStatus = a} :: ApplicationComponentDetail)

-- | The S3 bucket name and the Amazon S3 key name for the anti-pattern
-- report.
applicationComponentDetail_antipatternReportS3Object :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe S3Object)
applicationComponentDetail_antipatternReportS3Object = Lens.lens (\ApplicationComponentDetail' {antipatternReportS3Object} -> antipatternReportS3Object) (\s@ApplicationComponentDetail' {} a -> s {antipatternReportS3Object = a} :: ApplicationComponentDetail)

-- | The name of application component.
applicationComponentDetail_name :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_name = Lens.lens (\ApplicationComponentDetail' {name} -> name) (\s@ApplicationComponentDetail' {} a -> s {name = a} :: ApplicationComponentDetail)

-- | The top recommendation set for the application component.
applicationComponentDetail_recommendationSet :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe RecommendationSet)
applicationComponentDetail_recommendationSet = Lens.lens (\ApplicationComponentDetail' {recommendationSet} -> recommendationSet) (\s@ApplicationComponentDetail' {} a -> s {recommendationSet = a} :: ApplicationComponentDetail)

-- | The type of application component.
applicationComponentDetail_appType :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe AppType)
applicationComponentDetail_appType = Lens.lens (\ApplicationComponentDetail' {appType} -> appType) (\s@ApplicationComponentDetail' {} a -> s {appType = a} :: ApplicationComponentDetail)

-- | Set to true if the application component is running on multiple servers.
applicationComponentDetail_moreServerAssociationExists :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Bool)
applicationComponentDetail_moreServerAssociationExists = Lens.lens (\ApplicationComponentDetail' {moreServerAssociationExists} -> moreServerAssociationExists) (\s@ApplicationComponentDetail' {} a -> s {moreServerAssociationExists = a} :: ApplicationComponentDetail)

-- | A list of anti-pattern severity summaries.
applicationComponentDetail_listAntipatternSeveritySummary :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe [AntipatternSeveritySummary])
applicationComponentDetail_listAntipatternSeveritySummary = Lens.lens (\ApplicationComponentDetail' {listAntipatternSeveritySummary} -> listAntipatternSeveritySummary) (\s@ApplicationComponentDetail' {} a -> s {listAntipatternSeveritySummary = a} :: ApplicationComponentDetail) Prelude.. Lens.mapping Lens.coerced

-- | OS version.
applicationComponentDetail_osVersion :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_osVersion = Lens.lens (\ApplicationComponentDetail' {osVersion} -> osVersion) (\s@ApplicationComponentDetail' {} a -> s {osVersion = a} :: ApplicationComponentDetail)

-- | The ID of the application component.
applicationComponentDetail_id :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_id = Lens.lens (\ApplicationComponentDetail' {id} -> id) (\s@ApplicationComponentDetail' {} a -> s {id = a} :: ApplicationComponentDetail)

-- | Indicates whether the application component has been included for server
-- recommendation or not.
applicationComponentDetail_inclusionStatus :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe InclusionStatus)
applicationComponentDetail_inclusionStatus = Lens.lens (\ApplicationComponentDetail' {inclusionStatus} -> inclusionStatus) (\s@ApplicationComponentDetail' {} a -> s {inclusionStatus = a} :: ApplicationComponentDetail)

-- | The status of analysis, if the application component has source code or
-- an associated database.
applicationComponentDetail_analysisStatus :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe SrcCodeOrDbAnalysisStatus)
applicationComponentDetail_analysisStatus = Lens.lens (\ApplicationComponentDetail' {analysisStatus} -> analysisStatus) (\s@ApplicationComponentDetail' {} a -> s {analysisStatus = a} :: ApplicationComponentDetail)

-- | The status message for the anti-pattern.
applicationComponentDetail_antipatternReportStatusMessage :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_antipatternReportStatusMessage = Lens.lens (\ApplicationComponentDetail' {antipatternReportStatusMessage} -> antipatternReportStatusMessage) (\s@ApplicationComponentDetail' {} a -> s {antipatternReportStatusMessage = a} :: ApplicationComponentDetail)

-- | The application component subtype.
applicationComponentDetail_resourceSubType :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe ResourceSubType)
applicationComponentDetail_resourceSubType = Lens.lens (\ApplicationComponentDetail' {resourceSubType} -> resourceSubType) (\s@ApplicationComponentDetail' {} a -> s {resourceSubType = a} :: ApplicationComponentDetail)

-- | Details about the source code repository associated with the application
-- component.
applicationComponentDetail_sourceCodeRepositories :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe [SourceCodeRepository])
applicationComponentDetail_sourceCodeRepositories = Lens.lens (\ApplicationComponentDetail' {sourceCodeRepositories} -> sourceCodeRepositories) (\s@ApplicationComponentDetail' {} a -> s {sourceCodeRepositories = a} :: ApplicationComponentDetail) Prelude.. Lens.mapping Lens.coerced

-- | Configuration details for the database associated with the application
-- component.
applicationComponentDetail_databaseConfigDetail :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe DatabaseConfigDetail)
applicationComponentDetail_databaseConfigDetail = Lens.lens (\ApplicationComponentDetail' {databaseConfigDetail} -> databaseConfigDetail) (\s@ApplicationComponentDetail' {} a -> s {databaseConfigDetail = a} :: ApplicationComponentDetail)

-- | The ID of the server that the application component is running on.
applicationComponentDetail_associatedServerId :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_associatedServerId = Lens.lens (\ApplicationComponentDetail' {associatedServerId} -> associatedServerId) (\s@ApplicationComponentDetail' {} a -> s {associatedServerId = a} :: ApplicationComponentDetail)

-- | OS driver.
applicationComponentDetail_osDriver :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_osDriver = Lens.lens (\ApplicationComponentDetail' {osDriver} -> osDriver) (\s@ApplicationComponentDetail' {} a -> s {osDriver = a} :: ApplicationComponentDetail)

-- | A detailed description of the analysis status and any failure message.
applicationComponentDetail_statusMessage :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.Text)
applicationComponentDetail_statusMessage = Lens.lens (\ApplicationComponentDetail' {statusMessage} -> statusMessage) (\s@ApplicationComponentDetail' {} a -> s {statusMessage = a} :: ApplicationComponentDetail)

-- | The timestamp of when the application component was assessed.
applicationComponentDetail_lastAnalyzedTimestamp :: Lens.Lens' ApplicationComponentDetail (Prelude.Maybe Prelude.UTCTime)
applicationComponentDetail_lastAnalyzedTimestamp = Lens.lens (\ApplicationComponentDetail' {lastAnalyzedTimestamp} -> lastAnalyzedTimestamp) (\s@ApplicationComponentDetail' {} a -> s {lastAnalyzedTimestamp = a} :: ApplicationComponentDetail) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ApplicationComponentDetail where
  parseJSON =
    Core.withObject
      "ApplicationComponentDetail"
      ( \x ->
          ApplicationComponentDetail'
            Prelude.<$> (x Core..:? "antipatternReportStatus")
            Prelude.<*> (x Core..:? "antipatternReportS3Object")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "recommendationSet")
            Prelude.<*> (x Core..:? "appType")
            Prelude.<*> (x Core..:? "moreServerAssociationExists")
            Prelude.<*> ( x Core..:? "listAntipatternSeveritySummary"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "osVersion")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "inclusionStatus")
            Prelude.<*> (x Core..:? "analysisStatus")
            Prelude.<*> (x Core..:? "antipatternReportStatusMessage")
            Prelude.<*> (x Core..:? "resourceSubType")
            Prelude.<*> ( x Core..:? "sourceCodeRepositories"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "databaseConfigDetail")
            Prelude.<*> (x Core..:? "associatedServerId")
            Prelude.<*> (x Core..:? "osDriver")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "lastAnalyzedTimestamp")
      )

instance Prelude.Hashable ApplicationComponentDetail where
  hashWithSalt _salt ApplicationComponentDetail' {..} =
    _salt
      `Prelude.hashWithSalt` antipatternReportStatus
      `Prelude.hashWithSalt` antipatternReportS3Object
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recommendationSet
      `Prelude.hashWithSalt` appType
      `Prelude.hashWithSalt` moreServerAssociationExists
      `Prelude.hashWithSalt` listAntipatternSeveritySummary
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` inclusionStatus
      `Prelude.hashWithSalt` analysisStatus
      `Prelude.hashWithSalt` antipatternReportStatusMessage
      `Prelude.hashWithSalt` resourceSubType
      `Prelude.hashWithSalt` sourceCodeRepositories
      `Prelude.hashWithSalt` databaseConfigDetail
      `Prelude.hashWithSalt` associatedServerId
      `Prelude.hashWithSalt` osDriver
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` lastAnalyzedTimestamp

instance Prelude.NFData ApplicationComponentDetail where
  rnf ApplicationComponentDetail' {..} =
    Prelude.rnf antipatternReportStatus
      `Prelude.seq` Prelude.rnf antipatternReportS3Object
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recommendationSet
      `Prelude.seq` Prelude.rnf appType
      `Prelude.seq` Prelude.rnf moreServerAssociationExists
      `Prelude.seq` Prelude.rnf listAntipatternSeveritySummary
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf inclusionStatus
      `Prelude.seq` Prelude.rnf analysisStatus
      `Prelude.seq` Prelude.rnf antipatternReportStatusMessage
      `Prelude.seq` Prelude.rnf resourceSubType
      `Prelude.seq` Prelude.rnf sourceCodeRepositories
      `Prelude.seq` Prelude.rnf databaseConfigDetail
      `Prelude.seq` Prelude.rnf associatedServerId
      `Prelude.seq` Prelude.rnf osDriver
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf
        lastAnalyzedTimestamp
