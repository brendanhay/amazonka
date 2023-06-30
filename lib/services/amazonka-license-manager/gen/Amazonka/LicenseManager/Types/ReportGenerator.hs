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
-- Module      : Amazonka.LicenseManager.Types.ReportGenerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ReportGenerator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.ReportContext
import Amazonka.LicenseManager.Types.ReportFrequency
import Amazonka.LicenseManager.Types.ReportType
import Amazonka.LicenseManager.Types.S3Location
import Amazonka.LicenseManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describe the details of a report generator.
--
-- /See:/ 'newReportGenerator' smart constructor.
data ReportGenerator = ReportGenerator'
  { -- | Time the report was created.
    createTime :: Prelude.Maybe Prelude.Text,
    -- | Description of the report generator.
    description :: Prelude.Maybe Prelude.Text,
    -- | Time the last report was generated at.
    lastReportGenerationTime :: Prelude.Maybe Prelude.Text,
    -- | Failure message for the last report generation attempt.
    lastRunFailureReason :: Prelude.Maybe Prelude.Text,
    -- | Status of the last report generation attempt.
    lastRunStatus :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the report generator.
    licenseManagerReportGeneratorArn :: Prelude.Maybe Prelude.Text,
    -- | License configuration type for this generator.
    reportContext :: Prelude.Maybe ReportContext,
    -- | The Amazon Web Services account ID used to create the report generator.
    reportCreatorAccount :: Prelude.Maybe Prelude.Text,
    -- | Details about how frequently reports are generated.
    reportFrequency :: Prelude.Maybe ReportFrequency,
    -- | Name of the report generator.
    reportGeneratorName :: Prelude.Maybe Prelude.Text,
    -- | Type of reports that are generated.
    reportType :: Prelude.Maybe [ReportType],
    -- | Details of the S3 bucket that report generator reports are published to.
    s3Location :: Prelude.Maybe S3Location,
    -- | Tags associated with the report generator.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportGenerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'reportGenerator_createTime' - Time the report was created.
--
-- 'description', 'reportGenerator_description' - Description of the report generator.
--
-- 'lastReportGenerationTime', 'reportGenerator_lastReportGenerationTime' - Time the last report was generated at.
--
-- 'lastRunFailureReason', 'reportGenerator_lastRunFailureReason' - Failure message for the last report generation attempt.
--
-- 'lastRunStatus', 'reportGenerator_lastRunStatus' - Status of the last report generation attempt.
--
-- 'licenseManagerReportGeneratorArn', 'reportGenerator_licenseManagerReportGeneratorArn' - Amazon Resource Name (ARN) of the report generator.
--
-- 'reportContext', 'reportGenerator_reportContext' - License configuration type for this generator.
--
-- 'reportCreatorAccount', 'reportGenerator_reportCreatorAccount' - The Amazon Web Services account ID used to create the report generator.
--
-- 'reportFrequency', 'reportGenerator_reportFrequency' - Details about how frequently reports are generated.
--
-- 'reportGeneratorName', 'reportGenerator_reportGeneratorName' - Name of the report generator.
--
-- 'reportType', 'reportGenerator_reportType' - Type of reports that are generated.
--
-- 's3Location', 'reportGenerator_s3Location' - Details of the S3 bucket that report generator reports are published to.
--
-- 'tags', 'reportGenerator_tags' - Tags associated with the report generator.
newReportGenerator ::
  ReportGenerator
newReportGenerator =
  ReportGenerator'
    { createTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastReportGenerationTime = Prelude.Nothing,
      lastRunFailureReason = Prelude.Nothing,
      lastRunStatus = Prelude.Nothing,
      licenseManagerReportGeneratorArn = Prelude.Nothing,
      reportContext = Prelude.Nothing,
      reportCreatorAccount = Prelude.Nothing,
      reportFrequency = Prelude.Nothing,
      reportGeneratorName = Prelude.Nothing,
      reportType = Prelude.Nothing,
      s3Location = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Time the report was created.
reportGenerator_createTime :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_createTime = Lens.lens (\ReportGenerator' {createTime} -> createTime) (\s@ReportGenerator' {} a -> s {createTime = a} :: ReportGenerator)

-- | Description of the report generator.
reportGenerator_description :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_description = Lens.lens (\ReportGenerator' {description} -> description) (\s@ReportGenerator' {} a -> s {description = a} :: ReportGenerator)

-- | Time the last report was generated at.
reportGenerator_lastReportGenerationTime :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_lastReportGenerationTime = Lens.lens (\ReportGenerator' {lastReportGenerationTime} -> lastReportGenerationTime) (\s@ReportGenerator' {} a -> s {lastReportGenerationTime = a} :: ReportGenerator)

-- | Failure message for the last report generation attempt.
reportGenerator_lastRunFailureReason :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_lastRunFailureReason = Lens.lens (\ReportGenerator' {lastRunFailureReason} -> lastRunFailureReason) (\s@ReportGenerator' {} a -> s {lastRunFailureReason = a} :: ReportGenerator)

-- | Status of the last report generation attempt.
reportGenerator_lastRunStatus :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_lastRunStatus = Lens.lens (\ReportGenerator' {lastRunStatus} -> lastRunStatus) (\s@ReportGenerator' {} a -> s {lastRunStatus = a} :: ReportGenerator)

-- | Amazon Resource Name (ARN) of the report generator.
reportGenerator_licenseManagerReportGeneratorArn :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_licenseManagerReportGeneratorArn = Lens.lens (\ReportGenerator' {licenseManagerReportGeneratorArn} -> licenseManagerReportGeneratorArn) (\s@ReportGenerator' {} a -> s {licenseManagerReportGeneratorArn = a} :: ReportGenerator)

-- | License configuration type for this generator.
reportGenerator_reportContext :: Lens.Lens' ReportGenerator (Prelude.Maybe ReportContext)
reportGenerator_reportContext = Lens.lens (\ReportGenerator' {reportContext} -> reportContext) (\s@ReportGenerator' {} a -> s {reportContext = a} :: ReportGenerator)

-- | The Amazon Web Services account ID used to create the report generator.
reportGenerator_reportCreatorAccount :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_reportCreatorAccount = Lens.lens (\ReportGenerator' {reportCreatorAccount} -> reportCreatorAccount) (\s@ReportGenerator' {} a -> s {reportCreatorAccount = a} :: ReportGenerator)

-- | Details about how frequently reports are generated.
reportGenerator_reportFrequency :: Lens.Lens' ReportGenerator (Prelude.Maybe ReportFrequency)
reportGenerator_reportFrequency = Lens.lens (\ReportGenerator' {reportFrequency} -> reportFrequency) (\s@ReportGenerator' {} a -> s {reportFrequency = a} :: ReportGenerator)

-- | Name of the report generator.
reportGenerator_reportGeneratorName :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_reportGeneratorName = Lens.lens (\ReportGenerator' {reportGeneratorName} -> reportGeneratorName) (\s@ReportGenerator' {} a -> s {reportGeneratorName = a} :: ReportGenerator)

-- | Type of reports that are generated.
reportGenerator_reportType :: Lens.Lens' ReportGenerator (Prelude.Maybe [ReportType])
reportGenerator_reportType = Lens.lens (\ReportGenerator' {reportType} -> reportType) (\s@ReportGenerator' {} a -> s {reportType = a} :: ReportGenerator) Prelude.. Lens.mapping Lens.coerced

-- | Details of the S3 bucket that report generator reports are published to.
reportGenerator_s3Location :: Lens.Lens' ReportGenerator (Prelude.Maybe S3Location)
reportGenerator_s3Location = Lens.lens (\ReportGenerator' {s3Location} -> s3Location) (\s@ReportGenerator' {} a -> s {s3Location = a} :: ReportGenerator)

-- | Tags associated with the report generator.
reportGenerator_tags :: Lens.Lens' ReportGenerator (Prelude.Maybe [Tag])
reportGenerator_tags = Lens.lens (\ReportGenerator' {tags} -> tags) (\s@ReportGenerator' {} a -> s {tags = a} :: ReportGenerator) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ReportGenerator where
  parseJSON =
    Data.withObject
      "ReportGenerator"
      ( \x ->
          ReportGenerator'
            Prelude.<$> (x Data..:? "CreateTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastReportGenerationTime")
            Prelude.<*> (x Data..:? "LastRunFailureReason")
            Prelude.<*> (x Data..:? "LastRunStatus")
            Prelude.<*> (x Data..:? "LicenseManagerReportGeneratorArn")
            Prelude.<*> (x Data..:? "ReportContext")
            Prelude.<*> (x Data..:? "ReportCreatorAccount")
            Prelude.<*> (x Data..:? "ReportFrequency")
            Prelude.<*> (x Data..:? "ReportGeneratorName")
            Prelude.<*> (x Data..:? "ReportType" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "S3Location")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ReportGenerator where
  hashWithSalt _salt ReportGenerator' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastReportGenerationTime
      `Prelude.hashWithSalt` lastRunFailureReason
      `Prelude.hashWithSalt` lastRunStatus
      `Prelude.hashWithSalt` licenseManagerReportGeneratorArn
      `Prelude.hashWithSalt` reportContext
      `Prelude.hashWithSalt` reportCreatorAccount
      `Prelude.hashWithSalt` reportFrequency
      `Prelude.hashWithSalt` reportGeneratorName
      `Prelude.hashWithSalt` reportType
      `Prelude.hashWithSalt` s3Location
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ReportGenerator where
  rnf ReportGenerator' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastReportGenerationTime
      `Prelude.seq` Prelude.rnf lastRunFailureReason
      `Prelude.seq` Prelude.rnf lastRunStatus
      `Prelude.seq` Prelude.rnf licenseManagerReportGeneratorArn
      `Prelude.seq` Prelude.rnf reportContext
      `Prelude.seq` Prelude.rnf reportCreatorAccount
      `Prelude.seq` Prelude.rnf reportFrequency
      `Prelude.seq` Prelude.rnf reportGeneratorName
      `Prelude.seq` Prelude.rnf reportType
      `Prelude.seq` Prelude.rnf s3Location
      `Prelude.seq` Prelude.rnf tags
