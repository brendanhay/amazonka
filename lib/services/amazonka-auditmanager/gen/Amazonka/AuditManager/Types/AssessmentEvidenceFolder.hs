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
-- Module      : Amazonka.AuditManager.Types.AssessmentEvidenceFolder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentEvidenceFolder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The folder where Audit Manager stores evidence for an assessment.
--
-- /See:/ 'newAssessmentEvidenceFolder' smart constructor.
data AssessmentEvidenceFolder = AssessmentEvidenceFolder'
  { -- | The unique identifier for the control.
    controlId :: Prelude.Maybe Prelude.Text,
    -- | The name of the user who created the evidence folder.
    author :: Prelude.Maybe Prelude.Text,
    -- | The name of the evidence folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | The total number of issues that were reported directly from Security
    -- Hub, Config, or both.
    evidenceByTypeComplianceCheckIssuesCount :: Prelude.Maybe Prelude.Int,
    -- | The total amount of evidence in the evidence folder.
    totalEvidence :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The amount of evidence that\'s included in the evidence folder.
    evidenceResourcesIncludedCount :: Prelude.Maybe Prelude.Int,
    -- | The date when the first evidence was added to the evidence folder.
    date :: Prelude.Maybe Data.POSIX,
    -- | The total number of Amazon Web Services resources that were assessed to
    -- generate the evidence.
    evidenceAwsServiceSourceCount :: Prelude.Maybe Prelude.Int,
    -- | The number of evidence that falls under the manual category. This
    -- evidence is imported manually.
    evidenceByTypeManualCount :: Prelude.Maybe Prelude.Int,
    -- | The number of evidence that falls under the user activity category. This
    -- evidence is collected from CloudTrail logs.
    evidenceByTypeUserActivityCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the folder that the evidence is stored in.
    id :: Prelude.Maybe Prelude.Text,
    -- | The total count of evidence that\'s included in the assessment report.
    assessmentReportSelectionCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the control set.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The number of evidence that falls under the compliance check category.
    -- This evidence is collected from Config or Security Hub.
    evidenceByTypeComplianceCheckCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Service that the evidence was collected from.
    dataSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the control.
    controlName :: Prelude.Maybe Prelude.Text,
    -- | The number of evidence that falls under the configuration data category.
    -- This evidence is collected from configuration snapshots of other Amazon
    -- Web Services such as Amazon EC2, Amazon S3, or IAM.
    evidenceByTypeConfigurationDataCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentEvidenceFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlId', 'assessmentEvidenceFolder_controlId' - The unique identifier for the control.
--
-- 'author', 'assessmentEvidenceFolder_author' - The name of the user who created the evidence folder.
--
-- 'name', 'assessmentEvidenceFolder_name' - The name of the evidence folder.
--
-- 'evidenceByTypeComplianceCheckIssuesCount', 'assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount' - The total number of issues that were reported directly from Security
-- Hub, Config, or both.
--
-- 'totalEvidence', 'assessmentEvidenceFolder_totalEvidence' - The total amount of evidence in the evidence folder.
--
-- 'assessmentId', 'assessmentEvidenceFolder_assessmentId' - The identifier for the assessment.
--
-- 'evidenceResourcesIncludedCount', 'assessmentEvidenceFolder_evidenceResourcesIncludedCount' - The amount of evidence that\'s included in the evidence folder.
--
-- 'date', 'assessmentEvidenceFolder_date' - The date when the first evidence was added to the evidence folder.
--
-- 'evidenceAwsServiceSourceCount', 'assessmentEvidenceFolder_evidenceAwsServiceSourceCount' - The total number of Amazon Web Services resources that were assessed to
-- generate the evidence.
--
-- 'evidenceByTypeManualCount', 'assessmentEvidenceFolder_evidenceByTypeManualCount' - The number of evidence that falls under the manual category. This
-- evidence is imported manually.
--
-- 'evidenceByTypeUserActivityCount', 'assessmentEvidenceFolder_evidenceByTypeUserActivityCount' - The number of evidence that falls under the user activity category. This
-- evidence is collected from CloudTrail logs.
--
-- 'id', 'assessmentEvidenceFolder_id' - The identifier for the folder that the evidence is stored in.
--
-- 'assessmentReportSelectionCount', 'assessmentEvidenceFolder_assessmentReportSelectionCount' - The total count of evidence that\'s included in the assessment report.
--
-- 'controlSetId', 'assessmentEvidenceFolder_controlSetId' - The identifier for the control set.
--
-- 'evidenceByTypeComplianceCheckCount', 'assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount' - The number of evidence that falls under the compliance check category.
-- This evidence is collected from Config or Security Hub.
--
-- 'dataSource', 'assessmentEvidenceFolder_dataSource' - The Amazon Web Service that the evidence was collected from.
--
-- 'controlName', 'assessmentEvidenceFolder_controlName' - The name of the control.
--
-- 'evidenceByTypeConfigurationDataCount', 'assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount' - The number of evidence that falls under the configuration data category.
-- This evidence is collected from configuration snapshots of other Amazon
-- Web Services such as Amazon EC2, Amazon S3, or IAM.
newAssessmentEvidenceFolder ::
  AssessmentEvidenceFolder
newAssessmentEvidenceFolder =
  AssessmentEvidenceFolder'
    { controlId =
        Prelude.Nothing,
      author = Prelude.Nothing,
      name = Prelude.Nothing,
      evidenceByTypeComplianceCheckIssuesCount =
        Prelude.Nothing,
      totalEvidence = Prelude.Nothing,
      assessmentId = Prelude.Nothing,
      evidenceResourcesIncludedCount = Prelude.Nothing,
      date = Prelude.Nothing,
      evidenceAwsServiceSourceCount = Prelude.Nothing,
      evidenceByTypeManualCount = Prelude.Nothing,
      evidenceByTypeUserActivityCount = Prelude.Nothing,
      id = Prelude.Nothing,
      assessmentReportSelectionCount = Prelude.Nothing,
      controlSetId = Prelude.Nothing,
      evidenceByTypeComplianceCheckCount =
        Prelude.Nothing,
      dataSource = Prelude.Nothing,
      controlName = Prelude.Nothing,
      evidenceByTypeConfigurationDataCount =
        Prelude.Nothing
    }

-- | The unique identifier for the control.
assessmentEvidenceFolder_controlId :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_controlId = Lens.lens (\AssessmentEvidenceFolder' {controlId} -> controlId) (\s@AssessmentEvidenceFolder' {} a -> s {controlId = a} :: AssessmentEvidenceFolder)

-- | The name of the user who created the evidence folder.
assessmentEvidenceFolder_author :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_author = Lens.lens (\AssessmentEvidenceFolder' {author} -> author) (\s@AssessmentEvidenceFolder' {} a -> s {author = a} :: AssessmentEvidenceFolder)

-- | The name of the evidence folder.
assessmentEvidenceFolder_name :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_name = Lens.lens (\AssessmentEvidenceFolder' {name} -> name) (\s@AssessmentEvidenceFolder' {} a -> s {name = a} :: AssessmentEvidenceFolder)

-- | The total number of issues that were reported directly from Security
-- Hub, Config, or both.
assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeComplianceCheckIssuesCount} -> evidenceByTypeComplianceCheckIssuesCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeComplianceCheckIssuesCount = a} :: AssessmentEvidenceFolder)

-- | The total amount of evidence in the evidence folder.
assessmentEvidenceFolder_totalEvidence :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_totalEvidence = Lens.lens (\AssessmentEvidenceFolder' {totalEvidence} -> totalEvidence) (\s@AssessmentEvidenceFolder' {} a -> s {totalEvidence = a} :: AssessmentEvidenceFolder)

-- | The identifier for the assessment.
assessmentEvidenceFolder_assessmentId :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_assessmentId = Lens.lens (\AssessmentEvidenceFolder' {assessmentId} -> assessmentId) (\s@AssessmentEvidenceFolder' {} a -> s {assessmentId = a} :: AssessmentEvidenceFolder)

-- | The amount of evidence that\'s included in the evidence folder.
assessmentEvidenceFolder_evidenceResourcesIncludedCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceResourcesIncludedCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceResourcesIncludedCount} -> evidenceResourcesIncludedCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceResourcesIncludedCount = a} :: AssessmentEvidenceFolder)

-- | The date when the first evidence was added to the evidence folder.
assessmentEvidenceFolder_date :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.UTCTime)
assessmentEvidenceFolder_date = Lens.lens (\AssessmentEvidenceFolder' {date} -> date) (\s@AssessmentEvidenceFolder' {} a -> s {date = a} :: AssessmentEvidenceFolder) Prelude.. Lens.mapping Data._Time

-- | The total number of Amazon Web Services resources that were assessed to
-- generate the evidence.
assessmentEvidenceFolder_evidenceAwsServiceSourceCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceAwsServiceSourceCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceAwsServiceSourceCount} -> evidenceAwsServiceSourceCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceAwsServiceSourceCount = a} :: AssessmentEvidenceFolder)

-- | The number of evidence that falls under the manual category. This
-- evidence is imported manually.
assessmentEvidenceFolder_evidenceByTypeManualCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeManualCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeManualCount} -> evidenceByTypeManualCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeManualCount = a} :: AssessmentEvidenceFolder)

-- | The number of evidence that falls under the user activity category. This
-- evidence is collected from CloudTrail logs.
assessmentEvidenceFolder_evidenceByTypeUserActivityCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeUserActivityCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeUserActivityCount} -> evidenceByTypeUserActivityCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeUserActivityCount = a} :: AssessmentEvidenceFolder)

-- | The identifier for the folder that the evidence is stored in.
assessmentEvidenceFolder_id :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_id = Lens.lens (\AssessmentEvidenceFolder' {id} -> id) (\s@AssessmentEvidenceFolder' {} a -> s {id = a} :: AssessmentEvidenceFolder)

-- | The total count of evidence that\'s included in the assessment report.
assessmentEvidenceFolder_assessmentReportSelectionCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_assessmentReportSelectionCount = Lens.lens (\AssessmentEvidenceFolder' {assessmentReportSelectionCount} -> assessmentReportSelectionCount) (\s@AssessmentEvidenceFolder' {} a -> s {assessmentReportSelectionCount = a} :: AssessmentEvidenceFolder)

-- | The identifier for the control set.
assessmentEvidenceFolder_controlSetId :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_controlSetId = Lens.lens (\AssessmentEvidenceFolder' {controlSetId} -> controlSetId) (\s@AssessmentEvidenceFolder' {} a -> s {controlSetId = a} :: AssessmentEvidenceFolder)

-- | The number of evidence that falls under the compliance check category.
-- This evidence is collected from Config or Security Hub.
assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeComplianceCheckCount} -> evidenceByTypeComplianceCheckCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeComplianceCheckCount = a} :: AssessmentEvidenceFolder)

-- | The Amazon Web Service that the evidence was collected from.
assessmentEvidenceFolder_dataSource :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_dataSource = Lens.lens (\AssessmentEvidenceFolder' {dataSource} -> dataSource) (\s@AssessmentEvidenceFolder' {} a -> s {dataSource = a} :: AssessmentEvidenceFolder)

-- | The name of the control.
assessmentEvidenceFolder_controlName :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_controlName = Lens.lens (\AssessmentEvidenceFolder' {controlName} -> controlName) (\s@AssessmentEvidenceFolder' {} a -> s {controlName = a} :: AssessmentEvidenceFolder)

-- | The number of evidence that falls under the configuration data category.
-- This evidence is collected from configuration snapshots of other Amazon
-- Web Services such as Amazon EC2, Amazon S3, or IAM.
assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeConfigurationDataCount} -> evidenceByTypeConfigurationDataCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeConfigurationDataCount = a} :: AssessmentEvidenceFolder)

instance Data.FromJSON AssessmentEvidenceFolder where
  parseJSON =
    Data.withObject
      "AssessmentEvidenceFolder"
      ( \x ->
          AssessmentEvidenceFolder'
            Prelude.<$> (x Data..:? "controlId")
            Prelude.<*> (x Data..:? "author")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> ( x
                            Data..:? "evidenceByTypeComplianceCheckIssuesCount"
                        )
            Prelude.<*> (x Data..:? "totalEvidence")
            Prelude.<*> (x Data..:? "assessmentId")
            Prelude.<*> (x Data..:? "evidenceResourcesIncludedCount")
            Prelude.<*> (x Data..:? "date")
            Prelude.<*> (x Data..:? "evidenceAwsServiceSourceCount")
            Prelude.<*> (x Data..:? "evidenceByTypeManualCount")
            Prelude.<*> (x Data..:? "evidenceByTypeUserActivityCount")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "assessmentReportSelectionCount")
            Prelude.<*> (x Data..:? "controlSetId")
            Prelude.<*> (x Data..:? "evidenceByTypeComplianceCheckCount")
            Prelude.<*> (x Data..:? "dataSource")
            Prelude.<*> (x Data..:? "controlName")
            Prelude.<*> (x Data..:? "evidenceByTypeConfigurationDataCount")
      )

instance Prelude.Hashable AssessmentEvidenceFolder where
  hashWithSalt _salt AssessmentEvidenceFolder' {..} =
    _salt `Prelude.hashWithSalt` controlId
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` evidenceByTypeComplianceCheckIssuesCount
      `Prelude.hashWithSalt` totalEvidence
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` evidenceResourcesIncludedCount
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` evidenceAwsServiceSourceCount
      `Prelude.hashWithSalt` evidenceByTypeManualCount
      `Prelude.hashWithSalt` evidenceByTypeUserActivityCount
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` assessmentReportSelectionCount
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` evidenceByTypeComplianceCheckCount
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` controlName
      `Prelude.hashWithSalt` evidenceByTypeConfigurationDataCount

instance Prelude.NFData AssessmentEvidenceFolder where
  rnf AssessmentEvidenceFolder' {..} =
    Prelude.rnf controlId
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf evidenceByTypeComplianceCheckIssuesCount
      `Prelude.seq` Prelude.rnf totalEvidence
      `Prelude.seq` Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf evidenceResourcesIncludedCount
      `Prelude.seq` Prelude.rnf date
      `Prelude.seq` Prelude.rnf evidenceAwsServiceSourceCount
      `Prelude.seq` Prelude.rnf evidenceByTypeManualCount
      `Prelude.seq` Prelude.rnf evidenceByTypeUserActivityCount
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf assessmentReportSelectionCount
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf
        evidenceByTypeComplianceCheckCount
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf controlName
      `Prelude.seq` Prelude.rnf
        evidenceByTypeConfigurationDataCount
