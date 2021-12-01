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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentEvidenceFolder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The folder in which Audit Manager stores evidence for an assessment.
--
-- /See:/ 'newAssessmentEvidenceFolder' smart constructor.
data AssessmentEvidenceFolder = AssessmentEvidenceFolder'
  { -- | The total number of issues that were reported directly from Security
    -- Hub, Config, or both.
    evidenceByTypeComplianceCheckIssuesCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the control set.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The total count of evidence included in the assessment report.
    assessmentReportSelectionCount :: Prelude.Maybe Prelude.Int,
    -- | The total amount of evidence in the evidence folder.
    totalEvidence :: Prelude.Maybe Prelude.Int,
    -- | The number of evidence that falls under the manual category. This
    -- evidence is imported manually.
    evidenceByTypeManualCount :: Prelude.Maybe Prelude.Int,
    -- | The date when the first evidence was added to the evidence folder.
    date :: Prelude.Maybe Core.POSIX,
    -- | The name of the specified evidence folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of evidence that falls under the user activity category. This
    -- evidence is collected from CloudTrail logs.
    evidenceByTypeUserActivityCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the specified control.
    controlId :: Prelude.Maybe Prelude.Text,
    -- | The total number of Amazon Web Services resources assessed to generate
    -- the evidence.
    evidenceAwsServiceSourceCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the user who created the evidence folder.
    author :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the folder in which evidence is stored.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Service from which the evidence was collected.
    dataSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the control.
    controlName :: Prelude.Maybe Prelude.Text,
    -- | The number of evidence that falls under the compliance check category.
    -- This evidence is collected from Config or Security Hub.
    evidenceByTypeComplianceCheckCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The number of evidence that falls under the configuration data category.
    -- This evidence is collected from configuration snapshots of other Amazon
    -- Web Services services such as Amazon EC2, Amazon S3, or IAM.
    evidenceByTypeConfigurationDataCount :: Prelude.Maybe Prelude.Int,
    -- | The amount of evidence included in the evidence folder.
    evidenceResourcesIncludedCount :: Prelude.Maybe Prelude.Int
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
-- 'evidenceByTypeComplianceCheckIssuesCount', 'assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount' - The total number of issues that were reported directly from Security
-- Hub, Config, or both.
--
-- 'controlSetId', 'assessmentEvidenceFolder_controlSetId' - The identifier for the control set.
--
-- 'assessmentReportSelectionCount', 'assessmentEvidenceFolder_assessmentReportSelectionCount' - The total count of evidence included in the assessment report.
--
-- 'totalEvidence', 'assessmentEvidenceFolder_totalEvidence' - The total amount of evidence in the evidence folder.
--
-- 'evidenceByTypeManualCount', 'assessmentEvidenceFolder_evidenceByTypeManualCount' - The number of evidence that falls under the manual category. This
-- evidence is imported manually.
--
-- 'date', 'assessmentEvidenceFolder_date' - The date when the first evidence was added to the evidence folder.
--
-- 'name', 'assessmentEvidenceFolder_name' - The name of the specified evidence folder.
--
-- 'evidenceByTypeUserActivityCount', 'assessmentEvidenceFolder_evidenceByTypeUserActivityCount' - The number of evidence that falls under the user activity category. This
-- evidence is collected from CloudTrail logs.
--
-- 'controlId', 'assessmentEvidenceFolder_controlId' - The unique identifier for the specified control.
--
-- 'evidenceAwsServiceSourceCount', 'assessmentEvidenceFolder_evidenceAwsServiceSourceCount' - The total number of Amazon Web Services resources assessed to generate
-- the evidence.
--
-- 'author', 'assessmentEvidenceFolder_author' - The name of the user who created the evidence folder.
--
-- 'id', 'assessmentEvidenceFolder_id' - The identifier for the folder in which evidence is stored.
--
-- 'dataSource', 'assessmentEvidenceFolder_dataSource' - The Amazon Web Service from which the evidence was collected.
--
-- 'controlName', 'assessmentEvidenceFolder_controlName' - The name of the control.
--
-- 'evidenceByTypeComplianceCheckCount', 'assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount' - The number of evidence that falls under the compliance check category.
-- This evidence is collected from Config or Security Hub.
--
-- 'assessmentId', 'assessmentEvidenceFolder_assessmentId' - The identifier for the specified assessment.
--
-- 'evidenceByTypeConfigurationDataCount', 'assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount' - The number of evidence that falls under the configuration data category.
-- This evidence is collected from configuration snapshots of other Amazon
-- Web Services services such as Amazon EC2, Amazon S3, or IAM.
--
-- 'evidenceResourcesIncludedCount', 'assessmentEvidenceFolder_evidenceResourcesIncludedCount' - The amount of evidence included in the evidence folder.
newAssessmentEvidenceFolder ::
  AssessmentEvidenceFolder
newAssessmentEvidenceFolder =
  AssessmentEvidenceFolder'
    { evidenceByTypeComplianceCheckIssuesCount =
        Prelude.Nothing,
      controlSetId = Prelude.Nothing,
      assessmentReportSelectionCount = Prelude.Nothing,
      totalEvidence = Prelude.Nothing,
      evidenceByTypeManualCount = Prelude.Nothing,
      date = Prelude.Nothing,
      name = Prelude.Nothing,
      evidenceByTypeUserActivityCount = Prelude.Nothing,
      controlId = Prelude.Nothing,
      evidenceAwsServiceSourceCount = Prelude.Nothing,
      author = Prelude.Nothing,
      id = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      controlName = Prelude.Nothing,
      evidenceByTypeComplianceCheckCount =
        Prelude.Nothing,
      assessmentId = Prelude.Nothing,
      evidenceByTypeConfigurationDataCount =
        Prelude.Nothing,
      evidenceResourcesIncludedCount = Prelude.Nothing
    }

-- | The total number of issues that were reported directly from Security
-- Hub, Config, or both.
assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeComplianceCheckIssuesCount} -> evidenceByTypeComplianceCheckIssuesCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeComplianceCheckIssuesCount = a} :: AssessmentEvidenceFolder)

-- | The identifier for the control set.
assessmentEvidenceFolder_controlSetId :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_controlSetId = Lens.lens (\AssessmentEvidenceFolder' {controlSetId} -> controlSetId) (\s@AssessmentEvidenceFolder' {} a -> s {controlSetId = a} :: AssessmentEvidenceFolder)

-- | The total count of evidence included in the assessment report.
assessmentEvidenceFolder_assessmentReportSelectionCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_assessmentReportSelectionCount = Lens.lens (\AssessmentEvidenceFolder' {assessmentReportSelectionCount} -> assessmentReportSelectionCount) (\s@AssessmentEvidenceFolder' {} a -> s {assessmentReportSelectionCount = a} :: AssessmentEvidenceFolder)

-- | The total amount of evidence in the evidence folder.
assessmentEvidenceFolder_totalEvidence :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_totalEvidence = Lens.lens (\AssessmentEvidenceFolder' {totalEvidence} -> totalEvidence) (\s@AssessmentEvidenceFolder' {} a -> s {totalEvidence = a} :: AssessmentEvidenceFolder)

-- | The number of evidence that falls under the manual category. This
-- evidence is imported manually.
assessmentEvidenceFolder_evidenceByTypeManualCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeManualCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeManualCount} -> evidenceByTypeManualCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeManualCount = a} :: AssessmentEvidenceFolder)

-- | The date when the first evidence was added to the evidence folder.
assessmentEvidenceFolder_date :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.UTCTime)
assessmentEvidenceFolder_date = Lens.lens (\AssessmentEvidenceFolder' {date} -> date) (\s@AssessmentEvidenceFolder' {} a -> s {date = a} :: AssessmentEvidenceFolder) Prelude.. Lens.mapping Core._Time

-- | The name of the specified evidence folder.
assessmentEvidenceFolder_name :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_name = Lens.lens (\AssessmentEvidenceFolder' {name} -> name) (\s@AssessmentEvidenceFolder' {} a -> s {name = a} :: AssessmentEvidenceFolder)

-- | The number of evidence that falls under the user activity category. This
-- evidence is collected from CloudTrail logs.
assessmentEvidenceFolder_evidenceByTypeUserActivityCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeUserActivityCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeUserActivityCount} -> evidenceByTypeUserActivityCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeUserActivityCount = a} :: AssessmentEvidenceFolder)

-- | The unique identifier for the specified control.
assessmentEvidenceFolder_controlId :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_controlId = Lens.lens (\AssessmentEvidenceFolder' {controlId} -> controlId) (\s@AssessmentEvidenceFolder' {} a -> s {controlId = a} :: AssessmentEvidenceFolder)

-- | The total number of Amazon Web Services resources assessed to generate
-- the evidence.
assessmentEvidenceFolder_evidenceAwsServiceSourceCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceAwsServiceSourceCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceAwsServiceSourceCount} -> evidenceAwsServiceSourceCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceAwsServiceSourceCount = a} :: AssessmentEvidenceFolder)

-- | The name of the user who created the evidence folder.
assessmentEvidenceFolder_author :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_author = Lens.lens (\AssessmentEvidenceFolder' {author} -> author) (\s@AssessmentEvidenceFolder' {} a -> s {author = a} :: AssessmentEvidenceFolder)

-- | The identifier for the folder in which evidence is stored.
assessmentEvidenceFolder_id :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_id = Lens.lens (\AssessmentEvidenceFolder' {id} -> id) (\s@AssessmentEvidenceFolder' {} a -> s {id = a} :: AssessmentEvidenceFolder)

-- | The Amazon Web Service from which the evidence was collected.
assessmentEvidenceFolder_dataSource :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_dataSource = Lens.lens (\AssessmentEvidenceFolder' {dataSource} -> dataSource) (\s@AssessmentEvidenceFolder' {} a -> s {dataSource = a} :: AssessmentEvidenceFolder)

-- | The name of the control.
assessmentEvidenceFolder_controlName :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_controlName = Lens.lens (\AssessmentEvidenceFolder' {controlName} -> controlName) (\s@AssessmentEvidenceFolder' {} a -> s {controlName = a} :: AssessmentEvidenceFolder)

-- | The number of evidence that falls under the compliance check category.
-- This evidence is collected from Config or Security Hub.
assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeComplianceCheckCount} -> evidenceByTypeComplianceCheckCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeComplianceCheckCount = a} :: AssessmentEvidenceFolder)

-- | The identifier for the specified assessment.
assessmentEvidenceFolder_assessmentId :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Text)
assessmentEvidenceFolder_assessmentId = Lens.lens (\AssessmentEvidenceFolder' {assessmentId} -> assessmentId) (\s@AssessmentEvidenceFolder' {} a -> s {assessmentId = a} :: AssessmentEvidenceFolder)

-- | The number of evidence that falls under the configuration data category.
-- This evidence is collected from configuration snapshots of other Amazon
-- Web Services services such as Amazon EC2, Amazon S3, or IAM.
assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceByTypeConfigurationDataCount} -> evidenceByTypeConfigurationDataCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceByTypeConfigurationDataCount = a} :: AssessmentEvidenceFolder)

-- | The amount of evidence included in the evidence folder.
assessmentEvidenceFolder_evidenceResourcesIncludedCount :: Lens.Lens' AssessmentEvidenceFolder (Prelude.Maybe Prelude.Int)
assessmentEvidenceFolder_evidenceResourcesIncludedCount = Lens.lens (\AssessmentEvidenceFolder' {evidenceResourcesIncludedCount} -> evidenceResourcesIncludedCount) (\s@AssessmentEvidenceFolder' {} a -> s {evidenceResourcesIncludedCount = a} :: AssessmentEvidenceFolder)

instance Core.FromJSON AssessmentEvidenceFolder where
  parseJSON =
    Core.withObject
      "AssessmentEvidenceFolder"
      ( \x ->
          AssessmentEvidenceFolder'
            Prelude.<$> ( x
                            Core..:? "evidenceByTypeComplianceCheckIssuesCount"
                        )
            Prelude.<*> (x Core..:? "controlSetId")
            Prelude.<*> (x Core..:? "assessmentReportSelectionCount")
            Prelude.<*> (x Core..:? "totalEvidence")
            Prelude.<*> (x Core..:? "evidenceByTypeManualCount")
            Prelude.<*> (x Core..:? "date")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "evidenceByTypeUserActivityCount")
            Prelude.<*> (x Core..:? "controlId")
            Prelude.<*> (x Core..:? "evidenceAwsServiceSourceCount")
            Prelude.<*> (x Core..:? "author")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "dataSource")
            Prelude.<*> (x Core..:? "controlName")
            Prelude.<*> (x Core..:? "evidenceByTypeComplianceCheckCount")
            Prelude.<*> (x Core..:? "assessmentId")
            Prelude.<*> (x Core..:? "evidenceByTypeConfigurationDataCount")
            Prelude.<*> (x Core..:? "evidenceResourcesIncludedCount")
      )

instance Prelude.Hashable AssessmentEvidenceFolder where
  hashWithSalt salt' AssessmentEvidenceFolder' {..} =
    salt'
      `Prelude.hashWithSalt` evidenceResourcesIncludedCount
      `Prelude.hashWithSalt` evidenceByTypeConfigurationDataCount
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` evidenceByTypeComplianceCheckCount
      `Prelude.hashWithSalt` controlName
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` evidenceAwsServiceSourceCount
      `Prelude.hashWithSalt` controlId
      `Prelude.hashWithSalt` evidenceByTypeUserActivityCount
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` evidenceByTypeManualCount
      `Prelude.hashWithSalt` totalEvidence
      `Prelude.hashWithSalt` assessmentReportSelectionCount
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` evidenceByTypeComplianceCheckIssuesCount

instance Prelude.NFData AssessmentEvidenceFolder where
  rnf AssessmentEvidenceFolder' {..} =
    Prelude.rnf
      evidenceByTypeComplianceCheckIssuesCount
      `Prelude.seq` Prelude.rnf evidenceResourcesIncludedCount
      `Prelude.seq` Prelude.rnf evidenceByTypeConfigurationDataCount
      `Prelude.seq` Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf evidenceByTypeComplianceCheckCount
      `Prelude.seq` Prelude.rnf controlName
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf evidenceAwsServiceSourceCount
      `Prelude.seq` Prelude.rnf controlId
      `Prelude.seq` Prelude.rnf evidenceByTypeUserActivityCount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf date
      `Prelude.seq` Prelude.rnf evidenceByTypeManualCount
      `Prelude.seq` Prelude.rnf totalEvidence
      `Prelude.seq` Prelude.rnf assessmentReportSelectionCount
      `Prelude.seq` Prelude.rnf controlSetId
