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
-- Module      : Amazonka.AuditManager.Types.AssessmentReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentReport where

import Amazonka.AuditManager.Types.AssessmentReportStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A finalized document that\'s generated from an Audit Manager assessment.
-- These reports summarize the relevant evidence that was collected for
-- your audit, and link to the relevant evidence folders. These evidence
-- folders are named and organized according to the controls that are
-- specified in your assessment.
--
-- /See:/ 'newAssessmentReport' smart constructor.
data AssessmentReport = AssessmentReport'
  { -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated assessment.
    assessmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the user who created the assessment report.
    author :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified Amazon Web Services account.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the assessment report was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the specified assessment report.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the assessment report.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name that\'s given to the assessment report.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the specified assessment report.
    status :: Prelude.Maybe AssessmentReportStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'assessmentReport_assessmentId' - The identifier for the specified assessment.
--
-- 'assessmentName', 'assessmentReport_assessmentName' - The name of the associated assessment.
--
-- 'author', 'assessmentReport_author' - The name of the user who created the assessment report.
--
-- 'awsAccountId', 'assessmentReport_awsAccountId' - The identifier for the specified Amazon Web Services account.
--
-- 'creationTime', 'assessmentReport_creationTime' - Specifies when the assessment report was created.
--
-- 'description', 'assessmentReport_description' - The description of the specified assessment report.
--
-- 'id', 'assessmentReport_id' - The unique identifier for the assessment report.
--
-- 'name', 'assessmentReport_name' - The name that\'s given to the assessment report.
--
-- 'status', 'assessmentReport_status' - The current status of the specified assessment report.
newAssessmentReport ::
  AssessmentReport
newAssessmentReport =
  AssessmentReport'
    { assessmentId = Prelude.Nothing,
      assessmentName = Prelude.Nothing,
      author = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The identifier for the specified assessment.
assessmentReport_assessmentId :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_assessmentId = Lens.lens (\AssessmentReport' {assessmentId} -> assessmentId) (\s@AssessmentReport' {} a -> s {assessmentId = a} :: AssessmentReport)

-- | The name of the associated assessment.
assessmentReport_assessmentName :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_assessmentName = Lens.lens (\AssessmentReport' {assessmentName} -> assessmentName) (\s@AssessmentReport' {} a -> s {assessmentName = a} :: AssessmentReport)

-- | The name of the user who created the assessment report.
assessmentReport_author :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_author = Lens.lens (\AssessmentReport' {author} -> author) (\s@AssessmentReport' {} a -> s {author = a} :: AssessmentReport)

-- | The identifier for the specified Amazon Web Services account.
assessmentReport_awsAccountId :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_awsAccountId = Lens.lens (\AssessmentReport' {awsAccountId} -> awsAccountId) (\s@AssessmentReport' {} a -> s {awsAccountId = a} :: AssessmentReport)

-- | Specifies when the assessment report was created.
assessmentReport_creationTime :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.UTCTime)
assessmentReport_creationTime = Lens.lens (\AssessmentReport' {creationTime} -> creationTime) (\s@AssessmentReport' {} a -> s {creationTime = a} :: AssessmentReport) Prelude.. Lens.mapping Data._Time

-- | The description of the specified assessment report.
assessmentReport_description :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_description = Lens.lens (\AssessmentReport' {description} -> description) (\s@AssessmentReport' {} a -> s {description = a} :: AssessmentReport)

-- | The unique identifier for the assessment report.
assessmentReport_id :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_id = Lens.lens (\AssessmentReport' {id} -> id) (\s@AssessmentReport' {} a -> s {id = a} :: AssessmentReport)

-- | The name that\'s given to the assessment report.
assessmentReport_name :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_name = Lens.lens (\AssessmentReport' {name} -> name) (\s@AssessmentReport' {} a -> s {name = a} :: AssessmentReport)

-- | The current status of the specified assessment report.
assessmentReport_status :: Lens.Lens' AssessmentReport (Prelude.Maybe AssessmentReportStatus)
assessmentReport_status = Lens.lens (\AssessmentReport' {status} -> status) (\s@AssessmentReport' {} a -> s {status = a} :: AssessmentReport)

instance Data.FromJSON AssessmentReport where
  parseJSON =
    Data.withObject
      "AssessmentReport"
      ( \x ->
          AssessmentReport'
            Prelude.<$> (x Data..:? "assessmentId")
            Prelude.<*> (x Data..:? "assessmentName")
            Prelude.<*> (x Data..:? "author")
            Prelude.<*> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable AssessmentReport where
  hashWithSalt _salt AssessmentReport' {..} =
    _salt `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` assessmentName
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData AssessmentReport where
  rnf AssessmentReport' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf assessmentName
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
