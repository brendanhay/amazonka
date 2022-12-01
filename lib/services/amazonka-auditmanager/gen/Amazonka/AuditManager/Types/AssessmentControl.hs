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
-- Module      : Amazonka.AuditManager.Types.AssessmentControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentControl where

import Amazonka.AuditManager.Types.ControlComment
import Amazonka.AuditManager.Types.ControlResponse
import Amazonka.AuditManager.Types.ControlStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The control entity that represents a standard control or a custom
-- control in an Audit Manager assessment.
--
-- /See:/ 'newAssessmentControl' smart constructor.
data AssessmentControl = AssessmentControl'
  { -- | The name of the control.
    name :: Prelude.Maybe Prelude.Text,
    -- | The amount of evidence that\'s generated for the control.
    evidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The response of the control.
    response :: Prelude.Maybe ControlResponse,
    -- | The amount of evidence in the assessment report.
    assessmentReportEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The status of the control.
    status :: Prelude.Maybe ControlStatus,
    -- | The description of the control.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the control.
    id :: Prelude.Maybe Prelude.Text,
    -- | The list of comments that\'s attached to the control.
    comments :: Prelude.Maybe [ControlComment],
    -- | The list of data sources for the evidence.
    evidenceSources :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assessmentControl_name' - The name of the control.
--
-- 'evidenceCount', 'assessmentControl_evidenceCount' - The amount of evidence that\'s generated for the control.
--
-- 'response', 'assessmentControl_response' - The response of the control.
--
-- 'assessmentReportEvidenceCount', 'assessmentControl_assessmentReportEvidenceCount' - The amount of evidence in the assessment report.
--
-- 'status', 'assessmentControl_status' - The status of the control.
--
-- 'description', 'assessmentControl_description' - The description of the control.
--
-- 'id', 'assessmentControl_id' - The identifier for the control.
--
-- 'comments', 'assessmentControl_comments' - The list of comments that\'s attached to the control.
--
-- 'evidenceSources', 'assessmentControl_evidenceSources' - The list of data sources for the evidence.
newAssessmentControl ::
  AssessmentControl
newAssessmentControl =
  AssessmentControl'
    { name = Prelude.Nothing,
      evidenceCount = Prelude.Nothing,
      response = Prelude.Nothing,
      assessmentReportEvidenceCount = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      comments = Prelude.Nothing,
      evidenceSources = Prelude.Nothing
    }

-- | The name of the control.
assessmentControl_name :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Text)
assessmentControl_name = Lens.lens (\AssessmentControl' {name} -> name) (\s@AssessmentControl' {} a -> s {name = a} :: AssessmentControl)

-- | The amount of evidence that\'s generated for the control.
assessmentControl_evidenceCount :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Int)
assessmentControl_evidenceCount = Lens.lens (\AssessmentControl' {evidenceCount} -> evidenceCount) (\s@AssessmentControl' {} a -> s {evidenceCount = a} :: AssessmentControl)

-- | The response of the control.
assessmentControl_response :: Lens.Lens' AssessmentControl (Prelude.Maybe ControlResponse)
assessmentControl_response = Lens.lens (\AssessmentControl' {response} -> response) (\s@AssessmentControl' {} a -> s {response = a} :: AssessmentControl)

-- | The amount of evidence in the assessment report.
assessmentControl_assessmentReportEvidenceCount :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Int)
assessmentControl_assessmentReportEvidenceCount = Lens.lens (\AssessmentControl' {assessmentReportEvidenceCount} -> assessmentReportEvidenceCount) (\s@AssessmentControl' {} a -> s {assessmentReportEvidenceCount = a} :: AssessmentControl)

-- | The status of the control.
assessmentControl_status :: Lens.Lens' AssessmentControl (Prelude.Maybe ControlStatus)
assessmentControl_status = Lens.lens (\AssessmentControl' {status} -> status) (\s@AssessmentControl' {} a -> s {status = a} :: AssessmentControl)

-- | The description of the control.
assessmentControl_description :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Text)
assessmentControl_description = Lens.lens (\AssessmentControl' {description} -> description) (\s@AssessmentControl' {} a -> s {description = a} :: AssessmentControl)

-- | The identifier for the control.
assessmentControl_id :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Text)
assessmentControl_id = Lens.lens (\AssessmentControl' {id} -> id) (\s@AssessmentControl' {} a -> s {id = a} :: AssessmentControl)

-- | The list of comments that\'s attached to the control.
assessmentControl_comments :: Lens.Lens' AssessmentControl (Prelude.Maybe [ControlComment])
assessmentControl_comments = Lens.lens (\AssessmentControl' {comments} -> comments) (\s@AssessmentControl' {} a -> s {comments = a} :: AssessmentControl) Prelude.. Lens.mapping Lens.coerced

-- | The list of data sources for the evidence.
assessmentControl_evidenceSources :: Lens.Lens' AssessmentControl (Prelude.Maybe [Prelude.Text])
assessmentControl_evidenceSources = Lens.lens (\AssessmentControl' {evidenceSources} -> evidenceSources) (\s@AssessmentControl' {} a -> s {evidenceSources = a} :: AssessmentControl) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AssessmentControl where
  parseJSON =
    Core.withObject
      "AssessmentControl"
      ( \x ->
          AssessmentControl'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "evidenceCount")
            Prelude.<*> (x Core..:? "response")
            Prelude.<*> (x Core..:? "assessmentReportEvidenceCount")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "comments" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "evidenceSources"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AssessmentControl where
  hashWithSalt _salt AssessmentControl' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` evidenceCount
      `Prelude.hashWithSalt` response
      `Prelude.hashWithSalt` assessmentReportEvidenceCount
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` comments
      `Prelude.hashWithSalt` evidenceSources

instance Prelude.NFData AssessmentControl where
  rnf AssessmentControl' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf evidenceCount
      `Prelude.seq` Prelude.rnf response
      `Prelude.seq` Prelude.rnf assessmentReportEvidenceCount
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf comments
      `Prelude.seq` Prelude.rnf evidenceSources
