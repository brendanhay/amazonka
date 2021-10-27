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
-- Module      : Network.AWS.AuditManager.Types.AssessmentControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.AssessmentControl where

import Network.AWS.AuditManager.Types.ControlComment
import Network.AWS.AuditManager.Types.ControlResponse
import Network.AWS.AuditManager.Types.ControlStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The control entity that represents a standard or custom control used in
-- an Audit Manager assessment.
--
-- /See:/ 'newAssessmentControl' smart constructor.
data AssessmentControl = AssessmentControl'
  { -- | The status of the specified control.
    status :: Prelude.Maybe ControlStatus,
    -- | The amount of evidence generated for the control.
    evidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The response of the specified control.
    response :: Prelude.Maybe ControlResponse,
    -- | The name of the specified control.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified control.
    id :: Prelude.Maybe Prelude.Text,
    -- | The list of data sources for the specified evidence.
    evidenceSources :: Prelude.Maybe [Prelude.Text],
    -- | The list of comments attached to the specified control.
    comments :: Prelude.Maybe [ControlComment],
    -- | The amount of evidence in the assessment report.
    assessmentReportEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The description of the specified control.
    description :: Prelude.Maybe Prelude.Text
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
-- 'status', 'assessmentControl_status' - The status of the specified control.
--
-- 'evidenceCount', 'assessmentControl_evidenceCount' - The amount of evidence generated for the control.
--
-- 'response', 'assessmentControl_response' - The response of the specified control.
--
-- 'name', 'assessmentControl_name' - The name of the specified control.
--
-- 'id', 'assessmentControl_id' - The identifier for the specified control.
--
-- 'evidenceSources', 'assessmentControl_evidenceSources' - The list of data sources for the specified evidence.
--
-- 'comments', 'assessmentControl_comments' - The list of comments attached to the specified control.
--
-- 'assessmentReportEvidenceCount', 'assessmentControl_assessmentReportEvidenceCount' - The amount of evidence in the assessment report.
--
-- 'description', 'assessmentControl_description' - The description of the specified control.
newAssessmentControl ::
  AssessmentControl
newAssessmentControl =
  AssessmentControl'
    { status = Prelude.Nothing,
      evidenceCount = Prelude.Nothing,
      response = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      evidenceSources = Prelude.Nothing,
      comments = Prelude.Nothing,
      assessmentReportEvidenceCount = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The status of the specified control.
assessmentControl_status :: Lens.Lens' AssessmentControl (Prelude.Maybe ControlStatus)
assessmentControl_status = Lens.lens (\AssessmentControl' {status} -> status) (\s@AssessmentControl' {} a -> s {status = a} :: AssessmentControl)

-- | The amount of evidence generated for the control.
assessmentControl_evidenceCount :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Int)
assessmentControl_evidenceCount = Lens.lens (\AssessmentControl' {evidenceCount} -> evidenceCount) (\s@AssessmentControl' {} a -> s {evidenceCount = a} :: AssessmentControl)

-- | The response of the specified control.
assessmentControl_response :: Lens.Lens' AssessmentControl (Prelude.Maybe ControlResponse)
assessmentControl_response = Lens.lens (\AssessmentControl' {response} -> response) (\s@AssessmentControl' {} a -> s {response = a} :: AssessmentControl)

-- | The name of the specified control.
assessmentControl_name :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Text)
assessmentControl_name = Lens.lens (\AssessmentControl' {name} -> name) (\s@AssessmentControl' {} a -> s {name = a} :: AssessmentControl)

-- | The identifier for the specified control.
assessmentControl_id :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Text)
assessmentControl_id = Lens.lens (\AssessmentControl' {id} -> id) (\s@AssessmentControl' {} a -> s {id = a} :: AssessmentControl)

-- | The list of data sources for the specified evidence.
assessmentControl_evidenceSources :: Lens.Lens' AssessmentControl (Prelude.Maybe [Prelude.Text])
assessmentControl_evidenceSources = Lens.lens (\AssessmentControl' {evidenceSources} -> evidenceSources) (\s@AssessmentControl' {} a -> s {evidenceSources = a} :: AssessmentControl) Prelude.. Lens.mapping Lens.coerced

-- | The list of comments attached to the specified control.
assessmentControl_comments :: Lens.Lens' AssessmentControl (Prelude.Maybe [ControlComment])
assessmentControl_comments = Lens.lens (\AssessmentControl' {comments} -> comments) (\s@AssessmentControl' {} a -> s {comments = a} :: AssessmentControl) Prelude.. Lens.mapping Lens.coerced

-- | The amount of evidence in the assessment report.
assessmentControl_assessmentReportEvidenceCount :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Int)
assessmentControl_assessmentReportEvidenceCount = Lens.lens (\AssessmentControl' {assessmentReportEvidenceCount} -> assessmentReportEvidenceCount) (\s@AssessmentControl' {} a -> s {assessmentReportEvidenceCount = a} :: AssessmentControl)

-- | The description of the specified control.
assessmentControl_description :: Lens.Lens' AssessmentControl (Prelude.Maybe Prelude.Text)
assessmentControl_description = Lens.lens (\AssessmentControl' {description} -> description) (\s@AssessmentControl' {} a -> s {description = a} :: AssessmentControl)

instance Core.FromJSON AssessmentControl where
  parseJSON =
    Core.withObject
      "AssessmentControl"
      ( \x ->
          AssessmentControl'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "evidenceCount")
            Prelude.<*> (x Core..:? "response")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> ( x Core..:? "evidenceSources"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "comments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "assessmentReportEvidenceCount")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable AssessmentControl

instance Prelude.NFData AssessmentControl
