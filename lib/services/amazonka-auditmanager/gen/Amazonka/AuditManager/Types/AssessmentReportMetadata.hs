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
-- Module      : Amazonka.AuditManager.Types.AssessmentReportMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentReportMetadata where

import Amazonka.AuditManager.Types.AssessmentReportStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata objects associated with the specified assessment report.
--
-- /See:/ 'newAssessmentReportMetadata' smart constructor.
data AssessmentReportMetadata = AssessmentReportMetadata'
  { -- | Specifies when the assessment report was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The current status of the assessment report.
    status :: Prelude.Maybe AssessmentReportStatus,
    -- | The name of the assessment report.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the user who created the assessment report.
    author :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the assessment report.
    id :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the associated assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The description of the specified assessment report.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated assessment.
    assessmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentReportMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'assessmentReportMetadata_creationTime' - Specifies when the assessment report was created.
--
-- 'status', 'assessmentReportMetadata_status' - The current status of the assessment report.
--
-- 'name', 'assessmentReportMetadata_name' - The name of the assessment report.
--
-- 'author', 'assessmentReportMetadata_author' - The name of the user who created the assessment report.
--
-- 'id', 'assessmentReportMetadata_id' - The unique identifier for the assessment report.
--
-- 'assessmentId', 'assessmentReportMetadata_assessmentId' - The unique identifier for the associated assessment.
--
-- 'description', 'assessmentReportMetadata_description' - The description of the specified assessment report.
--
-- 'assessmentName', 'assessmentReportMetadata_assessmentName' - The name of the associated assessment.
newAssessmentReportMetadata ::
  AssessmentReportMetadata
newAssessmentReportMetadata =
  AssessmentReportMetadata'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      name = Prelude.Nothing,
      author = Prelude.Nothing,
      id = Prelude.Nothing,
      assessmentId = Prelude.Nothing,
      description = Prelude.Nothing,
      assessmentName = Prelude.Nothing
    }

-- | Specifies when the assessment report was created.
assessmentReportMetadata_creationTime :: Lens.Lens' AssessmentReportMetadata (Prelude.Maybe Prelude.UTCTime)
assessmentReportMetadata_creationTime = Lens.lens (\AssessmentReportMetadata' {creationTime} -> creationTime) (\s@AssessmentReportMetadata' {} a -> s {creationTime = a} :: AssessmentReportMetadata) Prelude.. Lens.mapping Core._Time

-- | The current status of the assessment report.
assessmentReportMetadata_status :: Lens.Lens' AssessmentReportMetadata (Prelude.Maybe AssessmentReportStatus)
assessmentReportMetadata_status = Lens.lens (\AssessmentReportMetadata' {status} -> status) (\s@AssessmentReportMetadata' {} a -> s {status = a} :: AssessmentReportMetadata)

-- | The name of the assessment report.
assessmentReportMetadata_name :: Lens.Lens' AssessmentReportMetadata (Prelude.Maybe Prelude.Text)
assessmentReportMetadata_name = Lens.lens (\AssessmentReportMetadata' {name} -> name) (\s@AssessmentReportMetadata' {} a -> s {name = a} :: AssessmentReportMetadata)

-- | The name of the user who created the assessment report.
assessmentReportMetadata_author :: Lens.Lens' AssessmentReportMetadata (Prelude.Maybe Prelude.Text)
assessmentReportMetadata_author = Lens.lens (\AssessmentReportMetadata' {author} -> author) (\s@AssessmentReportMetadata' {} a -> s {author = a} :: AssessmentReportMetadata)

-- | The unique identifier for the assessment report.
assessmentReportMetadata_id :: Lens.Lens' AssessmentReportMetadata (Prelude.Maybe Prelude.Text)
assessmentReportMetadata_id = Lens.lens (\AssessmentReportMetadata' {id} -> id) (\s@AssessmentReportMetadata' {} a -> s {id = a} :: AssessmentReportMetadata)

-- | The unique identifier for the associated assessment.
assessmentReportMetadata_assessmentId :: Lens.Lens' AssessmentReportMetadata (Prelude.Maybe Prelude.Text)
assessmentReportMetadata_assessmentId = Lens.lens (\AssessmentReportMetadata' {assessmentId} -> assessmentId) (\s@AssessmentReportMetadata' {} a -> s {assessmentId = a} :: AssessmentReportMetadata)

-- | The description of the specified assessment report.
assessmentReportMetadata_description :: Lens.Lens' AssessmentReportMetadata (Prelude.Maybe Prelude.Text)
assessmentReportMetadata_description = Lens.lens (\AssessmentReportMetadata' {description} -> description) (\s@AssessmentReportMetadata' {} a -> s {description = a} :: AssessmentReportMetadata)

-- | The name of the associated assessment.
assessmentReportMetadata_assessmentName :: Lens.Lens' AssessmentReportMetadata (Prelude.Maybe Prelude.Text)
assessmentReportMetadata_assessmentName = Lens.lens (\AssessmentReportMetadata' {assessmentName} -> assessmentName) (\s@AssessmentReportMetadata' {} a -> s {assessmentName = a} :: AssessmentReportMetadata)

instance Core.FromJSON AssessmentReportMetadata where
  parseJSON =
    Core.withObject
      "AssessmentReportMetadata"
      ( \x ->
          AssessmentReportMetadata'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "author")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "assessmentId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "assessmentName")
      )

instance Prelude.Hashable AssessmentReportMetadata where
  hashWithSalt salt' AssessmentReportMetadata' {..} =
    salt' `Prelude.hashWithSalt` assessmentName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData AssessmentReportMetadata where
  rnf AssessmentReportMetadata' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf assessmentName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
