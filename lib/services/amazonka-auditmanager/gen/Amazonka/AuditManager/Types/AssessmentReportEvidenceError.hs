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
-- Module      : Amazonka.AuditManager.Types.AssessmentReportEvidenceError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentReportEvidenceError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An error entity for assessment report evidence errors. This is used to
-- provide more meaningful errors than a simple string message.
--
-- /See:/ 'newAssessmentReportEvidenceError' smart constructor.
data AssessmentReportEvidenceError = AssessmentReportEvidenceError'
  { -- | The error code that was returned.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message that was returned.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the evidence.
    evidenceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentReportEvidenceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'assessmentReportEvidenceError_errorCode' - The error code that was returned.
--
-- 'errorMessage', 'assessmentReportEvidenceError_errorMessage' - The error message that was returned.
--
-- 'evidenceId', 'assessmentReportEvidenceError_evidenceId' - The identifier for the evidence.
newAssessmentReportEvidenceError ::
  AssessmentReportEvidenceError
newAssessmentReportEvidenceError =
  AssessmentReportEvidenceError'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      evidenceId = Prelude.Nothing
    }

-- | The error code that was returned.
assessmentReportEvidenceError_errorCode :: Lens.Lens' AssessmentReportEvidenceError (Prelude.Maybe Prelude.Text)
assessmentReportEvidenceError_errorCode = Lens.lens (\AssessmentReportEvidenceError' {errorCode} -> errorCode) (\s@AssessmentReportEvidenceError' {} a -> s {errorCode = a} :: AssessmentReportEvidenceError)

-- | The error message that was returned.
assessmentReportEvidenceError_errorMessage :: Lens.Lens' AssessmentReportEvidenceError (Prelude.Maybe Prelude.Text)
assessmentReportEvidenceError_errorMessage = Lens.lens (\AssessmentReportEvidenceError' {errorMessage} -> errorMessage) (\s@AssessmentReportEvidenceError' {} a -> s {errorMessage = a} :: AssessmentReportEvidenceError)

-- | The identifier for the evidence.
assessmentReportEvidenceError_evidenceId :: Lens.Lens' AssessmentReportEvidenceError (Prelude.Maybe Prelude.Text)
assessmentReportEvidenceError_evidenceId = Lens.lens (\AssessmentReportEvidenceError' {evidenceId} -> evidenceId) (\s@AssessmentReportEvidenceError' {} a -> s {evidenceId = a} :: AssessmentReportEvidenceError)

instance Data.FromJSON AssessmentReportEvidenceError where
  parseJSON =
    Data.withObject
      "AssessmentReportEvidenceError"
      ( \x ->
          AssessmentReportEvidenceError'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "evidenceId")
      )

instance
  Prelude.Hashable
    AssessmentReportEvidenceError
  where
  hashWithSalt _salt AssessmentReportEvidenceError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` evidenceId

instance Prelude.NFData AssessmentReportEvidenceError where
  rnf AssessmentReportEvidenceError' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf evidenceId
