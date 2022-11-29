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
-- Module      : Amazonka.AuditManager.Types.BatchImportEvidenceToAssessmentControlError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.BatchImportEvidenceToAssessmentControlError where

import Amazonka.AuditManager.Types.ManualEvidence
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An error entity for the @BatchImportEvidenceToAssessmentControl@ API.
-- This is used to provide more meaningful errors than a simple string
-- message.
--
-- /See:/ 'newBatchImportEvidenceToAssessmentControlError' smart constructor.
data BatchImportEvidenceToAssessmentControlError = BatchImportEvidenceToAssessmentControlError'
  { -- | The error message that the @BatchImportEvidenceToAssessmentControl@ API
    -- returned.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code that the @BatchImportEvidenceToAssessmentControl@ API
    -- returned.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | Manual evidence that can\'t be collected automatically by Audit Manager.
    manualEvidence :: Prelude.Maybe ManualEvidence
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchImportEvidenceToAssessmentControlError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchImportEvidenceToAssessmentControlError_errorMessage' - The error message that the @BatchImportEvidenceToAssessmentControl@ API
-- returned.
--
-- 'errorCode', 'batchImportEvidenceToAssessmentControlError_errorCode' - The error code that the @BatchImportEvidenceToAssessmentControl@ API
-- returned.
--
-- 'manualEvidence', 'batchImportEvidenceToAssessmentControlError_manualEvidence' - Manual evidence that can\'t be collected automatically by Audit Manager.
newBatchImportEvidenceToAssessmentControlError ::
  BatchImportEvidenceToAssessmentControlError
newBatchImportEvidenceToAssessmentControlError =
  BatchImportEvidenceToAssessmentControlError'
    { errorMessage =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      manualEvidence =
        Prelude.Nothing
    }

-- | The error message that the @BatchImportEvidenceToAssessmentControl@ API
-- returned.
batchImportEvidenceToAssessmentControlError_errorMessage :: Lens.Lens' BatchImportEvidenceToAssessmentControlError (Prelude.Maybe Prelude.Text)
batchImportEvidenceToAssessmentControlError_errorMessage = Lens.lens (\BatchImportEvidenceToAssessmentControlError' {errorMessage} -> errorMessage) (\s@BatchImportEvidenceToAssessmentControlError' {} a -> s {errorMessage = a} :: BatchImportEvidenceToAssessmentControlError)

-- | The error code that the @BatchImportEvidenceToAssessmentControl@ API
-- returned.
batchImportEvidenceToAssessmentControlError_errorCode :: Lens.Lens' BatchImportEvidenceToAssessmentControlError (Prelude.Maybe Prelude.Text)
batchImportEvidenceToAssessmentControlError_errorCode = Lens.lens (\BatchImportEvidenceToAssessmentControlError' {errorCode} -> errorCode) (\s@BatchImportEvidenceToAssessmentControlError' {} a -> s {errorCode = a} :: BatchImportEvidenceToAssessmentControlError)

-- | Manual evidence that can\'t be collected automatically by Audit Manager.
batchImportEvidenceToAssessmentControlError_manualEvidence :: Lens.Lens' BatchImportEvidenceToAssessmentControlError (Prelude.Maybe ManualEvidence)
batchImportEvidenceToAssessmentControlError_manualEvidence = Lens.lens (\BatchImportEvidenceToAssessmentControlError' {manualEvidence} -> manualEvidence) (\s@BatchImportEvidenceToAssessmentControlError' {} a -> s {manualEvidence = a} :: BatchImportEvidenceToAssessmentControlError)

instance
  Core.FromJSON
    BatchImportEvidenceToAssessmentControlError
  where
  parseJSON =
    Core.withObject
      "BatchImportEvidenceToAssessmentControlError"
      ( \x ->
          BatchImportEvidenceToAssessmentControlError'
            Prelude.<$> (x Core..:? "errorMessage")
              Prelude.<*> (x Core..:? "errorCode")
              Prelude.<*> (x Core..:? "manualEvidence")
      )

instance
  Prelude.Hashable
    BatchImportEvidenceToAssessmentControlError
  where
  hashWithSalt
    _salt
    BatchImportEvidenceToAssessmentControlError' {..} =
      _salt `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` manualEvidence

instance
  Prelude.NFData
    BatchImportEvidenceToAssessmentControlError
  where
  rnf BatchImportEvidenceToAssessmentControlError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf manualEvidence
