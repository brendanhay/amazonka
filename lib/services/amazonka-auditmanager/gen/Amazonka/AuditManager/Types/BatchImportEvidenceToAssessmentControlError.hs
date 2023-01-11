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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.BatchImportEvidenceToAssessmentControlError where

import Amazonka.AuditManager.Types.ManualEvidence
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An error entity for the @BatchImportEvidenceToAssessmentControl@ API.
-- This is used to provide more meaningful errors than a simple string
-- message.
--
-- /See:/ 'newBatchImportEvidenceToAssessmentControlError' smart constructor.
data BatchImportEvidenceToAssessmentControlError = BatchImportEvidenceToAssessmentControlError'
  { -- | The error code that the @BatchImportEvidenceToAssessmentControl@ API
    -- returned.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message that the @BatchImportEvidenceToAssessmentControl@ API
    -- returned.
    errorMessage :: Prelude.Maybe Prelude.Text,
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
-- 'errorCode', 'batchImportEvidenceToAssessmentControlError_errorCode' - The error code that the @BatchImportEvidenceToAssessmentControl@ API
-- returned.
--
-- 'errorMessage', 'batchImportEvidenceToAssessmentControlError_errorMessage' - The error message that the @BatchImportEvidenceToAssessmentControl@ API
-- returned.
--
-- 'manualEvidence', 'batchImportEvidenceToAssessmentControlError_manualEvidence' - Manual evidence that can\'t be collected automatically by Audit Manager.
newBatchImportEvidenceToAssessmentControlError ::
  BatchImportEvidenceToAssessmentControlError
newBatchImportEvidenceToAssessmentControlError =
  BatchImportEvidenceToAssessmentControlError'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      manualEvidence =
        Prelude.Nothing
    }

-- | The error code that the @BatchImportEvidenceToAssessmentControl@ API
-- returned.
batchImportEvidenceToAssessmentControlError_errorCode :: Lens.Lens' BatchImportEvidenceToAssessmentControlError (Prelude.Maybe Prelude.Text)
batchImportEvidenceToAssessmentControlError_errorCode = Lens.lens (\BatchImportEvidenceToAssessmentControlError' {errorCode} -> errorCode) (\s@BatchImportEvidenceToAssessmentControlError' {} a -> s {errorCode = a} :: BatchImportEvidenceToAssessmentControlError)

-- | The error message that the @BatchImportEvidenceToAssessmentControl@ API
-- returned.
batchImportEvidenceToAssessmentControlError_errorMessage :: Lens.Lens' BatchImportEvidenceToAssessmentControlError (Prelude.Maybe Prelude.Text)
batchImportEvidenceToAssessmentControlError_errorMessage = Lens.lens (\BatchImportEvidenceToAssessmentControlError' {errorMessage} -> errorMessage) (\s@BatchImportEvidenceToAssessmentControlError' {} a -> s {errorMessage = a} :: BatchImportEvidenceToAssessmentControlError)

-- | Manual evidence that can\'t be collected automatically by Audit Manager.
batchImportEvidenceToAssessmentControlError_manualEvidence :: Lens.Lens' BatchImportEvidenceToAssessmentControlError (Prelude.Maybe ManualEvidence)
batchImportEvidenceToAssessmentControlError_manualEvidence = Lens.lens (\BatchImportEvidenceToAssessmentControlError' {manualEvidence} -> manualEvidence) (\s@BatchImportEvidenceToAssessmentControlError' {} a -> s {manualEvidence = a} :: BatchImportEvidenceToAssessmentControlError)

instance
  Data.FromJSON
    BatchImportEvidenceToAssessmentControlError
  where
  parseJSON =
    Data.withObject
      "BatchImportEvidenceToAssessmentControlError"
      ( \x ->
          BatchImportEvidenceToAssessmentControlError'
            Prelude.<$> (x Data..:? "errorCode")
              Prelude.<*> (x Data..:? "errorMessage")
              Prelude.<*> (x Data..:? "manualEvidence")
      )

instance
  Prelude.Hashable
    BatchImportEvidenceToAssessmentControlError
  where
  hashWithSalt
    _salt
    BatchImportEvidenceToAssessmentControlError' {..} =
      _salt `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` manualEvidence

instance
  Prelude.NFData
    BatchImportEvidenceToAssessmentControlError
  where
  rnf BatchImportEvidenceToAssessmentControlError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf manualEvidence
