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
-- Module      : Amazonka.AuditManager.Types.BatchDeleteDelegationByAssessmentError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.BatchDeleteDelegationByAssessmentError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An error entity for the @BatchDeleteDelegationByAssessment@ API. This is
-- used to provide more meaningful errors than a simple string message.
--
-- /See:/ 'newBatchDeleteDelegationByAssessmentError' smart constructor.
data BatchDeleteDelegationByAssessmentError = BatchDeleteDelegationByAssessmentError'
  { -- | The error message that the @BatchDeleteDelegationByAssessment@ API
    -- returned.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the delegation.
    delegationId :: Prelude.Maybe Prelude.Text,
    -- | The error code that the @BatchDeleteDelegationByAssessment@ API
    -- returned.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDelegationByAssessmentError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchDeleteDelegationByAssessmentError_errorMessage' - The error message that the @BatchDeleteDelegationByAssessment@ API
-- returned.
--
-- 'delegationId', 'batchDeleteDelegationByAssessmentError_delegationId' - The identifier for the delegation.
--
-- 'errorCode', 'batchDeleteDelegationByAssessmentError_errorCode' - The error code that the @BatchDeleteDelegationByAssessment@ API
-- returned.
newBatchDeleteDelegationByAssessmentError ::
  BatchDeleteDelegationByAssessmentError
newBatchDeleteDelegationByAssessmentError =
  BatchDeleteDelegationByAssessmentError'
    { errorMessage =
        Prelude.Nothing,
      delegationId = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message that the @BatchDeleteDelegationByAssessment@ API
-- returned.
batchDeleteDelegationByAssessmentError_errorMessage :: Lens.Lens' BatchDeleteDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchDeleteDelegationByAssessmentError_errorMessage = Lens.lens (\BatchDeleteDelegationByAssessmentError' {errorMessage} -> errorMessage) (\s@BatchDeleteDelegationByAssessmentError' {} a -> s {errorMessage = a} :: BatchDeleteDelegationByAssessmentError)

-- | The identifier for the delegation.
batchDeleteDelegationByAssessmentError_delegationId :: Lens.Lens' BatchDeleteDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchDeleteDelegationByAssessmentError_delegationId = Lens.lens (\BatchDeleteDelegationByAssessmentError' {delegationId} -> delegationId) (\s@BatchDeleteDelegationByAssessmentError' {} a -> s {delegationId = a} :: BatchDeleteDelegationByAssessmentError)

-- | The error code that the @BatchDeleteDelegationByAssessment@ API
-- returned.
batchDeleteDelegationByAssessmentError_errorCode :: Lens.Lens' BatchDeleteDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchDeleteDelegationByAssessmentError_errorCode = Lens.lens (\BatchDeleteDelegationByAssessmentError' {errorCode} -> errorCode) (\s@BatchDeleteDelegationByAssessmentError' {} a -> s {errorCode = a} :: BatchDeleteDelegationByAssessmentError)

instance
  Core.FromJSON
    BatchDeleteDelegationByAssessmentError
  where
  parseJSON =
    Core.withObject
      "BatchDeleteDelegationByAssessmentError"
      ( \x ->
          BatchDeleteDelegationByAssessmentError'
            Prelude.<$> (x Core..:? "errorMessage")
            Prelude.<*> (x Core..:? "delegationId")
            Prelude.<*> (x Core..:? "errorCode")
      )

instance
  Prelude.Hashable
    BatchDeleteDelegationByAssessmentError
  where
  hashWithSalt
    _salt
    BatchDeleteDelegationByAssessmentError' {..} =
      _salt `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` delegationId
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    BatchDeleteDelegationByAssessmentError
  where
  rnf BatchDeleteDelegationByAssessmentError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf delegationId
      `Prelude.seq` Prelude.rnf errorCode
