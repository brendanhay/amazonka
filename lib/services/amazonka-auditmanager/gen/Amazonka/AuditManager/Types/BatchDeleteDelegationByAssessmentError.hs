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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.BatchDeleteDelegationByAssessmentError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An error entity for the @BatchDeleteDelegationByAssessment@ API. This is
-- used to provide more meaningful errors than a simple string message.
--
-- /See:/ 'newBatchDeleteDelegationByAssessmentError' smart constructor.
data BatchDeleteDelegationByAssessmentError = BatchDeleteDelegationByAssessmentError'
  { -- | The identifier for the delegation.
    delegationId :: Prelude.Maybe Prelude.Text,
    -- | The error code that the @BatchDeleteDelegationByAssessment@ API
    -- returned.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message that the @BatchDeleteDelegationByAssessment@ API
    -- returned.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'delegationId', 'batchDeleteDelegationByAssessmentError_delegationId' - The identifier for the delegation.
--
-- 'errorCode', 'batchDeleteDelegationByAssessmentError_errorCode' - The error code that the @BatchDeleteDelegationByAssessment@ API
-- returned.
--
-- 'errorMessage', 'batchDeleteDelegationByAssessmentError_errorMessage' - The error message that the @BatchDeleteDelegationByAssessment@ API
-- returned.
newBatchDeleteDelegationByAssessmentError ::
  BatchDeleteDelegationByAssessmentError
newBatchDeleteDelegationByAssessmentError =
  BatchDeleteDelegationByAssessmentError'
    { delegationId =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The identifier for the delegation.
batchDeleteDelegationByAssessmentError_delegationId :: Lens.Lens' BatchDeleteDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchDeleteDelegationByAssessmentError_delegationId = Lens.lens (\BatchDeleteDelegationByAssessmentError' {delegationId} -> delegationId) (\s@BatchDeleteDelegationByAssessmentError' {} a -> s {delegationId = a} :: BatchDeleteDelegationByAssessmentError)

-- | The error code that the @BatchDeleteDelegationByAssessment@ API
-- returned.
batchDeleteDelegationByAssessmentError_errorCode :: Lens.Lens' BatchDeleteDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchDeleteDelegationByAssessmentError_errorCode = Lens.lens (\BatchDeleteDelegationByAssessmentError' {errorCode} -> errorCode) (\s@BatchDeleteDelegationByAssessmentError' {} a -> s {errorCode = a} :: BatchDeleteDelegationByAssessmentError)

-- | The error message that the @BatchDeleteDelegationByAssessment@ API
-- returned.
batchDeleteDelegationByAssessmentError_errorMessage :: Lens.Lens' BatchDeleteDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchDeleteDelegationByAssessmentError_errorMessage = Lens.lens (\BatchDeleteDelegationByAssessmentError' {errorMessage} -> errorMessage) (\s@BatchDeleteDelegationByAssessmentError' {} a -> s {errorMessage = a} :: BatchDeleteDelegationByAssessmentError)

instance
  Data.FromJSON
    BatchDeleteDelegationByAssessmentError
  where
  parseJSON =
    Data.withObject
      "BatchDeleteDelegationByAssessmentError"
      ( \x ->
          BatchDeleteDelegationByAssessmentError'
            Prelude.<$> (x Data..:? "delegationId")
            Prelude.<*> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
      )

instance
  Prelude.Hashable
    BatchDeleteDelegationByAssessmentError
  where
  hashWithSalt
    _salt
    BatchDeleteDelegationByAssessmentError' {..} =
      _salt
        `Prelude.hashWithSalt` delegationId
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage

instance
  Prelude.NFData
    BatchDeleteDelegationByAssessmentError
  where
  rnf BatchDeleteDelegationByAssessmentError' {..} =
    Prelude.rnf delegationId `Prelude.seq`
      Prelude.rnf errorCode `Prelude.seq`
        Prelude.rnf errorMessage
