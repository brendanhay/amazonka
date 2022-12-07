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
-- Module      : Amazonka.AuditManager.Types.BatchCreateDelegationByAssessmentError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.BatchCreateDelegationByAssessmentError where

import Amazonka.AuditManager.Types.CreateDelegationRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An error entity for the @BatchCreateDelegationByAssessment@ API. This is
-- used to provide more meaningful errors than a simple string message.
--
-- /See:/ 'newBatchCreateDelegationByAssessmentError' smart constructor.
data BatchCreateDelegationByAssessmentError = BatchCreateDelegationByAssessmentError'
  { -- | The API request to batch create delegations in Audit Manager.
    createDelegationRequest :: Prelude.Maybe CreateDelegationRequest,
    -- | The error message that the @BatchCreateDelegationByAssessment@ API
    -- returned.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code that the @BatchCreateDelegationByAssessment@ API
    -- returned.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateDelegationByAssessmentError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDelegationRequest', 'batchCreateDelegationByAssessmentError_createDelegationRequest' - The API request to batch create delegations in Audit Manager.
--
-- 'errorMessage', 'batchCreateDelegationByAssessmentError_errorMessage' - The error message that the @BatchCreateDelegationByAssessment@ API
-- returned.
--
-- 'errorCode', 'batchCreateDelegationByAssessmentError_errorCode' - The error code that the @BatchCreateDelegationByAssessment@ API
-- returned.
newBatchCreateDelegationByAssessmentError ::
  BatchCreateDelegationByAssessmentError
newBatchCreateDelegationByAssessmentError =
  BatchCreateDelegationByAssessmentError'
    { createDelegationRequest =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The API request to batch create delegations in Audit Manager.
batchCreateDelegationByAssessmentError_createDelegationRequest :: Lens.Lens' BatchCreateDelegationByAssessmentError (Prelude.Maybe CreateDelegationRequest)
batchCreateDelegationByAssessmentError_createDelegationRequest = Lens.lens (\BatchCreateDelegationByAssessmentError' {createDelegationRequest} -> createDelegationRequest) (\s@BatchCreateDelegationByAssessmentError' {} a -> s {createDelegationRequest = a} :: BatchCreateDelegationByAssessmentError)

-- | The error message that the @BatchCreateDelegationByAssessment@ API
-- returned.
batchCreateDelegationByAssessmentError_errorMessage :: Lens.Lens' BatchCreateDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchCreateDelegationByAssessmentError_errorMessage = Lens.lens (\BatchCreateDelegationByAssessmentError' {errorMessage} -> errorMessage) (\s@BatchCreateDelegationByAssessmentError' {} a -> s {errorMessage = a} :: BatchCreateDelegationByAssessmentError)

-- | The error code that the @BatchCreateDelegationByAssessment@ API
-- returned.
batchCreateDelegationByAssessmentError_errorCode :: Lens.Lens' BatchCreateDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchCreateDelegationByAssessmentError_errorCode = Lens.lens (\BatchCreateDelegationByAssessmentError' {errorCode} -> errorCode) (\s@BatchCreateDelegationByAssessmentError' {} a -> s {errorCode = a} :: BatchCreateDelegationByAssessmentError)

instance
  Data.FromJSON
    BatchCreateDelegationByAssessmentError
  where
  parseJSON =
    Data.withObject
      "BatchCreateDelegationByAssessmentError"
      ( \x ->
          BatchCreateDelegationByAssessmentError'
            Prelude.<$> (x Data..:? "createDelegationRequest")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "errorCode")
      )

instance
  Prelude.Hashable
    BatchCreateDelegationByAssessmentError
  where
  hashWithSalt
    _salt
    BatchCreateDelegationByAssessmentError' {..} =
      _salt
        `Prelude.hashWithSalt` createDelegationRequest
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    BatchCreateDelegationByAssessmentError
  where
  rnf BatchCreateDelegationByAssessmentError' {..} =
    Prelude.rnf createDelegationRequest
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
