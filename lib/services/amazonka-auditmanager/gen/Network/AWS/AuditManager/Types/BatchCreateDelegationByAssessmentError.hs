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
-- Module      : Network.AWS.AuditManager.Types.BatchCreateDelegationByAssessmentError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.BatchCreateDelegationByAssessmentError where

import Network.AWS.AuditManager.Types.CreateDelegationRequest
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An error entity for the @BatchCreateDelegationByAssessment@ API. This is
-- used to provide more meaningful errors than a simple string message.
--
-- /See:/ 'newBatchCreateDelegationByAssessmentError' smart constructor.
data BatchCreateDelegationByAssessmentError = BatchCreateDelegationByAssessmentError'
  { -- | The API request to batch create delegations in Audit Manager.
    createDelegationRequest :: Prelude.Maybe CreateDelegationRequest,
    -- | The error code returned by the @BatchCreateDelegationByAssessment@ API.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message returned by the @BatchCreateDelegationByAssessment@
    -- API.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'errorCode', 'batchCreateDelegationByAssessmentError_errorCode' - The error code returned by the @BatchCreateDelegationByAssessment@ API.
--
-- 'errorMessage', 'batchCreateDelegationByAssessmentError_errorMessage' - The error message returned by the @BatchCreateDelegationByAssessment@
-- API.
newBatchCreateDelegationByAssessmentError ::
  BatchCreateDelegationByAssessmentError
newBatchCreateDelegationByAssessmentError =
  BatchCreateDelegationByAssessmentError'
    { createDelegationRequest =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The API request to batch create delegations in Audit Manager.
batchCreateDelegationByAssessmentError_createDelegationRequest :: Lens.Lens' BatchCreateDelegationByAssessmentError (Prelude.Maybe CreateDelegationRequest)
batchCreateDelegationByAssessmentError_createDelegationRequest = Lens.lens (\BatchCreateDelegationByAssessmentError' {createDelegationRequest} -> createDelegationRequest) (\s@BatchCreateDelegationByAssessmentError' {} a -> s {createDelegationRequest = a} :: BatchCreateDelegationByAssessmentError)

-- | The error code returned by the @BatchCreateDelegationByAssessment@ API.
batchCreateDelegationByAssessmentError_errorCode :: Lens.Lens' BatchCreateDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchCreateDelegationByAssessmentError_errorCode = Lens.lens (\BatchCreateDelegationByAssessmentError' {errorCode} -> errorCode) (\s@BatchCreateDelegationByAssessmentError' {} a -> s {errorCode = a} :: BatchCreateDelegationByAssessmentError)

-- | The error message returned by the @BatchCreateDelegationByAssessment@
-- API.
batchCreateDelegationByAssessmentError_errorMessage :: Lens.Lens' BatchCreateDelegationByAssessmentError (Prelude.Maybe Prelude.Text)
batchCreateDelegationByAssessmentError_errorMessage = Lens.lens (\BatchCreateDelegationByAssessmentError' {errorMessage} -> errorMessage) (\s@BatchCreateDelegationByAssessmentError' {} a -> s {errorMessage = a} :: BatchCreateDelegationByAssessmentError)

instance
  Core.FromJSON
    BatchCreateDelegationByAssessmentError
  where
  parseJSON =
    Core.withObject
      "BatchCreateDelegationByAssessmentError"
      ( \x ->
          BatchCreateDelegationByAssessmentError'
            Prelude.<$> (x Core..:? "createDelegationRequest")
            Prelude.<*> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "errorMessage")
      )

instance
  Prelude.Hashable
    BatchCreateDelegationByAssessmentError

instance
  Prelude.NFData
    BatchCreateDelegationByAssessmentError
