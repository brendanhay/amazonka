{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AuditManager.BatchCreateDelegationByAssessment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a batch of delegations for an assessment in Audit Manager.
module Amazonka.AuditManager.BatchCreateDelegationByAssessment
  ( -- * Creating a Request
    BatchCreateDelegationByAssessment (..),
    newBatchCreateDelegationByAssessment,

    -- * Request Lenses
    batchCreateDelegationByAssessment_createDelegationRequests,
    batchCreateDelegationByAssessment_assessmentId,

    -- * Destructuring the Response
    BatchCreateDelegationByAssessmentResponse (..),
    newBatchCreateDelegationByAssessmentResponse,

    -- * Response Lenses
    batchCreateDelegationByAssessmentResponse_delegations,
    batchCreateDelegationByAssessmentResponse_errors,
    batchCreateDelegationByAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchCreateDelegationByAssessment' smart constructor.
data BatchCreateDelegationByAssessment = BatchCreateDelegationByAssessment'
  { -- | The API request to batch create delegations in Audit Manager.
    createDelegationRequests :: Prelude.NonEmpty CreateDelegationRequest,
    -- | The identifier for the assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateDelegationByAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDelegationRequests', 'batchCreateDelegationByAssessment_createDelegationRequests' - The API request to batch create delegations in Audit Manager.
--
-- 'assessmentId', 'batchCreateDelegationByAssessment_assessmentId' - The identifier for the assessment.
newBatchCreateDelegationByAssessment ::
  -- | 'createDelegationRequests'
  Prelude.NonEmpty CreateDelegationRequest ->
  -- | 'assessmentId'
  Prelude.Text ->
  BatchCreateDelegationByAssessment
newBatchCreateDelegationByAssessment
  pCreateDelegationRequests_
  pAssessmentId_ =
    BatchCreateDelegationByAssessment'
      { createDelegationRequests =
          Lens.coerced
            Lens.# pCreateDelegationRequests_,
        assessmentId = pAssessmentId_
      }

-- | The API request to batch create delegations in Audit Manager.
batchCreateDelegationByAssessment_createDelegationRequests :: Lens.Lens' BatchCreateDelegationByAssessment (Prelude.NonEmpty CreateDelegationRequest)
batchCreateDelegationByAssessment_createDelegationRequests = Lens.lens (\BatchCreateDelegationByAssessment' {createDelegationRequests} -> createDelegationRequests) (\s@BatchCreateDelegationByAssessment' {} a -> s {createDelegationRequests = a} :: BatchCreateDelegationByAssessment) Prelude.. Lens.coerced

-- | The identifier for the assessment.
batchCreateDelegationByAssessment_assessmentId :: Lens.Lens' BatchCreateDelegationByAssessment Prelude.Text
batchCreateDelegationByAssessment_assessmentId = Lens.lens (\BatchCreateDelegationByAssessment' {assessmentId} -> assessmentId) (\s@BatchCreateDelegationByAssessment' {} a -> s {assessmentId = a} :: BatchCreateDelegationByAssessment)

instance
  Core.AWSRequest
    BatchCreateDelegationByAssessment
  where
  type
    AWSResponse BatchCreateDelegationByAssessment =
      BatchCreateDelegationByAssessmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreateDelegationByAssessmentResponse'
            Prelude.<$> (x Data..?> "delegations" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchCreateDelegationByAssessment
  where
  hashWithSalt
    _salt
    BatchCreateDelegationByAssessment' {..} =
      _salt
        `Prelude.hashWithSalt` createDelegationRequests
        `Prelude.hashWithSalt` assessmentId

instance
  Prelude.NFData
    BatchCreateDelegationByAssessment
  where
  rnf BatchCreateDelegationByAssessment' {..} =
    Prelude.rnf createDelegationRequests
      `Prelude.seq` Prelude.rnf assessmentId

instance
  Data.ToHeaders
    BatchCreateDelegationByAssessment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    BatchCreateDelegationByAssessment
  where
  toJSON BatchCreateDelegationByAssessment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "createDelegationRequests"
                  Data..= createDelegationRequests
              )
          ]
      )

instance
  Data.ToPath
    BatchCreateDelegationByAssessment
  where
  toPath BatchCreateDelegationByAssessment' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/delegations"
      ]

instance
  Data.ToQuery
    BatchCreateDelegationByAssessment
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchCreateDelegationByAssessmentResponse' smart constructor.
data BatchCreateDelegationByAssessmentResponse = BatchCreateDelegationByAssessmentResponse'
  { -- | The delegations that are associated with the assessment.
    delegations :: Prelude.Maybe [Delegation],
    -- | A list of errors that the @BatchCreateDelegationByAssessment@ API
    -- returned.
    errors :: Prelude.Maybe [BatchCreateDelegationByAssessmentError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateDelegationByAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegations', 'batchCreateDelegationByAssessmentResponse_delegations' - The delegations that are associated with the assessment.
--
-- 'errors', 'batchCreateDelegationByAssessmentResponse_errors' - A list of errors that the @BatchCreateDelegationByAssessment@ API
-- returned.
--
-- 'httpStatus', 'batchCreateDelegationByAssessmentResponse_httpStatus' - The response's http status code.
newBatchCreateDelegationByAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchCreateDelegationByAssessmentResponse
newBatchCreateDelegationByAssessmentResponse
  pHttpStatus_ =
    BatchCreateDelegationByAssessmentResponse'
      { delegations =
          Prelude.Nothing,
        errors = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The delegations that are associated with the assessment.
batchCreateDelegationByAssessmentResponse_delegations :: Lens.Lens' BatchCreateDelegationByAssessmentResponse (Prelude.Maybe [Delegation])
batchCreateDelegationByAssessmentResponse_delegations = Lens.lens (\BatchCreateDelegationByAssessmentResponse' {delegations} -> delegations) (\s@BatchCreateDelegationByAssessmentResponse' {} a -> s {delegations = a} :: BatchCreateDelegationByAssessmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of errors that the @BatchCreateDelegationByAssessment@ API
-- returned.
batchCreateDelegationByAssessmentResponse_errors :: Lens.Lens' BatchCreateDelegationByAssessmentResponse (Prelude.Maybe [BatchCreateDelegationByAssessmentError])
batchCreateDelegationByAssessmentResponse_errors = Lens.lens (\BatchCreateDelegationByAssessmentResponse' {errors} -> errors) (\s@BatchCreateDelegationByAssessmentResponse' {} a -> s {errors = a} :: BatchCreateDelegationByAssessmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreateDelegationByAssessmentResponse_httpStatus :: Lens.Lens' BatchCreateDelegationByAssessmentResponse Prelude.Int
batchCreateDelegationByAssessmentResponse_httpStatus = Lens.lens (\BatchCreateDelegationByAssessmentResponse' {httpStatus} -> httpStatus) (\s@BatchCreateDelegationByAssessmentResponse' {} a -> s {httpStatus = a} :: BatchCreateDelegationByAssessmentResponse)

instance
  Prelude.NFData
    BatchCreateDelegationByAssessmentResponse
  where
  rnf BatchCreateDelegationByAssessmentResponse' {..} =
    Prelude.rnf delegations
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
