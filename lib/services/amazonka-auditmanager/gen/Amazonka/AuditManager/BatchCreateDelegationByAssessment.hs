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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    batchCreateDelegationByAssessmentResponse_errors,
    batchCreateDelegationByAssessmentResponse_delegations,
    batchCreateDelegationByAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "errors" Core..!@ Prelude.mempty)
              Prelude.<*> (x Core..?> "delegations" Core..!@ Prelude.mempty)
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
  Core.ToHeaders
    BatchCreateDelegationByAssessment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    BatchCreateDelegationByAssessment
  where
  toJSON BatchCreateDelegationByAssessment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "createDelegationRequests"
                  Core..= createDelegationRequests
              )
          ]
      )

instance
  Core.ToPath
    BatchCreateDelegationByAssessment
  where
  toPath BatchCreateDelegationByAssessment' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Core.toBS assessmentId,
        "/delegations"
      ]

instance
  Core.ToQuery
    BatchCreateDelegationByAssessment
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchCreateDelegationByAssessmentResponse' smart constructor.
data BatchCreateDelegationByAssessmentResponse = BatchCreateDelegationByAssessmentResponse'
  { -- | A list of errors that the @BatchCreateDelegationByAssessment@ API
    -- returned.
    errors :: Prelude.Maybe [BatchCreateDelegationByAssessmentError],
    -- | The delegations that are associated with the assessment.
    delegations :: Prelude.Maybe [Delegation],
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
-- 'errors', 'batchCreateDelegationByAssessmentResponse_errors' - A list of errors that the @BatchCreateDelegationByAssessment@ API
-- returned.
--
-- 'delegations', 'batchCreateDelegationByAssessmentResponse_delegations' - The delegations that are associated with the assessment.
--
-- 'httpStatus', 'batchCreateDelegationByAssessmentResponse_httpStatus' - The response's http status code.
newBatchCreateDelegationByAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchCreateDelegationByAssessmentResponse
newBatchCreateDelegationByAssessmentResponse
  pHttpStatus_ =
    BatchCreateDelegationByAssessmentResponse'
      { errors =
          Prelude.Nothing,
        delegations = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of errors that the @BatchCreateDelegationByAssessment@ API
-- returned.
batchCreateDelegationByAssessmentResponse_errors :: Lens.Lens' BatchCreateDelegationByAssessmentResponse (Prelude.Maybe [BatchCreateDelegationByAssessmentError])
batchCreateDelegationByAssessmentResponse_errors = Lens.lens (\BatchCreateDelegationByAssessmentResponse' {errors} -> errors) (\s@BatchCreateDelegationByAssessmentResponse' {} a -> s {errors = a} :: BatchCreateDelegationByAssessmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The delegations that are associated with the assessment.
batchCreateDelegationByAssessmentResponse_delegations :: Lens.Lens' BatchCreateDelegationByAssessmentResponse (Prelude.Maybe [Delegation])
batchCreateDelegationByAssessmentResponse_delegations = Lens.lens (\BatchCreateDelegationByAssessmentResponse' {delegations} -> delegations) (\s@BatchCreateDelegationByAssessmentResponse' {} a -> s {delegations = a} :: BatchCreateDelegationByAssessmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreateDelegationByAssessmentResponse_httpStatus :: Lens.Lens' BatchCreateDelegationByAssessmentResponse Prelude.Int
batchCreateDelegationByAssessmentResponse_httpStatus = Lens.lens (\BatchCreateDelegationByAssessmentResponse' {httpStatus} -> httpStatus) (\s@BatchCreateDelegationByAssessmentResponse' {} a -> s {httpStatus = a} :: BatchCreateDelegationByAssessmentResponse)

instance
  Prelude.NFData
    BatchCreateDelegationByAssessmentResponse
  where
  rnf BatchCreateDelegationByAssessmentResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf delegations
      `Prelude.seq` Prelude.rnf httpStatus
