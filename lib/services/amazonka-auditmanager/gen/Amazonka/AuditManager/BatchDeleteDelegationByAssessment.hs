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
-- Module      : Amazonka.AuditManager.BatchDeleteDelegationByAssessment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch of delegations for an assessment in Audit Manager.
module Amazonka.AuditManager.BatchDeleteDelegationByAssessment
  ( -- * Creating a Request
    BatchDeleteDelegationByAssessment (..),
    newBatchDeleteDelegationByAssessment,

    -- * Request Lenses
    batchDeleteDelegationByAssessment_delegationIds,
    batchDeleteDelegationByAssessment_assessmentId,

    -- * Destructuring the Response
    BatchDeleteDelegationByAssessmentResponse (..),
    newBatchDeleteDelegationByAssessmentResponse,

    -- * Response Lenses
    batchDeleteDelegationByAssessmentResponse_errors,
    batchDeleteDelegationByAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteDelegationByAssessment' smart constructor.
data BatchDeleteDelegationByAssessment = BatchDeleteDelegationByAssessment'
  { -- | The identifiers for the delegations.
    delegationIds :: Prelude.NonEmpty Prelude.Text,
    -- | The identifier for the assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDelegationByAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegationIds', 'batchDeleteDelegationByAssessment_delegationIds' - The identifiers for the delegations.
--
-- 'assessmentId', 'batchDeleteDelegationByAssessment_assessmentId' - The identifier for the assessment.
newBatchDeleteDelegationByAssessment ::
  -- | 'delegationIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'assessmentId'
  Prelude.Text ->
  BatchDeleteDelegationByAssessment
newBatchDeleteDelegationByAssessment
  pDelegationIds_
  pAssessmentId_ =
    BatchDeleteDelegationByAssessment'
      { delegationIds =
          Lens.coerced Lens.# pDelegationIds_,
        assessmentId = pAssessmentId_
      }

-- | The identifiers for the delegations.
batchDeleteDelegationByAssessment_delegationIds :: Lens.Lens' BatchDeleteDelegationByAssessment (Prelude.NonEmpty Prelude.Text)
batchDeleteDelegationByAssessment_delegationIds = Lens.lens (\BatchDeleteDelegationByAssessment' {delegationIds} -> delegationIds) (\s@BatchDeleteDelegationByAssessment' {} a -> s {delegationIds = a} :: BatchDeleteDelegationByAssessment) Prelude.. Lens.coerced

-- | The identifier for the assessment.
batchDeleteDelegationByAssessment_assessmentId :: Lens.Lens' BatchDeleteDelegationByAssessment Prelude.Text
batchDeleteDelegationByAssessment_assessmentId = Lens.lens (\BatchDeleteDelegationByAssessment' {assessmentId} -> assessmentId) (\s@BatchDeleteDelegationByAssessment' {} a -> s {assessmentId = a} :: BatchDeleteDelegationByAssessment)

instance
  Core.AWSRequest
    BatchDeleteDelegationByAssessment
  where
  type
    AWSResponse BatchDeleteDelegationByAssessment =
      BatchDeleteDelegationByAssessmentResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteDelegationByAssessmentResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchDeleteDelegationByAssessment
  where
  hashWithSalt
    _salt
    BatchDeleteDelegationByAssessment' {..} =
      _salt
        `Prelude.hashWithSalt` delegationIds
        `Prelude.hashWithSalt` assessmentId

instance
  Prelude.NFData
    BatchDeleteDelegationByAssessment
  where
  rnf BatchDeleteDelegationByAssessment' {..} =
    Prelude.rnf delegationIds `Prelude.seq`
      Prelude.rnf assessmentId

instance
  Data.ToHeaders
    BatchDeleteDelegationByAssessment
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
    BatchDeleteDelegationByAssessment
  where
  toJSON BatchDeleteDelegationByAssessment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("delegationIds" Data..= delegationIds)
          ]
      )

instance
  Data.ToPath
    BatchDeleteDelegationByAssessment
  where
  toPath BatchDeleteDelegationByAssessment' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/delegations"
      ]

instance
  Data.ToQuery
    BatchDeleteDelegationByAssessment
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteDelegationByAssessmentResponse' smart constructor.
data BatchDeleteDelegationByAssessmentResponse = BatchDeleteDelegationByAssessmentResponse'
  { -- | A list of errors that the @BatchDeleteDelegationByAssessment@ API
    -- returned.
    errors :: Prelude.Maybe [BatchDeleteDelegationByAssessmentError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDelegationByAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDeleteDelegationByAssessmentResponse_errors' - A list of errors that the @BatchDeleteDelegationByAssessment@ API
-- returned.
--
-- 'httpStatus', 'batchDeleteDelegationByAssessmentResponse_httpStatus' - The response's http status code.
newBatchDeleteDelegationByAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteDelegationByAssessmentResponse
newBatchDeleteDelegationByAssessmentResponse
  pHttpStatus_ =
    BatchDeleteDelegationByAssessmentResponse'
      { errors =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of errors that the @BatchDeleteDelegationByAssessment@ API
-- returned.
batchDeleteDelegationByAssessmentResponse_errors :: Lens.Lens' BatchDeleteDelegationByAssessmentResponse (Prelude.Maybe [BatchDeleteDelegationByAssessmentError])
batchDeleteDelegationByAssessmentResponse_errors = Lens.lens (\BatchDeleteDelegationByAssessmentResponse' {errors} -> errors) (\s@BatchDeleteDelegationByAssessmentResponse' {} a -> s {errors = a} :: BatchDeleteDelegationByAssessmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteDelegationByAssessmentResponse_httpStatus :: Lens.Lens' BatchDeleteDelegationByAssessmentResponse Prelude.Int
batchDeleteDelegationByAssessmentResponse_httpStatus = Lens.lens (\BatchDeleteDelegationByAssessmentResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteDelegationByAssessmentResponse' {} a -> s {httpStatus = a} :: BatchDeleteDelegationByAssessmentResponse)

instance
  Prelude.NFData
    BatchDeleteDelegationByAssessmentResponse
  where
  rnf BatchDeleteDelegationByAssessmentResponse' {..} =
    Prelude.rnf errors `Prelude.seq`
      Prelude.rnf httpStatus
