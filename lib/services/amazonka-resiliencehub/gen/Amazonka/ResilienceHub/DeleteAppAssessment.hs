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
-- Module      : Amazonka.ResilienceHub.DeleteAppAssessment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Resilience Hub application assessment. This is a
-- destructive action that can\'t be undone.
module Amazonka.ResilienceHub.DeleteAppAssessment
  ( -- * Creating a Request
    DeleteAppAssessment (..),
    newDeleteAppAssessment,

    -- * Request Lenses
    deleteAppAssessment_clientToken,
    deleteAppAssessment_assessmentArn,

    -- * Destructuring the Response
    DeleteAppAssessmentResponse (..),
    newDeleteAppAssessmentResponse,

    -- * Response Lenses
    deleteAppAssessmentResponse_httpStatus,
    deleteAppAssessmentResponse_assessmentArn,
    deleteAppAssessmentResponse_assessmentStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppAssessment' smart constructor.
data DeleteAppAssessment = DeleteAppAssessment'
  { -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the assessment. The format for this
    -- ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    assessmentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteAppAssessment_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'assessmentArn', 'deleteAppAssessment_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newDeleteAppAssessment ::
  -- | 'assessmentArn'
  Prelude.Text ->
  DeleteAppAssessment
newDeleteAppAssessment pAssessmentArn_ =
  DeleteAppAssessment'
    { clientToken = Prelude.Nothing,
      assessmentArn = pAssessmentArn_
    }

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
deleteAppAssessment_clientToken :: Lens.Lens' DeleteAppAssessment (Prelude.Maybe Prelude.Text)
deleteAppAssessment_clientToken = Lens.lens (\DeleteAppAssessment' {clientToken} -> clientToken) (\s@DeleteAppAssessment' {} a -> s {clientToken = a} :: DeleteAppAssessment)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
deleteAppAssessment_assessmentArn :: Lens.Lens' DeleteAppAssessment Prelude.Text
deleteAppAssessment_assessmentArn = Lens.lens (\DeleteAppAssessment' {assessmentArn} -> assessmentArn) (\s@DeleteAppAssessment' {} a -> s {assessmentArn = a} :: DeleteAppAssessment)

instance Core.AWSRequest DeleteAppAssessment where
  type
    AWSResponse DeleteAppAssessment =
      DeleteAppAssessmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAppAssessmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assessmentArn")
            Prelude.<*> (x Data..:> "assessmentStatus")
      )

instance Prelude.Hashable DeleteAppAssessment where
  hashWithSalt _salt DeleteAppAssessment' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` assessmentArn

instance Prelude.NFData DeleteAppAssessment where
  rnf DeleteAppAssessment' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf assessmentArn

instance Data.ToHeaders DeleteAppAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAppAssessment where
  toJSON DeleteAppAssessment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("assessmentArn" Data..= assessmentArn)
          ]
      )

instance Data.ToPath DeleteAppAssessment where
  toPath = Prelude.const "/delete-app-assessment"

instance Data.ToQuery DeleteAppAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppAssessmentResponse' smart constructor.
data DeleteAppAssessmentResponse = DeleteAppAssessmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the assessment. The format for this
    -- ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    assessmentArn :: Prelude.Text,
    -- | The current status of the assessment for the resiliency policy.
    assessmentStatus :: AssessmentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppAssessmentResponse_httpStatus' - The response's http status code.
--
-- 'assessmentArn', 'deleteAppAssessmentResponse_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'assessmentStatus', 'deleteAppAssessmentResponse_assessmentStatus' - The current status of the assessment for the resiliency policy.
newDeleteAppAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assessmentArn'
  Prelude.Text ->
  -- | 'assessmentStatus'
  AssessmentStatus ->
  DeleteAppAssessmentResponse
newDeleteAppAssessmentResponse
  pHttpStatus_
  pAssessmentArn_
  pAssessmentStatus_ =
    DeleteAppAssessmentResponse'
      { httpStatus =
          pHttpStatus_,
        assessmentArn = pAssessmentArn_,
        assessmentStatus = pAssessmentStatus_
      }

-- | The response's http status code.
deleteAppAssessmentResponse_httpStatus :: Lens.Lens' DeleteAppAssessmentResponse Prelude.Int
deleteAppAssessmentResponse_httpStatus = Lens.lens (\DeleteAppAssessmentResponse' {httpStatus} -> httpStatus) (\s@DeleteAppAssessmentResponse' {} a -> s {httpStatus = a} :: DeleteAppAssessmentResponse)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
deleteAppAssessmentResponse_assessmentArn :: Lens.Lens' DeleteAppAssessmentResponse Prelude.Text
deleteAppAssessmentResponse_assessmentArn = Lens.lens (\DeleteAppAssessmentResponse' {assessmentArn} -> assessmentArn) (\s@DeleteAppAssessmentResponse' {} a -> s {assessmentArn = a} :: DeleteAppAssessmentResponse)

-- | The current status of the assessment for the resiliency policy.
deleteAppAssessmentResponse_assessmentStatus :: Lens.Lens' DeleteAppAssessmentResponse AssessmentStatus
deleteAppAssessmentResponse_assessmentStatus = Lens.lens (\DeleteAppAssessmentResponse' {assessmentStatus} -> assessmentStatus) (\s@DeleteAppAssessmentResponse' {} a -> s {assessmentStatus = a} :: DeleteAppAssessmentResponse)

instance Prelude.NFData DeleteAppAssessmentResponse where
  rnf DeleteAppAssessmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assessmentArn
      `Prelude.seq` Prelude.rnf assessmentStatus
