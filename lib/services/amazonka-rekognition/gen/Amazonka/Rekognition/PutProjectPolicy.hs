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
-- Module      : Amazonka.Rekognition.PutProjectPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a project policy to a Amazon Rekognition Custom Labels project
-- in a trusting AWS account. A project policy specifies that a trusted AWS
-- account can copy a model version from a trusting AWS account to a
-- project in the trusted AWS account. To copy a model version you use the
-- CopyProjectVersion operation.
--
-- For more information about the format of a project policy document, see
-- Attaching a project policy (SDK) in the /Amazon Rekognition Custom
-- Labels Developer Guide/.
--
-- The response from @PutProjectPolicy@ is a revision ID for the project
-- policy. You can attach multiple project policies to a project. You can
-- also update an existing project policy by specifying the policy revision
-- ID of the existing policy.
--
-- To remove a project policy from a project, call DeleteProjectPolicy. To
-- get a list of project policies attached to a project, call
-- ListProjectPolicies.
--
-- You copy a model version by calling CopyProjectVersion.
--
-- This operation requires permissions to perform the
-- @rekognition:PutProjectPolicy@ action.
module Amazonka.Rekognition.PutProjectPolicy
  ( -- * Creating a Request
    PutProjectPolicy (..),
    newPutProjectPolicy,

    -- * Request Lenses
    putProjectPolicy_policyRevisionId,
    putProjectPolicy_projectArn,
    putProjectPolicy_policyName,
    putProjectPolicy_policyDocument,

    -- * Destructuring the Response
    PutProjectPolicyResponse (..),
    newPutProjectPolicyResponse,

    -- * Response Lenses
    putProjectPolicyResponse_policyRevisionId,
    putProjectPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutProjectPolicy' smart constructor.
data PutProjectPolicy = PutProjectPolicy'
  { -- | The revision ID for the Project Policy. Each time you modify a policy,
    -- Amazon Rekognition Custom Labels generates and assigns a new
    -- @PolicyRevisionId@ and then deletes the previous version of the policy.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project that the project policy is
    -- attached to.
    projectArn :: Prelude.Text,
    -- | A name for the policy.
    policyName :: Prelude.Text,
    -- | A resource policy to add to the model. The policy is a JSON structure
    -- that contains one or more statements that define the policy. The policy
    -- must follow the IAM syntax. For more information about the contents of a
    -- JSON policy document, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>.
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProjectPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRevisionId', 'putProjectPolicy_policyRevisionId' - The revision ID for the Project Policy. Each time you modify a policy,
-- Amazon Rekognition Custom Labels generates and assigns a new
-- @PolicyRevisionId@ and then deletes the previous version of the policy.
--
-- 'projectArn', 'putProjectPolicy_projectArn' - The Amazon Resource Name (ARN) of the project that the project policy is
-- attached to.
--
-- 'policyName', 'putProjectPolicy_policyName' - A name for the policy.
--
-- 'policyDocument', 'putProjectPolicy_policyDocument' - A resource policy to add to the model. The policy is a JSON structure
-- that contains one or more statements that define the policy. The policy
-- must follow the IAM syntax. For more information about the contents of a
-- JSON policy document, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>.
newPutProjectPolicy ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutProjectPolicy
newPutProjectPolicy
  pProjectArn_
  pPolicyName_
  pPolicyDocument_ =
    PutProjectPolicy'
      { policyRevisionId =
          Prelude.Nothing,
        projectArn = pProjectArn_,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_
      }

-- | The revision ID for the Project Policy. Each time you modify a policy,
-- Amazon Rekognition Custom Labels generates and assigns a new
-- @PolicyRevisionId@ and then deletes the previous version of the policy.
putProjectPolicy_policyRevisionId :: Lens.Lens' PutProjectPolicy (Prelude.Maybe Prelude.Text)
putProjectPolicy_policyRevisionId = Lens.lens (\PutProjectPolicy' {policyRevisionId} -> policyRevisionId) (\s@PutProjectPolicy' {} a -> s {policyRevisionId = a} :: PutProjectPolicy)

-- | The Amazon Resource Name (ARN) of the project that the project policy is
-- attached to.
putProjectPolicy_projectArn :: Lens.Lens' PutProjectPolicy Prelude.Text
putProjectPolicy_projectArn = Lens.lens (\PutProjectPolicy' {projectArn} -> projectArn) (\s@PutProjectPolicy' {} a -> s {projectArn = a} :: PutProjectPolicy)

-- | A name for the policy.
putProjectPolicy_policyName :: Lens.Lens' PutProjectPolicy Prelude.Text
putProjectPolicy_policyName = Lens.lens (\PutProjectPolicy' {policyName} -> policyName) (\s@PutProjectPolicy' {} a -> s {policyName = a} :: PutProjectPolicy)

-- | A resource policy to add to the model. The policy is a JSON structure
-- that contains one or more statements that define the policy. The policy
-- must follow the IAM syntax. For more information about the contents of a
-- JSON policy document, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>.
putProjectPolicy_policyDocument :: Lens.Lens' PutProjectPolicy Prelude.Text
putProjectPolicy_policyDocument = Lens.lens (\PutProjectPolicy' {policyDocument} -> policyDocument) (\s@PutProjectPolicy' {} a -> s {policyDocument = a} :: PutProjectPolicy)

instance Core.AWSRequest PutProjectPolicy where
  type
    AWSResponse PutProjectPolicy =
      PutProjectPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutProjectPolicyResponse'
            Prelude.<$> (x Data..?> "PolicyRevisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutProjectPolicy where
  hashWithSalt _salt PutProjectPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyRevisionId
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData PutProjectPolicy where
  rnf PutProjectPolicy' {..} =
    Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyDocument

instance Data.ToHeaders PutProjectPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.PutProjectPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutProjectPolicy where
  toJSON PutProjectPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyRevisionId" Data..=)
              Prelude.<$> policyRevisionId,
            Prelude.Just ("ProjectArn" Data..= projectArn),
            Prelude.Just ("PolicyName" Data..= policyName),
            Prelude.Just
              ("PolicyDocument" Data..= policyDocument)
          ]
      )

instance Data.ToPath PutProjectPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutProjectPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutProjectPolicyResponse' smart constructor.
data PutProjectPolicyResponse = PutProjectPolicyResponse'
  { -- | The ID of the project policy.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProjectPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRevisionId', 'putProjectPolicyResponse_policyRevisionId' - The ID of the project policy.
--
-- 'httpStatus', 'putProjectPolicyResponse_httpStatus' - The response's http status code.
newPutProjectPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutProjectPolicyResponse
newPutProjectPolicyResponse pHttpStatus_ =
  PutProjectPolicyResponse'
    { policyRevisionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the project policy.
putProjectPolicyResponse_policyRevisionId :: Lens.Lens' PutProjectPolicyResponse (Prelude.Maybe Prelude.Text)
putProjectPolicyResponse_policyRevisionId = Lens.lens (\PutProjectPolicyResponse' {policyRevisionId} -> policyRevisionId) (\s@PutProjectPolicyResponse' {} a -> s {policyRevisionId = a} :: PutProjectPolicyResponse)

-- | The response's http status code.
putProjectPolicyResponse_httpStatus :: Lens.Lens' PutProjectPolicyResponse Prelude.Int
putProjectPolicyResponse_httpStatus = Lens.lens (\PutProjectPolicyResponse' {httpStatus} -> httpStatus) (\s@PutProjectPolicyResponse' {} a -> s {httpStatus = a} :: PutProjectPolicyResponse)

instance Prelude.NFData PutProjectPolicyResponse where
  rnf PutProjectPolicyResponse' {..} =
    Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf httpStatus
