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
-- Module      : Amazonka.Comprehend.PutResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a resource-based policy to a custom model. You can use this
-- policy to authorize an entity in another AWS account to import the
-- custom model, which replicates it in Amazon Comprehend in their account.
module Amazonka.Comprehend.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_policyRevisionId,
    putResourcePolicy_resourceArn,
    putResourcePolicy_resourcePolicy,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_policyRevisionId,
    putResourcePolicyResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | The revision ID that Amazon Comprehend assigned to the policy that you
    -- are updating. If you are creating a new policy that has no prior
    -- version, don\'t use this parameter. Amazon Comprehend creates the
    -- revision ID for you.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom model to attach the policy
    -- to.
    resourceArn :: Prelude.Text,
    -- | The JSON resource-based policy to attach to your custom model. Provide
    -- your JSON as a UTF-8 encoded string without line breaks. To provide
    -- valid JSON for your policy, enclose the attribute names and values in
    -- double quotes. If the JSON body is also enclosed in double quotes, then
    -- you must escape the double quotes that are inside the policy:
    --
    -- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
    --
    -- To avoid escaping quotes, you can use single quotes to enclose the
    -- policy and double quotes to enclose the JSON names and values:
    --
    -- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
    resourcePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRevisionId', 'putResourcePolicy_policyRevisionId' - The revision ID that Amazon Comprehend assigned to the policy that you
-- are updating. If you are creating a new policy that has no prior
-- version, don\'t use this parameter. Amazon Comprehend creates the
-- revision ID for you.
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the custom model to attach the policy
-- to.
--
-- 'resourcePolicy', 'putResourcePolicy_resourcePolicy' - The JSON resource-based policy to attach to your custom model. Provide
-- your JSON as a UTF-8 encoded string without line breaks. To provide
-- valid JSON for your policy, enclose the attribute names and values in
-- double quotes. If the JSON body is also enclosed in double quotes, then
-- you must escape the double quotes that are inside the policy:
--
-- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
--
-- To avoid escaping quotes, you can use single quotes to enclose the
-- policy and double quotes to enclose the JSON names and values:
--
-- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
newPutResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'resourcePolicy'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pResourceArn_ pResourcePolicy_ =
  PutResourcePolicy'
    { policyRevisionId =
        Prelude.Nothing,
      resourceArn = pResourceArn_,
      resourcePolicy = pResourcePolicy_
    }

-- | The revision ID that Amazon Comprehend assigned to the policy that you
-- are updating. If you are creating a new policy that has no prior
-- version, don\'t use this parameter. Amazon Comprehend creates the
-- revision ID for you.
putResourcePolicy_policyRevisionId :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyRevisionId = Lens.lens (\PutResourcePolicy' {policyRevisionId} -> policyRevisionId) (\s@PutResourcePolicy' {} a -> s {policyRevisionId = a} :: PutResourcePolicy)

-- | The Amazon Resource Name (ARN) of the custom model to attach the policy
-- to.
putResourcePolicy_resourceArn :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourceArn = Lens.lens (\PutResourcePolicy' {resourceArn} -> resourceArn) (\s@PutResourcePolicy' {} a -> s {resourceArn = a} :: PutResourcePolicy)

-- | The JSON resource-based policy to attach to your custom model. Provide
-- your JSON as a UTF-8 encoded string without line breaks. To provide
-- valid JSON for your policy, enclose the attribute names and values in
-- double quotes. If the JSON body is also enclosed in double quotes, then
-- you must escape the double quotes that are inside the policy:
--
-- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
--
-- To avoid escaping quotes, you can use single quotes to enclose the
-- policy and double quotes to enclose the JSON names and values:
--
-- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
putResourcePolicy_resourcePolicy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourcePolicy = Lens.lens (\PutResourcePolicy' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicy' {} a -> s {resourcePolicy = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Prelude.<$> (x Data..?> "PolicyRevisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyRevisionId
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourcePolicy

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourcePolicy

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.PutResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyRevisionId" Data..=)
              Prelude.<$> policyRevisionId,
            Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just
              ("ResourcePolicy" Data..= resourcePolicy)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The revision ID of the policy. Each time you modify a policy, Amazon
    -- Comprehend assigns a new revision ID, and it deletes the prior version
    -- of the policy.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRevisionId', 'putResourcePolicyResponse_policyRevisionId' - The revision ID of the policy. Each time you modify a policy, Amazon
-- Comprehend assigns a new revision ID, and it deletes the prior version
-- of the policy.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { policyRevisionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The revision ID of the policy. Each time you modify a policy, Amazon
-- Comprehend assigns a new revision ID, and it deletes the prior version
-- of the policy.
putResourcePolicyResponse_policyRevisionId :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_policyRevisionId = Lens.lens (\PutResourcePolicyResponse' {policyRevisionId} -> policyRevisionId) (\s@PutResourcePolicyResponse' {} a -> s {policyRevisionId = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf httpStatus
