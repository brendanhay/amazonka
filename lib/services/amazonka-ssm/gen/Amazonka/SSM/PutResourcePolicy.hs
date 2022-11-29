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
-- Module      : Amazonka.SSM.PutResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a Systems Manager resource policy. A resource policy
-- helps you to define the IAM entity (for example, an Amazon Web Services
-- account) that can manage your Systems Manager resources. Currently,
-- @OpsItemGroup@ is the only resource that supports Systems Manager
-- resource policies. The resource policy for @OpsItemGroup@ enables Amazon
-- Web Services accounts to view and interact with OpsCenter operational
-- work items (OpsItems).
module Amazonka.SSM.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_policyId,
    putResourcePolicy_policyHash,
    putResourcePolicy_resourceArn,
    putResourcePolicy_policy,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_policyId,
    putResourcePolicyResponse_policyHash,
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | The policy ID.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | ID of the current policy version. The hash helps to prevent a situation
    -- where multiple users attempt to overwrite a policy.
    policyHash :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the resource to which the policies are
    -- attached.
    resourceArn :: Prelude.Text,
    -- | A policy you want to associate with a resource.
    policy :: Prelude.Text
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
-- 'policyId', 'putResourcePolicy_policyId' - The policy ID.
--
-- 'policyHash', 'putResourcePolicy_policyHash' - ID of the current policy version. The hash helps to prevent a situation
-- where multiple users attempt to overwrite a policy.
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - Amazon Resource Name (ARN) of the resource to which the policies are
-- attached.
--
-- 'policy', 'putResourcePolicy_policy' - A policy you want to associate with a resource.
newPutResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pResourceArn_ pPolicy_ =
  PutResourcePolicy'
    { policyId = Prelude.Nothing,
      policyHash = Prelude.Nothing,
      resourceArn = pResourceArn_,
      policy = pPolicy_
    }

-- | The policy ID.
putResourcePolicy_policyId :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyId = Lens.lens (\PutResourcePolicy' {policyId} -> policyId) (\s@PutResourcePolicy' {} a -> s {policyId = a} :: PutResourcePolicy)

-- | ID of the current policy version. The hash helps to prevent a situation
-- where multiple users attempt to overwrite a policy.
putResourcePolicy_policyHash :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyHash = Lens.lens (\PutResourcePolicy' {policyHash} -> policyHash) (\s@PutResourcePolicy' {} a -> s {policyHash = a} :: PutResourcePolicy)

-- | Amazon Resource Name (ARN) of the resource to which the policies are
-- attached.
putResourcePolicy_resourceArn :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourceArn = Lens.lens (\PutResourcePolicy' {resourceArn} -> resourceArn) (\s@PutResourcePolicy' {} a -> s {resourceArn = a} :: PutResourcePolicy)

-- | A policy you want to associate with a resource.
putResourcePolicy_policy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_policy = Lens.lens (\PutResourcePolicy' {policy} -> policy) (\s@PutResourcePolicy' {} a -> s {policy = a} :: PutResourcePolicy)

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
            Prelude.<$> (x Core..?> "PolicyId")
            Prelude.<*> (x Core..?> "PolicyHash")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` policyHash
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyHash
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf policy

instance Core.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.PutResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PolicyId" Core..=) Prelude.<$> policyId,
            ("PolicyHash" Core..=) Prelude.<$> policyHash,
            Prelude.Just ("ResourceArn" Core..= resourceArn),
            Prelude.Just ("Policy" Core..= policy)
          ]
      )

instance Core.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The policy ID. To update a policy, you must specify @PolicyId@ and
    -- @PolicyHash@.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | ID of the current policy version. The hash helps to prevent a situation
    -- where multiple users attempt to overwrite a policy. You must provide
    -- this hash when updating or deleting a policy.
    policyHash :: Prelude.Maybe Prelude.Text,
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
-- 'policyId', 'putResourcePolicyResponse_policyId' - The policy ID. To update a policy, you must specify @PolicyId@ and
-- @PolicyHash@.
--
-- 'policyHash', 'putResourcePolicyResponse_policyHash' - ID of the current policy version. The hash helps to prevent a situation
-- where multiple users attempt to overwrite a policy. You must provide
-- this hash when updating or deleting a policy.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { policyId =
        Prelude.Nothing,
      policyHash = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy ID. To update a policy, you must specify @PolicyId@ and
-- @PolicyHash@.
putResourcePolicyResponse_policyId :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_policyId = Lens.lens (\PutResourcePolicyResponse' {policyId} -> policyId) (\s@PutResourcePolicyResponse' {} a -> s {policyId = a} :: PutResourcePolicyResponse)

-- | ID of the current policy version. The hash helps to prevent a situation
-- where multiple users attempt to overwrite a policy. You must provide
-- this hash when updating or deleting a policy.
putResourcePolicyResponse_policyHash :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_policyHash = Lens.lens (\PutResourcePolicyResponse' {policyHash} -> policyHash) (\s@PutResourcePolicyResponse' {} a -> s {policyHash = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyHash
      `Prelude.seq` Prelude.rnf httpStatus
