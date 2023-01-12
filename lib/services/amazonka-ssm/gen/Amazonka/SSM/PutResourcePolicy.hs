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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    putResourcePolicy_policyHash,
    putResourcePolicy_policyId,
    putResourcePolicy_resourceArn,
    putResourcePolicy_policy,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_policyHash,
    putResourcePolicyResponse_policyId,
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | ID of the current policy version. The hash helps to prevent a situation
    -- where multiple users attempt to overwrite a policy. You must provide
    -- this hash when updating or deleting a policy.
    policyHash :: Prelude.Maybe Prelude.Text,
    -- | The policy ID.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the resource to which you want to attach a
    -- policy.
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
-- 'policyHash', 'putResourcePolicy_policyHash' - ID of the current policy version. The hash helps to prevent a situation
-- where multiple users attempt to overwrite a policy. You must provide
-- this hash when updating or deleting a policy.
--
-- 'policyId', 'putResourcePolicy_policyId' - The policy ID.
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - Amazon Resource Name (ARN) of the resource to which you want to attach a
-- policy.
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
    { policyHash = Prelude.Nothing,
      policyId = Prelude.Nothing,
      resourceArn = pResourceArn_,
      policy = pPolicy_
    }

-- | ID of the current policy version. The hash helps to prevent a situation
-- where multiple users attempt to overwrite a policy. You must provide
-- this hash when updating or deleting a policy.
putResourcePolicy_policyHash :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyHash = Lens.lens (\PutResourcePolicy' {policyHash} -> policyHash) (\s@PutResourcePolicy' {} a -> s {policyHash = a} :: PutResourcePolicy)

-- | The policy ID.
putResourcePolicy_policyId :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyId = Lens.lens (\PutResourcePolicy' {policyId} -> policyId) (\s@PutResourcePolicy' {} a -> s {policyId = a} :: PutResourcePolicy)

-- | Amazon Resource Name (ARN) of the resource to which you want to attach a
-- policy.
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
            Prelude.<$> (x Data..?> "PolicyHash")
            Prelude.<*> (x Data..?> "PolicyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyHash
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf policyHash
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.PutResourcePolicy" ::
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
          [ ("PolicyHash" Data..=) Prelude.<$> policyHash,
            ("PolicyId" Data..=) Prelude.<$> policyId,
            Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("Policy" Data..= policy)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | ID of the current policy version.
    policyHash :: Prelude.Maybe Prelude.Text,
    -- | The policy ID. To update a policy, you must specify @PolicyId@ and
    -- @PolicyHash@.
    policyId :: Prelude.Maybe Prelude.Text,
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
-- 'policyHash', 'putResourcePolicyResponse_policyHash' - ID of the current policy version.
--
-- 'policyId', 'putResourcePolicyResponse_policyId' - The policy ID. To update a policy, you must specify @PolicyId@ and
-- @PolicyHash@.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { policyHash =
        Prelude.Nothing,
      policyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ID of the current policy version.
putResourcePolicyResponse_policyHash :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_policyHash = Lens.lens (\PutResourcePolicyResponse' {policyHash} -> policyHash) (\s@PutResourcePolicyResponse' {} a -> s {policyHash = a} :: PutResourcePolicyResponse)

-- | The policy ID. To update a policy, you must specify @PolicyId@ and
-- @PolicyHash@.
putResourcePolicyResponse_policyId :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_policyId = Lens.lens (\PutResourcePolicyResponse' {policyId} -> policyId) (\s@PutResourcePolicyResponse' {} a -> s {policyId = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf policyHash
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf httpStatus
