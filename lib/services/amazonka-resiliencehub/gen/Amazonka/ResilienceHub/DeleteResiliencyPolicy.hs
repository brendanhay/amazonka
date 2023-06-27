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
-- Module      : Amazonka.ResilienceHub.DeleteResiliencyPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resiliency policy. This is a destructive action that can\'t be
-- undone.
module Amazonka.ResilienceHub.DeleteResiliencyPolicy
  ( -- * Creating a Request
    DeleteResiliencyPolicy (..),
    newDeleteResiliencyPolicy,

    -- * Request Lenses
    deleteResiliencyPolicy_clientToken,
    deleteResiliencyPolicy_policyArn,

    -- * Destructuring the Response
    DeleteResiliencyPolicyResponse (..),
    newDeleteResiliencyPolicyResponse,

    -- * Response Lenses
    deleteResiliencyPolicyResponse_httpStatus,
    deleteResiliencyPolicyResponse_policyArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResiliencyPolicy' smart constructor.
data DeleteResiliencyPolicy = DeleteResiliencyPolicy'
  { -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResiliencyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteResiliencyPolicy_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'policyArn', 'deleteResiliencyPolicy_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
newDeleteResiliencyPolicy ::
  -- | 'policyArn'
  Prelude.Text ->
  DeleteResiliencyPolicy
newDeleteResiliencyPolicy pPolicyArn_ =
  DeleteResiliencyPolicy'
    { clientToken =
        Prelude.Nothing,
      policyArn = pPolicyArn_
    }

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
deleteResiliencyPolicy_clientToken :: Lens.Lens' DeleteResiliencyPolicy (Prelude.Maybe Prelude.Text)
deleteResiliencyPolicy_clientToken = Lens.lens (\DeleteResiliencyPolicy' {clientToken} -> clientToken) (\s@DeleteResiliencyPolicy' {} a -> s {clientToken = a} :: DeleteResiliencyPolicy)

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteResiliencyPolicy_policyArn :: Lens.Lens' DeleteResiliencyPolicy Prelude.Text
deleteResiliencyPolicy_policyArn = Lens.lens (\DeleteResiliencyPolicy' {policyArn} -> policyArn) (\s@DeleteResiliencyPolicy' {} a -> s {policyArn = a} :: DeleteResiliencyPolicy)

instance Core.AWSRequest DeleteResiliencyPolicy where
  type
    AWSResponse DeleteResiliencyPolicy =
      DeleteResiliencyPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResiliencyPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyArn")
      )

instance Prelude.Hashable DeleteResiliencyPolicy where
  hashWithSalt _salt DeleteResiliencyPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData DeleteResiliencyPolicy where
  rnf DeleteResiliencyPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf policyArn

instance Data.ToHeaders DeleteResiliencyPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResiliencyPolicy where
  toJSON DeleteResiliencyPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("policyArn" Data..= policyArn)
          ]
      )

instance Data.ToPath DeleteResiliencyPolicy where
  toPath = Prelude.const "/delete-resiliency-policy"

instance Data.ToQuery DeleteResiliencyPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResiliencyPolicyResponse' smart constructor.
data DeleteResiliencyPolicyResponse = DeleteResiliencyPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResiliencyPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResiliencyPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policyArn', 'deleteResiliencyPolicyResponse_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
newDeleteResiliencyPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyArn'
  Prelude.Text ->
  DeleteResiliencyPolicyResponse
newDeleteResiliencyPolicyResponse
  pHttpStatus_
  pPolicyArn_ =
    DeleteResiliencyPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        policyArn = pPolicyArn_
      }

-- | The response's http status code.
deleteResiliencyPolicyResponse_httpStatus :: Lens.Lens' DeleteResiliencyPolicyResponse Prelude.Int
deleteResiliencyPolicyResponse_httpStatus = Lens.lens (\DeleteResiliencyPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteResiliencyPolicyResponse' {} a -> s {httpStatus = a} :: DeleteResiliencyPolicyResponse)

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteResiliencyPolicyResponse_policyArn :: Lens.Lens' DeleteResiliencyPolicyResponse Prelude.Text
deleteResiliencyPolicyResponse_policyArn = Lens.lens (\DeleteResiliencyPolicyResponse' {policyArn} -> policyArn) (\s@DeleteResiliencyPolicyResponse' {} a -> s {policyArn = a} :: DeleteResiliencyPolicyResponse)

instance
  Prelude.NFData
    DeleteResiliencyPolicyResponse
  where
  rnf DeleteResiliencyPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyArn
