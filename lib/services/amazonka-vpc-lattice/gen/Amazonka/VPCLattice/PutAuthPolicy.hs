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
-- Module      : Amazonka.VPCLattice.PutAuthPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the auth policy. The policy string in JSON must not
-- contain newlines or blank lines.
module Amazonka.VPCLattice.PutAuthPolicy
  ( -- * Creating a Request
    PutAuthPolicy (..),
    newPutAuthPolicy,

    -- * Request Lenses
    putAuthPolicy_policy,
    putAuthPolicy_resourceIdentifier,

    -- * Destructuring the Response
    PutAuthPolicyResponse (..),
    newPutAuthPolicyResponse,

    -- * Response Lenses
    putAuthPolicyResponse_policy,
    putAuthPolicyResponse_state,
    putAuthPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newPutAuthPolicy' smart constructor.
data PutAuthPolicy = PutAuthPolicy'
  { -- | The auth policy. The policy string in JSON must not contain newlines or
    -- blank lines.
    policy :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service network or service
    -- for which the policy is created.
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAuthPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putAuthPolicy_policy' - The auth policy. The policy string in JSON must not contain newlines or
-- blank lines.
--
-- 'resourceIdentifier', 'putAuthPolicy_resourceIdentifier' - The ID or Amazon Resource Name (ARN) of the service network or service
-- for which the policy is created.
newPutAuthPolicy ::
  -- | 'policy'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  Prelude.Text ->
  PutAuthPolicy
newPutAuthPolicy pPolicy_ pResourceIdentifier_ =
  PutAuthPolicy'
    { policy = pPolicy_,
      resourceIdentifier = pResourceIdentifier_
    }

-- | The auth policy. The policy string in JSON must not contain newlines or
-- blank lines.
putAuthPolicy_policy :: Lens.Lens' PutAuthPolicy Prelude.Text
putAuthPolicy_policy = Lens.lens (\PutAuthPolicy' {policy} -> policy) (\s@PutAuthPolicy' {} a -> s {policy = a} :: PutAuthPolicy)

-- | The ID or Amazon Resource Name (ARN) of the service network or service
-- for which the policy is created.
putAuthPolicy_resourceIdentifier :: Lens.Lens' PutAuthPolicy Prelude.Text
putAuthPolicy_resourceIdentifier = Lens.lens (\PutAuthPolicy' {resourceIdentifier} -> resourceIdentifier) (\s@PutAuthPolicy' {} a -> s {resourceIdentifier = a} :: PutAuthPolicy)

instance Core.AWSRequest PutAuthPolicy where
  type
    AWSResponse PutAuthPolicy =
      PutAuthPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAuthPolicyResponse'
            Prelude.<$> (x Data..?> "policy")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAuthPolicy where
  hashWithSalt _salt PutAuthPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData PutAuthPolicy where
  rnf PutAuthPolicy' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance Data.ToHeaders PutAuthPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAuthPolicy where
  toJSON PutAuthPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("policy" Data..= policy)]
      )

instance Data.ToPath PutAuthPolicy where
  toPath PutAuthPolicy' {..} =
    Prelude.mconcat
      ["/authpolicy/", Data.toBS resourceIdentifier]

instance Data.ToQuery PutAuthPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAuthPolicyResponse' smart constructor.
data PutAuthPolicyResponse = PutAuthPolicyResponse'
  { -- | The auth policy. The policy string in JSON must not contain newlines or
    -- blank lines.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The state of the auth policy. The auth policy is only active when the
    -- auth type is set to @AWS_IAM@. If you provide a policy, then
    -- authentication and authorization decisions are made based on this policy
    -- and the client\'s IAM policy. If the Auth type is @NONE@, then, any auth
    -- policy you provide will remain inactive. For more information, see
    -- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html#create-service-network Create a service network>
    -- in the /Amazon VPC Lattice User Guide/.
    state :: Prelude.Maybe AuthPolicyState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAuthPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putAuthPolicyResponse_policy' - The auth policy. The policy string in JSON must not contain newlines or
-- blank lines.
--
-- 'state', 'putAuthPolicyResponse_state' - The state of the auth policy. The auth policy is only active when the
-- auth type is set to @AWS_IAM@. If you provide a policy, then
-- authentication and authorization decisions are made based on this policy
-- and the client\'s IAM policy. If the Auth type is @NONE@, then, any auth
-- policy you provide will remain inactive. For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html#create-service-network Create a service network>
-- in the /Amazon VPC Lattice User Guide/.
--
-- 'httpStatus', 'putAuthPolicyResponse_httpStatus' - The response's http status code.
newPutAuthPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAuthPolicyResponse
newPutAuthPolicyResponse pHttpStatus_ =
  PutAuthPolicyResponse'
    { policy = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The auth policy. The policy string in JSON must not contain newlines or
-- blank lines.
putAuthPolicyResponse_policy :: Lens.Lens' PutAuthPolicyResponse (Prelude.Maybe Prelude.Text)
putAuthPolicyResponse_policy = Lens.lens (\PutAuthPolicyResponse' {policy} -> policy) (\s@PutAuthPolicyResponse' {} a -> s {policy = a} :: PutAuthPolicyResponse)

-- | The state of the auth policy. The auth policy is only active when the
-- auth type is set to @AWS_IAM@. If you provide a policy, then
-- authentication and authorization decisions are made based on this policy
-- and the client\'s IAM policy. If the Auth type is @NONE@, then, any auth
-- policy you provide will remain inactive. For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html#create-service-network Create a service network>
-- in the /Amazon VPC Lattice User Guide/.
putAuthPolicyResponse_state :: Lens.Lens' PutAuthPolicyResponse (Prelude.Maybe AuthPolicyState)
putAuthPolicyResponse_state = Lens.lens (\PutAuthPolicyResponse' {state} -> state) (\s@PutAuthPolicyResponse' {} a -> s {state = a} :: PutAuthPolicyResponse)

-- | The response's http status code.
putAuthPolicyResponse_httpStatus :: Lens.Lens' PutAuthPolicyResponse Prelude.Int
putAuthPolicyResponse_httpStatus = Lens.lens (\PutAuthPolicyResponse' {httpStatus} -> httpStatus) (\s@PutAuthPolicyResponse' {} a -> s {httpStatus = a} :: PutAuthPolicyResponse)

instance Prelude.NFData PutAuthPolicyResponse where
  rnf PutAuthPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
