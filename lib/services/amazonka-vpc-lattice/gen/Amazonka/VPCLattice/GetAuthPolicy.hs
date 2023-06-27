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
-- Module      : Amazonka.VPCLattice.GetAuthPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the auth policy for the specified service or
-- service network.
module Amazonka.VPCLattice.GetAuthPolicy
  ( -- * Creating a Request
    GetAuthPolicy (..),
    newGetAuthPolicy,

    -- * Request Lenses
    getAuthPolicy_resourceIdentifier,

    -- * Destructuring the Response
    GetAuthPolicyResponse (..),
    newGetAuthPolicyResponse,

    -- * Response Lenses
    getAuthPolicyResponse_createdAt,
    getAuthPolicyResponse_lastUpdatedAt,
    getAuthPolicyResponse_policy,
    getAuthPolicyResponse_state,
    getAuthPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetAuthPolicy' smart constructor.
data GetAuthPolicy = GetAuthPolicy'
  { -- | The ID or Amazon Resource Name (ARN) of the service network or service.
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAuthPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'getAuthPolicy_resourceIdentifier' - The ID or Amazon Resource Name (ARN) of the service network or service.
newGetAuthPolicy ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  GetAuthPolicy
newGetAuthPolicy pResourceIdentifier_ =
  GetAuthPolicy'
    { resourceIdentifier =
        pResourceIdentifier_
    }

-- | The ID or Amazon Resource Name (ARN) of the service network or service.
getAuthPolicy_resourceIdentifier :: Lens.Lens' GetAuthPolicy Prelude.Text
getAuthPolicy_resourceIdentifier = Lens.lens (\GetAuthPolicy' {resourceIdentifier} -> resourceIdentifier) (\s@GetAuthPolicy' {} a -> s {resourceIdentifier = a} :: GetAuthPolicy)

instance Core.AWSRequest GetAuthPolicy where
  type
    AWSResponse GetAuthPolicy =
      GetAuthPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAuthPolicyResponse'
            Prelude.<$> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "policy")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAuthPolicy where
  hashWithSalt _salt GetAuthPolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData GetAuthPolicy where
  rnf GetAuthPolicy' {..} =
    Prelude.rnf resourceIdentifier

instance Data.ToHeaders GetAuthPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAuthPolicy where
  toPath GetAuthPolicy' {..} =
    Prelude.mconcat
      ["/authpolicy/", Data.toBS resourceIdentifier]

instance Data.ToQuery GetAuthPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAuthPolicyResponse' smart constructor.
data GetAuthPolicyResponse = GetAuthPolicyResponse'
  { -- | The date and time that the auth policy was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The date and time that the auth policy was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The auth policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The state of the auth policy. The auth policy is only active when the
    -- auth type is set to @AWS_IAM@. If you provide a policy, then
    -- authentication and authorization decisions are made based on this policy
    -- and the client\'s IAM policy. If the auth type is @NONE@, then any auth
    -- policy you provide will remain inactive. For more information, see
    -- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html#create-service-network Create a service network>
    -- in the /Amazon VPC Lattice User Guide/.
    state :: Prelude.Maybe AuthPolicyState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAuthPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'getAuthPolicyResponse_createdAt' - The date and time that the auth policy was created, specified in
-- ISO-8601 format.
--
-- 'lastUpdatedAt', 'getAuthPolicyResponse_lastUpdatedAt' - The date and time that the auth policy was last updated, specified in
-- ISO-8601 format.
--
-- 'policy', 'getAuthPolicyResponse_policy' - The auth policy.
--
-- 'state', 'getAuthPolicyResponse_state' - The state of the auth policy. The auth policy is only active when the
-- auth type is set to @AWS_IAM@. If you provide a policy, then
-- authentication and authorization decisions are made based on this policy
-- and the client\'s IAM policy. If the auth type is @NONE@, then any auth
-- policy you provide will remain inactive. For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html#create-service-network Create a service network>
-- in the /Amazon VPC Lattice User Guide/.
--
-- 'httpStatus', 'getAuthPolicyResponse_httpStatus' - The response's http status code.
newGetAuthPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAuthPolicyResponse
newGetAuthPolicyResponse pHttpStatus_ =
  GetAuthPolicyResponse'
    { createdAt = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      policy = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that the auth policy was created, specified in
-- ISO-8601 format.
getAuthPolicyResponse_createdAt :: Lens.Lens' GetAuthPolicyResponse (Prelude.Maybe Prelude.UTCTime)
getAuthPolicyResponse_createdAt = Lens.lens (\GetAuthPolicyResponse' {createdAt} -> createdAt) (\s@GetAuthPolicyResponse' {} a -> s {createdAt = a} :: GetAuthPolicyResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time that the auth policy was last updated, specified in
-- ISO-8601 format.
getAuthPolicyResponse_lastUpdatedAt :: Lens.Lens' GetAuthPolicyResponse (Prelude.Maybe Prelude.UTCTime)
getAuthPolicyResponse_lastUpdatedAt = Lens.lens (\GetAuthPolicyResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetAuthPolicyResponse' {} a -> s {lastUpdatedAt = a} :: GetAuthPolicyResponse) Prelude.. Lens.mapping Data._Time

-- | The auth policy.
getAuthPolicyResponse_policy :: Lens.Lens' GetAuthPolicyResponse (Prelude.Maybe Prelude.Text)
getAuthPolicyResponse_policy = Lens.lens (\GetAuthPolicyResponse' {policy} -> policy) (\s@GetAuthPolicyResponse' {} a -> s {policy = a} :: GetAuthPolicyResponse)

-- | The state of the auth policy. The auth policy is only active when the
-- auth type is set to @AWS_IAM@. If you provide a policy, then
-- authentication and authorization decisions are made based on this policy
-- and the client\'s IAM policy. If the auth type is @NONE@, then any auth
-- policy you provide will remain inactive. For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html#create-service-network Create a service network>
-- in the /Amazon VPC Lattice User Guide/.
getAuthPolicyResponse_state :: Lens.Lens' GetAuthPolicyResponse (Prelude.Maybe AuthPolicyState)
getAuthPolicyResponse_state = Lens.lens (\GetAuthPolicyResponse' {state} -> state) (\s@GetAuthPolicyResponse' {} a -> s {state = a} :: GetAuthPolicyResponse)

-- | The response's http status code.
getAuthPolicyResponse_httpStatus :: Lens.Lens' GetAuthPolicyResponse Prelude.Int
getAuthPolicyResponse_httpStatus = Lens.lens (\GetAuthPolicyResponse' {httpStatus} -> httpStatus) (\s@GetAuthPolicyResponse' {} a -> s {httpStatus = a} :: GetAuthPolicyResponse)

instance Prelude.NFData GetAuthPolicyResponse where
  rnf GetAuthPolicyResponse' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
