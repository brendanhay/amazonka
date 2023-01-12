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
-- Module      : Amazonka.Glacier.GetVaultAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the @access-policy@ subresource set on the
-- vault; for more information on setting this subresource, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultAccessPolicy.html Set Vault Access Policy (PUT access-policy)>.
-- If there is no access policy set on the vault, the operation returns a
-- @404 Not found@ error. For more information about vault access policies,
-- see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies>.
module Amazonka.Glacier.GetVaultAccessPolicy
  ( -- * Creating a Request
    GetVaultAccessPolicy (..),
    newGetVaultAccessPolicy,

    -- * Request Lenses
    getVaultAccessPolicy_accountId,
    getVaultAccessPolicy_vaultName,

    -- * Destructuring the Response
    GetVaultAccessPolicyResponse (..),
    newGetVaultAccessPolicyResponse,

    -- * Response Lenses
    getVaultAccessPolicyResponse_policy,
    getVaultAccessPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input for GetVaultAccessPolicy.
--
-- /See:/ 'newGetVaultAccessPolicy' smart constructor.
data GetVaultAccessPolicy = GetVaultAccessPolicy'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVaultAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getVaultAccessPolicy_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'getVaultAccessPolicy_vaultName' - The name of the vault.
newGetVaultAccessPolicy ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  GetVaultAccessPolicy
newGetVaultAccessPolicy pAccountId_ pVaultName_ =
  GetVaultAccessPolicy'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
getVaultAccessPolicy_accountId :: Lens.Lens' GetVaultAccessPolicy Prelude.Text
getVaultAccessPolicy_accountId = Lens.lens (\GetVaultAccessPolicy' {accountId} -> accountId) (\s@GetVaultAccessPolicy' {} a -> s {accountId = a} :: GetVaultAccessPolicy)

-- | The name of the vault.
getVaultAccessPolicy_vaultName :: Lens.Lens' GetVaultAccessPolicy Prelude.Text
getVaultAccessPolicy_vaultName = Lens.lens (\GetVaultAccessPolicy' {vaultName} -> vaultName) (\s@GetVaultAccessPolicy' {} a -> s {vaultName = a} :: GetVaultAccessPolicy)

instance Core.AWSRequest GetVaultAccessPolicy where
  type
    AWSResponse GetVaultAccessPolicy =
      GetVaultAccessPolicyResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVaultAccessPolicyResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVaultAccessPolicy where
  hashWithSalt _salt GetVaultAccessPolicy' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName

instance Prelude.NFData GetVaultAccessPolicy where
  rnf GetVaultAccessPolicy' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName

instance Data.ToHeaders GetVaultAccessPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVaultAccessPolicy where
  toPath GetVaultAccessPolicy' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/access-policy"
      ]

instance Data.ToQuery GetVaultAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | Output for GetVaultAccessPolicy.
--
-- /See:/ 'newGetVaultAccessPolicyResponse' smart constructor.
data GetVaultAccessPolicyResponse = GetVaultAccessPolicyResponse'
  { -- | Contains the returned vault access policy as a JSON string.
    policy :: Prelude.Maybe VaultAccessPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVaultAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getVaultAccessPolicyResponse_policy' - Contains the returned vault access policy as a JSON string.
--
-- 'httpStatus', 'getVaultAccessPolicyResponse_httpStatus' - The response's http status code.
newGetVaultAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVaultAccessPolicyResponse
newGetVaultAccessPolicyResponse pHttpStatus_ =
  GetVaultAccessPolicyResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the returned vault access policy as a JSON string.
getVaultAccessPolicyResponse_policy :: Lens.Lens' GetVaultAccessPolicyResponse (Prelude.Maybe VaultAccessPolicy)
getVaultAccessPolicyResponse_policy = Lens.lens (\GetVaultAccessPolicyResponse' {policy} -> policy) (\s@GetVaultAccessPolicyResponse' {} a -> s {policy = a} :: GetVaultAccessPolicyResponse)

-- | The response's http status code.
getVaultAccessPolicyResponse_httpStatus :: Lens.Lens' GetVaultAccessPolicyResponse Prelude.Int
getVaultAccessPolicyResponse_httpStatus = Lens.lens (\GetVaultAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@GetVaultAccessPolicyResponse' {} a -> s {httpStatus = a} :: GetVaultAccessPolicyResponse)

instance Prelude.NFData GetVaultAccessPolicyResponse where
  rnf GetVaultAccessPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
