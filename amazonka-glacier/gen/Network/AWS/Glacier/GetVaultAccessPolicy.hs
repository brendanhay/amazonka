{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glacier.GetVaultAccessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glacier.GetVaultAccessPolicy
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

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetVaultAccessPolicy where
  type
    Rs GetVaultAccessPolicy =
      GetVaultAccessPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVaultAccessPolicyResponse'
            Prelude.<$> (Prelude.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVaultAccessPolicy

instance Prelude.NFData GetVaultAccessPolicy

instance Prelude.ToHeaders GetVaultAccessPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetVaultAccessPolicy where
  toPath GetVaultAccessPolicy' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/access-policy"
      ]

instance Prelude.ToQuery GetVaultAccessPolicy where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetVaultAccessPolicyResponse
