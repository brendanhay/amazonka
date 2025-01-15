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
-- Module      : Amazonka.Glacier.SetVaultAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures an access policy for a vault and will
-- overwrite an existing policy. To configure a vault access policy, send a
-- PUT request to the @access-policy@ subresource of the vault. An access
-- policy is specific to a vault and is also called a vault subresource.
-- You can set one access policy per vault and the policy can be up to 20
-- KB in size. For more information about vault access policies, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies>.
module Amazonka.Glacier.SetVaultAccessPolicy
  ( -- * Creating a Request
    SetVaultAccessPolicy (..),
    newSetVaultAccessPolicy,

    -- * Request Lenses
    setVaultAccessPolicy_policy,
    setVaultAccessPolicy_accountId,
    setVaultAccessPolicy_vaultName,

    -- * Destructuring the Response
    SetVaultAccessPolicyResponse (..),
    newSetVaultAccessPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | SetVaultAccessPolicy input.
--
-- /See:/ 'newSetVaultAccessPolicy' smart constructor.
data SetVaultAccessPolicy = SetVaultAccessPolicy'
  { -- | The vault access policy as a JSON string.
    policy :: Prelude.Maybe VaultAccessPolicy,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
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
-- Create a value of 'SetVaultAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'setVaultAccessPolicy_policy' - The vault access policy as a JSON string.
--
-- 'accountId', 'setVaultAccessPolicy_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'setVaultAccessPolicy_vaultName' - The name of the vault.
newSetVaultAccessPolicy ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  SetVaultAccessPolicy
newSetVaultAccessPolicy pAccountId_ pVaultName_ =
  SetVaultAccessPolicy'
    { policy = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The vault access policy as a JSON string.
setVaultAccessPolicy_policy :: Lens.Lens' SetVaultAccessPolicy (Prelude.Maybe VaultAccessPolicy)
setVaultAccessPolicy_policy = Lens.lens (\SetVaultAccessPolicy' {policy} -> policy) (\s@SetVaultAccessPolicy' {} a -> s {policy = a} :: SetVaultAccessPolicy)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
setVaultAccessPolicy_accountId :: Lens.Lens' SetVaultAccessPolicy Prelude.Text
setVaultAccessPolicy_accountId = Lens.lens (\SetVaultAccessPolicy' {accountId} -> accountId) (\s@SetVaultAccessPolicy' {} a -> s {accountId = a} :: SetVaultAccessPolicy)

-- | The name of the vault.
setVaultAccessPolicy_vaultName :: Lens.Lens' SetVaultAccessPolicy Prelude.Text
setVaultAccessPolicy_vaultName = Lens.lens (\SetVaultAccessPolicy' {vaultName} -> vaultName) (\s@SetVaultAccessPolicy' {} a -> s {vaultName = a} :: SetVaultAccessPolicy)

instance Core.AWSRequest SetVaultAccessPolicy where
  type
    AWSResponse SetVaultAccessPolicy =
      SetVaultAccessPolicyResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull SetVaultAccessPolicyResponse'

instance Prelude.Hashable SetVaultAccessPolicy where
  hashWithSalt _salt SetVaultAccessPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName

instance Prelude.NFData SetVaultAccessPolicy where
  rnf SetVaultAccessPolicy' {..} =
    Prelude.rnf policy `Prelude.seq`
      Prelude.rnf accountId `Prelude.seq`
        Prelude.rnf vaultName

instance Data.ToHeaders SetVaultAccessPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON SetVaultAccessPolicy where
  toJSON SetVaultAccessPolicy' {..} = Data.toJSON policy

instance Data.ToPath SetVaultAccessPolicy where
  toPath SetVaultAccessPolicy' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/access-policy"
      ]

instance Data.ToQuery SetVaultAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetVaultAccessPolicyResponse' smart constructor.
data SetVaultAccessPolicyResponse = SetVaultAccessPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetVaultAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetVaultAccessPolicyResponse ::
  SetVaultAccessPolicyResponse
newSetVaultAccessPolicyResponse =
  SetVaultAccessPolicyResponse'

instance Prelude.NFData SetVaultAccessPolicyResponse where
  rnf _ = ()
