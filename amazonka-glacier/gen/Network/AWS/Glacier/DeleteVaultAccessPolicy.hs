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
-- Module      : Network.AWS.Glacier.DeleteVaultAccessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the access policy associated with the specified
-- vault. The operation is eventually consistent; that is, it might take
-- some time for Amazon S3 Glacier to completely remove the access policy,
-- and you might still see the effect of the policy for a short time after
-- you send the delete request.
--
-- This operation is idempotent. You can invoke delete multiple times, even
-- if there is no policy associated with the vault. For more information
-- about vault access policies, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies>.
module Network.AWS.Glacier.DeleteVaultAccessPolicy
  ( -- * Creating a Request
    DeleteVaultAccessPolicy (..),
    newDeleteVaultAccessPolicy,

    -- * Request Lenses
    deleteVaultAccessPolicy_accountId,
    deleteVaultAccessPolicy_vaultName,

    -- * Destructuring the Response
    DeleteVaultAccessPolicyResponse (..),
    newDeleteVaultAccessPolicyResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | DeleteVaultAccessPolicy input.
--
-- /See:/ 'newDeleteVaultAccessPolicy' smart constructor.
data DeleteVaultAccessPolicy = DeleteVaultAccessPolicy'
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
-- Create a value of 'DeleteVaultAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteVaultAccessPolicy_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'deleteVaultAccessPolicy_vaultName' - The name of the vault.
newDeleteVaultAccessPolicy ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  DeleteVaultAccessPolicy
newDeleteVaultAccessPolicy pAccountId_ pVaultName_ =
  DeleteVaultAccessPolicy'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
deleteVaultAccessPolicy_accountId :: Lens.Lens' DeleteVaultAccessPolicy Prelude.Text
deleteVaultAccessPolicy_accountId = Lens.lens (\DeleteVaultAccessPolicy' {accountId} -> accountId) (\s@DeleteVaultAccessPolicy' {} a -> s {accountId = a} :: DeleteVaultAccessPolicy)

-- | The name of the vault.
deleteVaultAccessPolicy_vaultName :: Lens.Lens' DeleteVaultAccessPolicy Prelude.Text
deleteVaultAccessPolicy_vaultName = Lens.lens (\DeleteVaultAccessPolicy' {vaultName} -> vaultName) (\s@DeleteVaultAccessPolicy' {} a -> s {vaultName = a} :: DeleteVaultAccessPolicy)

instance Prelude.AWSRequest DeleteVaultAccessPolicy where
  type
    Rs DeleteVaultAccessPolicy =
      DeleteVaultAccessPolicyResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteVaultAccessPolicyResponse'

instance Prelude.Hashable DeleteVaultAccessPolicy

instance Prelude.NFData DeleteVaultAccessPolicy

instance Prelude.ToHeaders DeleteVaultAccessPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteVaultAccessPolicy where
  toPath DeleteVaultAccessPolicy' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/access-policy"
      ]

instance Prelude.ToQuery DeleteVaultAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVaultAccessPolicyResponse' smart constructor.
data DeleteVaultAccessPolicyResponse = DeleteVaultAccessPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVaultAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVaultAccessPolicyResponse ::
  DeleteVaultAccessPolicyResponse
newDeleteVaultAccessPolicyResponse =
  DeleteVaultAccessPolicyResponse'

instance
  Prelude.NFData
    DeleteVaultAccessPolicyResponse
