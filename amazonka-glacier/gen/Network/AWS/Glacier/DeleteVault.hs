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
-- Module      : Network.AWS.Glacier.DeleteVault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes a vault. Amazon S3 Glacier will delete a vault
-- only if there are no archives in the vault as of the last inventory and
-- there have been no writes to the vault since the last inventory. If
-- either of these conditions is not satisfied, the vault deletion fails
-- (that is, the vault is not removed) and Amazon S3 Glacier returns an
-- error. You can use DescribeVault to return the number of archives in a
-- vault, and you can use
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job (POST jobs)>
-- to initiate a new inventory retrieval for a vault. The inventory
-- contains the archive IDs you use to delete archives using
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive (DELETE archive)>.
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-vaults.html Deleting a Vault in Amazon Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-delete.html Delete Vault>
-- in the /Amazon S3 Glacier Developer Guide/.
module Network.AWS.Glacier.DeleteVault
  ( -- * Creating a Request
    DeleteVault (..),
    newDeleteVault,

    -- * Request Lenses
    deleteVault_accountId,
    deleteVault_vaultName,

    -- * Destructuring the Response
    DeleteVaultResponse (..),
    newDeleteVaultResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for deleting a vault from Amazon S3 Glacier.
--
-- /See:/ 'newDeleteVault' smart constructor.
data DeleteVault = DeleteVault'
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
-- Create a value of 'DeleteVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteVault_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'deleteVault_vaultName' - The name of the vault.
newDeleteVault ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  DeleteVault
newDeleteVault pAccountId_ pVaultName_ =
  DeleteVault'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
deleteVault_accountId :: Lens.Lens' DeleteVault Prelude.Text
deleteVault_accountId = Lens.lens (\DeleteVault' {accountId} -> accountId) (\s@DeleteVault' {} a -> s {accountId = a} :: DeleteVault)

-- | The name of the vault.
deleteVault_vaultName :: Lens.Lens' DeleteVault Prelude.Text
deleteVault_vaultName = Lens.lens (\DeleteVault' {vaultName} -> vaultName) (\s@DeleteVault' {} a -> s {vaultName = a} :: DeleteVault)

instance Prelude.AWSRequest DeleteVault where
  type Rs DeleteVault = DeleteVaultResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteVaultResponse'

instance Prelude.Hashable DeleteVault

instance Prelude.NFData DeleteVault

instance Prelude.ToHeaders DeleteVault where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteVault where
  toPath DeleteVault' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName
      ]

instance Prelude.ToQuery DeleteVault where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVaultResponse' smart constructor.
data DeleteVaultResponse = DeleteVaultResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVaultResponse ::
  DeleteVaultResponse
newDeleteVaultResponse = DeleteVaultResponse'

instance Prelude.NFData DeleteVaultResponse
