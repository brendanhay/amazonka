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
-- Module      : Amazonka.Glacier.AbortVaultLock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts the vault locking process if the vault lock is not
-- in the @Locked@ state. If the vault lock is in the @Locked@ state when
-- this operation is requested, the operation returns an
-- @AccessDeniedException@ error. Aborting the vault locking process
-- removes the vault lock policy from the specified vault.
--
-- A vault lock is put into the @InProgress@ state by calling
-- InitiateVaultLock. A vault lock is put into the @Locked@ state by
-- calling CompleteVaultLock. You can get the state of a vault lock by
-- calling GetVaultLock. For more information about the vault locking
-- process, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock>.
-- For more information about vault lock policies, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies>.
--
-- This operation is idempotent. You can successfully invoke this operation
-- multiple times, if the vault lock is in the @InProgress@ state or if
-- there is no policy associated with the vault.
module Amazonka.Glacier.AbortVaultLock
  ( -- * Creating a Request
    AbortVaultLock (..),
    newAbortVaultLock,

    -- * Request Lenses
    abortVaultLock_accountId,
    abortVaultLock_vaultName,

    -- * Destructuring the Response
    AbortVaultLockResponse (..),
    newAbortVaultLockResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input values for @AbortVaultLock@.
--
-- /See:/ 'newAbortVaultLock' smart constructor.
data AbortVaultLock = AbortVaultLock'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the
    -- AWS account ID associated with the credentials used to sign the request.
    -- You can either specify an AWS account ID or optionally a single \'@-@\'
    -- (hyphen), in which case Amazon Glacier uses the AWS account ID
    -- associated with the credentials used to sign the request. If you specify
    -- your account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortVaultLock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'abortVaultLock_accountId' - The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'abortVaultLock_vaultName' - The name of the vault.
newAbortVaultLock ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  AbortVaultLock
newAbortVaultLock pAccountId_ pVaultName_ =
  AbortVaultLock'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
abortVaultLock_accountId :: Lens.Lens' AbortVaultLock Prelude.Text
abortVaultLock_accountId = Lens.lens (\AbortVaultLock' {accountId} -> accountId) (\s@AbortVaultLock' {} a -> s {accountId = a} :: AbortVaultLock)

-- | The name of the vault.
abortVaultLock_vaultName :: Lens.Lens' AbortVaultLock Prelude.Text
abortVaultLock_vaultName = Lens.lens (\AbortVaultLock' {vaultName} -> vaultName) (\s@AbortVaultLock' {} a -> s {vaultName = a} :: AbortVaultLock)

instance Core.AWSRequest AbortVaultLock where
  type
    AWSResponse AbortVaultLock =
      AbortVaultLockResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveNull AbortVaultLockResponse'

instance Prelude.Hashable AbortVaultLock where
  hashWithSalt _salt AbortVaultLock' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName

instance Prelude.NFData AbortVaultLock where
  rnf AbortVaultLock' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName

instance Data.ToHeaders AbortVaultLock where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AbortVaultLock where
  toPath AbortVaultLock' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/lock-policy"
      ]

instance Data.ToQuery AbortVaultLock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAbortVaultLockResponse' smart constructor.
data AbortVaultLockResponse = AbortVaultLockResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortVaultLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAbortVaultLockResponse ::
  AbortVaultLockResponse
newAbortVaultLockResponse = AbortVaultLockResponse'

instance Prelude.NFData AbortVaultLockResponse where
  rnf _ = ()
