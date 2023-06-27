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
-- Module      : Amazonka.Glacier.CompleteVaultLock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation completes the vault locking process by transitioning the
-- vault lock from the @InProgress@ state to the @Locked@ state, which
-- causes the vault lock policy to become unchangeable. A vault lock is put
-- into the @InProgress@ state by calling InitiateVaultLock. You can obtain
-- the state of the vault lock by calling GetVaultLock. For more
-- information about the vault locking process,
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock>.
--
-- This operation is idempotent. This request is always successful if the
-- vault lock is in the @Locked@ state and the provided lock ID matches the
-- lock ID originally used to lock the vault.
--
-- If an invalid lock ID is passed in the request when the vault lock is in
-- the @Locked@ state, the operation returns an @AccessDeniedException@
-- error. If an invalid lock ID is passed in the request when the vault
-- lock is in the @InProgress@ state, the operation throws an
-- @InvalidParameter@ error.
module Amazonka.Glacier.CompleteVaultLock
  ( -- * Creating a Request
    CompleteVaultLock (..),
    newCompleteVaultLock,

    -- * Request Lenses
    completeVaultLock_accountId,
    completeVaultLock_vaultName,
    completeVaultLock_lockId,

    -- * Destructuring the Response
    CompleteVaultLockResponse (..),
    newCompleteVaultLockResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input values for @CompleteVaultLock@.
--
-- /See:/ 'newCompleteVaultLock' smart constructor.
data CompleteVaultLock = CompleteVaultLock'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the
    -- AWS account ID associated with the credentials used to sign the request.
    -- You can either specify an AWS account ID or optionally a single \'@-@\'
    -- (hyphen), in which case Amazon Glacier uses the AWS account ID
    -- associated with the credentials used to sign the request. If you specify
    -- your account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The @lockId@ value is the lock ID obtained from a InitiateVaultLock
    -- request.
    lockId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteVaultLock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'completeVaultLock_accountId' - The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'completeVaultLock_vaultName' - The name of the vault.
--
-- 'lockId', 'completeVaultLock_lockId' - The @lockId@ value is the lock ID obtained from a InitiateVaultLock
-- request.
newCompleteVaultLock ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'lockId'
  Prelude.Text ->
  CompleteVaultLock
newCompleteVaultLock pAccountId_ pVaultName_ pLockId_ =
  CompleteVaultLock'
    { accountId = pAccountId_,
      vaultName = pVaultName_,
      lockId = pLockId_
    }

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
completeVaultLock_accountId :: Lens.Lens' CompleteVaultLock Prelude.Text
completeVaultLock_accountId = Lens.lens (\CompleteVaultLock' {accountId} -> accountId) (\s@CompleteVaultLock' {} a -> s {accountId = a} :: CompleteVaultLock)

-- | The name of the vault.
completeVaultLock_vaultName :: Lens.Lens' CompleteVaultLock Prelude.Text
completeVaultLock_vaultName = Lens.lens (\CompleteVaultLock' {vaultName} -> vaultName) (\s@CompleteVaultLock' {} a -> s {vaultName = a} :: CompleteVaultLock)

-- | The @lockId@ value is the lock ID obtained from a InitiateVaultLock
-- request.
completeVaultLock_lockId :: Lens.Lens' CompleteVaultLock Prelude.Text
completeVaultLock_lockId = Lens.lens (\CompleteVaultLock' {lockId} -> lockId) (\s@CompleteVaultLock' {} a -> s {lockId = a} :: CompleteVaultLock)

instance Core.AWSRequest CompleteVaultLock where
  type
    AWSResponse CompleteVaultLock =
      CompleteVaultLockResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull CompleteVaultLockResponse'

instance Prelude.Hashable CompleteVaultLock where
  hashWithSalt _salt CompleteVaultLock' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName
      `Prelude.hashWithSalt` lockId

instance Prelude.NFData CompleteVaultLock where
  rnf CompleteVaultLock' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName
      `Prelude.seq` Prelude.rnf lockId

instance Data.ToHeaders CompleteVaultLock where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CompleteVaultLock where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CompleteVaultLock where
  toPath CompleteVaultLock' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/lock-policy/",
        Data.toBS lockId
      ]

instance Data.ToQuery CompleteVaultLock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCompleteVaultLockResponse' smart constructor.
data CompleteVaultLockResponse = CompleteVaultLockResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteVaultLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCompleteVaultLockResponse ::
  CompleteVaultLockResponse
newCompleteVaultLockResponse =
  CompleteVaultLockResponse'

instance Prelude.NFData CompleteVaultLockResponse where
  rnf _ = ()
