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
-- Module      : Network.AWS.Glacier.AbortVaultLock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.Glacier.AbortVaultLock
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

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AbortVaultLock where
  type Rs AbortVaultLock = AbortVaultLockResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull AbortVaultLockResponse'

instance Prelude.Hashable AbortVaultLock

instance Prelude.NFData AbortVaultLock

instance Prelude.ToHeaders AbortVaultLock where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AbortVaultLock where
  toPath AbortVaultLock' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/lock-policy"
      ]

instance Prelude.ToQuery AbortVaultLock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAbortVaultLockResponse' smart constructor.
data AbortVaultLockResponse = AbortVaultLockResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AbortVaultLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAbortVaultLockResponse ::
  AbortVaultLockResponse
newAbortVaultLockResponse = AbortVaultLockResponse'

instance Prelude.NFData AbortVaultLockResponse
